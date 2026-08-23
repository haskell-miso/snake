import { WASI, OpenFile, File, ConsoleStdout } from "https://cdn.jsdelivr.net/npm/@bjorn3/browser_wasi_shim@0.3.0/dist/index.js";
import ghc_wasm_jsffi from "./ghc_wasm_jsffi.js";

const args = [];
// GC tuning, measured with GHCRTS=-S:
//   -H64m (old): young gen ballooned, each GC recopied ~14MB live set
//                -> 100-160ms stall every ~10s. Never use.
//   default:     ~12ms minor GC every ~2s (copies ~2MB of live vdom).
//   -A8m:        same ~12ms pause, half as frequent. Survivor size is
//                bounded by live data, so a moderately bigger nursery
//                only spaces GCs out without growing the copy.
const env = ["GHCRTS=-A8m"];
const fds = [
  new OpenFile(new File([])), // stdin
  ConsoleStdout.lineBuffered((msg) => console.log(`[WASI stdout] ''${msg}`)),
  ConsoleStdout.lineBuffered((msg) => console.warn(`[WASI stderr] ''${msg}`)),
];
const options = { debug: false };
const wasi = new WASI(args, env, fds, options);

const instance_exports = {};
const { instance } = await WebAssembly.instantiateStreaming(fetch("app.wasm"), {
  wasi_snapshot_preview1: wasi.wasiImport,
  ghc_wasm_jsffi: ghc_wasm_jsffi(instance_exports),
});
Object.assign(instance_exports, instance.exports);

wasi.initialize(instance);
await instance.exports.hs_start(globalThis.example);
