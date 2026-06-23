----------------------------------------------------------------------------
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments    #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE CPP               #-}
----------------------------------------------------------------------------
module Main where
----------------------------------------------------------------------------
import           Control.Monad (void, when)
import           Data.Foldable (toList)
import           Data.IORef
import           Data.Sequence (Seq, ViewL(..), ViewR(..))
import qualified Data.Sequence as Seq
import           Data.Set (Set)
import qualified Data.Set as Set
----------------------------------------------------------------------------
import           Miso hiding (Phase)
import           Miso.CSS hiding (ms, background, Phase)
import           Miso.DSL (syncCallback1, requestAnimationFrame, fromJSValUnchecked, freeFunction, Function(..))
import qualified Miso.Html as H
import qualified Miso.Html.Property as HP
import qualified Miso.Svg as S
import qualified Miso.Svg.Property as SP
import           Miso.Lens
import           Miso.Random (replicateRM)
import           Miso.Reload
import           Miso.Subscription.Util (createSub)
----------------------------------------------------------------------------

gridSize :: Int
gridSize = 20

cellSize :: Int
cellSize = 24

boardPx :: Int
boardPx = gridSize * cellSize

tickInterval :: Double
tickInterval = 175.0

-- CSS transition string derived from tickInterval so they stay in sync.
segTransition :: Style
segTransition = transition ("transform " <> ms (round tickInterval :: Int) <> "ms linear")

data Dir = DUp | DDown | DLeft | DRight deriving (Show, Eq)

data Phase = NotStarted | Playing | GameOver deriving (Show, Eq)

data Model = Model
  { _snake    :: !(Seq (Int, Int))
  , _occupied :: !(Set (Int, Int))
  , _dir      :: !Dir
  , _queued   :: !Dir
  , _food     :: !(Int, Int)
  , _score    :: !Int
  , _phase    :: !Phase
  , _prevLen  :: !Int   -- snake length at start of last tick; used to suppress CSS transition on newly-grown segments
  } deriving (Show, Eq)

snake :: Lens Model (Seq (Int, Int))
snake = lens _snake $ \r x -> r { _snake = x }

occupied :: Lens Model (Set (Int, Int))
occupied = lens _occupied $ \r x -> r { _occupied = x }

dir :: Lens Model Dir
dir = lens _dir $ \r x -> r { _dir = x }

queued :: Lens Model Dir
queued = lens _queued $ \r x -> r { _queued = x }

food :: Lens Model (Int, Int)
food = lens _food $ \r x -> r { _food = x }

score :: Lens Model Int
score = lens _score $ \r x -> r { _score = x }

phase :: Lens Model Phase
phase = lens _phase $ \r x -> r { _phase = x }

prevLen :: Lens Model Int
prevLen = lens _prevLen $ \r x -> r { _prevLen = x }

data Action
  = Tick
  | Turn Dir
  | PlaceFood (Int, Int)
  | NewGame
  | NoOp
  deriving (Show, Eq)

main :: IO ()
#ifdef INTERACTIVE
main = reload mempty app
#else
main = startApp mempty app
#endif

#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

initSnake :: Seq (Int, Int)
initSnake = Seq.fromList [(10,10),(9,10),(8,10)]

initOccupied :: Set (Int, Int)
initOccupied = Set.fromList [(10,10),(9,10),(8,10)]

initFood :: (Int, Int)
initFood = (15,10)

emptyModel :: Model
emptyModel = Model
  { _snake    = initSnake
  , _occupied = initOccupied
  , _dir      = DRight
  , _queued   = DRight
  , _food     = initFood
  , _score    = 0
  , _phase    = NotStarted
  , _prevLen  = Seq.length initSnake
  }

rAFSubElapsed :: Double -> action -> Sub action
rAFSubElapsed interval action sink = createSub acquire release sink
  where
    acquire = do
      cbRef   <- newIORef (error "rAFSubElapsed: uninitialized, impossible")
      lastRef <- newIORef (0.0 :: Double)
      elapRef <- newIORef (0.0 :: Double)
      callback <- syncCallback1 $ \jsval -> do
        t    <- fromJSValUnchecked jsval
        prev <- readIORef lastRef
        writeIORef lastRef t
        let dt = if prev == 0 then 0 else min interval (t - prev)
        elap <- readIORef elapRef
        let newElap = elap + dt
        if newElap >= interval
          then do
            writeIORef elapRef (newElap - interval)
            sink action
          else writeIORef elapRef newElap
        void (requestAnimationFrame =<< readIORef cbRef)
      writeIORef cbRef callback
      void (requestAnimationFrame callback)
      pure callback
    release callback = freeFunction (Function callback)

app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { subs =
    [ rAFSubElapsed tickInterval Tick
    , \sink -> windowSub "keydown" keycodeDecoder (\case
        KeyCode 37 -> Turn DLeft
        KeyCode 38 -> Turn DUp
        KeyCode 39 -> Turn DRight
        KeyCode 40 -> Turn DDown
        KeyCode 78 -> NewGame
        _ -> NoOp) sink
    ]
  }

opposite :: Dir -> Dir
opposite DUp    = DDown
opposite DDown  = DUp
opposite DLeft  = DRight
opposite DRight = DLeft

step :: Dir -> (Int, Int) -> (Int, Int)
step DUp    (x,y) = (x, y-1)
step DDown  (x,y) = (x, y+1)
step DLeft  (x,y) = (x-1, y)
step DRight (x,y) = (x+1, y)

pickFood :: Set (Int, Int) -> IO (Int, Int)
pickFood occ = do
  [rx, ry] <- replicateRM 2
  let x = floor (rx * fromIntegral gridSize) `mod` gridSize
      y = floor (ry * fromIntegral gridSize) `mod` gridSize
  if Set.member (x, y) occ then pickFood occ else pure (x, y)

updateModel :: Action -> Effect parent props Model Action
updateModel = \case
  NoOp -> pure ()

  NewGame -> do
    put emptyModel { _phase = Playing }
    io $ pickFood initOccupied >>= pure . PlaceFood

  PlaceFood pos -> food .= pos

  Turn d -> do
    m <- get
    when (_phase m == NotStarted) (phase .= Playing)
    when (d /= opposite (_dir m)) (queued .= d)

  Tick -> do
    m <- get
    case _phase m of
      NotStarted -> pure ()
      GameOver   -> pure ()
      Playing    -> do
        let d        = _queued m
            body     = _snake m
            occ      = _occupied m
            headPos  = case Seq.viewl body of h :< _ -> h; _ -> (0,0)
            newHead  = step d headPos
            (nx, ny) = newHead
            wall     = nx < 0 || ny < 0 || nx >= gridSize || ny >= gridSize
            self     = Set.member newHead occ
        dir     .= d
        prevLen .= Seq.length body
        if wall || self
          then phase .= GameOver
          else case Seq.viewr body of
            EmptyR -> pure ()
            init' :> tailCell -> do
              let ate     = newHead == _food m
                  newBody | ate       = newHead Seq.<| body
                          | otherwise = newHead Seq.<| init'
                  newOcc  | ate       = Set.insert newHead occ
                          | otherwise = Set.insert newHead (Set.delete tailCell occ)
              snake    .= newBody
              occupied .= newOcc
              when ate $ do
                score += 1
                io $ pickFood newOcc >>= pure . PlaceFood

----------------------------------------------------------------------------
-- View
----------------------------------------------------------------------------

si :: Int -> MisoString
si = ms

svgCoord :: Int -> Int
svgCoord n = n * cellSize + 1

viewModel :: props -> Model -> View Model Action
viewModel _ m =
  H.div_
    [ style_
      [ display "flex"
      , flexDirection "column"
      , alignItems "center"
      , justifyContent "center"
      , height (vh 100.0)
      , margin "0"
      , padding "0"
      , backgroundColor (Hex "0d0d1a")
      , fontFamily "'Segoe UI', system-ui, monospace"
      , boxSizing "border-box"
      , overflow "hidden"
      ]
    ]

    [ H.style_ [] "html,body{margin:0;padding:0;overflow:hidden;}"
    , H.h1_
        [ style_
          [ margin "0 0 8px 0"
          , fontWeight "700"
          ]
        ]
        [ H.a_
            [ HP.href_ "https://github.com/haskell-miso/miso-snake"
            , HP.target_ "_blank"
            , style_
              [ color (Hex "4ade80")
              , fontSize (px 36)
              , letterSpacing "0.15em"
              , textShadow "0 0 30px rgba(74,222,128,0.6), 0 0 60px rgba(74,222,128,0.2)"
              , textDecoration "none"
              ]
            ]
            [ text "\x1F35C miso snake" ]
        ]
    , H.div_
        [ style_
          [ display "flex"
          , gap "32px"
          , marginBottom "12px"
          , color (Hex "94a3b8")
          , fontSize (px 16)
          , letterSpacing "0.05em"
          ]
        ]
        [ H.span_ [ style_ [ color (Hex "4ade80"), fontWeight "600" ] ]
            [ text ("SCORE  " <> ms (_score m)) ]
        ]
    , board m
    , H.div_
        [ style_
          [ marginTop "12px"
          , color (Hex "475569")
          , fontSize (px 12)
          , letterSpacing "0.1em"
          ]
        ]
        [ text "ARROW KEYS — MOVE    N — NEW GAME" ]
    ]

board :: Model -> View Model Action
board m =
  S.svg_
    [ HP.width_  (si (boardPx + 2))
    , HP.height_ (si (boardPx + 2))
    , SP.viewBox_ ("0 0 " <> si (boardPx + 2) <> " " <> si (boardPx + 2))
    , style_
      [ borderRadius "10px"
      , boxShadow "0 0 0 1px #1e2030, 0 0 40px rgba(74,222,128,0.12), 0 20px 60px rgba(0,0,0,0.6)"
      ]
    ]
    ( defs
    : background
    : gridLines
   ++ [renderFood (_food m)]
   ++ renderSnake (_prevLen m) (_snake m)
   ++ [overlay m]
    )

defs :: View Model Action
defs =
  S.defs_ []
    [ S.filter_
        [ HP.id_ "glow", SP.x_ "-50%", SP.y_ "-50%", HP.width_ "200%", HP.height_ "200%" ]
        [ S.feGaussianBlur_ [ SP.stdDeviation_ "3", SP.result_ "blur" ]
        , S.feMerge_ []
            [ S.feMergeNode_ [ SP.in_' "blur" ]
            , S.feMergeNode_ [ SP.in_' "SourceGraphic" ]
            ]
        ]
    , S.filter_
        [ HP.id_ "foodglow", SP.x_ "-80%", SP.y_ "-80%", HP.width_ "260%", HP.height_ "260%" ]
        [ S.feGaussianBlur_ [ SP.stdDeviation_ "5", SP.result_ "blur" ]
        , S.feMerge_ []
            [ S.feMergeNode_ [ SP.in_' "blur" ]
            , S.feMergeNode_ [ SP.in_' "SourceGraphic" ]
            ]
        ]
    , S.radialGradient_ [ HP.id_ "headGrad", SP.cx_ "40%", SP.cy_ "35%", SP.r_ "60%" ]
        [ S.stop_ [ SP.offset_  "0%", SP.stopColor_ "#86efac" ]
        , S.stop_ [ SP.offset_  "100%", SP.stopColor_ "#16a34a" ]
        ]
    , S.radialGradient_ [ HP.id_ "bodyGrad", SP.cx_ "40%", SP.cy_ "35%", SP.r_ "60%" ]
        [ S.stop_ [ SP.offset_  "0%", SP.stopColor_ "#4ade80" ]
        , S.stop_ [ SP.offset_  "100%", SP.stopColor_ "#15803d" ]
        ]
    , S.radialGradient_ [ HP.id_ "foodGrad", SP.cx_ "35%", SP.cy_ "30%", SP.r_ "65%" ]
        [ S.stop_ [ SP.offset_  "0%", SP.stopColor_ "#fb923c" ]
        , S.stop_ [ SP.offset_  "100%", SP.stopColor_ "#c2410c" ]
        ]
    ]

background :: View Model Action
background =
  S.g_ []
    [ S.rect_
        [ SP.x_ "0", SP.y_ "0"
        , HP.width_ (si (boardPx + 2)), HP.height_ (si (boardPx + 2))
        , SP.rx_ "10", SP.ry_ "10"
        , SP.fill_ "#0d0d1a"
        ]
    , S.rect_
        [ SP.x_ "1", SP.y_ "1"
        , HP.width_ (si boardPx), HP.height_ (si boardPx)
        , SP.rx_ "6", SP.ry_ "6"
        , SP.fill_ "#10101e"
        ]
    ]

gridLines :: [View Model Action]
gridLines =
  [ S.line_
      [ SP.x1_ (si (svgCoord col)), SP.y1_ "1"
      , SP.x2_ (si (svgCoord col)), SP.y2_ (si (boardPx + 1))
      , SP.stroke_ "#2a3f6f", SP.strokeWidth_ "1"
      ]
  | col <- [1..gridSize-1]
  ] ++
  [ S.line_
      [ SP.x1_ "1",              SP.y1_ (si (svgCoord row))
      , SP.x2_ (si (boardPx+1)), SP.y2_ (si (svgCoord row))
      , SP.stroke_ "#2a3f6f", SP.strokeWidth_ "1"
      ]
  | row <- [1..gridSize-1]
  ]

renderFood :: (Int, Int) -> View Model Action
renderFood (fx, fy) =
  let cx = svgCoord fx + cellSize `div` 2
      cy = svgCoord fy + cellSize `div` 2
      r  = cellSize `div` 2 - 3
  in S.g_ [ SP.filter_ "url(#foodglow)" ]
      [ S.circle_
          [ SP.cx_ (si cx), SP.cy_ (si cy)
          , SP.r_ (si r)
          , SP.fill_ "url(#foodGrad)"
          ]
      , S.circle_
          [ SP.cx_ (si (cx - 2)), SP.cy_ (si (cy - 3))
          , SP.r_ "2"
          , SP.fill_ "#fde68a"
          , SP.opacity_ "0.7"
          ]
      ]

-- Head is rendered at list index 0 so Miso always patches the same DOM
-- element for it — CSS transition fires correctly on every tick including
-- eating. Body segments follow head-to-tail; the segment at index >= prevLen
-- is newly grown and gets transition:none so it pops in at the old tail
-- position instead of flying from the SVG origin.
-- key_ is set on every element so Miso uses key-based reconciliation
-- (requires ALL siblings to have keys; without key_ it falls back to
-- position-based matching which also works but is less robust).
renderSnake :: Int -> Seq (Int, Int) -> [View Model Action]
renderSnake pl body = zipWith render [0..] (toList body)
  where
    render 0 pos = renderHead pos
    render i pos = renderBody (i >= pl) i pos

renderHead :: (Int, Int) -> View Model Action
renderHead (hx, hy) =
  let px  = svgCoord hx
      py  = svgCoord hy
      pad = 1
      sz  = cellSize - 2 * pad
      tx  = "translate(" <> ms px <> "px," <> ms py <> "px)"
  in S.g_
      [ key_ (0 :: Int)
      , style_ [ transform tx, segTransition ]
      ]
      [ S.rect_
          [ SP.x_ (si pad), SP.y_ (si pad)
          , HP.width_ (si sz), HP.height_ (si sz)
          , SP.rx_ "6", SP.ry_ "6"
          , SP.fill_ "url(#headGrad)"
          , SP.filter_ "url(#glow)"
          ]
      ]

renderBody :: Bool -> Int -> (Int, Int) -> View Model Action
renderBody isNew i (bx, by) =
  let px  = svgCoord bx
      py  = svgCoord by
      pad = 2
      sz  = cellSize - 2 * pad
      tx  = "translate(" <> ms px <> "px," <> ms py <> "px)"
      st  | isNew     = [ transform tx ]
          | otherwise = [ transform tx, segTransition ]
  in S.g_
      [ key_ i
      , style_ st
      ]
      [ S.rect_
          [ SP.x_ (si pad), SP.y_ (si pad)
          , HP.width_ (si sz), HP.height_ (si sz)
          , SP.rx_ "4", SP.ry_ "4"
          , SP.fill_ "url(#bodyGrad)"
          ]
      ]

overlay :: Model -> View Model Action
overlay m = case _phase m of
  Playing    -> S.g_ [] []
  NotStarted -> overlayBox "PRESS ANY ARROW KEY" "TO BEGIN" "#4ade80"
  GameOver   -> overlayBox "GAME OVER" "PRESS N TO RESTART" "#f87171"

overlayBox :: MisoString -> MisoString -> MisoString -> View Model Action
overlayBox title sub clr =
  S.g_ []
    [ S.rect_
        [ SP.x_ "0", SP.y_ "0"
        , HP.width_ (si (boardPx + 2)), HP.height_ (si (boardPx + 2))
        , SP.rx_ "10", SP.ry_ "10"
        , SP.fill_ "rgba(10,10,20,0.82)"
        ]
    , S.text_
        [ SP.x_ (si ((boardPx + 2) `div` 2))
        , SP.y_ (si ((boardPx + 2) `div` 2 - 20))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "middle"
        , SP.fill_ clr
        , SP.fontSize_ "26"
        , SP.fontWeight_ "700"
        , SP.letterSpacing_ "4"
        , SP.filter_ "url(#glow)"
        ] [ text title ]
    , S.text_
        [ SP.x_ (si ((boardPx + 2) `div` 2))
        , SP.y_ (si ((boardPx + 2) `div` 2 + 20))
        , SP.textAnchor_ "middle"
        , SP.dominantBaseline_ "middle"
        , SP.fill_ "#94a3b8"
        , SP.fontSize_ "13"
        , SP.letterSpacing_ "2"
        ] [ text sub ]
    ]
----------------------------------------------------------------------------
