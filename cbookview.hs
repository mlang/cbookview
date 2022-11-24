{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
import           Brick.AttrMap        (AttrName, attrMap, attrName)
import qualified Brick.Focus          as F
import           Brick.Main           (App (..), defaultMain, halt)
import           Brick.Types          (BrickEvent (VtyEvent), EventM,
                                       Location (Location), Widget, modify)
import           Brick.Util           (on)
import           Brick.Widgets.Border (border, borderWithLabel)
import           Brick.Widgets.Center (hCenter)
import           Brick.Widgets.Core   (hBox, hLimit, putCursor, str, txt,
                                       txtWrap, vBox, vLimit, withAttr, (<+>),
                                       (<=>))
import qualified Brick.Widgets.List   as L
import           Control.Monad        (void)
import           Data.Foldable        (foldl', toList)
import           Data.Ix
import           Data.List            (elemIndex, intersperse)
import           Data.List.Extra      (chunksOf)
import           Data.List.NonEmpty   (NonEmpty)
import qualified Data.List.NonEmpty   as NonEmpty
import           Data.Map             (Map)
import qualified Data.Map             as Map
import           Data.Maybe           (fromJust, fromMaybe)
import           Data.Tree            (Tree (..), foldTree)
import           Data.Tree.Zipper     (Full, TreePos, fromForest, label,
                                       nextTree)
import qualified Data.Tree.Zipper     as TreePos
import qualified Data.Vector          as Vector
import           Game.Chess           (Color (..), PieceType (..), Ply,
                                       Position, Square (A1, H8), color, doPly,
                                       isDark, pieceAt, plyTarget, startpos,
                                       toFEN)
import           Game.Chess.ECO       (Opening (..), defaultECO)
import qualified Game.Chess.ECO       as ECO
import           Game.Chess.PGN       (pgnForest, readPGNFile)
import           Game.Chess.Polyglot  (bookForest, defaultBook,
                                       readPolyglotFile)
import           Game.Chess.SAN       (toSAN, varToSAN)
import           Game.Chess.Tree      (plyForest)
import qualified Graphics.Vty         as Vty
import           Lens.Micro           ((&), (.~), (^.))
import           Lens.Micro.Mtl       (view, (%=))
import           Lens.Micro.TH        (makeLenses)
import           System.Environment   (getArgs)
import           System.FilePath

data Name = List | Board | BoardStyle deriving (Show, Ord, Eq)

type Style a = Position -> Square -> Widget a

data St = St { _initialPosition :: Position
             , _treePos         :: TreePos Full (NonEmpty Ply)
             , _boardStyle      :: L.List Name (String, Style Name)
             , _focusRing       :: F.FocusRing Name
             }

makeLenses ''St

initialState :: St
initialState = St { .. } where
  _initialPosition = startpos
  _treePos = fromJust . nextTree . fromForest
           $ pathTree <$> bookForest defaultBook _initialPosition
  _boardStyle = L.list BoardStyle (Vector.fromList styles) 1
  _focusRing = F.focusRing [List, Board, BoardStyle]

position, previousPosition :: St -> Position
position st = foldl' doPly (st^.initialPosition) (st^.treePos & label)
previousPosition st = foldl' doPly (st^.initialPosition) (st^.treePos & label & NonEmpty.init)

targetSquare :: St -> Square
targetSquare = plyTarget . NonEmpty.last . label . view treePos

elemList :: Eq a => n -> a -> [a] -> L.List n a
elemList n x xs = L.list n (Vector.fromList xs) 1 & L.listSelectedL .~ i where
  i = x `elemIndex` xs

plyList :: St -> L.List Name Ply
plyList (_treePos -> tp) = elemList List ply plies where
  ply = NonEmpty.last . TreePos.label $ tp
  plies = fmap (NonEmpty.last . rootLabel) . TreePos.forest $ tp

selectedAttr :: AttrName
selectedAttr = attrName "selected"

renderPosition :: Position -> Color -> Maybe Square -> Style Name -> Widget Name
renderPosition pos persp tgt sty = ranks <+> border board <=> files where
  rev = if persp == Black then reverse else id
  ranks = vBox (space : map (str . show) (rev [8 :: Int, 7..1]) <> [space])
  files = str $ rev "   a b c d e f g h   "
  board = hLimit 17 . vLimit 8 . vBox $ map (hBox . spacer . map pc) squares
  squares = reverse $ chunksOf 8 $ rev [A1 .. H8]
  pc sq = putCursorIf (tgt == Just sq) Board (0,0) $ sty pos sq
  spacer = (space :) . (<> [space]) . intersperse space
  space = txt " "

allPieces :: ((Color, PieceType), (Color, PieceType))
allPieces = ((Black, Pawn), (White, King))

english :: Style a
english pos sq = case pieceAt pos sq of
  Just piece           -> str . pure $ "pnbrqkPNBRQK" !! index allPieces piece
  Nothing | isDark sq  -> str "+"
          | otherwise  -> str " "

styles :: [(String, Style a)]
styles = [ ("English",  english)
         , ("Deutsch",  german)
         , ("Figurine", figurine)
         ]
 where
  german pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "bsltdkBSLTDK" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "
  figurine pos sq = case pieceAt pos sq of
    Just piece           -> str . pure $ "♟♞♝♜♛♚♙♘♗♖♕♔" !! index allPieces piece
    Nothing | isDark sq  -> str "+"
            | otherwise  -> str " "

putCursorIf :: Bool -> n -> (Int, Int) -> Widget n -> Widget n
putCursorIf True n loc = putCursor n $ Location loc
putCursorIf False _ _  = id

withAttrIf :: Bool -> AttrName -> Widget n -> Widget n
withAttrIf True attr = withAttr attr
withAttrIf False _   = id

type Command = EventM Name St ()

next, prev, firstChild, parent, root, firstLeaf :: Command
next       = treePos %= (fromMaybe <*> TreePos.next)
prev       = treePos %= (fromMaybe <*> TreePos.prev)
firstChild = treePos %= (fromMaybe <*> TreePos.firstChild)
parent     = treePos %= (fromMaybe <*> TreePos.parent)
root       = treePos %= TreePos.root
firstLeaf  = treePos %= go where go tp = maybe tp go $ TreePos.firstChild tp

nextCursor, prevCursor :: Command
nextCursor = focusRing %= F.focusNext
prevCursor = focusRing %= F.focusPrev

allPlies, internalBook :: Command
allPlies     = modify $ fromMaybe <*> loadForest plyForest startpos
internalBook = modify $ fromMaybe <*> loadForest (bookForest defaultBook) startpos

nextStyle, prevStyle :: Command
nextStyle = boardStyle %= L.listMoveDown
prevStyle = boardStyle %= L.listMoveUp

keyMap :: Map Vty.Event Command
keyMap = Map.fromList $ cursor <> vi <> common where
  cursor =
    [ (Vty.EvKey Vty.KDown [],       next)
    , (Vty.EvKey Vty.KUp [],         prev)
    , (Vty.EvKey Vty.KRight [],      firstChild)
    , (Vty.EvKey Vty.KLeft [],       parent)
    , (Vty.EvKey Vty.KHome [],       root)
    , (Vty.EvKey Vty.KEnd [],        firstLeaf)
    ]
  common =
    [ (Vty.EvKey (Vty.KChar '\t') [],        nextCursor)
    , (Vty.EvKey (Vty.KChar '\t') [Vty.MMeta], prevCursor)
    , (Vty.EvKey (Vty.KChar 'a') [],         allPlies)
    , (Vty.EvKey (Vty.KChar 'd') [],         internalBook)
    , (Vty.EvKey (Vty.KChar '+') [],         nextStyle)
    , (Vty.EvKey (Vty.KChar '-') [],         prevStyle)
    , (Vty.EvKey Vty.KEsc [],                halt)
    , (Vty.EvKey (Vty.KChar 'q') [],         halt)
    ]
  vi =
    [ (Vty.EvKey (Vty.KChar 'j') [], next)
    , (Vty.EvKey (Vty.KChar 'k') [], prev)
    , (Vty.EvKey (Vty.KChar 'l') [], firstChild)
    , (Vty.EvKey (Vty.KChar 'h') [], parent)
    ]

cbookview :: App St e Name
cbookview = App { .. } where
  appStartEvent = pure ()
  appDraw st = [ui] where
    ui = hBox [ hLimit 9 list
              , hLimit 23 $ hCenter board <=> str " " <=> eco
              , hCenter . hLimit 40 $ str " " <=> var
              ]
      <=> str " "
      <=> (str "FEN: " <+> fen)
      <=> str " "
      <=> hBox [str "Board style (+/- to change): ", style]
      <=> hBox [str "Up/Down (kj) = change ply, Left/Right (hl) = back/forward"
               , hCenter $ str " "
               , str "ESC (q) = Quit"
               ]
    eco = maybe (str " ") drawECO (ECO.lookup (position st) defaultECO)
    drawECO co = borderWithLabel (str "ECO " <+> txt (coCode co)) $
      case coVariation co of
        Nothing        -> txtWrap (coName co)
        Just variation -> txtWrap (coName co) <=> txtWrap variation
    style = vLimit 1 $ L.renderList drawStyle True (st^.boardStyle)
    drawStyle foc (n, _) = putCursorIf foc BoardStyle (0,0) $ str n
    selectedStyle = maybe english (snd . snd) $
      st^.boardStyle & L.listSelectedElement
    list = L.renderList (drawPly (previousPosition st)) True (plyList st)
    drawPly p foc = putCursorIf foc List (0,0)
                  . withAttrIf foc selectedAttr
                  . str . toSAN p
    board = renderPosition (position st) (color (previousPosition st)) (Just . targetSquare $ st) selectedStyle
    var = txtWrap . varToSAN (st^.initialPosition) $ st^.treePos & TreePos.label & toList
    fen = str . toFEN $ position st
  appHandleEvent (VtyEvent e) = fromMaybe (pure ()) $ Map.lookup e keyMap
  appHandleEvent _            = pure ()
  appAttrMap = const $ attrMap Vty.defAttr
             [(selectedAttr, Vty.white `on` Vty.green)
             ]
  appChooseCursor = F.focusRingCursor (view focusRing)

loadForest :: (Position -> [Tree Ply]) -> Position -> St -> Maybe St
loadForest f p st = case f p of
  [] -> Nothing
  ts -> Just $ st & initialPosition .~ p & treePos .~ tp where
    tp = fromJust . nextTree . fromForest $ pathTree <$> ts

pathTree :: Tree a -> Tree (NonEmpty a)
pathTree = foldTree $ \a -> Node (pure a) . (fmap . fmap) (NonEmpty.cons a)

main :: IO ()
main = do
  as <- getArgs
  case as of
    [] -> void $ defaultMain cbookview initialState
    [fp] -> case takeExtension fp of
      ".bin" -> do
        book <- readPolyglotFile fp
        case loadForest (bookForest book) startpos initialState of
          Just st -> void $ defaultMain cbookview st
          Nothing -> putStrLn "No moves found in book"
      ".pgn" -> readPGNFile fp >>= \case
        Right pgn -> case loadForest (const $ pgnForest pgn) startpos initialState of
          Just st -> void $ defaultMain cbookview st
          Nothing -> putStrLn "No moves found in PGN"
        Left err -> putStrLn err
      ext -> putStrLn $ "Unknown extension " <> ext <> ", only .bin (polyglot) and .pgn is supposed"
    _ -> putStrLn "Too many arguments."
