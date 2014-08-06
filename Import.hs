module Import
    ( module Import
    ) where

import           Prelude              as Import hiding (head, init, last,
                                                 readFile, tail, writeFile)
import           Yesod                as Import hiding (Route (..))

import           Control.Applicative  as Import (pure, (<$>), (<*>))
import           Data.Text            as Import (Text)

import           Foundation           as Import
import           Model                as Import
import           Settings             as Import
import           Settings.Development as Import
import           Settings.StaticFiles as Import
import           Nats                 as Import
import           Types                as Import
import           Instances            as Import
import           Data.List            as Import (genericTake)
import           Control.Arrow        as Import hiding (app)
import           Data.Maybe           as Import
import           Safe                 as Import
import           Control.Monad        as Import

#if __GLASGOW_HASKELL__ >= 704
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat),
                                                 (<>))
#else
import           Data.Monoid          as Import
                                                 (Monoid (mappend, mempty, mconcat))

infixr 5 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
#endif

bool :: a -> a -> Bool -> a
bool _ e True = e
bool e _ False = e

-- Template helpers
type ContentFunction = Row -> Column -> WidgetT App IO ()
chessSquare :: Row -> Column -> ContentFunction -> WidgetT App IO ()
chessSquare row col content = $(widgetFile "chessSquare")
    where color = if (toInteger row + toInteger col) `mod` 2 == 0 then "white" else "black" :: Text

chessRow :: Row -> ContentFunction -> WidgetT App IO ()
chessRow row content = $(widgetFile "chessRow")

chessBoard :: ContentFunction -> WidgetT App IO ()
chessBoard content = $(widgetFile "chessBoard")

-- TODO fix this
rowIndices = reverse . genericTake boardHeight $ [0..] :: [Row]
columnIndices = genericTake boardWidth [0..] :: [Column]

isWhite sessionId user1Id board row col = (&&) <$> ((==) <$> sessionId <*> pure user1Id) <*> ((==) <$> pieceTeam board row col <*> pure White)

isBlack sessionId user2Id board row col = (&&) <$> ((==) <$> sessionId <*> user2Id) <*> ((==) <$> pieceTeam board row col <*> pure Black)

contentSpan board row col = [whamlet| <span>#{spanContent board row col } |]

piece board row col = getPieceAt board (ChessSquare row col)

pieceTeam board row col = chessPieceTeam <$> piece board row col

spanContent board row col = case piece board row col of
    Just (ChessPiece Pawn _ Black) -> preEscapedToMarkup ("&#9823;" :: Text)
    Just (ChessPiece Knight _ Black) -> preEscapedToMarkup ("&#9822;" :: Text)
    Just (ChessPiece Bishop _ Black) -> preEscapedToMarkup ("&#9821;" :: Text)
    Just (ChessPiece Rook _ Black) -> preEscapedToMarkup ("&#9820;" :: Text)
    Just (ChessPiece Queen _ Black) -> preEscapedToMarkup ("&#9819;" :: Text)
    Just (ChessPiece King _ Black) -> preEscapedToMarkup ("&#9818;" :: Text)
    Just (ChessPiece Pawn _ White) -> preEscapedToMarkup ("&#9817;" :: Text)
    Just (ChessPiece Knight _ White) -> preEscapedToMarkup ("&#9816;" :: Text)
    Just (ChessPiece Bishop _ White) -> preEscapedToMarkup ("&#9815;" :: Text)
    Just (ChessPiece Rook _ White) -> preEscapedToMarkup ("&#9814;" :: Text)
    Just (ChessPiece Queen _ White) -> preEscapedToMarkup ("&#9813;" :: Text)
    Just (ChessPiece King _ White) -> preEscapedToMarkup ("&#9812;" :: Text)
    Nothing -> ""

lastTeamWhite = lastTeamIs White

lastTeamBlack = lastTeamIs Black

lastTeamIs team board moves = fromMaybe (team == Black && length moves == 0) con
    where con = (==) <$> pure team <*> lastMoveTeam board moves

lastMoveTeam board moves = join $ pieceTeam board <$> row <*> col
    where lastMove = lastMay moves
          row = (chessSquareRow . snd) <$> lastMove
          col = (chessSquareColumn . snd) <$> lastMove
