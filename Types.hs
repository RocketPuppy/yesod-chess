{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types where

import Nats
import Prelude
import Data.Either
import Data.List
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Safe
import Control.Applicative

boardSize = (boardWidth, boardHeight) :: (Column, Row)
boardWidth = 8 :: Column
boardHeight = 8 :: Row

data Piece = Pawn | Bishop | Rook | Knight | King | Queen
  deriving (Show, Eq)
data ChessTeam = Black | White
  deriving (Eq, Show)
newtype Row = Row { runRow :: Natural }
  deriving (Eq, Show, Read, Num, Real, Integral, Enum, Ord)
newtype Column = Column { runColumn :: Natural }
  deriving (Eq, Show, Read, Num, Real, Integral, Enum, Ord)

otherTeam Black = White
otherTeam White = Black

data ChessPiece = ChessPiece
  { chessPiecePiece :: Piece
  , chessPieceSquare :: ChessSquare
  , chessPieceTeam :: ChessTeam
  }
  deriving (Show, Eq)
data ChessSquare = ChessSquare
  { chessSquareRow :: Row
  , chessSquareColumn :: Column
  }
  deriving (Eq, Show, Read)
type Board = [ChessPiece]

mkChessPiece :: (Piece, (Column, Row), ChessTeam) -> ChessPiece
mkChessPiece (p, (col, row), t) = ChessPiece p (ChessSquare row col) t

initialBoard :: Board
initialBoard = map mkChessPiece $ white <> black
    where white = map (\(p, colrow) -> (p, colrow, White)) $ mkRow 0 powerRow <> mkRow 1 pawnRow
          black = map (\(p, colrow) -> (p, colrow, Black)) $ mkRow (boardHeight-1) powerRow <> mkRow (boardHeight-2) pawnRow
          mkRow row = map (\(p, col) -> (p, (col, row)))
          powerRow = [(Rook, 0), (Knight, 1), (Bishop, 2), (Queen, 3), (King, 4), (Bishop , 5), (Knight, 6), (Rook, 7)]
          pawnRow = [(Pawn, col) | col <- genericTake boardWidth [0..]]

forward :: ChessSquare -> ChessSquare
forward (ChessSquare row col) = ChessSquare row' col
    where row' = if row+1 < boardHeight then row+1 else row

backward :: ChessSquare -> ChessSquare
backward (ChessSquare row col) = ChessSquare (row-1) col

leftward :: ChessSquare -> ChessSquare
leftward (ChessSquare row col) = ChessSquare row (col-1)

rightward :: ChessSquare -> ChessSquare
rightward (ChessSquare row col) = ChessSquare row col'
    where col' = if col+1 < boardWidth then col+1 else col

pieceAt :: Board -> ChessSquare -> Maybe ChessPiece
pieceAt board pos = find ((pos ==) . chessPieceSquare) board

squareIfAnyTeamIsOnIt :: Board -> ChessSquare -> Maybe ChessSquare
squareIfAnyTeamIsOnIt board pos = foldl1 (>>) . map (flip (squareIfPieceIsOnIt board) pos) $ [White, Black]

squareIfAnyTeamIsNotOnIt :: Board -> ChessSquare -> Maybe ChessSquare
squareIfAnyTeamIsNotOnIt board pos = foldl1 (>>) . map (flip (squareIfPieceIsNotOnIt board) pos) $ [White, Black]

squareIfPieceIsOnIt :: Board -> ChessTeam -> ChessSquare -> Maybe ChessSquare
squareIfPieceIsOnIt board team pos = fmap chessPieceSquare $ find pred board
    where pred = uncurry (&&) . ((pos ==) . chessPieceSquare &&& (team ==) . chessPieceTeam)

squareIfPieceIsNotOnIt :: Board -> ChessTeam -> ChessSquare -> Maybe ChessSquare
squareIfPieceIsNotOnIt board team pos =
    case (find pred board) of
        Just _ -> Nothing
        Nothing ->  Just pos
    where pred = uncurry (&&) . ((pos ==) . chessPieceSquare &&& (team ==) . chessPieceTeam)

isDiagonal :: ChessSquare -> ChessSquare -> Bool
isDiagonal square1 square2 = (getRowNat square1 `diffNat` getRowNat square2) == (getColumnNat square1 `diffNat` getColumnNat square2)
    where
        getRowNat = runRow . chessSquareRow
        getColumnNat = runColumn . chessSquareColumn

isKnight :: ChessSquare -> ChessSquare -> Bool
isKnight square1 square2 = (getRowNat square1 `diffNat` getRowNat square2) + (getColumnNat square1 `diffNat` getColumnNat square2) == 3
    where
        getRowNat = runRow . chessSquareRow
        getColumnNat = runColumn . chessSquareColumn

skipSquare :: ChessSquare -> [ChessSquare] -> [ChessSquare]
skipSquare sq = filter (sq /=)

blackPawnRow = 6 :: Row
whitePawnRow = 1 :: Row

--TODO clean this up, there's lots of duplication
moves :: Board -> ChessPiece -> [ChessSquare]
moves board (ChessPiece Pawn pos team) = catMaybes $ capturing <> potential
    where capturing = map (squareIfPieceIsOnIt board (otherTeam team)) . map ($ pos) . map (direction . ) $ [leftward, rightward]
          potential = (squareIfAnyTeamIsNotOnIt board (direction pos)):secondSquare
          secondSquare = if onFirstSquare then [squareIfAnyTeamIsNotOnIt board (direction . direction $ pos)] else []
          onFirstSquare = any id $ zipWith (&&) [team == White, team == Black] [(whitePawnRow == row), (blackPawnRow == row)]
          row = chessSquareRow pos
          direction = case team of
            White -> forward
            Black ->  backward

moves board (ChessPiece Bishop pos team) = nub $ potential <> captures
    where potential =  concatMap (takeTilTheirTeam . takeTilMyTeam) possibleMoves
          captures =  catMaybes . map (headMay . catMaybes . map (squareIfPieceIsOnIt board (otherTeam team)) . takeTilMyTeam) $ possibleMoves
          possibleMoves = map (skipSquare pos . takeDiagonals . flip iterate pos) [forward.leftward, forward.rightward, backward.leftward, backward.rightward]
          takeDiagonals = takeWhile (isDiagonal pos)
          takeTilMyTeam = takeWhile (isJust . squareIfPieceIsNotOnIt board team)
          takeTilTheirTeam = takeWhile (isJust . squareIfPieceIsNotOnIt board (otherTeam team))

moves board (ChessPiece Rook pos team) = nub $ potential <> capturing
    where potential =  nub . concatMap (takeTilTheirTeam . takeTilMyTeam) $ possibleMoves
          capturing = catMaybes . map (headMay . catMaybes . map (squareIfPieceIsOnIt board (otherTeam team)) . takeTilMyTeam) $ possibleMoves
          possibleMoves = map (skipSquare pos . takeOnBoard . flip iterate pos) [forward, backward, leftward, rightward]
          takeOnBoard = take (fromIntegral $ toInteger boardWidth)
          takeTilMyTeam = takeWhile (isJust . squareIfPieceIsNotOnIt board team)
          takeTilTheirTeam = takeWhile (isJust .  squareIfPieceIsNotOnIt board (otherTeam team))

moves board (ChessPiece Knight pos team) = potential
    where potential = filter (\p -> isKnight pos p && (hasTheirs p || not (hasOurs p))) possibleMoves
          hasTheirs = isJust . squareIfPieceIsOnIt board (otherTeam team)
          hasOurs = isJust . squareIfPieceIsOnIt board team
          possibleMoves = map ($ pos) [ forward.forward.leftward
                                      , forward.forward.rightward
                                      , backward.backward.leftward
                                      , backward.backward.rightward
                                      , leftward.leftward.forward
                                      , leftward.leftward.backward
                                      , rightward.rightward.forward
                                      , rightward.rightward.backward ]

moves board (ChessPiece Queen pos team) = nub $ potential <> capturing
    where potential =  concatMap (takeTilTheirTeam . takeTilMyTeam) possibleMoves
          capturing = catMaybes . map (headMay . catMaybes . map (squareIfPieceIsOnIt board (otherTeam team)) . takeTilMyTeam) $ possibleMoves
          possibleMoves = map (skipSquare pos) $ possibleDiagMoves <> possibleStraightMoves
          possibleDiagMoves = map (takeDiagonals . flip iterate pos) [forward.leftward, forward.rightward, backward.leftward, backward.rightward]
          possibleStraightMoves = map (takeOnBoard . flip iterate pos) [forward, backward, leftward, rightward]
          takeDiagonals = takeWhile (isDiagonal pos)
          takeOnBoard = take (fromIntegral $ toInteger boardWidth)
          takeTilMyTeam = takeWhile (isJust . squareIfPieceIsNotOnIt board team)
          takeTilTheirTeam = takeWhile (isJust . squareIfPieceIsNotOnIt board (otherTeam team))

moves board (ChessPiece King pos team) = nub . skipSquare pos $ potential
    where potential = filter (\p -> (hasTheirs p || not (hasOurs p))) possibleMoves
          hasTheirs = isJust . squareIfPieceIsOnIt board (otherTeam team)
          hasOurs = isJust . squareIfPieceIsOnIt board team
          possibleMoves = map ($ pos) [ forward
                                      , backward
                                      , rightward
                                      , leftward
                                      , forward.leftward
                                      , forward.rightward
                                      , backward.leftward
                                      , backward.rightward ]

piecesForTeam :: ChessTeam -> Board -> [ChessPiece]
piecesForTeam team = filter ((team==) . chessPieceTeam)

isThreatened :: ChessTeam -> Board -> ChessSquare -> Bool
isThreatened team board sq = isJust . find (sq==) . concatMap (moves board) $ (piecesForTeam team board)

getPieceAt :: Board -> ChessSquare -> Maybe ChessPiece
getPieceAt board square = find ((square ==) . chessPieceSquare) board

getKing :: ChessTeam -> Board -> Maybe ChessPiece
getKing team = find (\(ChessPiece p s t) -> p == King && t == team)

movePiece :: Board -> (ChessSquare, ChessSquare) -> Board
movePiece board (from, to) = map moveIt board
    where moveIt p@(ChessPiece _ sq _) = if sq==from then p{chessPieceSquare=to} else p

processMove :: Board -> (ChessSquare, ChessSquare) -> Board
processMove board move@(from, to) = promotion
    where pieceT = getPieceAt board to
          pieceF = getPieceAt board from
          board' =
            case pieceT of
                Nothing -> movePiece board move
                Just p -> movePiece (filter (p /=) board) move
          promotion = promotePawns castle
          castle =
            if isCastle board from to then
                if chessSquareColumn from > chessSquareColumn to then
                    movePiece board' (ChessSquare (chessSquareRow to) 0, ChessSquare (chessSquareRow to) (chessSquareColumn to + 1))
                else
                    movePiece board' (ChessSquare (chessSquareRow to) 7, ChessSquare (chessSquareRow to) (chessSquareColumn to - 1))
            else
                board'

promotePawns board = map helper board
    where
        helper piece = case piece of
            (ChessPiece Pawn s@(ChessSquare 7 _) White) -> ChessPiece Queen s White
            (ChessPiece Pawn s@(ChessSquare 0 _) Black) -> ChessPiece Queen s Black
            _ -> piece

pieceMoved :: Piece -> Column -> ChessTeam -> [(ChessSquare, ChessSquare)] -> Bool
pieceMoved piece col team moves = any id . map (not . (==) piece' . flip getPieceAt pos) $ boards
    where boards = scanl processMove initialBoard moves
          pos = case team of
            White -> ChessSquare whiteRow col
            Black -> ChessSquare blackRow col
          whiteRow = if piece == Pawn then 1 else 0
          blackRow = if piece == Pawn then 6 else 7
          piece' = Just $ ChessPiece piece pos team

isCastle board from@(ChessSquare _ fcol) (ChessSquare _ tcol) =
    case chessPiecePiece <$> getPieceAt board from of
        Just King -> runColumn fcol `diffNat` runColumn tcol == 2
        _ -> False

kingMoved = pieceMoved King 4

kingRookMoved = pieceMoved Rook 7

queenRookMoved = pieceMoved Rook 0

queenCastlePathClear board row = [] == (catMaybes . map (getPieceAt board) . map (ChessSquare row) $ [1,2,3])

kingCastlePathClear board row = [] == (catMaybes . map (getPieceAt board) . map (ChessSquare row) $ [5, 6])

castleMoves :: ChessTeam -> Board -> [(ChessSquare, ChessSquare)] -> [ChessSquare]
castleMoves team board moves = filter (isNothing . getPieceAt board) . catMaybes $ [queenSide, kingSide]
    where
        queenSide = if queenCastlePathClear board row && not (kingMoved team moves || queenRookMoved team moves) then Just (ChessSquare row 2) else Nothing
        kingSide = if kingCastlePathClear board row && not (kingMoved team moves || kingRookMoved team moves) then Just (ChessSquare row 6) else Nothing
        row = case team of
            White -> 0
            Black -> 7

edgeMoves (ChessPiece piece _ team) board moves =
    case piece of
        King -> Just $ castleMoves team board moves
        _ -> Nothing
