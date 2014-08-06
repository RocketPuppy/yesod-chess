module Instances where

import Nats
import Prelude
import Control.Applicative
import Database.Persist.Class
import Database.Persist.Sql
import Types
import qualified Data.Text as T
import Web.PathPieces

instance PersistField Natural where
  toPersistValue = PersistInt64 . fromIntegral . natToInt
  fromPersistValue (PersistInt64 int) = Right . fromInteger . fromIntegral $ int

instance PersistFieldSql Natural where
  sqlType _ = SqlInt64

instance PersistField ChessSquare where
    toPersistValue (ChessSquare row col) = PersistText . T.pack . show $ (row, col)
    fromPersistValue (PersistText pair) = Right square
        where (row, col) = read . T.unpack $ pair
              square = ChessSquare row col
    fromPersistValue _ = Left (T.pack "incorrect value")

instance PersistFieldSql ChessSquare where
    sqlType _ = SqlString

instance PathMultiPiece ChessSquare where
    toPathMultiPiece (ChessSquare row col) = (T.pack . show . toInteger $ row) : [(T.pack . show . toInteger $ col)]
    fromPathMultiPiece (row:col:[]) = ChessSquare <$> text2Row row <*> text2Column col

instance PathPiece ChessSquare where
    toPathPiece = T.pack . show
    fromPathPiece s =
        case reads . T.unpack $ s of
            (square, ""):_ -> Just square
            [] -> Nothing

instance PathPiece Row where
    toPathPiece = T.pack . show . toInteger
    fromPathPiece = text2Row

instance PathPiece Types.Column where
    toPathPiece = T.pack . show . toInteger
    fromPathPiece = text2Column

text2Row s =
    case reads . T.unpack $ s of
        (i, ""):_
            | i < 0 -> Nothing
            | otherwise -> Just . Row . fromInteger $ i
        [] -> Nothing

text2Column s =
    case reads . T.unpack $ s of
        (i, ""):_
            | i < 0 -> Nothing
            | otherwise -> Just . Types.Column . fromInteger $ i
        [] -> Nothing
