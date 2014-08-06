module Handler.Moves where

import Import
import Types
import Data.Maybe

getMovesR :: GameId -> Row -> Column -> Handler Html
getMovesR gameId row col = do
    (game, user1, user2, moves) <- runDB $ do
        game@(Game user1_id user2_id _) <- get404 gameId
        moves <- selectList [MoveGameId ==. gameId] [Asc MoveCreated]
        user1 <- getJust user1_id
        user2 <- case user2_id of
            Just user2_id -> get user2_id
            Nothing -> return Nothing
        return (game, user1, user2, moves)
    sessionId <- lookupSession "userId"
    let isUser1 = (==) <$> sessionId <*> pure (userIdent user1)
    let isUser2 = (==) <$> sessionId <*> (fmap userIdent user2)
    let moves' = map ((moveStart_position &&& moveEnd_position) . entityVal) moves
    let board = foldl processMove initialBoard moves'
    let piece = getPieceAt board (ChessSquare row col)
    let isWhite = (==) <$> pure White <*> fmap chessPieceTeam piece
    let isBlack = (==) <$> pure Black <*> fmap chessPieceTeam piece
    let user1AndWhite = (&&) <$> isUser1 <*> isWhite
    let user2AndBlack = (&&) <$> isUser2 <*> isBlack
    let team = chessPieceTeam <$> piece
    let king = join $ getKing <$> team <*> pure board
    let movesForPiece = Types.moves <$> pure board <*> piece
    let boards = map <$> pure (processMove board . (,) (ChessSquare row col)) <*> movesForPiece
    let kings = map <$> pure (fmap chessPieceSquare . join . (<*>) (getKing <$> team) . pure) <*> boards
    let movesForPiece' = mapMaybe <$> pure (\(m,b,k) -> case (isThreatened <$> fmap otherTeam team <*> pure b <*> k) of { Just False -> Just m; _ -> Nothing}) <*> (zip3 <$> movesForPiece <*> boards <*> kings)
    let edgeMoves' = join $ edgeMoves <$> piece <*> pure board <*> pure moves'
    let movesForPiece'' = join . mapMaybe id $ [movesForPiece', edgeMoves']
    case (||) <$> user1AndWhite <*> user2AndBlack of
        Nothing -> redirect $ GameR gameId
        Just id -> defaultLayout $ do
            lift (liftIO (putStrLn "\n\n\n"))
            lift (liftIO (putStrLn (show board)))
            lift (liftIO (putStrLn "\n"))
            lift (liftIO (putStrLn (show piece)))
            lift (liftIO (putStrLn "\n"))
            lift (liftIO (putStrLn (show movesForPiece')))
            lift (liftIO (putStrLn "\n"))
            lift (liftIO (putStrLn (show edgeMoves')))
            lift (liftIO (putStrLn "\n"))
            lift (liftIO (putStrLn (show moves')))
            lift (liftIO (putStrLn "\n\n\n"))
            let content = contentFn board (fromJust piece) movesForPiece'' gameId
            setTitle $ toHtml $ userIdent user1 <> " vs. " <> maybe "Nobody" userIdent user2
            $(widgetFile "moves")

moveForm from to = renderDivs $
    Move <$> areq hiddenField (nameSettings "from") (pure from)
         <*> areq hiddenField (nameSettings "to") (pure to)
    where nameSettings n = FieldSettings "" Nothing Nothing (Just n) []

-- If this square is in the valid moves, color it differently and make a post form for it
contentFn board piece movesForPiece gameId row col =
    if elem square movesForPiece then
        [whamlet|
          <div .move>
            <form method=post action=@{MakeMoveR gameId}>
                ^{generateFormPost (moveForm (chessPieceSquare piece) square) >>= fst}
        |]
    else
        contentSpan board row col
    where square = ChessSquare row col
