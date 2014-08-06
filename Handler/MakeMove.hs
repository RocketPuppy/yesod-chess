module Handler.MakeMove where

import Import
import Data.Monoid
import Control.Monad
import Data.Time
import Data.Maybe

--TODO validate turns here
postMakeMoveR :: GameId -> Handler Html
postMakeMoveR gameId = do
    sessionId <- lookupSession "userId"
    (game, user, team, gameMoves) <- runDB $ do
        game@(Game user1_id user2_id _) <- get404 gameId
        moves <- selectList [MoveGameId ==. gameId] [Asc MoveCreated]
        user1 <- getJust user1_id
        user2 <- case user2_id of
            Just user2_id -> get user2_id
            Nothing -> return Nothing
        let isUser1 = (==) <$> sessionId <*> pure (userIdent user1)
        let isUser2 = (==) <$> sessionId <*> fmap userIdent user2
        let user1' = join $ bool <$> pure Nothing <*> pure (Just user1_id) <*> isUser1
        let user2' = join $ bool <$> pure Nothing <*> pure (user2_id) <*> isUser2
        let team = getFirst $ First (user1' >> pure White) <> First (user2' >> pure Black)
        let user = getFirst $ First user1' <> First user2'
        return (game, user, team, moves)
    case user of
        Nothing -> defaultLayout $ redirect HomeR
        Just user -> do
            time <- liftIO getCurrentTime
            result <- runInputPostResult $ Move <$> ireq hiddenField "from" <*> ireq hiddenField "to" <*> pure user <*> pure gameId <*> pure time
            let moves' = map ((moveStart_position &&& moveEnd_position) . entityVal) gameMoves
            let board = foldl processMove initialBoard moves'
            case result of
                FormSuccess move@(Move start end _ _ _) ->
                    case getPieceAt board start of
                        Nothing -> defaultLayout $ redirect $ GameR gameId
                        Just piece ->
                            let isValidTurn = lastTeamIs (otherTeam . chessPieceTeam $ piece) board moves'
                                edgeMoves' = edgeMoves piece board moves'
                                validMoves = join . mapMaybe id $ [Just (moves board piece), edgeMoves'] in
                            if isValidTurn && elem end validMoves
                                then do runDB $ insert move
                                        defaultLayout $ redirect $ GameR gameId
                                else defaultLayout $ redirect $ GameR gameId
                _ -> defaultLayout $ redirect HomeR
