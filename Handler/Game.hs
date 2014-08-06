{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Handler.Game where

import Import
import Data.Maybe
import Safe
import Control.Monad

getGameR :: GameId -> Handler Html
getGameR gameId = do
    (game, user1, user2, moves) <- runDB $ do
        game@(Game user1_id user2_id _) <- get404 gameId
        moves <- selectList [MoveGameId ==. gameId] [Asc MoveCreated]
        user1 <- getJust user1_id
        user2 <- case user2_id of
            Just user2_id -> get user2_id
            Nothing -> return Nothing
        return (game, user1, user2, moves)
    sessionId <- lookupSession "userId"
    let moves' = map ((moveStart_position &&& moveEnd_position) . entityVal) moves
    let board = foldl processMove initialBoard moves'
    let content = contentFn board moves' gameId (sessionId, userIdent user1, fmap userIdent user2)
    lift (liftIO $ print $ lastMoveTeam board moves')
    lift (liftIO $ print $ lastTeamWhite board moves')
    lift (liftIO $ print $ lastTeamBlack board moves')
    defaultLayout $ do
        setTitle $ toHtml $ userIdent user1 <> " vs. " <> maybe "Nobody" userIdent user2
        $(widgetFile "game")

linkContent board gameId row col = [whamlet| <a href=@{MovesR gameId row col}>^{contentSpan board row col} |]

contentFn board moves gameId (sessionId, user1Id, user2Id) row col =
    if isValidTurn
    then
        linkContent board gameId row col
    else
        contentSpan board row col
    where
        lastTeamBlackAndCurrentWhite = ((&&) <$> pure (lastTeamBlack board moves) <*> isWhite sessionId user1Id board row col)
        lastTeamWhiteAndCurrentBlack = ((&&) <$> pure (lastTeamWhite board moves) <*> isBlack sessionId user2Id board row col)
        isValidTurn = fromMaybe False $ (||) <$> lastTeamBlackAndCurrentWhite <*> lastTeamWhiteAndCurrentBlack
