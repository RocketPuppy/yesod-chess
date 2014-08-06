{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Data.Time

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    (formWidget, formEnctype) <- generateFormPost startForm
    defaultLayout $ do
        setTitle "Welcome to ChessRoulette!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, _), _) <- runFormPost startForm
    case result of
        FormSuccess user -> do
            time <- liftIO getCurrentTime
            game <- runDB $ do -- one transaction
                userId <- insert $ user time
                game <- selectFirst [GameUser2_Id ==. Nothing] []
                case game of
                    Just game -> update (entityKey game) [GameUser2_Id =. Just userId] >> return (entityKey game)
                    Nothing -> insert $ Game userId Nothing time
            setSession "userId" (userIdent (user time))
            defaultLayout $ redirect $ GameR game
        _ -> defaultLayout $ redirect HomeR

startForm = renderDivs $ User
    <$> areq textField "Your name" Nothing
