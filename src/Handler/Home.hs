{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Home where

import Import
import Text.Lucius
import Text.Julius
import Database.Persist.Postgresql

getHomeR :: UsuarioId -> Handler Html
getHomeR cid = do
    sess <- lookupSession "_ID"
    _ <- runDB $ get404 cid
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/home.hamlet")
        toWidget $(luciusFile "templates/home.lucius")
        toWidgetHead $(juliusFile "templates/home.julius")
        
