{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.User where

import Import
import Database.Persist.Postgresql

formUser :: Form (User, Text)
formUser = renderBootstrap $ (,)
    <$> (User
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing
            <*> areq passwordField "Senha: " Nothing
            <*> areq textField "CPF: " Nothing
            <*> areq textField "Telefone: " Nothing
            <*> areq textField "Cidade: " Nothing
            <*> areq textField "Estado " Nothing
            <*> areq textField "Endereço: " Nothing)
    <*> areq passwordField "Confirmação: " Nothing
    
getUserR :: Handler Html
getUserR = do
    msg <- getMessage
    (widget,enctype) <- generateFormPost formUser
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{UserR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
postUserR :: Handler Html
postUserR = do
    ((res,_),_) <- runFormPost formUser
    case res of
        FormSuccess (usr,confirmacao) -> do
            if confirmacao == (userSenha usr) then do
                runDB $ insert usr
                setMessage [shamlet|
                    <h1>
                        USUARIO CADASTRADO!
                |]
                redirect UserR
            else do
                setMessage [shamlet|
                    <h1>
                        CONFIRMACAO INCORRETA!
                |]
                redirect UserR
        _ -> redirect UserR