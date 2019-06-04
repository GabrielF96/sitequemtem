{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Usuario where

import Import
import Database.Persist.Postgresql

formUsuario :: Form (Usuario, Text)
formUsuario = renderBootstrap $ (,)
    <$> (Usuario
            <$> areq textField "Nome: " Nothing
            <*> areq emailField "E-mail: " Nothing
            <*> areq passwordField "Senha: " Nothing
            <*> areq textField "CPF: " Nothing
            <*> areq textField "Telefone: " Nothing
            <*> areq textField "Cidade: " Nothing
            <*> areq (selectField listaEstado) "Estado " Nothing
            <*> areq textField "Endereço: " Nothing)
    <*> areq passwordField "Confirmação: " Nothing
   
listaEstado = do 
    entidades <- runDB $ selectList [] [Asc EstadoNome]
    optionsPairs $ fmap (\ent -> (estadoNome $ entityVal ent, entityKey ent)) entidades

getUsuarioR :: Handler Html
getUsuarioR = do
    msg <- getMessage
    (widget,enctype) <- generateFormPost formUsuario
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/usuario.hamlet")
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{UsuarioR} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" class="w3-button w3-black w3-section" value="CADASTRAR">
            <!-- Footer -->
            <footer class="w3-center w3-black w3-padding-16">
                <p>Powered by <a href="http://fatecrl.edu.br" title="FATEC RUBENS LARA" target="_blank" class="w3-hover-text-green">FATEC</a>    
            
            
        |]
        
postUsuarioR :: Handler Html
postUsuarioR = do
    ((res,_),_) <- runFormPost formUsuario
    case res of
        FormSuccess (usr,confirmacao) -> do
            if confirmacao == (usuarioSenha usr) then do
                runDB $ insert usr
                redirect LoginR
            else do
                setMessage [shamlet|
                    <h1>
                        CONFIRMACAO INCORRETA!
                |]
                redirect UsuarioR
        _ -> redirect UsuarioR