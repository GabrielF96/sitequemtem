{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Aluguel where

import Import
import Database.Persist.Postgresql

formAluguel :: UsuarioId -> ProdutoId -> Form Aluguel
formAluguel cid pid = renderBootstrap $ Aluguel
    <$> pure pid
    <*> pure cid
    <*> areq textareaField "Quanto tempo deseja ficar com o produto?" Nothing
    <*> areq textareaField "Defina seu interesse no produto: " Nothing
    <*> areq textareaField "Escreva seu contato: " Nothing
    
getAluguelR :: UsuarioId -> ProdutoId -> Handler Html
getAluguelR cid pid = do 
    _ <- runDB $ get404 cid
    _ <- runDB $ get404 pid
    msg <- getMessage
    (widget,enctype) <- generateFormPost (formAluguel cid pid)
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{AluguelR cid pid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" class="w3-button w3-black w3-section" value="Cadastrar">
        |]