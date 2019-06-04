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
        $(whamletFile "templates/aluguel.hamlet")
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{AluguelR cid pid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" class="w3-button w3-black w3-section" value="CADASTRAR">
            <!-- Footer -->
                <footer class="w3-center w3-black w3-padding-16">
                    <p>Powered by <a href="http://fatecrl.edu.br" title="FATEC RUBENS LARA" target="_blank" class="w3-hover-text-green">FATEC</a>
        |]
        
postAluguelR :: UsuarioId -> ProdutoId -> Handler Html
postAluguelR cid pid = do
    ((res,_),_) <- runFormPost (formAluguel cid pid)
    case res of 
        FormSuccess aluguel -> do 
            aluid <- runDB $ insert aluguel
            redirect (HomeR cid)
        _ -> redirect (AluguelR cid pid) 