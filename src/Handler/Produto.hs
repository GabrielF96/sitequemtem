{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Database.Persist.Postgresql

formProduto :: UserId -> Form Produto
formProduto uid = renderBootstrap $ Produto
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Descrição do Produto: " Nothing
    <*> areq doubleField "Valor do aluguel mensal: " Nothing
    <*> areq (selectField tipoLista) "Tipo do Produto: " Nothing
    <*> pure uid
    
tipoLista = do
    entidades <- runDB $ selectList [] [Asc TipoNmtipo]
    optionsPairs $ fmap (\ent -> (tipoNmtipo $ entityVal ent, entityKey ent)) entidades
    
getProdutoR :: UserId -> Handler Html
getProdutoR uid = do
    msg <- getMessage
    (widget,enctype) <- generateFormPost (formProduto uid)
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{ProdutoR uid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" value="Cadastrar">
        |]
        
postProdutoR :: UserId -> Handler Html
postProdutoR uid = do
    ((res,_),_) <- runFormPost (formProduto uid)
    case res of
        FormSuccess produto -> do
            _ <- runDB $ insert produto
            setMessage [shamlet|
                <h1>
                    PRODUTO CADASTRADO COM SUCESSO!
            |]
            redirect ProdutoR
        _ -> redirect HomeR
    