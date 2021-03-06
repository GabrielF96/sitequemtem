{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE QuasiQuotes #-}
module Handler.Produto where

import Import
import Database.Persist.Postgresql

formProduto :: UsuarioId -> Form (Text, Text, Double, TipoId, UsuarioId, FileInfo)
formProduto cid = renderBootstrap $ (,,,,,)
    <$> areq textField "Nome: " Nothing
    <*> areq textField "Descrição do Produto: " Nothing
    <*> areq doubleField "Valor do aluguel mensal: " Nothing
    <*> areq (selectField tipoLista) "Tipo do Produto: " Nothing
    <*> pure cid
    <*> areq fileField 
        FieldSettings{fsId=Just "hident1",
                      fsLabel="Arquivo: ",
                      fsTooltip= Nothing,
                      fsName= Nothing,
                      fsAttrs=[("accept","image/jpeg")]} 
        Nothing
    
tipoLista = do
    entidades <- runDB $ selectList [] [Asc TipoNmtipo]
    optionsPairs $ fmap (\ent -> (tipoNmtipo $ entityVal ent, entityKey ent)) entidades
    
getProdutoR :: UsuarioId -> Handler Html
getProdutoR cid = do
    _ <- runDB $ get404 cid
    msg <- getMessage
    (widget,enctype) <- generateFormPost (formProduto cid)
    defaultLayout $ do
        addStylesheet $ StaticR css_bootstrap_css
        $(whamletFile "templates/mprodutos.hamlet")
        [whamlet|
            $maybe mensagem <- msg
                ^{mensagem}
            <form action=@{ProdutoR cid} method=post enctype=#{enctype}>
                ^{widget}
                <input type="submit" class="w3-button w3-black w3-section" value="Cadastrar">
        |]
        
postProdutoR :: UsuarioId -> Handler Html
postProdutoR cid = do
    ((res,_),_) <- runFormPost (formProduto cid)
    case res of
        FormSuccess (nome, descricao, valor, tipo, cid, arq) -> do
            let img = unpack $ fileName arq
            liftIO $ fileMove arq ("static/" ++ img)
            _ <- runDB $ insert (Produto nome descricao valor tipo cid img)
            redirect (MeusProdutosR cid)
        _ -> redirect (HomeR cid)
        
getMeusProdutosR :: UsuarioId -> Handler Html
getMeusProdutosR cid = do
    _ <- runDB $ get404 cid
    todosMP <- runDB $ selectList [ProdutoCdusuario ==. cid] [Desc ProdutoCdusuario] 
    defaultLayout $(whamletFile "templates/produto.hamlet")
    
getOutrosProdutosR :: UsuarioId -> Handler Html
getOutrosProdutosR cid = do
    _ <- runDB $ get404 cid
    todosOP <- runDB $ selectList [ProdutoCdusuario !=. cid] [Asc ProdutoNmproduto]
    defaultLayout $(whamletFile "templates/produtoOutro.hamlet")

getPerfilProdR :: UsuarioId -> ProdutoId -> Handler Html
getPerfilProdR clid prid = do
    cliente <- runDB $ get404 clid
    produto <- runDB $ get404 prid
    defaultLayout $(whamletFile "templates/perfilproduto.hamlet")