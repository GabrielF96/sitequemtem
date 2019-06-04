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