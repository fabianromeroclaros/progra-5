module RenderHtml where
{-# LANGUAGE OverloadedStrings #-}

import Text.Blaze.Html5 as H
import Text.Blaze.Html.Renderer.String (renderHtml)

miPagina :: Html
miPagina = docTypeHtml $ do
    H.head $ do
        H.title $ toHtml "Mi Página"
    H.body $ do
        H.h1 $ toHtml "Hola mundo"
        H.p $ toHtml "Ejemplo"
        H.ul $ do
            H.li $ toHtml "Elemento 1"
            H.li $ toHtml "Elemento 2"
            H.li $ toHtml "Elemento 3"

pagina = renderHtml miPagina