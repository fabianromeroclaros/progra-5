module Parser where

import UU.Parsing 
import Scanner 
import AbstractGrammar

pSlides = Slides <$> pList pSlide

pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndSlide "---"

pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

instance Symbol Token

getValue :: Token -> String
getValue (Token _ v _ _) = v

tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)

tStr = getValue <$> pSym (Token String "" 0 0)

pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword

pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock

pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock

pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide

pStrings = tStr

pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList  pMarkdownBlock <* pEndBlock "}"

pMarkdownBlock = MdParagraph <$> pStrings
                <|> MdH1 <$ pKeyword "#" <*> pStrings

