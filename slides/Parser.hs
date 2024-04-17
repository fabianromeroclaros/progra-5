module Parser where

import UU.Parsing 
import Scanner 
import AbstractGrammar

pSlides = Slides <$> pList pSlide

pSlide = Slide <$> pTitleSlide <*> pBodySlide <* pEndBlock "---"

pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

getValue :: Token -> String
getValue (Token _ v _ _) = v

tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)

tStr = getValue <$> pSym (Token String "" 0 0)

pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword

pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock

pStrings = tStr

pBodySlide = BodySlide <$> pList  pMarkdownBlock

pMarkdownBlock = MdParagraph <$> pStrings
                <|> MdH1 <$ pKeyword "#" <*> pStrings

instance Symbol Token
