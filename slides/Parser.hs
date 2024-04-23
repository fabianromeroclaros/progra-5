module Parser where

import AbstractGrammar
import Scanner
import UU.Parsing

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

pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarkdownBlock <* pEndBlock "}"


pMarkdownBlock :: Parser Token MarkdownBlock
pMarkdownBlock = MdParagraph <$> pStrings
    <|> pAutoLinkBlock
    <|> HeaderBlock H1 <$ pKeyword "#" <*> pStrings

pAutoLinkBlock :: Parser Token MarkdownBlock
pAutoLinkBlock = pURL

pURL :: Parser Token MarkdownBlock
pURL = AutoLinkBlock URL <$ pKeyword "<" <*> pStrings <* pKeyword ">"

pEMAIL :: Parser Token MarkdownBlock
pEMAIL = AutoLinkBlock URL <$ pKeyword "<" <*> pStrings <* pKeyword ">"

pEmphasizeBlock :: Parser Token MarkdownBlock
pEmphasizeBlock = pItalicBlock <|> pBoldBlock

pBoldBlock :: Parser Token MarkdownBlock
pBoldBlock = EmphasizeBlock Bold <$ (pKeyword "**" <|> pKeyword "__") <*> pStrings <* (pKeyword "**" <|> pKeyword "__")

pItalicBlock :: Parser Token MarkdownBlock
pItalicBlock = EmphasizeBlock Italic <$ (pKeyword "*" <|> pKeyword "_") <*> pStrings <* (pKeyword "*" <|> pKeyword "_")
