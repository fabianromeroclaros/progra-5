module Parser where
 
import UU.Parsing
import Scanner
    ( Token(..), Type(EndSlide, String, Keyword, OpenBlock, EndBlock) )
import AbstractGrammar
 
pSlides = Slides <$> pList pSlide
 
pSlide = Slide <$> (pTitleSlide <|> pBigTitleSlide <|> pure (TitleSlide "")) 
    <*> (pTheme <|> pure LightTheme) 
    <*> (pBgImage <|> pure EmptyBgImage) 
    <*> pBodySlide 
    <* pEndSlide "---"
 
pTitleSlide = TitleSlide <$ pKeyword "!" <*> pStrings

pEmptyTitle = return TitleSlide ""

pBigTitleSlide = BigTitleSlide <$ pKeyword "!!" <*> pStrings

pTheme = LightTheme <$ pKeyword "? Light"
        <|> DarkTheme <$ pKeyword "? Dark"

pBgImage = BgImage <$ pKeyword "|" <*> pStrings

pBodySlide = BodySlide <$ pOpenBlock "{" <*> pList pMarckdownBlock <* pEndBlock "}"  
 
--- Join Scanner with Parser
instance Symbol Token
 
getValue:: Token -> String
getValue (Token _ v _ _) = v
 
tSym :: Type -> String -> Parser Token String
tSym typ value = getValue <$> pSym (Token typ value 0 0)
 
tStr :: AnaParser [Token] Pair Token (Maybe Token) String
tStr = getValue <$> pSym (Token String "" 0 0)
 
pKeyword :: String -> Parser Token String
pKeyword = tSym Keyword
 
pOpenBlock :: String -> Parser Token String
pOpenBlock = tSym OpenBlock
 
pEndBlock :: String -> Parser Token String
pEndBlock = tSym EndBlock
 
pEndSlide :: String -> Parser Token String
pEndSlide = tSym EndSlide
 
pStrings :: Parser Token String
pStrings = tStr

pMarckdownBlock :: Parser Token MarkdownBlock
pMarckdownBlock = MdParagraph <$> pStrings
            <|> pHeaders
            <|> pLineBreak
            <|> pAutoLinkBlock
            <|> pEmphasizeBlock
            <|> pImage
            <|> pCode
            <|> pBulletPoint
            <|> pNumberPoint

pHeaders :: Parser Token MarkdownBlock
pHeaders = HeaderBlock H1 <$ pKeyword "#" <*> pStrings
               <|> HeaderBlock H2 <$ pKeyword "##" <*> pStrings
               <|> HeaderBlock H3 <$ pKeyword "###" <*> pStrings
               <|> HeaderBlock H4 <$ pKeyword "####" <*> pStrings
               <|> HeaderBlock H5 <$ pKeyword "#####" <*> pStrings
               <|> HeaderBlock H6 <$ pKeyword "######" <*> pStrings

pAutoLinkBlock :: Parser Token MarkdownBlock
pAutoLinkBlock = AutoLink <$ pKeyword "<" <*> pStrings <* pKeyword ">"

pEmphasizeBlock :: Parser Token MarkdownBlock
pEmphasizeBlock = pItalicBlock <|> pBoldBlock

pBoldBlock :: Parser Token MarkdownBlock
pBoldBlock = EmphasizeBlock Bold <$ (pKeyword "**" <|> pKeyword "__") <*> pStrings <* (pKeyword "**" <|> pKeyword "__")

pItalicBlock :: Parser Token MarkdownBlock
pItalicBlock = EmphasizeBlock Italic <$ (pKeyword "*" <|> pKeyword "_") <*> pStrings <* (pKeyword "*" <|> pKeyword "_")

pImage :: Parser Token MarkdownBlock
pImage = Image <$ pKeyword "[" <*> pStrings <* pKeyword "]"

pCode :: Parser Token MarkdownBlock
pCode = Code <$ pKeyword "`" <*> pStrings <* pKeyword "`"

pBulletPoint :: Parser Token MarkdownBlock
pBulletPoint = ListBlock BulletPoint <$ pKeyword "-" <*> pStrings

pNumberPoint :: Parser Token MarkdownBlock
pNumberPoint = ListBlock NumberPoint <$ pKeyword "%" <*> pStrings

pLineBreak :: Parser Token MarkdownBlock
pLineBreak = LineBreak <$ pKeyword "$"