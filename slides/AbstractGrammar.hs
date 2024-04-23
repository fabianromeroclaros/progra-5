module AbstractGrammar where

type Strings = String
type Paragraph = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String

data Headers = H1 | H2 | H3 | H4 | H5 | H6 deriving Show

data Slides = Slides [Slide]
    deriving Show

data Slide = Slide TitleSlide BodySlide
    deriving Show

data TitleSlide = TitleSlide Strings
    deriving Show

data Cad = Cad Strings
    deriving Show

data BodySlide = BodySlide [MarkdownBlock]
    deriving Show

data MarkdownBlock = MdParagraph Paragraph
                   | AutoLinkBlock AutoBlock Strings  
                   | HeaderBlock Headers Strings
                   | EmphasizeBlock Emphasize Strings 
        deriving Show

data Emphasize = Bold | Italic
        deriving Show

data AutoBlock = Email | URL 
        deriving Show

