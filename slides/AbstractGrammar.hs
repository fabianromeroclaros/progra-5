module AbstractGrammar where

type Strings = String
type Paragraph = String
type H1 = String

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
                   | MdH1 H1
        deriving Show
{- 
data Paragraph = Paragraph Strings
    deriving Show -}
