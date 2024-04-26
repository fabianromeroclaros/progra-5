module AbstractGrammar where

type Strings = String
type Paragraph = String
type H1 = String
type H2 = String
type H3 = String
type H4 = String
type H5 = String
type H6 = String

type Image = String
type Code = String

data Headers = H1 | H2 | H3 | H4 | H5 | H6 deriving Show

data Slides = Slides [Slide]
    deriving Show

data Slide = Slide Titles Themes BgImage BodySlide
    deriving Show

data Titles = TitleSlide Strings | BigTitleSlide Strings
    deriving Show

data Themes = LightTheme | DarkTheme
    deriving Show

data BgImage = BgImage Strings | EmptyBgImage
    deriving Show

data Cad = Cad Strings
    deriving Show

data BodySlide = BodySlide [MarkdownBlock]
    deriving Show

data List = BulletPoint | NumberPoint
    deriving Show

data MarkdownBlock = MdParagraph Paragraph
                   | AutoLink Strings  
                   | HeaderBlock Headers Strings
                   | EmphasizeBlock Emphasize Strings 
                   | ListBlock List String
                   | Image String
                   | Code String
                   | LineBreak
        deriving Show

data Emphasize = Bold | Italic
        deriving Show

