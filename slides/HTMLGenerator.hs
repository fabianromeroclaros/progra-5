module HTMLGenerator where

import AbstractGrammar

generateHTML :: Slides -> String
generateHTML (Slides slides) = "<!Doctype HTMl/>\n<html>\n<body><div class=\"slides\">\n" ++ generateSlides slides ++ "</div>\n</body>\n</html>"

generateSlides :: [Slide] -> String
generateSlides slides = concatMap generateSlide slides

generateSlide :: Slide -> String
generateSlide (Slide titleSlide bodySlide) =
  "<div class=\"slide\">\n" ++ generateTitleSlide titleSlide ++ generateBodySlide bodySlide ++ "</div>\n"

generateTitleSlide :: TitleSlide -> String
generateTitleSlide (TitleSlide title) = "<h1 class=\"title\">" ++ title ++ "</h1>\n"

generateBodySlide :: BodySlide -> String
generateBodySlide (BodySlide blocks) = "<div class=\"content\">\n" ++ concatMap generateMarkdownBlock blocks ++ "</div>\n"

generateMarkdownBlock :: MarkdownBlock -> String
generateMarkdownBlock (MdParagraph text) = "<p>" ++ text ++ "</p>\n"
generateMarkdownBlock (AutoLinkBlock URL link) = "<a href=\"" ++ link ++ "\">" ++ link ++ "</a>\n"
generateMarkdownBlock (AutoLinkBlock Email email) = "<a href=\"" ++ "mailto:" ++ email ++ "\">" ++ email ++ "</a>\n"
generateMarkdownBlock (HeaderBlock header text) = "<h" ++ show (headerLevel header) ++ ">" ++ text ++ "</h" ++ show (headerLevel header) ++ ">\n"
generateMarkdownBlock (EmphasizeBlock emphasize text) = "<" ++ emphasizeTag emphasize ++ ">" ++ text ++ "</" ++ emphasizeTag emphasize ++ ">\n"

headerLevel :: Headers -> Int
headerLevel H1 = 1
headerLevel H2 = 2
headerLevel H3 = 3
headerLevel H4 = 4
headerLevel H5 = 5
headerLevel H6 = 6

emphasizeTag :: Emphasize -> String
emphasizeTag Bold = "strong"
emphasizeTag Italic = "em"

slides :: Slides
slides = Slides [
    Slide (TitleSlide "Slide 1") (BodySlide [
        HeaderBlock H1 "h1 Hola Slide 1",
        HeaderBlock H2 "h2 Hola Slide 1",
        MdParagraph "asdasdasd",
        MdParagraph "asdasdasdasd",
        MdParagraph "Spacedas",
        MdParagraph "das",
        AutoLinkBlock URL "bold text",
        MdParagraph "hola",
        MdParagraph "dasasd",
        AutoLinkBlock URL "italic text",
        AutoLinkBlock URL "autolink text"
    ]),
    Slide (TitleSlide "Slide 2") (BodySlide [
        AutoLinkBlock URL "Hola Slide 2",
        MdParagraph "Hola H2 Slide 2",
        AutoLinkBlock URL "Hola H3 Slide 2",
        MdParagraph "Hola H4 Slide 2",
        AutoLinkBlock URL "Hola H5 Slide 2",
        MdParagraph "Hola H6 Slide 2"
    ])
  ]