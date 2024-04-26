module HTMLGenerator where

import AbstractGrammar

starterHtml :: String
starterHtml =
  "<!DOCTYPE html/>\n\
  \ <html>\n\
  \   <head>\n\
  \     <link\n\
  \       rel=\"stylesheet\"\n\
  \       href=\"https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/css/bootstrap.min.css\"\n\
  \       integrity=\"sha384-Gn5384xqQ1aoWXA+058RXPxPg6fy4IWvTNh0E263XmFcJlSAwiGgFAW/dAiS6JXm\"\n\
  \       crossorigin=\"anonymous\"\n\
  \     />\n\
  \     <link rel=\"stylesheet\" href=\"style.css\" />\n\
  \     <link\n\
  \       rel=\"stylesheet\"\n\
  \       href=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.25.0/themes/prism-okaidia.min.css\"\n\
  \     />\n\
  \   </head>\n\
  \   <body>\n\
  \     <div\n\
  \       id=\"carouselExampleIndicators\"\n\
  \       class=\"carousel slide\"\n\
  \       data-ride=\"carousel\"\n\
  \       data-interval=\"false\"\n\
  \     >\n\
  \   <div class=\"carousel-inner\">"

finishHtml :: String
finishHtml =
  "</div>\n\
  \  <a\n\
  \    class=\"carousel-control-prev\"\n\
  \    href=\"#carouselExampleIndicators\"\n\
  \    role=\"button\"\n\
  \    data-slide=\"prev\"\n\
  \  >\n\
  \    <span class=\"carousel-control-prev-icon\" aria-hidden=\"true\"></span>\n\
  \    <span class=\"sr-only\">Previous</span>\n\
  \  </a>\n\
  \  <a\n\
  \    class=\"carousel-control-next\"\n\
  \    href=\"#carouselExampleIndicators\"\n\
  \    role=\"button\"\n\
  \    data-slide=\"next\"\n\
  \  >\n\
  \    <span class=\"carousel-control-next-icon\" aria-hidden=\"true\"></span>\n\
  \    <span class=\"sr-only\">Next</span>\n\
  \  </a>\n\
  \</div>\n\
  \\n\
  \<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.25.0/prism.min.js\"></script>\n\
  \<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.25.0/components/prism-haskell.min.js\"></script>\n\
  \<script src=\"https://cdnjs.cloudflare.com/ajax/libs/prism/1.25.0/components/prism-javascript.min.js\"></script>\n\
  \\n\
  \<script\n\
  \  src=\"https://code.jquery.com/jquery-3.2.1.slim.min.js\"\n\
  \  integrity=\"sha384-KJ3o2DKtIkvYIK3UENzmM7KCkRr/rE9/Qpg6aAZGJwFDMVNA/GpGFF93hXpG5KkN\"\n\
  \  crossorigin=\"anonymous\"\n\
  \></script>\n\
  \<script\n\
  \  src=\"https://cdn.jsdelivr.net/npm/popper.js@1.12.9/dist/umd/popper.min.js\"\n\
  \  integrity=\"sha384-ApNbgh9B+Y1QKtv3Rn7W3mgPxhU9K/ScQsAP7hUibX39j7fakFPskvXusvfa0b4Q\"\n\
  \  crossorigin=\"anonymous\"\n\
  \></script>\n\
  \<script\n\
  \  src=\"https://cdn.jsdelivr.net/npm/bootstrap@4.0.0/dist/js/bootstrap.min.js\"\n\
  \  integrity=\"sha384-JZR6Spejh4U02d8jOt6vLEHfe/JQGiRRSQQxSfFWpi1MquVdAyjUar5+76PVCmYl\"\n\
  \  crossorigin=\"anonymous\"\n\
  \></script>\n\
  \</body>\n\
  \</html>"

generateHTML :: Slides -> String
generateHTML (Slides slides) =
  starterHtml ++ generateCarouselIndicators slides ++ generateSlides slides ++ finishHtml

generateCarouselIndicators :: [Slide] -> String
generateCarouselIndicators slides =
  "<ol class=\"carousel-indicators\">\n"
    ++ concatMap generateIndicator [0 .. (length slides - 1)]
    ++ "</ol>\n"

generateIndicator :: Int -> String
generateIndicator index =
  "<li data-target=\"#carouselExampleIndicators\" data-slide-to=\""
    ++ show index
    ++ "\""
    ++ (if index == 0 then " class=\"active\"" else "")
    ++ "></li>\n"

generateSlides :: [Slide] -> String
generateSlides slides = concatMap generateSlideWithIndex (zip [0 ..] slides)

generateSlideWithIndex :: (Int, Slide) -> String
generateSlideWithIndex (index, slide) = generateSlide index slide

generateSlide :: Int -> Slide -> String
generateSlide index (Slide titleSlide theme bgImage bodySlide) =
  "<div class=\"carousel-item "
    ++ activeClass
    ++ " "
    ++ getThemeClass theme
    ++ "\">\n"
    ++ "<div class=\"slide-container\"\
       \style=\""
    ++ generateBgImage bgImage
    ++ "\
       \background-repeat: no-repeat; background-size: cover;\">\n"
    ++ generateTitleSlide titleSlide
    ++ generateBodySlide bodySlide
    ++ "</div>\n"
    ++ "</div>\n"
  where
    activeClass = if index == 0 then "active" else ""

getThemeClass :: Themes -> String
getThemeClass LightTheme = "light-theme"
getThemeClass DarkTheme = "dark-theme"

generateBgImage :: BgImage -> String
generateBgImage (BgImage url) = "background-image: url('" ++ url ++ "');"
generateBgImage EmptyBgImage = ""

generateTitleSlide :: Titles -> String
generateTitleSlide (TitleSlide "") = ""
generateTitleSlide (TitleSlide title) = "<h1 class=\"title\">" ++ title ++ "</h1>\n"
generateTitleSlide (BigTitleSlide title) = "<h1 class=\"big-title\">" ++ title ++ "</h1>\n"

generateBodySlide :: BodySlide -> String
generateBodySlide (BodySlide blocks) = "<div class=\"content\">\n" ++ concatMap generateMarkdownBlock blocks ++ "</div>\n"

generateMarkdownBlock :: MarkdownBlock -> String
generateMarkdownBlock LineBreak = "<br/>"
generateMarkdownBlock (MdParagraph text) = "<p>" ++ text ++ "</p>\n"
generateMarkdownBlock (AutoLink link) =
  "<a href=\"" ++ link ++ "\">" ++ link ++ "</a>\n"
generateMarkdownBlock (HeaderBlock header text) =
  "<h" ++ show (headerLevel header) ++ ">" ++ text ++ "</h" ++ show (headerLevel header) ++ ">\n"
generateMarkdownBlock (EmphasizeBlock emphasize text) =
  "<" ++ emphasizeTag emphasize ++ ">" ++ text ++ "</" ++ emphasizeTag emphasize ++ ">\n"
generateMarkdownBlock (Image url) = "<img src=\"" ++ url ++ "\" alt=\"image\"/>"
generateMarkdownBlock (Code codeStr) =
  "<pre><code class=\"language-javascript language-java language-haskell\">" ++ codeStr ++ "</code></pre>"
generateMarkdownBlock (ListBlock BulletPoint text) =
  "<li class=\"bullet-point\">" ++ text ++ "</li>"
generateMarkdownBlock (ListBlock NumberPoint text) =
  "<li class=\"number-point\">" ++ text ++ "</li>"

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
