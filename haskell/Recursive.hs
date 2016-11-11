type HTML = [HTML_element]

data HTML_element
  = HTML_text String
  | HTML_font Font_tag HTML
  | HTML_p HTML
  | HTML_ul [HTML]
  | HTML_ol [HTML]

data Font_tag = Font_tag String

strip_font_tags :: HTML -> HTML
strip_font_tags [] = []
strip_font_tags html = map f html
  where f (HTML_text str) = HTML_text str
        f (HTML_font _ es) = strip_font_tags es 
        f (HTML_p es) = HTML_p (strip_font_tags es)
        f (HTML_ul ess) = HTML_ul (map strip_font_tags ess)
        f (HTML_ol ess) = HTML_ol (map strip_font_tags ess)
