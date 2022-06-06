source("6.0 Litteraturreview - netværk.R")

test <- readxl::read_xlsx("SCOPUS søgning resultater.xlsx") 

test %>% 
  kable(align = "left", booktabs = TRUE, format = "latex", label = "søgeord", caption = "Resultater af litteratursøgning i SCOPUS. Søgning begrænset til resultater indenfor keywords, abstract eller titel samt udelukkende danske og engelske resultater", 
        linesep = "\\addlinespace") %>%
  kable_styling(latex_options = "striped") %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>%
  kable_styling(font_size = 6) %>% 
  row_spec(0, bold = TRUE) %>% 
  row_spec(15, hline_after = TRUE)

test
