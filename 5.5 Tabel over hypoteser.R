options(encoding = "UTF-8")

source("3.0 Samlede analyser.R")

t <- readxl::read_xlsx("Tabel over hypoteser.xlsx")

colnames(t)

e <- t %>% 
  kable(booktabs = TRUE, format = "latex") %>% 
  column_spec(2, width = "8cm") %>% 
  column_spec(c(1), width = "1.5cm") %>% 
  column_spec(3, width = "3cm") %>% 
  column_spec(6, width = "2cm") %>%
  kable_styling(font_size = 10, latex_options = "striped", position = "center") %>% 
  kableExtra::landscape()

e

#uden resultat-kolonner

t2 <- t %>% select(-c("Styrke af effekt", "Resultat", "Test"))

e2 <- t2 %>% 
  kable(booktabs = TRUE, format = "latex") %>% 
  column_spec(2, width = "14cm") %>% 
  column_spec(c(1), width = "1.5cm") %>% 
  column_spec(3, width = "3cm") %>% 
  kable_styling(font_size = 10, latex_options = "striped", position = "center") %>% 
  kableExtra::landscape()

e2
