source("1 Ny eksamensdata.R")

ny_eksamensdata <- ny_eksamensdata %>% 
  mutate(Studienr = case_when(
    name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
     name == "ANONYMISERET" ~ "ANONYMISERET",
    name == (ny_eksamensdata %>% filter(grepl("ANONYMISERET", name)) %>% pull(name)) ~ "ANONYMISERET", #det her kan for some reason ikke kopieres ind
    TRUE ~ as.character(Studienr)
  ))

ny_eksamensdata <- ny_eksamensdata %>% filter(is.na(name) | name != "xx - test")



