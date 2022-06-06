edus <- c("International Business and Politics", 
"Statskundskab", 
"Sociologi", 
"Samfundsfag", 
"Samfundsvidenskabelig bacheloruddannelse",
"Sociologi", 
"Politik og økonomi" , 
"Samfundsvidenskabelig international bacheloruddannelse", 
"Politik og Administration")

source("0.3 Design-indstillinger.R")

file.list <- paste("KOT tal/", 
                   list.files(path = "KOT tal/", pattern='*.xlsx', recursive = TRUE),
                   sep = "")

read_kot <- function(x) {
  
  read_excel(path = x) %>% 
    filter(str_detect(Optagelsesområder, str_c(edus, collapse="|"))) %>% 
    mutate(year = x,
           year = sub('.*(\\d{4}).*', '\\1', year)) %>% 
    select(starts_with("Optagelsesområder"), starts_with("Adgangskvotient"), year)
  
}

o <- purrr::map_dfr(lapply(file.list, read_kot), ~.x)

o <- o %>% mutate(Adgangskvotient = gsub(",", ".", Adgangskvotient), 
             Adgangskvotient = as.numeric(Adgangskvotient),
             Optagelsesområder = gsub(", journalistik", "+ journalistik", Optagelsesområder),
             Optagelsesområder = gsub(", andet fag", "+ andet fag", Optagelsesområder)) 

o <- o %>% 
  separate(Optagelsesområder, sep = ",", into = c("Fag", "Sted", "Studiestart"), convert = TRUE) %>% 
  filter(year != 2014) 


o$Fag <- gsub("Samfundsvidenskabelig bacheloruddannelse\\+ journalistik \\+ andet fag", "Samfundsvidenskabelig bacheloruddannelse + journalistik + andet fag", o$Fag)


o <- o %>%   mutate(fag_sted = paste(Fag, Sted, sep = ""),
         fag_sted = gsub("Aalborg Øst", "Aalborg", fag_sted),
         Sted = gsub("Aalborg Øst", "Aalborg", Sted),
         Sted = gsub("Esbjerg", "Odense M", Sted,  
                     )) %>% 
  mutate_all(funs(ifelse(is.na(.), 0, .)))

o <- o %>% filter(!fag_sted %in% c("Sociologi og kulturanalyse Esbjerg", "Samfundsfag Aalborg"))

op <- ggplot() + 
  geom_point(data = o, aes(x = year, y = Adgangskvotient, fill = Fag, colour = Sted, group = fag_sted), alpha = 1) +
  geom_line(data = o, aes(x = year, y = Adgangskvotient, fill = Fag, colour = Sted, group = fag_sted), size = 0.5, alpha = 1, linetype = "dashed") + 
  geom_line(data = o %>% filter(Sted == " Roskilde"), aes(x = year, y = Adgangskvotient, fill = Fag, colour = Sted, group = fag_sted), size = 1) + 
  guides(group = guide_legend("fag_sted"), fill = FALSE) + 
  theme_linedraw() +
  theme(legend.position = "bottom") +
  labs(x = "") +
  scale_colour_manual(labels = c(
    "AAU", 
    "AU", 
    "CBS", 
    "KU", 
    "SDU", 
    "RUC"
  ), values = ruc_palette_samlet) +#+ scale_colour_manual(values = ruc_palette_samlet) 
  annotate("text", x = c(2.2, 3.5, 2.2), 
           y=c(4.15, 10.5, 7.5), 
           label = c("Samfundsvidenskabelig \n bacheloruddannelse", 
                     "Samfundsvidenskabelig \n bacheloruddannelse + journalistik", 
                     "Samfundsvidenskabelig international \n bacheloruddannelse"), size = 2.5, fontface = "bold")
op

ggsave(op, filename = "KOT_plot.png", width = 5, height = 5)



