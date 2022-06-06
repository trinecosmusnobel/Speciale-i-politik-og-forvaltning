source("2.1 Join af dataset.R")

#######
df <- full %>% ungroup() %>%
  #  filter(type_eksamen == "alm.eksamen") %>% 
  mutate(eksamensform_dummy = as.numeric(recode(year,
                                                "2017" = 0,
                                                "2018" = 1,
                                                "2019" = 1)), 
         #        AndelPoint_vores = (SurveyOrdinaer + BeskrivOrdinaer + RegressionOrdinaer + SammenhaengOrdinaer)/4, 
         merit_dummy = recode(merit_dummy, 
                              "Ja" = 1, 
                              "Nej" = 0)) %>% 
  mutate(AndelPoint_vores = AndelPoint)

#######

u <- paste(round(prop.table(table(df$year, df$Bestaaet_dummy), margin = 1), digits = 4)*100,
      " %, n = ", 
table(df$year, df$Bestaaet_dummy), sep = "")

u[1:3]
u[4:6]

i <- cbind(
  "Andel der ikke består eksamen" = u[1:3]
)

rownames(i) <- 2017:2019

i %>% Dumpeprocent
  kable(format = "latex", caption = "Andel der ikke består eksamen i kvantitativ metode i årene 2017-2019", label = "dumpeprocent", booktabs = TRUE) %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>% 
  row_spec(0, bold = TRUE)

#######

u <- paste(round(prop.table(table(df$year, df$status_gennemfoert), margin = 1), digits = 4)*100,
           " %, n = ", 
           table(df$year, df$status_gennemfoert), sep = "")

u[1:3]
u[4:6]

i <- cbind(
  "Andel der har besvaret surveyen" = u[4:6]
)

rownames(i) <- 2017:2019

i %>% 
  kable(align = "c", format = "latex", caption = "Andel der har besvaret surveyen årene 2017-2019", label = "besvarprocent", booktabs = TRUE) %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>% 
  row_spec(0, bold = TRUE)


#######

table(full$Bestaaet_dummy)

####Antal i data

o <- t(t(table(df$year)))
colnames(o) <- "Antal"

o %>% 
  kable(align = "c", format = "latex", caption = "Antal eksamener", label = "antaleksameneridata", booktabs = TRUE) %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>% 
  row_spec(0, bold = TRUE)




