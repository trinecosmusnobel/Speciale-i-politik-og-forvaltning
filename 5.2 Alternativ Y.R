source("3.0 Samlede analyser.R")

m17 <- lm(antal_forelaesninger ~ 
            mor_højest_udd_dummy + 
            eksamensform_dummy + 
            merit_dummy + 
            Adg_kvotient + 
            self_efficacy + 
            relevel(as.factor(Adg_type_dummy), ref = "STX"),
          data = df)

summary(m17)