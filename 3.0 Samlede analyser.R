#KODE SAMLET

####Source af data og indstillinger

options(scipen = 100, digits = 4)
options(encoding = "UTF-8") 

source("2.1 Join af dataset.R")
source("0.3 Design-indstillinger.R")

##### 
##### DEL 1 - Variansanalyse af andel point
b6 <- ggplot(df %>% filter(bedømmer_anon == "Bedømmer_6"), aes(x = AndelPoint, y = as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_vline(xintercept = 0.4537, linetype = "dashed") + 
  theme_ridges() + 
  theme(legend.position = "none") +
  theme_linedraw() + 
  scale_fill_manual(values = ruc_palette_samlet) + 
  
  labs(x = "Andel point") + 
  scale_x_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  
  guides(fill = "none") + 
  theme(axis.title.y = element_blank(), 
        plot.title = element_text(family = "serif", size = 15, hjust = 0.5)) +
  labs(title = "Bedømmer 6")

b7 <- ggplot(df %>% filter(bedømmer_anon == "Bedømmer_7"), aes(x = AndelPoint, y = as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_vline(xintercept = 0.4537, linetype = "dashed") + 
  theme_ridges() + 
  theme(legend.position = "none") +
  theme_linedraw() + 
  
  scale_fill_manual(values = ruc_palette_samlet) + 
  
  labs(x = "Andel point") + 
  scale_x_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  
  guides(fill = "none") + 
  theme(axis.title.y = element_blank(), 
        plot.title = element_text(family = "serif", size = 15, hjust = 0.5)) +
  labs(title = "Bedømmer 7")


b8 <- ggplot(df %>% filter(bedømmer_anon == "Bedømmer_8"), aes(x = AndelPoint, y = as.factor(year), fill = as.factor(year))) +
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_vline(xintercept = 0.4537, linetype = "dashed") + 
  theme_ridges() + 
  theme(legend.position = "none") +
  theme_linedraw() + 
  scale_fill_manual(values = c(ruc_palette_samlet[1], ruc_palette_samlet[3])) + 
  
  
  labs(x = "Andel point") + 
  scale_x_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  
  guides(fill = "none") + 
  theme(axis.title.y = element_blank(), 
        plot.title = element_text(family = "serif", size = 15, hjust = 0.5)) +
  labs(title = "Bedømmer 8")

b9 <- ggplot(df, 
             aes(x = AndelPoint, 
                 y = as.factor(year), 
                 fill = as.factor(year))) +
  geom_density_ridges(quantile_lines=TRUE,
                      quantile_fun=function(x,...)mean(x)) +
  geom_vline(xintercept = 0.4537, linetype = "dashed") + 
  theme_ridges() + 
  theme(legend.position = "none") +
  theme_linedraw() + 
  scale_fill_manual(values = ruc_palette_samlet) + 
  labs(x = "Andel point") + 
  scale_x_continuous(labels = scales::percent_format(), limits = c(0,1)) +
  guides(fill = "none") + 
  theme(axis.title.y = element_blank(), 
        plot.title = element_text(family = "serif", size = 15, hjust = 0.5)) +
  labs(title = "Alle bedømmere")

ggsave(filename = "FINALIMG/arrange_ridge_plots.png", plot = 
         ggpubr::ggarrange(b6, b7, b8, b9), 
       width = 6, height = 5
)
##### 
### LEVENES TEST FOR HOMOSKEDASTICITET

lev1 <- car::leveneTest(AndelPoint ~ as.factor(eksamensform_dummy), data = df, center = "mean")
lev2 <- car::leveneTest(AndelPoint ~ as.factor(year), data = df, center = "mean")

#F-værdier med signifikans
t <- setNames(as.data.frame(t(round(as.numeric(gsub(" ", "", paste(gtools::stars.pval(
  c(
    lev1$`Pr(>F)`[1],
    lev2$`Pr(>F)`[1]
  )), c(
    lev1$`F value`[1],
    lev2$`F value`[1]
  ), sep = ""))), digits = 3)), 
  row.names = c("Levene's test")), 
  #Kolonnenavne (argument i setNames)
  c("x = Bedømmelsesform", "x = År"))

t %>% 
  kable(align = "left", booktabs = TRUE, format = "latex", caption = "Tests for homoskedasticitet for y = andel point. Ingen af testsne er siginifikante", label = "levene") %>% 
  kable_classic_2(full_width = F, html_font = "Garamond") %>% 
  row_spec(0, bold = TRUE)

##### 
##### DEL 2 - Har karakter en effekt på præstation og trivsel?

#For hvert år
summary <- Rmisc::summarySE(data = df, measurevar = "AndelPoint", groupvars = "year")

plot1 <- ggplot(summary) +
  geom_bar(aes(x=year, y=AndelPoint), fill = c(ruc_palette_samlet[2], ruc_palette_samlet[4], ruc_palette_samlet[4]), stat="identity", alpha=1, colour = "black", width = 0.75) +
  geom_errorbar(aes(x=year, ymin=AndelPoint-ci, ymax=AndelPoint+ci), width=0.1, colour="black", alpha=0.9, size=0.5, position = position_dodge(0.9)) + 
  # theme_bw() 
  geom_text(aes(x = year, y = AndelPoint, label = paste(round(AndelPoint*100, digits = 2), "%")), vjust = -1.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) + 
  labs(y = "Andel point") + 
  coord_cartesian(ylim = c(0.05,1))  +
  scale_x_continuous(breaks = 2017:2019) +
  theme_linedraw() +
  theme(axis.title.x = element_blank())

ggsave(filename = "FINALIMG/plot_andelpoint_gennemsnit_pr_år.png", plot = plot1, width = 5, height = 4)

summary <- Rmisc::summarySE(data = df %>% filter(!is.na(vurdering_arbejdsbyrde)), measurevar = "vurdering_arbejdsbyrde", groupvars = "year")

plot2 <- ggplot(summary) +
  geom_bar(aes(x=year, y=vurdering_arbejdsbyrde), fill = c(ruc_palette_samlet[2], ruc_palette_samlet[4], ruc_palette_samlet[4]), stat="identity", alpha=1, colour = "black", width = 0.75) +
  geom_errorbar( aes(x=year, ymin=vurdering_arbejdsbyrde-ci, ymax=vurdering_arbejdsbyrde+ci), width=0.1, colour="black", alpha=0.9, size=0.5) + 
  theme_linedraw() +
  # theme_bw() 
  geom_text(aes(x = year, y = vurdering_arbejdsbyrde, label = paste(round(vurdering_arbejdsbyrde, digits = 2))), vjust = -1.5, size = 5) +
  labs(y = "Vurdering af arbejdsbyrde") + 
  theme(axis.title.x = element_blank()) +
  coord_cartesian(ylim = c(1, 5)) +
  theme_linedraw()  + 
  scale_x_continuous(breaks = 2017:2019) +
  theme(axis.title.x = element_blank())
  
ggsave(filename = "FINALIMG/plot_vurdering_arbejdsbyrde_gennemsnit_pr_år.png", plot = plot2, width = 5, height = 4)

#For de to eksamensformer
summary <- Rmisc::summarySE(data = df, measurevar = "AndelPoint", groupvars = "eksamensform_dummy")

plot3 <- ggplot(summary) +
  geom_bar(aes(x=eksamensform_dummy, y=AndelPoint, fill = as.factor(eksamensform_dummy)), stat="identity", alpha=1, colour = "black", width = 0.75) +
  geom_errorbar(aes(x=eksamensform_dummy, ymin=AndelPoint-ci, ymax=AndelPoint+ci), width=0.1, colour="black",  alpha=0.9, size=0.5, position = position_dodge(0.9)) + 
  #theme_linedraw()+ 
  geom_text(aes(x = eksamensform_dummy, y = AndelPoint, label = paste(round(AndelPoint*100, digits = 2), "%")), vjust = -4) +
  theme_bw() +
  ylim(0,1) + 
  scale_y_continuous(labels = scales::percent_format(accuracy = 5L), limits = c(0,1)) + 
  theme_linedraw() +
  theme(axis.title.x = element_blank()) +
  guides(fill = FALSE) + 
  coord_cartesian(ylim = c(0.05,1))  +
  scale_x_continuous(breaks = 0:1, labels = c("Bestået/ikke bestået", "Karakterer")) + 
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4])) 

##### 
##### DEL 3 - #Analyse af bedømmelsesmodellens effekt på præstation

m1 <- lm(AndelPoint ~ eksamensform_dummy, data = df)

m2 <- lm(AndelPoint ~ as.factor(year), data = df)

m3 <- lm(AndelPoint ~ 
           eksamensform_dummy + 
           merit_dummy + 
           Adg_kvotient + 
           relevel(as.factor(Adg_type_dummy), ref = "STX"), 
         data = df)

m3.2 <- lm(AndelPoint ~ 
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym +
             mindst_en_højtudd +
             relevel(gender_coded, ref = "Mand"),
           data = df)


#Tabel 8.2
STAR1 <- stargazer::stargazer(m1, m3,
                     covariate.labels = c(
                       "Karakter (d)" ,
                       "Meritstuderende (d)", 
                       "Adg. kvotient",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)", 
                       "Konstant"
                     ), type = "latex", 
                     column.labels = c("Uden kontrol", "Med kontrol"), 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point", 
                     font.size = "small", 
                     label = "regression:andelpoint")

#Tabel 8.3
STAR2 <- stargazer::stargazer(m3.2, type = "latex", 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     covariate.labels = c(
                       "Karakterbedømmelse (d)" ,
                       "Meritstuderende (d)", 
                       "Adg. karaktergns.",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)", 
                       "Matematik-evne (i)", 
                       "Mindst én forælder højtudd. (d)",
                       "Kvinde (d)",
                       "Konstant"
                     ),
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point med selvrapporterede variable som kontrol", 
                     label = "regandelpointekstra" ,
                     font.size = "small")

e <- check_model(m1, check = c("normality", #normalfordelte fejlled
                               "linearity",  #linearitet
                               "homogeneity", #varianshomogenitet
                               "outliers",  #outliers
                               "vif", #multikollinearitet
                               "qq")) 

e1 <- check_model(m3, check = c("normality", #normalfordelte fejlled
                                "linearity",  #linearitet
                                "homogeneity", #varianshomogenitet
                                "outliers",  #outliers
                                "vif", #multikollinearitet
                                "qq")) ##normalfordelte fejlled - 2

##### 
##### DEL 4 - Analyse af bedømmelsesformens effekt på trivsel

m1.1 <- lm(vurdering_arbejdsbyrde ~ eksamensform_dummy, data = df)

m1.2 <- lm(vurdering_arbejdsbyrde ~ 
             eksamensform_dummy + 
             relevel(as.factor(Adg_type_dummy), ref = "STX") +
             merit_dummy + Adg_kvotient +
             relevel(gender_coded, ref = "Mand"), data = df)

m1.3 <- lm(vurdering_arbejdsbyrde ~ 
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX") + 
             relevel(gender_coded, ref = "Mand") + 
             mindst_en_højtudd +
             intrinsisk_motivation +
             matematik_gym + 
             self_efficacy,
           data = df)

#Tabel 8.4
m1.1 <- lm(vurdering_arbejdsbyrde ~ eksamensform_dummy, data = df)

m1.2 <- lm(vurdering_arbejdsbyrde ~ 
             eksamensform_dummy + 
             relevel(as.factor(Adg_type_dummy), ref = "STX") +
             merit_dummy + Adg_kvotient +
             relevel(gender_coded, ref = "Mand"), data = df)

m1.3 <- lm(vurdering_arbejdsbyrde ~ 
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX") + 
             relevel(gender_coded, ref = "Mand") + 
             mindst_en_højtudd +
             matematik_gym, 
            data = df)

STAR3 <- stargazer::stargazer(m1.1, m1.2, m1.3,
                              covariate.labels = c(
                                "Karakterbedømmelse (d)",
                                
                                "HF (d)",
                                "HHX (d)",
                                "HTX (d)",
                                "IB (d)" ,
                                
                                "Meritstuderende (d)",
                                "Adg. kvotient",
                                "Kvinde (d)",
                                "Mindst en forælder højtudd. (d)" ,
                                
                                "Matematik-evne (i)",
                                "Konstant"
                              ),
                              type = "text", 
                              column.labels = c("Uden kontrol", "Kontrol for registervariable", "Alle kontroller"), 
                              keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                           ,"f"
                              ), 
                              dep.var.labels = c("y = Stressnivau"),
                              report = "vc*", 
                              star.cutoffs = c(0.05, 0.01, 0.001), 
                              notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                              dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                              align = TRUE, 
                              title = "Regressionsresultater for effekten af karakter som bedømmelse på trivsel", 
                              font.size = "small", 
                              label = "trivselregression", float.env  = "sidewaystable")

##### 
##### DEL 4 - Køn og andel point - med og uden interaktion med bedømmelsesform

##Deskriptive plot -- adgangsgivendekarakter gennemsnit de forskellige år
plot.koen.karakter <- df %>% 
  group_by(gender_coded, year) %>% 
  drop_na(Adg_kvotient, gender_coded) %>% 
  mutate(meankarakter = mean(Adg_kvotient)) %>% 
  distinct(meankarakter, gender_coded, year)

summary <- Rmisc::summarySE(data = df %>% drop_na(gender_coded, year, Adg_kvotient), 
                            measurevar = "Adg_kvotient", 
                            groupvars = c("gender_coded", "year"))

plotkoen <- ggplot(summary %>% mutate(Adg_kvotient = round(Adg_kvotient, digits = 2)), aes(x = year, y = Adg_kvotient, fill = gender_coded)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_errorbar( aes(x=year, ymin=Adg_kvotient-ci, ymax=Adg_kvotient+ci), width=0.1, colour="black", alpha=0.9, size=0.5, position = position_dodge(0.9)) +  
  geom_text(aes(x = year, 
                y = Adg_kvotient, 
                label = round(Adg_kvotient, 
                              digits = 3)), 
            position=position_dodge(width=0.9), 
            vjust=-1.5) +
  ylim(0,11.5) +
  coord_cartesian(ylim = c(1, 11.5)) + 
  xlab(" ") + ylab("Adgangsgivende karakter") +
  labs(fill = "Køn") +
  theme_linedraw() + 
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]))

ggsave(filename = "FINALIMG/adg_karakter_gennemsnit_pr_år_koen.png", plot = plotkoen, width = 5, height = 4)


#AndelPoint
plot7 <- Rmisc::summarySE(data = df, measurevar = "AndelPoint", groupvars = c("year", "gender_coded"), na.rm = TRUE) %>% 
  filter(!is.na(gender_coded)) %>% 
  ggplot(aes(x = year, 
             y = AndelPoint, 
             group = gender_coded, 
             fill = gender_coded)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black", alpha = 0.9, size = 0.5) +
  geom_errorbar(aes(x=year, 
                     ymin=AndelPoint-ci, 
                     ymax=AndelPoint+ci), 
                 width=0.1, 
                 colour="black", 
                 alpha=0.9, 
                 size=0.5, 
                 position = position_dodge(0.9)) +  
  geom_text(aes(x = year, 
                y = AndelPoint, 
                label = paste(round(AndelPoint*100, digits = 2), "%")), 
            position=position_dodge(width=0.9), vjust=-2, size = 2.5) +
  theme_bw() +
  ylim(0,1) +
  xlab(" ") + ylab("Andel point") +
  labs(fill = "Køn")  + 
  coord_cartesian(ylim = c(0.0, 1)) +
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4])) + 
  theme_linedraw() +
  scale_y_continuous(labels = scales::percent_format())

ggsave(filename = "FINALIMG/AndelPoint køn.png", plot = plot7, width = 5, height = 4)

#Tabel 5
m6 <- lm(AndelPoint_vores ~ 
           relevel(gender_coded, ref = "Mand") + 
           eksamensform_dummy + 
           merit_dummy + 
           Adg_kvotient + 
           matematik_gym + 
           relevel(as.factor(Adg_type_dummy), ref = "STX"),data = df)

m7 <- lm(AndelPoint_vores ~ 
           relevel(gender_coded, ref = "Mand") + 
           eksamensform_dummy + 
           relevel(gender_coded, ref = "Mand"):eksamensform_dummy + 
           merit_dummy + 
           Adg_kvotient + 
           matematik_gym +
           relevel(as.factor(Adg_type_dummy), ref = "STX"),data = df)


m8 <- lm(AndelPoint_vores ~ 
           eksamensform_dummy + 
           relevel(gender_coded, ref = "Mand") + 
           relevel(gender_coded, ref = "Mand"):eksamensform_dummy,data = df)

##Tabel 8.5
STAR4 <- stargazer::stargazer(m8, m6, m7, type = "latex",
                     covariate.labels = c(
                       "Karakterbedømmelse",
                       "Merit (d)",
                       "Adg. karaktergns.",
                       "Matematikevne",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)", 
                       "Karakterbedømmelse*Kvinde (i)",
                       "Kvinde (d)",
                       "Karakterbedømmelse*Kvinde (i)",
                       "Konstant"),
                     column.labels = c("Uden kontrol ", "Med kontrol eksl. interaktion", "Med kontrol"), 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for kønseffekten af karakter som bedømmelse på andel af point", 
                     font.size = "small",
                     float.env = "sidewaystable", 
                    label = "tab:reg_koen_bed")

##Scatterplot med forskel i hældning -- med kontrolleret hældning
m9 <- lm(vurdering_arbejdsbyrde ~ 
           gender_coded:eksamensform_dummy +
           gender_coded +
           eksamensform_dummy +
           merit_dummy + 
           Adg_kvotient + 
           matematik_gym +
           relevel(as.factor(Adg_type_dummy), ref = "STX"),data = df)

m9.interaction.plot <- ggpredict(m9, terms = c("eksamensform_dummy", "gender_coded"))

int.plot2 <- 
  ggplot(m9.interaction.plot, aes(x, predicted, colour = group, group = group, label = round(predicted, 2))) + 
  geom_line(linetype = "dotted", size = 1) + 
  #geom_smooth(method=lm, se = FALSE, linetype = "dashed", size = 1.5) +
  geom_errorbar(aes(ymin = conf.high, ymax = conf.low), width = 0.05, size = 1) +
  scale_x_continuous(breaks = c(0,1), labels = c("Bestået/ikke bestået", "Karakterer"), limits = c(-0.25,1.25)) +
  xlab("Eksamensform") +
  ylab("Vurdering af arbejdsbyrde") +
  theme(axis.text.x = element_text(size = 12), legend.key=element_blank()) +
  ylim(1,5) +  
  theme_linedraw() + 
  scale_colour_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]))  +
    labs(colour = "Køn")  +
    guides(colour = guide_legend(override.aes = list(size=3))) 
  
ggsave(filename = "FINALIMG/interaktion køn stress.png", plot = int.plot2, width = 5, height = 4)

#Køn og vurderet arbejdsbyrde
køn.vurd.plot <- Rmisc::summarySE(data = df, measurevar = "vurdering_arbejdsbyrde", groupvars = c("year", "gender_coded"), na.rm = TRUE) %>% 
  filter(!is.na(gender_coded)) %>% 
  ggplot(aes(x = year, y = vurdering_arbejdsbyrde, group = gender_coded, fill = gender_coded)) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_errorbar(aes(ymin = vurdering_arbejdsbyrde-ci, ymax = vurdering_arbejdsbyrde+ci), width = 0.1, colour = "black", alpha = 0.9, size = 0.5, position=position_dodge(.9)) +
  theme_bw() +
  xlab(" ") + ylab("Vurderet arbejdsbyrde") +
  labs(fill = "Køn") +
  geom_text(aes(x = year, 
                y = vurdering_arbejdsbyrde, 
                label = round(vurdering_arbejdsbyrde, 
                              digits = 2)), 
            position=position_dodge(width=0.9), 
            vjust=-1.5) +
  ylim(0,5) +
  coord_cartesian(ylim = c(1,5)) + 
  labs(fill = "Køn") +
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]))

ggsave(filename = "FINALIMG/Køn vurderet arbejdsbyrde.png", plot = køn.vurd.plot, width = 5, height = 4)











# Har niveau af stress en større betydning for præstation når der afgives karakter?
m80<- lm(AndelPoint_vores ~ 
           vurdering_arbejdsbyrde + 
           eksamensform_dummy + 
           vurdering_arbejdsbyrde:eksamensform_dummy,data = df)

#med kontroller
m90 <- lm(AndelPoint_vores ~ 
            vurdering_arbejdsbyrde + 
            eksamensform_dummy + 
            merit_dummy + 
            Adg_kvotient + 
            matematik_gym +
            relevel(as.factor(Adg_type_dummy), ref = "STX") +
            vurdering_arbejdsbyrde:eksamensform_dummy, data = df)

#med kontroller uden interaktionsled
m92 <- lm(AndelPoint_vores ~ 
            vurdering_arbejdsbyrde + 
            eksamensform_dummy + 
            merit_dummy + 
            Adg_kvotient + matematik_gym +
            relevel(as.factor(Adg_type_dummy), ref = "STX"), data = df)

summary(m90)

#Tabel 8.6
STAR5 <- stargazer::stargazer(m80, m92, m90, type = "latex",
                     covariate.labels = c(
                       "Vurderet arbejdsbyrde",
                       "Karakterbedømmelse",
                       "Vurderet arbejdsbyrde*Karakterbedømmelse",
                       "Merit (d)",
                       "Adg. karaktergnst.",
                       "Matematikevne",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)",
                       "Konstant"),
                     column.labels = c("Uden kontrol", "Med kontrol", "Med kontrol eksl. interaktion"), 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for interaktion ml. stress og karakter som bedømmelse på andel af point", 
                     font.size = "small",
                     float.env = "sidewaystable",
                     label = "eksamensformstresspræs")

anova(m90, m92)

#Res.Df: 

#Interaktion mellem køn, adgangsgivende karakter og eksamensform
df.k <- df %>% filter(gender_coded == "Kvinde")
df.m <- df %>% filter(gender_coded == "Mand")

df.bestået <- df %>% filter(eksamensform_dummy == 0)
df.karakter <- df %>% filter(eksamensform_dummy == 1)

# lm(vurdering_arbejdsbyrde ~ eksamensform_dummy + Adg_kvotient + eksamensform_dummy:Adg_kvotient, data = df.k) %>% summary()
# lm(vurdering_arbejdsbyrde ~ eksamensform_dummy + Adg_kvotient + eksamensform_dummy:Adg_kvotient, data = df.m) %>% summary()
# 
# lm(vurdering_arbejdsbyrde ~ gender_coded + Adg_kvotient + gender_coded:Adg_kvotient, data = df.bestået) %>% summary()
# lm(vurdering_arbejdsbyrde ~ gender_coded + Adg_kvotient + gender_coded:Adg_kvotient, data = df.karakter) %>% summary()


#syn på mat-evner og køn

## Plot - kønsforskel i andel point/kønsforskel i vurderet arbejdsbyrde

# forskel_køn_point <- Rmisc::summarySE(data = df, measurevar = "AndelPoint_vores", groupvars = c("year", "gender_coded"), na.rm = TRUE) %>% 
#   drop_na(gender_coded) %>%
#   ggplot(aes(x = year, y = AndelPoint_vores, group = gender_coded, fill = gender_coded)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes(ymin = AndelPoint_vores-ci, ymax = AndelPoint_vores+ci), width = 0.2, position=position_dodge(.9)) +
#   theme_bw()

# forskel_køn_vurd12 <- Rmisc::summarySE(data = df, measurevar = "vurdering_arbejdsbyrde", groupvars = c("year", "gender_coded"), na.rm = TRUE) %>% 
#   drop_na(gender_coded) %>%
#   ggplot(aes(x = year, y = vurdering_arbejdsbyrde, group = gender_coded, fill = gender_coded)) + 
#   geom_bar(stat = "identity", position = "dodge") +
#   geom_errorbar(aes(ymin = vurdering_arbejdsbyrde-ci, ymax = vurdering_arbejdsbyrde+ci), width = 0.2, position=position_dodge(.9)) +
#   theme_bw()

###intrinsisk

m19 <- lm(AndelPoint_vores ~  + eksamensform_dummy:intrinsisk_motivation +
            intrinsisk_motivation + eksamensform_dummy,
          data = df)

m191 <- lm(AndelPoint_vores ~ 
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             intrinsisk_motivation + 
             matematik_gym + 
             relevel(as.factor(Adg_type_dummy), ref = "STX"),
           data = df)



m193<- lm(AndelPoint_vores ~ 
            eksamensform_dummy:intrinsisk_motivation + 
            intrinsisk_motivation +
            eksamensform_dummy +
            merit_dummy + 
            Adg_kvotient +
            matematik_gym + 
            relevel(as.factor(Adg_type_dummy), ref = "STX"),
          data = df)



#Tabel 8.7
STAR6 <- stargazer::stargazer(m19, m191, m193, type  = "latex",
                     covariate.labels = c(
                       "Intrinsisk motivation",
                       "Matematikevne",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)", 
                       "Karakterbedømmelse (d)",
                       "Intrinsisk motivation*Karakterbedømmelse(i)",
                       "Merit (d)",
                       "Adg. karaktergnst",
                       "Konstant"),
                     column.labels = c("Uden kontrol", "Med kontrol eksl. interaktion", "Med kontrol"), 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel Point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for betydningen af intrinsisk motivation med og uden karakterbedømmelse for præstation", 
                     font.size = "small",
                     float.env = "sidewaystable", 
                     label = "intrinsisk_karakter_præst")

######## Self-efficacy og intrinsisk motivation

#Self-efficacy
m181 <- lm(AndelPoint_vores ~ 
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             self_efficacy + matematik_gym + 
             relevel(as.factor(Adg_type_dummy), ref = "STX"),
           data = df)

m182 <- lm(AndelPoint_vores ~ 
             eksamensform_dummy:self_efficacy + 
             self_efficacy +
             eksamensform_dummy, data = df)

m183<- lm(AndelPoint_vores ~ 
            eksamensform_dummy + 
            eksamensform_dummy:self_efficacy + 
            self_efficacy + matematik_gym + 
            merit_dummy + 
            Adg_kvotient + 
            relevel(as.factor(Adg_type_dummy), ref = "STX"),
          data = df)


#Tabel 8.8
STAR7 <- stargazer::stargazer(m182, m181, m183, type = "latex",
                     covariate.labels = c(
                       "Mestringstro",
                       "Matematikevne",
                       "HF (d)",
                       "HHX (d)", 
                       "HTX (d)", 
                       "IB (d)", 
                       "Karakterbedømmelse (d)",
                       "Mestringstro*Karakterbedømmelse(i)",
                       "Merit (d)",
                       "Adg. kvotient",
                       "Konstant"),
                     column.labels = c("Uden kontrol", "Med kontrol eksl. interaktion", "Med kontrol"), 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel Point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for betydningen af mestringstro med og uden karakterbedømmelse for præstation", 
                     font.size = "small",
                     float.env = "sidewaystable", 
                     label = "mestringstro_regression")


###DESKRIPTIVT OM SOCIAL BAGGRUND

##Adgangsgivende karakter

summary <- Rmisc::summarySE(data = df %>% drop_na(mindst_en_højtudd, year, Adg_kvotient), 
                            measurevar = "Adg_kvotient", 
                            groupvars = c("mindst_en_højtudd", "year"))

plotSES_gym <- ggplot(summary %>% mutate(Adg_kvotient = round(Adg_kvotient, digits = 2)), aes(x = year, y = Adg_kvotient, fill = as.character(mindst_en_højtudd))) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_errorbar( aes(x=year, ymin=Adg_kvotient-ci, ymax=Adg_kvotient+ci), width=0.1, colour="black", alpha=0.9, size=0.5, position = position_dodge(0.9)) +  
  geom_text(aes(x = year, 
                y = Adg_kvotient, 
                label = round(Adg_kvotient, 
                              digits = 3)), 
            position=position_dodge(width=0.9), 
            vjust=-1.9) +
  ylim(0,11.5) +
  coord_cartesian(ylim = c(1, 11.5)) + 
  xlab(" ") + ylab("Adg. karaktergns.") +
  labs(fill = "Højtuddannet forælder ") +
  theme_bw() + 
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]), labels = c("Mindst en højtudd. forælder", "Ingen højtudd. forældre")) + 
  theme(legend.title=element_blank(), legend.position = "bottom") 

ggsave(filename = "FINALIMG/SES_andelpoint_gym.png", plot = plotSES_gym, width = 5, height = 4)



##Ekstrinsisk motivation

m190 <- lm(AndelPoint_vores ~ 
             eksamensform_dummy:passer_karakter_betyder_meget +
             eksamensform_dummy + passer_karakter_betyder_meget,
           data = df)

m1910 <- lm(AndelPoint_vores ~ 
              eksamensform_dummy + 
              merit_dummy + 
              Adg_kvotient +
              matematik_gym + 
              passer_karakter_betyder_meget + 
              relevel(as.factor(Adg_type_dummy), ref = "STX"),
            data = df)



m1930<- lm(AndelPoint_vores ~ 
             eksamensform_dummy:passer_karakter_betyder_meget + 
             passer_karakter_betyder_meget +
             eksamensform_dummy +
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX"),
           data = df)


###DESKRIPTIVT OM SOCIAL BAGGRUND
##Adgangsgivende karakter

# summary <- Rmisc::summarySE(data = df %>% drop_na(mindst_en_højtudd, year, Adg_kvotient), 
#                             measurevar = "Adg_kvotient", 
#                             groupvars = c("mindst_en_højtudd", "year"))
# 
# plotSES_gym <- ggplot(summary %>% mutate(Adg_kvotient = round(Adg_kvotient, digits = 2)), aes(x = year, y = Adg_kvotient, fill = as.character(mindst_en_højtudd))) + 
#   geom_bar(stat = "identity", position = "dodge", colour = "black") +
#   geom_errorbar( aes(x=year, ymin=Adg_kvotient-ci, ymax=Adg_kvotient+ci), width=0.1, colour="black", alpha=0.9, size=0.5, position = position_dodge(0.9)) +  
#   geom_text(aes(x = year, 
#                 y = Adg_kvotient, 
#                 label = round(Adg_kvotient, 
#                               digits = 3)), 
#             position=position_dodge(width=0.9), 
#             vjust=-1.5) +
#   ylim(0,11.5) +
#   coord_cartesian(ylim = c(1, 11.5)) + 
#   xlab(" ") + ylab("Adgangsgivende karakter") +
#   labs(fill = "Højtuddannet forælder ") +
#   theme_bw()
# 
# ggsave(filename = "FINALIMG/SES_andelpoint_gym.png", plot = plotSES_gym, width = 5, height = 4)


# Andelpoint

xx <- lm(AndelPoint ~ mindst_en_højtudd:as.factor(year) + as.factor(year) + mindst_en_højtudd, data = df)

summary <- Rmisc::summarySE(data = df %>% drop_na(mindst_en_højtudd, year, AndelPoint), 
                            measurevar = "AndelPoint", 
                            groupvars = c("mindst_en_højtudd", "year"))

plotSES_point <- ggplot(summary %>% mutate(AndelPoint = round(AndelPoint, digits = 4)), aes(x = year, y = AndelPoint, fill = as.character(mindst_en_højtudd))) + 
  geom_bar(stat = "identity", position = "dodge", colour = "black") +
  geom_errorbar( aes(x=year, ymin=AndelPoint-ci, ymax=AndelPoint+ci), width=0.1, colour="black", alpha=0.9, size=0.5, position = position_dodge(0.9)) +  
  scale_y_continuous(labels = scales::percent_format(), limits = 0:1) + 
  geom_text(aes(x = year, 
                y = AndelPoint, 
                label = paste(round(AndelPoint*100, digits = 2), "%")), 
            position=position_dodge(width=0.9), 
            vjust=-2.4, size = 3.75) +
  coord_cartesian(ylim = c(0,1)) + 
  xlab(" ") + ylab("Andel point") +
  labs(fill = "Højtuddannet forælder ") +
  coord_cartesian(ylim = c(0,1))+
  scale_fill_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]), labels = c("Mindst en højtudd. forælder", "Ingen højtudd. forældre")) +
  theme(legend.title = element_blank(), legend.position = "bottom")

ggsave(filename = "FINALIMG/SES_andelpoint_plot.png", plot = plotSES_point, width = 5, height = 4)


  #interaktionsplot


##Scatterplot med forskel i hældning -- med kontrolleret hældning

m10 <- lm(AndelPoint ~ 
            mindst_en_højtudd:eksamensform_dummy +
            mindst_en_højtudd +
            eksamensform_dummy +
            merit_dummy + 
            Adg_kvotient + 
            matematik_gym +
            relevel(as.factor(Adg_type_dummy), ref = "STX"),data = df)

m10.interaction.plot <- ggpredict(m10, terms = c("eksamensform_dummy", "mindst_en_højtudd"))


SES_interaktion <- ggplot(m10.interaction.plot, aes(x, predicted, colour = group)) + 
  geom_line(linetype = "dotted", size = 1) +
  geom_errorbar(aes(ymin = conf.high, ymax = conf.low), width = 0.05, size = 1) +
  theme_bw() +
  xlab("") +
  ylab("Andel point") +
  labs(colour ="Højtuddannet forælder") +
  theme(axis.text.x = element_text(size = 12)) +
  scale_x_continuous(breaks = 0:1, labels = c("\nBestået/ikke bestået", "\nKarakterer"), limits = c(-0.25,1.25)) +
  scale_y_continuous(labels = scales::percent_format(), limits = 0:1) + 
  scale_colour_manual(values = c(ruc_palette_samlet[2], ruc_palette_samlet[4]), labels = c("Mindst en højtudd. forælder", "Ingen højtudd. forældre"))  +
  theme(legend.title=element_blank(), legend.position = "bottom")  +
  guides(colour = guide_legend(override.aes = list(size=3))) 

ggsave(filename = "FINALIMG/SES_interaktion.png", plot = SES_interaktion, width = 5, height = 4)




  ##### 
##### Del x - Uddannelse

UDD1 <- lm(AndelPoint ~
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             relevel(gender_coded, ref = "Mand"), data = df)

UDD2 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             begge_forældre_højest_udd_dummy +
             relevel(gender_coded, ref = "Mand"), data = df)

UDD3 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             mindst_en_højtudd + 
             relevel(gender_coded, ref = "Mand"), data = df)

UDD4 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             ingen_forældre_højest_udd_dummy + 
             relevel(gender_coded, ref = "Mand"), data = df)

#Tabel 8.9
STAR8 <- stargazer::stargazer(UDD1,
                     UDD2,
                     UDD3,
                     UDD4, type = "latex", 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  ,"f"
                     ), 
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point med inddragelse af forældres uddannelsesniveau", 
                     font.size = "small", 
                     covariate.labels = c(
                       "Karakterbedømmelse (d)",
                       "Merit (d)",
                       "Adg. kvotient",
                       "HF (d)",
                       "HHX",
                       "HTX",
                       "International",
                       "Matematik-evne (i)",
                       "Begge forældre højtudd. (d)",
                       "Mindst en forælder højtudd. (d)",
                       "Ingen forældre højtudd. (d)",
                       "Kvinde (d)",
                       "Konstant"
                     ),
                     
                     float.env = "sidewaystable", label = "table:edu_reg"
)

#Medd interaktion

UDD1_i <- lm(AndelPoint ~
               eksamensform_dummy + 
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX") + matematik_gym + relevel(gender_coded, ref = "Mand"), data = df)

UDD2_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               
               matematik_gym + relevel(gender_coded, ref = "Mand") +
               begge_forældre_højest_udd_dummy +
               begge_forældre_højest_udd_dummy:eksamensform_dummy, data = df)

UDD3_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               matematik_gym + relevel(gender_coded, ref = "Mand") +
               mindst_en_højtudd + 
               mindst_en_højtudd:eksamensform_dummy, data = df)

UDD4_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               matematik_gym + relevel(gender_coded, ref = "Mand") +
               ingen_forældre_højest_udd_dummy + 
               ingen_forældre_højest_udd_dummy:eksamensform_dummy, data = df)

#Tabel 8.10
STAR9 <- stargazer::stargazer(UDD1_i,
                          UDD2_i,
                          UDD3_i,
                          UDD4_i, type = "latex", 
                          keep.stat =c("f", "n", "rsq", "adj.rsq", "aic"#,
                                       ,"f"
                          ), 
                          dep.var.labels = c("y = Andel point"),
                          report = "vc*", 
                          star.cutoffs = c(0.05, 0.01, 0.001), 
                          notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                          dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                          align = TRUE, 
                          title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point med inddragelse af forældres uddannelsesniveau og interaktioner", 
                          font.size = "small", 
                          covariate.labels = c(
                            "Karakterbedømmelse (d)",
                            "Merit (d)",
                            "Adg. kvotient",
                            "HF (d)",
                            "HHX (d)",
                            "HTX (d)",
                            "International (d)",
                            "Matematik-evne (i)",
                            "Kvinde (d)",
                            "Begge forældre højtudd. (d)",
                            "Karakterbedømmelse*Begge forældre højtudd.",
                            "Mindst en forælder højtudd. (d)",
                            "Karakterbedømmelse*Mindst en forælder højtudd.",
                            "Ingen forældre højtudd. (d)",
                            "Karakterbedømmelse*Ingen forældre højtudd. (d)",
                            "Konstant"
                          ),
                          float.env = "sidewaystable", label = "table:edu_reg_no_int")
                          









########### BILAG - Antal forelæsninger som y

m10_kontrol <- lm(antal_forelaesninger ~ eksamensform_dummy + 
                    merit_dummy + 
                    Adg_kvotient + 
                    relevel(as.factor(Adg_type_dummy), ref = "STX") + 
                    matematik_gym + 
                    mindst_en_højtudd + 
                    KVINDE_dummy, data = df)

STAR10 <- stargazer::stargazer(m10_kontrol, type = "latex", covariate.labels = c(
  "Karakterbedømmelse (d)", 
  "Meritstuderende (d)", 
  "Adg. karaktergnst", 
  "HF (d)", 
  "HHX (d)", 
  "HTX (d)",
  "IB (d)", 
  "Matematik-evne (i)", 
  "Mindst en forælder højtudd. (d)", 
  "Kvinde (d)", 
  "Konstant", 
  keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
               ,"f"
  )
), dep.var.labels = c("y = Antal forelæsninger"),
report = "vc*", 
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Reference for gymnasiel baggrunds-dummies = STX", 
dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
align = TRUE, 
title = "Regressionsresultater for effekten af karakterbedømmelse og kontroller på antal forelæsninger, den studerende møder op til", 
font.size = "small", 
label = "antal_forel_reg" )

#########
##########BILAG - Antal forelæsninger

m10_kontrol <- lm(antal_forelaesninger ~ eksamensform_dummy + 
                    merit_dummy + 
                    Adg_kvotient + 
                    relevel(as.factor(Adg_type_dummy), ref = "STX") + 
                    matematik_gym + 
                    mindst_en_højtudd + 
                    KVINDE_dummy, data = df)

STAR11 <- stargazer::stargazer(m10_kontrol, type = "latex", covariate.labels = c(
  "Karakterbedømmelse (d)", 
  "Meritstuderende (d)", 
  "Adg. karaktergnst", 
  "HF (d)", 
  "HHX (d)", 
  "HTX (d)",
  "IB (d)", 
  "Matematik-evne (i)", 
  "Mindst en forælder højtudd. (d)", 
  "Kvinde (d)", 
  "Konstant", 
  keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
               ,"f"
  )
), dep.var.labels = c("y = Antal forelæsninger"),
report = "vc*", 
star.cutoffs = c(0.05, 0.01, 0.001), 
notes = "Reference for gymnasiel baggrunds-dummies = STX", 
dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
align = TRUE, 
title = "Regressionsresultater for effekten af karakterbedømmelse og kontroller på antal forelæsninger, den studerende møder op til", 
font.size = "small", 
label = "antal_forel_reg")
##########

#df$ingen_forældre_højest_udd_dummy <- ifelse(df$mor_højest_udd_dummy == 0 & df$far_højest_udd_dummy == 0, 1, 0)

#Medd interaktion

UDD1 <- lm(AndelPoint ~
             eksamensform_dummy + 
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             gender_coded, data = df)

UDD2 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             begge_forældre_højest_udd_dummy +
             gender_coded, data = df)

UDD3 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             mindst_en_højtudd + 
             gender_coded, data = df)

UDD4 <- lm(AndelPoint ~eksamensform_dummy+
             merit_dummy + 
             Adg_kvotient + 
             relevel(as.factor(Adg_type_dummy), ref = "STX")  +
             matematik_gym + 
             ingen_forældre_højest_udd_dummy + 
             gender_coded, data = df)

jtools::plot_coefs(UDD1,
                   UDD2,
                   UDD3,
                   UDD4)

stargazer::stargazer(UDD1,
                     UDD2,
                     UDD3,
                     UDD4, type = "latex", 
                     keep.stat =c("n", "rsq", "adj.rsq", "aic"#,
                                  #"f"
                     ), 
                     dep.var.labels = c("y = Andel point"),
                     report = "vc*", 
                     star.cutoffs = c(0.05, 0.01, 0.001), 
                     notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                     dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                     align = TRUE, 
                     title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point med inddragelse af forældres uddannelsesniveau", 
                     font.size = "small", 
                     covariate.labels = c(
                       "Karakterbedømmelse (d)",
                       "Merit (d)",
                       "Adg. kvotient",
                       "HF (d)",
                       "HHX",
                       "HTX",
                       "International",
                       "Matematik-evne (i)",
                       "Begge forældre højtudd. (d)",
                       "Mindst en forælder højtudd. (d)",
                       "Ingen forældre højtudd. (d)",
                       "Mand (d)",
                       "Konstant"
                     ),
                     
                     float.env = "sidewaystable", label = "table:edu_reg"
)

#Medd interaktion

UDD1_i <- lm(AndelPoint ~
               eksamensform_dummy + 
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX") + matematik_gym + gender_coded, data = df)

UDD2_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               
               matematik_gym + gender_coded +
               begge_forældre_højest_udd_dummy +
               begge_forældre_højest_udd_dummy:eksamensform_dummy, data = df)

UDD3_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               matematik_gym + gender_coded +
               mindst_en_højtudd + 
               mindst_en_højtudd:eksamensform_dummy, data = df)

UDD4_i <- lm(AndelPoint ~eksamensform_dummy+
               merit_dummy + 
               Adg_kvotient + 
               relevel(as.factor(Adg_type_dummy), ref = "STX")  +
               matematik_gym + gender_coded +
               ingen_forældre_højest_udd_dummy + 
               ingen_forældre_højest_udd_dummy:eksamensform_dummy, data = df)

jtools::plot_coefs(UDD1_i,
                   UDD2_i,
                   UDD3_i,
                   UDD4_i)


o <- stargazer::stargazer(UDD1_i,
                          UDD2_i,
                          UDD3_i,
                          UDD4_i, type = "text", 
                          keep.stat =c("f", "n", "rsq", "adj.rsq", "aic"#,
                                       #"f"
                          ), 
                          dep.var.labels = c("y = Andel point"),
                          report = "vc*", 
                          star.cutoffs = c(0.05, 0.01, 0.001), 
                          notes = "Reference for gymnasiel baggrunds-dummies = STX", 
                          dep.var.caption = "", decimal.mark = ",", no.space = TRUE, 
                          align = TRUE, 
                          title = "Regressionsresultater for effekten af karakter som bedømmelse på andel af point med inddragelse af forældres uddannelsesniveau og interaktioner", 
                          font.size = "small", 
                          covariate.labels = c(
                            "Karakterbedømmelse (d)",
                            "Merit (d)",
                            "Adg. kvotient",
                            "HF (d)",
                            "HHX (d)",
                            "HTX (d)",
                            "International (d)",
                            "Matematik-evne (i)",
                            "Mand (d)",
                            "Begge forældre højtudd. (d)",
                            "Karakterbedømmelse*Begge forældre højtudd.",
                            "Mindst en forælder højtudd. (d)",
                            "Karakterbedømmelse*Mindst en forælder højtudd.",
                            "Ingen forældre højtudd. (d)",
                            "Karakterbedømmelse*Ingen forældre højtudd. (d)",
                            "Konstant"
                          ),
                          float.env = "sidewaystable", label = "table:edu_reg_no_int"
)


