#### Script 04 - Data analyzes - GLM Model Selection - Chapter 1 - 2023 #### 

#### Library ----
pacman::p_load(betareg, car, DHARMa, effectsize, glmmTMB, MuMIn, sjPlot, 
               tidyverse)

#### Data Wrangling ----
anls_data <- pcad_data %>% 
  select(1:6, 9:17, 
         pca1_1000_llpc, pca1_1000_lcpc, pca1_250_flpc, pca1_250_clpc) %>% 
  rename('LNDS' = pca1_1000_llpc,
         'FRST' = pca1_1000_lcpc,
         'FARM' = pca1_250_flpc,
         'CROP' = pca1_250_clpc) %>% 
  glimpse()

View(anls_data)

#### Model selection for WN ----
##### Full model ----
WN.FULL <- glmmTMB(formula = WN ~ LNDS + FRST + FARM + CROP,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

summary(WN.FULL)

##### Model selection ----
options(na.action = 'na.fail')

mdsl_btwn <- dredge(WN.FULL, m.lim = c(0, 4))

mdsl_btwn

sw(mdsl_btwn)

sw_mdsl_btwn <- as.data.frame(sw(mdsl_btwn)) %>% 
  rownames_to_column(var = 'Predictor') %>% 
  rename('Value' = `sw(mdsl_btwn)`) %>% 
  mutate(Predictor = dplyr::recode(Predictor,
                                   `cond(CROP)` = 'Crop Heterogeneity',
                                   `cond(FRST)` = 'Forest Configuration',
                                   `cond(FARM)` = 'Farm Complexity',
                                   `cond(LNDS)` = 'Landscape Structure')) %>% 
  print()

mdsl_btwn <- as.data.frame(mdsl_btwn)

write.table(mdsl_btwn, 'Results/mdsl_btwn.csv')

##### Best fitted model ---- 
WN.FITT <- glmmTMB(formula = WN ~ CROP + FRST, 
                   data = anls_data,
                   family = beta_family(link = 'logit'))

WN.NULL <- glmmTMB(formula = WN ~ 1, 
                   data = anls_data,
                   family = beta_family(link = 'logit'))

anova(WN.NULL, WN.FITT, test = "Chisq")
summary(WN.FITT)

effectsize(WN.FITT)


simulateResiduals(fittedModel = WN.FITT, plot = TRUE, n = 1000)

#### Model selection for ST ----
##### Full model ----
ST.FULL <- glmmTMB(formula = ST ~ LNDS + FRST + FARM + CROP,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

summary(ST.FULL)

##### Model selection ----
options(na.action = 'na.fail')

mdsl_btst <- dredge(ST.FULL, m.lim = c(0, 4))

mdsl_btst

sw(mdsl_btst)

sw_mdsl_btst <- as.data.frame(sw(mdsl_btst)) %>% 
  rownames_to_column(var = 'Predictor') %>% 
  rename('Value' = `sw(mdsl_btst)`) %>% 
  mutate(Predictor = dplyr::recode(Predictor,
                                   `cond(CROP)` = 'Crop Heterogeneity',
                                   `cond(FRST)` = 'Forest Configuration',
                                   `cond(FARM)` = 'Farm Complexity',
                                   `cond(LNDS)` = 'Landscape Structure')) %>% 
  print()

mdsl_btst <- as.data.frame(mdsl_btst)

write.table(mdsl_btst, 'Results/mdsl_btst.csv')

##### Best fitted model ---- 
ST.FITT <- glmmTMB(formula = ST ~ CROP,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

ST.NULL<- glmmTMB(formula = ST ~ 1,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

anova(ST.NULL, ST.FITT, test = "Chisq")

summary(ST.FITT)

effectsize(ST.FITT)

testResiduals(ST.FITT)

simulateResiduals(fittedModel = ST.FITT, plot = TRUE, n = 1000)




#### Model selection for OS ----
##### Full model ----
OS.FULL <- glmmTMB(formula = OS ~ LNDS + FRST + FARM + CROP,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

summary(OS.FULL)

##### Model selection ----
options(na.action = 'na.fail')

mdsl_btos <- dredge(OS.FULL, m.lim = c(0, 4))

mdsl_btos

sw(mdsl_btos)

sw_mdsl_btos <- as.data.frame(sw(mdsl_btos)) %>% 
  rownames_to_column(var = 'Predictor') %>% 
  rename('Value' = `sw(mdsl_btos)`) %>% 
  mutate(Predictor = dplyr::recode(Predictor,
                                   `cond(CROP)` = 'Crop Heterogeneity',
                                   `cond(FRST)` = 'Forest Configuration',
                                   `cond(FARM)` = 'Farm Complexity',
                                   `cond(LNDS)` = 'Landscape Structure')) %>% 
  print()

mdsl_btos <- as.data.frame(mdsl_btos)
write.table(mdsl_btos, 'Results/mdsl_btos.csv')

##### Best fitted model ----
OS.FITT <- glmmTMB(formula = OS ~ LNDS + FARM,
                   data = anls_data,
                   family = beta_family(link = 'logit'))


OS.NULL <- glmmTMB(formula = OS ~ 1,
                  data = anls_data,
                  family = beta_family(link = 'logit'))

anova(OS.NULL, OS.FITT, test = "Chisq")

summary(OS.FITT)

Anova(OS.FITT)

effectsize(OS.FITT)

testResiduals(OS.FITT)

simulateResiduals(fittedModel = OS.FITT, plot = TRUE, n = 1000)



#### Model selection for STh ----
##### Full model ----
STh.FULL <- glmmTMB(formula = ST.h ~ LNDS + FRST + FARM + CROP,
                    data = anls_data,
                    family = beta_family(link = 'logit'))

summary(STh.FULL)

##### Model selection ----
options(na.action = 'na.fail')

mdsl_btsh <- dredge(STh.FULL, m.lim = c(0, 4))

mdsl_btsh

sw(mdsl_btsh)

sw_mdsl_btsh <- as.data.frame(sw(mdsl_btsh)) %>% 
  rownames_to_column(var = 'Predictor') %>% 
  rename('Value' = `sw(mdsl_btsh)`) %>% 
  mutate(Predictor = dplyr::recode(Predictor,
                                   `cond(CROP)` = 'Crop Heterogeneity',
                                   `cond(FRST)` = 'Forest Configuration',
                                   `cond(FARM)` = 'Farm Complexity',
                                   `cond(LNDS)` = 'Landscape Structure')) %>% 
  print()

mdsl_btsh <- as.data.frame(mdsl_btsh)

write.table(mdsl_btsh, 'Results/mdsl_btsh.csv')

##### Best fitted model ---- 
STh.FITT <- glmmTMB(formula = ST.h ~ FARM,
                    data = anls_data,
                    family = beta_family(link = 'logit'))

STh.NULL <- glmmTMB(formula = ST.h ~ 1,
                   data = anls_data,
                   family = beta_family(link = 'logit'))

anova(STh.NULL, STh.FITT, test = "Chisq")

summary(STh.FITT)

effectsize(STh.FITT)

testResiduals(STh.FITT)

simulateResiduals(fittedModel = STh.FITT, plot = TRUE, n = 1000)



#### Model selection for S ----
##### Full model ----
S.FULL <- glmmTMB(formula = S ~ LNDS + FRST + FARM + CROP,
                  data = anls_data,
                  family = beta_family(link = 'logit'))

summary(S.FULL)

##### Model selection ----
options(na.action = 'na.fail')

mdsl_btss <- dredge(S.FULL, m.lim = c(0, 4))

mdsl_btss

sw(mdsl_btss)

sw_mdsl_btss <- as.data.frame(sw(mdsl_btss)) %>% 
  rownames_to_column(var = 'Predictor') %>% 
  rename('Value' = `sw(mdsl_btss)`) %>% 
  mutate(Predictor = dplyr::recode(Predictor,
                                   `cond(CROP)` = 'Crop Heterogeneity',
                                   `cond(FRST)` = 'Forest Configuration',
                                   `cond(FARM)` = 'Farm Complexity',
                                   `cond(LNDS)` = 'Landscape Structure')) %>% 
  print()


mdsl_btss <- as.data.frame(mdsl_btss)

write.table(mdsl_btss, 'Results/mdsl_btss.csv')

##### Best fitted model ---- 
S.FITT <- glmmTMB(formula = S ~ CROP,
                  data = anls_data,
                  family = beta_family(link = 'logit'))

S.NULL <- glmmTMB(formula = S ~ 1,
                    data = anls_data,
                    family = beta_family(link = 'logit'))

anova(S.NULL, S.FITT, test = "Chisq")

summary(S.FITT)

Anova(S.FITT)

effectsize(S.FITT)

testResiduals(S.FITT)

simulateResiduals(fittedModel = S.FITT, plot = TRUE, n = 1000)


save.image(file = 'Chapter1_Plant-PollinartorNetworkComposition_environ.RData')
