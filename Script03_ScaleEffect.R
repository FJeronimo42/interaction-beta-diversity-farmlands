#### Script 03 - Data analyzes - Scale of Effect - Chapter 1 - 2023 #### 

#### Scale of Effect for WN ----

#### Library ---- 
pacman::p_load(glmmTMB, tidyverse)

#### Data ---- 
scoe_data <- pcad_data %>% 
  select('WN',
         ends_with('_llpc'),
         ends_with('_lcpc')) %>% 
  glimpse()

View(scoe_data)

#### Modeling loop ----
### Empty list
rslt_list <- data.frame()

### GLM loop
for (i in 2:9) {
  form_list <- as.formula(paste('WN ~', names(scoe_data)[i]))
  mdls_fitt <- glmmTMB(form_list, data = scoe_data, family = beta_family())
  summ_mdls <- summary(mdls_fitt)
  cfct_mdls <- data.frame(
    Predictor = as.character(names(scoe_data)[i]),
    AIC = summ_mdls$AICtab[1]
  )
  rslt_list <- rbind(rslt_list, cfct_mdls)
  row.names(rslt_list) <- NULL
  
}

# Scale of effect results 
rslt_list

save.image(file = 'Chapter1_Plant-PollinartorNetworkComposition_environ.RData')


################################################################################

# #### Older version ####
# ###### Scale of Effect for WN #####
# ansl.land.data.WN <- pcad_data %>% 
#   select('WN',
#          ends_with('_llpc'),
#          ends_with('_lcpc')) %>% 
#   glimpse()
# 
# mod.lis <- data.frame()  # Inicializar mod.lis como dataframe vazio
# 
# for (i in names(ansl.land.data.WN)[-1]) {
#   lin.mod <- lm(WN ~ ., data = ansl.land.data.WN[, c("WN", i)])
#   sum.mod <- summary(lin.mod)
#   coe.mod <- data.frame(
#     r.squared = sum.mod$r.squared,
#     f.value = sum.mod$fstatistic[1],
#     df = sum.mod$fstatistic[2],
#     p.value = sum.mod$coefficients[8],
#     id = i
#   )
#   mod.lis <- rbind(mod.lis, coe.mod)
# }
# 
# mod.lis <- mod.lis %>%
#   format(scientific = FALSE)
# 
# 
# View(mod.lis)
# 
# write.csv(mod.lis, "Results/mod.lis.land.data.WN.csv")
# 
# 
# 
# ###### Scale of Effect for OS #####
# ansl.land.data.OS <- pcad_data %>% 
#   select('OS',
#          ends_with('_llpc'),
#          ends_with('_lcpc')) %>% 
#   glimpse()
# 
# mod.lis <- data.frame()  # Inicializar mod.lis como dataframe vazio
# 
# for (i in names(ansl.land.data.OS)[-1]) {
#   lin.mod <- lm(OS ~ ., data = ansl.land.data.OS[, c("OS", i)])
#   sum.mod <- summary(lin.mod)
#   coe.mod <- data.frame(
#     r.squared = sum.mod$r.squared,
#     f.value = sum.mod$fstatistic[1],
#     df = sum.mod$fstatistic[2],
#     p.value = sum.mod$coefficients[8],
#     id = i
#   )
#   mod.lis <- rbind(mod.lis, coe.mod)
# }
# 
# mod.lis <- mod.lis %>%
#   format(scientific = FALSE)
# 
# 
# View(mod.lis)
# 
# write.csv(mod.lis, "Results/mod.lis.land.data.OS.csv")
# 
# 
# 
# ###### Scale of Effect for ST #####
# ansl.land.data.ST <- pcad_data %>% 
#   select('ST',
#          ends_with('_llpc'),
#          ends_with('_lcpc')) %>% 
#   glimpse()
# 
# mod.lis <- data.frame()  # Inicializar mod.lis como dataframe vazio
# 
# for (i in names(ansl.land.data.ST)[-1]) {
#   lin.mod <- lm(ST ~ ., data = ansl.land.data.ST[, c("ST", i)])
#   sum.mod <- summary(lin.mod)
#   coe.mod <- data.frame(
#     r.squared = sum.mod$r.squared,
#     f.value = sum.mod$fstatistic[1],
#     df = sum.mod$fstatistic[2],
#     p.value = sum.mod$coefficients[8],
#     id = i
#   )
#   mod.lis <- rbind(mod.lis, coe.mod)
# }
# 
# mod.lis <- mod.lis %>%
#   format(scientific = FALSE)
# 
# 
# View(mod.lis)
# 
# write.csv(mod.lis, "Results/mod.lis.land.data.ST.csv")