#### Script 02 - Data analyzes - PCA - Chapter 1 - 2023 #### 

#### Library ----
pacman::p_load(tidyverse, factoextra, cowplot)

#### Data wrangling ----
###### Landscape metrics in landscape level for surrounding landscape ----
llpc_data <- land_data %>% 
  select(1:6,
         starts_with('Lands_')) %>%
  select(-(c(contains('_cv_'),
             contains('_sd_'),
             contains('_shei_')))) %>% 
  filter(!(SU %in% c('SU02', 'SU09'))) %>% 
  glimpse()



##### Landscape metrics in class level for surrounding landscape ----
lcpc_data <- land_data %>% 
  select(1:6, 
         starts_with('Class_') & contains('_3_')) %>%
  select(-(c(contains('_cv_'),
             contains('_sd_')))) %>% 
  filter(!(SU %in% c('SU02', 'SU09'))) %>% 
  glimpse()



##### Landscape metrics in landscape level for farms ----
flpc_data <- farm_data %>% 
  select(1:6,
         starts_with('Lands_')) %>%
  select(-(c(contains('_cv_'),
             contains('_sd_'),
             contains('_shei_')))) %>% 
  filter(!(SU %in% c('SU02', 'SU09'))) %>% 
  glimpse()



##### Landscape metrics in class level for farms ----
fcpc_data <- farm_data %>% 
  select(1:6, 
         starts_with('Class_') & contains('_1_')) %>%
  select(-(c(contains('_cv_'),
             contains('_sd_')))) %>% 
  filter(!(SU %in% c('SU02', 'SU09'))) %>% 
  glimpse()



##### Landscape metrics in landscape level for crops ----
clpc_data <- crop_data %>% 
  select(1:6,
         starts_with('Lands_')) %>%
  select(-(c(contains('_cv_'),
             contains('_sd_'),
             contains('_shei_')))) %>% 
  filter(!(SU %in% c('SU02', 'SU09'))) %>% 
  glimpse()



##### Landscape metrics in class level for crops ----
# ccpc_data <- crop_data %>% 
#   select(1:6, 
#          starts_with('Class_') & contains('_1_')) %>%
#   select(-(c(contains('_cv_'),
#              contains('_sd_')
#   ))) %>% 
#   filter(!(SU %in% c('SU02', 'SU09'))) %>% 
#   glimpse()



# Beta diversity indexes for meta-local 
meta_data <- beta_data %>%
  filter(i %in% 'WEB.META.COM.API.TOT') %>%
  mutate(SU = c('SU01', 'SU03', 'SU04', 'SU05',
                'SU06', 'SU07', 'SU08', 'SU10',
                'SU11', 'SU12', 'SU13', 'SU14', 'SU15'),
         .before = S) %>%
  mutate(Code = c('BRU.BUR', 'CLA.COR', 'JOA.IMP', 'JOS.BRA',
                  'CAM.GRE', 'SIL.MAT', 'HAM.POS', 'WAN.BUD',
                  'ALE.LES', 'VAL.FON', 'WAL.SCO', 'ANT.FON', 'MAI.VAL'),
         .before = S) %>%
  filter(!(j %in% c('WEB.UA02.COM.API.TOT', 'WEB.UA09.COM.API.TOT'))) %>%
  select(-c('WN.repl', 'OS.repl')) %>%
  glimpse()



# Data frame for Principal Component Regression
pcad_data <- full_join(llpc_data, meta_data)


#### PCA ----
##### PCA 500 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_500m_llpc <- llpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_0500m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 1000 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_1000m_llpc <- llpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_1000m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 1500 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_1500m_llpc <- llpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_1500m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 2000 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_2000m_llpc <- llpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_2000m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



#### PCA computing for 500m ----
rpca_500m_llpc <- prcomp(pcad_500m_llpc, scale = T)

# PCA computing for 1000m ----
rpca_1000m_llpc <- prcomp(pcad_1000m_llpc, scale = T)

# PCA computing for 1500m ----
rpca_1500m_llpc <- prcomp(pcad_1500m_llpc, scale = T)

# PCA computing for 2000m ----
rpca_2000m_llpc <- prcomp(pcad_2000m_llpc, scale = T)

# PCA Axis 1 to data
pca1_0500_llpc <- rpca_500m_llpc$x[,1]
pca1_1000_llpc <- rpca_1000m_llpc$x[,1]
pca1_1500_llpc <- rpca_1500m_llpc$x[,1]
pca1_2000_llpc <- rpca_2000m_llpc$x[,1]

# Data frame with PCA axis
pcad_data <- pcad_data %>% 
  mutate(pca1_0500_llpc = (pca1_0500_llpc)) %>%
  mutate(pca1_1000_llpc = (pca1_1000_llpc)) %>%
  mutate(pca1_1500_llpc = (pca1_1500_llpc)) %>%
  mutate(pca1_2000_llpc = (pca1_2000_llpc)) %>% 
  glimpse()

##### PCA 500 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_500m_lcpc <- lcpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_0500m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 1000 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_1000m_lcpc <- lcpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_1000m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 1500 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_1500m_lcpc <- lcpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_1500m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



##### PCA 2000 m (for Landscape metrics in landscape level for surrounding landscape) ----
# Data filter
pcad_2000m_lcpc <- lcpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_2000m')) %>%
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



#### PCA computing for 500m ----
rpca_500m_lcpc <- prcomp(pcad_500m_lcpc, scale = T)

# PCA computing for 1000m ----
rpca_1000m_lcpc <- prcomp(pcad_1000m_lcpc, scale = T)

# PCA computing for 1500m ----
rpca_1500m_lcpc <- prcomp(pcad_1500m_lcpc, scale = T)

# PCA computing for 2000m ----
rpca_2000m_lcpc <- prcomp(pcad_2000m_lcpc, scale = T)

# PCA Axis 1 to data
pca1_0500_lcpc <- rpca_500m_lcpc$x[,1]
pca1_1000_lcpc <- rpca_1000m_lcpc$x[,1]
pca1_1500_lcpc <- rpca_1500m_lcpc$x[,1]
pca1_2000_lcpc <- rpca_2000m_lcpc$x[,1]

# Data frame with PCA axis
pcad_data <- pcad_data %>% 
  mutate(pca1_0500_lcpc = (pca1_0500_lcpc)) %>%
  mutate(pca1_1000_lcpc = (pca1_1000_lcpc)) %>%
  mutate(pca1_1500_lcpc = (pca1_1500_lcpc)) %>%
  mutate(pca1_2000_lcpc = (pca1_2000_lcpc)) %>% 
  glimpse()



#### FARM LEVEL ---- 
##### PCA 250 m (for Landscape metrics in landscape level for farms) ----
# Data filter
pcad_0250m_flpc <- flpc_data %>%
  column_to_rownames('SU') %>% 
  select(ends_with('_0250m')) %>% 
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



#### PCA computing for 500m ----
rpca_0250m_flpc <- prcomp(pcad_0250m_flpc, scale = T)

# PCA Axis 1 to data
pca1_0250_flpc <- rpca_0250m_flpc$x[,1]

# Data frame with PCA axis
pcad_data <- pcad_data %>% 
  mutate(pca1_250_flpc = (pca1_0250_flpc)) %>%
  glimpse()



##### PCA 250 m (for Landscape metrics in class level for farms) ----
# Data filter
pcad_0250m_fcpc <- fcpc_data %>% 
  column_to_rownames('SU') %>% 
  select(ends_with('_0250m')) %>% 
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



#### PCA computing for 500m ----
rpca_0250m_fcpc <- prcomp(pcad_0250m_fcpc, scale = T)

# PCA Axis 1 to data
pca1_0250_fcpc <- rpca_0250m_fcpc$x[,1]

# Data frame with PCA axis
pcad_data <- pcad_data %>% 
  mutate(pca1_250_fcpc = (pca1_0250_fcpc)) %>%
  glimpse()



#### CROP LEVEL ---- 
##### PCA 250 m (for Landscape metrics in landscape level for crops) ----
# Data filter
pcad_0250m_clpc <- clpc_data %>%
  column_to_rownames('SU') %>% 
  select(ends_with('_0250m')) %>% 
  select_if(~all(complete.cases(.))) %>% 
  glimpse()



#### PCA computing for 250m ----
rpca_0250m_clpc <- prcomp(pcad_0250m_clpc, scale = T)

# PCA Axis 1 to data
pca1_0250_clpc <- rpca_0250m_clpc$x[,1]

# Data frame with PCA axis
pcad_data <- pcad_data %>% 
  mutate(pca1_250_clpc = (pca1_0250_clpc)) %>%
  dplyr::select(c(1:6, 308:327, 329)) %>%
  glimpse()

pcad_data %>% 
  glimpse()

save.image(file = 'Chapter1_Plant-PollinartorNetworkComposition_environ.RData')