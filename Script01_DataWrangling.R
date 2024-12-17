#### Script 01 - Data wrangling - Chapter 1 - 2023 #### 

#### Library ----
pacman::p_load(tidyverse)

dir.create('Figures')
dir.create('Results')

#### Data frames ----

# landscape metrics from landscape
land_data <- read_csv('Data/miringuava_landscapemetrics_lands_fulldata.csv') %>% 
  select(-1) %>% 
  slice(-(16:22)) %>% 
  glimpse()

# landscape metrics from farms
farm_data <- read_csv('Data/miringuava_landscapemetrics_farms_fulldata.csv') %>% 
  select(-1) %>% 
  glimpse()

# landscape metrics from crops
crop_data <- read_csv('Data/miringuava_landscapemetrics_crops_fulldata.csv') %>% 
  select(-1) %>% 
  glimpse()

# beta diversity indexes
beta_data <- read_csv('Data/beta.com.api.tot.csv') %>% 
  select(-1) %>% 
  filter(!(i %in% c('WEB.CONV.COM.API.TOT', 
                    'WEB.ORGA.COM.API.TOT'))) %>%
  filter(!(j %in% c('WEB.CONV.COM.API.TOT', 
                    'WEB.ORGA.COM.API.TOT'))) %>% 
  glimpse()
View(beta_data)

save.image(file = 'Chapter1_Plant-PollinartorNetworkComposition_environ.RData')
