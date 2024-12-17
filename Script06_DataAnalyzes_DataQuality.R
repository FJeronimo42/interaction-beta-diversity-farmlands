#### Script 06 - Data analyzes- data quality - Chapter 1 - 2023 #### 
# Poisot et al., 2012
update.packages()

#### Library ----
pacman::p_load(cowplot, tidyverse)

#### Data ----
glimpse(pcad_data)

#### Correlation plot ----
cor1_bwbs <- ggplot(data = pcad_data,
                    mapping = aes(y = WN, x = S)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(size = 2) +
  labs(x = expression(beta[S]), y = expression(beta[WN]))+
  xlim (0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 12))
cor1_bwbs

cor2_osbs <- ggplot(data = pcad_data,
                    mapping = aes(y = OS, x = S)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(size = 2) +
  labs(x = expression(beta[S]),        
       y = expression(paste(beta,`'`[OS])))+
  xlim (0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 12))

cor2_osbs

cor3_stbs <- ggplot(data = pcad_data,
                    mapping = aes(y = ST, x = S)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(size = 2) +
  labs(x = expression(beta[S]), y = expression(beta[ST]))+
  xlim (0, 1) +
  ylim(0, 1) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 12))

cor3_stbs

cor4_oswn <- ggplot(data = pcad_data,
                    mapping = aes(y = OS, x = WN)) +
  geom_abline(intercept = 0, slope = 1, linetype = 2) +
  geom_point(size = 2) +
  labs(x = expression(beta[WN]), 
       y = expression(paste(beta,`'`[OS])))+
  xlim(0, 1) +
  ylim(0, 1) +
  # scale_x_continuous(breaks = seq(0, 1, 0.2)) +
  # scale_y_continuous(breaks = seq(0, 1, 0.2)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(size = 16, face = 'bold'),
        axis.text = element_text(size = 12))

cor4_oswn

corr_boar <- plot_grid(cor1_bwbs, cor2_osbs, cor3_stbs, cor4_oswn,
                       labels = 'auto')
corr_boar

ggsave('Figures/corr_boar.tiff',
       plot = corr_boar, 
       width = 16, 
       height = 15, 
       units = 'cm',
       dpi = 600)
