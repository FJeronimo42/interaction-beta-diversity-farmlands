pacman::p_load(cowplot, tidyverse, patchwork)


sw_fig_btwn <- ggplot(data = sw_mdsl_btwn,
                      mapping = aes(x = reorder(Predictor, -Value),
                                    y = Value,
                                    fill = Predictor))+
  geom_col()+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = '',
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(title = expression(bold(beta[WN])),
       x = '', 
       y = 'Relative weight')+
  scale_x_discrete(labels = c('Crop Heterogeneity' = 'Crop \nHeterogeneity',
                              'Foest Configuration' = 'Forest \nConfiguration',
                              'Farm Complexity' = 'Farm \nComplexity',
                              'Landscape Configuration' = 'Landsacape \nStructure'
  ))+
  geom_text(aes(label = round(Value, digits = 2)), vjust = -0.25) + 
  scale_fill_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))

sw_fig_btwn

sw_fig_btst <- ggplot(data = sw_mdsl_btst,
                      mapping = aes(x = reorder(Predictor, -Value),
                                    y = Value,
                                    fill = Predictor))+
  geom_col()+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = '',
        
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(title = expression(bold(beta[ST])),
       x = '', 
       y = '')+
  scale_x_discrete(labels = c('Crop Heterogeneity' = 'Crop \nHeterogeneity',
                              'Foest Configuration' = 'Forest \nConfiguration',
                              'Farm Complexity' = 'Farm \nComplexity',
                              'Landscape Configuration' = 'Landsacape \nStructure'
  ))+
  geom_text(aes(label = round(Value, digits = 2)), vjust = -0.25)  + 
  #scale_fill_manual(values = c('#63498A', '#FAD510', '#0B775E', '#046C9A'))
  scale_fill_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))

sw_fig_btst

sw_fig_btos <- ggplot(data = sw_mdsl_btos,
                      mapping = aes(x = reorder(Predictor, -Value),
                                    y = Value,
                                    fill = Predictor))+
  geom_col()+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = '',
        
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text.x = element_blank(),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(title = expression(bold(paste(beta,`'`[OS]))),
       x = 'Predictor', 
       y = 'Relative weight')+
  annotate('text',
           x = 4.15,
           y = 0.13,
           label = '0')+
  scale_x_discrete(labels = c('Crop Heterogeneity' = 'Crop \nHeterogeneity',
                              'Foest Configuration' = 'Forest \nConfiguration',
                              'Farm Complexity' = 'Farm \nComplexity',
                              'Landscape Configuration' = 'Landsacape \nStructure'
  ))+
  geom_text(aes(label = round(Value, digits = 2)), vjust = -0.25) + 
  scale_fill_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))

sw_fig_btos

sw_fig_btsh <- ggplot(data = sw_mdsl_btsh,
                      mapping = aes(x = reorder(Predictor, -Value),
                                    y = Value,
                                    fill = Predictor))+
  geom_col()+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = '',
        
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(title = expression(bold(beta[Poll])),
       x = 'Predictor', 
       y = '')+
  scale_x_discrete(labels = c('Crop Heterogeneity' = 'Crop \nHeterogeneity',
                              'Foest Configuration' = 'Forest \nConfiguration',
                              'Farm Complexity' = 'Farm \nComplexity',
                              'Landscape Configuration' = 'Landsacape \nStructure'
  ))+
  geom_text(aes(label = round(Value, digits = 2)), vjust = -0.25) + 
  scale_fill_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))

sw_fig_btsh

sw_fig_btss <- ggplot(data = sw_mdsl_btss,
                      mapping = aes(x = reorder(Predictor, -Value),
                                    y = Value,
                                    fill = Predictor))+
  geom_col()+
  ylim(0, 1)+
  theme_bw()+
  theme(legend.position = '',
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text = element_blank(),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(title = expression(bold(beta[S])),
       x = 'Predictor', 
       y = 'Relative weight')+
  scale_x_discrete(labels = c('Crop Heterogeneity' = 'Crop \nHeterogeneity',
                              'Foest Configuration' = 'Forest \nConfiguration',
                              'Farm Complexity' = 'Farm \nComplexity',
                              'Landscape Configuration' = 'Landsacape \nStructure'
  ))+
  geom_text(aes(label = round(Value, digits = 2)), vjust = -0.25) + 
  scale_fill_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))

sw_fig_btss

subtitle <- data.frame(x = c(0.25, 0.5, 0.75, 1), 
                       y = c(2, 2, 2, 2), 
                       z = c('Crop \nHeterogeneity',  'Farm \nComplexity',
                             'Forest \nConfiguration', 'Landsacape \nStructure'))

subtitle

subtitle.fig <- ggplot(data = subtitle,
                       mapping = aes(x = x,
                                     y = y,
                                     color = z))+
  geom_point(size = 15, shape = 15)+
  labs(x = '', y = '')+
  scale_color_manual(values = c('#471264FF', '#EBE51AFF', '#1F958BFF', '#355F8DFF'))+
  #scale_color_manual(values = c('#F3C623', '#028391', '#1F509A', '#FF6500'))+
  geom_text(aes(label = z), color = 'black', vjust = 2, size = 4)+
  xlim(0, 1.25) +
  ylim(1.9, 2.1) +
  theme_void() +
  theme(legend.position = '',
        plot.background = element_rect(fill = 'white',
                                       color= 'white'))


subtitle.fig

ggsave('Figures/label_ptbr.png', 
       plot = subtitle.fig, 
       width = 33,
       height = 5.5,
       units = 'cm',
       dpi = 600)

sw_fig_grid <- plot_grid(plot_grid(sw_fig_btwn, sw_fig_btst, sw_fig_btos,
                                   sw_fig_btsh,
                                   subtitle.fig,
                                   labels = 'auto',
                                   rel_widths = 1,
                                   rel_heights = 1,
                                   nrow = 2,
                                   ncol = 2),
                         subtitle.fig,
                         nrow = 2,
                         ncol = 1,
                         rel_widths = c(2, 0.5),
                         rel_heights = c(2, 0.5))

sw_fig_grid



ggsave('Figures/sw_fig_grid.png', 
       plot = sw_fig_grid, 
       width = 22,
       height = 24.75,
       units = 'cm',
       dpi = 600)







scev_fig_grid <- cowplot::plot_grid(scev_1000m_llpc, scev_1000m_lcpc, 
                                    scev_0250m_flpc, scev_0250m_clpc,
                                    ncol = 2)

scev_fig_grid

ggsave('Figures/scev_fig_grid.png', 
       plot = scev_fig_grid, 
       width = 30, 
       height = 30,
       units = 'cm',
       dpi = 600)


sume_fig_grid <- cowplot::plot_grid(sume_1000m_llpc, sume_1000m_lcpc, 
                                    sume_0250m_flpc, sume_0250m_clpc,
                                    nrow = 2,
                                    ncol = 2)
sume_fig_grid

ggsave('Figures/sume_fig_grid.png', 
       plot = sume_fig_grid, 
       width = 30, 
       height = 30,
       units = 'cm',
       dpi = 600)


# General results plot
boxp_data <- anls_data %>% 
  select(SU, S:ST.lh) %>% 
  pivot_longer(cols = 2:8, names_to = 'Index', values_to = 'Value') %>% 
  mutate(Index = factor(Index, 
                        levels = c('WN', 'OS', 'ST', 'ST.l', 'ST.h','ST.lh', 'S'))) %>%
  mutate(Index = recode(Index,
                        ST.h = 'Poll', 
                        ST.l = 'Crop', 
                        ST.lh = 'Crop-Poll')) %>% 
  filter(Index %in% c('WN', 'OS', 'ST', 'Poll', 'Crop', 'Crop-Poll')) %>%
  glimpse()



genr_figu <- ggplot(data = boxp_data,
                    mapping = aes(x = Index,
                                  y = Value))+
  geom_boxplot(fill = 'grey75',
               staplewidth = 0.25)+
  theme_bw()+
  theme(legend.position = '',
        axis.title = element_text(size = 14, 
                                  face = 'bold'),
        axis.text = element_text(size = 12),
        plot.title = element_text(size = 14, 
                                  face = 'bold'))+
  labs(x = 'β-diversity index',
       y = 'Dissimilarity value')+
  stat_summary(fun = mean, 
               color = 'grey95', 
               position = position_dodge(0.5),
               geom = 'point', 
               shape = 18, 
               size = 3, 
               show.legend = FALSE)+
  lims(y = c(0,1))



genr_figu 


ggsave('Figures/summ_results.png', 
       plot = genr_figu, 
       width = 15, 
       height = 15,
       units = 'cm',
       dpi = 600)

save.image(file = 'Chapter1_Plant-PollinartorNetworkComposition_environ.RData')
