#### Script 05 - Data visualization - PCA - Chapter 1 - 2023 ####

#### Library ----
pacman::p_load(tidyverse, factoextra, cowplot)

## PCA visualization for 1000m buffer for Landscape Level in Surrouding Landscape
# Eigenvalue scree plot
scev_1000m_llpc <- fviz_eig(rpca_1000m_llpc,
                            barfill = '#355F8DFF',                             
                            barcolor = '#355F8DFF',
                            title = 'Landscape structure',
                            labelsize = 6,
                            pointsize = 3,
                            arrowsize = 1,
                            ggtheme = theme_bw()+ 
                              theme(axis.title = element_text(size = 14, 
                                                              face = 'bold'),
                                    axis.text = element_text(size = 12),
                                    plot.title = element_text(size = 14, face = 'bold')))+
  coord_cartesian(ylim = c(0, 60))+
  xlab('')

scev_1000m_llpc

ggsave('Figures/screeplot_1000m_llpc.png', 
       plot = scev_1000m_llpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU similarity 
susi_1000m_llpc <- fviz_pca_ind(rpca_1000m_llpc,
                                col.ind = 'cos2', # Color by the quality of representation
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Landscape structure',
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))

susi_1000m_llpc

ggsave('Figures/susimilarity_1000m_llpc.png', 
       plot = susi_1000m_llpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# Metrics axis
meax_1000m_llpc <- fviz_pca_var(rpca_1000m_llpc,
                                col.var = 'contrib', # Color by contributions to the PC
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Landscape structure', 
                                select.var = list(contrib = 5),
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))
meax_1000m_llpc

ggsave('Figures/metricsaxis_1000m_llpc.png', 
       plot = meax_1000m_llpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU and Metrics
sume_1000m_llpc <- fviz_pca_biplot(rpca_1000m_llpc, 
                                   repel = TRUE, 
                                   title = 'Landscape structure',
                                   col.var = '#355F8DFF', # Variables color
                                   col.ind = '#4E2A1E',  # Individuals color
                                   select.var = list(contrib = 5),
                                   labelsize = 6,
                                   pointsize = 3,
                                   arrowsize = 1,
                                   ggtheme = theme_bw()+ 
                                     theme(axis.title = element_text(size = 14, 
                                                                     face = 'bold'),
                                           axis.text = element_text(size = 12),
                                           plot.title = element_text(size = 14, 
                                                                     face = 'bold')))

ggsave('Figures/sumetrics_1000m_llpc.png', 
       plot = sume_1000m_llpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

## PCA visualization for 1000m buffer for Class Level in Surrouding Landscape
# Eigenvalue scree plot
scev_1000m_lcpc <- fviz_eig(rpca_1000m_lcpc,
                            barfill = '#1F958BFF',                             
                            barcolor = '#1F958BFF',
                            title = 'Forest configuration',
                            labelsize = 6,
                            pointsize = 3,
                            arrowsize = 1,
                            ggtheme = theme_bw()+ 
                              theme(axis.title = element_text(size = 14, 
                                                              face = 'bold'),
                                    axis.text = element_text(size = 12),
                                    plot.title = element_text(size = 14, 
                                                              face = 'bold')))+
  coord_cartesian(ylim = c(0, 60))+
  ylab('')+
  xlab('')

scev_1000m_lcpc


ggsave('Figures/screeplot_1000m_lcpc.png', 
       plot = scev_1000m_lcpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU similarity 
susi_1000m_lcpc <- fviz_pca_ind(rpca_1000m_lcpc,
                                col.ind = 'cos2', # Color by the quality of representation
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Forest configuration',
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))
                                
ggsave('Figures/susimilarity_1000m_lcpc.png', 
       plot = susi_1000m_lcpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# Metrics axis
meax_1000m_lcpc <- fviz_pca_var(rpca_1000m_lcpc,
                                col.var = 'contrib', # Color by contributions to the PC
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Forest configuration', 
                                select.var = list(contrib = 5),
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))

ggsave('Figures/metricsaxis_1000m_lcpc.png', 
       plot = meax_1000m_lcpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU and Metrics
sume_1000m_lcpc <- fviz_pca_biplot(rpca_1000m_lcpc, 
                                   repel = TRUE, 
                                   title = 'Forest configuration',
                                   col.var = '#1F958BFF', # Variables color
                                   col.ind = '#4E2A1E',  # Individuals color
                                   select.var = list(contrib = 5),
                                   labelsize = 6,
                                   pointsize = 3,
                                   arrowsize = 1,
                                   ggtheme = theme_bw()+ 
                                     theme(axis.title = element_text(size = 14, 
                                                                     face = 'bold'),
                                           axis.text = element_text(size = 12),
                                           plot.title = element_text(size = 14, 
                                                                     face = 'bold')))

ggsave('Figures/sumetrics_1000m_lcpc.png', 
       plot = sume_1000m_lcpc, 
       width = 9, 
       height = 9, 
       dpi = 300)



## PCA visualization for 250m buffer for Landscape Level in Farms
# Eigenvalue scree plot
scev_0250m_flpc <- fviz_eig(rpca_0250m_flpc,
                            barfill = '#EBE51AFF',                            
                            barcolor = '#EBE51AFF',
                            title = 'Farm complexity',
                            labelsize = 6,
                            pointsize = 3,
                            arrowsize = 1,
                            ggtheme = theme_bw()+ 
                              theme(axis.title = element_text(size = 14, 
                                                              face = 'bold'),
                                    axis.text = element_text(size = 12),
                                    plot.title = element_text(size = 14, 
                                                              face = 'bold')))+
  coord_cartesian(ylim = c(0, 60))

scev_0250m_flpc

ggsave('Figures/screeplot_0250m_flpc.png', 
       plot = scev_0250m_flpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU similarity 
susi_0250m_flpc <- fviz_pca_ind(rpca_0250m_flpc,
                                col.ind = 'cos2', # Color by the quality of representation
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Farm complexity',
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))
ggsave('Figures/susimilarity_0250m_flpc.png', 
       plot = susi_0250m_flpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# Metrics axis
meax_0250m_flpc <- fviz_pca_var(rpca_0250m_flpc,
                                col.var = 'contrib', # Color by contributions to the PC
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Farm complexity', 
                                select.var = list(contrib = 5),
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))

ggsave('Figures/metricsaxis_0250m_flpc.png', 
       plot = meax_0250m_flpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU and Metrics
sume_0250m_flpc <- fviz_pca_biplot(rpca_0250m_flpc, 
                                   repel = TRUE, 
                                   title = 'Farm complexity',
                                   col.var = '#EBE51AFF', # Variables color
                                   col.ind = '#4E2A1E',  # Individuals color
                                   select.var = list(contrib = 5),
                                   labelsize = 6,
                                   pointsize = 3,
                                   arrowsize = 1,
                                   ggtheme = theme_bw()+ 
                                     theme(axis.title = element_text(size = 14, 
                                                                     face = 'bold'),
                                           axis.text = element_text(size = 12),
                                           plot.title = element_text(size = 14, 
                                                                     face = 'bold')))

ggsave('Figures/sumetrics_0250m_flpc.png', 
       plot = sume_0250m_flpc, 
       width = 9, 
       height = 9, 
       dpi = 300)


## PCA visualization for 250m buffer for Landscape Level in Crops
# Eigenvalue scree plot
scev_0250m_clpc <- fviz_eig(rpca_0250m_clpc,
                            barfill = '#471264FF',                             
                            barcolor = '#471264FF',
                            title = 'Crop heterogeneity',
                            labelsize = 6,
                            pointsize = 3,
                            arrowsize = 1,
                            ggtheme = theme_bw()+ 
                              theme(axis.title = element_text(size = 14, 
                                                              face = 'bold'),
                                    axis.text = element_text(size = 12),
                                    plot.title = element_text(size = 14, 
                                                              face = 'bold')))+
  coord_cartesian(ylim = c(0, 60))+
  ylab('')

scev_0250m_clpc

ggsave('Figures/screeplot_0250m_clpc.png', 
       plot = scev_0250m_clpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU similarity 
susi_0250m_clpc <- fviz_pca_ind(rpca_0250m_clpc,
                                col.ind = 'cos2', # Color by the quality of representation
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Crop heterogeneity',
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))

ggsave('Figures/susimilarity_0250m_clpc.png', 
       plot = susi_0250m_clpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# Metrics axis
meax_0250m_clpc <- fviz_pca_var(rpca_0250m_clpc,
                                col.var = 'contrib', # Color by contributions to the PC
                                gradient.cols = c('#46ACC8', '#E2D200',   '#B40F20'),
                                repel = TRUE, 
                                title = 'Crop heterogeneity', 
                                select.var = list(contrib = 5),
                                labelsize = 6,
                                pointsize = 3,
                                arrowsize = 1,
                                ggtheme = theme_bw()+ 
                                  theme(axis.title = element_text(size = 14, 
                                                                  face = 'bold'),
                                        axis.text = element_text(size = 12),
                                        plot.title = element_text(size = 14, 
                                                                  face = 'bold')))


ggsave('Figures/metricsaxis_0250m_clpc.png', 
       plot = meax_0250m_clpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

# SU and Metrics
sume_0250m_clpc <- fviz_pca_biplot(rpca_0250m_clpc, 
                                   repel = TRUE, 
                                   title = 'Crop heterogeneity',
                                   col.var = '#471264FF', # Variables color
                                   col.ind = '#4E2A1E',  # Individuals color
                                   select.var = list(contrib = 5),
                                   labelsize = 6,
                                   pointsize = 3,
                                   arrowsize = 1,
                                   ggtheme = theme_bw()+ 
                                     theme(axis.title = element_text(size = 14, 
                                                                     face = 'bold'),
                                           axis.text = element_text(size = 12),
                                           plot.title = element_text(size = 14, 
                                                                     face = 'bold')))
sume_0250m_clpc 

ggsave('Figures/sumetrics_0250m_clpc.png', 
       plot = sume_0250m_clpc, 
       width = 9, 
       height = 9, 
       dpi = 300)

