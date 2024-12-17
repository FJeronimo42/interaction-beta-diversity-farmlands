## PCA visualization for 500m buffer
# Eigenvalue scree plot
scev_0500 <- fviz_eig(rpca_500m)
ggsave('Figures/screeplot_0500m.png', 
       plot = scev_0500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU similarity 
susi_0500 <- fviz_pca_ind(rpca_500m,
                          col.ind = 'cos2', # Color by the quality of representation
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " "     
)
ggsave('Figures/susimilarity_0500m.png', 
       plot = susi_0500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# Metrics axis
meax_0500 <- fviz_pca_var(rpca_500m,
                          col.var = 'contrib', # Color by contributions to the PC
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " ", 
                          select.var = list(contrib = 10)
)

ggsave('Figures/metricsaxis_0500m.png', 
       plot = meax_0500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU and Metrics
sume_0500 <- fviz_pca_biplot(rpca_500m, 
                             repel = TRUE, 
                             title = " ",
                             col.var = "#00A8B5", # Variables color
                             col.ind = "#DE4383",  # Individuals color
                             select.var = list(contrib = 10)
)

ggsave('Figures/sumetrics_0500m.png', 
       plot = sume_0500, 
       width = 9, 
       height = 9, 
       dpi = 600)

## PCA visualization for 1000m buffer
# Eigenvalue scree plot
scev_1000 <- fviz_eig(rpca_1000m)
ggsave('Figures/screeplot_1000m.png', 
       plot = scev_1000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU similarity 
susi_1000 <- fviz_pca_ind(rpca_1000m,
                          col.ind = 'cos2', # Color by the quality of representation
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " "     
)
ggsave('Figures/susimilarity_1000m.png', 
       plot = susi_1000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# Metrics axis
meax_1000 <- fviz_pca_var(rpca_1000m,
                          col.var = 'contrib', # Color by contributions to the PC
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " ", 
                          select.var = list(contrib = 10)
)

ggsave('Figures/metricsaxis_1000m.png', 
       plot = meax_1000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU and Metrics
sume_1000 <- fviz_pca_biplot(rpca_1000m, 
                             repel = TRUE, 
                             title = " ",
                             col.var = "#00A8B5", # Variables color
                             col.ind = "#DE4383",  # Individuals color
                             select.var = list(contrib = 10)
)

ggsave('Figures/sumetrics_1000m.png', 
       plot = sume_1000, 
       width = 9, 
       height = 9, 
       dpi = 600)

## PCA visualization for 1500m buffer
# Eigenvalue scree plot
scev_1500 <- fviz_eig(rpca_1500m)
ggsave('Figures/screeplot_1500m.png', 
       plot = scev_1500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU similarity 
susi_1500 <- fviz_pca_ind(rpca_1500m,
                          col.ind = 'cos2', # Color by the quality of representation
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " "     
)
ggsave('Figures/susimilarity_1500m.png', 
       plot = susi_1500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# Metrics axis
meax_1500 <- fviz_pca_var(rpca_1500m,
                          col.var = 'contrib', # Color by contributions to the PC
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " ", 
                          select.var = list(contrib = 10)
)

ggsave('Figures/metricsaxis_1500m.png', 
       plot = meax_1500, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU and Metrics
sume_1500 <- fviz_pca_biplot(rpca_1500m, 
                             repel = TRUE, 
                             title = " ",
                             col.var = "#00A8B5", # Variables color
                             col.ind = "#DE4383",  # Individuals color
                             select.var = list(contrib = 10)
)

ggsave('Figures/sumetrics_1500m.png', 
       plot = sume_1500, 
       width = 9, 
       height = 9, 
       dpi = 600)      

## PCA visualization for 2000m buffer
# Eigenvalue scree plot
scev_2000 <- fviz_eig(rpca_2000m)
ggsave('Figures/screeplot_2000m.png', 
       plot = scev_2000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU similarity 
susi_2000 <- fviz_pca_ind(rpca_2000m,
                          col.ind = 'cos2', # Color by the quality of representation
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " "     
)
ggsave('Figures/susimilarity_2000m.png', 
       plot = susi_2000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# Metrics axis
meax_2000 <- fviz_pca_var(rpca_2000m,
                          col.var = 'contrib', # Color by contributions to the PC
                          gradient.cols = c('#F3AE4B', '#00A8B5',  '#DE4383'),
                          repel = TRUE, 
                          title = " ", 
                          select.var = list(contrib = 10)
)

ggsave('Figures/metricsaxis_2000m.png', 
       plot = meax_2000, 
       width = 9, 
       height = 9, 
       dpi = 600)

# SU and Metrics
sume_2000 <- fviz_pca_biplot(rpca_2000m, 
                             repel = TRUE, title = " ",
                             col.var = "#00A8B5", # Variables color
                             col.ind = "#DE4383",  # Individuals color
                             select.var = list(contrib = 10)
)

ggsave('Figures/sumetrics_2000m.png', 
       plot = sume_2000, 
       width = 9, 
       height = 9, 
       dpi = 600)


#### Figures boards ####
fig_brd_scev <- plot_grid(scev_0500, scev_1000, scev_1500, scev_2000,
                          ncol=2, nrow=2,
                          rel_widths = 1,
                          rel_heights = 1,
                          labels = c('500m', '1000m', '1500m', '2000m'))

ggsave('Figures/figbrdscev.png', 
       plot = fig_brd_scev, 
       width = 9, 
       height = 9, 
       dpi = 600)

fig_brd_susi <- plot_grid(susi_0500, susi_1000, susi_1500, susi_2000,
                          ncol=2, nrow=2,
                          rel_widths = 1,
                          rel_heights = 1,
                          labels = c('500m', '1000m', '1500m', '2000m'))

ggsave('Figures/figbrdsusi.png', 
       plot = fig_brd_susi, 
       width = 9, 
       height = 9, 
       dpi = 600)

fig_brd_meax <- plot_grid(meax_0500, meax_1000, meax_1500, meax_2000,
                          ncol=2, nrow=2,
                          rel_widths = 1,
                          rel_heights = 1,
                          labels = c('500m', '1000m', '1500m', '2000m'))

ggsave('Figures/figbrdmeax.png', 
       plot = fig_brd_meax, 
       width = 9, 
       height = 9, 
       dpi = 600)


fig_brd_sume <- plot_grid(seum_0500, seum_1000, seum_1500, seum_2000,
                          ncol=2, nrow=2,
                          rel_widths = 1,
                          rel_heights = 1,
                          labels = c('500m', '1000m', '1500m', '2000m'))

ggsave('Figures/figbrdsume.png', 
       plot = fig_brd_sume, 
       width = 9, 
       height = 9, 
       dpi = 600)
