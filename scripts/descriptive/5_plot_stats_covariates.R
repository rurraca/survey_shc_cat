rm(list=ls(all = TRUE))

library(tidyverse)
library(rstatix)
library(ggpubr)
library(rstatix)

source('scripts/descriptive/utils_labels.R')

vars_rm = c('Q23_environmental_concern', 'Q01B_money_free')

#-------------------------------------------------------------------------------
# function
plot_residuals = function(residuals, stats) {
  g = ggplot(residuals, aes(x = cov_cat, y = var_cat, fill = residual)) +
    facet_grid(var~cov, scales = 'free', space = 'free', switch = 'y', labeller = labeller(cov = lab_cov)) + 
    geom_tile() +
    geom_text(aes(label = sig), size = 4, color = 'black') +
    geom_text(data = stats, aes(x = -Inf, y = Inf, label = label), hjust = -0.1, vjust = 1.3,
              size = 2.5, fontface = "bold", inherit.aes = FALSE
    ) +
    coord_fixed(expand= FALSE) +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, na.value = "#bfbfbf") + 
    labs(x = NULL, y = NULL, fill = "Adjusted \n residuals") +
    guides(fill = guide_colorbar(barheight = unit(6, "cm"))) +
    scale_y_discrete(position = "right") +
    theme_bw() +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(strip.background = element_blank()) + 
    theme(strip.text.y.left = element_text(angle = 0, hjust = 1)) +
    theme(legend.position = 'right')
  
  return(g)
}


#-------------------------------------------------------------------------------
# read
stats = read_csv('data/proc/descriptive/results_chi-cramer.csv') %>%
  filter(!(var %in% vars_rm)) %>%
  mutate(sig = case_when(
    (p < 0.05) & (p >= 0.01) ~ '*',
    (p < 0.01) & (p >= 0.001) ~ '**',
    p < 0.001 ~ '***',
    TRUE ~ ''
  )) %>%
  mutate(label = sprintf('V = %.2f%s', cramerv, sig))  %>%
  mutate(cov = factor(cov, lev_cov)) %>%
  mutate(var_type = case_when(
    var %in% lev_m ~ 'motivations',
    var %in% lev_b ~ 'barriers',
    TRUE ~ 'general'
  ))

residuals = read_csv('data/proc/descriptive/results_residuals.csv') %>%
  filter(!(var %in% vars_rm)) %>%
  left_join(stats %>% select(var, cov, p)) %>%
  mutate(residual = if_else(p > 0.05, NA, residual)) %>%
  mutate(sig = case_when(
    (abs(residual) > 1.96) & (abs(residual) <= 2.58) ~ '*',
    (abs(residual) > 2.58) & (abs(residual) <= 3.29) ~ '**',
    abs(residual) > 3.29 ~ '***',
    TRUE ~ ''
  )) %>%
  mutate(cov = factor(cov, lev_cov)) %>%
  mutate(cov_cat = factor(cov_cat, lev_cov_cat)) %>%
  mutate(var_type = case_when(
    var %in% lev_m ~ 'motivations',
    var %in% lev_b ~ 'barriers',
    TRUE ~ 'general'
  ))
  
#-------------------------------------------------------------------------------
# plot

# general
lev_var = c(
  'use', 'buy', 'Q01B_money_free',  
  'Q07_quantity_second_vs_new', 'Q03_channel', 'Q06_buy_for_who',  
  'Q05_buying_frequency', 'Q15_disposal_frequency') #,  'Q23_environmental_concern')
lab_var = c(
  'use' = 'Second-hand use', 
  'buy' = 'Second-hand purchase', 
  'Q01B_money_free' = 'Acquisition mode',
  'Q03_channel' = 'Purchase channel', 
  'Q05_buying_frequency' = 'Purchase frequency', 
  'Q06_buy_for_who' = 'Purchase recipient', 
  'Q07_quantity_second_vs_new' = 'Second-hand vs new quantity', 
  'Q15_disposal_frequency' = 'Disposal frequency',  
  'Q23_environmental_concern' = 'Environmental concern'
)

stats_i = stats %>% 
  filter(var_type == 'general') %>%
  mutate(var = factor(var, lev_var)) %>%
  mutate(var = recode(var, !!!lab_var)) 
residuals_i = residuals %>% 
  filter(var_type == 'general') %>%
  mutate(var = factor(var, lev_var)) %>%
  mutate(var = recode(var, !!!lab_var)) %>%
  mutate(var_cat = factor(var_cat, lev_var_cat)) %>%
  mutate(var_cat = recode(var_cat, !!!lab_var_cat)) %>%
  mutate(cov_cat = factor(cov_cat, lev_cov_cat)) %>%
  mutate(cov_cat = recode(cov_cat, !!!lab_cov_cat)) 
  
g = plot_residuals(residuals=residuals_i, stats=stats_i)
ggsave('figs/descriptive/x2-residuals_general.png', g, width = 22, height = 18, unit = 'cm', bg = 'white', dpi = 400)
ggsave('figs/descriptive/x2-residuals_general.pdf', g, width = 22, height = 18, unit = 'cm', bg = 'white', dpi = 400)
stats_i %>% write_csv('out/descriptive/x2_general.csv')
residuals_i %>% write_csv('out/descriptive/x2-residuals_general.csv')

# -------------------------------------
# motivations
lev_m_cat = c('low', 'medium', 'high')
lab_m_cat = c('low' = 'Low', 'medium' = 'Medium', 'high' = 'High')
stats_i = stats %>% 
  filter(var_type == 'motivations')  %>%
  mutate(var = factor(var, lev_m)) %>%
  mutate(var = recode(var, !!!lab_m)) 
residuals_i = residuals %>% 
  filter(var_type == 'motivations') %>%
  mutate(var = factor(var, lev_m)) %>%
  mutate(var = recode(var, !!!lab_m)) %>% 
  mutate(var_cat = factor(var_cat, lev_m_cat))   %>%
  mutate(var_cat = recode(var_cat, !!!lab_m_cat))  %>%
  mutate(cov_cat = factor(cov_cat, lev_cov_cat)) %>%
  mutate(cov_cat = recode(cov_cat, !!!lab_cov_cat)) 
g = plot_residuals(residuals=residuals_i, stats=stats_i)
ggsave(sprintf('figs/descriptive/x2-residuals_motivations.png'), g, width = 22, height = 22, unit = 'cm', bg = 'white', dpi = 400)
ggsave(sprintf('figs/descriptive/x2-residuals_motivations.pdf'), g, width = 22, height = 22, unit = 'cm', bg = 'white', dpi = 400)
stats_i %>% write_csv('out/descriptive/x2_motivations.csv')
residuals_i %>% write_csv('out/descriptive/x2-residuals_motivations.csv')

# -------------------------------------
# barriers
lev_b_cat = c('low', 'medium', 'high')
lab_b_cat = c('low' = 'Low', 'medium' = 'Medium', 'high' = 'High')
stats_i = stats %>% 
  filter(var_type == 'barriers') %>%
  mutate(var = factor(var, lev_b)) %>%
  mutate(var = recode(var, !!!lab_b)) 
residuals_i = residuals %>% 
  filter(var_type == 'barriers') %>%
  mutate(var = factor(var, lev_b)) %>%
  mutate(var = recode(var, !!!lab_b)) %>% 
  mutate(var_cat = factor(var_cat, lev_b_cat))   %>%
  mutate(var_cat = recode(var_cat, !!!lab_b_cat)) %>%
  mutate(cov_cat = factor(cov_cat, lev_cov_cat)) %>%
  mutate(cov_cat = recode(cov_cat, !!!lab_cov_cat)) 
g = plot_residuals(residuals=residuals_i, stats=stats_i)
ggsave(sprintf('figs/descriptive/x2-residuals_barriers.png'), g, width = 22, height = 23, unit = 'cm', bg = 'white', dpi = 400)
ggsave(sprintf('figs/descriptive/x2-residuals_barriers.pdf'), g, width = 22, height = 23, unit = 'cm', bg = 'white', dpi = 400)
stats_i %>% write_csv('out/descriptive/x2_barriers.csv')
residuals_i %>% write_csv('out/descriptive/x2-residuals_barriers.csv')

