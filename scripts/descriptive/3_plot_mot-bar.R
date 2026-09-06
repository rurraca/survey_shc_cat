rm(list=ls(all = TRUE))

library(readxl)
library(tidyverse)
library(rstatix)
library(ggpubr)

source('scripts/descriptive/utils_labels.R')

#-------------------------------------------------------------------------------
lev_group = c('non-buyers', 'buyers')
lab_group = c(
  'non-buyers' = 'Non-buyers of second-hand clothing', 
  'buyers' = 'Buyers of second-hand clothing'
)
pal_group = c(
  'non-buyers' = '#56B4E9', 
  'buyers' = '#E69F00'
)

# ------------------------------------------------------------------------------------------------
vars = c(c('id', 'buy', 'use'), lev_m, lev_b)
df = read_csv('data/proc/descriptive/proc_descritpive.csv') %>%
  dplyr::select(all_of(vars)) 
dfb = df %>%
  filter(buy == 'buyers') %>%
  dplyr::select(id, any_of(lev_m)) 
dfn = df %>%
  filter(buy == 'non-buyers') %>%
  dplyr::select(id, any_of(lev_b))


calc_friedman = function(data, levels, labels, title, group, space_b = 0.1) {
  
  # ranking
  
  mat = data %>% dplyr::select(-id) %>% as.matrix()
  
  ranks <- t(apply(mat, 1, rank))
  colnames(ranks) <- colnames(mat)
  
  avg_ranks = colMeans(ranks) %>%
    as_tibble() %>%
    mutate(var = colnames(mat)) 
  lev_var = avg_ranks %>% arrange(value) %>% pull(var)
  avg_ranks = avg_ranks %>%
    mutate(var = factor(var, rev(lev_var))) %>%
    arrange(var)

  gA = ggplot(avg_ranks, aes(x = var, y = value)) +
    geom_point() +
    labs(x = NULL, y = 'Mean rank') +
    coord_cartesian(ylim = c(1, 6)) +
    scale_y_continuous(breaks = seq(1, 6, 1)) +
    theme_bw() +
    theme(panel.border = element_blank()) + 
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
    theme(axis.line.y = element_line()) +
    theme(panel.grid.minor.y = element_blank())
  
  # distribution
  
  long = data %>%
    gather(var, value, -id) %>%
    mutate(var = factor(var, rev(lev_var))) %>%
    mutate(group = group)
  
  gB = ggplot(long, aes(x = var, y = value, fill = group, group = var)) +
    geom_boxplot(size = .4, outlier.size = .4, show.legend = FALSE) + #fill = '#0099cc',
    stat_summary(fun = mean, geom = "point", shape = 23, size = 1.5, fill = "black", color = "black")+
    theme_bw() +
    labs(x = title, y = 'Likert scale (1 to 6)') +
    scale_fill_manual(values = pal_group, labels = lab_group, breaks = names(pal_group)) +
    scale_x_discrete(labels = labels) + 
    theme(panel.border = element_blank()) + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1), axis.ticks.x = element_blank()) +
    theme(axis.line.y = element_line()) +
    theme(panel.grid.major.x = element_blank())
  
  res = long %>% friedman_test(value ~ var |id)
  print(res)
  
  # pannel
  g = ggpubr::ggarrange(
    gA + theme(plot.margin = unit(c(.1, .1, .1, .1), 'cm')), 
    gB + theme(plot.margin = unit(c(.1, .1, space_b, .1), 'cm')), 
    nrow = 2, ncol = 1, heights = c(.4, 1))
  return(g)
}

g1 = calc_friedman(data = dfb, labels = lab_m, title = '\n Motivations for second-hand clothing purchase', group = 'buyers', space_b = .1)  
g2 = calc_friedman(data = dfn, labels = lab_b, title = 'Barriers for second-hand clothing purchase', group = 'non-buyers', space_b = 0.1)  
g_empty = ggplot() + geom_blank() + theme_bw() + theme(panel.border = element_blank())
g = ggpubr::ggarrange(
  g_empty,
  g1  + theme(plot.margin = unit(c(.1, .1, .1, .4), 'cm')), 
  g2 + theme(plot.margin = unit(c(.1, .1, .1, .4), 'cm')),
  g_empty,
  nrow = 1, ncol = 4, widths = c(1.5, 1 + 7, 1 + 8, 1.5), labels = c('', 'a', 'b'))
ggsave('figs/descriptive/friedman.png', g, width = 23, height = 12, unit = 'cm', bg = 'white', dpi = 400)
ggsave('figs/descriptive/friedman.pdf', g, width = 25, height = 12, unit = 'cm', bg = 'white')
