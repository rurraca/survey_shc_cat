rm(list = ls(all = TRUE))

library(tidyverse)
library(magrittr)
library(lpSolve)
library(ggpubr)

source('src/calc_subsample.R')
source('src/utils.R')

# ------------------------------------------------------------------------------

# read 
sample = read_csv('data/proc/sample.csv') 
sample_stats = read_csv('data/proc/sample_stats.csv') 


out = tibble()
for (i in c('raw',  'adj_cte')) {
  
  pop_stats = read_csv('data/proc/pop_stats_adj.csv') %>%
    filter(pop_type == i)
  
  # calc max subsample size (for perfect match)
  all = full_join(sample_stats, pop_stats %>% rename(n_pop = n, pp_pop = pp), by = c("var", "cat")) %>%
    select(var, cat, n, n_pop, pp, pp_pop) %>%
    mutate(max_n = n / (pp_pop / 100)) 
  max_n = floor(min(all$max_n))
  cat("Maximum feasible subsample size:", max_n, "\n")
  
  # sensitivity analysis
  tolerances = seq(0, 0.05, 0.0025)
  sizes = seq(max_n, max_n + 300, 10)
  
  df = expand_grid(tolerances, sizes) %>%
    mutate(solution = FALSE)
  
  for (t in tolerances) {
    for (n in sizes) {
      solution = calc_subsample(df_sample=sample, df_stats_pop=pop_stats, tol_pct=t, n_sub=n)
      if (solution$status == 0) {
        df = df %>% mutate(solution = if_else(tolerances == t & sizes == n, TRUE, solution))
      } 
    }
  }
  out = bind_rows(out, df %>% mutate(pop_type = i))
}


# ------------------------------------------------------------------------------
# plot 

# filter maximum size per tolerance
stats = out %>% 
  filter(solution) %>%
  group_by(pop_type, tolerances) %>%
  filter(sizes == max(sizes)) %>%
  ungroup() %T>% 
  write_csv('data/out/sensitivity.csv')


g = ggplot(stats, aes(x = tolerances * 100, y = sizes, color = pop_type)) +
  geom_point() +
  geom_line() +
  labs(x = 'Tolerance [%]', y = 'Subsample size', color = NULL) +
  scale_color_manual(values = pal_pop_type, labels = lab_pop_type) +
  theme_bw(base_family = 'Arial') +
  theme(legend.position = 'top')
ggsave('figs/sensitivty.png', g, width = 16, height = 12, units = 'cm', dpi = 500, bg = 'white')

