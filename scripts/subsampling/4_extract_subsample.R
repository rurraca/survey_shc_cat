rm(list = ls(all = TRUE))

library(tidyverse)
library(lpSolve)
library(ggpubr)

source('src/calc_subsample.R')
source('src/utils.R')


# ------------------------------------------------------------------------------
# params
x = 'adj_cte'
tol_pct = 0.02

# ------------------------------------------------------------------------------
# read 
sample = read_csv('data/proc/sample.csv') 
sample_stats = read_csv('data/proc/sample_stats.csv')
pop_stats = read_csv('data/proc/pop_stats_adj.csv') %>% 
  filter(pop_type == x)

sensitivity = read_csv('data/out/sensitivity.csv')
n_sub = sensitivity %>% 
  filter(tolerances == tol_pct, pop_type == x) %>% 
  pull(sizes)

solution = calc_subsample(df_sample=sample, df_stats_pop=pop_stats, tol_pct=tol_pct, n_sub=n_sub)

if (solution$status != 0) {
  stop("No feasible solution found. Try increasing tol.")
} 
selected <- which(solution$solution == 1)
subsample <- sample[selected, ]

# subsample stats
sub_stats = subsample %>% 
  gather("var", "cat", -ID) %>%
  group_by(var, cat) %>%
  summarize(
    n = n(),
    pp = 100 * n / nrow(subsample)
  )

# check tolerance
check = pop_stats %>% 
  select(var, cat, pp_pop = pp) %>%
  left_join(sub_stats %>% select(var, cat, pp_sub = pp)) %>%
  mutate(diff = pp_pop - pp_sub)
cat("Final subsample size:", nrow(subsample), "\n")
cat('Max difference:', max(abs(check$diff)))

# save
subsample %>%
  write_csv(sprintf('data/out/subsample_tol-%.2f.csv', tol_pct * 100))
sub_stats %>%
  write_csv(sprintf('data/out/subsample_stats_tol-%.2f.csv', tol_pct * 100))


