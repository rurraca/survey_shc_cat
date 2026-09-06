rm(list=ls(all = TRUE))

library(tidyverse)
library(rstatix)
library(ggpubr)


source('scripts/descriptive/utils_labels.R')


# ------------------------------------------------------------------------------------------------

calc_stats = function(y , x) {
  
  tbl = table(x, y)
  
  res_chi <- chisq.test(tbl)
  res_cramer = cramer_v(tbl)
  
  stats_i = tibble(
    x2 = res_chi$statistic, 
    p = res_chi$p.value, 
    cramerv = res_cramer
  )  
  
  residuals_i = res_chi$residuals %>%  
    as.data.frame() %>%
    rename(x_cat = x, y_cat = y, residual = Freq) %>%
    mutate(significant = abs(residual) > 1.96)
  
  return(list('stats' = stats_i, 'residuals' = residuals_i))
}

# ------------------------------------------------------------------------------------------------
vars = c(c('buy', 'use', 'Q01B_money_free', 'Q07_quantity_second_vs_new', 'Q03_channel', 
           'Q05_buying_frequency', 'Q15_disposal_frequency', 'Q06_buy_for_who', 'Q23_environmental_concern'), lev_m, lev_b)
covs = c('Q18_gender', 'Q19_age', 'Q21_annual_gross_salary', 'Q22B_degurba', 'Q22C_province')

df = read_csv('data/proc/descriptive/proc_descritpive_grouped.csv') %>%
  select(all_of(vars), all_of(covs)) %>%
  mutate(Q01B_money_free = if_else(Q01B_money_free == 'A01B_no_use_no_buy', NA, Q01B_money_free))

stats = tibble()
residuals = tibble()

for (var in vars) {
  for (cov in covs) {
    print(sprintf('%s, %s', var, cov))
    
    if (var %in% lev_m) {
      aux = df %>% filter(buy == 'buyers')
    } else if (var %in% lev_b) {
      aux = df %>% filter(buy == 'non-buyers') 
    } else
      aux = df
    
    out_i = calc_stats(x = aux %>% pull(cov), y = aux %>% pull(var))
    
    stats_i = out_i[['stats']] %>%
      mutate(var = var, cov = cov)
    
    residuals_i = out_i[['residuals']] %>%
      mutate(var = var, cov = cov) %>%
      rename(var_cat = y_cat, cov_cat = x_cat)

    stats = bind_rows(stats, stats_i)
    residuals = bind_rows(residuals, residuals_i)
    }
}
stats %>%
  write_csv('data/proc/descriptive/results_chi-cramer.csv')
residuals %>%
  write_csv('data/proc/descriptive/results_residuals.csv')
