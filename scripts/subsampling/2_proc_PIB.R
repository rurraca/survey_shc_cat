rm(list = ls(all = TRUE))

library(tidyverse)


# ------------------------------------------------------------------------------
# we asume PIB_increase == income_increae
# https://govern.cat/gov/notes-premsa/752612/el-govern-preveu-que-l-economia-catalana-creixi-un-25-percent-el-2025-i-un-21-percent-el-2026?utm_source=chatgpt.com
d_pib_2023 = 1.026
d_pib_2024 = 1.036
d_pib_2025 = 1.025 # estimate,
factor = d_pib_2023 * d_pib_2024 * d_pib_2025

# ------------------------------------------------------------------------------
# fit a log-normal distribution to the deciles adjusted to 2025

# adjust the deciles to 2025
deciles = read_csv('data/raw/pib_deciles_2022.csv') %>%
  mutate(pib_25 = pib_22 * factor)  %>%
  mutate(decil = decil / 10)

# fit the log-normal
z <- qnorm(deciles$decil)
log_pib <- log(deciles$pib_25)
fit <- lm(log_pib ~ z)
mu_hat <- coef(fit)[1]
sigma_hat <- coef(fit)[2]

# evaluate the quality of the fit
fitted_quantiles <- exp(mu_hat + sigma_hat * z)
plot(deciles$pib_25, fitted_quantiles,
     xlab = "Observed deciles",
     ylab = "Fitted log-normal deciles",
     main = "Log-normal fit using deciles")
abline(0, 1, col = "red")

# function to calculate pct between two values
calc_pct_ln = function(min, max) {
  
  pct = 
    plnorm(max, meanlog=mu_hat, sdlog=sigma_hat) - 
    plnorm(min, meanlog=mu_hat, sdlog=sigma_hat)
  
  return(pct)
}
  

# ------------------------------------------------------------------------------
# adjust the intervals from 22 to 25
intervals = read_csv('data/raw/pib_intervals_2022.csv') %>%
  mutate(max_22 = if_else(interval == 'R21_50000_mes', 1e9, max_22)) %>%
  mutate(max_22_round = if_else(interval == 'R21_50000_mes', 1e9, max_22_round)) %>%
  mutate(min_25 = min_22 * factor)  %>%
  mutate(max_25 = max_22 * factor) %>%
  mutate(pct_25_cte = NA) %>%
  mutate(pct_25_ln = NA) %>%
  mutate(d_cte = pct_23 / (max_25 - min_25)) # constant  density

# recalculate population pct
# from [min_25, max_25] to [min_22_round, max_22_round]

# option 1) assume constant densitiy within each salari interval
for(i in 1:nrow(intervals)) {
  
  row = intervals %>% slice(i)
  
  if (i == 1) {
    x = row['d_cte'] * (row['max_22_round'] - row['min_25'])
                    
  } else if (i == nrow(intervals)) {
    row_prev = intervals %>% slice(i-1)
    x = row_prev['d_cte'] * (row['min_25'] - row['min_22_round']) + row['pct_23'] 
    
  } else {
    row_prev = intervals %>% slice(i-1)
    x = row_prev['d_cte'] * (row['min_25'] - row['min_22_round']) + row['d_cte'] * (row['max_22_round'] - row['min_25'])
    
  }
  intervals[i, 'pct_25_cte'] = x
}


# option 2) log normal
for(i in 1:nrow(intervals)) {
  intervals[i, 'pct_25_ln'] = calc_pct_ln(min=intervals$min_22_round[i], max=intervals$max_22_round[i]) * 100
}

# check
sum(intervals['pct_25_cte'])
sum(intervals['pct_25_ln'])


# ------------------------------------------------------------------------------
# update pop_stats
aux = bind_rows(
  intervals %>% select(cat = interval, pp = pct_25_cte) %>% mutate(pop_type='adj_cte'),
  intervals %>% select(cat = interval, pp = pct_25_ln) %>% mutate(pop_type='adj_ln')
) %>%
  mutate(var='P21_salari_brut_anual') %>%
  mutate(n=NA)

pop_stats = read_csv('data/proc/pop_stats.csv') 
bind_rows(
    pop_stats %>% mutate(pop_type = 'raw'),
    pop_stats %>% filter(var != 'P21_salari_brut_anual') %>% mutate(pop_type = 'adj_cte'),
    pop_stats %>% filter(var != 'P21_salari_brut_anual') %>% mutate(pop_type = 'adj_ln')) %>%
  bind_rows(aux) %>%
  arrange(pop_type, var, cat) %>%
  write_csv('data/proc/pop_stats_adj.csv')

