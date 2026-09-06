rm(list=ls(all = TRUE))

library(readxl)
library(tidyverse)
library(magrittr)

source('scripts/descriptive/utils_labels.R')

# ------------------------------------------------------------------------------
# df
df = read_excel("data/proc/subsample_proc/answers_849.xlsx", sheet = 'Survey_results_849') %>%
  rename(id = ID_new) %>%
  #filter(Q03B_channel_new != 'A03B_dont_buy_new') %>%
  mutate(buy = if_else(Q01A_use_buy %in% c('A01A_only_buy', 'A01A_use_buy'), 'buyers', 'non-buyers')) %>%
  mutate(use = if_else(Q01A_use_buy %in% c('A01A_only_use', 'A01A_use_buy'), 'users', 'non-users')) %>%
  mutate(Q03_channel = if_else(buy == 'buyers', Q03A_channel_second, Q03B_channel_new)) %>%
  mutate(Q03_channel = str_replace(Q03_channel, 'A03A_', '')) %>%
  mutate(Q03_channel = str_replace(Q03_channel, 'A03B_', '')) %>%
  mutate(Q05_buying_frequency = str_replace(Q05_buying_frequency, 'A05_', ''))  %>%
  mutate(Q15_disposal_frequency = str_replace(Q15_disposal_frequency, 'A15_', ''))  %>%
  mutate(across(starts_with("A06"), as.numeric)) %>%
  mutate(A06_others = A06_family | A06_friends | A06_other) %>%
  mutate(Q06_buy_for_who = case_when(
    A06_me & !A06_others ~ 'me',
    !A06_me & A06_others ~ 'others',
    A06_me & A06_others ~ 'both',
    TRUE ~ NA
  )) %>%
  mutate(across(starts_with("Q08"), as.integer)) %>%
  mutate(Q08_occasional = if_else((Q08_occasional_wear_maternity == 1) | (Q08_occasional_wear_suits == 1), 1, 0)) %>%
  mutate(Q08_bags_accessories = if_else((Q08_bags == 1) | (Q08_textile_apparel_accessories == 1), 1, 0)) %>%
  dplyr::select(-Q08_other, -Q08_leggins_stockings_tights_socks_underwear_swimwear, 
                -Q08_occasional_wear_suits, -Q08_occasional_wear_maternity, 
                -Q08_textile_apparel_accessories, -Q08_bags) %>%
  dplyr::select(
    id,
    buy, use, Q01A_use_buy, Q01B_money_free,
    Q07_quantity_second_vs_new, Q03_channel, Q05_buying_frequency, Q15_disposal_frequency, Q06_buy_for_who, Q23_environmental_concern,
    starts_with('Q08'), starts_with('A02A'), starts_with('A02B'), any_of(lev_cov)) %T>%
  write_csv('data/proc/descriptive/proc_descritpive.csv')


# ------------------------------------------------------------------------------
# collapse categories
cat_motivation_3 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ "low",
    x %in% c(3, 4) ~ "medium",
    x %in% c(5, 6) ~ "high",
    TRUE ~ NA
  )
}

df_proc = df  %>%
  mutate(across(all_of(lev_m), cat_motivation_3))  %>%
  mutate(across(all_of(lev_b), cat_motivation_3))  %>%
  mutate(Q19_age = case_when(
    Q19_age %in% c('A19_16-24years', 'A19_25-34years') ~ '16-34',
    Q19_age %in% c('A19_35-44years', 'A19_45-54years') ~ '35-54',
    Q19_age %in% c('A19_55-64years', 'A19_65year_or_more') ~ '55+',
    TRUE ~ Q19_age
  )) %>%
  mutate(Q21_annual_gross_salary = case_when(
    Q21_annual_gross_salary %in% c('A21_no_income', 'A21_less_11000', 'A21_11000-16999') ~ '< 17,000',
    Q21_annual_gross_salary %in% c('A21_17000-24999', 'A21_25000-35999') ~ '[17,000, 36,000)',
    Q21_annual_gross_salary %in% c('A21_36000-49999', 'A21_50000_or_more') ~ '> 36,000',
    TRUE ~ Q21_annual_gross_salary
  )) %>% 
  mutate(Q03_channel = case_when(
    Q03_channel %in% c('online', 'online>physic') ~ 'online',
    Q03_channel %in% c('physic', 'physic>online') ~ 'physical',
    Q03_channel %in% c('physic=online') ~ 'equal',
    TRUE ~ NA
  )) %>%
  mutate(Q07_quantity_second_vs_new = case_when( 
    Q07_quantity_second_vs_new %in% c('A07_less', 'A07_much_less')  ~ 'less',
    Q07_quantity_second_vs_new %in% c('A07_more', 'A07_much_more', 'A07_all')  ~ 'more',
    Q07_quantity_second_vs_new %in% c('A07_same')  ~ 'same',
    TRUE ~ NA
  )) %>%
  mutate(Q05_buying_frequency = case_when(
    Q05_buying_frequency %in% c('1week', '2month', '1month', '6-11year') ~ 'high',
    Q05_buying_frequency %in% c('3-5year') ~ 'medium',
    Q05_buying_frequency %in% c('2year', '1year', 'less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q15_disposal_frequency = case_when(
    Q15_disposal_frequency %in% c('2month', '1month', '6-11year', '3-5year') ~ 'high',
    Q15_disposal_frequency %in% c('2year', '1year') ~ 'medium',
    Q15_disposal_frequency %in% c('less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q23_environmental_concern = case_when(
    Q23_environmental_concern %in% c('A23_no_concern', 'A23_hardly_change_habits') ~ 'low',
    Q23_environmental_concern %in% c('A23_change_habits') ~ 'medium',
    Q23_environmental_concern %in% c('A23_change_many_habits', 'A23_ecoansiety') ~ 'high',
    TRUE  ~ NA
  )) %T>%
  write_csv('data/proc/descriptive/proc_descritpive_grouped.csv')
  
  