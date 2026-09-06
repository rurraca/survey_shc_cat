rm(list=ls(all = TRUE))

library(readxl)
library(tidyverse)


cols_cov = c(
  'Q18_gender', 'Q19_age', 'Q21_annual_gross_salary', 'Q22B_degurba', 
  'Q22C_province', 'Q20_education'
  )

#-------------------------------------------------------------------------------
# read
df = read_excel("data/proc/subsample_proc/answers_849.xlsx", sheet = 'Survey_results_849') 
cov = df[c(cols_cov, 'Q01A_use_buy', 'Q03B_channel_new')]


#-------------------------------------------------------------------------------
# collapse covariates

# NA
cov %>% 
  filter(if_any(everything(), is.na))

# freq before
cov %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)

cov_proc = cov %>% 
  mutate(Q19_age = case_when(
    Q19_age %in% c('A19_16-24years', 'A19_25-34years') ~ '16-34',
    Q19_age %in% c('A19_35-44years', 'A19_45-54years') ~ '35-54',
    Q19_age %in% c('A19_55-64years', 'A19_65year_or_more') ~ '55+',
    TRUE ~ Q19_age
  )) %>%
  mutate(Q20_education = case_when(
    Q20_education %in% c('A20_no_education', 'A20_primary', 'A20_secondary') ~ 'low',
    Q20_education %in% c('A20_bachelor_degree', 'A20_higher_vocational_training') ~ 'medium',
    Q20_education %in% c('A20_master_phd') ~ 'high',
    TRUE ~ Q20_education
  )) %>%
  mutate(Q21_annual_gross_salary = case_when(
    Q21_annual_gross_salary %in% c('A21_no_income', 'A21_less_11000', 'A21_11000-16999') ~ '< 17,000',
    Q21_annual_gross_salary %in% c('A21_17000-24999', 'A21_25000-35999') ~ '[17,000, 36,000)',
    Q21_annual_gross_salary %in% c('A21_36000-49999', 'A21_50000_or_more') ~ '> 36,000',
    TRUE ~ Q21_annual_gross_salary
  ))

# set reference lcasses
cov_proc = cov_proc %>%
  mutate(across(where(is.character), factor)) 

# NA
cov_proc %>% 
  filter(if_any(everything(), is.na))

cov_proc %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)



cor = calc_cramerV(df=cov_proc %>% dplyr::select(all_of(cols_cov)))




#-------------------------------------------------------------------------------
# corrleation
cor = calc_cramerV(df=cov_proc %>% dplyr::select(all_of(cols_cov)))
ggplot(cor, aes(x = Var1, y = Var2, fill = CramerV)) +
  geom_tile() +
  scale_fill_viridis_b() +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


#-------------------------------------------------------------------------------
# split into buy / no buy

# buy
cov_proc %>%
  filter(Q01A_use_buy %in% c('A01A_use_buy', 'A01A_only_buy')) %>%
  dplyr::select(-Q01A_use_buy) %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)

cov %>%
  filter(Q01A_use_buy %in% c('A01A_use_buy', 'A01A_only_buy')) %>%
  dplyr::select(-Q01A_use_buy, -Q03B_channel_new) %>%
  write_csv('data/proc/lca/cov_b.csv')
cov_proc %>%
  filter(Q01A_use_buy %in% c('A01A_use_buy', 'A01A_only_buy')) %>%
  dplyr::select(-Q01A_use_buy, -Q03B_channel_new) %>%
  write_csv('data/proc/lca/cov_b_proc.csv')


# dont buy
cov_proc %>%
  filter(Q01A_use_buy %in% c('A01A_only_use', 'A01A_no_use_no_buy')) %>%
  dplyr::select(-Q01A_use_buy) %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)

cov %>%
  filter(Q01A_use_buy %in% c('A01A_only_use', 'A01A_no_use_no_buy')) %>%
  filter(Q03B_channel_new != 'A03B_dont_buy_new') %>%
  dplyr::select(-Q01A_use_buy, -Q03B_channel_new) %>%
  write_csv('data/proc/lca/cov_nb.csv')
cov_proc %>%
  filter(Q01A_use_buy %in% c('A01A_only_use', 'A01A_no_use_no_buy')) %>%
  filter(Q03B_channel_new != 'A03B_dont_buy_new') %>%
  dplyr::select(-Q01A_use_buy, -Q03B_channel_new) %>%
  write_csv('data/proc/lca/cov_nb_proc.csv')




