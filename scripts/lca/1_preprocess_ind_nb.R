rm(list=ls())

library(readxl)
library(tidyverse)


vars_ind = c(
  'A02B_unknown_use', 'A02B_embarrassing', 'A02B_less_quality', 'A02B_no_trendy', 
  'A02B_no_size', 'A02B_same_price', 'A02B_no_sustainable', 'A02B_not_accessible',
  'Q01A_use', 'Q15_disposal_frequency', 'Q05_buying_frequency', 'Q23_environmental_concern',
  'Q03B_channel_new', 'Q06_buy_for_who' 
) 

#-------------------------------------------------------------------------------
df = read_excel("data/data/proc/subsample_proc/answers_849.xlsx", sheet = 'Survey_results_849') %>%
  filter(Q01A_use_buy %in% c('A01A_only_use', 'A01A_no_use_no_buy')) %>%
  filter(Q03B_channel_new != 'A03B_dont_buy_new') %>%
  mutate(Q01A_use = if_else(Q01A_use_buy == 'A01A_only_use', TRUE, FALSE))

Q06 = df %>% 
  dplyr::select(ID_new, A06_me, A06_family, A06_friends, A06_other) %>%
  mutate(across(!ID_new, as.numeric)) %>%
  mutate(across(!ID_new, as.logical)) %>%
  mutate(A06_others = A06_family | A06_friends | A06_other) %>%
  mutate(Q06_buy_for_who = case_when(
    A06_me & !A06_others ~ 'me',
    !A06_me & A06_others ~ 'others',
    A06_me & A06_others ~ 'both',
    TRUE ~ NA
  )) 

df = df %>%
  left_join(Q06 %>% dplyr::select(ID_new, Q06_buy_for_who))
ind = df[vars_ind]

# NA
ind %>% 
  filter(if_any(everything(), is.na))  

# freq 
ind %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100) 
# NA
ind %>% 
  filter(if_any(everything(), is.na)) 

# freq 
ind %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)

# save
ind %>% write_csv('data/proc/lca/ind_nb.csv')



#-------------------------------------------------------------------------------
# collapse indicators - option 1
cat_motivation_3 <- function(x) {
  case_when(
    x %in% c(1, 2) ~ "low",
    x %in% c(3, 4) ~ "medium",
    x %in% c(5, 6) ~ "high",
    TRUE ~ NA
  )
}

motivation_vars =  c(
  'A02B_unknown_use', 'A02B_embarrassing', 'A02B_less_quality', 'A02B_no_trendy', 
  'A02B_no_size', 'A02B_same_price', 'A02B_no_sustainable', 'A02B_not_accessible'
)
ind_proc = ind %>% 
  mutate(across(all_of(motivation_vars), cat_motivation_3))  %>%
  mutate(Q03B_channel_new = case_when(
    Q03B_channel_new %in% c('A03B_online', 'A03B_online>physic') ~ 'online',
    Q03B_channel_new %in% c('A03B_physic', 'A03B_physic>online') ~ 'physical',
    Q03B_channel_new %in% c('A03B_physic=online') ~ 'equal',
    TRUE ~ NA
  )) %>%
  mutate(Q05_buying_frequency = case_when(
    Q05_buying_frequency %in% c('A05_1week', 'A05_2month', 'A05_1month', 'A05_6-11year') ~ 'high',
    Q05_buying_frequency %in% c('A05_3-5year') ~ 'medium',
    Q05_buying_frequency %in% c('A05_2year', 'A05_1year', 'A05_less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q15_disposal_frequency = case_when(
    Q15_disposal_frequency %in% c('A15_2month', 'A15_1month', 'A15_6-11year', 'A15_3-5year') ~ 'high',
    Q15_disposal_frequency %in% c('A15_2year', 'A15_1year') ~ 'medium',
    Q15_disposal_frequency %in% c('A15_less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q23_environmental_concern = case_when(
    Q23_environmental_concern %in% c('A23_no_concern', 'A23_hardly_change_habits') ~ 'low',
    Q23_environmental_concern %in% c('A23_change_habits') ~ 'medium',
    Q23_environmental_concern %in% c('A23_change_many_habits', 'A23_ecoansiety') ~ 'high',
    TRUE  ~ NA
  ))

# NA after
ind_proc %>% 
  filter(if_any(everything(), is.na))

# freq after
ind_proc %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)

# save
ind_proc %>% write_csv('data/proc/lca/ind_nb_proc1.csv')





#-------------------------------------------------------------------------------
cat_motivation_2 <- function(x) {
  case_when(
    x <= 2 ~ "low",
    x > 2 ~ "high",
    TRUE ~ NA
  )
}

ind_proc = ind %>% 
  mutate(across(all_of(motivation_vars), cat_motivation_3))  %>%
  mutate(Q03B_channel_new = case_when(
    Q03B_channel_new %in% c('A03B_online', 'A03B_online>physic') ~ 'online',
    Q03B_channel_new %in% c('A03B_physic', 'A03B_physic>online') ~ 'physical',
    Q03B_channel_new %in% c('A03B_physic=online') ~ 'equal',
    TRUE ~ NA
  )) %>%
  mutate(Q05_buying_frequency = case_when(
    Q05_buying_frequency %in% c('A05_1week', 'A05_2month', 'A05_1month', 'A05_6-11year') ~ 'high',
    Q05_buying_frequency %in% c('A05_3-5year') ~ 'medium',
    Q05_buying_frequency %in% c('A05_2year', 'A05_1year', 'A05_less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q15_disposal_frequency = case_when(
    Q15_disposal_frequency %in% c('A15_2month', 'A15_1month', 'A15_6-11year', 'A15_3-5year') ~ 'high',
    Q15_disposal_frequency %in% c('A15_2year', 'A15_1year') ~ 'medium',
    Q15_disposal_frequency %in% c('A15_less_1year') ~ 'low',
    TRUE  ~ NA
  )) %>%
  mutate(Q23_environmental_concern = case_when(
    Q23_environmental_concern %in% c('A23_no_concern', 'A23_hardly_change_habits') ~ 'low',
    Q23_environmental_concern %in% c('A23_change_habits') ~ 'medium',
    Q23_environmental_concern %in% c('A23_change_many_habits', 'A23_ecoansiety') ~ 'high',
    TRUE  ~ NA
  ))

# freq after
ind_proc %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  print(n = 100)


# save
ind_proc %>% write_csv('data/proc/lca/ind_nb_proc2.csv')

