rm(list=ls(all = TRUE))

library(readxl)
library(tidyverse)


cols_ind = c(
  'A02A_consumption_patterns', 'A02A_sustainable', 'A02A_cheap', 'A02A_unique_piece', 
  'A02A_shopping_experience', 'A02A_trendy', 'A02A_quality', 'Q03A_channel_second',
  'Q05_buying_frequency', 'Q07_quantity_second_vs_new', 
  'Q06_buy_for_who', 'Q01A_use', 'Q01B_money_free', 
  'Q23_environmental_concern',
  'SR_average',
  "Q08n_general_clothing", "Q08n_shoes", "Q08n_occasional_clothing", "Q08n_sportswear", "Q08n_children_clothing", "Q08n_other",              
  'Q15_disposal_frequency'
  ) 


#-------------------------------------------------------------------------------
df = read_excel("data/proc/subsample_proc/answers_849.xlsx", sheet = 'Survey_results_849') %>%
  filter(Q01A_use_buy %in% c('A01A_use_buy', 'A01A_only_buy')) %>%
  mutate(Q01A_use = if_else(Q01A_use_buy == 'A01A_use_buy', TRUE, FALSE))

# extra columns
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

Q08 = df %>% 
  dplyr::select(ID_new, starts_with('Q08')) %>%
  mutate(across(!ID_new, as.numeric)) %>%
  mutate(across(!ID_new, as.logical)) %>%
  mutate(Q08n_general_clothing = Q08_jackets_coats | Q08_sweaters_midlayers | Q08_dresses_skirts_jumpsuits | Q08_tshirts_shirts_blouses | Q08_pants_shorts | Q08_leggins_stockings_tights_socks_underwear_swimwear) %>%
  mutate(Q08n_shoes = Q08_shoes) %>%
  mutate(Q08n_occasional_clothing = Q08_occasional_wear_suits | Q08_occasional_wear_maternity) %>%
  mutate(Q08n_sportswear = Q08_sportswear) %>%
  mutate(Q08n_children_clothing = Q08_children_clothing) %>%
  mutate(Q08n_other = Q08_bags | Q08_textile_apparel_accessories | Q08_household | Q08_other) %>%
  dplyr::select(ID_new, starts_with('Q08n'))

SR =  read_excel("data/proc/subsample_proc/sr.xlsx", sheet = 'SR_D_REE_849') %>%
  filter(ID_new %in% df$ID_new) %>%
  dplyr::select(ID_new, SR_average)
   
df = df %>%
  left_join(Q06 %>% dplyr::select(ID_new, Q06_buy_for_who)) %>%
  left_join(Q08) %>%
  left_join(SR)
ind = df[cols_ind]

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
ind %>% write_csv('data/proc/lca/ind_b.csv')

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
  'A02A_consumption_patterns', 'A02A_sustainable', 'A02A_cheap', 'A02A_unique_piece', 
  'A02A_shopping_experience', 'A02A_trendy', 'A02A_quality'
)
ind_proc = ind %>% 
  mutate(across(all_of(motivation_vars), cat_motivation_3))  %>%
  mutate(Q03A_channel_second = case_when(
    Q03A_channel_second %in% c('A03A_online', 'A03A_online>physic') ~ 'online',
    Q03A_channel_second %in% c('A03A_physic', 'A03A_physic>online') ~ 'physical',
    Q03A_channel_second %in% c('A03A_physic=online') ~ 'equal',
    TRUE ~ NA
  )) %>%
  mutate(Q07_quantity_second_vs_new = case_when( 
    Q07_quantity_second_vs_new %in% c('A07_less', 'A07_much_less')  ~ 'less',
    Q07_quantity_second_vs_new %in% c('A07_more', 'A07_much_more', 'A07_all')  ~ 'more',
    Q07_quantity_second_vs_new %in% c('A07_same')  ~ 'same',
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
  mutate(SR_average = case_when(
    SR_average < 0.33 ~ 'low',
    (SR_average >= 0.33) & (SR_average < 0.66) ~ 'medium',
    SR_average >= 0.66 ~ 'high',
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
ind_proc %>% write_csv('data/proc/lca/ind_b_proc1.csv')



#-------------------------------------------------------------------------------
# collapse indicators - option 2

cat_motivation_3_num <- function(x) {
  case_when(
    x <= 2 ~ "low",
    (x > 2) & (x <= 4) ~ "medium",
    x > 4 ~ "high",
    TRUE ~ NA
  )
}

m_new = ind %>% 
  dplyr::select(starts_with('A02A')) %>%
  mutate(across(starts_with('A02A'), as.numeric)) %>%
  mutate(
    m_economic = A02A_cheap,
    m_critical = (A02A_sustainable + A02A_consumption_patterns) / 2,
    m_hedonic = (A02A_unique_piece + A02A_quality + A02A_shopping_experience) / 3,
    m_fashion = A02A_trendy
    ) %>%
  mutate(across(all_of(starts_with('m_')), cat_motivation_3_num))  %>%
  dplyr::select(starts_with('m_')) 
  

ind_proc = m_new %>%
  bind_cols(ind_proc %>% dplyr::select(!starts_with('A02A'))) 
  

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
ind_proc %>% write_csv('data/proc/lca/ind_b_proc2.csv')
