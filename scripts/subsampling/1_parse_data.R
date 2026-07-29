rm(list = ls(all = TRUE))

library(tidyverse)
library(readxl)


# ------------------------------------------------------------------------------
# process sample variables
# - merge R21_sense_ingressos', 'R21_menys_11000'
# - remove no_resposta, altres

sample = read_excel("data/raw/02_Variables soci-demogràfiques mostra 1459 respostes.xlsx") %>%
  mutate(P21_salari_brut_anual = if_else(P21_salari_brut_anual == 'R21_sense_ingressos', 'R21_menys_11000', P21_salari_brut_anual)) %>% 
  filter(P18_genere != 'R18_nb_altres') %>%
  filter(P18_genere != 'R18_no_resposta') %>%
  filter(P22_provincia != 'R22_sense_resposta') %>%
  filter(P21_salari_brut_anual != 'R21_no_resposta') 

sample_stats = sample %>% 
  gather("var", "cat", -ID) %>%
  group_by(var, cat) %>%
  summarize(
    n = n(),
    pp = 100 * n() / nrow(sample)
    )

sample %>% write_csv("data/proc/sample.csv")
sample_stats %>% write_csv("data/proc/sample_stats.csv")


# ------------------------------------------------------------------------------
# process population variabless
# - harmonize population categories to sample cateogries
sex = read_excel("data/raw/01_Variables socio-demogràfiques població catalana.xlsx", sheet = "Sexe") %>%
  slice(1) %>%
  select(3:4) %>%
  gather("cat", 'n') %>%
  mutate(cat = case_when(
    cat == "Homes" ~ "R18_home",
    cat == "Dones" ~ "R18_dona"
  )) %>%
  mutate(var = "P18_genere")
  
eda = read_excel("data/raw/01_Variables socio-demogràfiques població catalana.xlsx", sheet = "Edat") %>%
  slice(1) %>%
  select(3:8) %>%
  gather("cat", 'n')  %>%
  mutate(cat = case_when(
    cat == "16-24" ~ "R19_16-24anys",
    cat == "25-34" ~ "R19_25-34anys",
    cat == "35-44" ~ "R19_35-44anys",
    cat == "45-54" ~ "R19_45-54anys",
    cat == "55-64" ~ "R19_55-64anys",
    cat == "+65...8" ~ "R19_65anys_mes"
  )) %>%
  mutate(var = 'P19_edat')
         
ing = read_excel("data/raw/01_Variables socio-demogràfiques població catalana.xlsx", sheet = "Ingressos") %>%
  slice(1) %>%
  select(3:8) %>%
  gather("cat", 'pp')  %>%
  mutate(cat = case_when(
    cat == "<11.000" ~ "R21_menys_11000", # sensa ingressos here????
    cat == "11.000-16.999" ~ "R21_11000-16999",
    cat == "17.000-24.999" ~ "R21_17000-24999",
    cat == "25.000-35.999" ~ "R21_25000-35999",
    cat == "36.000-49.999" ~ "R21_36000-49999",
    cat == "50.000+" ~ "R21_50000_mes"
  )) %>%
  mutate(var = 'P21_salari_brut_anual') %>%
  mutate(pp = pp * 100)

mun = read_excel("data/raw/01_Variables socio-demogràfiques població catalana.xlsx", sheet = "Municipis") %>%
  slice(1) %>%
  select(3:5) %>%
  gather("cat", 'n') %>%
  mutate(cat = case_when(
    cat == "Urbà" ~ "R22_urba",
    cat == "Semidens" ~ "R22_semidens",
    cat == "Rural" ~ "R22_rural"
  )) %>%
  mutate(var = "P22_urba_rural")

prov = read_excel("data/raw/01_Variables socio-demogràfiques població catalana.xlsx", sheet = "Provinces") %>%
  slice(1) %>%
  select(3:6) %>%
  gather("cat", 'n') %>%
  mutate(cat = case_when(
    cat == "Barcelona" ~ "R22_barcelona",
    cat == "Girona" ~ "R22_girona",
    cat == "Lleida" ~ "R22_lleida",
    cat == "Tarragona" ~ "R22_tarragona"
  )) %>%
  mutate(var = "P22_provincia")

pop_stats = bind_rows(sex, eda, mun, prov) %>%
  group_by(var) %>%
  mutate(pp = 100 * n / sum(n)) %>%
  ungroup() %>%
  bind_rows(ing) %>%
  select(var, cat, n, pp)

pop_stats %>% write_csv("data/proc/pop_stats.csv")

