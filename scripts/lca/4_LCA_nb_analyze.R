rm(list=ls(all = TRUE))

library(tidyverse)
library(poLCA) # packages glca vs poLCA
library(ggpubr)

source('scripts/lca/utils_lca.R')
source('scripts/lca/utils_lca_labels.R')
source('scripts/lca/utils_lca_covariates.R')
source('scripts/lca/utils_lca_profiles.R')

#-------------------------------------------------------------------------------
# PARAMS
run = 'nb_proc1'
DIR_OUT = sprintf('out/lca/%s', run)
DIR_FIG = sprintf('figs/lca/%s', run)

vars_cov = c('Q18_gender', 'Q19_age', 'Q22B_degurba', 'Q22C_province', 'Q20_education','Q21_annual_gross_salary')
vars_cov_sel = c('Q18_gender', 'Q19_age', 'Q22B_degurba')

vars_ind = read_rds(sprintf('%s/fits/vars_ind_all.RDS', DIR_OUT))
vars_ind_sel = read_rds(sprintf('%s/fits/vars_ind_sel.RDS', DIR_OUT))


#-------------------------------------------------------------------------------
# labels
lev_ind_cat = c(
  'low', 'medium', 'high', 
  'me', 'both', 'others', 
  'physical', 'equal', 'online', 
  'TRUE', 'FALSE'
)

lab_ind_cat = c(
  'low' = 'Low', 'medium' = 'Medium', 'high' = 'High', 
  'me' = 'Only myself', 'both' = 'Myself and others', 'others' = 'Only others', 
  'physical' = 'Physic', 'equal' = 'Equal', 'online' = 'Online', 
  'TRUE' = 'Use', 'FALSE' = "Don't use"
)

pal_ind_cat = c(
  #'low' = '#FFE9C2', 'medium' = '#ffcf66', 'high' = '#e69f00', 
  'low' = '#CFECF9', 'medium' = '#8FCCF0', 'high' = '#56B4E9', 
  'physical' = '#55B4A0', 'equal' = '#918EA3', 'online' = '#CC79A7', 
  'me' = '#D55E00', 'both' = '#AA4025', 'others' = '#80224A',
  'TRUE' = '#F0E442', 'FALSE' = '#BEBEBE'
) 

lev_ind = c(
  'A02B_not_accessible', 
  'A02B_unknown_use', 
  'A02B_no_trendy',
  'A02B_same_price',
  'A02B_no_size',  
  'A02B_less_quality',   
  'A02B_no_sustainable', 
  'A02B_embarrassing',
  
  'Q05_buying_frequency', 
  'Q15_disposal_frequency',
  'Q06_buy_for_who' ,
  'Q01A_use', 
  'Q03B_channel_new', 

  'Q23_environmental_concern'
) 

lab_ind = c(
  'A02B_unknown_use' = 'Barrier: hygene concerns', 
  'A02B_embarrassing' = 'Barrier: social stigma', 
  'A02B_less_quality' = 'Barrier: perceived lower quality', 
  'A02B_no_trendy' = 'Barrier: limited style availability', 
  'A02B_no_size' = 'Barrier: limited size availability', 
  'A02B_same_price' = 'Barrier: comparable price', 
  'A02B_no_sustainable' = 'Barrier: enviornmental scepticism', 
  'A02B_not_accessible' = 'Barrier: limited access',
  'Q01A_use',  'Q05_buying_frequency' = 'Purchase frequency', 
  'Q23_environmental_concern' = 'Environmental concern',
  'Q15_disposal_frequency' = 'Disposal frequency',
  'Q03B_channel_new' = 'Purchase channel', 
  'Q06_buy_for_who' = 'Purchase recipient',
  'Q01A_use' = 'Second-hand use'
) 

#-------------------------------------------------------------------------------
# read
nc = 3

ind = read_csv(sprintf('data/proc/lca/ind_%s.csv', run)) %>%
  dplyr::select(all_of(vars_ind)) %>%
  mutate(across(everything(), as.factor))
cov = read_csv('data/proc/lca/cov_nb_proc.csv') %>%
  dplyr::select(all_of(vars_cov_sel)) %>%
  mutate(across(everything(), as.factor)) %>% 
  mutate(Q18_gender = relevel(Q18_gender, 'A18_man')) %>%
  #mutate(Q21_annual_gross_salary = relevel(Q21_annual_gross_salary, '< 17,000')) %>%
  mutate(Q19_age = relevel(Q19_age, '16-34')) %>%
  mutate(Q22B_degurba = relevel(Q22B_degurba, 'A22B_urban')) 
fit = readRDS(sprintf('%s/fits/fit_%s.csv', DIR_OUT, nc))



#-------------------------------------------------------------------------------
# consumer profiles
# this not the class probability exactly!
#"Given that an individual belongs to latent class c, what is the probability they choose response category r for variable j?"
df = proc_probs(fit=fit) 
gA = plot_profile(
  df_prob=df,  fit=fit, 
  lev_ind_cat=lev_ind_cat, pal_ind_cat=pal_ind_cat, lab_ind_cat = lab_ind_cat, 
  lev_ind = rev(lev_ind), lab_ind = lab_ind,
  delta_class = 2
)
ggsave(sprintf('%s/%s-classes_prob1.png', DIR_FIG, nc), gA, width = 20, height = 20, unit = 'cm', bg = 'white')
df %>%
  mutate(ind = factor(ind, lev_ind)) %>%
  mutate(ind_cat = factor(ind_cat, lev_ind_cat)) %>%
  arrange(ind, ind_cat) %>%
  mutate(across(starts_with("prob"), ~ sprintf("%.2f", .x))) %>%
  write_csv(sprintf('%s/stats_%s-classes_prob1.csv', DIR_OUT, nc))

# ------------------------------------------------------------------------------
# covariates effect

# vif
vif_formula = as.formula(paste("as.numeric(y_pred) ~", paste(names(cov), collapse = " + ")))
print(car::vif(lm(vif_formula, data = cov %>% mutate(y_pred = fit$predclass))))

# normal regression
aux = fit_covariates(lca_fit=fit, cov=cov) %>%
  mutate(comparison = sprintf('Profile %s vs Profile %s', as.integer(comparison), 1)) %>%
  filter(variable != '(Intercept)')
g = plot_cov_effect_or(df = aux)
ggsave(sprintf('%s/%s-classes_cov_or.png', DIR_OUT, nc), g, width = 25, height = 9, unit = 'cm', bg = 'white')

# 3-step regression
aux = fit_covariates_ml(lca_fit=fit, cov=cov, class_ref = 1) %>%
  filter(variable != 'X.Intercept.') %>%
  mutate(variable = factor(variable, rev(lev_cov2))) %>%
  mutate(variable = recode(variable, !!!lab_cov2)) %>%
  mutate(comparison = str_replace(comparison, '3', '5')) %>%
  mutate(comparison = str_replace(comparison, '2', '4')) %>%
  mutate(comparison = str_replace(comparison, '1', '3')) 
  
gB = plot_cov_effect_or(df = aux)
ggsave(sprintf('%s/%s-classes_cov_ml_or.png', DIR_OUT, nc), gB, width = 25, height = 9, unit = 'cm', bg = 'white')
aux %>%
  mutate(coeff = sprintf('%.2f [%.2f, %.2f]', coeff, coeff_min, coeff_max)) %>%
  mutate(or = sprintf('%.2f [%.2f, %.2f]', or, or_min, or_max)) %>%
  mutate(p = sprintf('%.3f', p)) %>%
  mutate(variable = str_replace(variable, ' \n', ':')) %>%
  dplyr::select(comparison, variable, coeff, or, p) %>%
  write_csv(sprintf('%s/stats_%s-classes_cov_ml.csv', DIR_OUT, nc)) 




# ------------------------------------------------------------------------------
# Pannel
g = ggpubr::ggarrange(
  gA + theme(legend.position = 'bottom') + theme(plot.margin = unit(c(.1, .01, .1, .1), 'cm')), 
  gB + theme(legend.position = 'top') + theme(plot.margin = unit(c(.1, .1, 3.1, .01), 'cm')), 
  nrow = 1, ncol = 2, widths = c(1.2 + 3*1, 1.8),
  labels = c('a', 'b'))
ggsave(sprintf('%s/%s-classes_pannel.png', DIR_FIG, nc), g, width = 25, height = 14, unit = 'cm', bg = 'white', dpi=400)
ggsave(sprintf('%s/%s-classes_pannel.pdf', DIR_FIG, nc), g, width = 25, height = 15, unit = 'cm', bg = 'white')



#-------------------------------------------------------------------------------
# descriptive statistics
ind = read_csv(sprintf('data/proc/lca/ind_%s.csv', run)) %>%
  dplyr::select(all_of(vars_ind)) %>%
  mutate(across(everything(), as.factor))
cov = read_csv('data/proc/lca/cov_nb_proc.csv') %>%
  dplyr::select(all_of(vars_cov)) %>%
  mutate(across(everything(), as.factor))

# frequencies
stats = summary_probs(fit) 
stats_ind = calc_freq(df=ind, class_p=fit$predclass)
stats_cov = calc_freq(df=cov, class_p=fit$predclass)

g = plot_profile_freq(df = stats_ind %>% filter(class_p != 'Total'), pal = pal_ind_cat)
ggsave(sprintf('%s/%s-classes_freq_ind.png', DIR_FIG, nc), g, width = 30, height = 18, unit = 'cm', bg = 'white')

g = plot_profile_freq(df = stats_cov %>% filter(class_p != 'Total'), pal = pal_cov_cat)
ggsave(sprintf('%s/%s-classes_freq_cov.png', DIR_FIG, nc), g, width = 15, height = 18, unit = 'cm', bg = 'white')

stats_ind = stats_ind %>%
  dplyr::select(var, cat, class_p, nr) %>%
  mutate(class_p = str_replace(class_p, ' ', '_')) %>%
  spread(class_p, nr) %>%
  mutate(var = factor(var, lev_ind)) %>%
  mutate(cat = factor(cat, lev_ind_cat)) %>%
  arrange(var, cat) %>%
  mutate(type = 'Indicator')
stats_cov = stats_cov %>%
  dplyr::select(var, cat, class_p, nr) %>%
  mutate(class_p = str_replace(class_p, ' ', '_')) %>%
  spread(class_p, nr) %>%
  mutate(var = factor(var, lev_cov)) %>%
  mutate(cat = factor(cat, lev_cov_cat)) %>%
  arrange(var, cat) %>%
  mutate(type = 'Covariate')

stats  %>%
  mutate(across(starts_with("Profile"), ~ sprintf("%.2f", .x))) %>%
  mutate(Total = sprintf('%.2f', Total)) %>%
  write_csv(sprintf('%s/stats_%s-classes_general.csv', DIR_OUT, nc))
bind_rows(stats_ind,  stats_cov) %>%
  dplyr::select(type, var, cat, starts_with('Profile'), Total) %>%
  mutate(across(starts_with("Profile"), ~ sprintf("%.1f", .x))) %>%
  mutate(Total = sprintf('%.1f', Total)) %>%
  write_csv(sprintf('%s/stats_%s-classes_freq.csv', DIR_OUT, nc))


stats = calc_prob_avg(df=ind, class_prob=fit$posterior)
g = plot_profile_prob(df = stats)
ggsave(sprintf('%s/%s-classes_prob2_ind.png', DIR_FIG, nc), g, width = 35, height = 9, unit = 'cm', bg = 'white')

stats = calc_prob_avg(df=cov, class_prob=fit$posterior)
g = plot_profile_prob(df = stats)
ggsave(sprintf('%s/%s-classes_prob2_cov.png', DIR_FIG, nc), g, width = 25, height = 9, unit = 'cm', bg = 'white')

