rm(list=ls(all = TRUE))

library(tidyverse)
library(poLCA) # packages glca vs poLCA
library(ggpubr)
library(rcompanion)

source('scripts/lca/utils_lca.R')

#-------------------------------------------------------------------------------
# PARAMS
run = 'b_proc1'
DIR_OUT = sprintf('out/lca/%s', run)

vars_ind = c(
  'm_critical', 'm_economic', 'm_fashion', 'm_hedonic',
  'Q05_buying_frequency', 'Q07_quantity_second_vs_new', 
  'Q06_buy_for_who', 'Q01B_money_free', 
  'Q01A_use',
  'SR_average',
  'Q23_environmental_concern',
  'Q15_disposal_frequency',
  "Q08n_general_clothing", "Q08n_shoes", "Q08n_occasional_clothing", "Q08n_sportswear", "Q08n_children_clothing", "Q08n_other"              
) 
vars_ind_sel = vars_ind


#-------------------------------------------------------------------------------
# read
ind = read_csv(sprintf('data/proc/lca/ind_%s.csv', run)) %>%
  mutate(across(everything(), as.factor))
class(ind) = "data.frame"

#-------------------------------------------------------------------------------
# select covariates
# 1) # > 90% in one category -> aggregate or remove
# 2) # highly correlated and explaining the same consumption pattern
# 2.1) correlation
# 2.2)standardized residual > |3|

# 1) 
# freq after
ind %>%
  gather(var, cat) %>%
  group_by(var, cat) %>%
  summarize(n = n()) %>%
  mutate(nr = 100 * n / nrow(ind)) %>%
  mutate(small = nr < 10) %>%
  print(n = 100) 

rm_small = c('Q01A_use', "Q08n_general_clothing", "Q08n_shoes", 
             "Q08n_occasional_clothing", "Q08n_sportswear", "Q08n_children_clothing", "Q08n_other")
vars_ind_sel = vars_ind_sel[!vars_ind_sel %in% rm_small]

# 2.1) mutlicolinearity
cor = calc_cramerV(df=ind[vars_ind_sel])
ggplot(cor, aes(x = Var1, y = Var2, fill = CramerV)) +
  geom_tile() +
  scale_fill_viridis_b() +
  coord_cartesian(expand = FALSE) +
  labs(x = NULL, y = NULL) +
  theme_bw() + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1))


# 2.2) standardized residual
lhs = paste0("cbind(", paste(vars_ind_sel, collapse = ", "), ")")
f = as.formula(paste(lhs, "~", 1))
lca2 <- poLCA(f, data = ind, nclass = 2, nrep = 10, verbose = FALSE)

residuals = biv_residuals(model=lca2, data=ind, vars=vars_ind_sel) 
ggplot(residuals, aes(x = Var1, y = Var2, fill = BivResid)) +
  geom_tile() +
  scale_fill_viridis_b( breaks = seq(0, 30, 5)) +
  labs(x = NULL, y = NULL) +
  theme_bw() + 
  coord_cartesian(expand = FALSE) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1))

rm_cor = c()
vars_ind_sel = vars_ind_sel[!vars_ind_sel %in% rm_cor]


saveRDS(vars_ind, sprintf('%s/fits/vars_ind_all.RDS', DIR_OUT))
saveRDS(vars_ind_sel, sprintf('%s/fits/vars_ind_sel.RDS', DIR_OUT))

#------------------------------------------------------------------------------
# optimization

# prepare the data for 1-step or 3-step LCA
lhs <- paste0("cbind(", paste(vars_ind_sel, collapse = ", "), ")")

stats = data.frame()
for (k in 1:5) {
  
  cat("Fitting", k, "classes\n")
  
  fit_k = poLCA(
    as.formula(paste(lhs, "~", 1)),
    data = ind,
    nclass = k,
    nrep = 100,
    maxiter = 5000,
    tol = 1e-10,
    verbose = FALSE
  )
  stats_k = calc_stats(fit=fit_k)
  stats = rbind(stats, stats_k)
  
  saveRDS(fit_k, sprintf('%s/fits/fit_%s.csv', DIR_OUT, k))
  
}

print(stats)
stats %>% write_csv(sprintf('%s/stats.csv', DIR_OUT))
