rm(list = ls(all = TRUE))

library(tidyverse)
library(ggpubr)

source('scripts/subsample/utils.R')

#library(showtext)
#font_add("Arial", "arial.ttf")
#showtext_auto()

#-------------------------------------------------------------------------------
# 1) corrections of the salary distribution
sample_stats = read_csv('data/proc/subsample/sample_stats.csv') %>%
  filter(var == 'P21_salari_brut_anual') %>%
  mutate(pop_type = 'sample')
pop_stats = read_csv('data/proc/subsample/pop_stats_adj.csv') %>%
  filter(var == 'P21_salari_brut_anual') 
aux = bind_rows(sample_stats, pop_stats) %>%
  mutate(cat = factor(cat, levels = lev_salary)) %>%
  mutate(pop_type = factor(pop_type, levels = lev_pop_type))

g = ggplot(aux, aes(x = cat, y = pp, fill = pop_type)) + 
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x ='Gross annual salary [EUR]', fill = NULL, y = 'Percentage [%]') +
  guides(fill = guide_legend(ncol = 2)) + 
  scale_fill_manual(values= pal_pop_type, labels = lab_pop_type) +
  scale_x_discrete(breaks=lev_salary, labels=lab_salary) + 
  theme_bw(base_family = 'sans') +
  theme(panel.grid.major.x = element_blank()) + 
  theme(legend.position = 'top') 
#theme(axis.text.x = element_text(angle = 30, hjust = 1)) 

ggsave('figs/subsample/salari.png', g, width = 20, height = 15, units = 'cm', dpi = 500, bg = 'white')


#-------------------------------------------------------------------------------
# 2) sensitivity analysis
sensitivty = read_csv("data/proc/subsample/sensitivity.csv")

g = ggplot(sensitivty, aes(x = tolerances * 100, y = sizes, color = pop_type)) +
  geom_vline(xintercept = 2, color = "red") +
  geom_point() +
  geom_line() +
  labs(x = 'Tolerance [%]', y = 'Subsample size', color = NULL) +
  scale_color_manual(values = pal_pop_type, labels = lab_pop_type) +
  theme_bw(base_family = 'sans') +
  theme(legend.position = 'top')
ggsave('figs/subsample/sensitivty.png', g, width = 16, height = 12, units = 'cm', dpi = 500, bg = 'white')


#-------------------------------------------------------------------------------
# 3) final subsample

tol = 2
sub_stats = read_csv(sprintf('data/proc/subsample/subsample_stats_tol-%.2f.csv', tol)) %>%
  mutate(pop_type = 'subsample')
sample_stats = read_csv('data/proc/subsample/sample_stats.csv') %>%
  mutate(pop_type = 'sample')
pop_stats = read_csv('data/proc/subsample/pop_stats_adj.csv') %>%
  filter(pop_type == "adj_cte") 

lev_cat = c(lev_gender, lev_age, lev_salary, lev_province, lev_degurba)
lab_cat = c(lab_gender, lab_age, lab_salary, lab_province, lab_degurba)

aux = bind_rows(pop_stats, sample_stats, sub_stats) %>%
  mutate(pop_type = factor(pop_type, lev_pop_type)) %>%
  mutate(cat = factor(cat, lev_cat)) %>%
  mutate(var = factor(var, lev_vars))

n_sample =  aux %>% filter(pop_type == 'sample', var == "P18_genere") %>% pull(n) %>% sum()
n_subsample =  aux %>% filter(pop_type == 'subsample', var == "P18_genere") %>% pull(n) %>% sum()
lab_pop_type = c(
  "adj_cte" = sprintf("population"),
  "sample" = sprintf("sample (n = %s)", formatC(n_sample, format = "d", big.mark = ",")),
  "subsample" = sprintf("subsample (n = %s)", formatC(n_subsample, format = "d", big.mark = ","))
)
lev_pop_type = c("adj_cte", "sample", "subsample")

g = ggplot(aux, aes(x = cat, y = pp, fill = pop_type)) + 
  facet_wrap(~var, scales = 'free_x', space = 'free_x', nrow = 1, labeller = labeller(var = lab_vars)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x = NULL, fill = NULL, y = 'Percentage (%)') +
  scale_x_discrete(breaks = lev_cat, labels = lab_cat) + 
  scale_fill_manual(values= pal_pop_type, labels = lab_pop_type) +
  theme_bw(base_family = 'sans') +
  theme(panel.grid.major.x = element_blank()) + 
  theme(strip.background.x = element_blank(), strip.text.x = element_text(size = 10, face = 'bold')) +
  theme(panel.border = element_blank(), axis.line.y = element_line(), axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) + 
  theme(legend.position = 'right')
ggsave('figs/subsample/subsample.png', g, width = 30, height = 10, units = 'cm', dpi = 500, bg = 'white')
ggsave('figs/subsample/subsample.pdf', g, width = 30, height = 10, units = 'cm', dpi = 500, bg = 'white')
