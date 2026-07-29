rm(list = ls(all = TRUE))

library(tidyverse)
library(ggpubr)

source('src/utils.R')

#library(showtext)
#font_add("Arial", "arial.ttf")
#showtext_auto()

#-------------------------------------------------------------------------------
# 1) corrections of the salary distribution
sample_stats = read_csv('data/proc/sample_stats.csv') %>%
  filter(var == 'P21_salari_brut_anual') %>%
  mutate(pop_type = 'sample')
pop_stats = read_csv('data/proc/pop_stats_adj.csv') %>%
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

ggsave('figs/salari.png', g, width = 20, height = 15, units = 'cm', dpi = 500, bg = 'white')


#-------------------------------------------------------------------------------
# 2) sensitivity analysis
sensitivty = read_csv("data/out/sensitivity.csv")

g = ggplot(sensitivty, aes(x = tolerances * 100, y = sizes, color = pop_type)) +
  geom_vline(xintercept = 2, color = "red") +
  geom_point() +
  geom_line() +
  labs(x = 'Tolerance [%]', y = 'Subsample size', color = NULL) +
  scale_color_manual(values = pal_pop_type, labels = lab_pop_type) +
  theme_bw(base_family = 'sans') +
  theme(legend.position = 'top')
ggsave('figs/sensitivty.png', g, width = 16, height = 12, units = 'cm', dpi = 500, bg = 'white')


#-------------------------------------------------------------------------------
# 3) final subsample

tol = 2
sub_stats = read_csv(sprintf('data/out/subsample_stats_tol-%.2f.csv', tol)) %>%
  mutate(pop_type = 'subsample')
sample_stats = read_csv('data/proc/sample_stats.csv') %>%
  mutate(pop_type = 'sample')
pop_stats = read_csv('data/proc/pop_stats_adj.csv') %>%
  filter(pop_type == "adj_cte") 

lev_cat = c(lev_gender, lev_age, lev_salary, lev_province, lev_degurba)
lab_cat = c(lab_gender, lab_age, lab_salary, lab_province, lab_degurba)


aux = bind_rows(pop_stats, sample_stats, sub_stats) %>%
  mutate(pop_type = factor(pop_type, lev_pop_type)) %>%
  mutate(cat = factor(cat, lev_cat))

lab_pop_type = c(
  "adj_cte" = sprintf("population"),
  "sample" = sprintf("sample (n = %d)", aux %>% filter(pop_type == 'sample', var == "P18_genere") %>% pull(n) %>% sum()),
  "subsample" = sprintf("subsample (n = %d)", aux %>% filter(pop_type == 'subsample', var == "P18_genere") %>% pull(n) %>% sum())
)
lev_pop_type = c("adj_cte", "sample", "subsample")

g1 = ggplot(aux %>% filter(pop_type %in% c("adj_cte", 'sample')), aes(x = cat, y = pp, fill = pop_type)) + 
  facet_wrap(~var, scales = 'free_x', space = 'free_x', nrow = 1, labeller = labeller(var = lab_vars)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x = NULL, fill = NULL, y = 'Percentage [%]') +
  scale_fill_manual(values= pal_pop_type, labels = lab_pop_type) +
  scale_x_discrete(breaks = lev_cat, labels = lab_cat) + 
  theme_bw(base_family = 'sans') +
  theme(panel.grid.major.x = element_blank()) + 
  theme(strip.background = element_blank(), strip.text = element_text(size = 11)) + 
  theme(axis.text.x = element_blank(), axis.ticks.x = element_blank()) +
  theme(legend.position = 'right')
g2 = ggplot(aux %>% filter(pop_type %in% c("adj_cte", 'subsample')), aes(x = cat, y = pp, fill = pop_type)) + 
  facet_wrap(~var, scales = 'free_x', space = 'free_x', nrow = 1, labeller = labeller(var = lab_vars)) +
  geom_bar(stat = "identity", position=position_dodge()) +
  labs(x = NULL, fill = NULL, y = 'Percentage [%]') +
  scale_x_discrete(breaks = lev_cat, labels = lab_cat) + 
  scale_fill_manual(values= pal_pop_type, labels = lab_pop_type) +
  theme_bw(base_family = 'sans') +
  theme(panel.grid.major.x = element_blank()) + 
  theme(strip.background = element_blank(), strip.text = element_blank()) + 
  theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(legend.position = 'right')
g = ggarrange(
  g1 + theme(plot.margin = unit(c(.5,.45,.1,.1), "cm")), 
  g2 + theme(plot.margin = unit(c(.5,.1,.1,.1), "cm")), 
  nrow = 2, ncol = 1, heights = c(1, 1.1),
  labels = c("a)", "b)"))
ggsave('figs/subsample.png', g, width = 30, height = 20, units = 'cm', dpi = 500, bg = 'white')