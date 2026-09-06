rm(list=ls(all = TRUE))

library(tidyverse)
library(rstatix)
library(ggpubr)

#source('scripts/descriptive/preprocess_frequencies.R')
source('scripts/descriptive/utils_labels.R')

lev_group = c('buyers', 'non-buyers') # 'all')
lab_group = c(
  'non-buyers' = 'Non-buyers of second-hand clothing', 
  'buyers' = 'Buyers of second-hand clothing'
  #'all' = 'All respondents'
)
pal_group = c(
  'non-buyers' = '#56B4E9', 
  'buyers' = '#E69F00'
)
#  'all' = '#404040'


#-------------------------------------------------------------------------------
# read
df = read_csv('data/proc/descriptive/proc_descritpive.csv')

#-------------------------------------------------------------------------------
# functions

calc_freq = function(data) {
  
  n_tot = data %>%
    group_by(group) %>%
    summarize(n_tot = n()) %>%
    ungroup()
  
  stats = data %>%
    group_by(group, var) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    left_join(n_tot) %>%
    mutate(nr = 100 * n / n_tot) %>%

  return(stats)
  
}

plot_bars = function(stats, levels, labels, title, y_max, y_int) {
  
  legend_df <- data.frame(x = 1,  y = 1,  group = factor(names(pal_group), levels = names(pal_group)))

  stats = stats %>%
    mutate(group = factor(group, lev_group)) %>%
    mutate(var = factor(var, levels = levels)) %>%
    mutate(facet = title)
  g = ggplot(stats, aes(x = var, y = nr, fill = group)) +
    facet_wrap(~facet) +
    geom_bar(stat = 'identity', position="dodge", show.legend = FALSE) +
    labs(x = NULL, y = NULL, fill = 'Group') +
    geom_point(data = legend_df, aes(x = x, y = y, fill = group), shape = 22, color = 'white', size = 6, alpha = 0, key_glyph = "point") +
    scale_fill_manual(values = pal_group, labels = lab_group, breaks = names(pal_group)) +
    guides(fill = guide_legend(override.aes = list(alpha = 1, shape = 22))) +
    #guides(fill = guide_legend(breaks = c('buyers', 'non-buyers'))) +
    scale_fill_manual(values = pal_group, labels = lab_group, drop = FALSE) +
    coord_cartesian(ylim = c(0, y_max)) + 
    scale_x_discrete(labels = labels) +
    scale_y_continuous(breaks = seq(0, y_max, y_int)) +
    theme_bw() +
    theme(legend.position = 'none') +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(panel.grid.minor.x = element_blank(), panel.grid.major.x = element_blank()) +
    theme(panel.grid.minor.y = element_blank()) +
    theme(strip.background.x = element_blank(), strip.text.x = element_text(size = 10, face = 'bold')) +
    theme(panel.border = element_blank(), axis.line.y = element_line(), axis.ticks.x = element_blank())
  return(g)
} 

#-------------------------------------------------------------------------------
# plotting variables divided by total sample

lev_1a = c('A01A_no_use_no_buy', 'A01A_only_use', 'A01A_only_buy', 'A01A_use_buy')
lab_1a = c(
  'A01A_no_use_no_buy' = 'Neither buy nor use', 
  'A01A_only_use' = "Use, but don't buy", 
  'A01A_only_buy' = "Buy, but don't use", 
  'A01A_use_buy' = 'Buy and use'
  )

lev_1b = c("A01B_no_use_no_buy", "A01B_free", "A01B_money_free", "A01B_money")
lab_1b = c(
  "A01B_no_use_no_buy" = "Not applicable", 
  "A01B_money" = 'Only formal', 
  "A01B_free" = 'Only informal', 
  "A01B_money_free" = 'Formal and informal'
  )


pal_1b = c(
  "A01B_no_use_no_buy" = "grey", 
  "A01B_money" = '#009E73', 
  "A01B_free" = '#0072B2', 
  "A01B_money_free" = '#F0E442'
)


pal_1b = c(
  "A01B_no_use_no_buy" = "grey", 
  "A01B_money" = '#009E73', 
  "A01B_free" = '#F0E442', 
  "A01B_money_free" = '#78C15B'
)

#pal_1b = c(
#  "A01B_no_use_no_buy" = "grey", 
#  "A01B_money" = '#243B53', 
#  "A01B_free" = '#F28C6B', 
#  "A01B_money_free" = '#78516B'
##)

# use
aux = df %>%
  group_by(Q01A_use_buy, Q01B_money_free) %>%
  summarize(n = n()) %>%
  mutate(nr = 100 * n / df %>% nrow()) %>%
  mutate(group = 'all') %>%
  mutate(group = factor(group, levels = lev_group)) %>%
  mutate(facet = 'Second-hand clothing use and purchase') %>%
  mutate(Q01A_use_buy = factor(Q01A_use_buy, levels = lev_1a)) %>%
  mutate(Q01B_money_free = factor(Q01B_money_free, levels = rev(lev_1b)))

g_buy =  ggplot(aux, aes(x = Q01A_use_buy, y = nr, fill = Q01B_money_free)) +
  facet_wrap(~facet) +
  geom_bar(stat = 'identity', position="stack") +
  labs(x = NULL, y = 'Percentage of all respondents (%)', fill = 'Acquisition mode') +
  #geom_point(data = legend_df, aes(x = x, y = y, fill = group), shape = 22, color = 'white', size = 6, alpha = 0, key_glyph = "point") +
  #scale_fill_manual(values = pal_group, labels = lab_group, breaks = names(pal_group)) +
  #guides(fill = guide_legend(override.aes = list(alpha = 1, shape = 22))) +
  scale_fill_manual(values = pal_1b, labels = lab_1b, drop = FALSE) +
  scale_x_discrete(labels = lab_1a) +
  scale_y_continuous(breaks = seq(0, 40, 5)) +
  theme_bw() +
  #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
  theme(panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank()) +
  theme(panel.grid.minor.x = element_blank()) +
  theme(strip.background.x = element_blank(), strip.text.x = element_text(size = 10, face = 'bold')) +
  theme(panel.border = element_blank(), axis.line.x = element_line(), axis.ticks.y = element_blank())

#g_buy2 = plot_bars(
#  stats = aux,
#  levels = c('A01A_no_use_no_buy', 'A01A_only_use', 'A01A_only_buy', 'A01A_use_buy'),
#  labels = c('A01A_no_use_no_buy' = 'Neither buy nor use', 'A01A_only_use' = 'Use, but do not buy', 'A01A_only_buy' = 'Buy, but do not use', 'A01A_use_buy' = 'Buy and use'),
#  title = 'Second-hand use and purchase'
#  )

#-------------------------------------------------------------------------------
# plotting variables divided by buyers/non-buyers

df2 = df %>%
  dplyr::rename(group = buy) 
  
 
#A) quantity
stats = df2 %>%
  rename(var = Q07_quantity_second_vs_new) %>%
  filter(group == 'buyers') %>%
  calc_freq()
g_quantity = plot_bars(stats = stats, levels = lev_quantity, labels = lab_quantity, 
                       title = 'Second-hand vs new quantity', y_max = 40, y_int = 10)

#B) channel
stats = df2 %>%
  rename(var = Q03_channel) %>%
  calc_freq()  %>%
  add_row(group = 'buyers', var = 'dont_buy_new', nr = 0, n = 0)
g_channel = plot_bars(stats = stats, levels = lev_channel, labels = lab_channel, 
                      title = 'Purchase channel', y_max = 50, y_int = 10)

#C) buying frequency
stats = df2 %>%
  rename(var = Q05_buying_frequency) %>%
  calc_freq() %>%
  add_row(group = 'buyers', var = 'n/a', nr = 0, n = 0)
g_freq_b = plot_bars(stats = stats, levels = lev_frequency, labels = lab_frequency, 
                     title = 'Purchase frequency', y_max = 40, y_int = 10)

#D) disposal frequency
stats = df2 %>%
  rename(var = Q15_disposal_frequency) %>%
  calc_freq() 
g_freq_d = plot_bars(stats = stats, levels = lev_frequency, labels = lab_frequency, 
                     title = 'Disposal frequency', y_max = 40, y_int = 10)

#E) clothes
stats = df2 %>%
  filter(group == 'buyers') %>%
  dplyr::select(group, starts_with('Q08')) %>%
  gather(clothes, value, -group) %>%
  mutate(value = as.numeric(value)) %>%
  group_by(clothes) %>%
  summarize(n = sum(value, na.rm = TRUE)) %>%
  mutate(nr = 100 * n / df2 %>% filter(group == 'buyers') %>% nrow()) %>%
  mutate(group = 'buyers') %>%
  rename(var = clothes) 
levels = stats %>% arrange(nr) %>% pull(var) %>% rev()
g_clothes = plot_bars(stats = stats, levels = levels, labels = lab_clothes, 
                      title = 'Product category', y_max = 50, y_int = 10)

#G) purchase recipient
stats = df2 %>%
  rename(var = Q06_buy_for_who) %>%
  calc_freq() %>%
  mutate(var = if_else(is.na(var), 'n/a', var))  %>%
  add_row(group = 'buyers', var = 'n/a', nr = 0, n = 0)
g_recipient = plot_bars(stats = stats, levels = lev_recipient, labels = lab_recipient, 
            title = 'Purchase recipient', y_max = 75, y_int = 15)

#-------------------------------------------------------------------------------
# pannel

library(grid)
library(egg)
g_empty = ggplot() + geom_blank() + theme_bw() + theme(panel.border = element_blank())
gA = egg::ggarrange(
  g_empty,
  g_buy+ theme(legend.position = 'right') + coord_flip(),
  g_empty,
  nrow = 1, ncol = 3, widths = c(.5, 4, .5), labels = c('','a', ''))
gB = egg::ggarrange(
  g_empty,
  g_quantity + labs(y = 'Percentage within group (%)'), 
  g_channel, 
  g_recipient, 
  g_empty,
  nrow = 1, ncol = 5,  widths = c(1, 6 * 0.42 + 0.5, 6, 4, 1), labels = c('       b', '', '', '', '')
  )
gC = egg::ggarrange(
  g_empty,
  g_freq_b  + labs(y = 'Percentage within group (%)'),  
  g_freq_d, 
  g_empty,
  nrow = 1, ncol = 4, widths = c(0, 9 + 0.5,  7, 0))
gD = egg::ggarrange(
  g_empty,
  g_clothes+ theme(legend.position = 'right') + labs(y = 'Percentage within group (%)'), 
  g_empty,
  nrow = 1, ncol = 3, widths = c(5, 11 * 0.45 + 2, 2))
g = grid.arrange(gA, gB, gC, gD, heights = c(.6, 1, 1, 1.12), ncol = 1)
ggsave('figs/descriptive/frequencies.png', g, width = 23, height = 25, unit = 'cm', bg = 'white', dpi = 400)
ggsave('figs/descriptive/frequencies.pdf', g, width = 23, height = 25, unit = 'cm', bg = 'white')
