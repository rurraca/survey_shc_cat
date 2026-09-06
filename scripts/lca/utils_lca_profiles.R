source('scripts/lca/utils_lca_labels.R')

summary_probs = function(fit) {
  
  df = fit$post %>%
    as.data.frame() %>% 
    mutate(class_p = fit$predclass) %>%
    gather(class, pp, -class_p) %>%
    mutate(class = as.integer(str_replace(class, 'V', ''))) %>%
    filter(class == class_p) %>%
    group_by(class) %>%
    summarize(
      n = n(),
      nr = 100 * n / length(fit$predclass),
      pp_avg = mean(pp)
    ) %>%
    gather(var, value, -class) %>%
    mutate(class = sprintf('Profile_%s', class)) %>%
    spread(class, value) %>%
    mutate(Total = rowSums(pick(where(is.numeric)), na.rm = TRUE)) %>%
    mutate(Total = if_else(var == 'pp_avg', NA, Total))
  
  
  return(df)
  
}


calc_freq = function(df, class_p) {
  
  n_class = class_p %>% table() %>% data.frame() %>% 
    rename(class_p = '.', n_class = Freq)
  
  aux = df %>%
    mutate(class_p = factor(class_p)) %>%
    gather(var, cat, -class_p) 
  
  stats = aux %>%
    group_by(class_p, var, cat) %>%
    summarize(n = n()) %>%
    ungroup()  %>%
    left_join(n_class)  %>%
    mutate(class_p = str_c('Profile ', class_p))  
  
  stats_tot = aux %>%
    group_by(var, cat) %>%
    summarize(n = n()) %>%
    ungroup() %>%
    mutate(class_p = 'Total') %>%
    mutate(n_class = sum(n_class$n_class))
  
  out = stats %>%
    bind_rows(stats_tot) %>%
    mutate(nr = 100 * n / n_class) %>%
    dplyr::select(-n_class)
    
  
  return(out)

}

calc_prob_avg = function (df, class_prob) {
  stats = df %>%
    bind_cols(as.data.frame(class_prob)) %>%
    gather(class, class_prob, starts_with('V')) %>%
    gather(var, cat, -class, -class_prob) %>%
    group_by(class, var, cat) %>%
    summarize(prob_mean = mean(class_prob)) %>%
    ungroup() %>%
    mutate(class = str_replace(class, 'V', 'Profile '))
  
  return (stats)
  
}

# covariate profile s
plot_profile_freq = function(df, pal_cat) {
 
  g = ggplot(df, aes(x = var, y = nr, fill = cat)) +
    facet_wrap(~class_p, ncol = 1) +
    geom_bar(stat = 'identity') +
    labs(x =NULL, y = 'Percentage [%]', fill = NULL) +
    scale_fill_manual(values = pal_cat) +
    #guides(fill = guide_legend(ncol = 2)) +
    theme_bw() +
    theme(legend.position = 'right') +
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(strip.background = element_rect(fill = 'white', color = 'white')) +
    theme(panel.border = element_blank(), axis.ticks.x = element_blank(), axis.line.y = element_line()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())
  
  return(g)
  
}


plot_profile_prob = function(df) {
  
  g = ggplot(df, aes(x = cat, y = prob_mean, fill = class)) +
    facet_wrap(~var, nrow = 1, space = 'free_x', scales = 'free_x') +
    geom_bar(stat = 'identity') +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = NULL, y = 'Average probability', fill = NULL) + 
    theme_bw() + 
    theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(strip.background = element_rect(fill = 'white', color = 'white')) +
    theme(panel.border = element_blank(), axis.ticks.x = element_blank(), axis.line.y = element_line()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank())  
  return(g)
}
