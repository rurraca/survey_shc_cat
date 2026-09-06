library('poLCA') # packages glca vs poLCA

calc_cramerV = function(df) {
  vars = names(df)
  cor = expand.grid(Var1 = vars, Var2 = vars, stringsAsFactors = FALSE) %>%
    filter(Var1 < Var2) %>%  # upper triangle only, no duplicates
    rowwise() %>%
    mutate(CramerV = cramerV(df[[Var1]], df[[Var2]], bias.correct = TRUE)) %>%
    ungroup() %>% arrange(desc(CramerV))
  
  return(cor)
}

biv_residuals <- function(model, data, vars) {
  pairs = expand.grid(Var1 = vars, Var2 = vars, stringsAsFactors = FALSE) %>%
    filter(Var1 < Var2)  %>%
    pmap(c)
  
  results <- lapply(pairs, function(p) {
    obs <- table(data[[p[1]]], data[[p[2]]])
    
    # Expected frequencies from model
    probs1 <- model$probs[[p[1]]]  # K x categories matrix
    probs2 <- model$probs[[p[2]]]
    class_probs <- model$P  # mixing proportions
    
    exp <- matrix(0, nrow = nrow(obs), ncol = ncol(obs))
    for (k in seq_along(class_probs)) {
      exp <- exp + class_probs[k] * outer(probs1[k, ], probs2[k, ])
    }
    exp <- exp * sum(obs)
    
    # Standardised residual (like a chi-sq contribution)
    resid <- sum((obs - exp)^2 / exp)
    
    data.frame(Var1 = p[1], Var2 = p[2], BivResid = round(resid, 2))
  })
  
  do.call(rbind, results) |> dplyr::arrange(desc(BivResid))
}



calc_stats = function(fit) {
  
  pp = fit$posterior
  n = nrow(pp)
  n_class = ncol(pp)
  entropy = 1 + (sum(pp * log(pp), na.rm = TRUE) / (n * log(n_class)))
  
  mat = pp %>%
    as.data.frame() %>%
    mutate(class_p = fit$predclass) %>%
    mutate(class_p = str_c('V', class_p)) %>%
    gather(class, prob, -class_p) %>%
    group_by(class, class_p) %>%
    summarize(prob = mean(prob)) %>%
    ungroup()
  
  results = data.frame(
    nclass= n_class,
    LL = fit$llik,
    AIC = fit$aic,
    BIC = fit$bic,
    #chi2 = fit$Chisq, 
    entropy = entropy,
    pp_min = mat %>% filter(class == class_p)  %>% pull(prob) %>% min(),
    pp_avg = mat %>% filter(class == class_p)  %>% pull(prob) %>% mean(),
    n_min = fit$predclass %>% table() %>% min()
  ) %>%
    mutate(nr_min = 100 * n_min / n)
  
  return(results)
}


proc_probs = function(fit) {
  
  # parse class-conditional probability
  probs_list = fit$probs
  df = do.call(
    rbind, lapply(names(probs_list), function(indicator) {
      
      mat <- probs_list[[indicator]]
      
      df = data.frame(
        ind = indicator,
        ind_cat = colnames(mat),
        row.names = NULL
      )
      for (i in seq_len(nrow(mat))) {
        df[[paste0("prob_", i)]] <- as.numeric(mat[i, ])
      }
      df
    })
  )
  

  return(df)
}



plot_profile = function(df_prob, fit, lev_ind_cat, pal_ind_cat, lab_ind_cat, lev_ind, lab_ind,
                        delta_class=0){
  
  library(scales)
  
  n_class = tibble(
    'class' = fit$predclass
  ) %>%
    group_by(class) %>%
    summarize(n_class = n()) %>%
    ungroup() %>%
    mutate(class = str_c('Profile ', class + delta_class)) 

  
  df_prob = df_prob  %>%
    gather(class, prob, -ind, - ind_cat) %>%
    mutate(class = as.numeric(str_replace(class, 'prob_', '')) + delta_class) %>%
    mutate(class = str_c('Profile ', class)) %>%
    left_join(n_class) %>%
    mutate(class = sprintf('%s (n = %d)', class, n_class)) %>%
    mutate(ind_cat = factor(ind_cat, lev_ind_cat)) %>%
    mutate(ind = factor(ind, lev_ind))
  g = ggplot(df_prob, aes(x = ind, y = prob, fill = ind_cat)) +
    facet_wrap(~class, nrow = 1, strip.position = 'top') +
    geom_bar(stat = 'identity') +
    scale_fill_manual(values = pal_ind_cat, labels = lab_ind_cat) +
    scale_x_discrete(labels = lab_ind) +
    scale_y_continuous(labels = label_number(drop0trailing = TRUE)) +
    labs(x = NULL, y  = 'Class-conditional probability', fill = NULL) +
    guides(fill = guide_legend(nrow = 3)) +
    coord_flip() +
    theme_bw() +
    #theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
    theme(strip.background = element_rect(fill = 'white', color = 'white')) +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank(), axis.line.x = element_line()) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank()) +
    theme(strip.text = element_text(face = 'bold')) +
    theme(legend.position = 'bottom')
  
  return(g)
}



