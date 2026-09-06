source('scripts/lca/utils_lca_labels.R')

# standard 3-step fit, not accounting for classification error
fit_covariates = function(lca_fit, cov) {
  
  library(nnet)
  
  data = cov %>%
    mutate(y_pred = lca_fit$predclass) %>% 
    mutate(across(where(is.character), factor)) %>% 
    mutate(across(where(is.numeric), factor)) %>%
    mutate(across(where(is.logical), factor))
  fit = multinom(
    as.formula(paste('y_pred', "~", paste(names(cov), collapse = " + "))),
    data = data
  )
  s <- summary(fit)
  
  coef_mat <- s$coefficients
  se_mat   <- s$standard.errors
  

  # z statistics
  z_mat <- coef_mat / se_mat
  
  # two-sided p-values
  p_mat <- 2 * (1 - pnorm(abs(z_mat)))
  
  # odds ratios
  or_mat <- exp(coef_mat)
  
  # Combine into a tidy table
  if (is.matrix(coef_mat) == FALSE) {
    results <- data.frame(
      comparison  = 2,
      variable  = names(coef_mat),
      coeff = as.vector(coef_mat),
      coeff_se   = as.vector(se_mat),
      p     = as.vector(p_mat)
    )
  } else {
    results <- data.frame(
      comparison   = rep(rownames(coef_mat), each = ncol(coef_mat)),
      variable  = rep(colnames(coef_mat), times = nrow(coef_mat)),
      coeff = as.vector(t(coef_mat)),
      coeff_se   = as.vector(t(se_mat)),
      p     = as.vector(t(p_mat))
    )
  }
  results = results %>%
    mutate(coeff_min = coeff - 1.96 * coeff_se) %>%
    mutate(coeff_max = coeff + 1.96 * coeff_se) %>%
    mutate(or = exp(coeff)) %>%
    mutate(or_min = exp(coeff_min)) %>%
    mutate(or_max = exp(coeff_max))
  
  return(results)
}


# bias-adjusted Maximum Likelihood (ML) 3-step approach (Vermunt, 2010; Bakk, Tekle & Vermunt, 2013),
# this is what Mplus's R3STEP does internally.
fit_covariates_ml = function(lca_fit, cov, class_ref) {
  
  post  <- lca_fit$posterior      # N x K posterior class probabilities (step 1)
  modal <- lca_fit$predclass      # N-length modal class assignment (step 2)
  K     <- ncol(post)
  
  # classification probability matrix: R[t, w] = P(modal class = w | true class = t)
  R <- matrix(0, K, K)
  for (t in 1:K) {
    for (w in 1:K) {
      R[t, w] <- sum(post[modal == w, t]) / sum(post[, t])
    }
  }
  
  data <- cov %>%
    mutate(across(where(is.character), factor)) %>%
    mutate(across(where(is.numeric), factor)) %>%
    mutate(across(where(is.logical), factor))
  
  X <- model.matrix(~ ., data = data)   # includes intercept column
  p <- ncol(X)
  
  # Remap modal class labels so ref_class becomes 1, others follow in order
  other_classes <- setdiff(1:K, class_ref)
  class_order   <- c(class_ref, other_classes)   # new position i = old class class_order[i]
  # Remap modal: old class label -> new position
  remap         <- order(class_order)            # remap[old_class] = new_position
  modal_remapped <- remap[modal]
  # Reorder R rows AND columns to match new class positions
  R_reordered <- R[class_order, class_order]

  # corrected negative log-likelihood: P(W=w_i | x_i) = sum_t P(T=t|x_i;beta) * R[t, w_i]
  negLL <- function(par) {
    B   <- matrix(par, nrow = K - 1, ncol = p)   # class 1 = reference, as in multinom()
    eta <- cbind(0, X %*% t(B))
    expe  <- exp(eta - apply(eta, 1, max))
    probs <- expe / rowSums(expe)                 # N x K, P(T=t|x)
    
    RW <- R_reordered[, modal_remapped]         # K x N
    pw <- rowSums(probs * t(RW))                  # P(W=w_i|x_i)
    -sum(log(pmax(pw, 1e-300)))
  }
  
  start <- rep(0, (K - 1) * p)
  opt <- optim(start, negLL, method = "BFGS", hessian = TRUE, control = list(maxit = 5000, reltol = 1e-12))
  if (opt$convergence != 0) warning("optim did not converge — try different starting values or method = 'nlminb'")
  
  # Label rows as "classX vs ref_class"
  comparison_labels <- paste0("Profile ", other_classes, " vs Profile ", class_ref)
  
  B_hat  <- matrix(opt$par, nrow = K - 1, ncol = p, dimnames = list(comparison_labels, colnames(X)))
  se_mat <- matrix(sqrt(diag(solve(opt$hessian))), nrow = K - 1, ncol = p, dimnames = dimnames(B_hat))
  
  res_coef = data.frame(comparison = rownames(B_hat), B_hat, row.names=NULL) %>% gather(variable, coeff, -comparison)
  res_se = data.frame(comparison = rownames(se_mat), se_mat, row.names=NULL) %>% gather(variable, coeff_se, -comparison)
  
  results = res_coef %>%
    left_join(res_se) %>%
    arrange(comparison) %>%
    mutate(z = coeff / coeff_se) %>%
    mutate(p = 2 * (1 - pnorm(abs(z)))) %>%
    mutate(coeff_min = coeff - 1.96 * coeff_se) %>%
    mutate(coeff_max = coeff + 1.96 * coeff_se) %>%
    mutate(or = exp(coeff)) %>%
    mutate(or_min = exp(coeff_min)) %>%
    mutate(or_max = exp(coeff_max))
  
  return(results)
}

plot_cov_effect = function(df) {

  g = ggplot(df, aes(x = coeff, y = variable, color = comparison)) +
    geom_vline(xintercept = 0) +
    geom_point(size = 2, position = position_dodge(width = 0.5)) +
    geom_segment(aes(x = coeff_min, xend = coeff_max, y = variable), position = position_dodge(width = 0.5)) +
    labs(y = NULL, x = 'Coefficient with 95 % Confidence Interval ', color = NULL) +
    scale_y_discrete(breaks = lev_cov2, labels = lab_cov2) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white', color = 'white')) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_blank(), axis.ticks.y = element_blank(), axis.line.x = element_line())
  
  return(g)
}

plot_cov_effect_or = function(df) {
  #df = df %>%
  #  mutate(variable = factor(variable, rev(lev_cov2))) 
  
  g = ggplot(df, aes(x = or, y = variable)) +
    facet_wrap(~comparison, ncol = 1) +
    geom_vline(xintercept = 1, size = .4) +
    geom_point(size = 1, position = position_dodge(width = 0.5), color = '#333333') +
    geom_segment(aes(x = or_min, xend = or_max,  y = variable), position = position_dodge(width = 0.5), size = .4, color = '#333333') +
    scale_x_log10(
      breaks = c(0.2, 0.5, 1, 2, 4), 
      labels = label_number(drop0trailing = TRUE),
      minor_breaks = c(seq(0.1, 1, by = 0.1), seq(1, 4, by = 0.5))) +
    labs(y = NULL, x = 'Adjusted odds ratio (95 % CI) ', color = NULL) +
    theme_bw() +
    theme(strip.background = element_rect(fill = 'white', color = 'white')) +
    theme(panel.grid.major.y = element_blank(), panel.grid.minor.y = element_blank()) +
    theme(panel.border = element_blank(), axis.line.x = element_line(), axis.ticks.y = element_blank())
    
  
  return(g)
}

