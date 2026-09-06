
calc_subsample = function(df_sample, df_stats_pop, tol_pct, n_sub) {
  
  # quota sampling with linear solver, with percentage tolrenace
  n_sample <- nrow(df_sample)
  constraints <- list()
  rhs <- c()
  dir <- c()
  
  # --- Marginal constraints with percentage tolerance ---
  vars = colnames(df_sample)[-1]
  for (v in vars) {
  
    stats_v <- df_stats_pop %>% filter(var == v)
    
    for (lvl in stats_v$cat) {
      
      # create a vector = 1 for lvl = TRUE
      idx = which(sample[[v]] == lvl)
      row_vec = rep(0, n_sample)
      row_vec[idx] = 1 
      
      # tolerance
      target_pct = (stats_v %>% filter(cat == lvl) %>% pull(pp) / 100) 
      lower_n = floor((target_pct - tol_pct) * n_sub)
      upper_n = ceiling((target_pct + tol_pct) * n_sub)
      
      # Guard against negative lower bounds
      lower_n <- max(lower_n, 0)
      
      # lower bound
      constraints[[length(constraints) + 1]] = row_vec
      rhs = c(rhs, lower_n)
      dir = c(dir, ">=")
      
      # upper bound
      constraints[[length(constraints) + 1]] = row_vec
      rhs = c(rhs, upper_n)
      dir = c(dir, "<=")
    }
  }
  
  # --- Total sample size constraint ---
  constraints[[length(constraints) + 1]] = rep(1, n_sample)
  rhs <- c(rhs, n_sub)
  dir <- c(dir, "==")
  
  const_mat <- do.call(rbind, constraints)
  
  # solve
  solution <- lp(
    direction = "max",
    objective.in = rep(1, n_sample),
    const.mat = const_mat,
    const.dir = dir,
    const.rhs = rhs,
    all.bin = TRUE
  )
  
  return(solution)
}
