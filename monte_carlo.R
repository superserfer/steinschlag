# Define the has_net_exceed_maxium function
has_net_exceed_maxium <- function(index, mass_net, time_since_emptying, df) {
  if (index == 1) {
    return(FALSE)
  }
  time_since_emptying <- time_since_emptying + df$time_diff[index]
  if (time_since_emptying > 24) {
    return(FALSE)
  }
  mass_net <- mass_net + df$mass[index - 1]
  if (mass_net >= 2000) {
    return(TRUE)
  }
  return(has_net_exceed_maxium(index-1, mass_net, time_since_emptying, df))
}

# Define the monte_carlo function
monte_carlo <- function(df, n) {
  future_lapply(1:n, function(i) {
    df$kin_energy[i] >= 1200 || (df$kin_energy[i] >= 600 && has_net_exceed_maxium(i, 0, 0, df))
  }) %>% 
    unlist() %>% 
    sum()
}