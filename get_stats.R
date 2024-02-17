get_overall_stats <- function() {
  hist_games <- read.csv("C:\\Users\\sowin\\OneDrive\\collegebball\\Historical Output.csv")
  
  stats <- list()
  stats$win_percentage <- 100 * (sum(hist_games$Winner) / nrow(hist_games))
  stats$total_10 <- 100 * (sum(hist_games$Total_10) / nrow(hist_games))
  stats$total_12.5 <- 100 * (sum(hist_games$Total_12.5) / nrow(hist_games))
  stats$total_15 <- 100 * (sum(hist_games$Total_15) / nrow(hist_games))
  
  stats$spread_5 <- 100 * (sum(hist_games$Spread_5) / nrow(hist_games))
  stats$spread_7.5 <- 100 * (sum(hist_games$Spread_7.5) / nrow(hist_games))
  stats$spread_10 <- 100 * (sum(hist_games$Spread_10) / nrow(hist_games))
  
  return(stats)
}

get_month_stats <- function() {
  hist_games <- read.csv("C:\\Users\\sowin\\OneDrive\\collegebball\\November 2024 Output.csv")
  
  stats <- list()
  stats$win_percentage <- 100 * (sum(hist_games$Winner) / nrow(hist_games))
  stats$total_10 <- 100 * (sum(hist_games$Total_10) / nrow(hist_games))
  stats$total_12.5 <- 100 * (sum(hist_games$Total_12.5) / nrow(hist_games))
  stats$total_15 <- 100 * (sum(hist_games$Total_15) / nrow(hist_games))
  
  stats$spread_5 <- 100 * (sum(hist_games$Spread_5) / nrow(hist_games))
  stats$spread_7.5 <- 100 * (sum(hist_games$Spread_7.5) / nrow(hist_games))
  stats$spread_10 <- 100 * (sum(hist_games$Spread_10) / nrow(hist_games))
  
  return(stats)
}
