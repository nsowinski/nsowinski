game_predictor(home_tm = "Wisconsin", away_tm = "Purdue", today_year = "2024", neutral_site = "FALSE")

  game_predictor <- function(today_year, away_tm, home_tm, neutral_site) {

  model <- train_model()

  #Get season stats for today's year
  url_string <- paste("https://www.sports-reference.com/cbb/seasons/men/", today_year, "-school-stats.html#basic_school_stats", sep = "")
  tmstats_url <- getURL(url_string,.opts = list(ssl.verifypeer = FALSE))
  tmstats_mid <- readHTMLTable(tmstats_url)
  tmstats <- list.clean(tmstats_mid, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(tmstats, function(t) dim(t)[1]))
  
  #Removes null columns
  tmstats$basic_school_stats <- subset(tmstats$basic_school_stats, select = - c(4,5,6,9,10,11,12,13,14,15,16,17,18,20,21,22,25,28,31))
  
  #Removes 'NCAA' from school names and "School/Overall" entries
  tmstats$basic_school_stats$School <- lapply(tmstats$basic_school_stats$School, str_remove, pattern = "NCAA")
  tmstats$basic_school_stats$School <- lapply(tmstats$basic_school_stats$School, trim)
  tmstats$basic_school_stats <- subset(tmstats$basic_school_stats, tmstats$basic_school_stats$School != "School")
  tmstats$basic_school_stats <- subset(tmstats$basic_school_stats, tmstats$basic_school_stats$School != "Overall")
  
  #Convert to ints
  tmstats$basic_school_stats[3] <- lapply(tmstats$basic_school_stats[3], strtoi)
  tmstats$basic_school_stats[4:5] <- lapply(tmstats$basic_school_stats[4:5], as.numeric)
  tmstats$basic_school_stats[6:19] <- lapply(tmstats$basic_school_stats[6:19], strtoi)
  
  #Divide stats by games played
  tmstats$basic_school_stats[6:19] <- tmstats$basic_school_stats[6:19] / tmstats$basic_school_stats[,3]
  
  #Renames to permanent DF
  tmstats$basic_school_stats$School <-lapply(tmstats$basic_school_stats$School, gsub, pattern = "\'", replacement = "")
  
  pred_game <- data.frame(matrix(ncol=36, nrow= 0))
  colnames(pred_game) <- c("Team", "Score", "Opponent", "Year", 
                       "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB", "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", 
                       "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF")
  
  pred_game[1,"Team"] <- away_tm
  pred_game[1, "Opponent"] <- home_tm
  pred_game[1, "Year"] <- today_year
  pred_game[1, "Home"] <- FALSE
  
  pred_game[1, 5:20] <- tmstats$basic_school_stats[tmstats$basic_school_stats$School == away_tm, 4:19]
  pred_game[1, 21:36] <- tmstats$basic_school_stats[tmstats$basic_school_stats$School == home_tm, 4:19]
  
  pred_game[2, "Team"] <- home_tm
  pred_game[2, "Opponent"] <- away_tm
  pred_game[2, "Year"] <- today_year
  pred_game[2, "Home"] <- TRUE
  
  pred_game[2, 5:20] <- tmstats$basic_school_stats[tmstats$basic_school_stats$School == home_tm, 4:19]
  pred_game[2, 21:36] <- tmstats$basic_school_stats[tmstats$basic_school_stats$School == away_tm, 4:19]
  
  if (neutral_site == TRUE) {
    input <- pred_game[, c("Score", "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB",
                           "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", 
                           "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF", "Home")]
    
    model$coefficients[34] = 0
    predicted_score <- predict(model, newdata = input)
    output <- data.frame("Away" = pred_game$Team, "My Away Score" = predicted_score, "Home" = pred_game$Opponent, "My Home Score" = NA)
    output$My.Home.Score[1] <- output$My.Away.Score[2]
    output <- output[-(2),]
    output <- cbind(output, "My Spread" = output$My.Home.Score - output$My.Away.Score, "My O/U" = output$My.Away.Score + output$My.Home.Score)
    
  }
  else {
    input <- pred_game[, c("Score", "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB",
                           "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", 
                           "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF", "Home")]
    predicted_score <- predict(model, newdata = input)
    
    output <- data.frame("Away" = pred_game$Team, "My Away Score" = predicted_score, "Home" = pred_game$Opponent, "My Home Score" = NA)
    output$My.Home.Score[1] <- output$My.Away.Score[2]
    output <- output[-(2),]
    output <- cbind(output, "My Spread" = output$My.Home.Score - output$My.Away.Score, "My O/U" = output$My.Away.Score + output$My.Home.Score)
    
  }
  
 
  return(output)
}
