today_predictions <- function() {
  
  model <- train_model()
  
  #Start here for ad hoc
  #
  #
  
  #Get today's date (subtract 1 from year if in second half of season)
  date <- Sys.Date()
  today <- str_split(date, "[-]")
  if(as.integer(today[[1]][2]) > 6) {
    today_year <- as.character(as.integer(today[[1]][1]) + 1)
  } else {
    today_year <- today[[1]][1]
  }
  
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
  assign(paste("tmstats_current_", today[[1]][1], sep = ""), tmstats)
  
  
  #Get today's games from sportsreference
  url_string <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=", today[[1]][2], "&day=", today[[1]][3], "&year=", today[[1]][1], sep = "")
  day_games_url <- getURL(url_string,.opts = list(ssl.verifypeer = FALSE) )
  day_games <- readHTMLTable(day_games_url)
  day_games <- list.clean(day_games, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(day_games, function(t) dim(t)[1]))
  
  games <- data.frame(matrix(ncol=36, nrow= 0))
  colnames(games) <- c("Team", "Score", "Opponent", "Year", 
                       "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB", "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", 
                       "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF")
  
  #Replace differences in game vs season data
  replacements <- data.frame(c("UConn", "LSU", "VCU", "BYU", "UCF", "Ole Miss", "UNC", "N/A"), 
                             c("Connecticut", "Louisiana State", "Virginia Commonwealth", "Brigham Young", "Central Florida", "Mississippi", "North Carolina", "Miami (FL)"))
  colnames(replacements) <- c("Orignial", "Replacement")
  
  #For all games today
  if(length(day_games) > 0) {
    #Add daily games to game DF
    for (l in 1:length(day_games)) {
      if(day_games[[l]][3,1] != "Women's") {
          
        
        #Remove ranking numbers
        tm <-day_games[[l]][1,1]
       
        opp <-day_games[[l]][2,1]
        str<-"[\\(,\\),1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]"
        tm <- gsub(str, "", tm)
        tm <- trim(tm)
        tm <- gsub("\'", "", tm)
        opp <- gsub(str, "", opp)
        opp <- trim(opp)
        opp <- gsub("\'", "", opp)
        
        #Check if replacement is needed
        if (tm %in% replacements$Orignial) {
          tm <- replacements[replacements$Orignial == tm, 2]
        }
        if (opp %in% replacements$Orignial) {
          opp <- replacements[replacements$Orignial == opp, 2]
        }
        
        #Combine with team/opponent's season stats
        yr_src <- paste("tmstats_current_", today_year, sep = "")
        if (tm %in% get(yr_src)$basic_school_stats$School & opp %in% get(yr_src)$basic_school_stats$School) {
          #Add winning team data to games
          games[nrow(games)+1,"Team"] <- tm
          games[nrow(games), "Score"] <- day_games[[l]][1,2]
          games[nrow(games), "Opponent"] <- opp
          games[nrow(games), "Year"] <- today[[1]][1]
          
          #Append winning team stats to games
          games[nrow(games), 5:20] <- get(yr_src)$basic_school_stats[get(yr_src)$basic_school_stats$School == tm, 4:19]
          games[nrow(games), 21:36] <- get(yr_src)$basic_school_stats[get(yr_src)$basic_school_stats$School == opp, 4:19]
          
          #Add losing team data to games
          games[nrow(games)+1, "Team"] <- opp
          games[nrow(games), "Score"] <- day_games[[l]][2,2]
          games[nrow(games), "Opponent"] <- tm
          games[nrow(games), "Year"] <- today[[1]][1]
          
          games[nrow(games), 5:20] <- get(yr_src)$basic_school_stats[get(yr_src)$basic_school_stats$School == opp, 4:19]
          games[nrow(games), 21:36] <- get(yr_src)$basic_school_stats[get(yr_src)$basic_school_stats$School == tm, 4:19]
        }
      }
    }
  }
  
  #Add distinction for home team
  games <- cbind(games, "Home" = NA)
  
  for (m in 1:(length(games[[1]]) - 1)) {
    if (games$Opponent[m] == games$Team[m + 1]){
      games$Home[m] <- FALSE
    } else {
      games$Home[m] <- TRUE
    }
  }
  games$Home[length(games[[1]])] <- TRUE
  
  #Rearrange for input to model
  games$Score <- as.numeric(games$Score)
  input <- games[, c("Score", "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB",
                     "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", 
                     "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF", "Home")]
  
  #Run today's games through model
  predicted_scores <- predict(model, newdata = input)
  
  #Assemble predicted scores into output DF
  output <- data.frame("Away" = games$Team, "My Away Score" = predicted_scores, "Home" = games$Opponent, "My Home Score" = NA)
  i <- 1
  while (i < nrow(output)) {
    output$My.Home.Score[i] <- output$My.Away.Score[i + 1]
    output <- output[-(i+1),]
    i <- i + 1
  }
  output <- cbind(output, "My Spread" = output$My.Home.Score - output$My.Away.Score, "My O/U" = output$My.Away.Score + output$My.Home.Score)
  assign(paste("output", date, sep = ""), output)
  
  #Write to excel
  write.csv(get(paste("output", date, sep = "")),  file = "C:\\Users\\sowin\\OneDrive\\collegebball\\Model Output.csv", row.names = FALSE)
}
