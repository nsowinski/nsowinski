yest_results <- function() {
  
  model_output_yest <- read.csv("C:\\Users\\sowin\\OneDrive\\collegebball\\Model Output.csv", )
  
  yesterday_date <- Sys.Date() - 1
  yesterday <- str_split(yesterday_date, "[-]")
  
  yr <- yesterday[[1]][1]
  mth <- yesterday[[1]][2]
  day <- yesterday[[1]][3]
  
  url_string <- paste("https://www.sports-reference.com/cbb/boxscores/index.cgi?month=", mth, "&day=", day, "&year=", yr, sep = "")
  day_games_url <- getURL(url_string,.opts = list(ssl.verifypeer = FALSE) )
  day_games <- readHTMLTable(day_games_url)
  day_games <- list.clean(day_games, fun = is.null, recursive = FALSE)
  n.rows <- unlist(lapply(day_games, function(t) dim(t)[1]))
  
  yest_games <- data.frame(matrix(ncol = 5, nrow = 0))
  colnames(yest_games) <- c("Away", "Away Score", "Home", "Home Score", "Total")
  
  replacements <- data.frame(c("UConn", "LSU", "VCU", "BYU", "UCF", "Ole Miss", "UNC"), 
                             c("Connecticut", "Louisiana State", "Virginia Commonwealth", "Brigham Young", "Central Florida", "Mississippi", "	North Carolina"))
  colnames(replacements) <- c("Orignial", "Replacement")
  
  
  if(length(day_games) > 0) {
    #Add daily games to game DF
    for (l in 1:length(day_games)) {
      if(day_games[[l]][3,1] == "Men's") {
        tm <-day_games[[l]][1,1]
        opp <-day_games[[l]][2,1]
        str<-"[\\(,\\),1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25]"
        tm <- gsub(str, "", tm)
        tm <- trim(tm)
        tm <- gsub("\'", "", tm)
        opp <- gsub(str, "", opp)
        opp <- trim(opp)
        opp <- gsub("\'", "", opp)
        if (tm %in% replacements$Orignial) {
          tm <- replacements[replacements$Orignial == tm, 2]
        }
        if (opp %in% replacements$Orignial) {
          opp <- replacements[replacements$Orignial == opp, 2]
        }
        score1 <- as.integer(day_games[[l]][1,2])
        score2 <- as.integer(day_games[[l]][2,2])
        if (tm %in% model_output_yest$Away & opp %in% model_output_yest$Home) {
          #Add winning team data to games
          yest_games[nrow(yest_games)+1,"Away"] <- tm
          yest_games[nrow(yest_games), "Away Score"] <- score1
          yest_games[nrow(yest_games), "Home"] <- opp
          yest_games[nrow(yest_games), "Home Score"] <- score2
          yest_games[nrow(yest_games), "Total"] <- score1 + score2
        }
      }
    }
  }
  yest_games <- cbind(yest_games, "Actual Spread" = yest_games$`Home Score` - yest_games$`Away Score`)
  
  yest_games <- merge(yest_games, model_output_yest, by = "Away")
  
  yest_games <- yest_games[, c(1, 3, 2, 4, 7, 9, 6, 10, 5, 11)]

  yest_games <- cbind(yest_games, "Winner" = as.numeric(sign(yest_games$My.Spread) == sign(yest_games$`Actual Spread`)), 
                                  "Total_10" = as.numeric(abs(yest_games$Total - yest_games$My.O.U) < 10), 
                                  "Total_12.5" = as.numeric(abs(yest_games$Total - yest_games$My.O.U) < 12.5),
                                  "Total_15" = as.numeric(abs(yest_games$Total - yest_games$My.O.U) < 15), 
                                  "Spread_5" = as.numeric(abs(yest_games$`Actual Spread` - yest_games$My.Spread) < 5), 
                                  "Spread_7.5" = as.numeric(abs(yest_games$`Actual Spread` - yest_games$`My.Spread`) < 7.5),
                                  "Spread_10" = as.numeric(abs(yest_games$`Actual Spread` - yest_games$`My.Spread`) < 10))


  assign("to_excel", yest_games)
  write.table(to_excel, file = "C:\\Users\\sowin\\OneDrive\\collegebball\\Historical Output.csv", sep = ",", append = TRUE, col.names = FALSE, row.names = FALSE)
}
