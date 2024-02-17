train_model <- function() {
  saved_games <- read.csv("C:\\Users\\sowin\\OneDrive\\collegebball\\Games_18_22.csv")
  
  input <- saved_games[, c("Score", "Tm_SRS", "Tm_SOS", "Tm_PTSFOR", "Tm_FG", "Tm_FGA", "Tm_3P", "Tm_3PA", "Tm_FT", "Tm_FTA", "Tm_ORB", "Tm_TRB",
                           "Tm_AST", "Tm_STL", "Tm_BLK", "Tm_TOV", "Tm_PF", "Opp_SRS", "Opp_SOS", "Opp_PTSFOR", "Opp_FG", "Opp_FGA", "Opp_3P", "Opp_3PA", 
                           "Opp_FT", "Opp_FTA", "Opp_ORB", "Opp_TRB", "Opp_AST", "Opp_STL", "Opp_BLK", "Opp_TOV", "Opp_PF", "Home")]
  
  input$Score <- as.numeric(input$Score)
  model <- lm(input$Score ~., data = input)
  
  return(model)
}
