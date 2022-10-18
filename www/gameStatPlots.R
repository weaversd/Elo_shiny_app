create_MOV_histogram <- function(games) {
  plot <- ggplot(games) +
    geom_bar(aes(x = MOV)) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank()) +
    labs(title = "Margin of Victory Histogram",
         x = "Margin of Victory",
         y = "Count")
  
  returnPlotly <- ggplotly(plot)
  return(returnPlotly)
}

create_score_histogram <- function(games) {
  finalScore <- c(games$homeScore, games$awayScore)
  plot <- ggplot() +
    geom_bar(aes(x = finalScore)) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank()) +
    labs(title = "final Scores Histogram",
         x = "Final Score",
         y = "Count")
  
  returnPlotly <- ggplotly(plot)
  return(returnPlotly)
}

create_homeAwayMOV_histogram <- function(games) {
  away_winners <- games[games$awayScore > games$homeScore,]
  home_winners <- games[games$homeScore > games$awayScore,]
  away_winners$category <- "away_win"
  home_winners$category <- "home_win"
  plotDF <- bind_rows(home_winners, away_winners)
  
  plot <- ggplot(plotDF) +
    geom_density(aes(x = MOV, color = category, linetype = category)) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank()) +
    labs(title = "Margin of Victory: Home vs Away",
         x = "Margin of Victory",
         y = "Density (out of 1)") +
    geom_hline(yintercept = 0)
  
  returnPlotly <- ggplotly(plot)
  return(returnPlotly)
}

create_homeAwayScore_histogram <- function(games) {
  homeScores <- data.frame(score = games$homeScore,
                           category = "home")
  awayScores <- data.frame(score = games$awayScore,
                           category = "away")
  plotDF <- bind_rows(homeScores, awayScores)
  
  plot <- ggplot(plotDF) +
    geom_density(aes(x = score, color = category, linetype = category)) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank()) +
    labs(title = "Final Score: Home vs Away",
         x = "Final Score",
         y = "Density (out of 1)") +
    geom_hline(yintercept = 0)
  
  returnPlotly <- ggplotly(plot)
  return(returnPlotly)
}

create_homeAwayScore_chart <- function(WinLossTieDF) {
  
  plotDF <- data.frame(category = names(WinLossTieDF),
                       value = as.numeric(WinLossTieDF[1,]))
  
  plot <- ggplot(plotDF) +
    geom_bar(aes(x = category, y = value, fill = category), stat = "identity") +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank(),
          legend.position = "none") +
    labs(title = "Win Pct: Home vs Away",
         x = element_blank(),
         y = "% of Games") +
    scale_y_continuous(limits = c(0,100))
  
  returnPlotly <- ggplotly(plot)
  return(returnPlotly)
}