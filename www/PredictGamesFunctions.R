fill_elo_scores_prediction <- function(historical_elo, future_games, startingElo) {
  teams <- unique(c(future_games$awayTeam, future_games$homeTeam)) 
  
  future_games$homeElo <- NA
  future_games$awayElo <- NA
  
  for (team in teams) {
    team_historical_df <- historical_elo[historical_elo$homeTeam == team | historical_elo$awayTeam == team,]
    if (nrow(team_historical_df) > 0){
      final_elo_score_df <- team_historical_df[nrow(team_historical_df),]
      if (final_elo_score_df$homeTeam[1] == team) {
        team_elo_score <- final_elo_score_df$newHomeElo[1]
      } else {
        team_elo_score <- final_elo_score_df$newAwayElo[1]
      }
      
      for (i in 1:nrow(future_games)) {
        if (future_games$homeTeam[i] == team) {
          future_games$homeElo[i] <- team_elo_score
        } else if (future_games$awayTeam[i] == team) {
          future_games$awayElo[i] <- team_elo_score
        }
      }
    }
  }
  future_games$homeElo[is.na(future_games$homeElo)] <- startingElo
  future_games$awayElo[is.na(future_games$awayElo)] <- startingElo
  return(future_games)
}

fill_winPcts_prediction <- function(future_games, drawRate, EloDenom, HFA) {
  De <- get_draw_value(drawRate/100, EloDenom)
  future_games$homeWinPct <- NA
  future_games$awayWinPct <- NA
  future_games$drawPct <- NA
  for (i in 1:nrow(future_games)) {
    
    eloDif <- get_elo_dif_withHFA(future_games$homeElo[i],
                                  future_games$awayElo[i], HFA)
    winPcts <- get_win_pcts(eloDif, De, EloDenom)
    
    ExpHome <- winPcts[1]
    ExpAway <- winPcts[2]
    ExpDraw <- winPcts[3]
    
    future_games$homeWinPct[i] <- ExpHome
    future_games$awayWinPct[i] <- ExpAway
    future_games$drawPct[i] <- ExpDraw
  }
  return(future_games)
}


summarise_predictions <- function(games) {
  teams <- unique(c(games$homeTeam, games$awayTeam))
  teamStatLines <- list()
  for (team in teams) {
    teamDF <- games[games$homeTeam == team | games$awayTeam == team,]
    wins <- 0
    losses <- 0
    ties <- 0
    for (i in 1:nrow(teamDF)) {
      if (teamDF$homeTeam[i] == team) {
        wins <- wins + teamDF$homeWinPct[i]
        losses <- losses + teamDF$awayWinPct[i]
        ties <- ties + teamDF$drawPct[i]
      } else if (teamDF$awayTeam[i] == team) {
        wins <- wins + teamDF$awayWinPct[i]
        losses <- losses + teamDF$homeWinPct[i]
        ties <- ties + teamDF$drawPct[i]
      }
    }
    winPCT <- round(((wins + (0.5 * ties)) / nrow(teamDF)) * 100, 2)
    teamStats <- data.frame(team = team,
                            PredictedWins = round(wins, 0),
                            PredictedLosses = round(losses,0),
                            PredictedTies = round(ties,0),
                            winPCT_unrounded = winPCT)
    teamStatLines[[length(teamStatLines) + 1]] <- teamStats
  }
  
  returnDF <- bind_rows(teamStatLines)
  return(returnDF)
}

