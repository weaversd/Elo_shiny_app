get_draw_value <- function(drawRate, EloDenom) {
  De <- EloDenom * (log(-((drawRate + 1)/(drawRate - 1)), base = 10))
  return(De)
}

get_MOV_value <- function(MOV, eloDif, MOVint) {
  MOVvalue <- log(MOV + MOVint, base = exp(1)) * (2.2 / (((eloDif) * 0.0001) + 2.2))
  return(MOVvalue)
}

get_elo_dif_withHFA <- function(homeElo, awayElo, HFA) {
  eloDif <- homeElo - awayElo + HFA
  return(eloDif)
}

get_win_pcts <- function(eloDif, De, EloDenom) {
  Weh <- 1 / ((10^(((De - eloDif)/EloDenom))) + 1)
  Wea <- 1 / ((10^(((De + eloDif)/EloDenom))) + 1)
  Wed <- 1 - Weh - Wea
  
  return(c(Weh, Wea, Wed))
}

fill_elo_historical <- function(elo_games, HFA, EloDenom,
                                MOVmult, MOVint, K, playoffMult,
                                startingElo, regressionMult, drawRate) {
  
  elo_games$previousHomeElo <- startingElo
  elo_games$previousAwayElo <- startingElo
  elo_games$newHomeElo <- NA
  elo_games$newAwayElo <- NA
  elo_games$homeWinPct <- NA
  elo_games$awayWinPct <- NA
  elo_games$drawPct <- NA
  elo_games$expectedWinner <- NA
  elo_games$correct <- NA
  
  De <- get_draw_value(drawRate/100, EloDenom)
  
  for (i in 1:nrow(elo_games)) {
    
    eloDif <- get_elo_dif_withHFA(elo_games$previousHomeElo[i],
                                  elo_games$previousAwayElo[i], HFA)
    winPcts <- get_win_pcts(eloDif, De, EloDenom)
    
    ExpHome <- winPcts[1]
    ExpAway <- winPcts[2]
    ExpDraw <- winPcts[3]
    
    elo_games$homeWinPct[i] <- ExpHome
    elo_games$awayWinPct[i] <- ExpAway
    elo_games$drawPct[i] <- ExpDraw
    
    if (ExpHome >= ExpAway) {
      if (ExpHome > ExpDraw) {
        elo_games$expectedWinner[i] <- elo_games$homeTeam[i]
      } else {
        elo_games$expectedWinner[i] <- "DRAW"
      }
    } else if (ExpAway > ExpHome) {
      if (ExpAway > ExpDraw) {
        elo_games$expectedWinner[i] <- elo_games$awayTeam[i]
      } else {
        elo_games$expectedWinner[i] <- "DRAW"
      }
    }
    
    if (elo_games$expectedWinner[i] == elo_games$winner[i]) {
      elo_games$correct[i] <- T
    } else {
      elo_games$correct[i] <- F
    }
    
    if (elo_games$playoff[i]) {
      importance <- playoffMult
    } else {
      importance <- 1
    }
    
    
    MOV <- abs(elo_games$homeScore[i] - elo_games$awayScore[i])
    MOVvalue <- get_MOV_value(MOV, eloDif, MOVint)
    
    if (MOVmult == 0) {
      MOVfactor <- 1
    } else {
      MOVfactor <- MOVmult * MOVvalue
    }
    
    if (elo_games$winner[i] == "DRAW") {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOVfactor * K * importance * (0.5 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOVfactor * importance * (0.5 - ExpAway))
    } else if (elo_games$homeScore[i] > elo_games$awayScore[i]) {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOVfactor * K * importance * (1 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOVfactor * K * importance * (0 - ExpAway))
    } else if (elo_games$homeScore[i] < elo_games$awayScore[i]) {
      elo_games$newHomeElo[i] <- elo_games$previousHomeElo[i] + (MOVfactor * K * importance * (0 - ExpHome))
      elo_games$newAwayElo[i] <- elo_games$previousAwayElo[i] + (MOVfactor * K * importance * (1 - ExpAway))
    }
    if (i == nrow(elo_games)) {
      break
    }
    for (j in (i+1):nrow(elo_games)) {
      if (elo_games$homeTeam[i] == elo_games$homeTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousHomeElo[j] <- elo_games$newHomeElo[i]
        } else {
          elo_games$previousHomeElo[j] <- elo_games$newHomeElo[i] - ((elo_games$newHomeElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      } else if (elo_games$homeTeam[i] == elo_games$awayTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousAwayElo[j] <- elo_games$newHomeElo[i]
        } else {
          elo_games$previousAwayElo[j] <- elo_games$newHomeElo[i] - ((elo_games$newHomeElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      }
    }
    
    for (j in (i+1):nrow(elo_games)) {
      if (elo_games$awayTeam[i] == elo_games$homeTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousHomeElo[j] <- elo_games$newAwayElo[i]
        } else {
          elo_games$previousHomeElo[j] <- elo_games$newAwayElo[i] - ((elo_games$newAwayElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      } else if (elo_games$awayTeam[i] == elo_games$awayTeam[j]) {
        if (elo_games$season[i] == elo_games$season[j]) {
          elo_games$previousAwayElo[j] <- elo_games$newAwayElo[i]
        } else {
          elo_games$previousAwayElo[j] <- elo_games$newAwayElo[i] - ((elo_games$newAwayElo[i] - startingElo) * (regressionMult / 100))
        }
        break
      }
    }
  }
  return(elo_games)
}

calculate_percent_correct <- function(populated_elo_games, pctToNotInclude) {
  gamesToNotInclude <- floor(pctToNotInclude/100 * nrow(populated_elo_games))
  correctPctDF <- populated_elo_games[gamesToNotInclude:nrow(populated_elo_games),]
  percentCorrect <- (sum(correctPctDF$correct) / nrow(correctPctDF)) * 100
  return(percentCorrect)
} 


create_plotable_data <- function(elo_games) {
  season_vector <- unique(elo_games$season)
  team_vector <- unique(c(elo_games$homeTeam, elo_games$awayTeam))
  
  elo_games$date <- as.Date(elo_games$date)
  
  elo_list_by_team <- list()
  
  year_list <- list()
  for (season in season_vector) {
    elo_list_by_team <- list()
    for (i in 1:length(team_vector)) {
      team <- team_vector[i]
      team_games_df <- elo_games[elo_games$homeTeam == team | elo_games$awayTeam == team,]
      team_games_year <- team_games_df[grepl(season, team_games_df$season),]
      
      elo_list <- list()
      if (nrow(team_games_year) != 0) {
        for (j in 1:nrow(team_games_year)) {
          if (team_games_year$homeTeam[j] == team) {
            elo_list[[length(elo_list) + 1]] <- team_games_year$newHomeElo[j]
          } else if (team_games_year$awayTeam[j] == team) {
            elo_list[[length(elo_list) + 1]] <- team_games_year$newAwayElo[j]
          }
        }
        team_elos <- data.frame(team = team,
                                date = team_games_year$date,
                                elo = unlist(elo_list))
        
        elo_list_by_team[[length(elo_list_by_team) + 1]] <- team_elos
      }
    }
    year_list[[length(year_list) + 1]] <- elo_list_by_team
    
  }
  
  cutoffs <- list()
  cutoffs[[1]] <- 0

  year_list_dfs <- list()
  for (i in 1:length(year_list)) {
    year_list_dfs[[i]] <- bind_rows(year_list[[i]])
    year_list_dfs[[i]]$date <- as.Date(year_list_dfs[[i]]$date)
  }
  
  
  for (i in 1:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- as.numeric(year_list_dfs[[i]]$date)
  }
  
  for (i in 1:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric - min(year_list_dfs[[i]]$date_numeric)
  }
  
  
  for (i in 2:length(year_list_dfs)) {
    year_list_dfs[[i]]$date_numeric <- year_list_dfs[[i]]$date_numeric + max(year_list_dfs[[i-1]]$date_numeric) + 30
    cutoffs[[length(cutoffs) + 1]] <- max(year_list_dfs[[i-1]]$date_numeric) + 15
  }
  
  
  plot_df <- bind_rows(year_list_dfs)
  
  cutoffs[[length(cutoffs) + 1]] <- max(plot_df$date_numeric) + 15
  
  cutoff_vector <- unlist(cutoffs)
  
  return(list(plot_df, cutoff_vector))
}

create_elo_plot <- function(eloPlotDF, eloCutOffs) {
  plot <- ggplot(eloPlotDF, aes(date = date)) +
    geom_vline(xintercept = eloCutOffs) + 
    geom_line(aes(x = date_numeric, y = elo, colour = team)) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(x = "Time (arbitrary units)")
  
  plotlyObj <- ggplotly(plot, tooltip = c("date", "y", "colour"))
  return(plotlyObj)
}

percent_correct_time_df <- function(populated_elo_games, pctToNotInclude) {
  populated_elo_games$livePctCorrect <- NA
  populated_elo_games$livePctCorrectx <- 1:nrow(populated_elo_games)
  populated_elo_games$subsetLivePctCorrect <- NA
  gamesToNotInclude <- floor(pctToNotInclude/100 * nrow(populated_elo_games))
  for (i in 1:nrow(populated_elo_games)) {
    populated_elo_games$livePctCorrect[i] <- {
      (sum(populated_elo_games$correct[0:i]) / length(populated_elo_games$correct[0:i])) * 100
    }
    if (i >= gamesToNotInclude) {
      populated_elo_games$subsetLivePctCorrect[i] <- {
        (sum(populated_elo_games$correct[gamesToNotInclude:i]) / length(populated_elo_games$correct[gamesToNotInclude:i])) * 100
      }
    }
  }
  return(populated_elo_games)
}

plot_live_pct_correct <- function(livePctCorrectDF, pctToNotInclude) {
  includeCutoff <- floor(pctToNotInclude/100 * nrow(livePctCorrectDF))
  plot <- ggplot(livePctCorrectDF) +
    geom_vline(xintercept = includeCutoff, color = "grey") +
    geom_line(aes(x = livePctCorrectx, y = livePctCorrect), color = "blue") +
    geom_line(aes(x = livePctCorrectx, y = subsetLivePctCorrect), color = "green4") +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Percent Correct over time",
         x = "Time (arbitrary units)",
         y = "Correct (%)") +
    scale_y_continuous(limits = c(0,100))
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}

plot_live_percentile_score <- function(livePctCorrectDF, pctToNotInclude) {
  includeCutoff <- floor(pctToNotInclude/100 * nrow(livePctCorrectDF))
  plot <- ggplot(livePctCorrectDF) +
    geom_vline(xintercept = includeCutoff, color = "grey") +
    geom_line(aes(x = livePctCorrectx, y = livePercentileScore), color = "blue") +
    geom_line(aes(x = livePctCorrectx, y = subsetLivePercentileScore), color = "green4") +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Percentile Score over time (home win % and MOV)",
         x = "Time (arbitrary units)",
         y = "Percentile Score") +
    scale_y_continuous(limits = c(0,1))
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}

plot_live_combined_score <- function(livePctCorrectDF, pctToNotInclude) {
  includeCutoff <- floor(pctToNotInclude/100 * nrow(livePctCorrectDF))
  plot <- ggplot(livePctCorrectDF) +
    geom_vline(xintercept = includeCutoff, color = "grey") +
    geom_line(aes(x = livePctCorrectx, y = liveCombScore), color = "blue") +
    geom_line(aes(x = livePctCorrectx, y = subsetLiveCombScore), color = "green4") +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()) +
    labs(title = "Combined Score over time",
         x = "Time (arbitrary units)",
         y = "Combined Score") +
    scale_y_continuous(limits = c(0,1))
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}

plot_percentile_score_density <- function(percentileScoreDF) {
  plot <- ggplot(percentileScoreDF) +
    geom_density(aes(x = percentileScore, y = after_stat(count), color = correct, fill = correct),
                 alpha = 0.3, adjust = 1/1) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())+
    labs(y = "Density",
         x = "<- Worse Percentile Agreement | Better Percentile Agreement ->",
         title = "Percentile Score Density")
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}

plot_percentile_score_distribution <- function(percentileScoreDF) {
  apex <- density(percentileScoreDF$percentileScore)$x[match(max(density(percentileScoreDF$percentileScore)$y),
                density(percentileScoreDF$percentileScore)$y)]
  print(apex)
  plot <- ggplot(percentileScoreDF) +
    geom_density(aes(x = percentileScore), color = "green4",
                 fill = "green4", alpha = 0.3, adjust = 1/1) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank()) + 
  geom_vline(xintercept = apex) +
  labs(y = "Density",
       x = "<- Worse Percentile Agreement | Better Percentile Agreement ->",
       title = "Percentile Score Density")
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}



overlap_value <- function(elo_games) {
  homeMOV <- scale(elo_games$homeScore - elo_games$awayScore)
  homeWinPct <- scale(elo_games$homeWinPct)
  compare <- list(homeMOV, homeWinPct)
  out <- overlap(compare)$OV[[1]]
  return(out)
}

overlap_subset_value <- function(elo_games, pctToNotInclude) {
  gamesToNotInclude <- floor(pctToNotInclude/100 * nrow(elo_games))
  correctPctDF <- elo_games[gamesToNotInclude:nrow(elo_games),]
  return(overlap_value(correctPctDF))
}

overlap_plot <- function(elo_games, pctToNotInclude) {
  MOV <- scale(elo_games$homeScore - elo_games$awayScore)
  WinPct <- scale(elo_games$homeWinPct)
  MOVDF <- data.frame(category = "MOV",
                      value = MOV)
  WinPctDF <- data.frame(category = "WinPct",
                         value = WinPct)
  plotDF <- bind_rows(MOVDF, WinPctDF)
  plot <- ggplot(plotDF, aes(x = value, color = category, fill = category)) +
    geom_density(aes(y = ..density..), alpha = 0.3) +
    theme_bw(base_size = 20) +
    theme(panel.grid = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text = element_blank()) +
    labs(x = "Scaled Distribution",
         y = "Density",
         title = "Overlap between scaled home MOV and home Exp Win %",
         fill = "",
         color = "")
  
  plotlyObj <- ggplotly(plot)
  return(plotlyObj)
}

populate_percentile_score <- function(elo_games, pctToNotInclude, weightOfCorrect) {
  
  elo_games$homeMOV <- elo_games$homeScore - elo_games$awayScore
  
  elo_games$MOVNorm <- minMax(elo_games$homeMOV)
  elo_games$WinPctNorm <- minMax(elo_games$homeWinPct)
  
  percentileMOVNorm <- ecdf(elo_games$MOVNorm)
  percentileWinPctNorm <- ecdf(elo_games$WinPctNorm)
  
  elo_games$percentileMOVNorm <- percentileMOVNorm(elo_games$MOVNorm)
  elo_games$percentileWinPctNorm <- percentileWinPctNorm(elo_games$WinPctNorm)
  
  elo_games$percentileDif <- abs(elo_games$percentileWinPctNorm - elo_games$percentileMOVNorm)
  
  elo_games$maxPercDif <- NA
  for (i in 1:nrow(elo_games)) {
    dif1 <- 1 - elo_games$percentileWinPctNorm[i]
    dif2 <- elo_games$percentileWinPctNorm[i]
    elo_games$maxPercDif[i] <- max(c(dif1, dif2))
  }
  
  elo_games$pctPercentileDif <- elo_games$percentileDif / elo_games$maxPercDif
  
  elo_games$percentileScore <- 1 - elo_games$pctPercentileDif
  
  elo_games$combined_Score <- NA
  for (i in 1:nrow(elo_games)) {
    elo_games$combined_Score[i] <- return_weighted_score(elo_games$percentileScore[i],
                                                        as.numeric(elo_games$correct[i]),
                                                        weightOfCorrect)
  }
  
  elo_games$livePercentileScore <- NA
  elo_games$subsetLivePercentileScore <- NA
  elo_games$liveCombScore <- NA
  elo_games$subsetLiveCombScore <- NA
  gamesToNotInclude <- floor(pctToNotInclude/100 * nrow(elo_games))
  for (i in 1:nrow(elo_games)) {
    elo_games$liveCombScore[i] <- {
      (sum(elo_games$combined_Score[0:i]) / length(elo_games$combined_Score[0:i]))
    }
    elo_games$livePercentileScore[i] <- {
      (sum(elo_games$percentileScore[0:i]) / length(elo_games$percentileScore[0:i]))
    }
    if (i >= gamesToNotInclude) {
      elo_games$subsetLivePercentileScore[i] <- {
        (sum(elo_games$percentileScore[gamesToNotInclude:i]) / length(elo_games$percentileScore[gamesToNotInclude:i]))
      }
      elo_games$subsetLiveCombScore[i] <- {
        (sum(elo_games$combined_Score[gamesToNotInclude:i]) / length(elo_games$combined_Score[gamesToNotInclude:i]))
      }
    }
  }
  return(elo_games)
}

return_weighted_score <- function(percentileSCore, correct, weightOfCorrect) {

  score <- ((correct * weightOfCorrect) + (percentileSCore * (1 - weightOfCorrect)))
  
  return(score)
}
