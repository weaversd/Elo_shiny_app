create_calibration_DF <- function(elo_games, pctToNotInclude) {
  
  gamesToNotInclude <- floor(pctToNotInclude/100 * nrow(elo_games))
  elo_games <- elo_games[gamesToNotInclude:nrow(elo_games),]
  
  elo_games$homeWinner <- NA
  for (i in 1:nrow(elo_games)) {
    if (elo_games$homeTeam[i] == elo_games$winner[i]) {
      elo_games$homeWinner[i] <- 1
    } else if (elo_games$winner[i] == "DRAW") {
      elo_games$homeWinner[i] <- 0.5
    } else {
      elo_games$homeWinner[i] <- 0
    }
  }
  
  bins <- seq(0,1, 0.05)
  binDFs <- list()
  calibrationDF <- data.frame(expWinPct = bins[1:length(bins) - 1] + 0.025,
                              actualWinPct = NA,
                              count = NA)
  
  for (i in 1:nrow(calibrationDF)) {
    df <- elo_games[elo_games$homeWinPct > calibrationDF$expWinPct[i] & elo_games$homeWinPct <= calibrationDF$expWinPct[i] + 0.05,]
    df$bin <- calibrationDF$expWinPct[i]
    calibrationDF$count[i] <- nrow(df)
    calibrationDF$actualWinPct[i] <- sum(df$homeWinner) / nrow(df)
    binDFs[[length(binDFs) + 1]] <- df
  }
  
  calibrationDF$residual <- calibrationDF$actualWinPct - calibrationDF$expWinPct
  calibrationDF$scaledResidual <- (calibrationDF$residual * calibrationDF$count) / sum(calibrationDF$count)
  calibrationDF$absResidual <- abs(calibrationDF$residual)
  calibrationDF$absScaledResidual <- abs(calibrationDF$scaledResidual)
  
  return(calibrationDF)
}

return_calibration_error <- function(calibrationDF) {
  error <- sum(calibrationDF$absScaledResidual)
  averageError <- sum(calibrationDF$scaledResidual)
  
  return(c(error, averageError))
}

plot_calibration_curve <- function(calibrationDF) {
  return(ggplotly(ggplot(calibrationDF) +
             geom_abline(slope = 1, color = "grey") +
             geom_point(aes(x = expWinPct, y = actualWinPct, size = count), color = "black", alpha = 0.8,
                        fill = "blue", shape = 21) +
             theme_bw(base_size = 10) +
             theme(panel.grid = element_blank()) +
             labs(x = "Expected Win Percent",
                  y = "Actual Win Percent",
                  size = "Number\nof\ngames",
                  title = "Exp win % vs Actual win %") +
             geom_segment(aes(x = expWinPct, xend = expWinPct, y = actualWinPct, yend = expWinPct), color = "red",
                          linetype = "dash")))
}


plot_calibration_residuals <- function(calibrationDF) {
  return(ggplotly(ggplot(calibrationDF) +
             geom_hline(yintercept = 0, color = "grey") +
             geom_point(aes(x = expWinPct, y = residual, size = count), color = "black", alpha = 0.8,
                        fill = "blue", shape = 21) +
             theme_bw(base_size = 10) +
             theme(panel.grid = element_blank()) +
             labs(x = "Expected Win Percent",
                  y = "Difference From Actual",
                  size = "Number\nof\ngames",
                  title = "Residual Plot") +
             geom_segment(aes(x = expWinPct, xend = expWinPct, y = residual, yend = 0), color = "red",
                          linetype = "dash")))
}

plot_scaled_calibration_residuals <- function(calibrationDF) {
  return(ggplotly(ggplot(calibrationDF) +
             geom_hline(yintercept = 0, color = "grey") +
             geom_point(aes(x = expWinPct, y = scaledResidual, size = count), color = "black", alpha = 0.8,
                        fill = "blue", shape = 21) +
             theme_bw(base_size = 10) +
             theme(panel.grid = element_blank()) +
             labs(x = "Expected Win Percent",
                  y = "Difference From Actual scaled by number of games in bin",
                  size = "Number\nof\ngames",
                  title = "Scaled Residual Plot") +
             geom_segment(aes(x = expWinPct, xend = expWinPct, y = scaledResidual, yend = 0), color = "red",
                          linetype = "dash")))
}





  