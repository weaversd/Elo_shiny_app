optimize_KandHFA <- function(elo_games, HFARange, HFAiter, EloDenom,
                       MOVmult, MOVint, playoffMult,
                       startingElo, regressionMult, drawRate,
                       KRange, Kiter, pctToNotInclude) {
  
  Pct_correct_List <- list()
  
  for (iK in optimizerSteps(KRange[1], KRange[2], Kiter)) {
    for (iHFA in optimizerSteps(HFARange[1], HFARange[2], HFAiter)) {
      print(paste0(iHFA, " in ", HFARange[1], " to ", HFARange[2]))
      print(paste0(iK, " in ", KRange[1], " to ", KRange[2]))
      tempDF <- fill_elo_historical(elo_games, HFA = iHFA,
                                    EloDenom = EloDenom,
                                    MOVmult = MOVmult,
                                    MOVint = MOVint, K = iK,
                                    playoffMult = playoffMult,
                                    startingElo = startingElo,
                                    regressionMult = regressionMult,
                                    drawRate = drawRate)
      correctPct <- calculate_percent_correct(tempDF, pctToNotInclude)
      
      calibrationDF <- create_calibration_DF(tempDF, pctToNotInclude)
      
      calibrationScores <- return_calibration_error(calibrationDF)
      
      tempLine <- data.frame(value1 = iK,
                             value2 = iHFA,
                             correctPct = correctPct,
                             calibrationError = calibrationScores[1],
                             avgCalibrationError = calibrationScores[2])
      
      Pct_correct_List[[length(Pct_correct_List) + 1]] <- tempLine
    }
  }
  
  plotDF <- bind_rows(Pct_correct_List)
  return(plotDF)
}


plot_opt2D_DF <- function(optDF, title, xtitle, ytitle) {
  plot <- ggplot(optDF) +
    geom_tile(aes(x = value1, y = value2, fill = correctPct)) +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank()) +
    labs(x = xtitle, title = title, y = ytitle) +
    scale_fill_gradient(low = "yellow", high = "navy")
  
  
  plotlyObj <- ggplotly(plot)
  
  return(plotlyObj)
}

plot_opt2D_DF_residual <- function(optDF, title, xtitle, ytitle) {
  plot <- ggplot(optDF) +
    geom_tile(aes(x = value1, y = value2, fill = calibrationError)) +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank()) +
    labs(x = xtitle, title = title, y = ytitle) +
    scale_fill_gradient(low = "navy", high = "yellow")
  
  
  plotlyObj <- ggplotly(plot)
  
  return(plotlyObj)
}