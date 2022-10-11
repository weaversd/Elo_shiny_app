optimize_K <- function(elo_games, HFARange, EloDenom,
                       MOVmultRange, MOVint, playoffMult,
                       startingElo, regressionMultRange, drawRate,
                       KRange, Kiter, pctToNotInclude) {
  
  Pct_correct_List <- list()
  
  for (iK in optimizerSteps(KRange[1], KRange[2], Kiter)) {
    tempDF <- fill_elo_historical(elo_games, HFA = mean(HFARange),
                                  EloDenom = EloDenom,
                                  MOVmult = mean(MOVmultRange),
                                  MOVint = MOVint, K = iK,
                                  playoffMult = playoffMult,
                                  startingElo = startingElo,
                                  regressionMult = mean(regressionMultRange),
                                  drawRate = drawRate)
    correctPct <- calculate_percent_correct(tempDF, pctToNotInclude) 
    
    tempLine <- data.frame(value = iK,
                           correctPct = correctPct)
    
    Pct_correct_List[[length(Pct_correct_List) + 1]] <- tempLine
  }
  
  plotDF <- bind_rows(Pct_correct_List)
  return(plotDF)
}

optimize_MOVmult <- function(elo_games, HFARange, EloDenom,
                       MOVmultRange, MOVint, playoffMult,
                       startingElo, regressionMultRange, drawRate,
                       KRange, MOVmultiter, pctToNotInclude) {
  
  Pct_correct_List <- list()
  
  for (iMOVmult in optimizerSteps(MOVmultRange[1], MOVmultRange[2], MOVmultiter)) {
    tempDF <- fill_elo_historical(elo_games, HFA = mean(HFARange),
                                  EloDenom = EloDenom,
                                  MOVmult = iMOVmult,
                                  MOVint = MOVint, K = mean(KRange),
                                  playoffMult = playoffMult,
                                  startingElo = startingElo,
                                  regressionMult = mean(regressionMultRange),
                                  drawRate = drawRate)
    correctPct <- calculate_percent_correct(tempDF, pctToNotInclude) 
    
    tempLine <- data.frame(value = iMOVmult,
                           correctPct = correctPct)
    
    Pct_correct_List[[length(Pct_correct_List) + 1]] <- tempLine
  }
  
  plotDF <- bind_rows(Pct_correct_List)
  return(plotDF)
}



optimize_PctReg <- function(elo_games, HFARange, EloDenom,
                             MOVmultRange, MOVint, playoffMult,
                             startingElo, regressionMultRange, drawRate,
                             KRange, regressionMultIter, pctToNotInclude) {
  
  Pct_correct_List <- list()
  
  for (iPctReg in optimizerSteps(regressionMultRange[1], regressionMultRange[2], regressionMultIter)) {
    tempDF <- fill_elo_historical(elo_games, HFA = mean(HFARange),
                                  EloDenom = EloDenom,
                                  MOVmult = mean(MOVmultRange),
                                  MOVint = MOVint, K = mean(KRange),
                                  playoffMult = playoffMult,
                                  startingElo = startingElo,
                                  regressionMult = iPctReg,
                                  drawRate = drawRate)
    correctPct <- calculate_percent_correct(tempDF, pctToNotInclude) 
    
    tempLine <- data.frame(value = iPctReg,
                           correctPct = correctPct)
    
    Pct_correct_List[[length(Pct_correct_List) + 1]] <- tempLine
  }
  
  plotDF <- bind_rows(Pct_correct_List)
  return(plotDF)
}

optimize_HFA <- function(elo_games, HFARange, HFAiter, EloDenom,
                            MOVmultRange, MOVint, playoffMult,
                            startingElo, regressionMultRange, drawRate,
                            KRange, pctToNotInclude) {
  
  Pct_correct_List <- list()
  
  for (iHFA in optimizerSteps(HFARange[1], HFARange[2], HFAiter)) {
    tempDF <- fill_elo_historical(elo_games, HFA =iHFA,
                                  EloDenom = EloDenom,
                                  MOVmult = mean(MOVmultRange),
                                  MOVint = MOVint, K = mean(KRange),
                                  playoffMult = playoffMult,
                                  startingElo = startingElo,
                                  regressionMult = mean(regressionMultRange),
                                  drawRate = drawRate)
    correctPct <- calculate_percent_correct(tempDF, pctToNotInclude) 
    
    tempLine <- data.frame(value = iHFA,
                           correctPct = correctPct)
    
    Pct_correct_List[[length(Pct_correct_List) + 1]] <- tempLine
  }
  
  plotDF <- bind_rows(Pct_correct_List)
  return(plotDF)
}


plot_opt_DF <- function(optDF, title, xtitle, color = "black") {
  plot <- ggplot(optDF) +
    geom_line(aes(x = value, y = correctPct), color = color) +
    theme_bw(base_size = 15) +
    theme(panel.grid = element_blank()) +
    labs(x = xtitle, title = title, y = "Games Predicted Correctly (%)")
  
  
  plotlyObj <- ggplotly(plot)
  
  return(plotlyObj)
}
