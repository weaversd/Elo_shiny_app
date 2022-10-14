generate_matrix <- function(RangeList) {
  
  HFAseq <- seq(RangeList[["HFARange"]][1], RangeList[["HFARange"]][2], length.out = 5)
  MOVseq <- seq(RangeList[["MOVRange"]][1], RangeList[["MOVRange"]][2], length.out = 5)
  Regseq <- seq(RangeList[["REGRange"]][1], RangeList[["REGRange"]][2], length.out = 5)
  Kseq <- seq(RangeList[["KRange"]][1], RangeList[["KRange"]][2], length.out = 5)
  
  trials <- expand.grid(HFA = HFAseq,
                        MOV = MOVseq,
                        REG = Regseq,
                        K = Kseq)
  return(trials)
}

runReplicate <- function(RangeList,
                         elo_games, pctToNotInclude, EloDenom,
                         MOVint, playoffMult,
                         startingElo, drawRate, updateProgress = NULL, iterationN) {
  
  testConditions <- generate_matrix(RangeList)
  testConditions$residualScore <- NA
  for (i in 1:nrow(testConditions)) {
    
    if (is.function(updateProgress)) {
      text <- paste0("Iteration: ", iterationN)
      value <- i / nrow(testConditions)
      updateProgress(detail = text, value = value)
    } else {
      print("no progress bar")
    }
    
    #print(paste0(round(i/nrow(testConditions) * 100, 1), "% Complete"))
    data <- fill_elo_historical(elo_games = elo_games, HFA = testConditions$HFA[i],
                                EloDenom = EloDenom,
                                MOVmult = testConditions$MOV[i],
                                MOVint = MOVinteger, K = testConditions$K[i],
                                playoffMult = playoffMult,
                                startingElo = startingElo,
                                regressionMult = testConditions$REG[i],
                                drawRate = drawRate)
    calibration <- create_calibration_DF(data, pctToNotInclude = pctToNotInclude)
    
    testConditions$residualScore[i] <- return_calibration_error(calibration)[1]
  }
  testConditions <- testConditions[order(testConditions$residualScore),]
  return(testConditions)
}

create_new_ranges <- function(oldRangeList,
                              replicateDF,
                              topN, 
                              convergeFactor) {
  top <- replicateDF[1:topN,]
  newHFA <- median(top$HFA)
  newMOV <- median(top$MOV)
  newREG <- median(top$REG)
  newK <- median(top$K)
  
  HFApm <- ((oldRangeList[["HFARange"]][2] - oldRangeList[["HFARange"]][1]) / 2) * convergeFactor
  MOVpm <- ((oldRangeList[["MOVRange"]][2] - oldRangeList[["MOVRange"]][1]) / 2) * convergeFactor
  REGpm <- ((oldRangeList[["REGRange"]][2] - oldRangeList[["REGRange"]][1]) / 2) * convergeFactor
  Kpm <- ((oldRangeList[["KRange"]][2] - oldRangeList[["KRange"]][1]) / 2) * convergeFactor
  
  HFARange <- c(newHFA - HFApm, newHFA + HFApm)
  MOVRange <- c(newMOV - MOVpm, newMOV + MOVpm)
  REGRange <- c(newREG - REGpm, newREG + REGpm)
  KRange <- c(newK - Kpm, newK + Kpm)
  
  HFARange[HFARange < 0] <- 0
  MOVRange[MOVRange < 0] <- 0
  REGRange[REGRange < 0] <- 0
  KRange[KRange < 0] <- 0
  
  return(list(RangeList = list(HFARange = HFARange,
                               MOVRange = MOVRange,
                               REGRange = REGRange,
                               KRange = KRange), medians = data.frame(HFA = newHFA,
                                                                      MOV = newMOV,
                                                                      REG = newREG,
                                                                      K = newK)))
  
} 

optimize_residul_score <- function(RangeList,
                                   elo_games, pctToNotInclude, EloDenom,
                                   MOVinteger, playoffMult, startingElo,
                                   drawRate, iterationN, topN, convergeFactor,
                                   updateProgress = NULL) {
  resultList <- list()
  medianList <- list()
  rangesList <- list()
  for (n in 1:iterationN) {
    if (n == 1) {
      rangesList[[1]] <- RangeList
    }
    replicate <- runReplicate(RangeList,
                              elo_games, pctToNotInclude, EloDenom,
                              MOVinteger, playoffMult, startingElo,
                              drawRate, updateProgress = updateProgress,
                              iterationN = n)
    replicate$iteration <- as.character(n)
    resultList[[length(resultList) + 1]] <- replicate
    UpdatedValues <- create_new_ranges(RangeList, replicate, topN, convergeFactor)
    RangeList <- UpdatedValues[["RangeList"]]
    rangesList[[length(rangesList) + 1]] <- RangeList
    newValuesReplicate <- fill_elo_historical(elo_games = elo_games, HFA = UpdatedValues[["medians"]]$HFA[1],
                                              EloDenom = EloDenom,
                                              MOVmult = UpdatedValues[["medians"]]$MOV[1],
                                              MOVint = MOVinteger, K = UpdatedValues[["medians"]]$K[1],
                                              playoffMult = playoffMult,
                                              startingElo = startingElo,
                                              regressionMult = UpdatedValues[["medians"]]$REG[1],
                                              drawRate = drawRate)
    
    newValueCalibration <- create_calibration_DF(newValuesReplicate, pctToNotInclude = pctToNotInclude)
    
    UpdatedValues[["medians"]]$residualScore[1] <- return_calibration_error(newValueCalibration)[1]
    UpdatedValues[["medians"]]$iteration[1] <- paste0("median_", n)
    
    medianList[[length(medianList) + 1]] <- UpdatedValues[["medians"]]
  }
  all_Results <- bind_rows(resultList)
  all_medians <- bind_rows(medianList)
  all_Results_and_medians <- bind_rows(all_Results, all_medians)
  all_Results <- all_Results[order(all_Results$residualScore),]
  all_Results_and_medians <- all_Results_and_medians[order(all_Results_and_medians$residualScore),]
  
  topResults <- all_Results_and_medians[1,]
  
  range_table <- bind_rows(rangesList)
  range_table$iteration <- rep(1:(nrow(range_table)/2), each = 2)
  
  return(list(allTrialResults = all_Results,
              allCalculatedValues = all_Results_and_medians,
              allMedians = all_medians,
              optimizedValues = topResults,
              allRanges = range_table))
}

plot_residualScore_vs_Iteration <- function(OptimizationObj) {
  
  OptimizationObj$allMedians$iterationN <- 1:nrow(OptimizationObj$allMedians)
  
  plot <- ggplot() +
      geom_jitter(data = OptimizationObj$allTrialResults, aes(x = as.numeric(iteration), y = residualScore),
                  height = 0, width = .1) +
    theme_bw(base_size = 10) +
    theme(panel.grid = element_blank()) +
    scale_y_continuous(trans = "log2") +
    geom_point(data = OptimizationObj$allMedians, aes(x = iterationN, y = residualScore),
               color = "hotpink", size = 5) +
    labs(y = "Residual Score", x = "iteration",
         title = "Residual scores from each optmization simulation")
  
  return(ggplotly(plot))
}

plot_optimization_range_HFA <- function(OptimizationObj) {
  data <- OptimizationObj$allRanges
  data <- data[data$iteration < max(data$iteration),]
  plot <- ggplot() +
    geom_point(data = data, aes(x = iteration,
                                y = HFARange,
                                color = as.factor(iteration)),
               size = 4) +
    labs(x = "iteration", y = "HFA", title = "HFA Optimization Ranges") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")
  return(ggplotly(plot))
}

plot_optimization_range_REG <- function(OptimizationObj) {
  data <- OptimizationObj$allRanges
  data <- data[data$iteration < max(data$iteration),]
  plot <- ggplot() +
    geom_point(data = data, aes(x = iteration,
                                y = REGRange,
                                color = as.factor(iteration)),
               size = 4) +
    labs(x = "iteration", y = "Regression %", title = "Regression to Mean Optimization Ranges") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")
  return(ggplotly(plot))
}

plot_optimization_range_K <- function(OptimizationObj) {
  data <- OptimizationObj$allRanges
  data <- data[data$iteration < max(data$iteration),]
  plot <- ggplot() +
    geom_point(data = data, aes(x = iteration,
                                y = KRange,
                                color = as.factor(iteration)),
               size = 4) +
    labs(x = "iteration", y = "K Value", title = "K Optimization Ranges") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")
  return(ggplotly(plot))
}

plot_optimization_range_MOV <- function(OptimizationObj) {
  data <- OptimizationObj$allRanges
  data <- data[data$iteration < max(data$iteration),]
  plot <- ggplot() +
    geom_point(data = data, aes(x = iteration,
                                y = MOVRange,
                                color = as.factor(iteration)),
               size = 4) +
    labs(x = "iteration", y = "MOV", title = "Margin of Victory Optimization Ranges") +
    theme_bw(base_size = 10) +
    theme(legend.position = "none")
  return(ggplotly(plot))
}

# 
# 
# 
# 
# 
# HFARange <- c(50,150)
# MOVRange <- c(0.5,2.5)
# REGRange <- c(0,60)
# KRange <- c(10,60)
# 
# RangeList <- list(HFARange = HFARange,
#                   MOVRange = MOVRange,
#                   REGRange = REGRange,
#                   KRange = KRange)
# 
# test <- optimize_residul_score(RangeList = RangeList, elo_games = elo_games, pctToNotInclude = 10,
#                                EloDenom = 400, MOVinteger = 1, playoffMult = 1.2, startingElo = 400,
#                                drawRate = 0.16, iterationN = 5, topN = 10, convergeFactor = 0.5)
# 
# ggplot() +
#   geom_point(data = test$allRanges, aes(x = iteration, y = HFARange, color = as.factor(iteration)), size = 4) +
#   labs(x = "iteration", y = "HFA", title = "HFA Optimization")
# 
# ggplot() + 
#   geom_jitter(data = test$allTrialResults, aes(x = as.numeric(iteration), y = residualScore),
#               height = 0, width = .1) +
#   scale_y_continuous(trans = "log2") +
#   #geom_point(data = test$allMedians, aes(x = iterationN, y = residualScore),
#   #           color = "hotpink", size = 5) +
#   labs(y = "Residual Score", x = "iteration")
# 
# test$allMedians$iterationN <- 1:nrow(test$allMedians)
# 
# test$allCalculatedValues
