library(rsm)
elo_games <- read.csv("exampleData/test.csv")

pctToNotInclude <- 10
drawRate <- 0.001
playoffMult <- 1.2

KStart <- 30
Kpm <- 5

RegStart <- 20
Regpm <- 5

MOVStart <- 1
MOVpm <- 0.2

HFAStart <- 100
HFApm <- 10


createCodedTrials <- function(KStart, Kpm,
                              RegStart,Regpm,
                              MOVStart, MOVpm,
                              HFAStart,HFApm,
                              eloGames,
                              playoffMult,
                              drawRate,
                              pctToNotInclude) {
  Ks <- c(KStart - Kpm, KStart, KStart + Kpm)
  Regs <- c(RegStart - Regpm, RegStart, RegStart + Regpm)
  MOVs <- c(MOVStart - MOVpm, MOVStart, MOVStart + MOVpm)
  HFAs <- c(HFAStart - HFApm, HFAStart, HFAStart + HFApm)
  
  
  trials <- data.frame(expand.grid(K = Ks,
                                   Regression = Regs,
                                   MOVx = MOVs,
                                   HFA = HFAs))
  
  trials$residualScore <- NA
  
  
  for (i in 1:nrow(trials)) {
    print(paste0(round(i/nrow(trials)*100, 1), "% Complete"))
    data <- fill_elo_historical(elo_games = elo_games, HFA = trials$HFA[i],
                                EloDenom = EloDenom,
                                MOVmult = trials$MOVx[i],
                                MOVint = MOVinteger, K = trials$K[i],
                                playoffMult = playoffMult,
                                startingElo = startingElo,
                                regressionMult = trials$Regression[i],
                                drawRate = drawRate)
    
    calibration <- create_calibration_DF(data, pctToNotInclude = pctToNotInclude)
    
    trials$residualScore[i] <- return_calibration_error(calibration)[1]
  }
  
  codedTrials <- coded.data(trials, x1 ~ (K - KStart)/Kpm,
                            x2 ~ (Regression - RegStart)/Regpm,
                            x3 ~ (MOVx - MOVStart)/MOVpm,
                            x4 ~ (HFA - HFAStart)/HFApm)
  return(codedTrials)
}

trials1 <- createCodedTrials(KStart, Kpm,
                             RegStart,Regpm,
                             MOVStart,MOVpm,
                             HFAStart,HFApm,
                             eloGames = eloGames,
                             playoffMult = 1.2,
                             drawRate = drawRate)

exp1 <- rsm(data = trials1, formula = residualScore ~ FO(x1, x2, x3, x4))
summary1 <- summary(exp1)
significance <- summary1$lof$`Pr(>F)`[1]
SA1 <- steepest(exp1, descent = T)
resultingScore <- SA1$yhat[1]
SA1

SA1$residualScore <- NA
for (i in 1:nrow(SA1)) {
  print(paste0(round(i/nrow(SA1)*100, 1), "% Complete"))
  data <- fill_elo_historical(elo_games = elo_games, HFA = SA1$HFA[i],
                              EloDenom = EloDenom,
                              MOVmult = SA1$MOVx[i],
                              MOVint = MOVinteger, K = SA1$K[i],
                              playoffMult = playoffMult,
                              startingElo = startingElo,
                              regressionMult = SA1$Regression[i],
                              drawRate = drawRate)
  
  calibration <- create_calibration_DF(data, pctToNotInclude = pctToNotInclude)
  
  SA1$residualScore[i] <- return_calibration_error(calibration)[1]
}

newCenter <- SA1[which.min(SA1$residualScore),]

trials2 <- createCodedTrials(KStart = newCenter$K[1],
                             Kpm = Kpm,
                             RegStart = newCenter$Regression[1],
                             Regpm = Regpm,
                             MOVStart = newCenter$MOVx[1],
                             MOVpm = MOVpm,
                             HFAStart = newCenter$HFA[1],
                             HFApm = HFApm,
                             eloGames = eloGames,
                             playoffMult = playoffMult,
                             drawRate = drawRate,
                             pctToNotInclude = pctToNotInclude)

exp2 <- rsm(data = trials2, formula = residualScore ~ FO(x1, x2, x3, x4))
summary2 <- summary(exp2)
summary2$sa
code2val(-5*(summary2$sa), codings(trials2))
significance <- summary2$lof$`Pr(>F)`[1]


