get_team_list <- function(games) {
  teams <- unique(c(games$homeTeam, games$awayTeam))
  teamStatLines <- list()
  for (team in teams) {
   teamDF <- games[games$homeTeam == team | games$awayTeam == team,]
   wins <- 0
   losses <- 0
   ties <- 0
   pts_scored <- 0
   pts_against <- 0
   played <- nrow(teamDF)
   for (i in 1:nrow(teamDF)) {
     if (teamDF$winner[i] == team) {
       wins <- wins + 1
     } else if (teamDF$winner[i] == "DRAW") {
       ties <- ties + 1
     } else {
       losses <- losses + 1
     }
     
     if (teamDF$homeTeam[i] == team) {
       pts_scored <- pts_scored + teamDF$homeScore[i]
       pts_against <- pts_against + teamDF$awayScore[i]
     } else {
       pts_scored <- pts_scored + teamDF$awayScore[i]
       pts_against <- pts_against + teamDF$homeScore[i]
     }
    }
   winPCT <- round(((wins + (0.5 * ties)) / played) * 100, 2)
   plusMinus <- pts_scored - pts_against
   
   teamStats <- data.frame(team = team,
                           played = as.integer(played),
                           wins = as.integer(wins),
                           losses = as.integer(losses),
                           ties = as.integer(ties),
                           winPCT = winPCT,
                           pts_scored = as.integer(pts_scored),
                           pts_against = as.integer(pts_against),
                           plusMinus = as.integer(plusMinus))
   teamStatLines[[length(teamStatLines) + 1]] <- teamStats
  }
  
  returnDF <- bind_rows(teamStatLines)
  return(returnDF)
}

populate_games <- function(games) {
  
  games$winner <- NA
  games$loser <- NA
  games$MOV <- NA
  
  for (i in 1:nrow(games)) {
    if (games$homeScore[i] == games$awayScore[i]) {
      games$winner[i] <- "DRAW"
      games$loser[i] <- "DRAW"
      games$MOV[i] <- 0
    } else if (games$homeScore[i] > games$awayScore[i]) {
      games$winner[i] <- games$homeTeam[i]
      games$loser[i] <- games$awayTeam[i]
      games$MOV[i] <- games$homeScore[i] - games$awayScore[i]
    } else if (games$awayScore[i] > games$homeScore[i]) {
      games$winner[i] <- games$awayTeam[i]
      games$loser[i] <- games$homeTeam[i]
      games$MOV[i] <- games$awayScore[i] - games$homeScore[i]
    }
  }
  
  return(games)
}

optimizerSteps <- function(start, end, iter) {
  sequence <- seq(start, end, iter)
  if (sequence[length(sequence)] != end) {
    sequence <- c(sequence, end)
  }
  return(sequence)
}

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

