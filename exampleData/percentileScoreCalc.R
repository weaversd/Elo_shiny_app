test <- read.csv("exampleData/testEloData.csv")

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

homeMOV <- test$homeScore - test$awayScore
homeWinPct <- test$homeWinPct



df <- data.frame(MOV = homeMOV,
                WinPct = homeWinPct,
                MOVNorm = minMax(homeMOV),
                WinPctNorm = minMax(homeWinPct))

df$correct <- test$correct

ggplot(df) + 
  geom_jitter(aes(y = 1, x = MOV), width = 0)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = MOVNorm), width = 0)


ggplot(df) + 
  geom_jitter(aes(y = 1, x = WinPct), width = 0)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = WinPctNorm), width = 0)

percentileMOVNorm <- ecdf(df$MOVNorm)
percentileWinPctNorm <- ecdf(df$WinPctNorm)

df$percentileMOVNorm <- percentileMOVNorm(df$MOVNorm)
df$percentileWinPctNorm <- percentileWinPctNorm(df$WinPctNorm)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = percentileMOVNorm), width = 0)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = percentileWinPctNorm), width = 0)

df$percentileDif <- abs(df$percentileWinPctNorm - df$percentileMOVNorm)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = percentileDif), width = 0)

df$maxPercDif <- NA
for (i in 1:nrow(df)) {
  dif1 <- 1 - df$percentileWinPctNorm[i]
  dif2 <- df$percentileWinPctNorm[i]
  df$maxPercDif[i] <- max(c(dif1, dif2))
}

df$pctPercentileDif <- df$percentileDif / df$maxPercDif

ggplot(df) + 
  geom_jitter(aes(y = 1, x = pctPercentileDif), width = 0)

df$invPctPercentileDif <- 1 - df$pctPercentileDif

ggplot(df) + 
  geom_jitter(aes(y = 1, x = invPctPercentileDif, color = correct), width = 0)

