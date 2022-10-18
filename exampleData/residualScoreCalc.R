test <- read.csv("exampleData/testEloData.csv")
homeMOV <- test$homeScore - test$awayScore
homeWinPct <- test$homeWinPct
MOVz <- scale(homeMOV)
WinPctz <- scale(homeWinPct)
n <- length(homeMOV)
df <- data.frame(MOV = homeMOV,
                 WinPct = homeWinPct,
                 MOVz = MOVz,
                 WinPctz = WinPctz,
                 MOVp = pnorm(abs(MOVz), lower.tail = T),
                 WinPctp = pnorm(abs(WinPctz), lower.tail = T))
df$combP <- df$MOVp * df$WinPctp
df$difference <- abs(df$MOVz - df$WinPctz)
df$Pdif <- df$combP * df$difference

ggplot(df) + 
  geom_jitter(aes(y = 1, x = MOV))
ggplot(df) + 
  geom_jitter(aes(y = 1, x = MOVz))
ggplot(df) + 
  geom_jitter(aes(y = 1, x = MOVp))


ggplot(df) + 
  geom_jitter(aes(y = 1, x = WinPct))
ggplot(df) + 
  geom_jitter(aes(y = 1, x = WinPctz))
ggplot(df) + 
  geom_jitter(aes(y = 1, x = WinPctp))




ggplot(df) +
  geom_point(aes(x = WinPctz, y = MOVz, color = combP)) +
  geom_smooth(aes(x = WinPctz, y = MOVz), method = lm, se = F, color = "red", size = 2)

fit <- lm(df$MOVz ~ df$WinPctz)
slope <- fit$coefficients[[2]]
intercept <- fit$coefficients[[1]]

df$predicted <- df$WinPctz * slope + intercept
df$residual <- abs(df$MOVz - df$predicted)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = residual))

ggplot(df) +
  geom_point(aes(x = WinPctz, y = residual, color = combP))

ggplot(df) +
  geom_point(aes(x = combP, y = residual, color = residual * combP))

minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

df$normResidual <- 1 - minMax(df$residual)

ggplot(df) + 
  geom_jitter(aes(y = 1, x = normResidual))

df$score <- df$normResidual * df$combP

ggplot(df) +
  geom_point(aes(x = combP, y = normResidual, color = score))

ggplot(df) + 
  geom_jitter(aes(y = 1, x = (score)))

dfOrder <- df[order(df$score),]

dfOrder$index <- 1:nrow(dfOrder)

ggplot(dfOrder) + 
  geom_jitter(aes(y = 1, x = (index)))


