#### read the packages ####
library(stringr)
library(ggplot2)
library(mgcv)

#### read the raw data ####
dayList <- c("0218", "0219", "0220", "0221", "0222", 
             "0223", "0224", "0225", "0226", "0227", 
             "0228", "0301", "0302", "0303", "0304", 
             "0305", "0306", "0307", "0308", "0309", 
             "0310", "0311", "0312", "0313", "0314")
dataPathList <- list.files(path = "data/0.3.val/", full.names = T)

# make the save folder
saveFolder <- "midstream/2.0.visualizeDayChange/"
if (!dir.exists(saveFolder)) {
  dir.create(saveFolder)
}

# prepare the array to input the data
index1 <- c("sum", "mean", "max", "num")
index2 <- paste0("Q", seq(0, 90, 10))
index <- c(index1, index2)
plant <- paste0("ind-", str_pad(1:24, 2, pad = 0))
valArray <- array(data = NA, dim = c(length(plant), length(index), length(dayList)))
dimnames(valArray) <- list(plant, index, dayList)

# input the data
for (i in 1:length(dayList)) {
  # i <- 1
  eachDay <- dayList[i]
  eachDf <- read.csv(dataPathList[[i]], row.names = 1)
  eachMat <- as.matrix(eachDf)
  valArray[, , i] <- eachMat
}

# remove the outlier ind-11, 12, 13, 19
# remove the date of 0218 and 0219
valArray <- valArray[c(1:10, 14:18, 20:24), , ]
valArray <- valArray[, , 3:length(dayList)]

# apply each color to each treatment
colInd <- c("blue", "blue", "blue", "blue", 
                  "gray20", "gray20", "gray20", "red", 
                  "red", "red", 
                  "green", "green", "green", 
                  "gray60", "gray60", "gray60", 
                  "orange", "orange", "orange", "orange")
                  
block <- c("C", "C", "C", "C", 
           "Day1", "Day1", "Day1", "Day3", 
           "Day3", "Day3", 
           "Day4", "Day4", "Day4", 
           "Day2", "Day2", "Day2", 
           "Day5", "Day5", "Day5", "Day5")

dayList <- c("0220", "0221", "0222", "0223", "0224", 
             "0225", "0226", "0227", "0228", "0301", 
             "0302", "0303", "0304", "0305", "0306", 
             "0307", "0308", "0309", "0310", "0311", 
             "0312", "0313", "0314")

Cind <- block == "C"
D1ind <- block == "Day1"
D2ind <- block == "Day2"
D3ind <- block == "Day3"
D4ind <- block == "Day4"
D5ind <- block == "Day5"

# Day1, 2, 3 don't have data on 0313 and 0314
# Day4 and Control don't have data on 0314
valArray[(D1ind|D2ind|D3ind), , 22:length(dayList)] <- NA
valArray[(D4ind|Cind), , 23:length(dayList)] <- NA



# matplot(t(valArray[, 4, 9:length(dayList)]), type = "l", col = colInd, lty = 1)
# matplot(t(valArray[, 1, 3:length(dayList)]), type = "l", col = colInd, lty = 1)
# matplot(t(valArray[, 2, 9:length(dayList)]), type = "l", col = colInd, lty = 1)
# 
# matplot(t(valArray[c(1:11, 14:24), 4, 3:length(dayList)]), type = "l", col = colInd, lty = 1)
# matplot(t(valArray[c(1:11, 14:24), 4, 9:length(dayList)]), type = "l", col = colInd, lty = 1)
# matplot(t(valArray[c(1:11, 14:24), 1, 9:length(dayList)]), type = "l", col = colInd, lty = 1)
# matplot(t(valArray[c(1:11, 14:24), 2, 9:length(dayList)]), type = "l", col = colInd, lty = 1)
# valArray[c(1:11, 14:24), ,20]

# scale each day
valArrayScaled <- apply(valArray, c(2, 3), scale)
# matplot(t(valArrayScaled[, 2, ]), type = "l", col = colInd, lty = 1)
# matplot(t(valArrayScaled[, 6, ]), type = "l", col = colInd, lty = 1)
# matplot(t(valArrayScaled[, 10, ]), type = "l", col = colInd, lty = 1)

# make matplot of each index
blockRep <- rep(block, dim(valArray)[2] * dim(valArray)[3])
plantRep <- rep(dimnames(valArray)[[1]], dim(valArray)[2] * dim(valArray)[3])
indexRep <- rep(rep(index, each = dim(valArray)[1]), dim(valArray)[3])
dayRep <- rep(1:length(dayList), each = (dim(valArray)[1] * dim(valArray)[2]))
dfAll <- data.frame(Value = c(valArray),
                    Plant = plantRep,
                    Block = blockRep,
                    Index = indexRep,
                    Day = dayRep)
matFolder <- paste0(saveFolder, "matplot/")
if (!dir.exists(matFolder)) {
  dir.create(matFolder)
}
for (eachIndex in index) {
  # eachIndex <- index[1]
  dfEach <- dfAll[dfAll$Index == eachIndex, ]
  gMat <- ggplot(data = dfEach, aes(x = Day, y = Value, color = Plant)) + 
    geom_line() + 
    scale_color_manual(values = colInd) + 
    scale_x_continuous(breaks = c(7, 10, 11, 16, 17, 18)) + 
    geom_vline(xintercept = 7) + 
    geom_vline(xintercept = c(10, 11, 16, 17, 18), lty = 2)
  png(paste0(matFolder, eachIndex, ".png"), 
      width = 2880, height = 1440, res = 214)
  print(gMat)
  dev.off()
}

dfAllScaled <- data.frame(Value = c(valArrayScaled),
                          Plant = plantRep,
                          Block = blockRep,
                          Index = indexRep,
                          Day = dayRep)
for (eachIndex in index) {
  # eachIndex <- index[1]
  dfEach <- dfAllScaled[dfAllScaled$Index == eachIndex, ]
  gMat <- ggplot(data = dfEach, aes(x = Day, y = Value, color = Plant)) + 
    geom_line() + 
    scale_color_manual(values = colInd) + 
    scale_x_continuous(breaks = c(7, 10, 11, 16, 17, 18)) + 
    geom_vline(xintercept = 7) + 
    geom_vline(xintercept = c(10, 11, 16, 17, 18), lty = 2)
  png(paste0(matFolder, eachIndex, "Scaled.png"), 
      width = 2880, height = 1440, res = 214)
  print(gMat)
  dev.off()
}

# dfPA <- dfAll[dfAllScaled$Index == "num", ]
# dfFittedPA <- na.omit(dfPA)
# for (eachPlant in unique(dfPA$Block)) {
#   # eachPlant <- unique(dfPA$Block)[1]
#   dfPAEach <- dfPA[dfPA$Block == eachPlant, ]
#   # summary(dfPAEach)
#   # model <- gam(Value ~ s(Day), data = dfPAEach)
#   model <- lm(Value ~ Plant + Day + I(Day^2) + I(Day^3) + I(Day^4), data = dfPAEach)
#   
#   dfFittedPA$Value[dfFittedPA$Block == eachPlant] <- model$fitted.values
# }
# gMat <- ggplot(data = dfFittedPA, aes(x = Day, y = Value, color = Plant)) + 
#   geom_line() + 
#   scale_color_manual(values = colInd) + 
#   scale_x_continuous(breaks = c(7, 10, 11, 16, 17, 18)) + 
#   geom_vline(xintercept = 7) + 
#   geom_vline(xintercept = c(10, 11, 16, 17, 18), lty = 2)



# extract the plant area info
areaMat <- valArray[, 4, ]

# clustering by the treatments
areaMatMean <- apply(X = areaMat, 2, function(eachDayVal) {
  # eachDayVal <- areaMat[, 1]
  blockMean <- tapply(X = eachDayVal, INDEX = block, mean)
  return(blockMean)
})
areaMatSd <- apply(X = areaMat, 2, function(eachDayVal) {
  # eachDayVal <- areaMat[, 1]
  blockSd <- tapply(X = eachDayVal, INDEX = block, sd)
  return(blockSd)
})
dfAreaBlock <- data.frame(Mean = c(areaMatMean), 
                          Sd = c(areaMatSd), 
                          Block = rep(rownames(areaMatMean), ncol(areaMatMean)), 
                          Day = rep(1:ncol(areaMatMean), each = nrow(areaMatMean)))
gMatSd <- ggplot(data = dfAreaBlock, aes(x = Day, y = Mean, color = Block)) + 
  geom_line() + 
  scale_color_manual(values = c("blue", "gray20", "gray60", "red", "green", "orange")) + 
  scale_x_continuous(breaks = c(7, 10, 11, 16, 17, 18)) + 
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd, width = 0.3)) + 
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = c(10, 11, 16, 17, 18), lty = 2)
gMat <- ggplot(data = dfAreaBlock, aes(x = Day, y = Mean, color = Block)) + 
  scale_color_manual(values = c("blue", "gray20", "gray60", "red", "green", "orange")) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7, 10, 11, 16, 17, 18)) + 
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = c(10, 11, 16, 17, 18), lty = 2)
png(paste0(saveFolder, "matplotGroupSd.png"), 
    width = 2160, height = 1440, res = 214)
print(gMatSd)
dev.off()
png(paste0(saveFolder, "matplotGroup.png"), 
    width = 2160, height = 1440, res = 214)
print(gMat)
dev.off()


# clustering by the skeepon and no-skeepon
sp1 <- D1ind|D2ind|D3ind|D4ind|D5ind
sp2 <- D3ind|D4ind|D5ind
sp3 <- D4ind
spList <- list(!sp1, sp1, sp2, sp3)
spMatList <- lapply(spList, function(spEach) {
  # spEach <- spList[[1]]
  spEachMean <- apply(areaMat[spEach, ], 2, mean)
  spEachSd <- apply(areaMat[spEach, ], 2, sd)
  return(cbind(spEachMean, spEachSd))
})
spMat <- do.call(rbind, spMatList)

dfSp <- data.frame(Mean = spMat[, 1], 
                   Sd = spMat[, 2], 
                   Treatment = rep(c("Control", "Sp1", "Sp2", "Sp3"), each = length(dayList)), 
                   Day = rep(1:length(dayList), length(spList)))

# compare the sp1 and control
dfSp1 <- dfSp[grepl("Control|Sp1", x = dfSp$Treatment), ]
dfSp1 <- dfSp1[dfSp1$Day < 11, ]
dfSp1$Treatment[dfSp1$Treatment == "Sp1"] <- "Skeepon"
# dfSp1$Sp <- factor(dfSp1$Sp, levels = c("Skeepon", "Control"))
gSp1 <- ggplot(data = dfSp1, aes(x = Day, y = Mean, color = Treatment)) + 
  scale_color_manual(values = c("blue", "red")) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7:10)) + 
  geom_vline(xintercept = 7)
png(paste0(saveFolder, "matplotBeforeRewatering.png"), 
    width = 2160, height = 1440, res = 214)
print(gSp1)
dev.off()



# calculating the proportion of day 0228 / day 0226
propArea <- areaMat[, 9] / areaMat[, 7]
spInd <- rep("Skeepon", length(block))
spInd[Cind] <- "Control"
dfPropArea <- data.frame(Proportion = propArea, 
                         Treatment = spInd)
gProp <- ggplot(data = dfPropArea, aes(x = Treatment, y = Proportion, color = Treatment)) + 
  scale_color_manual(values = c("blue", "red")) + 
  geom_boxplot() + 
  geom_hline(yintercept = 1, lty = 2)

png(paste0(saveFolder, "propDay9Day7.png"), 
    width = 2160, height = 1440, res = 214)
print(gProp)
dev.off()

# compare the sp2 and control
dfSp2 <- dfSp[grepl("Control|Sp2", x = dfSp$Treatment), ]
dfSp2 <- dfSp2[dfSp2$Day < 17, ]
dfSp2$Treatment[dfSp2$Treatment == "Sp2"] <- "Skeepon"
gSp2 <- ggplot(data = dfSp2, aes(x = Day, y = Mean, color = Treatment)) + 
  scale_color_manual(values = c("blue", "red")) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7:16)) + 
  geom_vline(xintercept = 7)
gSp2Sd <- ggplot(data = dfSp2, aes(x = Day, y = Mean, color = Treatment)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7:16)) + 
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd, width = 0.3)) + 
  geom_vline(xintercept = 7)
png(paste0(saveFolder, "matplotSkeepon.png"), 
    width = 2160, height = 1440, res = 214)
print(gSp2)
dev.off()
png(paste0(saveFolder, "matplotSkeeponSd.png"), 
    width = 2160, height = 1440, res = 214)
print(gSp2)
dev.off()


# compare the sp3 and control
dfSp3 <- dfSp[grepl("Control|Sp3", x = dfSp$Treatment), ]
dfSp3 <- dfSp3[dfSp3$Day < 23, ]
dfSp3$Treatment[dfSp3$Treatment == "Sp3"] <- "Skeepon"
gSp3 <- ggplot(data = dfSp3, aes(x = Day, y = Mean, color = Treatment)) + 
  scale_color_manual(values = c("blue", "red")) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7, 17)) + 
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = 17, lty = 2)
gSp3Sd <- ggplot(data = dfSp3, aes(x = Day, y = Mean, color = Treatment)) + 
  geom_line() + 
  scale_x_continuous(breaks = c(7, 17)) + 
  geom_errorbar(aes(ymin = Mean - Sd, ymax = Mean + Sd, width = 0.3)) + 
  geom_vline(xintercept = 7) + 
  geom_vline(xintercept = 17, lty = 2)

png(paste0(saveFolder, "controlAndSkeepon.png"), 
    width = 2160, height = 1440, res = 214)
print(gSp3)
dev.off()
png(paste0(saveFolder, "controlAndSkeeponSd.png"), 
    width = 2160, height = 1440, res = 214)
print(gSp3Sd)
dev.off()


# # t-test
# # before the irrigation
# 
l <- 4
# before the irrigation
for (i in 1:10) {
  res <- t.test(x = valArray[Cind, l, i],
                y = valArray[(D1ind|D2ind|D3ind|D4ind|D5ind), l, i])
  print(res$p.value)
}

res1 <- t.test(x = valArray[Cind, l, 11],
               y = valArray[c(D2ind|D3ind|D4ind|D5ind), l, 11])
print(res1$p.value)

for (i in 12:16) {
  res2 <- t.test(x = valArray[Cind, l, i],
                 y = valArray[c(D3ind|D4ind|D5ind), l, i])
  print(res2$p.value)
}

res3 <- t.test(x = valArray[Cind, l, 17],
               y = valArray[c(D4ind|D5ind), l, 17])
print(res3$p.value)

for (i in 1:22) {
  res4 <- t.test(x = valArray[Cind, l, i],
                 y = valArray[D4ind, l, i])
  print(res4$p.value)
}
for (i in 8:10) {
  res1 <- t.test(x = valArray[Cind, l, i] / valArray[Cind, l, 7],
                 y = valArray[(D1ind|D2ind|D3ind|D4ind|D5ind), l, i] / valArray[(D1ind|D2ind|D3ind|D4ind|D5ind), l, 7])
  print(res1$p.value)
}

for (i in 11:16) {
  res2 <- t.test(x = valArray[Cind, l, i] / valArray[Cind, l, 7],
                 y = valArray[(D3ind|D4ind|D5ind), l, i] / valArray[(D3ind|D4ind|D5ind), l, 7])
  print(dayList[i])
  print(valArray[Cind, l, i] / valArray[Cind, l, 7])
  print(valArray[(D3ind|D4ind|D5ind), l, i] / valArray[(D3ind|D4ind|D5ind), l, 7])
  print(res2$p.value)
}

# compareing whithin the skeepon treatment
# plant area when start treatment
startPA0 <- c(areaMat[D1ind, 7], 
              areaMat[D2ind, 7], areaMat[D3ind, 7], 
              areaMat[D4ind, 7], areaMat[D5ind, 7])
startPA <- startPA0[sort(names(startPA0))]

# plant area when re-watering
rePA0 <- c(areaMat[D1ind, 10], 
           areaMat[D2ind, 11], areaMat[D3ind, 16], 
           areaMat[D4ind, 17], areaMat[D5ind, 18])
rePA <- rePA0[sort(names(rePA0))]

# plant area when 1 day before re-watering
rePA1 <- c(areaMat[D1ind, 9], 
           areaMat[D2ind, 10], areaMat[D3ind, 15], 
           areaMat[D4ind, 16], areaMat[D5ind, 17])
rePA1 <- rePA1[sort(names(rePA1))]

# the proportion strat re-watering / start treatment
propPA1 <- rePA / startPA

# the proportion 2-days after start treatment / start treatment
day2PA0 <- c(areaMat[D1ind, 9], 
             areaMat[D2ind, 9], areaMat[D3ind, 9], 
             areaMat[D4ind, 9], areaMat[D5ind, 9])
propPA2 <- day2PA0 / startPA

# the proportion  strat re-watering / 1 day before re-watering
propPA3 <- rePA / rePA1

# plant area after re-watered 5 days (imaginary biomass)
lastPA0 <- c(areaMat[D1ind, 15], 
             areaMat[D2ind, 16], areaMat[D3ind, 21], 
             areaMat[D4ind, 22], areaMat[D5ind, 23])
lastPA <- lastPA0[sort(names(lastPA0))]

# recovery proportion of plant area
RPPA <- lastPA / rePA

dfSkeepon <- data.frame(SPA = startPA, 
                        RPA = rePA, 
                        PPA = propPA1, 
                        PPA2 = propPA2, 
                        PPA3 = propPA3, 
                        R5DPA = lastPA, 
                        RPPA = RPPA)
png(paste0(saveFolder, "corrplotSkeepon.png"), 
    width = 2160, height = 1440, res = 214)
psych::pairs.panels(dfSkeepon, 
                    ellipses = F, smooth = F, method = "pearson", 
                    alpha = 0.05, stars = T, pch = c(rep(16, 15), 4))
dev.off()

dfSkeeponSel <- data.frame(Watering = rePA, 
                           WiltingRatio = propPA3, 
                           Recovery = lastPA, 
                           RecoveryRatio = RPPA)

png(paste0(saveFolder, "corrplotSkeeponSel.png"), 
    width = 2160, height = 1440, res = 214)
psych::pairs.panels(dfSkeeponSel, 
                    ellipses = F, smooth = F, method = "pearson", 
                    alpha = 0.05, stars = T, pch = 16)
dev.off()

png(paste0(saveFolder, "corrplotSkeeponSelColor.png"), 
    width = 2160, height = 1440, res = 214)
pairs(dfSkeeponSel, pch = 16, col = colInd[5:length(colInd)])
dev.off()

png(paste0(saveFolder, "corrplotSkeeponRatio.png"), 
    width = 2160, height = 1440, res = 214)
psych::pairs.panels(dfSkeeponSel[, c(2, 4)], 
                    ellipses = F, smooth = F, method = "pearson", 
                    alpha = 0.05, stars = T, pch = 16)
dev.off()
# plot(rePA, propPA1, pch = 16, cex = lastPA / 1000000)

# only the Day3-5
# plant area when start treatment
startPA0 <- c(areaMat[D3ind, 7], 
              areaMat[D4ind, 7], areaMat[D5ind, 7])
startPA <- startPA0[sort(names(startPA0))]

# plant area when re-watering
rePA0 <- c(areaMat[D3ind, 16], 
           areaMat[D4ind, 17], areaMat[D5ind, 18])
rePA <- rePA0[sort(names(rePA0))]

# plant area when 1 day before re-watering
rePA1 <- c(areaMat[D3ind, 15], 
           areaMat[D4ind, 16], areaMat[D5ind, 17])
rePA1 <- rePA1[sort(names(rePA1))]

# the proportion strat re-watering / start treatment
propPA1 <- rePA / startPA

# the proportion 2-days after start treatment / start treatment
day2PA0 <- c(areaMat[D3ind, 9], 
             areaMat[D4ind, 9], areaMat[D5ind, 9])
propPA2 <- day2PA0 / startPA

# the proportion  strat re-watering / 1 day before re-watering
propPA3 <- rePA / rePA1

# plant area after re-watered 5 days (imaginary biomass)
lastPA0 <- c(areaMat[D3ind, 21], 
             areaMat[D4ind, 22], areaMat[D5ind, 23])
lastPA <- lastPA0[sort(names(lastPA0))]

# recovery proportion of plant area
RPPA <- lastPA / rePA

dfSkeepon <- data.frame(SPA = startPA, 
                        RPA = rePA, 
                        PPA = propPA1, 
                        PPA2 = propPA2, 
                        PPA3 = propPA3, 
                        R5DPA = lastPA, 
                        RPPA = RPPA)
png(paste0(saveFolder, "corrplotSkeeponD345.png"), 
    width = 2160, height = 1440, res = 214)
psych::pairs.panels(dfSkeepon, 
                    ellipses = F, smooth = F, method = "pearson", 
                    alpha = 0.05, stars = T, pch = 16)
dev.off()

dfSkeeponSel <- data.frame(Watering = rePA, 
                           WiltingRatio = propPA3, 
                           Recovery = lastPA, 
                           RecoveryRatio = RPPA)

png(paste0(saveFolder, "corrplotSkeeponD345Sel.png"), 
    width = 2160, height = 1440, res = 214)
psych::pairs.panels(dfSkeeponSel, 
                    ellipses = F, smooth = F, method = "pearson", 
                    alpha = 0.05, stars = T, pch = 16)
dev.off()



# # plant area when start treatment
# startPA0 <- c(areaMat[D3ind, 7], 
#               areaMat[D4ind, 7], areaMat[D5ind, 7])
# startPA <- startPA0[sort(names(startPA0))]
# 
# # plant area when re-watering
# rePA0 <- c(areaMat[D3ind, 16], 
#            areaMat[D4ind, 17], areaMat[D5ind, 18])
# rePA <- rePA0[sort(names(rePA0))]
# 
# # the proportion strat re-watering / start treatment
# propPA1 <- rePA / startPA
# 
# # the proportion 2-days after start treatment / start treatment
# day2PA0 <- c(areaMat[D3ind, 8], 
#              areaMat[D4ind, 8], areaMat[D5ind, 8])
# propPA2 <- day2PA0 / startPA
# 
# # plant area after re-watered 5 days (imaginary biomass)
# lastPA0 <- c(areaMat[D3ind, 21], 
#              areaMat[D4ind, 22], areaMat[D5ind, 23])
# lastPA <- lastPA0[sort(names(lastPA0))]
# 
# cor(cbind(startPA, rePA, propPA1, propPA2, lastPA))
# pairs(cbind(startPA, rePA, propPA1, propPA2, lastPA))
# 
# plot(rePA, propPA1, pch = 16, cex = lastPA / 2000000)
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# deadInd <- c(4, 4, 4, 4, 
#              16, 16, 16, 16, 
#              4, 16, 
#              16, 16, 16, 
#              16, 16, 16, 
#              16, 4, 4, 4)
# 
# block <- c("C", "C", "C", "C", 
#            "Day1", "Day1", "Day1", "Day3", 
#            "Day3", "Day3", 
#            "Day4", "Day4", "Day4", 
#            "Day2", "Day2", "Day2", 
#            "Day5", "Day5", "Day5", "Day5")
# plot(prop, PA, col = colInd, pch = 16, cex = 2)
# plot(prop, PA, col = colInd, pch = deadInd, cex = 2)
# 
# 
# 
# blockRep <- rep(block, dim(valArray)[2] * dim(valArray)[3])
# plantRep <- rep(plant, dim(valArray)[2] * dim(valArray)[3])
# indexRep <- rep(rep(index, each = dim(valArray)[1]), dim(valArray)[3])
# dayRep <- rep(1:length(dayList), each = (dim(valArray)[1] * dim(valArray)[2]))
# df <- data.frame(Value = c(valArray), 
#                  Plant = plantRep, 
#                  Block = blockRep, 
#                  Index = indexRep, 
#                  Day = dayRep)
# 
# g <- ggplot(data = df[df$Index == "num", ], aes(x = Day, y = Value, color = Plant)) + 
#   geom_line()
# 
# #### read the pod and fresh weight ####
# weightMat0 <- read.csv("raw_data/weight.csv", 
#                        header = T, row.names = 1, check.names = F)
# weightMat <- weightMat0[1:24, 3:ncol(weightMat0)]
# colnames(weightMat)[1] <- "2023/2/26"
# rownames(weightMat) <- plant
# weightMat <- weightMat[rownames(areaMat), ]
# freshWeight <- weightMat[, ncol(weightMat)]
# 
# t.test(x = freshWeight[Cind], 
#        y = freshWeight[D4ind])
# # matplot(t(weightMat), type = "l", col = colInd, lty = 1)
# 
# for (i in 2:4) {
#   res <- t.test(x = weightMat[Cind, i], 
#                 y = weightMat[(D1ind|D2ind|D3ind|D4ind|D5ind), i])
#   print(res$p.value)
# }
# 
# res1 <- t.test(x = weightMat[Cind, 5], 
#                y = weightMat[c(D2ind|D3ind|D4ind|D5ind), 5])
# print(res1$p.value)
# 
# for (i in 6:10) {
#   res2 <- t.test(x = weightMat[Cind, i],
#                  y = weightMat[c(D3ind|D4ind|D5ind), i])
#   print(res2$p.value)
# }
# 
# res3 <- t.test(x = weightMat[Cind, 11],
#                y = weightMat[c(D4ind|D5ind), 11])
# print(res3$p.value)
# 
# res3 <- t.test(x = weightMat[Cind, 14],
#                y = weightMat[D4ind, 14])
# print(res3$p.value)
# 
# 
# for (i in 17:22) {
#   res4 <- t.test(x = weightMat[Cind, i],
#                  y = weightMat[D4ind, i])
#   print(res4$p.value)
# }
# weightMat[, 14]
# dim(weightMat)
# 
# plantAreaLastDay1 <- areaMat[D1ind, 15]
# plantAreaLastDay2 <- areaMat[D2ind, 16]
# plantAreaLastDay3 <- areaMat[D3ind, 21]
# plantAreaLastDay4 <- areaMat[D4ind, 22]
# plantAreaLastDay5 <- areaMat[D5ind, 23]
# plantAreaLastCont <- areaMat[Cind, 22]
# plantAreaLast0 <- c(plantAreaLastCont, plantAreaLastDay1, plantAreaLastDay2, 
#                     plantAreaLastDay3, plantAreaLastDay4, plantAreaLastDay5)
# plantAreaLast <- plantAreaLast0[sort(names(plantAreaLast0))]
# 
# dfSel <- data.frame(block, 
#                     PAL = plantAreaLast, 
#                     FW = freshWeight, 
#                     Prop = prop, 
#                     PAS = areaMat[, 7], 
#                     Response = areaMat[, 8] / areaMat[, 7], 
#                     PA)[Cind|D3ind|D4ind|D5ind, ]
# 
# pairs(dfSel[, 2:ncol(dfSel)])
# cor(dfSel[, 2:ncol(dfSel)])
# 
# pairs(cbind(PAL, FW, Prop, PA, PASareaMat[, 8:9] / areaMat[, 7])[D3ind|D4ind|D5ind, ])
# cor(cbind(PAL, FW, Prop, PA, PASareaMat[, 8:9] / areaMat[, 7])[D3ind|D4ind|D5ind, ])
# cor(cbind(PAL, FW, Prop, PA, areaMat / areaMat[, 7])[D3ind|D4ind|D5ind, ])
# cor(cbind(PAL, FW, Prop, PA)[D3ind|D4ind|D5ind, ])
# 
# cor.test(dfSel[, 3], dfSel[, 6])
# summary(lm(FW ~ Response, data = dfSel))
# summary(lm(freshWeight ~ a))
