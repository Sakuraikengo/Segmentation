#### read the packages ####
library(stringr)

#### set the parameters ####
# set the data day
dayList <- c("0218")

# set the plot number
n <- 24
ind <- rep(1:n, length(dayList))
ind <- str_pad(ind, 2, pad = 0)

#### gathering all image data ####
# make the save folder
saveFolder <- "data/0.0.allDayData/"

# set the path
dataPathList <- list.files("raw_data", pattern = "JPG", recursive = T, full.names = T)

# make the new file path
dayRep <- rep(dayList, each = 24)
newPathList <- paste0(saveFolder, dayRep, "-", ind, ".jpg")

# save the pictures to the saveFolder
for (i in 1:length(dataPathList)) {
  # i <- 1
  dataPath <- dataPathList[[i]]
  newPath <- newPathList[[i]]
  file.copy(from = dataPath, to = newPath)
}
