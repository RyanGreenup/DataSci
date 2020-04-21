mydata <- read.csv("")
N <- nrow(mydata)

NTrain <- round(N * 0.8)
NVal <- round(N * 0.1)
NTest <- N - NTrain - NVal

idx <- sample(N)

mydataTrain <- mydata[ idx[1:NTrain] , ]
mydataVal <- mydata[ idx[(NTrain + 1) : (NTrain + NVal)] , ]
mydataTest <- mydata[idx[(NTrain + NVal + 1) : N ] , ]
