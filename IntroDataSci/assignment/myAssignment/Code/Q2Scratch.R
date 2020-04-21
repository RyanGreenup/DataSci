# Questoin 2 Scratch ------------------------------------------------------
set.seed(31415)


cpu <- as_tibble(cpu)
predictors <- select(cpu, c( "CycleTime", "MinimumMainMemory", "MaximumMainMemory", "CacheSize", "MinimumNumberOfChannels", "MaximumNumberOfChannels"))
names(cpu)


# Linear

BestPred <- data.frame("Degree" = 1:6, "BestAttribute" = rep(NA, 6))

RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
for (i in 1:length(predictors)) {
  
  RSSVals[i,2] <- (glm(cpu$Performance ~ poly(as_vector(predictors[i]), 1))$residuals)^2 %>% sum()
  
}
RSSVals



BestPred[1,2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])


# Lin to 6th deg
mdg <- 9


BestPred <- data.frame("Degree" = 1:(mdg+1), "BestAttribute" = rep(NA, (mdg+1)))

for (j in (1:mdg)) {
  
RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
  for (i in 1:length(predictors)) {
    
    RSSVals[i,2] <- (glm(cpu$Performance ~ poly(as_vector(predictors[i]), j))$residuals)^2 %>% sum()
    
  }

BestPred[j,2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])
}

BestPred


# Hyperbolic

RSSVals <- data.frame(names(predictors), "RSS" = 1:length(predictors))
for (i in 1:length(predictors)) {
  
  RSSVals[i,2] <- (glm(cpu$Performance ~ (1/as_vector(predictors[i])))$residuals)^2 %>% sum()
  
}
RSSVals

RSSVals[which.min(RSSVals[,2]),1][1] %>% as.character()

BestPred$Degree[(mdg+1)] <- -1
BestPred[(mdg+1),2] <-  as.character(RSSVals[which.min(RSSVals[,2]),1][1])

BestPred

# Now I have to perform cross validation on every model
## Create a list of models

### Create the model names
modnames <- rep(NA, (mdg+1))
for (i in (1:(mdg+1))) {
 modnames[i] <- paste("Degree ", i) 
  
}
modnames[mdg+1] <- "Hyperbolic"

### Create the list of Models
mods <- vector(mode = "list", length = length(modnames))
names(mods) <- modnames

### Assign the models

for (i in 1:mdg) {
 mods[[i]] <-  glm(formula = paste("Performance ~ ", BestPred[i,2]), data = cpu)
}

mods[[(mdg+1)]] <- glm(Performance ~ I(1/CycleTime), data = cpu)
mods


## Use the list to perform Cross Validation

### Make a data frame to track the expected error
BestPred$CVError <- rep(NA, nrow(BestPred));  BestPred

### Compute the CV Error

#3 represents variable

for (r in 1:nrow(BestPred)) {
BestPred[r,3] <- sqrt(cv.glm(data = cpu, glmfit = mods[[r]], K = 10)$delta[1])
}

BestPred$names <- names(mods)
BestPred <- arrange(.data = BestPred, sort.by = Degree)
names(BestPred)[2] <- "Best Attribute"


ggplot(BestPred, aes(x = factor(Degree, labels = BestPred$names, ordered = TRUE), y = CVError, group = 1)) + 
  geom_point(aes(shape = `Best Attribute`, col = `Best Attribute`),  size = 5) +
  geom_line() +
  theme_classic()  +
  labs(x = "Model Type",
       y = TeX("Estimated Testing Error $\\pm $ Performance"),
       title = "Cross Validation of Different Models") +
  geom_vline(xintercept = which.min(BestPred$CVError))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


modtest <- glm(Performance ~ MaximumMainMemory, data = cpu)

cv.glm(data = cpu, glmfit = modtest, K = 10)$delta[1]
cv.glm(data = cpu, glmfit = glm(Performance ~ poly(MaximumMainMemory, 9), data = cpu), K = 10)$delta[1]




