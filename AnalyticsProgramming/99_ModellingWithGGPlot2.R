set.seed(88)

# Make some data
x <- seq(from = 1, to = 8, length.out = 1000)
y <- 1.8*(x-2)^3 - 10.1* (x-2)^2 - 2.8*x + 9.2 + rnorm(length(x), mean = 0, sd = 15)
mydata <- tibble("input" = x, "output" = y)


# Create Various Models
model_lm <- lm(output ~ input, data = mydata)
for (i in 2:9) {
 eval(
   parse(
     text = paste0("model_p", i, " <-  lm(output ~ poly(input, ", i, "), data = mydata)") 
     )
   )
}

# Create the Predictions

model_preds    <- tibble("input" = seq(from = 0, to = max(mydata$input)*0.9, length.out = 1000))
model_preds$lm <- predict(model_lm, newdata = model_preds[,1])
for (i in 2:7) {
 eval(
   parse(
     text = paste0("model_preds$p", i, " <-  predict(model_p", i, ", newdata = model_preds[,1])") 
     )
   )
}
model_preds

# Make the Predictions tidy

head(model_preds)
model_preds_tidy <- pivot_longer(data = model_preds, cols = names(model_preds[,-1]))
head(model_preds_tidy)

# Overlay the Model

     model_labels <- c("Linear Model", "Quadratic", "Cubic", "4th Order",
                  "5th Order", "6th Order", "7th Order") 

ggplot(data = mydata, aes(x = input, y = output)) +
  geom_point(size = 2, alpha = 0.9, col = "lightblue") +
  theme_classic() +
  labs(x = "Velocity of Vehicle", y = "Breaking Distance", title = "Vehicle Breaking") +
  geom_line(data = model_preds_tidy, aes(x = input, y = value, col = name), lty =2, lwd = 1) +
  scale_color_discrete(name = "Model Type", labels = model_labels)
