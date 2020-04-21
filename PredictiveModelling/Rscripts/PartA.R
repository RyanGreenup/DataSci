
# Preamble ----------------------------------------------------------------
  ##Clear latent variables
    rm(list = ls())
  
  ##Set working directory and load packages

  setwd.loadpac <- function() {
    
    if(require('pacman')){
      library('pacman')
    }else{
      install.packages('pacman')
      library('pacman')
    }
    
    pacman::p_load(tidyverse, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
    
    #Use the Rstudio API to get the working directory
    
    current_path <- getActiveDocumentContext()$path 
    setwd(dirname(current_path ))
    print( getwd() )
    
  }
  
  setwd.loadpac()

    if(require('pacman')){
      library('pacman')
    }else{
      install.packages('pacman')
      library('pacman')
    }
    
    pacman::p_load(tidyverse, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
    

  ##Load Dataset
  all.df <- read.csv(file = "practical04a.csv", header = TRUE, sep = ",")

  head(all.df)
  #capture.output(print(head(all.df)), file = "head_all_df", )
  #capture.output(print(head(all.df), print.gap =3), file ="alldata.txt")  
  
  

# Create Data Subsets -----------------------------------------------------
  ##Set Seed
  set.seed(23)
    #strictly speaking we don't need to set the seed, this data is a 5th degree
      # polynomial with added noise and this process will always provide a model that
      # is a 5th degree polynomial, moreover the plots are coded relatively such that
      # the crosshairs are always drawn at the point of lowest validation error.

  ##So we want to create 3 random samples of data
  
  N     <- nrow(all.df)
  idval <- sample(N)  
  
  train.ID <- idval[0:(N*0.5)]
  val.ID   <- idval[((N*0.5)+1):(N*0.75)]
  test.ID  <- idval[((N*0.75)+1):N] 
  
  train.df <- all.df[train.ID,]
  val.df   <- all.df[val.ID,]
  test.df  <- all.df[test.ID,]
  
  
      #Order the Data Frame
      train.df <- train.df[order(train.df$X),]
      val.df   <- val.df[order(val.df$X),]
      test.df  <- test.df[order(test.df$X),]
      
      
      
# Visualise the Data ------------------------------------------------------

  ##This isn't always possible but in this case it is
 ggplot(all.df, aes(x = input, y = output)) + 
    geom_point()
  
 ggplot(train.df, aes(x = input, y = output)) + 
    geom_point()
 
 ggplot(val.df, aes(x = input, y = output)) + 
    geom_point()
 
 ggplot(test.df, aes(x = input, y = output)) + 
    geom_point()
 
  all.df$sample            <- FALSE
  all.df[train.ID,]$sample <- "train"
  all.df[val.ID,]$sample   <- "val"
  all.df[test.ID,]$sample  <- "test"
 
 head(all.df) 
  
 
 all.plot <- ggplot(all.df, aes(x = input)) + 
   geom_point(data = all.df, size = 3, aes(y = output, col = sample)) +
   theme_classic() +
   labs(x = "Predictor", y = "Response", col = "Set", title = "Data to Model",
        subtitle = "Seperated into Sets",
        caption = "Sets created using uniform random values") 
 #  scale_color_brewer(palette="Pastel2")
 
 all.plot

# Train a Model -----------------------------------------------------------

  #train models of varying complexity
 # attach(train.df)
 head(train.df)
 
 mod.lm <- lm(output ~ input, data = train.df)
 mod.p2 <- lm(output ~ I(input^2) + input ,data = train.df)
 mod.p3 <- lm(output ~ I(input^3) + I(input^2) + input, data = train.df)
 mod.p4 <- lm(output ~ I(input^4) + I(input^3) + I(input^2) + input, data = train.df)
 mod.p5 <- lm(output ~ I(input^5) + I(input^4) + I(input^3) + I(input^2) + input, data = train.df)
 mod.p6 <- lm(output ~ I(input^6) + I(input^5) + I(input^4) + I(input^3) + I(input^2) + input, data = train.df)
 mod.p7 <- lm(output ~ I(input^7) + I(input^6) + I(input^5) + I(input^4) + I(input^3) + I(input^2) + input, data = train.df)
 

  #Create Predictions and throw everything into a list 
 train.df <- data.frame(train.df,
                        mod_lm = predict(mod.lm),
                        mod_p2 = predict(mod.p2),
                        mod_p3 = predict(mod.p3),
                        mod_p4 = predict(mod.p4), 
                        mod_p5 = predict(mod.p5),
                        mod_p6 = predict(mod.p6),
                        mod_p7 = predict(mod.p7)
                        )[order(train.df$X),]
 
 train.mod.df <- melt(data = train.df, id.vars = c("X", "input", "output"))

  train.mod.list <- list(mod.lm, mod.p2, mod.p3, mod.p4, mod.p5, mod.p6, mod.p7)
  
  training.list <- list(models = train.mod.list, training_data = train.df, model_predictions = train.mod.df )

  #Visualise the Models
    ##Create label and colour vectors
     plot.labels <- c("Linear Model", "Quadratic", "Cubic", "4th Degree",
                  "5th Degree", "6th Degree", "7th Degree",
                  "Test Set", "Training Set", "Validation Set") 
     
     colfunc <- colorRampPalette(c("purple", "Red")) 
     mycol <- c(colfunc(7), "#1B065E", "#30343F", "#60E1E0") 
     plot(rep(1,length(mycol)),col=mycol,pch=19,cex=3)
  
     ##Plot the models over the Data
      all.plot + 
        geom_line(data = train.mod.df, aes(x = input, y = value, col = variable)) +
        scale_color_manual(values = mycol, labels = plot.labels)
      
  
  
 

# Calculate Training Error ------------------------------------------------
  ##Calculate RMSE for Individual Models
      
  train.rmse.lm <- sqrt(sum((train.df$output-predict(mod.lm, newdata = train.df))^2)/nrow(train.df))
    ##Scrictly speaking there is no cause to specify the newdata in the call to predict,
      #the predict function will use the input data used to create the model to predict
      #the values for the model where 'newdata' is not specified, I have specified it here so later, when
      #I create a for loop and function, I don't forget the need to specify the input data.
 
  ##Create an RMSE function to prevent later Mistakes 
  
  rmse <- function(dframe, model){
    
    y     <- dframe[,names(dframe) == "output"]
    y.hat <- predict(object = model, newdata = dframe)
    resid <- y-y.hat
    RSS   <- sum(resid^2)
    T.Error <- RSS/nrow(dframe)
    RMSE <- sqrt(T.Error)
   
       return(RMSE)
  }
  
  ##Use a for loop to calculate all RMSE Values
    ##Create a data frame to store the values
       error.df <- data.frame(matrix(
         ncol = 4, nrow = length(training.list$models)))
       colnames(error.df) <- c("Order",
                               "Training Error",
                               "Validation Error",
                               "Test Error")
       
    ##Execute the loop
  for (i in 1:7) {
    
    rmse.val <- rmse(train.df, training.list$models[[i]])
  error.df[i,] <- c(order = i,
                    training_error = rmse.val,
                    validation_error = NA,
                    test_error = NA)  
  }
       
       head(error.df)
error.df 


# Calculate the Validation Errors -----------------------------------------

  ##Use a for loop to calculate all validation error values
       
    #use the error.df data frame from before
  for(i in 1:7){
    rmse.val <- rmse(val.df, training.list$models[[i]])
    error.df[i,3] <- rmse.val
  }
      error.df
  
# Plot the Errors
      error.df.melt <- melt(error.df[,-4], id.vars = "Order", variable.name = "Set", value.name = "Error")
      error.df.melt
      
      #Mininmum Validation Error
      min.val.error.y <- min(error.df$`Validation Error`)
      min.val.error.x <- error.df[
        error.df[,names(error.df)=="Validation Error"
                 ]==
          min(error.df$`Validation Error`),]$Order
      
 error.plot <- ggplot(error.df.melt, aes(x = Order, y = Error, col = Set)) + 
   geom_line(size = 4, alpha = 0.6) +
   geom_point(size = 6) +
   theme_classic() +
   labs(x = "Predictor", y = "Response", col = "Set", title = "Model Error",
        subtitle = "Training Error vs Validation Error",
        caption = "Observe that a 5th order Polynomial represents the lowest validation Error") +
   geom_vline(xintercept = min.val.error.x, col = "Purple") +
   geom_hline(yintercept = min.val.error.y, col = "Purple") ; error.plot +
   scale_color_brewer(palette="Accent")
 

 # Plot the Model ----------------------------------------------------------
 ##Specify the Correct Model
 correct.mod.degree <- error.df[error.df$`Validation Error`==min(error.df$`Validation Error`),]$Order
 correct.mod        <- train.mod.list[[correct.mod.degree]]
 
 train.df.correct <- train.df[,c(1,2,3)]
 train.df.correct$mod_correct <- predict(correct.mod)
 ##Plot that Model
 all.plot +
   geom_line(data = train.df.correct, aes(y = mod_correct, col = "Purple"),
             size = 7, alpha = 0.6) +
   scale_color_brewer(palette="Set3", labels = c("5th Degree Polynomial Model",
                                                 "Test Set",
                                                 "Training Set",
                                                 "Validation Set")) +
   guides(col = guide_legend(title = NULL))
 
 ##Make a slightly smoother plot
 correct.mod.df <- data.frame(input = seq(from = min(all.df$input),
                                          to = max(all.df$input), length.out = 1000))
 correct.mod.df$mod_correct <- predict(object = correct.mod,
                                       newdata = correct.mod.df)
 
 all.plot +
   geom_line(data = correct.mod.df, size = 2,
             alpha = 1,
             aes(y = mod_correct,
                 col = "purple")) +
   scale_color_brewer(palette="Set3",
                      labels = c("5th Order Polynomial Model",
                                 "Test Set",
                                 "Training Set",
                                 "Validation Set")) +
   guides(col = guide_legend(title = NULL))
 


# Assess the Model Performance --------------------------------------------
rmse(test.df, correct.mod)
sd(test.df$output)  
rmse(test.df, correct.mod)/sd(test.df$output)  
    #The rmse is a measure of the expected error from
      #data point to model value, the rmse is
      #37% the size of the standard deviation, 
      #hence the model can predict on the test set with greater
      #accuracy than merely taking the average output value
   
  

