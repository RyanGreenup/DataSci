
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
  
  pacman::p_load(ggmap, plotly, EnvStats, ggplot2, GGally, corrplot, dplyr, tidyr, stringr, reshape2, cowplot, ggpubr, reshape2, ggplot2, rmarkdown, dplyr, plotly, rstudioapi, wesanderson, RColorBrewer)
  
  #Use the Rstudio API to get the working directory
  
  current_path <- getActiveDocumentContext()$path 
  setwd(dirname(current_path ))
  print( getwd() )
  
}

setwd.loadpac()



# Example 1 ---------------------------------------------------------------
    # because X and W are essentially matrices, denote them with capitals

##Plot the Activation Function

g <- function(x){
  g.val <- 1/(1+exp(-x))
  return(g.val)
}

plot(g, -10, 10, ylab=("g(x)"), main = "Activation Function")

##Compute the Function values, for the provided weights for all
  #all Possible X
W <- matrix(c(-30, 20, 20), nrow = 3, ncol = 1)
X <- matrix(c(0,0,0,1,1,0,1,1), nrow = 4, ncol = 2, byrow = TRUE)
colnames(X) <- c("x1", "x2")


  ##Create a function that works with Matrices
  f <- function(X = X, i = 1){
    f.val <- g(W[1,1] + W[2,1]*X[i, 1] + W[3,1]*X[i,2])
    return(f.val)
  }
  
  ##Use a for loop to give all the returned values
  f.val.vec <- vector(length = nrow(X))
 for(i in 1:nrow(X)){
   f.val.vec[i] <- f(X = X, i = i)
   
 } 
  
  Results <- cbind(X, "f(X)"=f.val.vec)
  Results

  ##This could all be done with Matrix Multiplication
  Xint <- cbind("Intercept"=1,X)
  Xint


  multvec <- Xint %*% W
  colnames(multvec) <- "Mult"
  multvec
  
  Result <- cbind(Xint, multvec)
  Result
  Result <- cbind(Result, "g(Mult) = f(X)" = g(Result[,ncol(Result)]))
  Result
  Result[,ncol(Result)] <- Result[,ncol(Result)] %>% round()
  Result
  
  
  Result <- Result[,-4]
  colnames(Result)[4] <- "f(X)"
  
  Result
  
    #So this is training f(X) to become 'x1 And x2'
      #i.e. f(x) will be 1 if and only if x1=1 and x2=1 

# Example 2 ---------------------------------------------------------------

  #OK so we'll work out bit by bit what's happening here and then
    #we'll interpret what the Neural Network is trying to acheive
      #Or Actually Doing
  
  
W <- matrix(c(-10, 20, 20), nrow = 3, ncol = 1)
X <- matrix(c(0,0,0,1,1,0,1,1), nrow = 4, ncol = 2, byrow = TRUE)
colnames(X) <- c("x1", "x2")

Xint <- cbind("intercept" = 1, X)

Result <- Xint

Result <- cbind(Result, "Multiply" = Result %*% W)
colnames(Result)[ncol(Result)] <- "Multiply"
Result
Result <- cbind(Result, "g(Mult) = f(x)" = g(Result[,ncol(Result)]))
Result
Result[,ncol(Result)] <- Result[,ncol(Result)] %>% round()
Result

colnames(Result)[ncol(Result)] <- "f(X)"
Result

Result <- Result[,!colnames(Result) %in% "Multiply"]
Result
  
  #OK so this is training f(x) to be x1 OR x2
    #i.e. this will only be true if x1=1 or x2=1


# Example 3 ---------------------------------------------------------------
  # So now we are trying to find weights that will work to 
  #   train f(x) to return NOT X
  #   
  #   Given the activation function converts negatives to 0, i suspect that 
  #   negative 1 will work

w1 <- 30
w2 <- 55
  
W <- matrix(c(w1, w2), nrow = 2, ncol = 1)
X <- matrix(c(0,1), nrow = 2, ncol = 1)
colnames(X) <- "x"

Xint <- cbind("Intercept" = 1, X)

Result <- Xint
Result
Result <- cbind(Result, "Multiply" = Result %*% W)
colnames(Result)[ncol(Result)] <- "Multiply"
Result
Result <- cbind(Result, "g(Mult) = f(x)" = g(Result[,ncol(Result)]))
Result
Result[,ncol(Result)] <- Result[,ncol(Result)] %>% round()
Result

colnames(Result)[ncol(Result)] <- "f(X)"
Result

Result <- Result[,!colnames(Result) %in% "Multiply"]
Result <- Result[,!colnames(Result) %in% "Intercept"]
Result
  
 # Now that we have all that set up, we just need to try different weight values,
  #let's try this through a function

NotFunNeural <- function(w1, w2){
  
W <- matrix(c(w1, w2), nrow = 2, ncol = 1)
X <- matrix(c(0,1), nrow = 2, ncol = 1)
colnames(X) <- "x"

Xint <- cbind("Intercept" = 1, X)

Result <- Xint
Result
Result <- cbind(Result, "Multiply" = Result %*% W)
colnames(Result)[ncol(Result)] <- "Multiply"
Result
Result <- cbind(Result, "g(Mult) = f(x)" = g(Result[,ncol(Result)]))
Result
Result[,ncol(Result)] <- Result[,ncol(Result)] %>% round()
Result

colnames(Result)[ncol(Result)] <- "f(X)"
Result

Result <- Result[,!colnames(Result) %in% "Multiply"]
Result <- Result[,!colnames(Result) %in% "Intercept"]
Result

return(Result)
}

  #Try -1
      NotFunNeural(-1, -1)

        #Nope
  #Looking at the Matrix Multiplication, the first weight does nothing
      # The first number needs to be negative enough and the second
          #number needs be much larger than the first was negative
      #The first weight needs to be somewhat positive (say+10^2)
        #The second number needs to be significantly more
          #Negative (Say -(10^5))
      NotFunNeural(10^2,-(10^5))
      

# Example 4 ---------------------------------------------------------------

  #In order to assess these various networks we will first create a function
      
Neuraleg4 <- function(w1,w2,w3){
W <- matrix(c(10, -20, -20), nrow = 3, ncol = 1)
X <- matrix(c(0,0,0,1,1,0,1,1), nrow = 4, ncol = 2, byrow = TRUE)
colnames(X) <- c("x1", "x2")

Xint <- cbind("intercept" = 1, X)

Result <- Xint

Result <- cbind(Result, "Multiply" = Result %*% W)
colnames(Result)[ncol(Result)] <- "Multiply"
Result
Result <- cbind(Result, "g(Mult) = f(x)" = g(Result[,ncol(Result)]))
Result
Result[,ncol(Result)] <- Result[,ncol(Result)] %>% round()
Result

colnames(Result)[ncol(Result)] <- "f(X)"
Result

Result <- Result[,!colnames(Result) %in% "Multiply"]
Result <- Result[,!colnames(Result) %in% "intercept"]
Result

return(Result)
}
      
blueTT <- Neuraleg4(10, -20, -20) #TT as in Truth table
yellowTT <- Neuraleg4(30, -20, -20)  #The colours correspond to the lecture Notes
greenTT <- Neuraleg4(-10, 20, 20)  
orangeTT <- Neuraleg4(20, 20, -30)  

TruthTables <- list("Blue" = blueTT, "Yellow" = yellowTT, "Green" = greenTT, "Orange" = orangeTT)

print(TruthTables)
  
  #So the yellowTT corresponds to ( f(x) = [ NOT{X1} AND NOT{x2} ] )