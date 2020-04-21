#So you must use assignments inside any model you create otherwise, you cannot
#predict over it later.

#Fortunately though, this script below automaticcally finds the name and puts
#it into the predict function, this means that assignment can be arbitrary or
#ambiguous, because This script finds the name of the variable anyway
set.seed(89)
example.df <- data.frame(input = runif(n = 50), output = runif(50))

input.mod  <- example.df$input
output.mod <- example.df$output

example.mod <- lm(output.mod~input.mod) 

input.mod.name       <- as.character(variable.names(example.mod)[2])

input.newdata        <- data.frame(1:5) 
names(input.newdata) <- input.mod.name

predict(example.mod, input.newdata)


# Wrapping this into a function, observe: ---------------------------------
  #Initial Data Frame
  example.df <- data.frame(input = runif(n = 50), output = runif(50))
  
  #Create Model
    #First create assignments, they can be ambiguous and non-descript
    input.mod  <- example.df$input
    output.mod <- example.df$output
  
  example.mod <- (output.mod ~ input.mod)
  
  #Creating a Prediction from the model given new data
  
  ryanspredict <- function(object, newdata){
    
    input.mod.name       <- as.character(variable.names(object)[2])
    
    input.newdata        <- newdata
    names(input.newdata) <- input.mod.name
    
    predict(object, input.newdata)
  }
  
  ryanspredict(object = example.mod, newdata = data.frame(1:5))
