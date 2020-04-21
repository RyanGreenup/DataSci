# Where I'm at
# * TODO Create a class of all these functions
# * TODO Wrap this into a top down script with arguments


## Preamble ---------------------------------------------------

### Install Pacman ============================================

# StartTime <- Sys.time()

load.pac <- function() {
  
  if(require("pacman")){
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  
pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
               parallel, dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr,
               swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart,
               rstudioapi, plot.matrix)

}

load.pac()
mise()

## Set Working Directory
  # Use the Rstudio API to get the working directory
  current_path <- getActiveDocumentContext()$path
  current_path <- dirname(current_path)
  setwd(current_path)
  rm(current_path)
 
## Game of life

  ### Starting Parameters
  n <- 5 # Square Size # Anything after 30 is too slow
  r <- 0.3 # Probability of Positive
  s <- 1   # Number of possible Outcomes (1 means 1/0)

  starting_point <- rbinom(n^2, size = 1, prob = 0.3) %>% matrix(nrow = n)
  
  #### Rename the Columns of the Matrix

  make_colnames <- function(mat) {
                       factor(paste("col", 1:ncol(mat)),
                              levels = paste("col", 1:ncol(mat)),
                              ordered = TRUE)
  }
  make_rownames <- function(mat) {
                       factor(paste("row", 1:nrow(mat)),
                              levels = paste("row", 1:nrow(mat)),
                              ordered = TRUE)
  }
  
  make_rownames_rev <- function(mat) {
                       factor(paste("row", nrow(mat):1),
                              levels = paste("row", nrow(mat):1),
                              ordered = TRUE)
  }
  
  rownames(starting_point) <- make_rownames(starting_point)
  colnames(starting_point) <- make_colnames(starting_point)
  starting_point

  
  #### Pad the Matrix to describe the walls
  pad_sequene <- seq(9, 9, length.out = ncol(starting_point))

  pad_vector <- function(mat, MARGIN = 1) {
      if (MARGIN == 1) {
        seq(9, 9, length.out = ncol(starting_point))
      } else if (MARGIN == 2) {
        seq(9, 9, length.out = nrow(starting_point))
      } else {
	  print("MARGIN Must be 1 for rows, 2 for columns")
      }
  }

  pad_matrix <- function (mat) {
    # The warnings from this function appear erroneous.
      mat <- rbind("w" = pad_vector(mat, 1), mat, "w" = pad_vector(mat, 1))
      mat <- cbind("w" = pad_vector(mat, 2), mat, "w" = pad_vector(mat, 2))
      return(mat)
  }
  
#   starting_point_raw <- starting_point
#   starting_point <- pad_matrix(starting_point)
  
  make_walls_dead <- function(mat) {
    mat[mat==9] <- 0
    return(mat)
  }

#  starting_point <- make_walls_dead(starting_point)
  starting_point


  factorise_matrix <- function(mat, ordered = FALSE) {
   values_vector <- factor(starting_point,
                           levels = sort(unique(as.vector(starting_point))),
                           ordered = ordered) 
   matrix(values_vector, nrow = nrow(mat))
  }
  
#  starting_point <- factorise_matrix(starting_point)
  

  ### Plot the Matrices
  
  # So the problem with the method provided in the handout is that it cannot plot factors, only numbers

  rotate <- function(x) {
      t(apply(x, 2, rev))
  }
  
  make_matrix_plot <- function (mat) {
    mat <- make_walls_dead(mat)
    par(pty = "s", mai = c(0.1, 0.1, 0.4, 0.1))
    image(rotate(mat), col = c("red", "green", "blue"),
          axes = FALSE,
          frame.plot = TRUE,
          main = "Conways Game of Life")
  }
  
  
##  GGPlot2
# -----------------------------  
  
rotate_ACW <- function(x) {
    t(apply(x, 1, rev))
}
  


  # Create Column Names  
make_colnames <- function(matrL) {
                     factor(paste("col", 1:ncol(matrL)),
                            levels = paste("col", 1:ncol(matrL)),
                            ordered = TRUE)
}

 # Create Row Names
make_rownames <- function(matrL) {
                     factor(paste("row", 1:nrow(matrL)),
                            levels = paste("row", 1:nrow(matrL)),
                            ordered = TRUE)
}

# Create Row Names with reversed Order
make_rownames <- function(matrL) {
                     factor(paste("row", nrow(matrL):1),
                            levels = paste("row", nrow(matrL):1),
                            ordered = TRUE)
}


expand.grid_matrix <- function(mat) {
  mat <- factorise_matrix(mat)
  data <- expand.grid("x" = make_colnames(mat), "y" = make_rownames_rev(mat))
  data$z <- as.vector(rotate(mat))
  
  return(data)
}



(unsquare_test_matrix <- matrix(factor(sample(1:3, 36, replace = TRUE)), nrow = 9))
data <- expand.grid_matrix(unsquare_test_matrix)

  


  make_matrix_ggplot <-  function(mat) {
    data <- expand.grid_matrix(mat)
    plot <- ggplot(data, aes(x = x, y = y, fill =z)) +
      geom_tile() + 
      theme(axis.text.x=element_blank()) +
      theme(axis.text.y=element_blank()) +
      scale_fill_manual(labels = c("Dead", "Alive"),
                         values = c("grey", "green")) +
      labs(title = "Conways Game of Life", x = "", y = "")
      print(plot)
  }
  
  make_matrix_ggplot(starting_point)
#  ---------------------------- 
  
  
  
  
  
  
  
  
  
  
  # Make plot function for GGPlot
  
  make_matrix_plot(starting_point)
  
  # This library doesn't escape loops
  
#   make_matrix_plot <- function(mat) {
#     mat <- factorise_matrix(mat)
#     library(plot.matrix)
#     par(mar=c(5.1, 4.1, 4.1, 4.1)) # adapt margins
#     plot(mat, main = "Conway's Game of Life")
#   }

  make_matrix_plot(starting_point)
  
  # TODO - Make a plot with GGPlot2

  ### Apply the rules 

  cat_matrix <- function(mat) {
    for (r in 1:nrow(mat)) {
      for (c in 1:ncol(mat)) {
          cat(mat[r,c])
      }
     cat("\n")
    }
  }

 # cat_matrix(starting_point)

  cat_alive  <- function(mat) {
    for (r in 1:nrow(mat)) {
      for (c in 1:ncol(mat)) {
          if (mat[r,c]) {
              cat(mat[r,c])
          }
      }
     cat("\n")
    }
  }

# cat_alive(starting_point)


# TODO How do I get the surrounding values on edges?
# So we have a few options here, we could program lots of logic
# to carefully tiptoe around the edges or we could pad the matrix with
# Other values to represent edges.
#     The advantage to padding is that it will make it far easier to change the
#        range of influence from  only 1 cell to 2 cells worth of distance etc.
# Moreover if we wanted walls to behave differently it would be possible to
#    encode the padding with a different value, 
# Another advantage to encoding walls with some unique value 
#  (say a prime that can be dealt with via mods) is that
#   interaction effects caused by thin walls could be simulated


surrounding_sum  <- function(mat, r, c) {
  # Pad Matrix by 1 (and increment row/column count)
  mat      <- pad_matrix(mat); r <- r+1; c <- c+1
  mat      <- make_walls_dead(mat) # Make 9s 0
  
  # Add the Neighbour Cells (subtract the cell of interest)
  sum_of_neighbours <- sum(mat[(r-1):(r+1), (c-1):(c+1)]) - mat[r,c]
  
  return(sum_of_neighbours)
}


surrounding_sum(starting_point, 1, 4)

neighbour_count <- function(mat) {
  count_mat <- mat
  for (r in 1:nrow(mat)) {
    for (c in 1:ncol(mat)) {
      n <- surrounding_sum(mat, r, c)
      count_mat[r,c] <- n
    }
  }
  return(count_mat)
}


neighbour_count <- function(mat) {
  count_mat <- mat
  for (r in 1:nrow(mat)) {
    for (c in 1:ncol(mat)) {
      n <- surrounding_sum(mat, r, c)
      count_mat[r,c] <- n
    }
  }
  return(count_mat)
}



reproduction_rules <- function(mat) {
  count_mat <- neighbour_count(mat)
  mat[count_mat < 2]  <- 0 # UnderPop
  mat[count_mat > 3] <- 0  # OverPop
  mat[mat == 0 & count_mat == 3] <- 1 # Reproduce
  return(mat)
}

# Use While so the loop ends ends when the whole thing is dead
cycles <- 100
mat <- starting_point
while (cycles > 0 && sum(mat)!=0 ) {
  # this will run 10 times
  mat <- reproduction_rules(mat)
  # Plot after change so when it finishes it finishes all dead
  make_matrix_plot(mat) # %>% print()
  Sys.sleep(0.1)
  cycles <- cycles-1
}

# TODO Speed this up using pmap or using something else?


