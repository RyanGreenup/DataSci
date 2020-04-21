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
  (current_path <- dirname(current_path))
  setwd(current_path)
  rm(current_path)
 

# Random Walk -------------------------------------------------------------

  main <- function(size=sample.int(50, 1), wait = TRUE, t = 10^4, save = FALSE) {
    if(save) dir.create(path = "/tmp/rwFig")
     board   <- make_board(size)  
    position <- start_position(board) 
    simulate_walk(board, position, wait, t, save)
    if(save) make_gif()
  }
  
  #
  # This function creates the board for walking and the matrix for counting
  #
  make_board <- function(size) {
     board <- matrix(data = FALSE, nrow = size+2, ncol = size+2)
     board[2:(nrow(board)-1), 2:(nrow(board)-1)] <- TRUE
     count_mat <<- matrix(0, nrow = size, ncol = size)     # Create a global counting variable
     board
  }
  
  start_position <- function(board) {
     b <- board
     s <-sample(which(b==TRUE), size = 1)
     r <- ifelse(s != nrow(b), s %% nrow(b), s) # a === 0 (mod a), we want row a not row zero
                                                 # Don't add 1 because the remainders count from 1 not zero
     c <- floor(s/nrow(b)) + 1  # Add 1 because we start counting from 1 not zero
     count_step((r-1), (c-1)) # The Counting matrix doesn't have walls
     return(c(r,c))
  }
  
  count_step <- function(r,c) {
    count_mat[r,c] <<- count_mat[r,c] +1
  }
  
  simulate_walk <- function(board, position, wait, t, save) {
    for(i in 1:t) {
     position <- take_step(board, position)
     if(wait) {
       Sys.sleep(0.1)
       plot_prob(total = i+1, save)
     } 
     # print(i)
    }
    plot_prob(total = t, save = FALSE)
  }
  
    take_step <- function(board, position) {
      r    <- position[1] + 1 # Add 1 for the surrounding wall
      c    <- position[2] + 1  
      dirs <- available_directions(board, position)
      pos  <- sample(dirs, size = 1)[[1]] %>% as.vector
      count_step(pos[1]-1, pos[2]-1) # Counting Matrix has no surrounding wall so minus 1
      return(pos)
    }
    
    available_directions <- function(board, position) {
      r <- position[1]
      c <- position[2]
      v <- vector(length = 4)
     #    1     
     #  4   2     
     #    3     
     # 
      dirs <- list()
      if(board[(r+1),(c)]) dirs <- append(dirs, list(c(r+1, c)))     # North / UP
      if(board[(r),(c+1)]) dirs <- append(dirs, list(c(r, c+1)))     # East  / Right
      if(board[(r),(c-1)]) dirs <- append(dirs, list(c(r, c-1)))     # south / Down
      if(board[(r-1),(c)]) dirs <- append(dirs, list(c(r-1, c)))     # West  / Left
      return(dirs)
    }
  
    
    plot_prob <- function(total, save) {
       pm <-  calculate_prob(total) 
          make_matrix_plot(mat = pm, i = total-1, save = save)
    } 
    
     calculate_prob <- function(total) {
       count_mat/total   # This would be faster, summing inside a loop is expensive
#      count_mat/sum(count_mat)   
     }
     
  make_matrix_plot <- function (mat, i = 1, save) {
    
    if (save) {
      
      save_matrix_plot(mat, i)
      
    } else {
      
      par(pty = "s", mai = c(0.1, 0.1, 0.4, 0.1))
      image(
        rotate(mat),
     #   col = c("red", "green", "blue"),
        axes = FALSE,
        frame.plot = TRUE,
        main = "Random Walk")
      
    }
  }
  
  
  save_matrix_plot <- function(mat, i, path = "/tmp/rwFig/") {
    # Make Nmaes
    names <- paste0(path, "random_walk_", str_pad(string = 1:9999, width = 6, pad = 0), ".png") # Hard coded size limit so I don't give me self big problems <<hclim>>
    # Open PNG Capture Device
    png(file=names[i], width=300, height=300)
    # Make the Plot
    make_matrix_plot(mat, save = FALSE, i = 1)
    # Turn off the PNG Capture Device
    dev.off()
  }

     
  rotate <- function(x) {
      t(apply(x, 2, rev))
  }
     
  
  
  
  make_gif <- function(path = "/tmp/rwFig/", gif_name = "random_walk.gif") {
    
  # convert pngs to one gif using ImageMagick
    # system("convert -delay 40 /tmp/rwFig/*.png /tmp/rwFig/random_walk.gif")
  conv_str <- paste0("convert -delay 20 ", path, "*.png ", path, gif_name)
  system(conv_str)

  # cleaning up
  # system("rm /tmp/rwFig/*png")
    files_to_delete <- list.files(path, pattern = "*.png")
    file.remove(paste0(path, files_to_delete)) 
  
  # Open the gif in firefox
  system("firefox /tmp/rwFig/random_walk.gif")
  }
  
  
  # Be aware I set a hard coded limit at 9999 images to prevent headaches [[hclim]]
  
#  main(size = 100, wait = TRUE, t = 10^4, save = FALSE) 
  main(size = 10, wait = TRUE, t = 10^2, save = TRUE)
     
     
     
     
     