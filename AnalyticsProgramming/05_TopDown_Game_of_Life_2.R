# TODO Have the while loop stop if the matrix hasn't changed for 10 iterations?



# Preamble ----------------------------------------------------------------

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
  

# Game of Life ------------------------------------------------------------

main <- function(n = 20, prob = 0.3, cycles = 100, save = TRUE) {
    start <- cycles
    dir.create("/tmp/golFig")
  
 # Initial mat_zerorix 
  mat_zero <- rbinom(n^2, size = 1, prob) %>% matrix(nrow = n)
  
  while (cycles > 0 && sum(mat_zero)!=0 ) {
    mat_zero <- reproduction_rules(mat_zero)
    # Plot after change so when it finishes it finishes all dead
    
    make_matrix_plot(mat_zero, save = save, i = cycles)  # %>% print()
    
    Sys.sleep(0.1)
    cycles <- cycles-1
    
    # Print Progress
    paste(signif((1-cycles/start), 2) * 100, "%") %>% print()
  }
  
  if (save) {
    make_gif()
  }
}




# Reproduction Rules ======================================================

reproduction_rules <- function(mat) {
  count_mat <- neighbour_count(mat)
  mat[count_mat < 2]  <- 0 # UnderPop
  mat[count_mat > 3] <- 0  # OverPop
  mat[mat == 0 & count_mat == 3] <- 1 # Reproduce
  return(mat)
}



# Plot Matrix =============================================================

  make_matrix_plot <- function (mat, save = TRUE, i = 1) {
    
    if (save) {
      
      save_matrix_plot(mat, i)
      
    } else {
      
      par(pty = "s", mai = c(0.1, 0.1, 0.4, 0.1))
      image(
        rotate(mat),
        col = c("red", "green", "blue"),
        axes = FALSE,
        frame.plot = TRUE,
        main = "Conways Game of Life")
      
    }
  }


  rotate <- function(x) {
      t(apply(x, 2, rev))
  }
  
  save_matrix_plot <- function(mat, i, path = "/tmp/golFig/") {
    # Make Nmaes
    names <- paste0(path, "game_of_life_", 1:99999, ".png")
    # Open PNG Capture Device
    png(file=names[i], width=300, height=300)
    # Make the Plot
    make_matrix_plot(mat, save = FALSE, i = 1)
    # Turn off the PNG Capture Device
    dev.off()
  }


# Neighbour Count =========================================================


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
  
  

# Surrounding element Sum #################################################

  
# Later on I may want to make a cell of distance two half as influential
  # That's why I've included the `l` (length) variable.

surrounding_sum  <- function(mat, r, c, l = 1) {
  # Pad Matrix by 1 (and increment row/column count)
  mat      <- pad_matrix(mat); r <- r+1; c <- c+1
  mat      <- make_walls_dead(mat) # Make 9s 0
  
  # Add the Neighbour Cells (subtract the cell of interest)
  sum_of_neighbours <- sum(mat[(r-l):(r+l), (c-l):(c+l)]) - mat[r,c]
  
  return(sum_of_neighbours)
}
  
 

# Pad Matrix ##############################################################

  # don't use rbind/cbind, they're way too slow.
  
pad_matrix <- function(mat, l = 1) {
  # Make variables to describe starting length and width
  nr <- nrow(mat)
  nc <- ncol(mat)
  
  # What should the new sizes be?
  nr_pad <- nr + l*2 # multiply by 2 for top/bottom
  nc_pad <- nc + l*2 # multiply by 2 for top/bottom
  
  # Make a blank Matrix
  matbig <- matrix(9, nr_pad, nc_pad)
  
  # Fill the blank matrix with the original matrix accordingly
    # so we need just need to offset the matrix by:
        # l+1 so it starts filing from l and stops at nr + l
  matbig[ (l+1):(nr + l), (l+1):(nc + l)] <- mat
  
  return(matbig)
  
}

  # I have elected to encode walls as the number 9,
    # In this simulatoin, for the moment, walls are considered dead
        # so convert them to dead cells.
  make_walls_dead <- function(mat) {
    mat[mat==9] <- 0
    return(mat)
  }
  

# Make a Gif --------------------------------------------------------------

  make_gif <- function(path = "/tmp/golFig/", gif_name = "game_of_life.gif") {
    
  # convert pngs to one gif using ImageMagick
    # system("convert -delay 40 /tmp/golFig/*.png /tmp/golFig/game_of_life.gif")
  conv_str <- paste0("convert -delay 40 ", path, "*.png ", path, gif_name)
  system(conv_str)

  # cleaning up
  # system("rm /tmp/golFig/*png")
    files_to_delete <- list.files(path, pattern = "*.png")
    file.remove(paste0(path, files_to_delete)) 
  
  # Open the gif in firefox
  system("firefox /tmp/golFig/game_of_life.gif")
  }
  

# Call the Main Function --------------------------------------------------

# main(n = 20, cycles = 100, save = FALSE)
 main(n = 50, cycles = 200)
 
  
  
  
  