---
title: "Creating Gifs in R Using Image Magick"
output: 
  html_document: 
    keep_md: yes
    toc: yes
---

This is a test 

# Set Figure Directory





# Creating Gifs in R Using ImageMagick's Convert Function

This isn't so hard, just create a bunch of png images and then tell imagemackick to make them into a gif file:

## Simple Example

This will generate, save and automatically load a gif file


```r
dir.create("./figure/")
```

```
## Warning in dir.create("./figure/"): './figure' already exists
```

```r
dir.create("./figure/Gifs")
```

```
## Warning in dir.create("./figure/Gifs"): './figure/Gifs' already exists
```

```r
setwd("./figure/Gifs")

# example 1: simple animated countdown from 10 to "GO!".
png(file="example%02d.png", width=200, height=200)
for (i in c(10:1, "G0!")){
  plot.new()
  text(.5, .5, i, cex = 6)
}
dev.off()
```

```
## png 
##   2
```

```r
# convert the .png files to one .gif file using ImageMagick. 
# The system() function executes the command as if it was done
# in the terminal. the -delay flag sets the time between showing
# the frames, i.e. the speed of the animation.
system("convert -delay 80 *.png example_1.gif")

# to not leave the directory with the single jpeg files
# I remove them.
file.remove(list.files(pattern=".png"))
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
# Open the gif in firefox
system("firefox example_1.gif & disown")
```

```
## Warning in system("firefox example_1.gif & disown"): error in running command
```


## More Useful Example

### first Create some set up functions etc.


```r
  if(require("pacman")){
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
```

```
## Loading required package: pacman
```

```r
  pacman::p_load(lattice, tidyverse)
  
  b0 <- 10
  b1 <- .5
  b2 <- .3
  g <- expand.grid(x = 1:20, y = 1:20)
  g$z <- b0 + b1*g$x + b2*g$y
  wireframe(z ~ x * y, data = g)
```

![](figure/unnamed-chunk-2-1.png)<!-- -->

```r
  # to rotate the plot
  wireframe(z ~ x * y, data = g,
            screen = list(z = 10, x = -60))
```

![](figure/unnamed-chunk-2-2.png)<!-- -->

### Now Create the Gif


```r
  # example 2
  png(file="example%03d.png", width=300, height=300)
    for (i in seq(0, 350 , 10)){
      print(wireframe(z ~ x * y, data = g,
                screen = list(z = i, x = -60)))
    }
  dev.off()
```

```
## png 
##   2
```

```r
  # convert pngs to one gif using ImageMagick
  system("convert -delay 40 *.png example_2.gif")

  # cleaning up
  file.remove(list.files(pattern=".png"))
```

```
##  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [16] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
## [31] TRUE TRUE TRUE TRUE TRUE TRUE
```

```r
  # Open the gif in firefox
  system("firefox example_2.gif")
```

### using RMarkdown


```r
  pacman::p_load(rmarkdown)
for (i in 1:10) plot(runif(100), ylim = c(0, 1)) # for example
```

![](figure/chunk-label.gif)
