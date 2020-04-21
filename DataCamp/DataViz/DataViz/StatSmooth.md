---
title: "Data Viz with ggplot2"
author: "Ryan Greenup"
date: "26/12/2019"
output: 
  html_document: 
    keep_md: yes
---

# Specifying `groups`

## Load Packages

So first load any useful packages:

```r
load.pac <- function() {
  
  if(require('pacman')){
    library('pacman')
  }else{
    install.packages('pacman')
    library('pacman')
  }
  
  pacman::p_load(tidyverse, rmarkdown, plotly, colorspace, RColorBrewer, mise, formatR)
  
}

if (!exists("execFlag")) {
 load.pac()
  execFlag <- FALSE
}
```

```
## Loading required package: pacman
```

```r
mise()
```



## Example Plot

So take a fairly ordinary plot from `mtcars`:



```r
library(colorspace)
library(tidyverse)
library(RColorBrewer)

# Plot 1: change the LOESS span
ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point() +
  # Add span below
  geom_smooth(se = FALSE, span = 0.7) + 
  theme_bw()
```

```
## `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](StatSmooth_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

### Clustering 

By looking at this plot it seems fairly obvious that there are three clusters, this could be visualised using Heirarchical Clustering:


```r
mcars <- as_tibble(mtcars)
mcars$cyl <- factor(mcars$cyl, levels = c(4,6,8), ordered = TRUE )
dst <- mcars %>% dist()
hc.iris.complete <- hclust(dst, method = 'complete')

#### 2. Cluster the observations using average and single linkage.
hc.iris.average  <- hclust(dst, method = 'average')
hc.iris.single   <- hclust(dst, method = 'single')

##### Plot the Predictions
#Now that the models have been made, by specifying the number of groups required, the function `cutree` will determine at what height to cut off the dendogram:

hcPreds <- cutree(hc.iris.complete, k = 3)

# Make the Data
groupPred <- factor(hcPreds, levels = c(1,2,3), ordered = FALSE)
mcars$KMpred <- groupPred

# Plot the Data
ggplot(mcars, aes(y = mpg, x = wt, col = KMpred)) +
  geom_point() +
  labs(col = "Predicted\nGroup", caption = "Ellipses represent 90% Normal confidence levels,\n
       predictions made using K-means algorithm with 3 classes") +
  stat_ellipse(level = 0.9) +
  theme_bw()
```

![](StatSmooth_files/figure-html/unnamed-chunk-3-1.png)<!-- -->



## Seperate Models

In order to accomodate the multiple models it would be desirable to make seperate plots for the clusters, this could be acheived by simply asking `ggplot` to:

* make a linear model when the `col` variable is specified

But if we still wanted to fit a loess model across the entire model then we would need to set the group to 1:

* `stat_smooth(aes(group=1))`

So putting all that together (and substituting cylinder instead of clustering):


```r
# Plot 2: Set the second stat_smooth() to use LOESS with a span of 0.7
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "loess", aes(group = 1), se = FALSE, col = "black", span = 0.7) +
  theme_bw()
```

![](StatSmooth_files/figure-html/unnamed-chunk-4-1.png)<!-- -->

### Adding 'All' to the legend and colours

The problem with the plot now however is that the loess model isn't considered it's own group which means:

 * it won't appear in the legend
 * it won't behave when feeding colours to the plot
 
In order to address this:

 * Don't specify the colour in the call of the loess model
 * inside the aesthetics component of the `stat_smooth()` layer specify the name of this new group
    + In this case we've specified only 1 group may exist with `group=1`, hence we just need to give it a name, in this case `Everything!!!`.
 

```r
# Plot 3: Set col to "All", inside the aes layer of stat_smooth()
ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE) +
  stat_smooth(method = "loess",
              # Add col inside aes()
              aes(group = 1, col = "All"),
              # Remove the col argument below
              se = FALSE, span = 0.7)
```

![](StatSmooth_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### Custom Colours

Now the colours look about right we will specify our own colours with by using `RColorBrewer`

First Generate the Vector of colours:


```r
library(RColorBrewer)
myColors <- brewer.pal(4, "Dark2")
```

Now give those colours to the plut by adding a `scale_colour_manual()` layer and specify the name for the colour variable. (The name for the colour variale could also be specified using `labs(col = "Cylinders"))`


```r
# Plot 4: Add scale_color_manual to change the colors
TheMTCarsPlot <- ggplot(mtcars, aes(x = wt, y = mpg, col = factor(cyl))) +
  geom_point() +
  stat_smooth(method = "lm", se = FALSE, span = 0.7) +
  stat_smooth(method = "loess", 
              aes(group = 1, col="All"), 
              se = FALSE, span = 0.7)+
  theme_bw()
```



```r
TheMTCarsPlot +
  scale_color_manual("Cylinders", values = myColors)
```

![](StatSmooth_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

##### Seperate Colour for Loess

If we wanted to specify a particular colour for the loess mode, say for example a blue/grey colour, 
we could acheive that by adding it to the very end of the colour vector:


```r
myColors <- c(brewer.pal(n = 3, name = "Accent"), "blue")


TheMTCarsPlot +
  scale_color_manual("Cylinders", values = myColors)
```

![](StatSmooth_files/figure-html/unnamed-chunk-9-1.png)<!-- -->



































# Modifying [`stat_smooth`](https://campus.datacamp.com/courses/data-visualization-with-ggplot2-2/chapter-1-statistics?ex=4)
