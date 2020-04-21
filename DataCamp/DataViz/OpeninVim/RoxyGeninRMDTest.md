---
title: "R Notebook"
output: html_notebook
editor_options: 
  chunk_output_type: inline
---


```r
#' First check that pacman is installed and install it if it isn't

if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}

#' Now use pacman to load everything you're going to want

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap, mise
               parallel, dplyr, plotly, tidyverse)

#' in order to make sure this script can stand on it's own two
#' feet I'll make sure to clear everything that's ungood

mise()

vim <- function(){
  library(rstudioapi)
  system(paste("kitty -e nvim ", rstudioapi::getSourceEditorContext()$path))
}

naut <- function(){
  library(rstudioapi)
  system(paste("nautilus ", rstudioapi::getSourceEditorContext()$path))
}
	       
```

```
## Error: <text>:13:16: unexpected symbol
## 12: pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap, mise
## 13:                parallel
##                    ^
```

