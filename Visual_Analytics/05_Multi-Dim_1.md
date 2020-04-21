---
title: "(05) Multi Dimensional Data 1"
tags: [Notebooks/Thinking About Data]
output: 
  html_document: 
    keep_md: yes
    theme: cosmo
    code_folding: hide
    toc: yes
  html_notebook: 
    toc: yes
---




# (05) Multi Dimensional Data 1


## Preamble


```r
# Preamble

## Install Pacman
load.pac <- function() {
  
  if(require("pacman")){
    library(pacman)
  }else{
    install.packages("pacman")
    library(pacman)
  }
  
  pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap,
                 parallel, dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr,
                 swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice,
                 coin)
  
  
   mise()
}
load.pac()
```

```
## Loading required package: pacman
```




```r
knitr::opts_chunk$set(
  fig.path = "./figure/"
)
```



# (05) Multi Dimensional Data


## Question 1

A simple scatter plot may be produced thusly:


```r
reactor <- read.csv(file = "~/Notes/DataSci/Visual_Analytics/05_Data/Tutorial5Ex1.csv", header = TRUE, sep = ",")
p <- ggplot(reactor, aes(x = purity, y = yield)) +
  geom_point(col = "#D2691e") + 
  labs(x = "Purity", y = "Yield (%)", title = "Yield from a Batch Reactor", col = "%") + 
  theme_classic() 

ggplotly(p)
```

<!--html_preserve--><div id="htmlwidget-d05c42e915afe80e7290" style="width:672px;height:480px;" class="plotly html-widget"></div>
<script type="application/json" data-for="htmlwidget-d05c42e915afe80e7290">{"x":{"data":[{"x":[60.4,61.2,50.7,58.6,69.5,54.3,66.5,59.8,56,64.4,60.7,62.2,61.3,77.1,58.1,54.7,61.3,61.2,61.5,53.8,58.8,57.3,63.1,60.9,56.2,59.4,59,63.1,56,59.5,67.3,50.5,62.3,61.9,70.2,54.7,55,65.4,61.7,64.7,62.8,67.5,63.7,54.9,56.3,61.7,74.4,58.5,71.5,58.5,56.6,61.2,58,56.6,73.5,65.2,61.1,63,53.5,59.7,55.5,58.9,63.8,64.1,55.5,58.4,57.1,50.3,61.5,62.5,58.6,56.4,61.1,65.8,57,58.1,60.6,76.9,62,58.4,62.8,64.3,61.3,63.1,60.6,62.5,75.1,56.7,58.7,59.4,81.7,54.5,58.9,63,68.7,65.3,55.4,60,56.1,65,54.3,74.8,65.4,71.1,61.3,62.8,55.5,81.4,64.6,71.3,61.8,56.1,61.3,57.3,60.4,60.6,58.8,59.5,66.4,57.1,59.8,57.5,65.6,68.5,60.4,58.4,52.3,59.9,50.2,61.5,64.6,57.1,61.8,58.3,58.8,56.9,70.1,77.6,50.3,56.8,60.8,68.4,64.8,57.5,62.3,64.9,59.9,61.2,60.8,72,51,60.3,61.3,57.2,57.3,59.4,81.8,64.9,55.1,60.2,69.5,54.6,73.7,60.4,58.8,61.1,59.3,64.3,65.5,75.8,57.3,65.3,57.6,65.6,51,65,57.8,57.2,62.8,55.2,65.4,62.5,61.1,58.7,58.6,53,63.9,65.7,56.2,59.3,58.2,61.7,60.8,77.5,51.6,61.6,65,61.6,76.2,72.1,56.9,62.2,77.1,56.8,50.1,56.1,57.5,60.8,59.1,67.3,53,59,60.4,63.7,66.3,64.7,56.2,64.3,59.8,63.7,57.7,52.7,57.6,63.7,56.9,66.5,51.3,60,55.4,50.7,60.8,63,60.2,59.3,66.9,57.6,56.7,56,56.9,65,59.1],"y":[72.6,81.7,75.1,83,69.7,79.1,71.4,74.8,78.1,69,72.8,82.9,71.7,74.6,80.8,79.3,80.1,58.4,60.7,75.7,77.2,66,67.5,74.2,64,75.3,77.1,86.5,79.7,70.8,82.9,67,63.8,77.6,85.7,73.7,81.2,75.9,81.9,72.5,74.5,90.3,75.5,77.3,73.6,73.2,65.5,65.4,79.7,79.2,80.5,76.3,75.7,82.4,79.9,81.4,89.3,82.1,68.9,82.6,80.3,80.5,71.8,75.9,75.8,85.5,70.3,83.8,83.8,86.1,76.2,64.9,77.1,75.5,77.1,69.8,77.6,80.7,69.1,73.2,73.7,75,75.1,69.8,84.4,79.8,78.7,74.8,73.7,79.9,77.5,72.8,72.1,72.8,85.1,84.1,77.5,74.6,64.6,65.1,82.5,75,76.9,97.1,73.5,71.1,80.1,67.7,72.7,63.5,72.5,77.8,78.6,71.6,73.8,77.7,74.6,59.3,79.1,81.5,73.5,73.6,79.4,66.9,56.9,66.2,74.6,73.9,69.1,71.9,63.5,78,60,86.8,76.6,77.9,63.2,72.9,80.5,74.9,69.5,67.7,70.8,72,69.3,83.6,71.4,70.4,76.3,68.5,81.1,61.2,82.7,74.9,73.7,81.1,90.1,76.6,82.6,74.1,70.8,68.9,76.5,85.3,71.1,78.2,79.3,84.4,79.8,68.5,76.7,70.3,83.7,87.8,77,70.1,69.5,71.3,80.3,76.1,71.2,77.2,79,79.6,81.7,79.6,70.1,71.5,70.5,64.1,69.1,66.2,85.3,70,77.7,80.4,77.8,77.1,67.9,66.6,74.4,61.6,77.2,68.2,75.4,75.5,75.4,68.7,80.5,75.3,73.4,82.2,80.2,77.3,75.9,71.4,72,81.2,72.2,76.5,74.6,73.5,89.3,82.1,80.7,81.6,80.5,74.7,74.5,74.1,77.8,76.7,72.4,69.2,69.5,69.5,71.9,82.6,75.5,73.7,64.8],"text":["purity: 60.4<br />yield: 72.6","purity: 61.2<br />yield: 81.7","purity: 50.7<br />yield: 75.1","purity: 58.6<br />yield: 83.0","purity: 69.5<br />yield: 69.7","purity: 54.3<br />yield: 79.1","purity: 66.5<br />yield: 71.4","purity: 59.8<br />yield: 74.8","purity: 56.0<br />yield: 78.1","purity: 64.4<br />yield: 69.0","purity: 60.7<br />yield: 72.8","purity: 62.2<br />yield: 82.9","purity: 61.3<br />yield: 71.7","purity: 77.1<br />yield: 74.6","purity: 58.1<br />yield: 80.8","purity: 54.7<br />yield: 79.3","purity: 61.3<br />yield: 80.1","purity: 61.2<br />yield: 58.4","purity: 61.5<br />yield: 60.7","purity: 53.8<br />yield: 75.7","purity: 58.8<br />yield: 77.2","purity: 57.3<br />yield: 66.0","purity: 63.1<br />yield: 67.5","purity: 60.9<br />yield: 74.2","purity: 56.2<br />yield: 64.0","purity: 59.4<br />yield: 75.3","purity: 59.0<br />yield: 77.1","purity: 63.1<br />yield: 86.5","purity: 56.0<br />yield: 79.7","purity: 59.5<br />yield: 70.8","purity: 67.3<br />yield: 82.9","purity: 50.5<br />yield: 67.0","purity: 62.3<br />yield: 63.8","purity: 61.9<br />yield: 77.6","purity: 70.2<br />yield: 85.7","purity: 54.7<br />yield: 73.7","purity: 55.0<br />yield: 81.2","purity: 65.4<br />yield: 75.9","purity: 61.7<br />yield: 81.9","purity: 64.7<br />yield: 72.5","purity: 62.8<br />yield: 74.5","purity: 67.5<br />yield: 90.3","purity: 63.7<br />yield: 75.5","purity: 54.9<br />yield: 77.3","purity: 56.3<br />yield: 73.6","purity: 61.7<br />yield: 73.2","purity: 74.4<br />yield: 65.5","purity: 58.5<br />yield: 65.4","purity: 71.5<br />yield: 79.7","purity: 58.5<br />yield: 79.2","purity: 56.6<br />yield: 80.5","purity: 61.2<br />yield: 76.3","purity: 58.0<br />yield: 75.7","purity: 56.6<br />yield: 82.4","purity: 73.5<br />yield: 79.9","purity: 65.2<br />yield: 81.4","purity: 61.1<br />yield: 89.3","purity: 63.0<br />yield: 82.1","purity: 53.5<br />yield: 68.9","purity: 59.7<br />yield: 82.6","purity: 55.5<br />yield: 80.3","purity: 58.9<br />yield: 80.5","purity: 63.8<br />yield: 71.8","purity: 64.1<br />yield: 75.9","purity: 55.5<br />yield: 75.8","purity: 58.4<br />yield: 85.5","purity: 57.1<br />yield: 70.3","purity: 50.3<br />yield: 83.8","purity: 61.5<br />yield: 83.8","purity: 62.5<br />yield: 86.1","purity: 58.6<br />yield: 76.2","purity: 56.4<br />yield: 64.9","purity: 61.1<br />yield: 77.1","purity: 65.8<br />yield: 75.5","purity: 57.0<br />yield: 77.1","purity: 58.1<br />yield: 69.8","purity: 60.6<br />yield: 77.6","purity: 76.9<br />yield: 80.7","purity: 62.0<br />yield: 69.1","purity: 58.4<br />yield: 73.2","purity: 62.8<br />yield: 73.7","purity: 64.3<br />yield: 75.0","purity: 61.3<br />yield: 75.1","purity: 63.1<br />yield: 69.8","purity: 60.6<br />yield: 84.4","purity: 62.5<br />yield: 79.8","purity: 75.1<br />yield: 78.7","purity: 56.7<br />yield: 74.8","purity: 58.7<br />yield: 73.7","purity: 59.4<br />yield: 79.9","purity: 81.7<br />yield: 77.5","purity: 54.5<br />yield: 72.8","purity: 58.9<br />yield: 72.1","purity: 63.0<br />yield: 72.8","purity: 68.7<br />yield: 85.1","purity: 65.3<br />yield: 84.1","purity: 55.4<br />yield: 77.5","purity: 60.0<br />yield: 74.6","purity: 56.1<br />yield: 64.6","purity: 65.0<br />yield: 65.1","purity: 54.3<br />yield: 82.5","purity: 74.8<br />yield: 75.0","purity: 65.4<br />yield: 76.9","purity: 71.1<br />yield: 97.1","purity: 61.3<br />yield: 73.5","purity: 62.8<br />yield: 71.1","purity: 55.5<br />yield: 80.1","purity: 81.4<br />yield: 67.7","purity: 64.6<br />yield: 72.7","purity: 71.3<br />yield: 63.5","purity: 61.8<br />yield: 72.5","purity: 56.1<br />yield: 77.8","purity: 61.3<br />yield: 78.6","purity: 57.3<br />yield: 71.6","purity: 60.4<br />yield: 73.8","purity: 60.6<br />yield: 77.7","purity: 58.8<br />yield: 74.6","purity: 59.5<br />yield: 59.3","purity: 66.4<br />yield: 79.1","purity: 57.1<br />yield: 81.5","purity: 59.8<br />yield: 73.5","purity: 57.5<br />yield: 73.6","purity: 65.6<br />yield: 79.4","purity: 68.5<br />yield: 66.9","purity: 60.4<br />yield: 56.9","purity: 58.4<br />yield: 66.2","purity: 52.3<br />yield: 74.6","purity: 59.9<br />yield: 73.9","purity: 50.2<br />yield: 69.1","purity: 61.5<br />yield: 71.9","purity: 64.6<br />yield: 63.5","purity: 57.1<br />yield: 78.0","purity: 61.8<br />yield: 60.0","purity: 58.3<br />yield: 86.8","purity: 58.8<br />yield: 76.6","purity: 56.9<br />yield: 77.9","purity: 70.1<br />yield: 63.2","purity: 77.6<br />yield: 72.9","purity: 50.3<br />yield: 80.5","purity: 56.8<br />yield: 74.9","purity: 60.8<br />yield: 69.5","purity: 68.4<br />yield: 67.7","purity: 64.8<br />yield: 70.8","purity: 57.5<br />yield: 72.0","purity: 62.3<br />yield: 69.3","purity: 64.9<br />yield: 83.6","purity: 59.9<br />yield: 71.4","purity: 61.2<br />yield: 70.4","purity: 60.8<br />yield: 76.3","purity: 72.0<br />yield: 68.5","purity: 51.0<br />yield: 81.1","purity: 60.3<br />yield: 61.2","purity: 61.3<br />yield: 82.7","purity: 57.2<br />yield: 74.9","purity: 57.3<br />yield: 73.7","purity: 59.4<br />yield: 81.1","purity: 81.8<br />yield: 90.1","purity: 64.9<br />yield: 76.6","purity: 55.1<br />yield: 82.6","purity: 60.2<br />yield: 74.1","purity: 69.5<br />yield: 70.8","purity: 54.6<br />yield: 68.9","purity: 73.7<br />yield: 76.5","purity: 60.4<br />yield: 85.3","purity: 58.8<br />yield: 71.1","purity: 61.1<br />yield: 78.2","purity: 59.3<br />yield: 79.3","purity: 64.3<br />yield: 84.4","purity: 65.5<br />yield: 79.8","purity: 75.8<br />yield: 68.5","purity: 57.3<br />yield: 76.7","purity: 65.3<br />yield: 70.3","purity: 57.6<br />yield: 83.7","purity: 65.6<br />yield: 87.8","purity: 51.0<br />yield: 77.0","purity: 65.0<br />yield: 70.1","purity: 57.8<br />yield: 69.5","purity: 57.2<br />yield: 71.3","purity: 62.8<br />yield: 80.3","purity: 55.2<br />yield: 76.1","purity: 65.4<br />yield: 71.2","purity: 62.5<br />yield: 77.2","purity: 61.1<br />yield: 79.0","purity: 58.7<br />yield: 79.6","purity: 58.6<br />yield: 81.7","purity: 53.0<br />yield: 79.6","purity: 63.9<br />yield: 70.1","purity: 65.7<br />yield: 71.5","purity: 56.2<br />yield: 70.5","purity: 59.3<br />yield: 64.1","purity: 58.2<br />yield: 69.1","purity: 61.7<br />yield: 66.2","purity: 60.8<br />yield: 85.3","purity: 77.5<br />yield: 70.0","purity: 51.6<br />yield: 77.7","purity: 61.6<br />yield: 80.4","purity: 65.0<br />yield: 77.8","purity: 61.6<br />yield: 77.1","purity: 76.2<br />yield: 67.9","purity: 72.1<br />yield: 66.6","purity: 56.9<br />yield: 74.4","purity: 62.2<br />yield: 61.6","purity: 77.1<br />yield: 77.2","purity: 56.8<br />yield: 68.2","purity: 50.1<br />yield: 75.4","purity: 56.1<br />yield: 75.5","purity: 57.5<br />yield: 75.4","purity: 60.8<br />yield: 68.7","purity: 59.1<br />yield: 80.5","purity: 67.3<br />yield: 75.3","purity: 53.0<br />yield: 73.4","purity: 59.0<br />yield: 82.2","purity: 60.4<br />yield: 80.2","purity: 63.7<br />yield: 77.3","purity: 66.3<br />yield: 75.9","purity: 64.7<br />yield: 71.4","purity: 56.2<br />yield: 72.0","purity: 64.3<br />yield: 81.2","purity: 59.8<br />yield: 72.2","purity: 63.7<br />yield: 76.5","purity: 57.7<br />yield: 74.6","purity: 52.7<br />yield: 73.5","purity: 57.6<br />yield: 89.3","purity: 63.7<br />yield: 82.1","purity: 56.9<br />yield: 80.7","purity: 66.5<br />yield: 81.6","purity: 51.3<br />yield: 80.5","purity: 60.0<br />yield: 74.7","purity: 55.4<br />yield: 74.5","purity: 50.7<br />yield: 74.1","purity: 60.8<br />yield: 77.8","purity: 63.0<br />yield: 76.7","purity: 60.2<br />yield: 72.4","purity: 59.3<br />yield: 69.2","purity: 66.9<br />yield: 69.5","purity: 57.6<br />yield: 69.5","purity: 56.7<br />yield: 71.9","purity: 56.0<br />yield: 82.6","purity: 56.9<br />yield: 75.5","purity: 65.0<br />yield: 73.7","purity: 59.1<br />yield: 64.8"],"type":"scatter","mode":"markers","marker":{"autocolorscale":false,"color":"rgba(210,105,30,1)","opacity":1,"size":5.66929133858268,"symbol":"circle","line":{"width":1.88976377952756,"color":"rgba(210,105,30,1)"}},"hoveron":"points","showlegend":false,"xaxis":"x","yaxis":"y","hoverinfo":"text","frame":null}],"layout":{"margin":{"t":43.7625570776256,"r":7.30593607305936,"b":40.1826484018265,"l":37.2602739726027},"plot_bgcolor":"rgba(255,255,255,1)","paper_bgcolor":"rgba(255,255,255,1)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187},"title":{"text":"Yield from a Batch Reactor","font":{"color":"rgba(0,0,0,1)","family":"","size":17.5342465753425},"x":0,"xref":"paper"},"xaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[48.515,83.385],"tickmode":"array","ticktext":["50","60","70","80"],"tickvals":[50,60,70,80],"categoryorder":"array","categoryarray":["50","60","70","80"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"y","title":{"text":"Purity","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"yaxis":{"domain":[0,1],"automargin":true,"type":"linear","autorange":false,"range":[54.89,99.11],"tickmode":"array","ticktext":["60","70","80","90"],"tickvals":[60,70,80,90],"categoryorder":"array","categoryarray":["60","70","80","90"],"nticks":null,"ticks":"outside","tickcolor":"rgba(51,51,51,1)","ticklen":3.65296803652968,"tickwidth":0.66417600664176,"showticklabels":true,"tickfont":{"color":"rgba(77,77,77,1)","family":"","size":11.689497716895},"tickangle":-0,"showline":true,"linecolor":"rgba(0,0,0,1)","linewidth":0.66417600664176,"showgrid":false,"gridcolor":null,"gridwidth":0,"zeroline":false,"anchor":"x","title":{"text":"Yield (%)","font":{"color":"rgba(0,0,0,1)","family":"","size":14.6118721461187}},"hoverformat":".2f"},"shapes":[{"type":"rect","fillcolor":null,"line":{"color":null,"width":0,"linetype":[]},"yref":"paper","xref":"paper","x0":0,"x1":1,"y0":0,"y1":1}],"showlegend":false,"legend":{"bgcolor":"rgba(255,255,255,1)","bordercolor":"transparent","borderwidth":1.88976377952756,"font":{"color":"rgba(0,0,0,1)","family":"","size":11.689497716895}},"hovermode":"closest","barmode":"relative"},"config":{"doubleClick":"reset","showSendToCloud":false},"source":"A","attrs":{"58d63c44c10b":{"x":{},"y":{},"type":"scatter"}},"cur_data":"58d63c44c10b","visdat":{"58d63c44c10b":["function (y) ","x"]},"highlight":{"on":"plotly_click","persistent":false,"dynamic":false,"selectize":false,"opacityDim":0.2,"selected":{"opacity":1},"debounce":0},"shinyEvents":["plotly_hover","plotly_click","plotly_selected","plotly_relayout","plotly_brushed","plotly_brushing","plotly_clickannotation","plotly_doubleclick","plotly_deselect","plotly_afterplot","plotly_sunburstclick"],"base_url":"https://plot.ly"},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->

### Mapping Colours

It may help to map yield to size and purity to colour, notice in this case that a monotone [^mtn] rather than a divergint pallete is more appropriate for this data, because the data are strictly positive:

[^mtn]: Monotone here in the mathematical sense, as in always decreasing or increasing as opposed to referring necessarily to the colour


```r
reactor <- read.csv(file = "~/Notes/DataSci/Visual_Analytics/05_Data/Tutorial5Ex1.csv", header = TRUE, sep = ",")
ggplot(reactor, aes(x = purity, y = yield, col = purity, size = yield)) +
  geom_point() + 
  scale_color_gradient(low="grey", high="purple") +
  labs(x = "Purity", y = "Yield (%)", title = "Yield from a Batch Reactor", col = "%") + 
  theme_classic()
```

![](./figure/unnamed-chunk-4-1.png)<!-- -->

### Overlaying a Model

There appears to be no significant relationship between purity and yield, *Ordinary Least Squares Regression* may produce linear Model that can be used to quantify this observation:


```r
# Make mappings only in point layer to not conflict with model
ggplot(reactor, aes(x = purity, y = yield)) +
  geom_point(aes(size = yield, col = purity)) + 
  labs(x = "Purity", y = "Yield (%)", title = "Yield from a Batch Reactor", col = "%") + 
  theme_classic() +
    stat_smooth(method = 'lm', col = "#8b0a50", lty = 2 ) +
  scale_color_gradient(low="grey", high="purple") + 
  guides()
```

![](./figure/unnamed-chunk-5-1.png)<!-- -->

This model clearly suggests that there is no relationship between purity and yield because the model has no rate of change.

### Adjust the Scale

The data only has observations on the domain of purity between 50-80%, it may be desiralbe to adjust the limits:


```r
# Make mappings only in point layer to not conflict with model
ggplot(reactor, aes(x = purity, y = yield)) +
  geom_point(aes(size = yield, col = purity)) + 
  labs(x = "Purity", y = "Yield (%)", title = "Yield from a Batch Reactor", col = "%") + 
  theme_classic() +
    stat_smooth(method = 'lm', col = "#8b0a50", lty = 2 ) +
  scale_color_gradient(low="grey", high="purple") + 
  guides() +
  scale_x_continuous(limits = c(0, 100)) +
  scale_y_continuous(limits = c(0, 100)) +
  scale_size_continuous(range = c(0.3,1.75))
```

![](./figure/unnamed-chunk-6-1.png)<!-- -->

This suggests that if more data was collected perhaps there could be a relationship between yield and purity, perhaps a type of [sigmoid function](https://en.wikipedia.org/wiki/Sigmoid_function) would potentially be an appropriate model by way of an analogy to population growth and carrying capacity.

### Conclusion

The Data does not clearly show any significant relationship between purity and yield, however a wider range of data and larger amount of data should be collected in order to confirm that patterns in the data aren't obfuscated by noise.





## Question 2

The chart provided has a misleading base, the base begins from 40% as opposed to beginning from 0 which gives the illusion that the relative proportion by which the plot has changed has been more significant than it is. 


![image-20200401190803552](./figure/image-20200401190803552.png)


It can be apporporiate to scale continuous scatter or time series plots by reducing the range of the y-axis in order to draw focus to a narrow range of change, however, it is not in this context appropriate because the plot is a bar chart.



## Question 3


```r
eg_cars <- read.csv(file = "/home/ryan/Notes/DataSci/Visual_Analytics/05_Data/Tutorial5Ex3.csv", header = TRUE, sep = ",")
```

### Scatter Plot


```r
ggplot(data = eg_cars, mapping = aes(x = speed, y = dist)) +
  geom_point(mapping = aes(col = dist), size = 3) +
  scale_color_continuous(low = "#90ee90", high = "#Cd3278") +
  guides(col = guide_legend("Stopping Distance", reverse = TRUE)) +
  theme_light() + 
  labs(x = "Vehicle Speed", y = "Stopping Distance", title = "Stopping Distance Predicted by Vehicles")
```

![](./figure/unnamed-chunk-8-1.png)<!-- -->

### Modelling the Data

A Linear Model could be fit to the data:



```r
mise()
```



```r
cars_model <- lm(dist ~ speed, data = cars)

newdata <- data.frame(speed = c(30, 33, 37))
newdata$dist <- predict(object = cars_model, newdata)
newdata$datatype <- c("pred")

cars$datatype <- c("obs")

cars <- rbind(cars, newdata)
cars$datatype <- factor(cars$datatype)


ggplot(data = cars, aes(x = speed, y = dist, shape = datatype)) +
  geom_point(mapping = aes(col = dist), size = 3) +
  scale_color_continuous(low = "#90ee90", high = "#Cd3278") +
  guides(col = guide_legend("Stopping Distance", reverse = TRUE)) +
  stat_smooth(method = "lm", aes(group = 1), lty = 2, se = FALSE, col = "lightblue") +
  theme_light() + 
  labs(x = "Vehicle Speed", y = "Stopping Distance", title = "Stopping Distance Predicted by Vehicles")
```

A superior Model would be a Quadratic Model because it would be expected that the stopping force generated by any car would be more or less constant (ideally manufacturers would reach a threshold), then by calculus:

$$
\begin{aligned}
F &=  m\cdot  a \\
\implies  F &\propto a
\end{aligned}
$$

and hence the distance travelled while breaking will be

$$
\begin{aligned}
v &=  a \cdot   t \\
\int v \mathrm{d}t &=  \int a\cdot t \mathrm{d}t  \\
s&= \frac{1}{2}a\cdot  t^2 \\
2s &= a\cdot  \left( \frac{v}{a} \right)^2\\
2as &= v^2
\end{aligned}
$$

Under the assumption that deceleration is constant:

$$
\begin{aligned}
s \propto v^2
\end{aligned}
$$

And so a better model would be:


```r
ggplot(data = cars, aes(x = speed, y = dist)) +
  geom_point(mapping = aes(col = dist), size = 3) +
  scale_color_continuous(low = "#90ee90", high = "#Cd3278") +
  guides(col = guide_legend("Stopping Distance", reverse = TRUE)) +
  stat_smooth(method = "lm", aes(group = 1), lty = 2, se = FALSE, formula = y ~ poly(x, 2), col = "lightblue") +
  labs(x = "Vehicle Speed", y = "Stopping \nDistance", title = "Stopping Distance Predicted by Vehicles") +
  theme_light() + 
    scale_linetype_manual(values = c(1, 2), drop = FALSE)
```

![](./figure/unnamed-chunk-10-1.png)<!-- -->

This model appears to fit the data more closely.

### Conclusion

There appears to be a quadratic relationship between a vehicle's speed and it's stopping distance.

## Question 4 

### Scatter Plot

A scatter plot can be produced, the y-axis can be mapped to the Sepal Length because it explains the most variation that is distinct from the other features (as shown by PCA and a biplot below), the remaining features would be best mapped to different colours because colours are the most distinct mapping element, the species can be mapped to the shape of the plotted item.



```r
data <- pivot_longer(iris, cols = c("Petal.Length", "Petal.Width", "Sepal.Width"))
data <- pivot_longer(iris, cols = c("Petal.Length", "Petal.Width", "Sepal.Width"))

ggplot(data = data, aes(y = Sepal.Length, col = name, x = value, shape = Species)) +
  geom_point(size = 2.5, alpha = 0.9) +
  theme_bw() +
  scale_color_brewer(palette = "Accent", labels = c("Petal Length", "Petal Width", "Sepal Width")) +
  labs(title = "Iris Data Set", y = "Sepal Length", x = "Measurment") +
  guides(col = guide_legend("Measured \n Feature"))
```

![](./figure/unnamed-chunk-11-1.png)<!-- -->


#### Map all features Uniquely 

Another option would be dedicating the x and y axis to Sepal Length and petal width (these being the choices that explain the most variation of the data and map best to the axis as justified below) and use other mapping variables to denote the other features.

The available continuous aesthetics made availabel in ggplot2 are:

* Colour
* Size
* Transperancy

Colour and size are more effective aesthetics than Transperancy and so will be used.



```r
ggplot(iris, aes(y = Sepal.Length, x = Petal.Width, col = Petal.Length, size = Sepal.Width, shape = Species)) +
  geom_point() +
  guides(col = guide_legend("Petal Width"),
         size = guide_legend("Sepal Width")) +
  labs(y = "Sepal Length", x = "Petal Width", title = "Iris Data")
```

![](./figure/unnamed-chunk-12-1.png)<!-- -->

This mapping technique however is less effectinve than the earlier plots because generally physical seperation is a more effective aesthetic technique than either size or continuous colour.


##### Choosing the X/Y axis

In order to choose the X/Y axis in the above plots it was necessary to consider:

1. Which variable explained the most data
  + The physical location of a visualisation is the most effective visual aesthetic and so should be reserved for the features that explain the most variation
2. Which Features have data further apart
  + Far apart data may not be practically plotted on the same axis.


#### Bar Chart

In order to consider the spread of the features look at the bar plot of the mean values:


```r
iris_stats <- data.frame("means"  = apply(iris[, -5], 2, mean),
                         "spread" = apply(iris[, -5], 2, sd))
iris_stats <- data.frame(Observation = rownames(iris_stats), iris_stats)
iris_stats <- data.frame(Observation = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"), iris_stats)

ggplot(iris_stats, aes(x = Observation, y = means, fill = Observation)) +
  geom_col(col = "grey") +
#  scale_fill_manual(values = c("red", "green", "Blue", "Black")) +
  theme_bw() +
  labs(y = "Mean Value", x = "", title = "Mean Values of Iris Values") +
  guides(fill = FALSE)  +
  scale_fill_brewer(palette = "Dark2")
```

![](./figure/unnamed-chunk-13-1.png)<!-- -->

This however tells us nothing about the spread of the data, a better way may be to plot representations of density curves:



```r
(read.csv(file = "/home/ryan/Notes/DataSci/Visual_Analytics/05_Data/Tutorial5Ex4.csv") == iris) %>% mean()
```

```
## [1] 1
```

```r
names(iris)
```

```
## [1] "Sepal.Length" "Sepal.Width"  "Petal.Length" "Petal.Width"  "Species"
```

```r
data <- pivot_longer(iris, cols = c("Petal.Length", "Petal.Width", "Sepal.Length", "Sepal.Width"))
ggplot(data = data, aes(x = value, fill = name )) +
# geom_histogram(position = "dodge", aes(y = ..density..), bins = 5) + guides(col = FALSE) +
stat_function(fun = dnorm, args = list(mean = mean(iris$Petal.Length), sd = sd(iris$Petal.Length)), aes(col = "Petal.Length")) +
stat_function(fun = dnorm, args = list(mean = mean(iris$Petal.Width), sd = sd(iris$Petal.Width)), aes(col = "Petal.Width")) +
stat_function(fun = dnorm, args = list(mean = mean(iris$Sepal.Length), sd = sd(iris$Sepal.Length)), aes(col = "Sepal.Length")) +
stat_function(fun = dnorm, args = list(mean = mean(iris$Sepal.Width), sd = sd(iris$Sepal.Width)), aes(col= "Sepal.Width")) +
  theme_classic() + 
  labs(x = "Measurement Value", y = "Density", title = "Distribution of Measurement Values") +
  guides(col = guide_legend("Measurement")) +
#  scale_color_discrete(labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width")) +
  scale_color_brewer(palette = "Dark2", labels = c("Sepal Length", "Sepal Width", "Petal Length", "Petal Width"))
```

![](./figure/unnamed-chunk-14-1.png)<!-- -->

```r
#ggplot(iris, aes(x))


#So Sepal length should be x and width, petal length should be y
# 
```

This shows that Sepal Width and Petal width are the two features with values furthest apart meaning they may be good candidates for the x/y axis respectively. 

### PCA

Pca can be used to show which factors explain the most variance in the data independent of each other:



```r
irisTB <- iris[-5]
pca.mod <- prcomp(irisTB, scale = TRUE)
# biplot(pca.mod, scale = 0, cex = 0.5)

# library(devtools); install_github("vqv/ggbiplot")
ggbiplot::ggbiplot(pca.mod) +
  theme_bw() +
  labs(title = "PCA or IRIS Data")
```

```
## ------------------------------------------------------------------------------
```

```
## You have loaded plyr after dplyr - this is likely to cause problems.
## If you need functions from both plyr and dplyr, please load plyr first, then dplyr:
## library(plyr); library(dplyr)
```

```
## ------------------------------------------------------------------------------
```

```
## 
## Attaching package: 'plyr'
```

```
## The following objects are masked from 'package:Hmisc':
## 
##     is.discrete, summarize
```

```
## The following object is masked from 'package:purrr':
## 
##     compact
```

```
## The following objects are masked from 'package:plotly':
## 
##     arrange, mutate, rename, summarise
```

```
## The following objects are masked from 'package:dplyr':
## 
##     arrange, count, desc, failwith, id, mutate, rename, summarise,
##     summarize
```

```
## 
## Attaching package: 'scales'
```

```
## The following object is masked from 'package:coin':
## 
##     pvalue
```

```
## The following object is masked from 'package:purrr':
## 
##     discard
```

```
## The following object is masked from 'package:readr':
## 
##     col_factor
```

![](./figure/unnamed-chunk-15-1.png)<!-- -->

In this case Sepal width and either petal measurement explains the most variance.

Given that petal width has the lower variance (from the density curves) and has a value further away from Sepal Width than petal length it will be used for the corresponding axis.

## Question 5

A multi Plot can be generated by using the `facet_grid()` layer in ggplot2. It would be possible to map different plots to different features or different species, but, as will be shown, using seperate plots for seperate species will allow for better seperated data (because the data follow the same trend simply offset by the different *starting point* afforded by the change in species.)


```r
names_pretty <- c("Petal\nLength", "Petal\nWidth", "Sepal\nWidth")
data         <- pivot_longer(iris, cols = c("Petal.Length", "Petal.Width", "Sepal.Width"))
data$name    <- factor(x = data$name, levels = unique(data$name), labels = c("Petal\n Length", "Petal\n Width", "Sepal\n Width"))
data$Species <- factor(x = data$Species, levels = unique(data$Species), labels = c("Setosa", "Versicolor", "Virginica"))

ggplot(data = data, aes(y = Sepal.Length, x = value, col = name)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_brewer(palette = "Accent", labels = c("Petal Length", "Petal Width", "Sepal Width")) +
  labs(title = "Iris Data Set", y = "Sepal Length", x = "Measurment") +
  guides(col = guide_legend("Measured \n Feature")) +
  facet_grid(. ~ Species)
```

![](./figure/unnamed-chunk-16-1.png)<!-- -->

```r
ggplot(data = data, aes(y = Sepal.Length, x = value, col = Species)) +
  geom_point(size = 2) +
  theme_bw() +
  scale_color_brewer(palette = "Accent") +
  labs(title = "Iris Data Set", y = "Sepal Length", x = "Measurement") +
  guides(col = guide_legend("Measured \n Feature")) +
  facet_grid(. ~ name)
```

![](./figure/unnamed-chunk-16-2.png)<!-- -->

Due to the pysical seperation, it would be more effective to plot Species in different plots.

It's also worth noting that **_R_** does come in with a built in function to acheive this:


```r
plot(iris[,-5])
```

![](./figure/unnamed-chunk-17-1.png)<!-- -->

## Question 6 (Parallel Coordinate Visualisation)

A parallel Coordinate Visualisation can be created in ggplot 2 using by creating a unique id for each observation and passing that as a `group()` parameter to the `geom_line()` layer:


```r
# Import the Data Set
parcd <- read.csv(file = "/home/ryan/Notes/DataSci/Visual_Analytics/05_Data/Tutorial5Ex6.csv", header = TRUE, sep = ",")

# Give Each Row an ID Value
parcd <- data.frame(id = 1:nrow(parcd), parcd)
parcd$id <- factor(parcd$id, unique(parcd$id), ordered = FALSE)

# Make the Data Frame Longer
parcd <- pivot_longer(parcd, cols = c("Item.1", "Item.2", "Item.3", "Item.4", "Item.5"))

# Use Grous to Seperate the LInes
ggplot(parcd, aes(x = name, y = value, col = id)) +
  geom_line(aes(group = id) )  +
  guides(col = FALSE) +
  theme_bw() +
  labs(x = "", y = "Value", title = "Parallell Coordinate Plots")
```

![](./figure/unnamed-chunk-18-1.png)<!-- -->

It is also possible to do make other aesthetic mappings when using a parallell coordinate plot.


```r
mise()
```



```r
my_diamonds <- data.frame(id = 1:nrow(diamonds), diamonds)
my_diamonds <- my_diamonds[sample(1:nrow(diamonds), size = 100),]
my_diamonds <- pivot_longer(my_diamonds, cols = c("carat","depth", "price", "table","x","y"))





ggplot(my_diamonds, aes(x = name, y = value, col = color, alpha = cut) ) +
  geom_line(aes(group = id), alpha = 0.25) +
  geom_point() +
  theme_bw()
```

































































































