---
title: "2_Practical; Linear Regression"
author: "Ryan Greenup 17805315"
date: "12 March 2019"
output:
  html_document:
    code_folding: hide
    keep_md: yes
    theme: flatly
    toc: yes
    toc_depth: 4
    toc_float: yes
  pdf_document: 
    keep_tex: yes
    toc: yes
always_allow_html: yes
  ##Shiny can be good but {.tabset} will be more compatible with PDF
    ##but you can submit HTML in turnitin so it doesn't really matter.
    
    ##If a floating toc is used in the document only use {.tabset} on more or less copy/pasted k
        #sections with different datasets
---




# Simple Linear Regression
Material of Tue 12 2019, week 2 

## Question 1

### (a) Import the Data


```r
adv <- read.csv(file = "Datasets/Advertising.csv", header = TRUE, sep = ",")
```

```
## Warning in file(file, "rt"): cannot open file 'Datasets/Advertising.csv': No such file or directory
```

```
## Error in file(file, "rt"): cannot open the connection
```


#### Inspect the structure of the Data Set


```r
head(adv)
```

```
## Error in head(adv): object 'adv' not found
```

```r
str(adv)
```

```
## Error in str(adv): object 'adv' not found
```

```r
summary(adv)
```

```
## Error in summary(adv): object 'adv' not found
```


### (b) Construct Scatter Plots {.tabset}

So I'd like to do a shiny ggplot here, however it's probably just as easy to use tabset by appending `{.tabset}` to the heading



```r
# par(mfrow=c(2,2))
# plot(lm(y~x))
```


Multiple Plots may be fitted into one output using either the `par()` package or the `layout()` package, I personally prefer the `layout()` package, I think because in the past I had a bad experience with `par()`:

* **In order to use `par`**:
  - `par( mfcol = c(ROW, COLS))`
  - `par (mfcol = c(ROW, COLS))`
    - That's not a typo, `mfrow` and `mfcol` are identical in this case
* **In order to use `layout`**:
  - `layout(MATRIX)`
    - The matrix should be a grid, the plots will be fed to that grid in numerical order so for example:
      - `layout(matrix(1:3, nrow = 1))` will fit the plots to the following matrix in the order specified:
      
      $$
      \begin{bmatrix}
      1 & 2 & 3
      \end{bmatrix}
      $$
* **In order to use `grid.layout():
  - grid.arrange(plot1, plot2, ncol = 2))
    - This is the only one that will work with ggplot2

#### Multi Fit Base Plots {.tabset}



```r
# Set the layout:
  # Using `layout()` command:

    layout(matrix(1:3, nrow =1))

  # using `par()` command:

    #par(mfrow=c(1,3)) # Specify the

# Set the plot Domain
pdom <- c(0, 300) #Plot Domain

#Generate the plots
plot(formula = Sales ~ TV, data = adv, xlim = pdom,
     main = "Sales Given TV Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```

```r
plot(formula = Sales ~ Newspaper, data = adv, xlim = pdom,
     main = "Sales Given Newspaper Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```

```r
plot(formula = Sales ~ Radio, data = adv, xlim = pdom,
     main = "Sales Given Radio Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```



#### GGPlot {.tabset}

##### Television Advertising


```r
adv$MeanAdvertising <- rowMeans(adv[,c(!(names(adv) == "Sales"))])
```

```
## Error in is.data.frame(x): object 'adv' not found
```

```r
AdvTVPlot <- ggplot(data = adv, aes(x = TV, y = Sales, col = MeanAdvertising)) +
  geom_point() + 
  theme_bw() +
  stat_smooth(method = 'lm', formula = y ~ poly(x, 2, raw = TRUE), se = FALSE) +
 ##stat_smooth(method = 'lm', formula = y ~ log(x), se = FALSE) +
  labs(col = "Mean Advertising", x= "TV Advertising") 
```

```
## Error in ggplot(data = adv, aes(x = TV, y = Sales, col = MeanAdvertising)): object 'adv' not found
```

```r
if(knitr::is_latex_output()){
 AdvTVPlot 
} else {
  AdvTVPlot%>% ggplotly()
}
```

```
## Error in eval(lhs, parent, parent): object 'AdvTVPlot' not found
```

##### Radio Advertising


```r
 AdvRadPlot <- ggplot(data = adv, aes(x = Radio, y = Sales, col = MeanAdvertising)) +
   geom_point() + 
   theme_bw() +
   labs(col = "Mean Advertising", x= "Radio Advertising") + 
   geom_smooth(method = 'lm')
```

```
## Error in ggplot(data = adv, aes(x = Radio, y = Sales, col = MeanAdvertising)): object 'adv' not found
```

```r
# padv %>% ggplotly() plotly doesn't work with knitr/LaTeX so test the output and choose accordingly:
#Thise could be combined into an interactive graph by wrapping in ggplotly(padv)

if(knitr::is_latex_output()){
  AdvRadPlot 
} else {
 AdvRadPlot %>% ggplotly()
}
```

```
## Error in eval(lhs, parent, parent): object 'AdvRadPlot' not found
```


##### Newspaper Advertising


```r
AdvNewsPlot <- ggplot(data = adv, aes(x = Newspaper, y = Sales, col = MeanAdvertising)) +
  geom_point() + 
  theme_bw() +
  labs(col = "Mean Advertising", x= "Newspaper Advertising")
```

```
## Error in ggplot(data = adv, aes(x = Newspaper, y = Sales, col = MeanAdvertising)): object 'adv' not found
```

```r
# padv %>% ggplotly() plotly doesn't work with knitr/LaTeX so test the output and choose accordingly:
#Thise could be combined into an interactive graph by wrapping in ggplotly(padv)

if(knitr::is_latex_output()){
  AdvNewsPlot
} else {
AdvNewsPlot %>% ggplotly()
}
```

```
## Error in eval(lhs, parent, parent): object 'AdvNewsPlot' not found
```



#### Base Plot



```r
pdom <- c(0, 300) #Plot Domain
plot(formula = Sales ~ TV, data = adv, xlim = pdom,
     main = "Sales Given TV Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```

```r
plot(formula = Sales ~ Newspaper, data = adv, xlim = pdom,
     main = "Sales Given Newspaper Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```

```r
plot(formula = Sales ~ Radio, data = adv, xlim = pdom,
     main = "Sales Given Radio Advertising")
```

```
## Error in eval(m$data, eframe): object 'adv' not found
```























