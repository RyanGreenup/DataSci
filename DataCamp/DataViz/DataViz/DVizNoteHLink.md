---
title: "R Notebook"
output: html_notebook
---

# Working on External Editor


These are notes taken from the [Data Visualisation Stream](https://campus.datacamp.com/courses/data-visualization-with-ggplot2-2/chapter-1-statistics?ex=1)

This sort of works does the live update work?
Do they talk?

$$
\begin{aligned}
f\left( a \right)= \oint_{\gamma} \left( \frac{f\left( z \right)}{z- a} \right) \mathrm{d}z 
\end{aligned}
$$

So it does somewhat work, but it breaks the notebook preview which is fairly bloody annoying.
Welp there's a simple equation.

blah

Actually it might as well be live

When you first start up **_R_** you're going to want to use:

```
: imap jj <Esc> 
: map <Space> <Leader>
: nnoremap <Leader>w :w<CR>
```

```{r}
editInVim <- function()
  rstudioapi::terminalExecute(paste("nvim", rstudioapi::getSourceEditorContext()$path))
editInVim
```


When you first start up **_R_** you're going to want to use:

```
: imap jj <Esc> 
: map <Space> <Leader>
: nnoremap <Leader>w :w<CR>
```


This is an [R Markdown](http://rmarkdown.rstudio.com) Notebook. When you execute code within the notebook, the results appear beneath the code. 

Try executing this chunk by clicking the *Run* button within the chunk or by placing your cursor inside it and pressing *Ctrl+Shift+Enter*. 

```{r}k
plot(cars)
```

Add a new chunk by clicking the *Insert Chunk* button on the toolbar or by pressing *Ctrl+Alt+I*.

When you save the notebook, an HTML file containing the code and output will be saved alongside it (click the *Preview* button or press *Ctrl+Shift+K* to preview the HTML file).

The preview shows you a rendered HTML copy of the contents of the editor. Consequently, unlike *Knit*, *Preview* does not run any R code chunks. Instead, the output of the chunk when it was last run in the editor is displayed.
