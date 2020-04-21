rmd <- list.files(pattern = '*.Rmd', recursive = T)
chunks <- paste0("```{r child = '", rmd, "'}\n```\n")
cat(chunks, sep = '\n')
# ```{r child = 'chapter1.Rmd'}
# ```
#
# ```{r child = 'chapter2.Rmd'}
# ```

