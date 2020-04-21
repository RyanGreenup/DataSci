
pacman::p_load(caret, scales, ggplot2, rmarkdown, shiny, ISLR, class, BiocManager, xtable,
               corrplot, plotly, tidyverse, latex2exp, stringr, reshape2, cowplot, ggpubr,
               rstudioapi, wesanderson, RColorBrewer, colorspace, gridExtra, grid, car,
               boot, colourpicker, tree, ggtree, mise, rpart, rpart.plot, knitr, MASS,
               magrittr, EnvStats,tidyverse,tidyr,devtools, bookdown, leaps, car, clipr,
               tikzDevice, e1071, ggbiplot, base)
#install.packages("ggbiplot")

mise()
set.seed(23)



desc.stats <- data.frame(
Mean = apply(envFeat, 2, mean), # 1 is rows, 2 is cols p. 401 ISL TB
Variance = apply(envFeat, 2, var) # 1 is rows, 2 is cols p. 401 ISL TB
)
desc.stats$variable <- row.names(desc.stats)

descStatsTidy <- pivot_longer(desc.stats, cols = c(Mean, Variance), names_to = "Statistic", values_to = "Value")

ggplot(descStatsTidy, aes(x = variable, y = Value, fill = Statistic)) +
  geom_col(position = "dodge")


envDF