#If we take SSR and SSE as they are described in my notes, will they add to SST?



regression_error <- rnorm(n = 3000, mean=0, sd=300)
x_regression <- seq(1:30000)
y_regression <- x_regression*3+1+regression_error
line_regression <- lm(y_regression~x_regression)


plot(x = x_regression, y = y_regression)
abline(lm(y_regression~x_regression))


intercept <- line_regression$coefficients[1]
slope <- line_regression$coefficients[2]


anova_regression <- anova(line_regression)
SSR_R <- anova_regression$`Sum Sq`[1]      
SSE_R <- anova_regression$`Sum Sq`[2]



SSR= sum(((x_regression*slope+intercept)-mean(y_regression))^2 )
if(round(SSR,3)==round(SSR_R,3)){print("SSR is Correct")}else(print("SSR is Wrong"))

SSE=sum((y_regression-(x_regression*slope+intercept))^2 )
if(round(SSE,3)==round(SSE_R,3)){print("SSE is Correct")}else(print("SSE is Wrong"))
  

SST_form=sum((y_regression-mean(y_regression))^2)
SST_add=SSE+SSR




if(round(SST_form,0)==round(SST_add,0)){print("The Formulas are the same")}else(print("They are different"))






