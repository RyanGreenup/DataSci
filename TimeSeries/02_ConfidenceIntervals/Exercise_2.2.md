

```r
# Exercise 2.2
```

```r
###Variables


# Load Packages
if(require('pacman')){
  library('pacman')
}else{
  install.packages('pacman')
  library('pacman')
}
pacman::p_load(scales)

x    <- 30   #The value on the normal distribution we are concerned with
mean <- 20   #the population mean.
sd   <- 5    #The population standard deviation





# As we know the population standard deviation and the population mean
# a comparison to p-values on a standard normal distribution can be used,
#R will do all of this on it's own, without even the need to calculate a relative Z value.

pnorm(x,                #the value on the normal distribution
      mean=mean,        #the population mean value of the normal distribution.
      sd=sd,            # the population Standard deviation of the of the normal distribution.
      lower.tail=FALSE, #If this is TRUE the test will be P(X<_x)
      log.p=FALSE       #if TRUE the probabilites are given as log(p)
)
```

```
## [1] 0.02275013
```

```r
#Observe that this is the same if we use a z value relative to the 
#Standard Normal distribution

z <- (30-20)/5
pnorm(z, mean=0, sd=1, lower.tail=FALSE, log.p=FALSE)
```

```
## [1] 0.02275013
```

```r
answer <- pnorm(z, mean=0, sd=1, lower.tail=FALSE, log.p=FALSE)


###Probability of mean exceeding 30:
percent(answer)        #The scales package must be enables for this
```

```
## [1] "2%"
```

