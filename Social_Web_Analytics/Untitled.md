---
tags: [Notebooks/Analytics Programming]
title: Introduction to R
created: '2020-03-20T23:39:02.222Z'
modified: '2020-03-20T23:53:28.315Z'
---

# Practical 02


## Preamble


```r
## (01) Clean up the Iris Data

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
                 swirl, corrplot, gridExtra, mise, latex2exp, tree, rpart, MASS, rtweet)
  
}

load.pac()
```

```
## Loading required package: pacman
```

```r
 mise()
```


 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 
 

## 2.10 Decompose the Following Script


```r
main <- function() {
    f = findFactors(1012)
    print(f)
}

findFactors <- function(x) {
    factors = c()
    for (a in 1:x) {
        if ((x%%a) == 0) {
            factors = c(factors, a)
        }
    }
    return(factors)
}

main()
```

```
##  [1]    1    2    4   11   22   23   44   46   92  253  506 1012
```

if $a$ is less than $x$ and $x$ is divisible by a then a must be a factor of x


if $x$ is divisible by $a$ for some real natural numbers $x < a$ then that value is a factor, this is logically equivalant to saying:

$$
x  \mod{a} = 0, \enspace \forall x \in \{n: \enspace 1 \leq a\}
$$

The function `findfactors()` tests every natural number less than $x$ and then adds it to a vector if it's a factor as determined by the modulus operator `%%`.

The function `main()` calls `findfactors()` which generates the list of factors and then following that prints it could be improved to:

* Return factors of a specified number thusly:
* Not Return 1
  + 1 is the multiplicative identity, it is neither a prime nor a composite number, hence it should not be considered a factor.
  + A number is not a factor of itself generally because it would need to be multiplied by 1.



```r
 
    f = findFactors(natural)
    print(f)
}

findFactors <- function(x) {
    factors = c()
    for (a in 1:x) {
        if ((x%%a) == 0 & a != x & a != x ) {
            factors = c(factors, a)
        }
    }
    return(factors)
}

print_factors(357)
```

```
## Error: <text>:4:1: unexpected '}'
## 3:     print(f)
## 4: }
##    ^
```




# Using R Tweet

You may need your private keys and tokens etc; I have saved theme [here](/home/ryan/Dropbox/Studies/2020Autumn/Social_Web_Analytics/Practicals/Twitter_Tokens.org)


```r
followers.id <- rtweet::get_followers("Tesla")
followers.id %>% head()
```

```
## # A tibble: 6 x 1
##   user_id            
##   <chr>              
## 1 381867603          
## 2 2801924232         
## 3 1240842150620860423
## 4 1240831512272146432
## 5 1240841648529014785
## 6 715206894357360640
```



























