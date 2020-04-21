---
title: "Using the t-test Quiz"
output: 
  html_document: 
    keep_md: yes
---

# Question 1
Compute the lower bound of the 90% confidence interval for the mean BD for females - mean BD for males. Assume equal variances.


```r
c1 <- read.csv("./0datasets/crabsJ1hcWQfwp52r3XqzMIh4.csv")
head(c1)
```

```
##   sp sex index   FL   RW   CL   CW   BD
## 1  B   F    27 13.4 11.8 28.4 32.7 11.7
## 2  O   F    22 17.5 14.4 34.5 39.0 16.0
## 3  B   M    30 16.1 11.6 33.8 39.0 14.4
## 4  O   M    49 23.0 16.8 47.2 52.1 21.5
## 5  O   M    32 18.0 13.4 36.7 41.3 17.1
## 6  B   M    10 11.8 10.5 25.2 29.3 10.3
```

```r
t.test(BD ~ sex, data = c1, var.equal = TRUE, conf = 0.9)
```

```
## 
## 	Two Sample t-test
## 
## data:  BD by sex
## t = -1.4537, df = 98, p-value = 0.1492
## alternative hypothesis: true difference in means is not equal to 0
## 90 percent confidence interval:
##  -2.3801741  0.1580853
## sample estimates:
## mean in group F mean in group M 
##        13.51837        14.62941
```


# Question 2
The centre line is the median of a boxplot

# Question 3
Use R and a t test to compute a p-value for a difference in mean length of carapace length (CL) by sex. Assume equal variances.

```r
c2 <- read.csv("./0datasets/crabsINtTIDdA9ps6siFdyHIJ.csv")
head(c2)
```

```
##   sp sex index   FL   RW   CL   CW   BD
## 1  O   F    25 18.0 14.9 34.7 39.5 15.7
## 2  B   M    25 15.0 11.9 32.5 37.2 13.6
## 3  O   M     6 12.5  9.4 24.2 27.0 11.2
## 4  O   M    48 22.1 15.8 44.6 49.6 20.5
## 5  O   M    27 17.4 12.8 36.1 39.5 16.2
## 6  O   F     2 11.4  9.2 21.7 24.1  9.7
```

```r
t.test(CL ~ sex, data = c2, var.equal = TRUE, conf = 0.9)
```

```
## 
## 	Two Sample t-test
## 
## data:  CL by sex
## t = -1.2901, df = 98, p-value = 0.2
## alternative hypothesis: true difference in means is not equal to 0
## 90 percent confidence interval:
##  -4.0790587  0.5120715
## sample estimates:
## mean in group F mean in group M 
##        30.11458        31.89808
```


# Question 4
Use R and a Wilcoxon-Mann-Whitney test to compute a p-value for a difference in mean size of rear width (RW) by sex.

```r
c3 <- read.csv("./0datasets/crabsuHxn8uYEHU750PEuUzNo.csv")
head(c1)
```

```
##   sp sex index   FL   RW   CL   CW   BD
## 1  B   F    27 13.4 11.8 28.4 32.7 11.7
## 2  O   F    22 17.5 14.4 34.5 39.0 16.0
## 3  B   M    30 16.1 11.6 33.8 39.0 14.4
## 4  O   M    49 23.0 16.8 47.2 52.1 21.5
## 5  O   M    32 18.0 13.4 36.7 41.3 17.1
## 6  B   M    10 11.8 10.5 25.2 29.3 10.3
```

```r
wilcox.test(RW ~ sex, c3, alternative = "two.sided")
```

```
## 
## 	Wilcoxon rank sum test with continuity correction
## 
## data:  RW by sex
## W = 1636.5, p-value = 0.005753
## alternative hypothesis: true location shift is not equal to 0
```

