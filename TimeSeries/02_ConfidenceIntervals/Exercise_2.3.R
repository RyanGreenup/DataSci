#Exercise 2,3
#---------------------------
#Part a)
#------------------------


##Variables
  alpha <-  0.05
  mean  <-  0.5           #Std. Deviation of Background Site TcCB
  sd    <-  0.8           #Mean of Background Site TcCB 


#qnorm returns the value at which the specified probability under the 
#curve of a normal distribution occurs.

TcCB_95_back <-  qnorm(alpha, mean=mean, sd=sd, lower.tail=FALSE, log.p=FALSE)
TcCB_95_back

#Part b)
#------------------------
#We need to find the probability that the sample mean of TcCB will exceed 
#The 95th Percentile (TcCB_95_back=1.815883).

#pnorm gives the value at which the area under the curve 
#corresponds to a given x-value.


n  <- 1                      #Sample Size for part B question
SE <- sd/sqrt(n)  #Standard Error of the Sample Mean


z_sample_23b <- (TcCB_95_back-mean)/SE #The standard Z-value for the distribution of sample means.


pnorm(z_sample_23b, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
answer_23b <- pnorm(z_sample_23b, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
answer_23b

#Observe that it would not even be necessary to calculate the standard Z-value
pnorm(TcCB_95_back, mean = mean, sd = SE, lower.tail = FALSE, log.p = FALSE)

#Thus the Probability that 1 sample size will have a mean value above the 95th percentile of the background factory is:
percent(answer_23b)  #Must have the Scales Package enabled.

#Part c)
#------------------------
#We need to use a binomial distribution to determine this probability

####Variables
p <- answer_23b   #The probability success, i.e. that a Sample will be above the 95th percentile
q <- 1-p      #The probability of failure, as above.

k <- 2            #The relevant number of successes, the relevant number of samples above the 95th percentile
n_23c <- 10           #The number of samples taken

###Find the probability
ans_23c <- pbinom(k, n_23c, p, lower.tail=FALSE, log.p=FALSE)

##The probability, of 2 samples of the 10, being above the 95th percentile is:
percent(ans_23c)
