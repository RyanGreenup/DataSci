

## * Load Packages -----------------------------------------------------------

if (require("pacman")) {
  library(pacman)
} else{
  install.packages("pacman")
  library(pacman)
}

pacman::p_load(xts, sp, gstat, ggplot2, rmarkdown, reshape2, ggmap, parallel,
               dplyr, plotly, tidyverse, reticulate, UsingR, Rmpfr, swirl,
               corrplot, gridExtra, mise, latex2exp, tree, rpart, lattice, coin,
               primes, epitools, maps, clipr, ggmap, twitteR, ROAuth, tm,
               rtweet, base64enc, httpuv, SnowballC, RColorBrewer, wordcloud,
               ggwordcloud)

mise()


## ** Set up Tokens ===========================================================

options(RCurlOptions = list(
  verbose = FALSE,
  capath = system.file("CurlSSL", "cacert.pem", package = "RCurl"),
  ssl.verifypeer = FALSE
))

setup_twitter_oauth(
  consumer_key = "dE7HNKhwHxMqqWdBl1qo9OAkN",
  consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
  access_token = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
  access_secret = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ"
)

## ** rtweet ==================================================================
tk <-    rtweet::create_token(
  app = "SWA",
  consumer_key    = "dE7HNKhwHxMqqWdBl1qo9OAkN",
  consumer_secret = "7ByyC6lR9d2aqLoXHHBkOtKRiU10cHhGguroiGKQ69AGDMMI9V",
  access_token    = "1240821178014388225-sFWver0NDDY3BhPdPyg8d4mtQxnl0K",
  access_secret   = "HLBWzHcemHzYJnw5ZLvKpEhQ5KaWwK6Nsj6cxBjUf51NJ",
  set_renv        = FALSE
)

## * Political Tweets --------------------------------------------------------
n <- 100

# twitteR::userTimeline("billshortemp", n = n)
abtw <- rtweet::get_timeline("AdamBandt",       n = n, token = tk) ## Grn
altw <- rtweet::get_timeline("AlboMP",          n = n, token = tk) ## Lbr
smtw <- rtweet::get_timeline("ScottMorrisonMP", n = n, token = tk) ## Lib

# Create a Corpus ==============================================================

tweets <- c(altw$text, smtw$text, abtw$text)    # Get all the text
tweet_source <- tm::VectorSource(tweets)        # Create the
tweet_corpus <- tm::VCorpus(x = tweet_source)
tweet_corpus[[1]]$content
strwrap(tweet_corpus[[1]])


# Create a WordCloud #########################################################

## Clean the Corpus
sw <- c(stopwords(), "one")
clean_corp <- function(corpus, sw = stopowds()) {
  corpus <- tm_map(corpus, FUN = removeNumbers)
  corpus <- tm_map(corpus, FUN = removePunctuation)
  corpus <- tm_map(corpus, FUN = stripWhitespace)
  corpus <- tm_map(corpus, FUN = removeWords, sw)
  ## stopwords() returns characters and is fead as second argument
  corpus <- tm_map(corpus, FUN = stemDocument)
}
tweet_corpus <- clean_corp(tweet_corpus, sw)

wordcloud(tweet_corpus)

# Create a WordCloud Using TF-IDF Weighting ################################
## Create a DTM
tweet_matrix <- as.matrix(DocumentTermMatrix(tweet_corpus))
colnames(tweet_matrix)[1:3]

## Remove Empty Tweets Any tweets rendered null by stopwords or Stemming Will
## cause problems (e.g. the cosine distance of an all zero vector is
## meaningless)

null = which(colSums(as.matrix(tweet_matrix)) == 0)
null
length(null)

if(length(null)!=0){
  tweet_matrix = tdm[,-null]
}



## Use Term-Frequency and Inter-Document Frequency
N  <- nrow(tweet_matrix)   # Number of Documents
ft <- colSums(tweet_matrix>0) ## Number of Documents containing t
     ## See Lecture 4 [[~/Dropbox/Studies/2020Autumn/Social_Web_Analytics/04_slides.pdf]]

TF  <- log(tweet_matrix + 1)
IDF <- log(N / ft)

# Because each term in TF needs to be multiplied through
# each column of IDF there would be two ways to do it,
# a for loop which will be really slow
# Diagonalise the matrix then use Matrix multiplication

tweet_weighted           <- TF %*% diag(IDF)
colnames(tweet_weighted) <- colnames(tweet_matrix)

## Filter Relevant words
relevant <-
  sort(apply(tweet_weighted, 2, mean), decreasing = TRUE)[1:30]

## Make a wordcloud
p <- brewer.pal(n = 5, name = "Set3")
wordcloud(
  words = names(relevant),
  freq = relevant,
  colors = p,
  random.color = FALSE
)
# Posting Tweets using R =======================================================
# rtweet::post_tweet(status = "My first tweet from R, #WSU300958", token = tk)
# rtweet::post_tweet(status = "Political Wordcloud Using TF-IDF #WSU300958", media = "~/Pictures/WordCloud.png", token = tk)


# Use PCA To Visualise ----------------------------------------------------

# PCA Must be applied to a DTM
tweet_weighted[1:3, 1:3]
## Scaling introduces error
pol.pca <- prcomp(tweet_weighted, scale = FALSE)

# Inspect the Loading Vectors
pol.pca$rotation[1:6, 1:6]
str(pol.pca)

# View the biplot
biplot(pol.pca, cex = 0.5)
  ## see :
  ## https://github.com/vqv/ggbiplot

# Inspect the Scree Plot

pcaVar <- pol.pca$sdev ^ 2
pcaVar <- pcaVar[1:10]
pcaVarpr <- pcaVar / sum(pcaVar)
pcaVarpr <- enframe(pcaVarpr)

names(pcaVarpr) <- c("Principal.Component", "Proportion.Variance")
                        ## This throws a warning
for (i in 1:nrow(pcaVarpr)) {
  pcaVarpr[["Principal.Component"]][i] <- paste("PC", i)
}

pcaVarpr$Principal.Component <-
  factor(
    pcaVarpr$Principal.Component,
    ordered = TRUE,
    levels = pcaVarpr$Principal.Component
  )

ggplot(data = pcaVarpr,
       aes(x = Principal.Component,
           y = Proportion.Variance,
           group = 1)) +
  geom_point(size = 3, alpha = 0.7, col = "RoyalBlue")  +
  geom_line(col = "IndianRed") +
  labs(x = "Principal Component",
       y = "Proportion of Variance",
       title = "Variance Explained by PC, TF-IDF Weigthing Tweets by Party Leaders") +
  theme_classic() +
  geom_vline(xintercept = 4,
             col = "purple",
             lty = 2)

# Build the Plot ==============================================================
pca_data        <- data.frame(pol.pca$x[, 1:4])
pca_data$Party  <-
  factor(c(rep("Greens", n), rep("Labor", n), rep("Liberal", n)))
pol.km <-
  kmeans(tweet_weighted, centers = 3, nstart = 200) %>%  as.vector()
pca_data$kmeans <- as.factor(pol.km$cluster)
head(pca_data)

(ggpca <-
ggplot(data = pca_data,
       aes(x = PC1, y = PC2, size = PC3, col = Party)) +
  geom_point(alpha = 0.3) +
  ##  geom_point(aes(shape = kmeans)) +
  theme_classic() +
  ## Colours are applied in order of appearance of the factor
  ## Introduce factors alphabetically where possible
  scale_color_manual(values = c("Palegreen3", "DodgerBlue", "Palevioletred3")) +
  scale_size(range = c(0.1, 3)) +
  labs(main = "Tweets by Part Leaders using TF-IDF Weighting",
       subtitle = "First Two Principle Components without scaling") +
  stat_ellipse() ## +
  ## Scaling Fix
  ##  scale_x_continuous(limits = c(-quantile(pca_data$PC1, c(0.99)), quantile(pca_data$PC1, c(0.99)))) +
  ##  scale_y_continuous(limits = c(-quantile(pca_data$PC2, c(0.99)), quantile(pca_data$PC2, c(0.99))))
)

# Use MDSTo Visualise ----------------------------------------------------

# Euclidean Distance ===========================================================

tweet_matrix[1:3, 1:3]
tweet_dist          <- dist(tweet_weighted)
mds_data            <- cmdscale(d = tweet_dist, k = 2)
mds_data            <- as.data.frame(mds_data)
names(mds_data)     <- c("MDX", "MDY")
head(mds_data)

mds_data$Party <- factor(c(rep("Greens", n), rep("Labor", n), rep("Liberal", n)))


(ggmds <- ggplot(mds_data, aes(x = MDX, y = MDY, col = Party)) +
  geom_point() +
  scale_color_manual(values = c("Palegreen3", "DodgerBlue", "Palevioletred3")) +
  theme_classic())

# Compare to PCA ###############################################################
compar_mat_pca <- -pca_data[,1:2] ## Rotate the PCA
compar_mat_mds <- mds_data
(comp_mat <- cbind(compar_mat_mds, compar_mat_pca)) %>% as_tibble()

## Plot
## comp_mat_tidy <- cbind(
## comp_mat[,c(1,3)] %>%
##   as_tibble() %>%
##     pivot_longer(cols = c("MDX", "PC1"), names_to = "Technique", values_to = "xValues"),
## comp_mat[,c(2,4)] %>%
##   as_tibble() %>%
##     pivot_longer(cols = c("MDY", "PC2"), names_to = "Technique2", values_to = "yValues") # %>%
## )
##
## comp_mat_tidy$Technique <- substr(comp_mat_tidy$Technique, start = 1, stop = 2)
## (comp_mat_tidy <- as_tibble(comp_mat_tidy[,-3]))
##
## ggplot(comp_mat_tidy, aes(x = xValues, y = yValues, col = Technique)) +
##   geom_point() +
##   facet_grid(. ~ Technique)

# Binary Distance ==============================================================
## Remember to use the weigted values though,
## The weighted values are a good adjustment and is consistent with different
## measures of distance
dist_mat_bin <- dist(tweet_matrix, method = "binary")
mds_data_bin <- cmdscale(dist_mat_bin, k = 2)
plot(mds_data_bin[,1], mds_data_bin[,2], col = c("ForestGreen", "PowderBlue", "MediumVioletRed")[pca_data$Party])

dist(rbind(c(1,1,1), c(3,4,1)), method = "binary")
dist(rbind(c(1,1,1), c(3,4,1)), method = "euclidean")
# Cosine Distance===============================================================
## Using the Identity described here
##

# Create Unit Vectors ##########################################################
## Recall that
U   <- tweet_matrix %*% diag(1/sqrt(colSums(tweet_matrix^2)))
U_w <- tweet_weighted %*% diag(1/sqrt(colSums(tweet_weighted^2)))

# Create Distance Matrix #######################################################

# Plot the MDS #################################################################
## Make the Distance Matrix
dist_mat_cos <-dist(U_w, method = "euclidean")^2/2
## Make the MDS
mds_data_cos <- (cmdscale(dist_mat_cos, k = 2)
plot(mds_data_cos[,1], mds_data_cos[,2], col = pca_data$Party)
## Make the Plot

