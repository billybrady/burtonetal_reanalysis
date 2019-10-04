library(openxlsx)
library(tidyverse)
library(MASS)
library(boot)

#### MeToo tweets ----

#read encoded data
metoo_clean <- read.xlsx("clean_metoo.xlsx")

#view distribution -- interesting to note that there appear to be extreme outliers...a tweet with 60K RTs when median is near 0
metoo_clean %>% ggplot(aes(x=Diffusion)) + geom_histogram(color = "black", fill="lightblue") +
  theme_bw() +
  geom_vline(aes(xintercept=mean(Diffusion)),
             color="red", linetype="dashed", size=1) +
  labs(x='Frequency', y="Retweet Count") +
  theme(text=element_text(size = 12, face = 'bold'))

median(metoo_clean$Diffusion)

#make sure reproduce results - confirmed estimate matches what is reported in paper
summary(glm.nb(Diffusion ~ `Moral-Emotional`+ Moral+ Emotional, data = metoo_clean))

#Step 1: Bootstrap to examine stability of results: guessing outliers create instability
cof <- function(formula, data, indices) {
  d <- data[indices,] 
  fit <- glm.nb(formula, data=d)
  return(summary(fit)$coefficients[2])
}
# bootstrapping with 500 replications
results <- boot(data=metoo_clean, statistic=cof,
                R=500, formula=Diffusion ~ `Moral-Emotional`+ Moral+ Emotional)
#view results
results
plot(results)
boot.ci(results, type = "basic")

#store in list to plot w/ ggplot
a <- results[2]

#Step 2: investigate the extreme outliers to confirm they drive instability seen in bootstrapping
#sort by diffusion
metoo_clean <- metoo_clean %>% arrange(desc(Diffusion))

#remove top 10 Diffusion counts and re-run model -- sign of ME actually flips to positive
metoo_clean_rmv10 <- metoo_clean %>% tail(-10)
summary(glm.nb(Diffusion ~ `Moral-Emotional`+ Moral+ Emotional, data = metoo_rmv10))

#remove top 1% Diffusion counts -- sign of ME remains positive and now sig...
metoo_rmv1pct <- metoo_clean %>% tail(-nrow(.)*.01)
z <- summary(glm.nb(Diffusion ~ `Moral-Emotional`+ Moral+ Emotional, data = metoo_rmv1pct))


#xyz re-analysis ----
# Subset
INSERT_DF <- metoo_clean[c("clean_text", "diffusion")] 

# Count characters in tweet message before removing x, y, z
tweets <- INSERT_DF$clean_text
charCount1 <- nchar(tweets) 

# Remove "x", "y" , and "z"
tweets <- gsub("z", "", tweets)  
tweets <- gsub("y", "", tweets) 
tweets <- gsub("x", "", tweets)

# Count characters after removing x, y, z
charCount2 <- nchar(tweets)  

# Determine number of x's, y's, and z's present by calculating how many characters we removed in the process
XYZcount <- charCount1 - charCount2

# Bind the XYZcount onto the dataframe for analysis
INSERT_DF$XYZcount <- XYZcount
metoo_xyz <- INSERT_DF

#confirm analysis reported in text
summary(glm.nb(diffusion ~ XYZcount , data = metoo_xyz))

#random count gen bounded by range of ME count in metoo sample
#can loop this to generate multiple rand count generations and model results
range(metoo_xyz$`Moral-Emotional`)
metoo_xyz$random <-sample.int(14, 151572, replace = TRUE)
summary(glm.nb(diffusion ~ XYZcount , data = metoo_xyz))

# bootstrapping with 500 replications
results <- boot(data=metoo_xyz, statistic=cof,
                R=500, formula=diffusion ~ random)

# view results
results
plot(results)
boot.ci(results, type = "basic")
