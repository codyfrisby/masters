# read and combine all the sims files
# combine all the simulation files:
rm(list = ls())
files <- list.files("~/Documents/masters/data/sims/") 
setwd("~/Documents/masters/data/sims/")
##create a list of files in the sims directory
finish <- data.frame()  ##create empty data frame
for (i in 1:length(files)){  
  ## loop through files and write data to data frame
  df2 <- read.csv(files[i])
  finish <- rbind(finish, df2)
}
rm(df2)
finish$X <- NULL
# now to create a table or something of all the rankings by player
# experimenting with a summary of everything
# to calculate the probabilities:
library(data.table)
finish <- data.table(finish)
X <- read.csv("~/Documents/masters/data/ranks.2016.csv")
X <- X[order(X$name), ]
df <- finish[, list(firstplace = .N), by = c("name", "rank")]
rm(finish)
##### compute correlations based on the ranks of the frequency of 
# first place finishes.
x <- df[df$rank == 1, ]
x <- x[order(x$name), ]
y <- X[X$name %in% x$name, ]
y <- y[order(y$name), ]
x$simranks <- rank(-x$firstplace, ties.method = "min")
xx <- x[order(x$name), ]$simranks
yy <- y[order(y$name), ]$rank
print(cor(xx, yy, method = "spearman"))
df$prob <- df$firstplace/10000
# just for testing and evaluating.
# order the data table by rank at the top and most probable.
df <- df[order(df$rank, -rank(df$prob), decreasing = FALSE), ]
df$odds <- df$prob/(1 - df$prob)
# we could also say most likely to finish top 10 by
# summing the probabilities if rank <= 10.....?
top10 <- df[df$rank <= 10, ]
probtop10 <- aggregate(prob ~ name, top10, sum)
rm(top10)
setwd("~/Documents/masters/data")
write.csv(df, "model6WITHOUTplayereffect.csv", row.names = FALSE)
