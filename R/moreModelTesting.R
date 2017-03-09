# comparing actual finishes to Vegas:
rm(list = ls())
vegas <- read.csv("~/Documents/masters/data/vegasOdds2016.csv")
masters2016 <- read.csv("~/Documents/masters/data/ranks.2016.csv")
masters2016 <- masters2016[order(masters2016$rank), ]
# merge the two data frames:
# first fix some of the name differences:
temp <- merge(vegas, masters2016)
temp$score <- NULL
names(temp)[4] <- "ActualRank"
print(cor(temp$ActualRank, temp$vegasRanks, method = "spearman"))
plot(temp$vegasRanks, temp$ActualRank, 
     pch = ifelse(temp$name == "Willett, Danny", 18, 1))
text(temp$vegasRanks[temp$name == "Willett, Danny"], 
     temp$ActualRank[temp$name == "Willett, Danny"], "Willet", pos = 3,
     cex = .75)
# side-by-side with our model:
df <- read.csv("~/Documents/masters/data/model6WITHOUTplayereffect.csv")
df <- df[df$rank == 1, ]
df$rank <- NULL; df$firstplace <- NULL; df$odds <- NULL
df$model6without <- rank(-df$prob, ties.method = "min")
df$prob <- NULL
temp2 <- merge(temp, df) # finishers who placed first in simulation
temp2 <- temp2[order(temp2$ActualRank), ]
print(cor(temp2[,3:5]))
temp3 <- temp2[temp2$ActualRank < 30, ] # top 30 finishers
print(cor(temp3[, 3:5]))

