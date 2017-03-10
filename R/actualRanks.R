# getting the ranks of a certain year:
df <- read.csv("~/Documents/masters/data/masters data file for OLR.csv")
df2015 <- df[df$year == 2015, ]
df2014 <- df[df$year == 2014, ]
rm(df)
library(data.table)
df <- data.table(df2015); rm(df2015)
temp <- df[, list(score = sum(Score, na.rm = TRUE), holes = .N), 
           by = c("player")]
temp$holes <- ifelse(temp$score < 200, 36, 72)
temp2 <- temp[temp$holes == 72, ]
temp2$rank <- rank(temp2$score, ties.method = "min")
temp3 <- temp[temp$holes != 72, ]
temp3$rank <- rank(temp3$score, ties.method = "min") + max(temp2$rank)
temp <- rbind(temp2, temp3)
temp$holes <- NULL
names(temp)[1] <- "name"
write.csv(temp, "~/Documents/masters/data/ranks2015.csv")
# for 2014
df <- data.table(df2014); rm(df2014)
temp <- df[, list(score = sum(Score, na.rm = TRUE), holes = .N), 
           by = c("player")]
temp$holes <- ifelse(temp$score < 240, 36, 72)
temp2 <- temp[temp$holes == 72, ]
temp2$rank <- rank(temp2$score, ties.method = "min")
temp3 <- temp[temp$holes != 72, ]
temp3$rank <- rank(temp3$score, ties.method = "min") + max(temp2$rank)
temp <- rbind(temp2, temp3)
temp$holes <- NULL
names(temp)[1] <- "name"
write.csv(temp, "~/Documents/masters/data/ranks2014.csv")
