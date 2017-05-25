df <- read.csv("~/Documents/masters/data/masters - SG april to april.csv")
# first need a subset of 2017 data:
df17 <- df[df$year == 2017, ]
# subsetting the data while creating a new column that sums the scores.
score <- aggregate(Score ~ player, df17, FUN = "sum")
score$score17 <- score$Score; score$Score <- NULL
holes <- aggregate(Score ~ player, df17, FUN = "length")
holes$Holes <- holes$Score; holes$Score <- NULL
temp <- merge(score, holes, ra); rm(score); rm(holes)
temp$rank17 <- NA
temp[temp$Holes == 72, "rank17"] <- rank(temp$score17[temp$Holes == 72], ties.method = "min")
mv <- max(temp$rank17, na.rm = TRUE)
temp[temp$Holes == 36, "rank17"] <- rank(temp$score17[temp$Holes == 36], ties.method = "min") + mv
# justing got second place, there was a playoff
temp[temp$player == "Rose, Justin", "rank17"] <- 2
df17 <- merge(df17, temp)
df17$score <- df17$score17; df17$score17 <- NULL
df17$holes <- df17$Holes; df17$Holes <- NULL
df17$rank <- df17$rank17; df17$rank17 <- NULL
df17 <- df17[names(df)]
# now to add more rows to the players that missed the cut
temp <- df17[df17$holes == 36, ]
temp2 <- temp[rep(sequence(nrow(temp)), 1),]
temp2$Round.. <- ifelse(temp2$Round.. == 1, 3, 4)
temp2[, 7:14] <- NA
temp <- rbind(temp, temp2); rm(temp2)
df17 <- df17[df17$holes != 36, ]
df17 <- rbind(df17, temp)
df17 <- df17[order(df17$player), ]
df <- df[df$year != 2017, ]
df <- rbind(df, df17)
rm(df17); rm(temp); rm(mv)
df1 <- df[rep(sequence(nrow(df)), 5), ]
df1 <- df1[order(df1$year, df1$player, df1$Hole.., df1$Round..), ]
df1$Level <- rep(c(3, 2, 1, 0, -1), dim(df1)[1] / 5)

write.csv(df1, "~/Documents/masters/completemasters.csv", row.names = FALSE)
