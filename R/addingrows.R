# we need to add rows to those players who don't have 72 rows
# for each year they played in the masters.  NA in score column
df <- read.csv("~/Documents/masters/data/addrows.csv")
# first I'd like to identify the rows which need duplicating...
df <- data.table::data.table(df)
# maybe subset df where df2$rounds < 72...?
df2 <- df[df$holes < 72, ] # subset df
df <- df[df$holes == 72, ] # subset df with the rest
df3 <- df2[rep(sequence(nrow(df2)), 1), ] # duplicate each row of df2
df3$score <- NA; df3$rtp.score <- NA
# change round to 3 and 4.
df3$round <- ifelse(df3$round == 1, 3, 4)
df4 <- rbind(df2, df3)
df <- rbind(df, df4)
rm(df2); rm(df3); rm(df4)
write.csv(df, "number of masters imputed.csv", row.names = FALSE)
############### Test ######################
test <- df[, list(total=.N),by=c("year", "player")]
# nice!  I love the syntax of data.table.
summary(factor(test$total))
