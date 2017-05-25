#1 Sum the data by round and year.  18 hole scores for each round 
#  that each player has played.  So I think the total number of rows would be 
#  divided by 4 (or some weighted avg. of 2 and 4)?
#2 Sum the data by year.  Maybe average 18 hole score each year since 
#  some players miss the cut?

# here's the summary for the first item.  I use libraries tidyr and dplyr
temp <- read.csv("~/Documents/masters/data/predprobsprelim.csv")
df.temp <- tidyr::spread(temp, Level, Mu)
#### first data table
library(dplyr)
df <- df.temp %>%
  group_by(name, year, round) %>%
  summarise_each(funs(sum), score)
head(df)
write.csv(df, "df1.csv")
### second data table
library(data.table)
df.temp <- data.table(df.temp)
df2 <- df.temp[, list(shots = sum(score), rounds = .N/18),
               by = c("name", "year")]
df2$meanscore <- df2$shots / df2$rounds
write.csv(df2, "df2.csv")
rm(df.temp); rm(temp)
# add a plot
library(ggplot2)
gg <- ggplot(df, aes(x = year, y = score, colour = factor(round)))
gg <- gg + geom_jitter(alpha = 0.4) + 
  geom_smooth(se = F, method = "loess", size = 1.5)
gg
