# first we need to read and clean up the data a little
# this inclues altering the structure of data matrix that SAS returns 
# since every probability is a response, each player for each hole, year, and round 
# has six probabilities.  We need each row to contain all six probs for each hole,
# round and year.  Going to use package tidyr, super simple data shape managing.
#### Packages used are tidyr and data.table
setwd("~/Documents/masters") # just change this to "~/Research/Cody" or whatever
####
# then make sure your prob file is in data/ directory
if (file.exists("data/sims/finish2014.csv")) file.remove("data/sims/finish2014.csv")
temp <- read.csv("data/Player probabilities - model 7 2014.csv")
names <- names(temp)
names(temp) <- c(names[-(39:40)], "pwith", "pwithout")
rm(names)
# all these variables prevent us from turning the table
# from "long" to "wide". So they will need to be NULL
# remove the columns that cause a problem with tidyr::spread
temp$rtp_score <- NULL; temp$score <- NULL; temp$pwithout <- NULL
df <- tidyr::spread(temp, Level, pwith) # now we have a "wide" table
### calculate the probs for the individual score on the hole
# there are some holes where we want prob = 0 for triple bogey
### calculate the probs for the individual score on the hole
df$P.triple <- ifelse(is.na(df$`3`), 0, df$`3`)
df$P.double <- ifelse(is.na(df$`3`), df$`2`, df$`2` - df$`3`)
df$P.bogey <- df$`1` - df$`2`
df$P.par <- df$`0` - df$`1`
df$P.birdie <- ifelse(is.na(df$`-1`), 1 - df$`0`, df$`-1` - df$`0`)
df$P.eagle <- ifelse(!is.na(df$`-1`), 1 - df$`-1`, 0)
# get rid of some columns we don't need anymore
df[, c("-1", "0", "1", "2", "3")] <- NULL
df.sim <- df; rm(df)
df.sim <- df.sim[, c("name", "round", "par", 
                     "P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")]
rm(temp) # done with this now
# create 2 matrices with the probs for each hole for rounds 1 and 2
probs1 <- df.sim[df.sim$round == 1, 
                 c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
probs2 <- df.sim[df.sim$round == 2, 
                 c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
topar <- c(3, 2, 1, 0, -1, -2)
########################### START of  SIMULATIONS ##################
# here is the beginning of the loop.  Each loop is one masters.
n <- 10000
# 1 - 10000
print(system.time(for(i in 1:n) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x) 
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probs3 <- dfnotcut[dfnotcut$round == 3, 
                     c("P.triple", "P.double", "P.bogey", 
                       "P.par", "P.birdie", "P.eagle")]
  probs4 <- dfnotcut[dfnotcut$round == 4, 
                     c("P.triple", "P.double", "P.bogey", 
                       "P.par", "P.birdie", "P.eagle")]
  score3 <- apply(probs3, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probs4, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score1[dfnotcut$round == 3] <- score3 + dfnotcut$par[dfnotcut$round == 3]
  dfnotcut$score1[dfnotcut$round == 4] <- score4 + dfnotcut$par[dfnotcut$round == 4]
  # summarising the next two rounds
  dfnotcut$sunday <- ave(dfnotcut$score1, dfnotcut$name,
                         FUN = function(x) sum(x, na.rm = TRUE))
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$sunday, ties.method = "min")
  finish1 <- round4[, c("name", "rank", "sunday")]
  finish1 <- finish1[finish1$rank == 1, ]
  finish1$simulationID <- as.factor(i)
  write.table(finish1, "data/sims/finish2014.csv", append = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE)
  rm(finish1); rm(dfnotcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
######## END of Simulation ###############
# now run some tests.  
rm(list = ls())
vars <- c("name", "place", "score", "simID")
finish <- read.csv("data/sims/finish2014.csv", header = F)
colnames(finish) <- vars; rm(vars)
finish$place <- NULL
library(data.table)
finish <- data.table(finish)
n <- max(finish$simID)
df <- finish[, list(firstplace = .N), by = "name"]
rm(finish)
df$modranks <- rank(-df$firstplace, ties.method = "min")
df$prob <- df$firstplace/n
df$firstplace <- NULL
masters <- read.csv("data/ranks2014.csv")
vegas <- read.csv("data/vegasodds2014.csv")
vegas$vegasranks <- rank(vegas$odds, ties.method = "min")
test <- merge(masters, vegas)
# damn, looks like Stephen Gallacher and Matt Jones are missing from vegas odds file.
test <- merge(test, df, all.x = TRUE)
# keeping all obs from masters and adding rank from simulations
test$modranks <- ifelse(is.na(test$modranks), 
                        max(test$modranks, na.rm = T) + 1, test$modranks)
print(dim(test)[1]) # number of observations we are comparing.
print(cor(test[, c("rank", "modranks", "vegasranks")], 
          method = "spearman")) # results
# save these results
write.csv(test, paste("data/modelResults/", gsub(":", ".", Sys.time()), 
                      ".csv", sep = ""), row.names = FALSE) 
rm(list = ls())
