# first we need to read and clean up the data a little
# this inclues altering the structure of data matrix that SAS returns 
# since every probability is a response, each player for each hole, year, and round 
# has six probabilities.  We need each row to contain all six probs for each hole,
# round and year.  Going to use package tidyr, super simple data shape managing.
temp <- read.csv("~/Documents/masters/data/model 2 - 2016 player probabilities.csv")
X <- read.csv("~/Documents/masters/data/ranks.2016.csv")
X <- X[order(X$name), ]
modtest <- numeric()
# all these variables prevent us from turning the table
# from "long" to "wide". So they will need to be NULL
temp$Mu <- NULL; temp$Mu.no.bulps <- NULL; temp$rtp_score_grouped <- NULL
temp$rtp_score <- NULL; temp$score <- NULL; temp$Mu..no.blups. <- NULL
df <- tidyr::spread(temp, Level, P) # now we have a "wide" table
### calculate the probs for the individual score on the hole
df$P.triple <- df$`3`
df$P.double <- df$`2` - df$`3`
df$P.bogey <- df$`1` - df$`2`
df$P.par <- df$`0` - df$`1`
df$P.birdie <- df$`-1` - df$`0`
df$P.eagle <- 1 - df$`-1`
# get rid of some columns we don't need anymore
df[, c("-1", "0", "1", "2", "3")] <- NULL
df.sim <- df; rm(df)
rm(temp) # done with this now
# create 2 matrices with the probs for each hole for rounds 1 and 2
probs1 <- df.sim[df.sim$round == 1, 
                 c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
probs2 <- df.sim[df.sim$round == 2, 
                 c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
finish <- data.frame()
topar <- c(3, 2, 1, 0, -1, -2)
########################### START of  SIMULATIONS ##################
# here is the beginning of the loop.  Each loop is one masters.
n <- 1000
# 1 - 1000
print(system.time(for(i in 1:n) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x) 
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 1001 - 2000
print(system.time(for(i in (n+1):(n*2)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 2001 - 3000
print(system.time(for(i in ((n*2) + 1):(n*3)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 3001 - 4000 
print(system.time(for(i in ((n*3) + 1):(n*4)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 4001 - 5000
print(system.time(for(i in ((n*4) + 1):(n*5)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 5001 - 6000
print(system.time(for(i in ((n*5) + 1):(n*6)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 6001 - 7000
print(system.time(for(i in ((n*6) + 1):(n*7)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 7001 - 8000
print(system.time(for(i in ((n*7) + 1):(n*8)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 8001 - 9000  
print(system.time(for(i in ((n*8) + 1):(n*9)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2)
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
finish <- data.frame()
# 9001 - 10000
print(system.time(for(i in ((n*9) + 1):(n*10)) {
  # simulate rounds 1 and 2
  score1 <- apply(probs1, 1, function(x) # simulate round 1
    sample(topar, 1, prob = x))
  score2 <- apply(probs2, 1, function(x) # simulate round 2
    sample(topar, 1, prob = x))
  df.sim$score1[df.sim$round == 1] <- score1 + df.sim$par[df.sim$round == 1]
  df.sim$score1[df.sim$round == 2] <- score2 + df.sim$par[df.sim$round == 2]
  # summarising the first two rounds
  df.sim$friday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  
  cut <- df.sim[!duplicated(df.sim$name), c("name", "friday")]
  cut$rank <- ave(cut$friday, FUN = function(x)  
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut <- merge(dfcut, cut)
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
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$sunday <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "friday", "sunday")],
          cut[, c("name", "rank", "friday", "sunday")])
  finish1$simulationID <- as.factor(i)
  finish1 <- finish1[order(finish1$name), ]   
  modtest[i] <- cor(X$rank, finish1$rank, method = "spearman")   
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(finish)
#rm(list = ls())
###########################  END of SIMULATIONS ######################
hist(modtest, col = "green", main = "10000 Spearman's Rho", 
     xlab = "Model 2")
quantile(modtest, c(0.025, 0.975))
summary(modtest)
