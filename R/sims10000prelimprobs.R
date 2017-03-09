# first we need to read and clean up the data a little
# this inclues altering the structure of data matrix that SAS returns 
# since every probability is a response, each player for each hole, year, and round 
# has six probabilities.  We need each row to contain all six probs for each hole,
# round and year.  Going to use package tidyr, super simple data shape managing.
temp <- read.csv("~/Documents/masters/data/predprobsprelim.csv") 
df <- tidyr::spread(temp, Level, Mu) # now we have a "wide" table
### calculate the probs for the individual score on the hole
df$P.triple <- df$`3`
df$P.double <- df$`2` - df$`3`
df$P.bogey <- df$`1` - df$`2`
df$P.par <- df$`0` - df$`1`
df$P.birdie <- df$`-1` - df$`0`
df$P.eagle <- 1 - df$`-1`
# get rid of some columns we don't need anymore
df[, c("-1", "0", "1", "2", "3")] <- NULL
rm(temp) # done with this now
# this is the field for 2017, updated 2-16-17.  
# source: http://www.pga.com/news/masters/2017-masters-tournament-field-list

#################### simulating since 2010 how about#######3###
#df <- df[df$year > 2009, ]
##########################################
# bring in the players who are in the 2017 field
players <- read.csv("~/Documents/masters/data/field2017.csv")
# subset the data with those players that are playing this year, 2017.
df <- df[df$name %in% players$name, ]
#df <- df[order(df$year, -(rank(df$round)), decreasing = TRUE), ]
# now to get the final simulation data frame.
df.sim <- df[!duplicated(df[c("hole", "name", "round")]) | 
               !duplicated(df[c("hole", "name", "round")], fromLast = FALSE),]
# create 2 matrices with the probs for each hole for rounds 1 and 2
probs1 <- df.sim[df.sim$round == 1, 
        c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
probs2 <- df.sim[df.sim$round == 2, 
                 c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
####################### start the loop for the 10,000 masters
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
  df_dups <- df[c("name", "year")]
  cut <- df.sim[!duplicated(df_dups), c("name", "year", "friday")]
  cut$rank <- ave(cut$friday, cut$year, FUN = function(x) 
    rank(x, ties.method = "min"))
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <- cut[cut$friday <= min(cut$friday) + 10 | cut$rank <= 50, ]
  cut <- cut[!(cut$friday <= min(cut$friday) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
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
  df.sim$score1[df.sim$round == 3] <- score3 + df.sim$par[df.sim$round == 3]
  df.sim$score1[df.sim$round == 4] <- score4 + df.sim$par[df.sim$round == 4]
  # summarising the first two rounds
  df.sim$sunday <- ave(df.sim$score1, df.sim$name, df.sim$year, 
                       FUN = function(x) sum(x, na.rm = TRUE))
  
  
  
  
  
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
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
  score1 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  score2 <- apply(probs, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score1
  df.sim$round1 <- ifelse(score1 == "par",df.sim$Par,
                          ifelse(score1 == "birdie",df.sim$Par - 1,
                                 ifelse(score1 == "eagle",df.sim$Par - 2,
                                        ifelse(score1 == "bogey",df.sim$Par + 1,
                                               ifelse(score1 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$score2 <- score2
  df.sim$round2 <- ifelse(score2 == "par", df.sim$Par,
                          ifelse(score2 == "birdie",df.sim$Par - 1,
                                 ifelse(score2 == "eagle",df.sim$Par - 2,
                                        ifelse(score2 == "bogey",df.sim$Par + 1,
                                               ifelse(score2 == "double", df.sim$Par + 2, df.sim$Par + 3)))))
  df.sim$temp <- df.sim$round1 + df.sim$round2
  df.sim$cumscore2 <- ave(df.sim$temp, 
                          df.sim$name, FUN = sum)
  df.sim$temp <- NULL #get rid of this variable
  # need to create the cut.  Here we will cut all players who are not
  # within 10 shots of the lead.
  cut <- df.sim[!duplicated(df.sim$name), ]
  cut$rank <- rank(cut$cumscore2, ties.method = "min")
  # the masters likes to cut players who aren't within 10 shots of the
  # leader and/or top 50.  Here we do that
  notcut <-
    cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
  cut <-
    cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
  dfcut <- df.sim[df.sim$name %in% cut$name, ]
  dfcut$rank <- NA
  cut$rank <- NA
  dfnotcut <- df.sim[df.sim$name %in% notcut$name, ]
  # simulate rounds 3 and 4
  probsnotcut <- dfnotcut[,c("P.triple", "P.double","P.bogey.",
                             "P.par.", "P.birdie","P.eagle")]
  score3 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  score4 <- apply(probsnotcut, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score3 <- score3
  dfnotcut$round3 <- ifelse(score3 == "par",
                            dfnotcut$Par,
                            ifelse(score3 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score3 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score3 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score3 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$score4 <- score4
  dfnotcut$round4 <- ifelse(score4 == "par",
                            dfnotcut$Par,
                            ifelse(score4 == "birdie",dfnotcut$Par - 1,
                                   ifelse(score4 == "eagle",dfnotcut$Par - 2,
                                          ifelse(score4 == "bogey",dfnotcut$Par + 1,
                                                 ifelse(score4 == "double", dfnotcut$Par + 2,dfnotcut$Par + 3)))))
  dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 +
    dfnotcut$round3 + dfnotcut$round4
  dfnotcut$cumscore4 <-ave(dfnotcut$temp, dfnotcut$name, FUN = sum)
  dfnotcut$temp <- NULL
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$cumscore4, ties.method = "min")
  dfnotcut <- merge(dfnotcut, round4[, c("name", "rank")],
                    by = "name", all.x = TRUE)
  cut$cumscore4 <- NA
  finish1 <-
    rbind(round4[, c("name", "rank", "cumscore2", "cumscore4")],
          cut[, c("name", "rank", "cumscore2", "cumscore4")])
  finish1$simulationID <- as.factor(i)
  finish <- rbind(finish, finish1)
  rm(finish1); rm(dfnotcut); rm(dfcut); rm(cut); rm(notcut)
  rm(probsnotcut); rm(round4); rm(score1); rm(score2); 
  rm(score3); rm(score4)
}))
# writing to file so I can remove large frame from RAM.
write.csv(finish, paste("~/Documents/masters/data/sims/finish", 
                        i, ".csv", sep = ""))
rm(list = ls())
###########################  END of SIMULATIONS ######################
# read and combine all the sims files
# combine all the simulation files:
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
df <- finish[, list(score = sum(cumscore2, na.rm = TRUE), 
                    freq = .N), by = c("name", "rank")]
write.csv(finish,"~/Documents/masters/data/simulationResults.csv")
rm(finish)
df$score <- NULL # not meaningful, get rid of it.
# careful not to run this without verifying the number of simulations.
df$prob <- df$freq/10000
# just for testing and evaluating.
# order the data table by rank at the top and most probable.
df <- df[order(df$rank, -rank(df$prob), decreasing = FALSE), ]
# now write to file :)
write.csv(df,"~/Documents/masters/data/probabilities.csv")
# we could also say most likely to finish top 10 by
# summing the probabilities if rank <= 10.....?
top10 <- df[df$rank <= 10, ]
probtop10 <- aggregate(prob ~ name, top10, sum)
rm(top10)
