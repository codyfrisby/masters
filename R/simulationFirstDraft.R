########## start simulations##################
# first we need to read and clean up the data a little
df <- read.csv("~/Documents/masters/data/dataforsimulation.csv")
col.names <- c("Player.Name", "Hole..", "Par", "P.triple.", "P.double.", 
               "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")
df.2016 <- df[df$Tournament.Year == 2016 & df$Round.. == 1, col.names]
rm(df)
##### testing to see how all this works.  Verbose code for now.
# simulate rounds 1 and 2
score1 <- numeric(dim(df.2016)[1])
for(i in 1:dim(df.2016)[1]){
  score1[i] <- sample(c("triple", "double", "bogey", "par", "birdie", "eagle"), 
                      1, prob = as.vector(df.2016[i, c("P.triple.", "P.double.",
                                                       "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")]))
}
score2 <- numeric(dim(df.2016)[1])
for(i in 1:dim(df.2016)[1]){
  score2[i] <- sample(c("triple", "double", "bogey", "par", "birdie", "eagle"), 1, 
                      prob = as.vector(df.2016[i, c("P.triple.", "P.double.",
                                                    "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")]))
}
df.2016$score1 <- score1
df.2016$round1 <- ifelse(score1 == "par", df.2016$Par, 
                         ifelse(score1 == "birdie", df.2016$Par - 1,
                                ifelse(score1 == "eagle", df.2016$Par - 2, 
                                       ifelse(score1 == "bogey", df.2016$Par + 1, 
                                              ifelse(score1 == "double", df.2016$Par + 2,
                                                     df.2016$Par + 3)))))
df.2016$score2 <- score2
df.2016$round2 <- ifelse(score2 == "par", df.2016$Par, 
                         ifelse(score2 == "birdie", df.2016$Par - 1,
                                ifelse(score2 == "eagle", df.2016$Par - 2, 
                                       ifelse(score2 == "bogey", df.2016$Par + 1, 
                                              ifelse(score2 == "double", df.2016$Par + 2,
                                                     df.2016$Par + 3)))))
df.2016$temp <- df.2016$round1 + df.2016$round2
df.2016$cumscore2 <- ave(df.2016$temp, df.2016$Player.Name, FUN = sum)
df.2016$temp <- NULL #get rid of this variable, it has served its purpose
# need to create the cut.  Here we will cut all players who are not 
# within 10 shots of the lead.
cut <- df.2016[!duplicated(df.2016$Player.Name), ]
cut$rank <- rank(cut$cumscore2, ties.method = "min")
# the masters likes to cut players who aren't within 10 shots of the 
# leader and/or top 50.  Here we do that
notcut <- cut[cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50, ]
cut <- cut[!(cut$cumscore2 <= min(cut$cumscore2) + 10 | cut$rank <= 50), ]
dfcut <- df.2016[df.2016$Player.Name %in% cut$Player.Name, ]
dfcut$rank <- NA
cut$rank <- NA
dfnotcut <- df.2016[df.2016$Player.Name %in% notcut$Player.Name, ]
rm(df.2016) # no longer need this data frame either.
# simulate rounds 3 and 4
score3 <- numeric(dim(dfnotcut)[1])
for(i in 1:dim(dfnotcut)[1]){
  score3[i] <- sample(c("triple", "double", "bogey", "par", "birdie", "eagle"), 
                      1, prob = as.vector(dfnotcut[i, c("P.triple.", "P.double.",
                                                        "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")]))
}
score4 <- numeric(dim(dfnotcut)[1])
for(i in 1:dim(dfnotcut)[1]){
  score4[i] <- sample(c("triple", "double", "bogey", "par", "birdie", "eagle"), 1, 
                      prob = as.vector(dfnotcut[i, c("P.triple.", "P.double.",
                                                     "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")]))
}
dfnotcut$score3 <- score3
dfnotcut$round3 <- ifelse(score3 == "par", dfnotcut$Par, 
                          ifelse(score3 == "birdie", dfnotcut$Par - 1,
                                 ifelse(score3 == "eagle", dfnotcut$Par - 2, 
                                        ifelse(score3 == "bogey", dfnotcut$Par + 1, 
                                               ifelse(score3 == "double", dfnotcut$Par + 2,
                                                      dfnotcut$Par + 3)))))
dfnotcut$score4 <- score4
dfnotcut$round4 <- ifelse(score4 == "par", dfnotcut$Par, 
                          ifelse(score4 == "birdie", dfnotcut$Par - 1,
                                 ifelse(score4 == "eagle", dfnotcut$Par - 2, 
                                        ifelse(score4 == "bogey", dfnotcut$Par + 1, 
                                               ifelse(score4 == "double", dfnotcut$Par + 2,
                                                      dfnotcut$Par + 3)))))
dfnotcut$temp <- dfnotcut$round1 + dfnotcut$round2 + 
  dfnotcut$round3 + dfnotcut$round4
dfnotcut$cumscore4 <- ave(dfnotcut$temp, dfnotcut$Player.Name, FUN = sum)
dfnotcut$temp <- NULL
round4 <- dfnotcut[!duplicated(dfnotcut$Player.Name), ]
round4$rank <- rank(round4$cumscore4, ties.method = "min")
dfnotcut <- merge(dfnotcut, round4[, c("Player.Name", "rank")], 
                  by = "Player.Name", all.x = TRUE)
cut$cumscore4 <- NA
finish <- rbind(round4[, c("Player.Name", "rank", "cumscore2", "cumscore4")], 
                cut[, c("Player.Name", "rank", "cumscore2", "cumscore4")])
rm(cut); rm(notcut); rm(round4)
# just for fun:
finish[finish$Player.Name %in% "Els, Ernie", ]
