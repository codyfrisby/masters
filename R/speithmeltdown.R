# Speith meltdown, 2016 masters.
if (file.exists("data/sims/speithmeltdown.csv")) file.remove("data/sims/speithmeltdown.csv")
n <- 10000
temp <- read.csv("data/Speith 2016 meltdown.csv")
temp <- temp[temp$player != "Johnson, Zach", ]
#########  NOTES of Sim #################
# where are we starting sim?
# pairings: 
#2:15 PM -- Danny Willett, Lee Westwood
#2:25 PM -- Jason Day, Dustin Johnson
#2:35 PM -- Bernhard Langer, Hideki Matsuyama
#2:45 PM -- Jordan Spieth, Smylie Kaufman
#starting points
# player score hole
#Speith -7 or 245 10
#Kaufman +1 or253 10
#Matsuyama +2 or 254 10 (He was certainly off the tee on 10 but we can’t start him in the middle of the hole)
#Johnson -1 or 255 11
#Day E or 256 11
#Willett -2 or 258 12
#Westwood E or 260 12
#Kjeldsen -1 or 262 13
########################################
# first get everyone's scores correct through 3.5 rounds
names <- names(temp)
# renaming stuff
names(temp) <- c(names[-(39:40)], "pwith", "pwithout")
rm(names)
# all these variables prevent us from turning the table
# from "long" to "wide". So they will need to be NULL
# remove the columns that cause a problem with tidyr::spread
temp$pwithout <- NULL
df <- tidyr::spread(temp, Level, pwith)
### calculate the probs for the individual score on the hole
df$P.triple <- ifelse(is.na(df$`3`), 0, df$`3`)
df$P.double <- ifelse(is.na(df$`3`), df$`2`, df$`2` - df$`3`)
df$P.bogey <- df$`1` - df$`2`
df$P.par <- df$`0` - df$`1`
df$P.birdie <- ifelse(is.na(df$`-1`), 1 - df$`0`, df$`-1` - df$`0`)
df$P.eagle <- ifelse(!is.na(df$`-1`), 1 - df$`-1`, 0)
# get rid of some columns we don't need anymore
df[, c("-1", "0", "1", "2", "3")] <- NULL
# players score through 3.5 rounds
df.sim <- df
df <- data.table::data.table(df)
score1 <- df[!(df$Hole.. > 9 & df$Round.. == 4), list(score63 = sum(Score), 
                                   through = .N), by = "player"]
score63 <- as.data.frame(score1); rm(score1)
rm(df)
df.sim <- df.sim[, c(2, 3, 5, 6, 38:43)]
names(df.sim) <- c("name", "hole", "round", "par", "P.triple", 
                   "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")
# simulate back 9 round 4 only
df.sim <- df.sim[df.sim$hole > 9 & df.sim$round == 4, ]
# have to change the probs for those that were past hole 10
df.sim[df.sim$name == "Day, Jason" & df.sim$hole == 10, 
       c("P.triple", "P.double", "P.bogey", 
        "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Johnson, Dustin" & df.sim$hole == 10, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Willett, Danny" & df.sim$hole == 10, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Willett, Danny" & df.sim$hole == 11, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Westwood, Lee" & df.sim$hole == 10, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Westwood, Lee" & df.sim$hole == 11, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 1, 0, 0, 0)
df.sim[df.sim$name == "Kjeldsen, Soren" & df.sim$hole == 10, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Kjeldsen, Soren" & df.sim$hole == 11, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
df.sim[df.sim$name == "Kjeldsen, Soren" & df.sim$hole == 12, 
       c("P.triple", "P.double", "P.bogey", 
         "P.par", "P.birdie", "P.eagle")] <- c(0, 0, 0, 1, 0, 0)
####### done fixing the probabilities for those players not on 10.
rm(temp) # done with this now
# create 2 matrices with the probs for each hole for rounds 1 and 2
topar <- c(3, 2, 1, 0, -1, -2)
########################### START of  SIMULATIONS ##################
# here is the beginning of the loop.  Each loop is one masters.
probs4 <- df.sim[, c("P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")]
for(i in 1:n) {
  # simulate round 4 back nine
  score4 <- apply(probs4, 1, function(x)
    sample(topar, 1, prob = x))
  df.sim$score1 <- score4 + df.sim$par
  # summarising the next two rounds
  df.sim$sunday <- ave(df.sim$score1, df.sim$name,
                         FUN = function(x) sum(x, na.rm = TRUE))
  round4 <- df.sim[!duplicated(df.sim$name), ]
  round4 <- merge(round4, score63, by.x = "name", by.y = "player")
  round4$sunday <- round4$sunday + round4$score63
  round4$rank <- rank(round4$sunday, ties.method = "min")
  finish1 <- round4[, c("name", "rank", "sunday")]
  finish1 <- finish1[finish1$rank == 1, ]
  finish1$simulationID <- as.factor(i)
  #####  This is the PLAYOFF  #########
  finish1$playoff <- ifelse(dim(finish1)[1] != 1, TRUE, FALSE)
  while (dim(finish1)[1] != 1){
    playoff <- df.sim[df.sim$name %in% finish1$name, ]
    playoff18 <- playoff[playoff$hole == 18 & playoff$round == 4, ]
    probs18 <- playoff18[, c("P.triple", "P.double", "P.bogey", 
                             "P.par", "P.birdie", "P.eagle")]
    finish1$playoff18 <- apply(probs18, 1, function(x) 
      sample(topar, 1, prob = x))
    finish1 <- finish1[which(finish1$playoff18 == min(finish1$playoff18)), ]
    finish1$playoff18 <- NULL
    if (dim(finish1)[1] != 1){
      playoff <- df.sim[df.sim$name %in% finish1$name, ]
      playoff10 <- playoff[playoff$hole == 10 & playoff$round == 4, ]
      probs10 <- playoff10[, c("P.triple", "P.double", "P.bogey", 
                               "P.par", "P.birdie", "P.eagle")]
      finish1$playoff10 <- apply(probs10, 1, function(x) 
        sample(topar, 1, prob = x))
      finish1 <- finish1[which(finish1$playoff10 == min(finish1$playoff10)), ]
      finish1$playoff10 <- NULL
    }
  }
  ### end playoff ###
  write.table(finish1, "data/sims/speithmeltdown.csv", append = TRUE,
              row.names = FALSE, col.names = FALSE)
  rm(finish1)
}
######## END of Simulation ###############

# now run some tests.  
vars <- c("name", "place", "score", "simID", "playoff")
finish <- read.table("data/sims/speithmeltdown.csv", header = F)
colnames(finish) <- vars
finish$place <- NULL
library(data.table)
finish <- data.table(finish)
df <- finish[, list(firstplace = .N), by = "name"]
playoffs <- finish$playoff
rm(finish)
df$modranks <- rank(-df$firstplace, ties.method = "min")
df$prob <- df$firstplace/n
df$firstplace <- NULL
score63[, 2:3] <- NULL
df <- merge(df, score63, by.x = "name", by.y = "player", 
            all.y = TRUE)
df$prob <- ifelse(is.na(df$modranks), 0, df$prob)
df$modranks <- ifelse(is.na(df$modranks), 
                      max(df$modranks, na.rm = TRUE) + 1, 
                      df$modranks)
df <- df[order(df$modranks), ]
View(df)
write.csv(df, paste0("data/modelResults/", gsub(":", ".", Sys.time()), 
                     ".csv"), row.names = FALSE) 
message("Here are the results:")
if(sum(df$modranks == 1) > 1){
  message(paste("There is a", sum(df$modranks == 1), "way tie for first"))
} else (message(paste("And the winner is: ", df$name[1])))
message(paste("With a probability of", df$prob[1]))
message(paste(sum(playoffs),"playoffs,", "or", 
              sum(playoffs) / n, "probability of a playoff"))
