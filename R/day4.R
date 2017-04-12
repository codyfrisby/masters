# first getting the data into the right shape
# then make sure your prob file is in data/ directory
if (file.exists("data/sims/finish2017.csv")) file.remove("data/sims/finish2017.csv")
# have to point temp to the file that has our probabilities
temp <- read.csv("data/Player probabilities - PPOM 2017 test.csv")
names <- names(temp)
# renaming stuff
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
df.sim <- df.sim[, c("name", "hole", "round", "par", 
                     "P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")]
df.sim <- df.sim[df.sim$name != "Johnson, Dustin", ]
sat <- read.csv("data/sat.csv")
rm(temp) # done with this now
dfnotcut <- df.sim[df.sim$name %in% sat$name, ]
dfnotcut <- merge(dfnotcut, sat)
probs3 <- dfnotcut[dfnotcut$round == 3, 
                   c("P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")]
probs4 <- dfnotcut[dfnotcut$round == 4, 
                   c("P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")]
########################### START of  SIMULATIONS ##################
# here is the beginning of the loop.  Each loop is one masters.
n <- 10000
topar <- c(3, 2, 1, 0, -1, -2)
# 1 - 10000
for(i in 1:n) {
  # simulate rounds 4
  score4 <- apply(probs4, 1, function(x)
    sample(topar, 1, prob = x))
  dfnotcut$score1[dfnotcut$round == 4] <- score4 + dfnotcut$par[dfnotcut$round == 4]
  # summarising the next two rounds
  dfnotcut$sunday <- ave(dfnotcut$score1, dfnotcut$name,
                         FUN = function(x) sum(x, na.rm = TRUE))
  dfnotcut$sunday <- dfnotcut$sunday + dfnotcut$startsun
  round4 <- dfnotcut[!duplicated(dfnotcut$name), ]
  round4$rank <- rank(round4$sunday, ties.method = "min")
  finish1 <- round4[, c("name", "rank", "sunday")]
  finish1 <- finish1[finish1$rank == 1, ]
  finish1$simulationID <- as.factor(i)
  #####  This is the PLAYOFF  #########
  finish1$playoff <- ifelse(dim(finish1)[1] != 1, TRUE, FALSE)
  while (dim(finish1)[1] != 1){
    playoff <- dfnotcut[dfnotcut$name %in% finish1$name, ]
    playoff18 <- playoff[playoff$hole == 18 & playoff$round == 4, ]
    probs18 <- playoff18[, c("P.triple", "P.double", "P.bogey", 
                             "P.par", "P.birdie", "P.eagle")]
    finish1$playoff18 <- apply(probs18, 1, function(x) 
      sample(topar, 1, prob = x))
    finish1 <- finish1[which(finish1$playoff18 == min(finish1$playoff18)), ]
    finish1$playoff18 <- NULL
    if (dim(finish1)[1] != 1){
      playoff <- dfnotcut[dfnotcut$name %in% finish1$name, ]
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
  write.table(finish1, "data/sims/finish2017.csv", append = TRUE, sep = ",",
              row.names = FALSE, col.names = FALSE)
  rm(finish1); rm(round4); rm(score4)
}
######## END of Simulation ###############
# now run some tests.  
rm(list = ls())
vars <- c("name", "place", "score", "simID", "playoff")
finish <- read.csv("data/sims/finish2017.csv", header = F)
colnames(finish) <- vars; rm(vars)
finish$place <- NULL
library(data.table)
finish <- data.table(finish)
n <- max(finish$simID)
df <- finish[, list(firstplace = .N), by = "name"]
playoffs <- finish$playoff
rm(finish)
df$modranks <- rank(-df$firstplace, ties.method = "min")
df$prob <- df$firstplace/n
df$firstplace <- NULL
df$mododds <- paste0(round((1 - df$prob) / df$prob, 1), ":", 1)
# save these results
df <- df[order(df$modranks), ]
write.csv(df, paste("data/modelResults/round4", gsub(":", ".", Sys.time()), 
                    ".csv", sep = ""), row.names = FALSE)
message("Here are the results:")
message(paste(sum(playoffs),"playoffs,", "or", 
              sum(playoffs) / n, "probability of a playoff"))
winner <- df$name[1]
winner <- sub(".*, ", "", winner)
message(paste("And the winner is", winner))
