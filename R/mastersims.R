# Requirements for this function...
#... 4 vegasodds files from 2014 - 2017
#... "PPOM player probabilities - 2005 to 2017 SG april to april.csv" file
#... The above 5 files need to be in "~/(path to)/data" directory. 
sims <- function(year = 2005, n = 100){
  library(data.table)
  message("Please Wait.  Reading large file....")
  # first getting the data into the right shape
  if (file.exists(paste0("data/sims/finish", year, ".csv"))) 
      file.remove(paste0("data/sims/finish", year, ".csv"))
  temp <- read.csv("data/PPOM player probabilities - 2005 to 2017 SG april to april.csv")
  names <- names(temp)
  # renaming stuff
  names(temp) <- c(names[-(39:40)], "pwith", "pwithout")
  rm(names)
  temp <- temp[temp$year == year, ]
  # all these variables prevent us from turning the table
  # from "long" to "wide". So they will need to be NULL
  # remove the columns that cause a problem with tidyr::spread
  temp$pwithout <- NULL
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
  df.sim <- df.sim[, c(2, 3, 5, 6, 18:23, 26, 28, 29, 38:43)]
  names(df.sim) <- c("name", "hole", "round", "par", "sg.approach",
                     "sg.around", "sg.tee", "sg.putt", "sg.total",
                     "score", "rank", "owgr.April", "owgr.Feb",
                     "P.triple", "P.double", "P.bogey", 
                     "P.par", "P.birdie", "P.eagle")
  finish_names <- df.sim[, c(1, 5:13)]
  finish_names <- finish_names[!duplicated(finish_names$name), ]
  finish_names$actualrank <- rank(finish_names$rank, ties.method = "min")
  finish_names <- data.table(finish_names)
  df.sim <- df.sim[, -(5:13)]
  rm(temp) # done with this now
  # create 2 matrices with the probs for each hole for rounds 1 and 2
  probs1 <- df.sim[df.sim$round == 1, 
                   c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
  probs2 <- df.sim[df.sim$round == 2, 
                   c("P.triple", "P.double", "P.bogey", "P.par", "P.birdie", "P.eagle")]
  topar <- c(3, 2, 1, 0, -1, -2)
  ########################### START of  SIMULATIONS ##################
  # here is the beginning of the loop.  Each loop is one masters.
  message("....Done reading file.  Running simulation Now ....")
  for(i in 1:n) {
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
    write.table(finish1, paste0("data/sims/finish", year, ".csv"), append = TRUE, sep = ",",
                row.names = FALSE, col.names = FALSE)
    rm(finish1); rm(dfnotcut); rm(cut); rm(notcut)
    rm(probs3); rm(probs4); rm(round4); rm(score1); rm(score2) 
    rm(score3); rm(score4)
  }
  ######## END of Simulation ###############
  # now run some tests.  
  vars <- c("name", "place", "score", "simID", "playoff")
  finish <- read.csv(paste0("data/sims/finish", year, ".csv"), header = F)
  colnames(finish) <- vars
  finish$place <- NULL
  finish <- data.table(finish)
  df <- finish[, list(firstplace = .N), by = "name"]
  playoffs <- finish$playoff
  rm(finish)
  df$modranks <- rank(-df$firstplace, ties.method = "min")
  df$prob <- df$firstplace/n
  df$firstplace <- NULL
  df <- merge(df, finish_names, by.x = "name", by.y = "name",
              all.y = TRUE)
  df$prob <- ifelse(is.na(df$modranks), 0, df$prob)
  df$modranks <- ifelse(is.na(df$modranks), 
                        max(df$modranks, na.rm = TRUE) + 1, 
                        df$modranks)
  df <- df[order(df$modranks), ]
  # save these results
  write.csv(df, paste0("data/modelResults/", gsub(":", ".", Sys.time()), 
                      ".csv"), row.names = FALSE) 
  message("Here are the results:")
  if(sum(df$modranks == 1) > 1){
    message(paste("There is a", sum(df$modranks == 1), "way tie for first"))
  } else (message(paste("And the winner is: ", df$name[1])))
  message(paste("With a probability of", df$prob[1]))
  message(paste(sum(playoffs),"playoffs,", "or", 
                sum(playoffs) / n, "probability of a playoff"))
  message(paste("Agreement between MODEL and REALITY:", 
                round(cor(df$modranks, df$actualrank, 
                          method = "spearman", use = "complete"), 4)))
  ## only for years 2014 - 2017 ##
  if(year == 2014 | year == 2015 | year == 2016 | year == 2017){
  # we could compare us to vegas and actual results here:
  vegas <- read.csv("data/vegasranks.csv")
  vegas <- vegas[vegas$year == year, ]
  vegas <- data.table(vegas)
  vegas$year <- NULL
  test <- merge(df, vegas, by.x = "name", by.y = "name", all.x = TRUE)
  message(paste("Agreement between MODEL and VEGAS:", 
                round(cor(test$modranks, test$vegasranks, 
                          method = "spearman", use = "complete"), 4)))
  } else(test <- df)
  test <- test[order(test$modranks), ]
  return(test)
}
