## pain in the ass to get the 2017 Masters data from the internet
# I copied that data from http://www.augusta.com/masters/leaderboard 
# called it masters2017

masters <- read.table("data/masters2017", header = TRUE, fill = TRUE)
keep <- c("firstname", "lastname", "TOTAL", "rank")
temp <- masters[54:93, ]
masters <- masters[1:53, ]
temp$R2 <- as.numeric(temp$R1)
temp$WINNINGS <- as.numeric(temp$R1)
temp$WINNINGS <- "$0"
temp$TOTAL <- as.numeric(temp$R4)
temp$R3 <- NA
temp$R4 <- NA
temp$TOTAL <- temp$R1 + temp$R2
temp$rank <- rank(temp$TOTAL, ties.method = "min") + 54
masters$rank <- rank(masters$TOTAL, ties.method = "min")
masters$rank[1:2] <- c(1, 2) # Sergio beat Justin
temp <- temp[, keep]
### merge the two
masters <- masters[, keep]
masters <- rbind(masters, temp); rm(temp)
masters$name <- paste0(masters$lastname, ", ", masters$firstname)
masters <- masters[, c("name", "rank", "TOTAL")]
names(masters) <- c("name", "rank", "score")
write.csv(masters, "~/Documents/masters/data/masters2017finish.csv", row.names = F)
# I then had fix a few of the names that don't match the 
# vegasodds2017.csv file

