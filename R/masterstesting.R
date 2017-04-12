# fixing thursday scores:
# copy and paste from URL: http://www.sportingnews.com/golf/news/the-masters-2017-live-leaderboard-first-round-april-6-dustin-johnson-jordan-spieth-rory-mcilroy-tiger-woods/pcvrjxkt1ydh1nqp0eq9bsqlo
thur <- read.table("~/Documents/masters/data/roundone2017", 
                       header = TRUE)
## names I had to alter:
# Billy Hurley III
# Si Woo Kim
# Byeong Hun An
# Jose Maria Olazaba

thur$name <- paste0(thur$last, ", ", thur$first)
thur$first <- NULL; thur$last <- NULL
write.csv(thur, "data/thur.csv", row.names = FALSE)
rm(list = ls())
# get my current names and then fix the names so they match
# running from "day2.R" we get the names.
name <- unique(df.sim$name)

thur <- read.csv("data/thur.csv")
thur$topar <- NULL
test <- merge(df.sim, thur)

fri <- read.table("~/Documents/masters/data/roundtwo", 
                  header = TRUE)
fri$name <- paste0(fri$last, ", ", fri$first)
fri$rank <- NULL; fri$first <- NULL; fri$last <- NULL
fri$teetim <- NULL
# to par after two rounds at Augusta is 72 * 2 = 144
fri$friday <- fri$topar + 144



name[!(name %in% fri$name)]
# missing a few names.... will need to fix.
fri$name[35] <- "An, Byeong Hun"
fri$topar <- NULL
# the other 4, Couples, Hagestad, Luck, Mize will not be simulated... we do not have data for them :(
write.csv(fri, "data/fri.csv", row.names = F)

sat <- read.table("~/Documents/masters/data/roundthree", 
                  header = FALSE)
sat$startsun <- sat$V4 + 72 * 3
sat$V1 <- NULL; sat$V5 <- NULL
sat$name <- paste0(sat$V3, ", ", sat$V2)
sat$V2 <- NULL; sat$V3 <- NULL; sat$V4 <- NULL
# missing a few names.... will need to fix.
sat$name[40] <- "An, Byeong Hun"
sat$topar <- NULL
sat[!(sat$name %in% fri$name), ]


# the other 4, Couples, Hagestad, Luck, Mize will not be simulated... we do not have data for them :(
write.csv(sat, "data/sat.csv", row.names = F)
