df <- read.csv("~/Documents/masters/data/dataforsimulation.csv")
col.names <- c("Player.Name", "Hole..", "Par", "P.triple.", "P.double.", 
               "P.bogey.", "P.par.", "P.birdie.", "P.eagle.")
players <- read.csv("~/Documents/masters/data/field2017.csv")
# so, 2016 doesn't include everyone...I'm going to try to include more
df <- df[order(df$Tournament.Year, -(rank(df$Round..)), decreasing = TRUE), ]
# limit to 1st round. Probabilities are the same for each round.
df <- df[df$Round.. == 1, ] # limit it to just one round, further subset df.
df <- df[df$Player.Name %in% players$Player.Name, ] # select those that are playing this year
# now to get the final simulation data frame.
df.sim <- df[!duplicated(df[c("Hole..", "Player.Name")]) | 
               !duplicated(df[c("Hole..", "Player.Name")], fromLast = FALSE),
             col.names]

#### side stuff.
masters <- summary(df$Player.Name)/18
# number of masters played up to and including 2016.
temp <- data.frame(Player.Name = names(masters), masters = as.vector(masters))
########### tests #################
# another test to see how many players aren't in our data set but in 2017 masters
m <- matrix(ncol = 2, nrow = length(players$Player.Name))
for(i in 1:length(players$Player.Name)){
    print(m[i,] <- unique(df.sim$Player.Name %in% players$Player.Name[i]))
}
# looks lie theres 18 players in the 2017 field that we don't have prior data for
check <- players$Player.Name[m[,1] == m[,2]]
# code appears to be working.
check # players who are in the 2017 field but NOT in our data set. 
