## comparison between updated vegas after round two and us:

df <- read.csv("data/modelResults/noDJ2017-04-08 08.20.35.csv")
df <- df[order(df$modranks), ]
vegas <- read.table("data/vegasroundtwo", header = F)
vegas$name <- paste(vegas$V2, ", ", vegas$V1, sep = "")
vegas$V1 <- NULL; vegas$V2 <- NULL
names(vegas) <- c("vegasodds", "name")

test <- merge(df, vegas)
# we are dropping:
dim(df)[1] - dim(test)[1]
df[!(df$name %in% vegas$name), ]

x <- test$vegasodds

test$vegasranks <-rank(as.numeric(sub(":.", "", x)) / as.numeric(sub(".*:", "", x)),
     ties.method = "min")

print((test <- test[, c("name", "modranks", "vegasranks", "mododds",
                        "vegasodds")]))
message(paste("Spearman's corr after round 2 =", 
              round(cor(test$modranks, test$vegasranks, method = "spearman"), 4)))
message(paste("n = "), dim(test)[1])
