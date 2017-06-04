## make some plots using the masters file
masters <- read.csv("~/Documents/masters/allyears.csv")
masters$rank <- NULL
# fix some of the ties for first
masters[masters$name == "DiMarco, Chris" & masters$year == 2005, "actualrank"] <- 2
masters[masters$name %in% c("Perry, Kenny", "Campbell, Chad") & masters$year == 2009, "actualrank"] <- 2
masters[masters$name == "Oosthuizen, Louis" & masters$year == 2012, "actualrank"] <- 2
masters[masters$name == "Cabrera, Angel" & masters$year == 2013, "actualrank"] <- 2
masters$winner <- factor(ifelse(masters$actualrank == 1, 1, 0))
# change name to first last for publication purposes
x <- as.character(masters$name)
l <- strsplit(x, ", ")
firstname <- sapply(l, "[", 2)
lastname <- sapply(l, "[", 1)
masters$name <- paste(firstname, lastname)
# make some plots ussing ggplot2
library(ggplot2)
#######
# Tiger compared to Phil plot
tigerphil <- masters[masters$name %in% 
                       c("Tiger Woods", "Phil Mickelson"), ]
gg <- ggplot(data = tigerphil, aes(x = year, y = prob))
gg <- gg + geom_point(aes(shape = name, color = name), size = 4) 
gg <- gg + geom_smooth(aes(group=name, linetype = name, color = name),
                       se = FALSE) + theme_bw()
gg <- gg + scale_x_continuous(breaks = 2005:2017) + 
  ylab("Modeled Probability of Winning") + theme(legend.title = element_blank())
gg
# more bivariate plots, label outliers too
library(dplyr)
library(ggrepel)
library(reshape)
## function to identify univariate outliers
is_outlier <- function(x) {
  return(x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
}
#######
# OWGR vs prob
df <- masters[, c("name", "year", "owgr.April", "prob", "winner")]
df$is_outlier <- ifelse(df$owgr.April >= 15 | df$winner == 1, 1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = owgr.April, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + 
  theme_bw() + ylab("Predicted Probability of Winning") + 
  xlab("World Golf Ranking in April") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(3, 5)) + 
  scale_alpha_manual(values = c(0.35, 1)) + 
  theme(legend.position = "none")
gg
ggplotly(gg)
###### 
#SG total vs prob
df <- masters[, c("name", "year", "sg.total", "prob", "winner")]
df$is_outlier <- ifelse(df$prob >= 0.3 | df$winner == 1, 1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = sg.total, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + theme_bw() + 
  ylab("Predicted Probability of Winning") + xlab("Strokes Gained Total") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(1, 3)) + 
  scale_alpha_manual(values = c(0.5, 1)) + 
  theme(legend.position = "none")
gg

####### 
#sgtee vs prob
df <- masters[, c("name", "year", "sg.tee", "prob", "winner")]
df$is_outlier <- ifelse(df$prob >= 0.3 | df$winner == 1 | df$sg.tee < -1.5, 
                        1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = sg.tee, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + theme_bw() + 
  ylab("Predicted Probability of Winning") + xlab("Strokes Gained Tee") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(2, 4)) + 
  scale_alpha_manual(values = c(0.35, 1)) + 
  theme(legend.position = "none")
gg
########
# sgputt vs prob
df <- masters[, c("name", "year", "sg.putt", "prob", "winner")]
df$is_outlier <- ifelse(df$prob >= 0.3 | df$winner == 1 | df$sg.putt >1.5,
                        1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = sg.putt, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + theme_bw() + 
  ylab("Predicted Probability of Winning") + xlab("Strokes Gained Putting") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(2, 4)) + 
  scale_alpha_manual(values = c(0.35, 1)) + 
  theme(legend.position = "none")
gg
#########
# sgaround vs prob
df <- masters[, c("name", "year", "sg.around", "prob", "winner")]
df$is_outlier <- ifelse(df$prob >= 0.3 | df$winner == 1 | df$sg.around <= -1,
                        1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = sg.around, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + theme_bw() + 
  ylab("Predicted Probability of Winning") + xlab("Strokes Gained Around") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(2, 4)) + 
  scale_alpha_manual(values = c(0.35, 1)) + 
  theme(legend.position = "none")
gg
###########
# sgapproach vs prob
df <- masters[, c("name", "year", "sg.approach", "prob", "winner")]
df$is_outlier <- ifelse(df$prob >= 0.3 | df$winner == 1 | abs(df$sg.approach) >= 1.5,
                        1, NA)
# so the year is displayed
df$name <- paste(df$name, df$year)
df$name[which(is.na(df$is_outlier))] <- as.numeric(NA)
gg <- ggplot(data = df, aes(x = sg.approach, y = prob))
gg <- gg + geom_point(aes(color = winner, shape = winner, 
                          size = winner, alpha = winner)) + theme_bw() + 
  ylab("Predicted Probability of Winning") + xlab("Strokes Gained Approach") + 
  geom_text_repel(aes(label = name), na.rm = TRUE, 
                  size = 2, segment.alpha = 0.5, 
                  segment.size = 0.5) + 
  geom_smooth(se = FALSE, color = "red", alpha = 0.1, size = 0.5,
              lty = 3) + 
  scale_size_manual(values = c(2, 4)) + 
  scale_alpha_manual(values = c(0.35, 1)) + 
  theme(legend.position = "none")
gg

#######
# code to prepare data for univariate boxplots, 2017
sg <- masters[masters$year == 2017, c(1, 4:8, 11)]
names(sg) <- c("name", "Approach", "Around", "Tee", "Putting",
               "Total", "OWGR_April")
# remove a few of the players that didn't play very much
sg <- sg[!(sg$name %in% c("Tommy Fleetwood", "Ross Fisher", "Yuta Ikeda")), ]
# code to label univariate outliers
df_melt <- reshape::melt(sg)
df_melt <- df_melt %>%
  group_by(variable) %>%
  mutate(is_outlier = ifelse(is_outlier(value), value, as.numeric(NA)))
df_melt$name[which(is.na(df_melt$is_outlier))] <- as.numeric(NA)

############
## code for univariate boxplots
gb <- ggplot(df_melt, aes(x = variable, y = value)) + 
  geom_boxplot(outlier.size = 1, outlier.alpha = 0.35, 
               outlier.colour = "red")
gb <- gb + facet_wrap(~variable, scales = "free", nrow = 2) + 
   theme_bw() + theme(strip.text.x = element_text(size = 7),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
gb <- gb + geom_text_repel(aes(label = name), na.rm = TRUE, 
                           size = 2, segment.alpha = 0.5, 
                           segment.size = 0.5)
gb <- gb + xlab("") + ylab("")
gb

## historgrams instead?
gh <- ggplot(df_melt, aes(x = value)) + 
  geom_histogram(aes(y = ..density..), binwidth = 0.25) + 
  theme_bw() + geom_density()
gh <- gh + facet_wrap(~variable, scales = "free", nrow = 2)
gh
#######
#another boxplot
# SG only variables:
df_sg <- df_melt[df_melt$variable != "OWGR_April", ]
gb <- ggplot(df_sg, aes(x = variable, y = value)) + 
  geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.35, 
               outlier.colour = "red")
gb <- gb + #facet_wrap(~variable, scales = "free", nrow = 2) + 
  theme_bw() + ggtitle("Strokes Gained, 2017") + xlab("") + ylab("")
gb <- gb + geom_text_repel(aes(label = name), na.rm = TRUE, 
                           size = 2.5, segment.alpha = 0.5, 
                           segment.size = 0.5)
gb
#######
## OWGR boxplot
df_owgr <- df_melt[df_melt$variable == "OWGR_April", ]
DJ <- c("Dustin Johnson (did not play)", "OWGR_April", 12.9, 12.9)
df_owgr[78, ] <- DJ
df_owgr$variable <- as.factor(df_owgr$variable)
df_owgr$value <- as.numeric(df_owgr$value); df_owgr$is_outlier <- as.numeric(df_owgr$is_outlier)
go <- ggplot(df_owgr, aes(x = variable, y = value)) + 
  geom_boxplot(outlier.size = 1.5, outlier.alpha = 0.35, 
               outlier.colour = "red")
go <- go + theme_bw() + theme(strip.text.x = element_text(size = 7),
                     axis.title.x = element_blank(),
                     axis.text.x = element_blank(),
                     axis.ticks.x = element_blank())
go <- go + geom_text_repel(aes(label = name), na.rm = TRUE, 
                           size = 2.5, segment.alpha = 0.5, 
                           segment.size = 0.5)
go <- go + ggtitle("Official World Golf Ranking Points, April") + ylab("") +
  xlab("")
go

################################
# make some plots using the huge main file. 
## histograms of score for hole 2 and hole 18
# first getting the data ready:
temp <- read.csv("data/PPOM player probabilities - 2005 to 2017 SG april to april.csv")
temp <- temp[temp$Hole.. == 2 | temp$Hole.. == 18, ]
names(temp) <- c(names(temp)[-(39:40)], "pwith", "pwithout")
temp <- temp[,c("year", "player", "Hole..", "Round..", "Par", 
                "Score", "RTP.Score", "Level", "pwith")]
df <- tidyr::spread(temp, Level, pwith)
df <- df[, 1:7]
df$Hole.. <- as.factor(df$Hole..)
names(df)[3] <- "hole"
# group scores greater than 3.
df$score <- ifelse(df$RTP.Score >= 3, 3, df$RTP.Score)
# using ggplot
library(ggplot2)
######
# Histogram of score for holes 2 and 18
df <- df[df$score != -3, ] # removed the single double eagle.
gg <- ggplot(data = df, aes(x = score, group = hole, fill = hole)) + 
  geom_bar(aes(y = ..prop..), position = "dodge") + theme_bw()
gg <- gg + xlab("Score to Par") + ylab("Proportion") + 
  scale_x_discrete(limit = -2:3, # custom labels: 
                   labels = c("-2", "-1", "0", "1", "2", "3+"))
gg
#geom_histogram(aes(y=..count../sum(..count..)))

######
# what if instead we plotted year by score with hole overlay?
gg <- ggplot(data = df, aes(x = year, y = score)) + 
  geom_jitter(aes(shape = hole, color = hole), size = 1, alpha = 0.2) +
  geom_smooth(aes(group = hole, color = hole), se = FALSE) + 
  scale_x_continuous(breaks = 2005:2017) + ylab("Score to Par") +
  theme_bw()
gg

# can I label any of the outliers?
library(dplyr)
library(ggrepel)
library(reshape)
## function to identify possible outliers

is_outlier <- function(x) {
  return(x < -2 | x > 2)
}
df_out <- df[!is.na(df$RTP.Score), ]
df_out <- df_out %>%
  group_by(year) %>%
  mutate(is_outlier = ifelse(is_outlier(RTP.Score), RTP.Score, as.numeric(NA)))
df_out$player[which(is.na(df_out$is_outlier))] <- as.numeric(NA)

#######
# scatter plot with "outliers".  
gg <- ggplot(data = df_out, aes(x = year, y = score)) + 
  geom_jitter(aes(shape = hole, color = hole), size = 1, alpha = 0.4) +
  geom_smooth(aes(group = hole, color = hole), se = FALSE) + 
  scale_x_continuous(breaks = 2005:2017) + ylab("Score to Par") +
  theme_bw() + 
  geom_text_repel(aes(label = player), na.rm = TRUE, 
                               size = 2, segment.alpha = 0.5, 
                               segment.size = 0.5)
gg

#######
# plot to illustrate hole position possiblly....
# first getting the data ready:
temp <- read.csv("data/PPOM player probabilities - 2005 to 2017 SG april to april.csv")
temp <- temp[temp$Hole.. == 16 | temp$Hole.. == 12, ]
names(temp) <- c(names(temp)[-(39:40)], "pwith", "pwithout")
temp <- temp[,c("year", "player", "Hole..", "Round..", "Par", 
                "Score", "RTP.Score", "Level", "pwith")]
df <- tidyr::spread(temp, Level, pwith)
df <- df[, 1:7]; rm(temp)
df$Hole.. <- as.factor(df$Hole..)
df$Round.. <- as.factor(df$Round..)
df$score <- ifelse(df$RTP.Score >= 3, 3, df$RTP.Score)
names(df)[3:4] <- c("hole", "round")
# remove NAs
df <- df[!is.na(df$RTP.Score), ]
df16 <- df[df$hole == 16, ]
gg <- ggplot(data = df16, aes(x = year, y = score)) + 
  geom_smooth(aes(group = round, color = round, linetype = round), se = FALSE) + 
  scale_x_continuous(breaks = 2005:2017) + ylab("Score to Par") + theme_bw() + 
  ggtitle("Hole 16")
gg

df12 <- df[df$hole == 12, ]
gg <- ggplot(data = df12, aes(x = year, y = score)) + 
  geom_smooth(aes(group = round, color = round, linetype = round), se = FALSE) + 
  scale_x_continuous(breaks = 2005:2017) + ylab("Score to Par") + theme_bw() + 
  ggtitle("Hole 12")
gg
