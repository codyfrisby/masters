test <- df[df$holes == 36, ]
test2 <- test[test$round == 3 | test$round == 4, ]

test3 <- test2[, list(shots = sum(score), holes = .N),
               by = c("player", "year")]

test3$rounds <- test3$holes/18
summary(factor(test3$rounds))
# code appears to be working