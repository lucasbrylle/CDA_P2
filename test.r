
test <- read.csv("CDA_P2/HR_data.csv")

test <- na.omit(test)

col_ <- colnames(test)

X_cols <- c(col_[0:56], col_[58])

y_cols <- c(col_[57], col_[59:68])

X <- test[X_cols]
Y <- test[y_cols]

X$Cohort <- as.numeric(as.factor(X$Cohort)) # should probably be one-hot encoded
X$Phase <- as.numeric(as.ordered(X$Phase))
X$Round <- as.numeric(as.ordered(X$Round))


cc1= cancor(X, Y)

# 
library(CCP)


rho <- cc1$cor

N <- 307

p <- 57

q <- 11

p.asym(rho, N, p, q)



