
# load dataframe
dataframe <- read.csv("CDA_P2/HR_data.csv")

# remove rows with nans
dataframe <- na.omit(dataframe)

# extract column names
col_ <- colnames(dataframe)

# build X columns
X_cols <- c(col_[2:56], col_[58])

# build Y columns
y_cols <- c(col_[57], col_[59:68])

# extract X and Y
X <- dataframe[X_cols]
Y <- dataframe[y_cols]

# convert to numeric
#   Right now, we are not doing any one-hot encoding on cohort
X$Cohort <- as.numeric(as.factor(X$Cohort)) # should probably be one-hot encoded
X$Phase <- as.numeric(as.ordered(X$Phase))
X$Round <- as.numeric(as.ordered(X$Round))

# load library
library(CCA)
library(CCP)

# perform CCA
cc1 <- cc(X, Y)

# print correlation
cc1$cor

# print dimensions of ycoef and ycoef
dim(cc1$ycoef) 
dim(cc1$xcoef)

# define values for statistical test
rho <- cc1$cor
N <- 307
p <- 56
q <- 11
p.asym(rho, N, p, q)


# plot xcoef and ycoef
plot(c(1:56),cc1$xcoef[0:56,1], "l")
plot(c(1:11),cc1$ycoef[0:11,1], "l")

# print two lines in one
plot(c(1:56),cc1$xcoef[0:56,1], "l", c(1:11),cc1$ycoef[0:11,1], "l")



# sort xcoef and print column names
index_ = sort(abs(cc1$xcoef[0:56, 1]), index.return=TRUE)$ix



colnames(X)[index_]


# sort xcoef and print column names
index_ = sort(abs(cc1$ycoef[0:11, 1]), index.return=TRUE)$ix

colnames(Y)[index_]


plot(c(1:56),cc1$xcoef[0:56,2], "l")
plot(c(1:11),cc1$ycoef[0:11,2], "l")


sort(abs(cc1$xcoef[0:56, 1]), index.return=TRUE)$ix
sort(abs(cc1$xcoef[0:56, 2]), index.return=TRUE)$ix



sort(abs(cc1$ycoef[0:11, 1]), index.return=TRUE)$ix
sort(abs(cc1$ycoef[0:11, 2]), index.return=TRUE)$ix


plot(c(1:56),cc1$xcoef[0:56,3], "l")
plot(c(1:11),cc1$ycoef[0:11,3], "l")




# REGULARIZED
# perform CCA
rcc1 <- rcc(X, Y, 1e-5, 1e-5)


# define values for statistical test
rrho <- rcc1$cor
N <- 307
p <- 56
q <- 11
p.asym(rrho, N, p, q)


# plot xcoef and ycoef
plot(c(1:56),rcc1$xcoef[0:56,1], "l")
plot(c(1:11),rcc1$ycoef[0:11,1], "l")
