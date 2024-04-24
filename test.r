
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



install.packages("ggpubr")
library(ggplot2)


v1 <- data.frame(x_feature = c(1:56), coefficient=cc1$xcoef[0:56,1], component='v1')
v2 <- data.frame(x_feature = c(1:56), coefficient=cc1$xcoef[0:56,2], component='v2')
v3 <- data.frame(x_feature = c(1:56), coefficient=cc1$xcoef[0:56,3], component='v3')


col_ = colnames(X)
ind = abs(v1$coefficient) < 100
col_[ind] = ''

test1 <- ggplot(data = v1, aes(x=x_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=x_feature,xend=x_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=x_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

col_ = colnames(X)
ind = abs(v2$coefficient) < 100
col_[ind] = ''

test2 <- ggplot(data = v2, aes(x=x_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=x_feature,xend=x_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=x_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

col_ = colnames(X)
ind = abs(v3$coefficient) < 100
col_[ind] = ''

test3 <- ggplot(data = v3, aes(x=x_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=x_feature,xend=x_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=x_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

library(ggpubr)
ggarrange(test1, test2, test3, labels=c('v1', 'v2', 'v3'),ncol=1, nrow=3,
font.label = list(size = 40, color = "black", face = "bold"))

ggsave("CDA_P2/v_plot.png",width=7, height=12, dpi=300)



u1 <- data.frame(y_feature = c(1:11), coefficient=cc1$ycoef[0:11,1], component='u1')
u2 <- data.frame(y_feature = c(1:11), coefficient=cc1$ycoef[0:11,2], component='u2')
u3 <- data.frame(y_feature = c(1:11), coefficient=cc1$ycoef[0:11,3], component='u3')


col_ = colnames(Y)
#ind = abs(u1$coefficient) < 10
#col_[ind] = ''

test1 <- ggplot(data = u1, aes(x=y_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=y_feature,xend=y_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=y_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

col_ = colnames(Y)
#ind = abs(u2$coefficient) < 100
#col_[ind] = ''

test2 <- ggplot(data = u2, aes(x=y_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=y_feature,xend=y_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=y_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

col_ = colnames(Y)
#ind = abs(u3$coefficient) < 100
#col_[ind] = ''

test3 <- ggplot(data = u3, aes(x=y_feature, y=coefficient)) +
    geom_point(size=4) +
    geom_segment(aes(x=y_feature,xend=y_feature,y=0,yend=coefficient), size=2, alpha=0.5) +
    theme(text = element_text(size = 20),
        legend.text = element_text(size=20)) +
        geom_text(aes(x=y_feature, y=coefficient, label=col_), fontface='bold', 
        color='red')

library(ggpubr)
ggarrange(test1, test2, test3, labels=c('u1', 'u2', 'u3'),ncol=1, nrow=3,
font.label = list(size = 40, color = "black", face = "bold"))

ggsave("CDA_P2/u_plot.png",width=7, height=12, dpi=300)




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


install.packages("tidyverse")
