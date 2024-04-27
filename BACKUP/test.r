# Date: 2021-04-26
# Description: This script is used to test the CCA and CCP libraries


# load dataframe
dataframe <- read.csv("CDA_P2/HR_data.csv")

# remove rows with nans
dataframe <- na.omit(dataframe)

# extract column names
col_ <- colnames(dataframe)

# build X columns
xcols <- c(col_[2:56], col_[58])

# build Y columns
ycols <- c(col_[57], col_[59:68])

# extract X and Y
x <- dataframe[xcols]
y <- dataframe[ycols]

# convert to numeric
#   Right now, we are not doing any one-hot encoding on cohort
x$Cohort <- as.numeric(as.factor(x$Cohort)) # should probably be one-hot encoded
x$Phase <- as.numeric(as.ordered(x$Phase))
x$Round <- as.numeric(as.ordered(x$Round))

# load library
library(CCA)
library(CCP)

# perform CCA
cc1 <- cc(x, y)

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

#* Choosing the three first components
v1 <- cc1$xcoef[0:56, 1]
v2 <- cc1$xcoef[0:56, 2]
v3 <- cc1$xcoef[0:56, 3]

u1 <- cc1$ycoef[0:11, 1]
u2 <- cc1$ycoef[0:11, 2]
u3 <- cc1$ycoef[0:11, 3]


# SIMPLE PLOTS
# plot xcoef and ycoef for the first component pair
plot(c(1:56), v1, "l")
plot(c(1:11), u1, "l")

# plot xcoef and ycoef for the second component pair
plot(c(1:56), v2, "l")
plot(c(1:11), u2, "l")

# plot xcoef and ycoef for the third component pair
plot(c(1:56), v3, "l")
plot(c(1:11), u3, "l")





# sorting xcoef by size
index_ <- sort(abs(v1), index.return = TRUE, decreasing = TRUE)$ix

v1[index_]

index_ <- sort(abs(v2), index.return = TRUE, decreasing = TRUE)$ix

v2[index_]

index_ <- sort(abs(v1), index.return = TRUE, decreasing = TRUE)$ix

v3[index_]

# sort ycoef by size
index_ <- sort(abs(u1), index.return = TRUE, decreasing = TRUE)$ix

u1[index_]

index_ <- sort(abs(u2), index.return = TRUE, decreasing = TRUE)$ix

u2[index_]

index_ <- sort(abs(u3), index.return = TRUE, decreasing = TRUE)$ix

u3[index_]




# ADVANCED PLOTTING, REQUIRES SOME PACKAGES
# install.packages("ggpubr")
# install.packages("ggplot2")
library(ggplot2)
library(ggpubr)


# collecting xcoef for the first three components
df_v1 <- data.frame(x_feature = c(1:56), coefficient = v1, component = "v1")
df_v2 <- data.frame(x_feature = c(1:56), coefficient = v2, component = "v2")
df_v3 <- data.frame(x_feature = c(1:56), coefficient = v3, component = "v3")


# plotting the largest N components
n1 <- 5
index1 <- sort(abs(v1), index.return = TRUE, decreasing = TRUE)$ix[n1 + 1:56]
col1 <- colnames(x)
col1[index1] <- ""

# plotting the first component
v1_plot <- ggplot(data = df_v1, aes(x = x_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = x_feature, xend = x_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = x_feature, y = coefficient, label = col1),
            fontface = "bold", color = "red")

# plotting the largest N components
n2 <- 6
index2 <- sort(abs(v2), index.return = TRUE, decreasing = TRUE)$ix[n2 + 1:56]
col2 <- colnames(x)
col2[index2] <- ""

# plotting the second component
v2_plot <- ggplot(data = df_v2, aes(x = x_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = x_feature, xend = x_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = x_feature, y = coefficient, label = col2),
            fontface = "bold", color = "red")

# plotting the largest N components
n3 <- 6
index3 <- sort(abs(v3), index.return = TRUE, decreasing = TRUE)$ix[n3 + 1:56]
col3 <- colnames(x)
col3[index3] <- ""

# plotting the third component
v3_plot <- ggplot(data = df_v3, aes(x = x_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = x_feature, xend = x_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = x_feature, y = coefficient, label = col3),
            fontface = "bold", color = "red")


ggarrange(v1_plot, v2_plot, v3_plot, labels = c("v1", "v2", "v3"),
          ncol = 1, nrow = 3,
          font.label = list(size = 40, color = "black", face = "bold"))

ggsave("CDA_P2/v_plot.png", dpi = 300)
# see pictured saved


df_u1 <- data.frame(y_feature = c(1:11), coefficient = u1, component = "u1")
df_u2 <- data.frame(y_feature = c(1:11), coefficient = u2, component = "u2")
df_u3 <- data.frame(y_feature = c(1:11), coefficient = u3, component = "u3")


col_ <- colnames(y)


u1_plot <- ggplot(data = df_u1, aes(x = y_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = y_feature, xend = y_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = y_feature, y = coefficient, label = col_),
            fontface = "bold", color = "red", size = 7)



u2_plot <- ggplot(data = df_u2, aes(x = y_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = y_feature, xend = y_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = y_feature, y = coefficient, label = col_),
            fontface = "bold", color = "red", size = 7)


u3_plot <- ggplot(data = df_u3, aes(x = y_feature, y = coefficient)) +
  geom_point(size = 4) +
  geom_segment(aes(x = y_feature, xend = y_feature, y = 0, yend = coefficient),
               linewidth = 2, alpha = 0.5) +
  theme(text = element_text(size = 20),
        legend.text = element_text(size = 20)) +
  geom_text(aes(x = y_feature, y = coefficient, label = col_),
            fontface = "bold", color = "red", size = 7)


ggarrange(u1_plot, u2_plot, u3_plot, labels = c("u1", "u2", "u3"),
          ncol = 1, nrow = 3,
          font.label = list(size = 40, color = "black", face = "bold"))

ggsave("CDA_P2/u_plot.png", dpi = 300)
# see picture saved



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
