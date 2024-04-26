# Date: 2021-04-26
# Description: This script is used to test the CCA and CCP libraries
#          here the emotions are separated into positive and negative


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

# ------------------------------------------------
# run this block to extract the negative and positive columns
# ------------------------------------------------
# extract negative and positive columns

{ 
  negative_cols <- ycols[c(1, 2, 3, 5, 7, 10)]
  df_negative <- dataframe[negative_cols]


  df_negative$Frustrated <- df_negative$Frustrated / 2

  positive_cols <- ycols[c(4, 6, 8, 9, 11)]
  df_positive <- dataframe[positive_cols]


  y_pos_mean <- apply(df_positive, 1, mean)

  y_neg_mean <- apply(df_negative, 1, mean)

  y <- as.data.frame(cbind(y_pos_mean, y_neg_mean))
}
# ------------------------------------------------

# extract X and Y
x <- dataframe[xcols]

{
  # convert to numeric
  x$Phase <- as.numeric(as.ordered(x$Phase))
  x$Round <- as.numeric(as.ordered(x$Round))

  # one-hot encoding
  x$Cohort_1 <- as.numeric(x$Cohort == "D1_1")
  x$Cohort_2 <- as.numeric(x$Cohort == "D1_2")
  x$Cohort_3 <- as.numeric(x$Cohort == "D1_3")
  x$Cohort_4 <- as.numeric(x$Cohort == "D1_4")
  x$Cohort_5 <- as.numeric(x$Cohort == "D1_5")
  x$Cohort_6 <- as.numeric(x$Cohort == "D1_6")

  x$Cohort <- NULL
}

# load library
library(CCA)
library(CCP)

# perform CCA
cc1 <- cc(x, y)

# print correlation
cc1$cor

# print dimensions of  ycoef and ycoef
dimx <- dim(x)
dimy <- dim(y)


# perform statistical test
p.asym(cc1$cor, dimx[1], dimx[2], dimy[2])


project_data <- function(x, y, cc1, n=1) {
  # plot the projected data
  x_projected <- as.matrix(x) %*% as.matrix(cc1$xcoef)
  y_projected <- as.matrix(y) %*% as.matrix(cc1$ycoef)

  # plot projected data
  plot(x_projected[, n], y_projected[, n], col = "red")

  # the correlation in cc1$cor corresponds
  # to the correlation of the projected data on u1 and v1
  cor(x_projected[, n], y_projected[, n])
}

# project on pair n
n <- 1
project_data(x, y, cc1, n)



#* Choosing the three first components
{
  v1 <- cc1$xcoef[, 1]
  v2 <- cc1$xcoef[, 2]

  u1 <- cc1$ycoef[, 1]
  u2 <- cc1$ycoef[, 2]
}


# ADVANCED PLOTTING, REQUIRES SOME PACKAGES
library(ggplot2)
library(ggpubr)


plot_components <- function(df, n, col) {

  index <- sort(abs(df$coefficient),
                index.return = TRUE,
                decreasing = TRUE)$ix[-n:0]

  col[index] <- ""

  # plotting the first component
  v1_plot <- ggplot(data = df, aes(x = feature, y = coefficient)) +
    geom_point(size = 4) +
    geom_segment(aes(x = feature, xend = feature, 
                     y = 0, yend = coefficient),
                     linewidth = 2, alpha = 0.5) +
    theme(text = element_text(size = 20),
          legend.text = element_text(size = 20)) +
    geom_text(aes(x = feature, y = coefficient, label = col),
              fontface = "bold", color = "red")

  return(v1_plot)
}

{
  # collecting xcoef for the first three components
  df_v1 <- data.frame(feature = c(1 : length(v1)),
                      coefficient = v1, component = "v1")
  df_v2 <- data.frame(feature = c(1 : length(v2)),
                      coefficient = v2, component = "v2")



  v1_plot <- plot_components(df_v1, 5, colnames(x))

  v2_plot <- plot_components(df_v2, 5, colnames(x))



  ggarrange(v1_plot, v2_plot, labels = c("v1", "v2"),
            ncol = 1, nrow = 2,
            font.label = list(size = 40, color = "black", face = "bold"))
}


ggsave("CDA_P2/v_plot.png", dpi = 300)
# see pictured saved

{
  df_u1 <- data.frame(feature = c(1:length(u1)),
                      coefficient = u1, component = "u1")
  df_u2 <- data.frame(feature = c(1:length(u2)),
                      coefficient = u2, component = "u2")


  u1_plot <- plot_components(df_u1, 5, colnames(y))

  u2_plot <- plot_components(df_u2, 5, colnames(y))


  ggarrange(u1_plot, u2_plot, labels = c("u1", "u2"),
            ncol = 1, nrow = 2,
            font.label = list(size = 40, color = "black", face = "bold"))

}

ggsave("CDA_P2/u_plot.png", dpi = 300)
# see picture saved


