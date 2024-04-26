# Date: 2021-04-26
# Description: This script is used to test the CCA and CCP libraries
#             using the regularized version of the algorithm.
#           regularized can handle the large number of columns


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

{
  x$Individual_1 <- as.numeric(x$Individual == 1)
  x$Individual_2 <- as.numeric(x$Individual == 2)
  x$Individual_3 <- as.numeric(x$Individual == 3)
  x$Individual_4 <- as.numeric(x$Individual == 4)
  x$Individual_5 <- as.numeric(x$Individual == 5)
  x$Individual_6 <- as.numeric(x$Individual == 6)
  x$Individual_7 <- as.numeric(x$Individual == 7)
  x$Individual_8 <- as.numeric(x$Individual == 8)
  x$Individual_9 <- as.numeric(x$Individual == 9)
  x$Individual_10 <- as.numeric(x$Individual == 10)
  x$Individual_11 <- as.numeric(x$Individual == 11)
  x$Individual_12 <- as.numeric(x$Individual == 12)
  x$Individual_13 <- as.numeric(x$Individual == 13)
  x$Individual_14 <- as.numeric(x$Individual == 14)
  x$Individual_15 <- as.numeric(x$Individual == 15)
  x$Individual_16 <- as.numeric(x$Individual == 16)
  x$Individual_17 <- as.numeric(x$Individual == 17)
  x$Individual_18 <- as.numeric(x$Individual == 18)
  x$Individual_19 <- as.numeric(x$Individual == 19)
  x$Individual_20 <- as.numeric(x$Individual == 20)
  x$Individual_21 <- as.numeric(x$Individual == 21)
  x$Individual_22 <- as.numeric(x$Individual == 22)
  x$Individual_23 <- as.numeric(x$Individual == 23)
  x$Individual_24 <- as.numeric(x$Individual == 24)
  x$Individual_25 <- as.numeric(x$Individual == 25)
  x$Individual_26 <- as.numeric(x$Individual == 26)

  x$Individual <- NULL
}


# load library
library(CCA)
library(CCP)

# REGULARIZED
# perform CCA
{
  lambda_x <- 1e-5
  lambda_y <- 1e-5


  rcc <- rcc(x, y, lambda_x, lambda_y)
}


# print dimensions of  ycoef and ycoef
dimx <- dim(x)
dimy <- dim(y)


# perform statistical test
p.asym(rcc$cor, dimx[1], dimx[2], dimy[2])



#* Choosing the 6 first components
{
  v1 <- rcc$xcoef[, 1]
  v2 <- rcc$xcoef[, 2]
  v3 <- rcc$xcoef[, 3]
  v4 <- rcc$xcoef[, 4]
  v5 <- rcc$xcoef[, 5]
  v6 <- rcc$xcoef[, 6]

  u1 <- rcc$ycoef[, 1]
  u2 <- rcc$ycoef[, 2]
  u3 <- rcc$ycoef[, 3]
  u4 <- rcc$ycoef[, 4]
  u5 <- rcc$ycoef[, 5]
  u6 <- rcc$ycoef[, 6]
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
  df_v3 <- data.frame(feature = c(1 : length(v3)),
                      coefficient = v3, component = "v3")
  df_v4 <- data.frame(feature = c(1 : length(v4)),
                      coefficient = v4, component = "v4")
  df_v5 <- data.frame(feature = c(1 : length(v5)),
                      coefficient = v5, component = "v5")
  df_v6 <- data.frame(feature = c(1 : length(v6)),
                      coefficient = v6, component = "v6")

  v1_plot <- plot_components(df_v1, 5, colnames(x))

  v2_plot <- plot_components(df_v2, 5, colnames(x))

  v3_plot <- plot_components(df_v3, 5, colnames(x))

  v4_plot <- plot_components(df_v4, 5, colnames(x))

  v5_plot <- plot_components(df_v5, 5, colnames(x))

  v6_plot <- plot_components(df_v6, 5, colnames(x))



  ggarrange(v1_plot, v2_plot, v3_plot, v4_plot, v5_plot, v6_plot,
            labels = c("v1", "v2", "v3", "v4", "v5", "v6"),
            ncol = 1, nrow = 6,
            font.label = list(size = 40, color = "black", face = "bold"))
}



{
  df_u1 <- data.frame(feature = c(1:length(u1)),
                      coefficient = u1, component = "u1")
  df_u2 <- data.frame(feature = c(1:length(u2)),
                      coefficient = u2, component = "u2")
  df_u3 <- data.frame(feature = c(1:length(u3)),
                      coefficient = u3, component = "u3")
  df_u4 <- data.frame(feature = c(1:length(u4)),
                      coefficient = u4, component = "u4")
  df_u5 <- data.frame(feature = c(1:length(u5)),
                      coefficient = u5, component = "u5")
  df_u6 <- data.frame(feature = c(1:length(u6)),
                      coefficient = u6, component = "u6")


  u1_plot <- plot_components(df_u1, 5, colnames(y))

  u2_plot <- plot_components(df_u2, 5, colnames(y))

  u3_plot <- plot_components(df_u3, 5, colnames(y))

  u4_plot <- plot_components(df_u4, 5, colnames(y))

  u5_plot <- plot_components(df_u5, 5, colnames(y))

  u6_plot <- plot_components(df_u6, 5, colnames(y))


  ggarrange(u1_plot, u2_plot, u3_plot, u4_plot, u5_plot, u6_plot,
            labels = c("u1", "u2", "u3", "u4", "u5", "u6"),
            ncol = 1, nrow = 6,
            font.label = list(size = 40, color = "black", face = "bold"))

}


