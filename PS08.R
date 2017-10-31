library(tidyverse)
library(caret)

# Package for easy timing in R
library(tictoc)



# Demo of timer function --------------------------------------------------
# Run the next 5 lines at once
tic()
Sys.sleep(3)
timer_info <- toc()
runtime <- timer_info$toc - timer_info$tic
runtime



# Get data ----------------------------------------------------------------
# Accelerometer Biometric Competition Kaggle competition data
# https://www.kaggle.com/c/accelerometer-biometric-competition/data
tic()
train <- read_csv("train.csv")
toc()

# YOOGE!
dim(train)



# knn modeling ------------------------------------------------------------
model_formula <- as.formula(Device ~ X + Y + Z)


runtime <- c()
n_value <- c()
k_value <- c()

n_values <- c(1000, 1000000, 10000000, 15000000, 20000000, 25000000, 29000000)
k_values <- c(1000, 1000000, 10000000, 15000000, 20000000, 25000000, 29000000)

runtime_dataframe <- expand.grid(n_values, k_values) %>%
  as_tibble() %>%
  rename(n=Var1, k=Var2) %>%
  mutate(runtime = rep(0))

count <- 0

for (i in n_values) {
  n <- train[1:i,]
  round <- i
  for (l in k_values) {
    k <- l
    
    tic()
    model_knn <- caret::knn3(model_formula, data=n, k = k)
    timer_info <- toc()
    
    # store run times
    runtime_dataframe$runtime[count] = 
      timer_info$toc - timer_info$tic
    #runtime_df$n_value[length(n_value)+1] = round
    #runtime_df$k_value[length(k_value)+1] = k
    
    count = count + 1

  }
}

# fix data frame because we started the for loop using count = 0 rather than count = 1.
# needed to shift all the runtime values down one cell, so that it is correspondinding to the 
# correct (n,k) pairing

runtime_dataframe2 <- runtime_dataframe

count <- 49

for (i in 1:49) {
  if (count > 1) {
  runtime_dataframe2$runtime[count] = runtime_dataframe2$runtime[count-1]
  
  count = count - 1
  }
  else 
    runtime_dataframe2$runtime[count] = 0.00
}


# Test runtime knn here -----------------------------------------------------------

# tic()
# model_knn <- caret::knn3(model_formula, data=train[1:29000000,], k = 29000000 )
# timer_info <- toc()


# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(runtime_dataframe2, aes(x=n, y=k, col=runtime)) +
  scale_colour_gradient(low = "white", high = "black") +
  geom_point(size = 8)

runtime_plot
ggsave(filename="meredith_manley.png", width=16, height = 9)

# From this plot we can see that the runtime seems to be most sensitive to the number of 
# neighbors that we select for our model. The greater the number of neighbors then the longer
# it takes to run the model. If we are looking for the model to only take about 50 seconds to 
# run then we could use the entire training set, but can use at most 20,000,000 neighbors to 
# to have the model run efficiently.


# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3

# We know that d is fixed at 3, so that will simply be a constant in the algorithm and we know
# from the plot we have just produced that k influences the runtime more significantly than
# size of the training set therefore the Big-O algorithmic complexity as a function may look
# like the follow: BigO_i = 3 + n + n^i
