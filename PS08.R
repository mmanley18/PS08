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

n_values <- c(1000, 500000, 750000, 1000000, 2000000, 5000000, 7000000, 10000000)
k_values <- c(1, 2, 5, 7, 10, 12, 15, 17, 20)

runtime_dataframe <- expand.grid(k_values, n_values) %>%
  as_tibble() %>%
  rename(k=Var1, n=Var2) %>%
  mutate(runtime = rep(0))

# for method 2
count <- 1

# for method 1
# runtime <- c()

for (i in 1:length(n_values)) {
  n <- train[1:n_values[i],]
  
  for (l in 1:length(k_values)) {
    k <- k_values[l]
    
    tic()
    model_knn <- caret::knn3(model_formula, data=n, k = k)
    timer_info <- toc()
    
    # store run times - method 1
    # runtime = c(runtime, timer_info$toc - timer_info$tic)
    
    # store run times - method 2
    runtime_dataframe$runtime[count] = timer_info$toc - timer_info$tic
    
    count = count + 1
    
    # store run times - method 3
    # runtime_dataframe$runtime[((i - 1)*length(k_values)) + l] = timer_info$toc - timer_info$tic

  }
}



# Test runtime knn here -----------------------------------------------------------

# tic()
# model_knn <- caret::knn3(model_formula, data=train[1:29000000,], k = 29000000 )
# timer_info <- toc()


# Plot your results ---------------------------------------------------------
# Think of creative ways to improve this barebones plot. Note: you don't have to
# necessarily use geom_point
runtime_plot <- ggplot(runtime_dataframe, aes(x=n, y=runtime, col=k)) +
  scale_colour_gradient(low = "white", high = "black") +
  geom_point(size = 3) + geom_smooth(method = "lm") + geom_jitter()

runtime_plot
ggsave(filename="meredith_manley.png", width=16, height = 9)

# From this plot we can see that as the size of the training set increases as the run time and thus
# are positively associated with each other. When determining how k-nieghbors affects the run time
# we look at the shade of the points. It appears as though the size of k does not substantially 
# affect the run time. However for a trainig set size of 7,000,000 the largest run time corresponds
# to the lowest k-neighbors value. Perhaps if time permitted and we added more possible values
# for k-neighbors we would be able to have a better idea as to how this parameter affects run time.


# Runtime complexity ------------------------------------------------------
# Can you write out the rough Big-O runtime algorithmic complexity as a function
# of:
# -n: number of points in training set
# -k: number of neighbors to consider
# -d: number of predictors used? In this case d is fixed at 3

# We know that d is fixed at 3, so that will simply be a constant in the algorithm and we know
# that there appears to be a positive linear relationship between run time and the size of the
# training set so perhaps the we would have a function as follows:
# complexity = n*k + d = O(n) as this appears to be linear.
