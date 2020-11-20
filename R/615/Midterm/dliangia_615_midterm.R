## Title: Fitted Denoising Function ##
## Name: Diana Liang                ##
## Date: 11/20/20                   ##
## Course: Biostats 615 Fall 2020   ##

## The functions and code below denoises a square matrix
## with a singular contiguous space R

## Contents:
## How to Use Algorithm
## Part A: Denoising Function
## Part B: Necessary Functions
## Part C: Demonstration

## How to Use Algorithm ------------------------------------------------------
# Step 1: Define all the functions in Part B
# (Mid Step: Make sure the Matrix package is installed)
# Step 2: Define denoise_matrix function in Part A
# Step 3: Load or create square noisy matrix
# Step 4: denoise_matrix(your_noisy_matrix) will output a denoised matrix

## A: Denoising Function -----------------------------------------------------

# Denoises a noisy square matrix
denoise_matrix <- function(og_matrix){
  ## Input: square matrix of noisy values
  ## Output: denoised square matrix
  ## Requires other functions defined in part B below
  
  ## set up
  require(Matrix)
  n <- nrow(og_matrix)
  
  ## find values for u_0 and u_1
  # u_0 will be maximum freq value
  test <- round(og_matrix)
  values <- sort(unique(c(test)))
  freq <- sapply(values, function(x) length(test[test==x]))
  unique <- data.frame("freq"=freq, "values"=values)
  max_freq <- max(unique$freq)
  u_0 <- unique$values[which.max(unique$freq)]
  # find u_1 value that gives minimum sd
  upper <- length(unique$values[unique$values > u_0])
  lower <- length(unique$values[unique$values < u_0])
  if(upper > lower){
    i <- u_0
    last_sd <- sd(og_matrix)
    next_sd <- last_sd
    while(next_sd <= last_sd){ # find index i when sd lowest
      i <- i+1
      last_sd <- next_sd
      value <- sapply(og_matrix,
                      function(x) set_values(x, u_0, i))
      value <- c(og_matrix)-value
      next_sd <- sd(value)
    }
    u_1 <- i-1
  } else{
    i <- u_0
    last_sd <- sd(og_matrix)
    next_sd <- next_sd
    while(next_sd <= last_sd){ # find index i when sd lowest
      i <- i-1
      last_sd <- next_sd
      value <- sapply(og_matrix,
                      function(x) set_values(x, i, u_0))
      value <- c(og_matrix)-value
      next_sd <- sd(value)
    }
    u_1 <- i+1
  }
  
  ## find cut off value to distinguish R or not R
  # find cut off value based on current sd
  start_cut <- (u_0^2-u_1^2)/(2*(u_0-u_1))
  start_cut <- start_cut - last_sd*(log(unique$freq[unique$values==u_0])-log(unique$freq[unique$values==u_1]))/(u_0-u_1)
  test <- sapply(og_matrix,
                 function(x) try_values(x, min(u_0, u_1), max(u_0, u_1), cut=start_cut))
  # want to incompass more of possible R
  sd <- sd(c(og_matrix)-test)
  if(u_0 > u_1){
    current_cut <- u_1 + 2*last_sd
  } else{
    current_cut <- u_1 - 2*last_sd
  }
  test <- sapply(og_matrix,
                 function(x) try_values(x, min(u_0, u_1), max(u_0, u_1), cut=current_cut))
  
  ## figure out R space
  # set up for looping
  test <- matrix((test-u_0), nrow=n, ncol=n)
  idx <- which(test != 0, arr.ind=TRUE)
  complete_list <- c(1:nrow(idx))
  current <- c()
  group <- c()
  picture <- c()
  dump <- c()
  # loop through to find the indices that are in R
  while(length(complete_list) != 0){
    current <- c(current, complete_list[1])
    while(length(current) != 0){
      i <- length(current)
      # check each direction and add that index if part of R
      up <- check_up(idx, current[i])
      if(up[[1]]==TRUE & !(up[[2]] %in% current) & !(up[[2]] %in% group)){
        current <- c(current, up[[2]])
      }
      left <- check_left(idx, current[i])
      if(left[[1]]==TRUE & !(left[[2]] %in% current) & !(left[[2]] %in% group)){
        current <- c(current, left[[2]])
      }
      down <- check_down(idx, current[i])
      if(down[[1]]==TRUE & !(down[[2]] %in% current) & !(down[[2]] %in% group)){
        current <- c(current, down[[2]])
      }
      right <- check_right(idx, current[i])
      if(right[[1]]==TRUE & !(right[[2]] %in% current) & !(right[[2]] %in% group)){
        current <- c(current, right[[2]])
      }
      group <- c(group, current[i])
      current <- current[-i]
    }
    if(length(group) < 0.05*(n^2)){ # get rid of indices that are not part of R
      dump <- c(dump, group)
      complete_list <- complete_list[!(complete_list %in% group)]
      group <- c()
    } else{ # keep only the indices that are in R
      picture <- group
      complete_list <- complete_list[!(complete_list %in% group)]
      dump <- c(dump, complete_list)
      complete_list <- c()
    }
  }
  # create the denoised new matrix
  idx <- idx[sort(picture), ]
  new_matrix <- sparseMatrix(i=idx[,1], j=idx[,2],
                             x=rep(u_1, nrow(idx)), dims=dim(og_matrix))
  new_matrix <- as.matrix(new_matrix + u_0)
  return(new_matrix)
}

## B: Necessary Functions ----------------------------------------------------
# Separate values
set_values <- function(x, min, max){
  if(x < min){
    x = min
  } else if(x > max){
    x = max
  } else{
    lower = abs(x-min)
    upper = abs(x-max)
    if(lower > upper){x=max} else{x=min}
  }
  return(x)
}

try_values <- function(x, min, max, cut="none"){
  if(cut=="none"){
    if(x<min){
      x=min
    } else if(x>max){
      x=max
    } else{
      x=x
    }
  } else{
    if(x<cut){
      x=min
    } else{
      x=max
    }
  }
  return(x)
}

# Check each direction
check_left <- function(idx, i){
  out <- FALSE
  row_n <- idx[i,1]
  col_n <- idx[i,2]
  current_row <- matrix(idx[idx[,1]==row_n, ], ncol=2)
  if((col_n-1) %in% current_row[,2]){
    out <- TRUE
  }
  if(out == FALSE){
    i <- NA
  } else{
    i <- which(idx[,1]==row_n & idx[,2]==(col_n-1))
  }
  return(list(check <- out, index <- i))
}
check_right <- function(idx, i){
  out <- FALSE
  row_n <- idx[i,1]
  col_n <- idx[i,2]
  current_row <- matrix(idx[idx[,1]==row_n, ], ncol=2)
  if((col_n+1) %in% current_row[,2]){
    out <- TRUE
  }
  if(out == FALSE){
    i <- NA
  } else{
    i <- which(idx[,1]==row_n & idx[,2]==(col_n+1))
  }
  return(list(check <- out, index <- i))
}
check_up <- function(idx, i){
  out <- FALSE
  row_n <- idx[i,1]
  col_n <- idx[i,2]
  current_col <- matrix(idx[idx[,2]==col_n, ], ncol=2)
  if((row_n-1) %in% current_col[,1]){
    out <- TRUE
  }
  if(out == FALSE){
    i <- NA
  } else{
    i <- which(idx[,1]==(row_n-1) & idx[,2]==col_n)
  }
  return(list(check <- out, index <- i))
}
check_down <- function(idx, i){
  out <- FALSE
  row_n <- idx[i,1]
  col_n <- idx[i,2]
  current_col <- matrix(idx[idx[,2]==col_n, ], ncol=2)
  if((row_n+1) %in% current_col[,1]){
    out <- TRUE
  }
  if(out == FALSE){
    i <- NA
  } else{
    i <- which(idx[,1]==(row_n+1) & idx[,2]==col_n)
  }
  return(list(check <- out, index <- i))
}

## C: Demonstration ---------------------------------------------------------

# set up
#load("images_examples.RData")
#tt <- sum(.Internal(gc(FALSE, TRUE, TRUE))[13:14])
#system.time(test_img <- denoise_matrix(noisy_img))
#sum(.Internal(gc(FALSE, FALSE, TRUE))[13:14]) - tt


# find error
#n <- nrow(noisy_img)
#error <- sum(test_img != true_img)/(n^2)
#sprintf("Error: %0.4f", error)
#abs_mean <- mean(abs(test_img-true_img))
#sprintf("Mean Absolute Difference: %0.4f", abs_mean)

# visualize
#library(graphics)
#image(noisy_img, col=hcl.colors(100, "Blue-Yellow-3"), main="A) Noisy Image")
#image(test_img, col=hcl.colors(100, "Blue-Yellow-3"), main="B) Denoised Image")
#image(true_img, col=hcl.colors(100, "Blue-Yellow-3"), main="C) True Image")
