#########################################
# FUNCTION TO CACULATE VALUE OF KERNEL: 
#########################################
# this function calculates the prediction for a specific input x
# for boxcar and gaussian kernel

# INPUT:
# df: dataset containing original values
# res: matrix containing the x for prediction in first column
# idx: row index; specifies the x we want to calculate the kernel for
# h: binwidth
# typ: type of kernel: either gaussian or boxcar
# sigma: variance of xi for calcualting variance of r_hat

# OUTPUT: 
# r_hat: kernel for the particular value of x 
# var_r_hat: variance as defined in problem statement

prediction_idx <- function(df, res, idx, h, typ, sigma = 1, myx, myy) {
  
  # define input for kernel
  kernel_x <-  (df[, get(myx)] - res[idx, 1]) / h
  
  # define value of kernel depending on type
  if(typ == "boxcar") {
    kernel_value <- 0.5*ifelse(abs(kernel_x) <= 1, 1, 0)
  } else if(typ == "gaussian") {
    kernel_value <- 1/(sqrt(2*pi))*exp(-kernel_x^2/2)
  }
  
  # get prediction based on kernel value
  r_hat <- sum(kernel_value*df[, get(myy)]) / sum(kernel_value)
  
  # estimate variance of r_hat
  var_r_hat <- ( sum(kernel_value^2) / sum(kernel_value)^2 )*(sigma^2)
  
  # return results  
  return(cbind(r_hat, var_r_hat))
  
}

#########################################
# FUNCTION TO CACULATE R_HAT PREDICTIONS: 
#########################################
# this function creates the predictions
# for the whole sequence of x values we need 
# for boxcar, gaussian and regressogram

# INPUT:
# df: dataset
# h: bin width
# [a, b] denotes boundaries of the data
# m: number of bins
# regressogram: Indicator for whether regressogram prediction should be calculated
# boxcar: Indicator for whether boxcar predictions should be generated
# gaussian: Indicator for whether gaussian predictions should be generated
# s: specified standard deviation; default is 1

# OUTPUT:
# res: data frame containing the x values in column 1, 
# the predictions from the regressogram in column 2, 
# the predictions from the boxcar in column 3, 
# the predictions from the gaussian in column 4
# the variance of the boxcar in column 5 and  
# the variance of the gaussian in column 6

kernel_predictions <- function(df, h, a, b, m, 
                               test_df,
                               myx, 
                               myy,
                               regressogram = TRUE, 
                               boxcar = TRUE, 
                               gaussian = TRUE, 
                               s = 1) {

  # create matrix to hold results
  # predict on input x
  res <- matrix(NA, nrow = nrow(test_df), ncol = 6)
  res[, 1] <- test_df[, get(myx)]

  
  # check boundaries of data 
  # if a, b missing, then whole range is used
  if(!missing(a) & !missing(b)) {
    df_reg <- df[get(myx) >= a & get(myx) <= b, ]
  } else {
    df_reg <- df
  }
  
  # calculate binwidth if m is specified, but not h 
  if(missing(h) & !missing(m)) {
    h <- (b - a) / m
  }
  
  ####################
  ### REGRESSOGRAM
  ####################
  
  if(regressogram == TRUE) {
    # create bins for regressogram
    # of equal size from a to b with width h
    v <- seq(a, b, by = h)
    # next, find the intervals the true x belongs to, based on this sequence
    intervals <- findInterval(df_reg[, get(myx)], v, 
                              rightmost.closed = TRUE, 
                              left.open = TRUE)
    df_reg[, interval :=  intervals]
    
    # calcualte the average value of y in each interval
    aggregate_group_means <- df_reg[, .(mean_interval = mean(get(myy))), by = interval]
    
    # find the interval the values in the prediction sequence of x belong to
    groups_sequence <- data.table(interval = findInterval(res[, 1], v, 
                                                          rightmost.closed = TRUE,
                                                          left.open = TRUE))
    # merge the original x with their predictions
    predictions_by_interval <- merge(groups_sequence, aggregate_group_means)[[2]]
    # attach to results
    res[, 2] <- predictions_by_interval
    
  }
  
  ####################
  ### BOXCAR
  ####################
  
  if(boxcar == TRUE) {
    for(i in 1:length(res[, 1])) {
      # prediction
      res[i, 3] <- prediction_idx(df = df_reg, res = res,
                                  idx = i, h = h, typ = "boxcar", 
                                  sigma = s, myy = myy, myx = myx)[[1]]
      # variance
      res[i, 5] <- prediction_idx(df = df_reg, res = res, 
                                  idx = i, h = h, typ = "boxcar", 
                                  sigma = s, myy = myy, myx = myx)[[2]]
    }
  }  
  
  ####################
  ### GAUSSIAN
  ####################
  
  if(gaussian == TRUE) {
    for(i in 1:length(res[, 1])) {
      # prediction
      res[i, 4] <- prediction_idx(df = df_reg, res = res,
                                  idx = i, h = h, typ = "gaussian", 
                                  sigma = s)[[1]]
      # variance
      res[i, 6] <- prediction_idx(df = df_reg, res = res, 
                                  idx = i, h = h, typ = "gaussian", 
                                  sigma = s)[[2]]
    }    
  }  
  
  # prettify results and return
  res <- data.table(res) 
  colnames(res) <- c("x", "regressogram", "boxcar", "gaussian", 
                     "boxcar_variance", "gaussian_variance")
  return(res)
  
}    
