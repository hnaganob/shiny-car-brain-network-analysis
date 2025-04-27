get_spdep_model_parameters <- function(time = NULL, sp_data, listw) {
  # If time is not provided or empty, default to full time index
  if (is.null(time) || length(time) == 0) {
    time <- 1:nrow(sp_data)
  }
  
  # Initialize containers
  intercept <- intercept_se <- intercept_p_value <-  lambda <- lambda_se <- lambda_p_value <- sigma2 <- numeric(length(time))
  
  # Add progress bar
  pb <- txtProgressBar(min = 0, max = length(time), style = 3)
  
  for (frame in seq_along(time)) {
    y <- sp_data[frame, ]
    model <- spautolm(y ~ 1, listw = listw, family = "CAR")
    model_summary <- summary(model)
    
    intercept[frame] <- model$fit$coefficients
    intercept_se[frame] <- model_summary$rest.se
    intercept_p_value[frame] <- model_summary$Coef[1, "Pr(>|z|)"]
    sigma2[frame] <- model$fit$s2
    lambda[frame] <- model$lambda
    lambda_se[frame] <- model$lambda.se
    lambda_p_value[frame] <- model_summary$LR1$p.value
    
    setTxtProgressBar(pb, frame)  # update progress
  }
  
  close(pb)  # close the progress bar
  
  intercept_significance_code <- symnum(
    intercept_p_value,
    corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )
  
  lambda_significance_code <- symnum(
    lambda_p_value,
    corr = FALSE, na = FALSE,
    cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
    symbols = c("***", "**", "*", ".", " ")
  )

  parameter_table <- data.frame(
    time, 
    intercept, intercept_se, intercept_significance_code,
    sigma2,
    lambda, lambda_se, lambda_p_value, lambda_significance_code
  )
  
  return(parameter_table)
}



# spdep_model <- function(time, sp_data, listw) {
#   intercept <- NULL
#   intercept_se <- NULL
#   lambda <- NULL
#   lambda_se <- NULL
#   lambda_p_value <- NULL
#   sigma2 <- NULL
# 
#   # add progress bar
#   pb <- txtProgressBar(min = 0, max = length(time), style = 3)
# 
#   for (frame in seq_along(time)) {
#     y <- sp_data[frame, ]
#     model <- spautolm(y ~ 1, listw = listw, family = "CAR")
#     model_summary <- summary(model)
# 
#     intercept <- c(intercept, model$fit$coefficients)
#     intercept_se <- c(intercept_se, model_summary$rest.se)
#     sigma2 <- c(sigma2, model$fit$s2)
#     lambda <- c(lambda, model$lambda)
#     lambda_se <- c(lambda_se, model$lambda.se)
#     lambda_p_value <- c(lambda_p_value, model_summary$LR1$p.value)
# 
#     setTxtProgressBar(pb, frame) # update progress
#   }
# 
#   lambda_table <- data.frame(
#     time, lambda, lambda_se, lambda_p_value
#   )
#   return(
#     list(lambda_table = lambda_table)
#   )
# }