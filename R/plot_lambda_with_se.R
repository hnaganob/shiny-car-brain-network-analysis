plot_lambda_with_se <- function(
    model_parameter_table,
    frame_of_interest = NULL,
    frame = NULL,
    alpha = 0.1,
    ...) {
  # If time is not provided or empty, default to full time index
  if (is.null(frame) || length(frame) == 0) {
    alpha <- 1
  }
  
  time <- model_parameter_table$time
  lambda <- model_parameter_table$lambda
  lambda_se <- model_parameter_table$lambda_se
  lambda_p_value <- model_parameter_table$lambda_p_value
  
  y_lim <- c(min(lambda - lambda_se), max(lambda + lambda_se))
  
  col_lambda <- ifelse(lambda_p_value < 0.05, 2, 1)
  col_lambda <- adjustcolor(col_lambda, alpha.f = alpha)
  
  cap_width <- 0.8
  
  if (alpha != 1) {
    bg_lambda <- "white"
  } else {
    bg_lambda <- ifelse(lambda_p_value < 0.05, 2, "white")
  }
  
  # draw empty plot
  plot(time, lambda, type = "n", col = col_lambda, ylim = y_lim, ...)
  
  # draw non-significant line
  abline(h = 0, lty = 3, col = "gray")
  
  # Horizontal caps
  segments(
    x0 = time - cap_width,
    y0 = lambda + lambda_se,
    x1 = time + cap_width,
    y1 = lambda + lambda_se,
    col = col_lambda
  )
  segments(
    x0 = time - cap_width,
    y0 = lambda - lambda_se,
    x1 = time + cap_width,
    y1 = lambda - lambda_se,
    col = col_lambda
  )
  segments(
    x0 = time,
    y0 = lambda - lambda_se,
    x1 = time,
    y1 = lambda + lambda_se,
    col = col_lambda
  )
  points(time, lambda, pch = 21, bg = bg_lambda, col = col_lambda)
  
  time_frame <- time[frame]
  lambda_frame <- lambda[frame]
  lambda_se_frame <- lambda_se[frame]
  col_lambda_frame <- ifelse(lambda_p_value < 0.05, 2, 1)[frame]
  bg_lambda_frame <- ifelse(lambda_p_value < 0.05, 2, "white")[frame]
  
  segments(
    x0 = time_frame - cap_width,  # adjust width of the cap
    y0 = lambda_frame + lambda_se_frame,
    x1 = time_frame + cap_width,
    y1 = lambda_frame + lambda_se_frame,
    col = col_lambda_frame
  )
  segments(
    x0 = time_frame - cap_width,
    y0 = lambda_frame - lambda_se_frame,
    x1 = time_frame + cap_width,
    y1 = lambda_frame - lambda_se_frame,
    col = col_lambda_frame
  )
  segments(
    x0 = time_frame,
    y0 = lambda_frame - lambda_se_frame,
    x1 = time_frame,
    y1 = lambda_frame + lambda_se_frame,
    col = col_lambda_frame
  )
  points(
    time_frame, lambda_frame,
    pch = 21, col = col_lambda_frame, bg = bg_lambda_frame
  )
}
