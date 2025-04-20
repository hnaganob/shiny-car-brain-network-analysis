plot_network_signal <- function(
    net,
    coord,
    signal,
    frame = 1,
    y_range = NULL,
    signal_color_ramp = c("darkblue", "blue", "white", "orange", "darkorange"),
    model_parameter_table,
    ...) {
  y <- scale(signal)[frame, ]

  # normarize y between 0 and 1
  if (is.null(y_range)) y_range <- c(min(signal), max(signal))
  y_norm <- (y - y_range[1]) / diff(y_range)

  # define vertex color
  ramp <- colorRamp(signal_color_ramp)
  vertex_col <- rgb(ramp(y_norm), max = 255)

  # plot the network
  plot(
    net,
    coord = coord,
    vertex.cex = abs(y) + 0.5,
    vertex.col = vertex_col,
    edge.col = rgb(0, 0, 0, 0.3),
    vertex.border = "gray60"
  )

  # define text position
  x_pos <- par("usr")[1] + 0.75 * (par("usr")[2] - par("usr")[1])

  # add lambda as text
  lambda <- model_parameter_table$lambda[frame]
  mtext(
    text = bquote(lambda == .(round(lambda, 4))),
    side = 3, line = -4, adj = 0, at = x_pos
  )

  # highlight the plot border if lambda is significant
  lambda_p_value <- model_parameter_table$lambda_p_value[frame]
  if (lambda_p_value < 0.05) box(which = "plot", col = "2", lwd = 5)
}