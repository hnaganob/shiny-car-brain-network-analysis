add_colorbar <- function(
    y_range = c(-5, 5),
    color_ramp = c("darkblue", "blue", "gray", "orange", "darkorange"),
    horizontal = TRUE,
    main = NULL,
    ...
) {
  # from min to max
  colorbar_seq <- seq(y_range[1], y_range[2], length = 101)

  # define colors
  ramp <- colorRamp(color_ramp)

  # normalize colorbar_seq to 0-1
  colorbar <- rgb(ramp((colorbar_seq - y_range[1]) / diff(y_range)), max = 255)

  # # this might be easier?
  # colorbar <- colorRampPalette(color_ramp)(length(colorbar_seq))

  mar_default <- if (horizontal) c(2, 1, 1, 1) else c(1, 1, 1, 2)
  mar_with_main <- if (horizontal) c(2, 1, 2, 1) else c(1, 1, 2, 2)
  par(mar = if (!is.null(main)) mar_with_main else mar_default, mgp = c(1.5, 0.5, 0))
  
  
  # image
  if (horizontal) {
    image(
      colorbar_seq, 1,
      matrix(1:length(colorbar_seq), ncol = 1),
      col = colorbar,
      axes = FALSE,
      xlab = "", ylab = "", ...
    )
    axis(1, at = pretty(colorbar_seq, n = 5))
  } else {
    image(
      1, colorbar_seq,
      matrix(1:length(colorbar_seq), nrow = 1),
      col = colorbar,
      axes = FALSE,
      xlab = "", ylab = "", ...
    )
    axis(4, at = pretty(colorbar_seq, n = 5))
  }

  # box around the plot
  box(which = "plot")

  # # add label if specified
  if (!is.null(main)) title(main = main, line = 0.8)

  # keep color vector for re-use
  invisible(colorbar)
}


# save plot ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# # save pdf plot 1 x 7 for horizontal
# pdf("figures/colorbar_horizontal.pdf", height = 2, width = 7)
# add_colorbar(
#   y_range = c(-5, 5),
#   color_ramp = c("darkblue", "blue", "gray", "orange", "darkorange"),
#   horizontal = TRUE,
#   main = "z-score"
# )
# dev.off()
# 
# # save pdf plot 7 x 1 for vertical
# # pdf("figures/colorbar_vertical.pdf", height = 7, width = 1)
# add_colorbar(horizontal = FALSE)
# # dev.off()
