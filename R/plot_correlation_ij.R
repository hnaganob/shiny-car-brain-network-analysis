plot_correlation_ij <- function(
    signal,
    adjacency_matrix,
    display_neighbor_points = TRUE,
    display_boxplot = TRUE,
    display_points = FALSE,
    jitter = FALSE,
    ...) {
  # get off-diagonal correlations
  cor_ij <- cor(signal)
  diag(cor_ij) <- NA

  # get correlation of neighbors
  cor_ij_neighbor <- adjacency_matrix * cor_ij

  # convert 0 to NA to hide in points plot
  cor_ij_neighbor[cor_ij_neighbor == 0] <- NA

  # prepare jitter
  if (jitter) {
    jitter_points <- rnorm(nrow(cor_ij), 0, 0.05)
  } else {
    jitter_points <- rep(0, nrow(cor_ij))
  }

  # base plot (empty)
  boxplot(
    cor_ij,
    frame.plot = FALSE,
    ylab = "Correlation",
    ylim = c(-1, 1),
    col = 0,
    border = 0,
    las = 3
  )

  # add null line
  abline(h = 0, col = "gray50", lty = 3)

  # boxplot at each roi
  if (display_boxplot) {
    boxplot(
      cor_ij,
      frame.plot = FALSE,
      add = TRUE,
      notch = TRUE,
      pch = "-",
      xaxt = "n", yaxt = "n",
      col = 0,
      border = "gray30"
    )
  }

  # plot all correlation points
  if (display_points) {
    for (i in 1:ncol(cor_ij)) {
      points(
        rep(i, ncol(cor_ij)) + jitter_points,
        cor_ij[, i],
        col = adjustcolor(1, alpha.f = 0.2),
        cex = 1.3,
        ...
      )
    }
  }

  # plot only neighbors
  if (display_neighbor_points) {
    for (i in 1:ncol(cor_ij)) {
      points(
        rep(i, ncol(cor_ij)) + jitter_points,
        cor_ij_neighbor[, i],
        bg = adjustcolor(2, alpha.f = 0.8),
        pch = 21,
        cex = 1.3,
        ...
      )
    }
  }

  box(which = "plot")
}


# plot_correlation_ij(
#   signal = scale(signal),
#   adjacency_matrix = adj_dti,
#   display_points = TRUE,
#   display_boxplot = FALSE,
#   jitter = TRUE
# )

# 
# # Get row medians (ignoring NA)
# row_medians <- apply(cor_ij, 1, median, na.rm = TRUE)
# 
# # Replicate medians into a matrix
# median_matrix <- matrix(row_medians, nrow = nrow(cor_ij), ncol = ncol(cor_ij))
# 
# # Compare element-wise (broadcasted row-wise)
# logical_matrix <- cor_ij_filtered > median_matrix
# rowSums(logical_matrix, na.rm = TRUE) 
# 
# 
# rowSums(adj_dti, na.rm = TRUE)
# 
# sum(rowSums(logical_matrix, na.rm = TRUE) ) /
# sum(rowSums(adj_dti, na.rm = TRUE))


