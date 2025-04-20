library(shiny)
library(spdep)
library(spatialreg)
library(network)
library(bslib) # nice shiny
# library(viridis) # colorblind-friendly colors

source("R/get_spdep_model_parameters.R")
source("R/plot_lambda_with_se.R")
source("R/plot_network_signal.R")
source("R/add_colorbar.R")
source("R/plot_correlation_ij.R")


# load data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# bold_raw <- read.csv("data/BOLD.csv")
# dti_raw <- read.csv("data/DTI.csv")

# bold signals
signal <- as.matrix(read.csv("data/s.csv", header = FALSE)) # 68 x 240

# dti number of fibers
dti <- as.matrix(read.csv("data/M_nf.csv", header = FALSE)) # 68 x 68

# names of roi
# http://www.ncbi.nlm.nih.gov/pmc/articles/PMC3500617/table/T1/
roi_table <- read.csv("data/roi.csv", header = TRUE) # 68 x 8






# process network data ----
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

# ~~~~~~~~~~~~~~~~~~
# signal
# ~~~~~~~~~~~~~~~~~~
# bold signal time
time <- seq(2, 480, by = 2) # TR = 2 sec

# number of roi
n_roi <- dim(roi_table)[1]

# number of frame
n_time <- length(time)

# signal time series
signal <- ts(t(signal), start = 2, end = 480, deltat = 2)
colnames(signal) <- roi_table$Code.Full


# ~~~~~~~~~~~~~~~~~~
# dti
# ~~~~~~~~~~~~~~~~~~
# convert data structure to matrix
rownames(dti) <- colnames(dti) <- colnames(signal)

# adjacency matrix
# solve(I - phi * H) %*% diag(sigma2)
thr <- 500
adj_dti <- 1 * (dti > thr)
diag(adj_dti) <- 0

# # check
# heatmap(adj_dti, Rowv = NA, symm = TRUE)
# # isSymmetric(adj_dti)  # TRUE

# convert to a spatial weights list ----
listw <- mat2listw(adj_dti, style = "B", zero.policy = TRUE) # "B" keeps binary structure





# # prepare spdep model parameter table ----
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
model_parameter_table <- read.csv("output/model_parameter_table.csv", header = TRUE)

# folder <- "output"
# filename <- paste0(folder, "/model_parameter_table.csv")
# 
# if (length(list.files(folder)) != 0) {
#   # load spdep model parameter table
  # model_parameter_table <- read.csv(filename, header = TRUE)
# } else {
#   # make spdep model parameter table
  # model_parameter_table <- get_spdep_model_parameters(
#     time = time,
#     sp_data = scale(signal),
#     listw = listw
#   )
#   # save spdep model parameter table in output folder
#   dir.create(folder)
#   write.csv(model_parameter_table, filename, row.names = FALSE)
# }
