source("global.R")

# prepare spdep model parameter table ----
#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
model_parameter_table <- get_spdep_model_parameters(
  time = time,
  sp_data = scale(signal),
  listw = listw
)

# save spdep model parameter table in data folder
folder <- "data"
filename <- paste0(folder, "/model_parameter_table.csv")
write.csv(model_parameter_table, filename, row.names = FALSE)
