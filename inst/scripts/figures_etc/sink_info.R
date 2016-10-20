model_names <- c("bsm_1000_iter", "bsm_noweight_1000_iter", "loocvm_10_iter", "loocvm_noweight_10_iter")


sink_summary <- function(object, sink_output = TRUE) {
  if (sink_output) sink(file.path(current_out_dir, "info"))
  cat("Model: ")
  cat(model_name)
  # print(Sys.time())
  # print(model_name)
  # print(sample_iter)
  # print(brt_params)
  # sink()
}
