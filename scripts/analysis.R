rm(list = ls())

# Libraries --------------------------------------------------------------------------------------------------------
library(ordinal)
library(sure)
library(tidyr)

# Settings ---------------------------------------------------------------------------------------------------------
data_loc <- "data/processed"
# Treatment is treated as ordinal when it is integer-valued with at most this many unique observed values
narrow_cutoff <- 6

# Load Data --------------------------------------------------------------------------------------------------------
files_list <- list.files(data_loc, pattern = "\\.rds$")
data_list  <- lapply(files_list, function(f) readRDS(file.path(data_loc, f)))
names(data_list) <- files_list
rm(data_loc, files_list)

# Helper Functions -------------------------------------------------------------------------------------------------
source(here::here("scripts", "helpers.R"))

# Run Analysis ----------------------------------------------------------------------------------------------------

results <- lapply(names(data_list), function(nm) {
  analyze_dataset(data_list[[nm]], nm)
})
names(results) <- names(data_list)


# Inspect Diagnostics ----------------------------------------------------------------------------------------------

lapply(names(results), function(nm) {
  cat("\n---", nm, "(", results[[nm]]$model_type, ") ---\n")
  if (results[[nm]]$model_type == "lm") {
    plot(results[[nm]]$fit, main = nm, which = 1)
    plot(results[[nm]]$fit, main = nm, which = 2)
    plot(results[[nm]]$fit, main = nm, which = 3)
  } else {
    print(results[[nm]]$diagnostics$summary)
    print(results[[nm]]$diagnostics$nominal_test)
    print(results[[nm]]$diagnostics$scale_test)
    print(results[[nm]]$diagnostics$qq_plot)
  }
})


# Combined SPID Summary Table --------------------------------------------------------------------------------------

spid_summary <- do.call(rbind, lapply(results, function(r) r$spid_coef))
rownames(spid_summary) <- NULL

print(spid_summary)

spid_summary |> 
  select(estimate, p_value, term, dataset, model_type) |> 
  mutate(adjusted_p = p.adjust(p_value, method = "BH")) |> 
  pivot_wider(names_from = term, values_from = c(1,2,6)) |> 
  select(c(1,2,3,7,11,4,8,12,5,9,13,6,10,14))

# Scratchpad ------------------------------------------------------------------------------------------------------
lapply(seq_along(results), function(df){
  paste0(names(results)[[df]], ", ", results[[df]]$model_type)
})

results[[1]]$model_type


View(pivot_wider(spid_summary, names_from = term, values_from = c(1:4)))




