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


order_mood <- c("CALM", "ENERGETIC", "IRRITATED", "LETHARGIC", "LISTLESS", "LIVELY",
                "NERVOUS", "RELAXED", "FOCUSED", "ANXIOUS",
                "AP", "nAnP", "nAP", "AnP",
                "PLEASURE", "AROUSAL", "VITALITY", "STABILITY")
order_stai <- c("CALM", "TENSE", "UPSET", "RELAXED", "CONTENT", "WORRIED", "STAI")

spid_list <- spid_summary |> 
  select(estimate, p_value, term, dataset, model_type) |> 
  mutate(adjusted_p = p.adjust(p_value, method = "BH")) |>
  mutate(exp_estimate = ifelse(model_type == "clm", exp(estimate), NA),
         treatment = gsub("^SPID", "", term),
         variable = gsub("^data_|\\.rds$", "", dataset)) |>
  select(-term, -dataset) |> 
  mutate(order = match(str_remove(variable, "stai_"), order_stai)) |> 
  arrange(coalesce(order, Inf)) |> 
  select(-order) |>
  mutate(order = match(str_remove(variable, "mood_"), order_mood)) |> 
  arrange(coalesce(order, Inf)) |> 
  select(variable, treatment, estimate, exp_estimate, p_value, adjusted_p) |> 
  mutate(across(where(is.numeric), ~ signif(.x, digits = 4))) |> 
  group_by(treatment) |> 
  group_split() 
names(spid_list) <- c("P", "R", "S", "T")

spid_list <- lapply(spid_list, function(x) x |> select(-treatment))
spid_list

lapply(seq_along(spid_list), function(x) {
  saveRDS(spid_list[[x]], file = paste0("results/tables/spid_", names(spid_list)[x], ".rds"))
})

# Scratchpad ------------------------------------------------------------------------------------------------------
lapply(seq_along(results), function(df){
  paste0(names(results)[[df]], ", ", results[[df]]$model_type)
})

results[[1]]$model_type


View(pivot_wider(spid_summary, names_from = term, values_from = c(1:4)))




