rm(list = ls())


# Libraries --------------------------------------------------------------------------------------------------------

library(ordinal)
library(sure)


# Settings ---------------------------------------------------------------------------------------------------------

# Folder containing the .rds data files to analyse
data_loc <- "data/test"

# Treatment is treated as ordinal when it is integer-valued with at most this many unique observed values
narrow_cutoff <- 5


# Load Data --------------------------------------------------------------------------------------------------------

files_list <- list.files(data_loc, pattern = "\\.rds$")
data_list  <- lapply(files_list, function(f) readRDS(file.path(data_loc, f)))
names(data_list) <- files_list
rm(data_loc, files_list)


# Helper Functions -------------------------------------------------------------------------------------------------

# Return "clm" or "lm" based on the Treatment variable and the narrow-range cutoff
classify_outcome <- function(treatment, cutoff) {
  if (is.ordered(treatment)) return("clm")
  if (is.integer(treatment) && length(unique(treatment)) <= cutoff) return("clm")
  "lm"
}

# Coerce Treatment to an ordered factor if it is not already
prepare_treatment <- function(treatment) {
  if (is.ordered(treatment)) return(treatment)
  ordered(treatment, levels = sort(unique(treatment)))
}

# Extract SPID coefficients and return a tidy data frame with standardised column names
extract_spid_coef <- function(fit, model_type, dataset_name) {
  coef_table <- coef(summary(fit))
  spid_rows  <- grepl("^SPID", rownames(coef_table))
  spid_coef  <- as.data.frame(coef_table[spid_rows, , drop = FALSE])
  colnames(spid_coef) <- c("estimate", "std_error", "statistic", "p_value")
  spid_coef$term       <- rownames(spid_coef)
  spid_coef$dataset    <- dataset_name
  spid_coef$model_type <- model_type
  rownames(spid_coef)  <- NULL
  spid_coef
}

# Analyse one dataset: classify, fit, run diagnostics, extract SPID coefficients
analyze_dataset <- function(df, dataset_name, cutoff = narrow_cutoff) {
  formula    <- Treatment ~ Baseline + age + sex + SPID
  model_type <- classify_outcome(df$Treatment, cutoff)

  if (model_type == "clm") {
    df$Treatment <- prepare_treatment(df$Treatment)
    fit <- clm(formula, data = df, link = "logit")
    diagnostics <- list(
      summary      = summary(fit),
      nominal_test = nominal_test(fit),
      scale_test   = scale_test(fit),
      qq_plot      = sure::autoplot.clm(fit, what = "qq")
    )
  } else {
    fit <- lm(formula, data = df)
    # Standard diagnostic plots are available via plot(results$<dataset>$fit)
    diagnostics <- list(
      summary = summary(fit)
    )
  }

  list(
    model_type  = model_type,
    fit         = fit,
    diagnostics = diagnostics,
    spid_coef   = extract_spid_coef(fit, model_type, dataset_name)
  )
}


# Run Analysis ----------------------------------------------------------------------------------------------------

results <- lapply(names(data_list), function(nm) {
  analyze_dataset(data_list[[nm]], nm)
})
names(results) <- names(data_list)


# Inspect Diagnostics ----------------------------------------------------------------------------------------------

lapply(names(results), function(nm) {
  cat("\n---", nm, "(", results[[nm]]$model_type, ") ---\n")
  if (results[[nm]]$model_type == "lm") {
    plot(results[[nm]]$fit, main = nm)
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

