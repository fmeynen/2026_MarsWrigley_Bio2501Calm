#This is a function that anonymizes data while keeping the data structure, so that I can create test data that feeds
#into AI / agent services such as copilot.
anonymize_data <- function(
    data,
    n_sample    = NULL,   # NULL = keep all rows
    cluster_var = NULL,   # character: name of cluster/grouping column
    n_clusters  = NULL,   # NULL = keep all clusters; integer to subsample
    preserve_na = TRUE,
    seed        = 42
) {
  if (!is.data.frame(data)) stop("`data` must be a data.frame or tibble.")
  set.seed(seed)
  df <- as.data.frame(data)
  
  # ---- Cluster-aware row selection -----------------------------------
  if (!is.null(cluster_var)) {
    if (!cluster_var %in% names(df))
      stop(sprintf("cluster_var '%s' not found in data.", cluster_var))
    
    all_clusters <- unique(df[[cluster_var]])
    
    # Subsample clusters if requested, otherwise keep all
    sel_clusters <- if (!is.null(n_clusters)) {
      n_clusters <- min(n_clusters, length(all_clusters))
      sample(all_clusters, size = n_clusters)
    } else {
      all_clusters
    }
    
    df <- df[df[[cluster_var]] %in% sel_clusters, , drop = FALSE]
    
  } else if (!is.null(n_sample)) {
    # Non-clustered row subsampling
    idx <- sample(nrow(df), size = min(n_sample, nrow(df)))
    df  <- df[idx, , drop = FALSE]
  }
  # else: n_sample = NULL and no cluster_var → keep all rows
  
  # ---- Helper: letter labels for any cardinality ----------------------
  make_labels <- function(n) {
    if (n <= 26) return(LETTERS[seq_len(n)])
    c(LETTERS, paste0("L", seq_len(n - 26)))
  }
  
  # ---- Anonymize each column -----------------------------------------
  for (col in names(df)) {
    x    <- df[[col]]
    miss <- is.na(x)
    n    <- sum(!miss)
    
    if (is.factor(x) || is.character(x)) {
      # Includes cluster_var itself — cluster IDs become C1, C2, ...
      lvls    <- if (is.factor(x)) levels(x) else sort(unique(na.omit(x)))
      new_lvl <- if (!is.null(cluster_var) && col == cluster_var) {
        paste0("C", seq_along(lvls))   # C1, C2, ... for cluster IDs
      } else {
        make_labels(length(lvls))       # A, B, C, ... for regular factors
      }
      map   <- setNames(new_lvl, lvls)
      x_new <- unname(map[as.character(x)])
      if (preserve_na) x_new[miss] <- NA
      df[[col]] <- factor(x_new, levels = new_lvl)
      
    } else if (is.logical(x)) {
      p     <- mean(x, na.rm = TRUE)
      x_new <- as.logical(rbinom(length(x), size = 1, prob = p))
      if (preserve_na) x_new[miss] <- NA
      df[[col]] <- x_new
      
    } else if (is.integer(x)) {
      x_new <- as.integer(round(rnorm(length(x))))
      if (preserve_na) x_new[miss] <- NA_integer_
      df[[col]] <- x_new
      
    } else if (is.numeric(x)) {
      x_new <- rnorm(length(x))
      if (preserve_na) x_new[miss] <- NA_real_
      df[[col]] <- x_new
      
    } else if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) {
      origin <- as.Date("2000-01-01")
      x_new  <- origin + sample(0:364, length(x), replace = TRUE)
      if (preserve_na) x_new[miss] <- NA
      df[[col]] <- x_new
      
    } else {
      warning(sprintf(
        "Column '%s' has unhandled type (%s); left unchanged.",
        col, class(x)[1]
      ))
    }
  }
  
  # ---- Attach summary as attribute -----------------------------------
  attr(df, "anonymization_summary") <- data.frame(
    column        = names(df),
    original_type = sapply(as.data.frame(data[1, , drop = FALSE]),
                           function(v) class(v)[1]),
    anon_type     = sapply(df, function(v) class(v)[1]),
    n_missing     = colSums(is.na(df)),
    row.names     = NULL
  )
  
  message(sprintf(
    "Anonymized: %d rows, %d cols (input was %d x %d)%s",
    nrow(df), ncol(df), nrow(data), ncol(data),
    if (!is.null(cluster_var))
      sprintf("; %d clusters kept", length(unique(df[[cluster_var]])))
    else ""
  ))
  
  df
}



# Return "clm" or "lm" based on the Treatment variable and the narrow-range cutoff.
classify_outcome <- function(treatment, cutoff) {
  if (is.ordered(treatment)) return("clm")
  if (is.integer(treatment) && length(unique(treatment)) <= cutoff) return("clm")
  "lm"
}

# Coerce Treatment to an ordered factor if it is not already (required by clm).
prepare_treatment <- function(treatment) {
  if (is.ordered(treatment)) return(treatment)
  ordered(treatment, levels = sort(unique(treatment)))
}

# Extract SPID coefficients from a fitted model and return a tidy data frame.
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

# Analyse one dataset: classify outcome, fit the appropriate model, run diagnostics,
# and extract SPID regression coefficients.
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

# Create tables for the report
create_table <- function(condition) {
  df <- readRDS(here::here("results", "tables", paste0("spid_", condition, ".rds")))
  
  df_table <- df |>
    mutate(
      group = case_when(
        str_starts(variable, "mood_")  ~ "Mood",
        str_starts(variable, "stai_")  ~ "STAI",
        str_starts(variable, "nasa_")  ~ "NASA",
        variable == "saliva"           ~ "Saliva",
        variable == "vas"              ~ "VAS"
      ),
      variable = str_remove(variable, "^(mood|stai|nasa)_")
    ) |>
    mutate(across(c(estimate, exp_estimate, p_value, adjusted_p),
                  \(x) round(x, 3))) |>
    mutate(group = factor(group, levels = c("Mood", "STAI", "NASA", "Saliva", "VAS"))) |>
    arrange(group)
  
  df_grouped <- as_grouped_data(df_table, groups = c("group"))
  
  as_flextable(df_grouped) |>
    set_header_labels(
      variable     = " ",
      estimate     = "Estimate",
      exp_estimate = "Exp(Estimate)",
      p_value      = "p-value",
      adjusted_p   = "Adjusted p"
    ) |>
    colformat_double(
      j      = c("estimate", "exp_estimate", "p_value", "adjusted_p"),
      digits = 3,
      na_str = ""
    ) |>
    bold(i = ~ !is.na(p_value) & p_value < 0.05) |>
    bg(i = ~ !is.na(group), bg = "#f2f2f2", part = "body") |>
    autofit()
}
create_sum_table <- function(condition) {
  list <- readRDS(here::here("results", "tables", "sum_vars_by_spid.rds"))
  df <- list[[condition]]
  
  df_table <- df |>
    select(-SPID) |> 
    mutate(
      group = case_when(
        str_starts(variable, "mood_")  ~ "Mood",
        str_starts(variable, "stai_")  ~ "STAI",
        str_starts(variable, "nasa_")  ~ "NASA",
        variable == "saliva"           ~ "Saliva",
        variable == "vas"              ~ "VAS"
      ),
      variable = str_remove(variable, "^(mood|stai|nasa)_"),
    ) |>
    mutate(group = factor(group, levels = c("Mood", "STAI", "NASA", "Saliva", "VAS"))) |>
    arrange(group)
  
  df_grouped <- as_grouped_data(df_table, groups = c("group"))
  
  as_flextable(df_grouped) |>
    set_header_labels(
      variable       = " ",
      Pre_Baseline   = "Baseline Pre",
      Post_Baseline  = "Baseline Post",
      Pre_Treatment  = "Treatment Pre",
      Post_Treatment = "Treatment Post"
    ) |>
    colformat_double(
      j      = c("Pre_Baseline", "Post_Baseline", "Pre_Treatment", "Post_Treatment"),
      digits = 4,
      na_str = ""
    ) |>
    bg(i = ~ !is.na(group), bg = "#f2f2f2", part = "body") |>
    autofit()
}
