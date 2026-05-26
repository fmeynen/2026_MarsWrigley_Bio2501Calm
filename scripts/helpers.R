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