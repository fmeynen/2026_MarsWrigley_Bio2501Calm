rm(list = ls())

library(ggplot2)
library(ordinal)

data_loc   <- "data/processed"
files_list <- list.files(data_loc)
data_list  <- lapply(files_list, function(f) {
  readRDS(paste0(data_loc, "/", f))
})
names(data_list) <- files_list
rm(data_loc, files_list)

lapply(seq_along(data_list), function(i){
  data_list[[i]][which(is.na(data_list[[i]])) %% 100,]
})

lapply(seq_along(data_list), function(i){
  var <- names(data_list)[i]
  data <- data_list[[i]]
  
  ggplot(data = data, aes(x = Baseline, y = Treatment, color = SPID)) +
    geom_point() +
    ggtitle(var)
})


lapply(seq_along(data_list), function(i){
  var <- names(data_list)[i]
  data <- data_list[[i]]
  
  ggplot(data = data, aes(x = Baseline, y = Treatment, color = SPID)) +
    geom_point() +
    ggtitle(var)
})

names(data_list)

lapply(seq(from = 1, to = 21), function(i){
  var <- names(data_list)[i]
  data <- data_list[[i]]
  model <- lm(Treatment ~ SPID + as.numeric(Baseline) + age + sex, data = data)
  cat(var)
  plot(model)
})

?ks.test()

lapply(seq(from = 22, to = 24), function(i){
  var <- names(data_list)[i]
  data <- data_list[[i]]
  model <- clm(Treatment ~ SPID + Baseline + age + sex, data = data)
  plot(model)
})