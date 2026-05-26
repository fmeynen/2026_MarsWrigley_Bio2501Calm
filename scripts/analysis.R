rm(list = ls())


# Load Data -------------------------------------------------------------------------------------------------------

data_loc   <- "data/test"
files_list <- list.files(data_loc)
data_list  <- lapply(files_list, function(f) {
  readRDS(paste0(data_loc, "/", f))
})
names(data_list) <- files_list
rm(data_loc, files_list)


# Libraries -------------------------------------------------------------------------------------------------------

library(ordinal)
library(sure)

# For all analyses, include baseline, age, sex and SPID as covariates

# Mood ------------------------------------------------------------------------------------------------------------
#outcome are integers. Use a simple linear model, unless the range of outcomes is too small, in which case it is
#better to use a ordinal regression (cumulative link model clm)


# Nasa ------------------------------------------------------------------------------------------------------------
# outcome are continuous, use lm
data_nasa_raw <- data_list$data_nasa_Raw_Score.rds
model_nasa_raw <- lm(Treatment ~ Baseline + SPID + age + sex, data = data_nasa_raw)
summary(model_nasa_raw)
plot(model_nasa_raw)

data_nasa_weighted <- data_list$data_nasa_Weighted_Score.rds
model_nasa_weighted <- lm(Treatment ~ Baseline + SPID + age + sex, data = data_nasa_weighted)
summary(model_nasa_weighted)
plot(model_nasa_weighted)

# Saliva ----------------------------------------------------------------------------------------------------------
# outcome are continuous, use lm
data_saliva <- data_list$data_saliva.rds
model_saliva <- lm(Treatment ~ Baseline + SPID + age + sex, data = data_saliva)
summary(model_saliva)
plot(model_saliva)

# STAI ------------------------------------------------------------------------------------------------------------
#outcome are integers and range is very small, opt to use an ordinal regression (cumulative link model clm)
data_stai_calm <- data_list$data_stai_CALM.rds
model_stai_calm <- clm(Treatment ~ Baseline + SPID + age + sex, data = data_stai_calm, link = "logit")
summary(model_stai_calm)
nominal_test(model_stai_calm)
scale_test(model_stai_calm)
sure::autoplot.clm(model_stai_calm, what = "qq")

# VAS --------------------------------------------------------------------------------------------------------------
# outcome are continous, use  lm

data_vas <- data_list$data_vas.rds
model_vas <- lm(Treatment ~ Baseline + SPID + age + sex, data = data_vas)
summary(model_vas)
plot(model_vas)


