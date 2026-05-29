
# Setup -----------------------------------------------------------------------------------------------------------
rm(list = ls())
# libraries -------------------------------------------------------------------------------------------------------

library(haven) # read sas files
library(readr) 
library(tidyverse)
library(ordinal)

# Helper Functions ------------------------------------------------------------------------------------------------

source(here::here("scripts", "helpers.R"))

# Data Management --------------------------------------------------------------------------------------------------


## Load Data -------------------------------------------------------------------------------------------------------

#overall data
data_loc   <- "data/raw/Bio2501_Data_Transfer/Processed_SAS/"
files_list <- list.files(data_loc)
data_list  <- lapply(files_list, function(f) {
  read_sas(paste0(data_loc, "/", f))
})
names(data_list) <- sub("\\.sas7bdat$", "", files_list)
rm(data_loc, files_list)


# Patient Characteristics
patient_chars <- data_list$alld |>
  select(SubjectID, age, sex, SPID) |> 
  mutate(sex = as.factor(sex),
         SPID = as.factor(SPID)) |> 
  mutate(SPID = relevel(SPID, ref = "Q"))


## HR --------------------------------------------------------------------------------------------------------------


## Mood ------------------------------------------------------------------------------------------------------------
##!! ATTENTION:CHECK Assuming _b is baseline and other is treatment !!

mood_treatment <- data_list$mood_ra |> 
  select(SubjectID, scale, Pre, Post) |> 
  mutate(diff = as.integer(Post - Pre),
         scale = as.factor(scale)) |> 
  filter(!is.na(diff)) |>
  select(SubjectID, scale, diff) |> 
  pivot_wider(names_from = scale, values_from = diff) |> 
  rename_with(~paste0(., "_Treatment"), - SubjectID)

mood_diff <- data_list$mood_ra_b |> 
  select(SubjectID, scale, Pre, Post) |> 
  mutate(diff = as.integer(Post - Pre),
         scale = as.factor(scale)) |> 
  filter(!is.na(diff)) |>
  select(SubjectID, scale, diff) |> 
  pivot_wider(names_from = scale, values_from = diff) |> 
  rename_with(~paste0(., "_Baseline"), - SubjectID) |> 
  left_join(mood_treatment) |> 
  droplevels()

vars <- c("CALM", "ENERGETIC", "IRRITATED", "LETHARGIC", "LISTLESS", "LIVELY",
          "NERVOUS", "RELAXED", "FOCUSED", "ANXIOUS",
          "AP", "nAnP", "nAP", "AnP",
          "PLEASURE", "AROUSAL", "VITALITY", "STABILITY")

# Create a named list of per-variable dataframes
mood_diff_list <- lapply(vars, function(v) {
  mood_diff  |>
    select(SubjectID, matches(paste0("^", v, "_(Baseline|Treatment)$"))) |>
    rename_with(~ sub(paste0("^", v, "_"), "", .x), matches(paste0("^", v, "_"))) |>
    left_join(patient_chars)
})
names(mood_diff_list) <- vars

#save data
lapply(names(mood_diff_list), function(x) {
  saveRDS(mood_diff_list[[x]], file = paste0("data/processed/data_mood_", x, ".rds" ))
})
lapply(names(mood_diff_list), function(x) {
  saveRDS(anonymize_data(mood_diff_list[[x]]), file = paste0("data/test/data_mood_", x, ".rds" ))
})
# because of narrow range of results, we will handle the outcome as an ordered factor



## NASA ------------------------------------------------------------------------------------------------------------

nasa_diff <- data_list$nasa_ra |> 
  select(SubjectID, period, score, Post_MTF, Pre_MTF) |> 
  mutate(diff = Post_MTF - Pre_MTF) |> 
  select(SubjectID, period, score, diff) |> 
  pivot_wider(names_from = period, values_from = diff) |> 
  pivot_wider(names_from = score, values_from = c(Baseline, Treatment))

vars <- c("Raw_Score", "Weighted_Score")
nasa_diff_list <- lapply(vars, function(v) {
  nasa_diff  |>
    select(SubjectID, matches(paste0("^", "(Baseline|Treatment)", "_", v, "$"))) |>
    rename_with(~ sub(paste0("_", v, "$"), "", .x), matches(paste0("_", v, "$"))) |>
    left_join(patient_chars)
})
names(nasa_diff_list) <- vars

lapply(names(nasa_diff_list), function(x) {
  saveRDS(nasa_diff_list[[x]], file = paste0("data/processed/data_nasa_", x, ".rds" ))
})
lapply(names(nasa_diff_list), function(x) {
  saveRDS(anonymize_data(nasa_diff_list[[x]]), file = paste0("data/test/data_nasa_", x, ".rds" ))
}) 

## Saliva ----------------------------------------------------------------------------------------------------------

saliva_diff <- data_list$saliva |> 
  mutate(diff = Post - Pre) |> 
  select(SubjectID, Period, diff) |> 
  pivot_wider(names_from = Period, values_from = diff) |>
  left_join(patient_chars) 

saliva_diff[which(is.na(saliva_diff))%%100,]

saveRDS(saliva_diff, file = "data/processed/data_saliva.rds")
saveRDS(anonymize_data(saliva_diff), file = "data/test/data_saliva.rds")  


## STAI ------------------------------------------------------------------------------------------------------------
stai <- readr::read_csv("data/raw/BIO2501_Data_Transfer/Raw Data Files/STAI_Compiled.csv")
stai_diff <- stai |>
  separate(TP, into = c("Condition", "Timing"), sep = " - ") |>
  pivot_wider(
    names_from  = Timing,
    values_from = c(CALM, TENSE, UPSET, RELAXED, CONTENT, WORRIED, STAI)
  ) |>
  mutate(
    CALM    = as.ordered(CALM_Post    - CALM_Pre),
    TENSE   = as.ordered(TENSE_Post   - TENSE_Pre),
    UPSET   = as.ordered(UPSET_Post   - UPSET_Pre),
    RELAXED = as.ordered(RELAXED_Post - RELAXED_Pre),
    CONTENT = as.ordered(CONTENT_Post - CONTENT_Pre),
    WORRIED = as.ordered(WORRIED_Post - WORRIED_Pre),
    STAI    = as.integer(STAI_Post    - STAI_Pre)
  ) |> 
  select(SubjectID, Condition, CALM, TENSE, UPSET, RELAXED, CONTENT, WORRIED, STAI) |>
  pivot_wider(
    names_from = Condition,
    values_from = c(CALM, TENSE, UPSET, RELAXED, CONTENT, WORRIED, STAI)
  ) |>
  mutate(across(ends_with("_Baseline"), as.integer)) |>
  droplevels()

vars <- c("CALM", "TENSE", "UPSET", "RELAXED", "CONTENT", "WORRIED", "STAI")
stai_diff_list <- lapply(vars, function(v) {
  stai_diff  |>
    select(SubjectID, matches(paste0("^", v, "_(Baseline|Treatment)$"))) |>
    rename_with(~ sub(paste0("^", v, "_"), "", .x), matches(paste0("^", v, "_"))) |>
    left_join(patient_chars)
})
names(stai_diff_list) <- vars

lapply(names(stai_diff_list), function(x) {
  saveRDS(stai_diff_list[[x]], file = paste0("data/processed/data_stai_", x, ".rds" ))
})
lapply(names(stai_diff_list), function(x) {
  saveRDS(anonymize_data(stai_diff_list[[x]]), file = paste0("data/test/data_stai_", x, ".rds" ))
})

## VAS -------------------------------------------------------------------------------------------------------------
##!! ATTENTION:CHECK Assuming _b is baseline and other is treatment !!
vas_baseline <- data_list$vas_b |> 
  select(SubjectID, CHG) |> 
  rename(VAS_Baseline = CHG)

vas_diff <- data_list$vas_ra |> 
  select(SubjectID, CHG) |> 
  rename(VAS_Treatment = CHG) |> 
  left_join(vas_baseline) |> 
  left_join(patient_chars) |> 
  rename(Treatment = VAS_Treatment,
         Baseline = VAS_Baseline) 

saveRDS(vas_diff, file = "data/processed/data_vas.rds")
saveRDS(anonymize_data(vas_diff), file = "data/test/data_vas.rds")



# Scratchpad ------------------------------------------------------------------------------------------------------

order_mood <- c("CALM", "ENERGETIC", "IRRITATED", "LETHARGIC", "LISTLESS", "LIVELY",
                "NERVOUS", "RELAXED", "FOCUSED", "ANXIOUS",
                "AP", "nAnP", "nAP", "AnP",
                "PLEASURE", "AROUSAL", "VITALITY", "STABILITY")

sum_list_mood <- data_list$mood_ra |> 
  select(SubjectID, scale, Pre, Post, SPID) |>
  rename(Pre_Treatment = Pre,
         Post_Treatment = Post) |> 
  left_join(data_list$mood_ra_b) |> 
  rename(Pre_Baseline = Pre,
         Post_Baseline = Post,
         variable = scale) |> 
  filter(variable != "") |> 
  left_join(patient_chars) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_by(variable,SPID) |> 
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 4))) |> 
  ungroup() |> 
  mutate(order = match(variable, order_mood)) |> 
  arrange(coalesce(order, Inf)) |> 
  select(-order) |> 
  group_by(SPID) |>
  mutate(variable = paste0("mood_", variable)) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_split()

sum_list_nasa <- data_list$nasa_ra |> 
  select(SubjectID, period, score, Post_MTF, Pre_MTF, SPID) |>
  pivot_wider(names_from = period, values_from = c(Post_MTF, Pre_MTF)) |> 
  rename(Post_Baseline  = Post_MTF_Baseline,
         Post_Treatment = Post_MTF_Treatment,
         Pre_Baseline   = Pre_MTF_Baseline,
         Pre_Treatment  = Pre_MTF_Treatment,
         variable       = score) |> 
  select(-SubjectID) |> 
  group_by(variable, SPID) |> 
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 4))) |>
  ungroup() |> 
  group_by(SPID) |> 
  mutate(variable = paste0("nasa_", variable)) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_split()

sum_list_saliva <- data_list$saliva |> 
  select(SubjectID, Period, Post, Pre, SPID) |>
  mutate(variable = "saliva") |> 
  pivot_wider(names_from = Period, values_from = c(Post, Pre)) |> 
  select(-SubjectID) |> 
  group_by(variable, SPID) |> 
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 4))) |>
  ungroup() |> 
  group_by(SPID) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_split()

order_stai <- c("CALM", "TENSE", "UPSET", "RELAXED", "CONTENT", "WORRIED", "STAI")
sum_list_stai <- stai |>
  pivot_longer(cols = c(CALM, TENSE, UPSET, RELAXED, CONTENT, WORRIED, STAI)) |> 
  left_join(patient_chars) |> 
  separate(TP, into = c("Condition", "Timing"), sep = " - ") |> 
  pivot_wider(names_from = Timing, values_from = value) |> 
  pivot_wider(names_from = Condition, values_from = c(Pre, Post)) |> 
  rename(variable = name) |> 
  mutate(order = match(variable, order_stai)) |> 
  arrange(coalesce(order, Inf)) |> 
  select(-order) |>
  select(-SubjectID, -sex) |> 
  group_by(variable, SPID) |> 
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 4))) |>
  ungroup() |> 
  group_by(SPID) |> 
  mutate(variable = paste0("stai_", variable)) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_split()


sum_list_vas <- data_list$vas_b |> 
  select(SubjectID, SPID, Pre, Post) |> 
  rename(Pre_Baseline = Pre,
         Post_Baseline = Post) |> 
  mutate(variable = "vas") |> 
  left_join(data_list$vas_ra) |> 
  rename(Pre_Treatment = Pre,
         Post_Treatment = Post) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_by(variable, SPID) |> 
  summarise(across(everything(), ~ round(mean(.x, na.rm = TRUE), 4))) |>
  ungroup() |> 
  group_by(SPID) |> 
  select(variable, SPID, Pre_Baseline, Post_Baseline, Pre_Treatment, Post_Treatment) |> 
  group_split()

sum_vars_by_spid <- Map(bind_rows,
                        sum_list_mood, sum_list_nasa, sum_list_saliva, sum_list_stai, sum_list_vas)

names(sum_vars_by_spid) <- LETTERS[16:20]
saveRDS(sum_vars_by_spid, file = "results/tables/sum_vars_by_spid.rds")
