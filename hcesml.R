# HCES ML Project
# Proxy Means Testing model using logistic regression as a baseline
# and gradient boosted trees for the model

#Libraries
library(haven)     # read_sav, as_factor, zap_labels
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)
library(forcats)
library(janitor)   # clean_names
library(labelled)  # val_labels, look_for
library(readr)

#======================Cleaning=====================================================


raw_dir <- "C:/Users/dioxi/OneDrive/Documents/HCES"
out_dir <- "C:/Users/dioxi/OneDrive/Documents/HCES/HCES ML project"

#Importing files

files <- list(
  "2022-23"=c(l01 = "LEVEL - 01(Section 1 and 1.1).sav",
              l02 = "LEVEL - 02 (Section 3).sav",
              l03 = "LEVEL - 03.sav",
              l04 = "LEVEL - 04 (Section 4.1).sav",
              l05 = "LEVEL_05.sav",
              l06 = "LEVEL - 06 (Section 7).sav",
              l07 = "LEVEL - 07 (Section 4.2).sav",
              l08 = "LEVEL - 08 (Section 8.1).sav",
              l09 = "LEVEL - 09 (Section 9 & 10 & 11).sav",
              l10 = "LEVEL - 10 (Section 12).sav",
              l11 = "LEVEL - 11 (Section 4.3).sav",
              l12 = "LEVEL - 12 (Section 13).sav",
              l13 = "Level - 13 (Section 14).sav",
              l14 = "LEVEL - 14 (Section  A1,B1 & C1).sav",
              l15 = "LEVEL - 15 (Section 1.1, A2,B2 & C2).sav"
              ),
  "2023-24"=c(l01 = "LEVEL - 01(Section 1 and 1.1).sav",
              l02 = "LEVEL - 02 (Section 3).sav",
              l03 = "LEVEL - 03.sav",
              l04 = "LEVEL - 04 (Section 4.1).sav",
              l05 = "LEVEL - 05 ( Sec 5 & 6).sav",
              l06 = "LEVEL - 06 (Section 7).sav",
              l07 = "LEVEL - 07 (Section 4.2).sav",
              l08 = "LEVEL - 08 (Section 8.1).sav",
              l09 = "LEVEL - 09 (Section 9 & 10 & 11).sav",
              l10 = "LEVEL - 10 (Section 12).sav",
              l11 = "LEVEL - 11 (Section 4.3).sav",
              l12 = "LEVEL - 12 (Section 13).sav",
              l13 = "Level - 13 (Section 14).sav",
              l14 = "LEVEL - 14 (Section  A1,B1 & C1).sav",
              l15 = "LEVEL - 15 (Section 1.1, A2,B2 & C2).sav")
)

expected_hh <- c("2022-23" = 261746, "2023-24" = 261953) # from the dictionaries

#Function to read level
read_level <- function(year, level) {
  path <- file.path(raw_dir, year, files[[year]][[level]])
  read_sav(path) %>% clean_names()
}

#Check the function and whether expected numbers line up 
l03 <- map(names(files), \(y) read_level(y, "l03")) %>% set_names(names(files))
stopifnot(nrow(l03[["2022-23"]]) == expected_hh[["2022-23"]],
          nrow(l03[["2023-24"]]) == expected_hh[["2023-24"]])

#Make common names for variables between HCES 2022-23 and 2023-24
#Format: new_name = "name in the file after janitor::clean_names()".

map_2223 <- c(
  fsu = "fsu", su_no = "b1q1pt7", subdiv_no = "b1q1pt10",
  stratum2_no = "b1q1pt11", hh_no = "b1q1pt12",
  sector = "sector", state = "state", district = "district",
  hh_size = "b2q2pt1", any_econ_activity = "b4q4pt1",
  nco = "b4q4pt3", nic = "b4q4pt5", main_income = "b4q4pt6",
  selfemp_sector = "b4q4pt7", wage_sector = "b4q4pt8", casual_sector = "b4q4pt9",
  hh_type = "b4q4pt10", religion = "b4q4pt11", social_group = "b4q4pt12",
  land_owned = "b4q4pt13", land_type = "b4q4pt14", land_acres = "b4q4pt15",
  has_dwelling = "b4q4pt16", dwelling_type = "b4q4pt17",
  cooking_fuel = "b4q4pt21", lighting = "b4q4pt22",
  ration_card = "b4q4pt29", rent_rate_avail = "b4q4pt30", pmgky = "b4q4pt31",
  mult = "mult"
)

map_2324 <- c(
  fsu = "fsu_serial_no", su_no = "sample_su_no", subdiv_no = "sample_sub_division_no",
  stratum2_no = "second_stage_stratum_no", hh_no = "sample_household_no",
  sector = "sector", state = "state", district = "district",
  hh_size = "hh_size_fdq", any_econ_activity = "engaged_in_economic_activity_las",
  nco = "nco_2015_code", nic = "nic_2008_code", main_income = "max_income_activity",
  selfemp_sector = "self_employment_source_sector",
  wage_sector = "regular_wage_source_sector",
  casual_sector = "casual_labour_source_sector",
  hh_type = "household_type", religion = "religion_of_hh_head",
  social_group = "social_group_of_hh_head",
  land_owned = "land_ownership", land_type = "type_of_land_owned",
  land_acres = "total_area_land_owned_acres",
  has_dwelling = "dwelling_unit_exists", dwelling_type = "type_of_dwelling_unit",
  cooking_fuel = "energy_source_cooking", lighting = "energy_source_lighting",
  ration_card = "ration_card_type", rent_rate_avail = "rent_rate_available_rural",
  pmgky = "benefitted_from_pmgky", mult = "multiplier"
)

#Variables that exist only in 2022-23 (kept apart for a full-feature comparison model).
extra_2223 <- c(wall = "b4q4pt18", roof = "b4q4pt19", floor = "b4q4pt20",
                water_source = "b4q4pt23", water_time = "b4q4pt24",
                latrine_access = "b4q4pt25", latrine_type = "b4q4pt26")

id_cols <- c("fsu", "su_no", "subdiv_no", "stratum2_no", "hh_no")

#Standardise function for the dataset: detect missing values and collapse
standardise <- function(df, map, yr) {
  missing <- setdiff(unname(map), names(df))
  if (length(missing) > 0) {
    stop("Not found in ", yr, ": ", paste(missing, collapse = ", "))
  }
  df %>%
    select(all_of(map)) %>%   # a named vector renames: new = old
    mutate(year = yr, .before = 1)
}

#Create key to ID individual households

add_id <- function(df) {
  df |>
    mutate(across(all_of(id_cols), \(x) as.character(zap_labels(x)))) |>
    mutate(hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
           .before = 1)
}

hh22 <- standardise(l03[["2022-23"]], map_2223, "2022-23") %>% add_id()
hh23 <- standardise(l03[["2023-24"]], map_2324, "2023-24") %>% add_id()

#check whether any HH IDs are duplicated
stopifnot(!anyDuplicated(hh22$hh_id), !anyDuplicated(hh23$hh_id))

extra22 <- l03[["2022-23"]] |>
  select(all_of(extra_2223)) |>
  mutate(hh_id = hh22$hh_id, .before = 1)

#Compare value codes across the years: 
#.sav files store value labels as metadata attached to the columns 
#so like. just get the savs next time
#extract value labels from the metadata and compare

cat_vars <- c("sector", "state", "any_econ_activity", "main_income",
              "wage_sector", "casual_sector", "hh_type",
              "religion", "social_group", "land_owned", "land_type",
              "has_dwelling", "dwelling_type", "cooking_fuel", "lighting",
              "ration_card", "rent_rate_avail", "pmgky")

#make a crosswalk table to compare label values between the two datasets
label_table <- function(df, vars, yr) {
  map_dfr(vars, \(v) {
    lab <- val_labels(df[[v]])
    if (is.null(lab)) {
      tibble(var = v, year = yr, code = NA_character_, label = NA_character_)
    } else {
      tibble(var = v, year = yr, code = as.character(unname(lab)), label = names(lab))
    }
  })
}

lab_wide <- bind_rows(label_table(hh22, cat_vars, "2022-23"),
                      label_table(hh23, cat_vars, "2023-24")) |>
  pivot_wider(names_from = year, values_from = label)
write_csv(lab_wide, file.path(out_dir, "value_label_comparison.csv"))

lab_wide %>%
  filter(is.na(`2022-23`) | is.na(`2023-24`) | `2022-23` != `2023-24`) %>%
  print(n = Inf)

#individually changed discrepant values
write.csv(lab_wide, file.path(out_dir, "value_label_comparison.csv"))
#Change categorical variables to factors

to_factor <- function(df, vars) {
  df %>%
    mutate(across(all_of(vars), \(x) {
      # Fall back gracefully if x is not a haven_labelled vector
      f <- if (inherits(x, "haven_labelled")) {
        haven::as_factor(x, levels = "labels")
      } else {
        as.factor(x)
      }
      
      fct_relabel(f, \(l) str_squish(str_to_lower(l)))
    }))
}

hh22 <- to_factor(hh22, cat_vars)
hh23 <- to_factor(hh23, cat_vars)

level_diff <- map_dfr(cat_vars, \(v) {
  tibble(var = v,
         only_2223 = paste(setdiff(levels(hh22[[v]]), levels(hh23[[v]])), collapse = "; "),
         only_2324 = paste(setdiff(levels(hh23[[v]]), levels(hh22[[v]])), collapse = "; "))
}) %>% filter(only_2223 != "" | only_2324 != "")
print(level_diff, n = Inf)

#clean up NA variables
hh22 <- hh22 %>%
  mutate(
    religion     = fct_na_value_to_level(religion,     level = "not reported"),
    social_group = fct_na_value_to_level(social_group, level = "not reported")
  )

# Clean values

clean_household <- function(df) {
  df |>
    mutate(
      across(c(hh_size, land_acres, mult), \(x) as.numeric(zap_labels(x))),
      # Group the high-cardinality codes before modelling.
      nic2 = factor(str_sub(str_pad(as.character(zap_labels(nic)), 5, pad = "0"), 1, 2)),
      nco1 = factor(str_sub(str_pad(as.character(zap_labels(nco)), 3, pad = "0"), 1, 1)),
      land_acres_log1p = log1p(land_acres)
    )
}

hh22 <- clean_household(hh22)
hh23 <- clean_household(hh23)

# Add in member-level features

#id columns
id_map_2223 <- c(fsu = "fsu", su_no = "b1q1pt7", subdiv_no = "b1q1pt10", 
                 stratum2_no = "b1q1pt11", hh_no = "b1q1pt12")

id_map_2324 <- c(fsu = "fsu_serial_no", su_no = "sample_su_no", 
                 subdiv_no = "sample_sub_division_no", 
                 stratum2_no = "second_stage_stratum_no", hh_no = "sample_household_no")

pv_map_2223 <- c(age = "b3q5", gender = "b3q4", educ = "b3q7")
pv_map_2324 <- c(age = "age",     gender = "gender",     educ = "education_level")

#member summary function

member_summary <- function(l02_raw, yr, id_map, pv_map) {
  
  df <- l02_raw %>%
    select(all_of(c(id_map, pv_map))) %>%
    mutate(year = yr, .before = 1)
  
  df <- df %>%
    mutate(across(all_of(id_cols), \(x) as.character(zap_labels(x)))) %>%
    mutate(hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
           .before = 1)
  
  df <- df %>%
    mutate(
      age = as.numeric(zap_labels(age)),
      female = if_else(as_factor(gender) %>% str_to_lower() %in% c("female", "2"), 1, 0),
      educ = as.numeric(zap_labels(educ))
    )
  df %>%
    group_by(hh_id) %>%
    summarise(
      n_members    = n(),
      mean_age     = mean(age, na.rm = TRUE),
      share_female = mean(female, na.rm = TRUE),
      max_educ     = if (all(is.na(educ))) NA_real_ else max(educ, na.rm = TRUE),
      .groups      = "drop"
    )
}

m22 <- member_summary(read_level("2022-23", "l02"), "2022-23", id_map_2223, pv_map_2223)
m23 <- member_summary(read_level("2023-24", "l02"), "2023-24", id_map_2324, pv_map_2324)

#joining member-level features to the main dataset
hh22 <- left_join(hh22, m22, by = "hh_id")
hh23 <- left_join(hh23, m23, by = "hh_id")

member_cols <- c("n_members", "mean_age", "share_female", "max_educ")

#diagnostics
hh22 %>%
  summarise(
    total_hh = n(),
    matched_members = sum(!is.na(n_members)),
    missing_members = sum(is.na(n_members))
  )

#======================Building outcome variable (MPCE)============================

#integrate other levels into the hh_id structure, sum up expenditure for that level
#lv14 has summaries for expenditure

summarize_level_14 <- function(year, id_map) {
  
  subtotal_weights <- c(
    # --- 7-Day Items (x 30/7) ---
    "169" = 30/7, "219" = 30/7, "239" = 30/7, "249" = 30/7, "199" = 30/7,
    "189" = 30/7, "269" = 30/7, "279" = 30/7, "289" = 30/7, "299" = 30/7,
    "309" = 30/7, "319" = 30/7, "329" = 30/7,
    
    # --- 30-Day Items (x 1) ---
    "129" = 1, "139" = 1, "159" = 1, "179" = 1,
    "349" = 1, "459" = 1, "479" = 1, "429" = 1,
    "519" = 1, "499" = 1, "439" = 1, "529" = 1, "539" = 1,
    
    # --- 365-Day Items (x 1/12) ---
    "409" = 1/12, "419" = 1/12,
    "379" = 1/12, "399" = 1/12, "389" = 1/12, "629" = 1/12,
    "609" = 1/12, "099" = 1/12, "619" = 1/12, "599" = 1/12,
    "579" = 1/12, "559" = 1/12, "569" = 1/12, "649" = 1/12
  )
  
  df_raw <- read_level(year, "l14")
  
  item_col <- if (year == "2022-23") "ba1b1c1_2" else "item_code"
  val_col  <- if (year == "2022-23") "ba1b1c1_3" else "value_rs"
  
  df_raw %>%
    select(all_of(id_map), item_code = all_of(item_col), value = all_of(val_col)) %>%
    mutate(
      year = year,
      across(all_of(id_cols), \(x) as.character(zap_labels(x))),
      hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
      item_code = as.character(zap_labels(item_code)),
      val_numeric = as.numeric(zap_labels(value))
    ) %>%
    filter(item_code %in% names(subtotal_weights)) %>%
    mutate(
      scale_factor = subtotal_weights[item_code],
      exp_scaled = val_numeric * scale_factor
    ) %>%
    group_by(hh_id) %>%
    summarise(item_exp = sum(exp_scaled, na.rm = TRUE), .groups = "drop")
}

#testing
l14_test <- summarize_level_14("2023-24", id_map_2324)
summary(l14_test$item_exp)

#calculate expenditure per household per month
calculate_level14_mpce <- function(year, id_map) {
  hh_expenditure <- summarize_level_14(year = year, id_map = id_map)
  
  return(hh_expenditure)
}

exp_2223 <- calculate_level14_mpce("2022-23", id_map_2223)
exp_2324 <- calculate_level14_mpce("2023-24", id_map_2324)

summary(exp_2223)

l03_2324 <- read_level("2023-24", "l03")

# Create household ID and count members per household
hh_size_2324 <- l03_2324 %>%
  mutate(
    year = "2023-24",
    fsu = as.character(zap_labels(fsu_serial_no)),
    su_no = as.character(zap_labels(sample_su_no)),
    subdiv_no = as.character(zap_labels(sample_sub_division_no)),
    stratum2_no = as.character(zap_labels(second_stage_stratum_no)),
    hh_no = as.character(zap_labels(sample_household_no)),
    hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
    hh_size = as.numeric(hh_size_fdq)
  ) %>%
  select(hh_id, hh_size)

#MPCE per household created for 23-24
mpce_final_2324 <- exp_2324 %>%
  inner_join(hh_size_2324, by = "hh_id") %>%
  mutate(mpce_per_capita = item_exp / hh_size)

l03_2223 <- read_level("2022-23", "l03")

# Create household ID and count members per household
hh_size_2223 <- l03_2223 %>%
  mutate(
    year = "2022-23",
    fsu = as.character(zap_labels(fsu)),
    su_no = as.character(zap_labels(b1q1pt7)),
    subdiv_no = as.character(zap_labels(b1q1pt10)),
    stratum2_no = as.character(zap_labels(b1q1pt11)),
    hh_no = as.character(zap_labels(b1q1pt12)),
    hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
    hh_size = as.numeric(b2q2pt1)
  ) %>%
  select(hh_id, hh_size)

#MPCE per household created for 22-23
mpce_final_2223 <- exp_2223 %>%
  inner_join(hh_size_2223, by = "hh_id") %>%
  mutate(mpce_per_capita = item_exp / hh_size)

#validation
summary(mpce_final_2223$mpce_per_capita)

# Extract sector and multiplier(for weighting) mapping from Level 03
sector_mult_map_2324 <- l03_2324 %>%
  mutate(
    year = "2023-24",
    fsu = as.character(zap_labels(fsu_serial_no)),
    su_no = as.character(zap_labels(sample_su_no)),
    subdiv_no = as.character(zap_labels(sample_sub_division_no)),
    stratum2_no = as.character(zap_labels(second_stage_stratum_no)),
    hh_no = as.character(zap_labels(sample_household_no)),
    hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
    
    sec_code = as.character(zap_labels(sector)),
    sector_label = if_else(sec_code == "1", "Rural", "Urban"),
    
    multiplier = as.numeric(multiplier)
  ) %>%
  select(hh_id, sector_label, multiplier) %>%
  distinct()

mpce_by_sector_2324 <- mpce_final_2324 %>%
  inner_join(sector_mult_map_2324, by = "hh_id")

mpce_summary_sector_2324 <- mpce_by_sector_2324 %>%
  group_by(sector_label) %>%
  summarise(
    Households = n(),
    Min = min(mpce_per_capita),
    Q1 = quantile(mpce_per_capita, 0.25),
    Median = median(mpce_per_capita),
    Mean = mean(mpce_per_capita),
    Q3 = quantile(mpce_per_capita, 0.75),
    Max = max(mpce_per_capita),
    .groups = "drop"
  )

# Calculate Weighted Mean MPCE per Sector (23-24)
weighted_mpce_2324 <- mpce_by_sector_2324 %>%
  group_by(sector_label) %>%
  summarise(
    # Formula: Sum(MPCE * hh_size * multiplier) / Sum(hh_size * multiplier)
    Weighted_Mean_MPCE = sum(mpce_per_capita * hh_size * multiplier, na.rm = TRUE) / 
      sum(hh_size * multiplier, na.rm = TRUE),
    Total_Sample_HH = n(),
    .groups = "drop"
  )

print(weighted_mpce_2324)

#MPCE for 22-23
sector_mult_map_2223 <- l03_2223 %>%
  mutate(
    year = "2022-23",
    fsu = as.character(zap_labels(fsu)),
    su_no = as.character(zap_labels(b1q1pt7)),
    subdiv_no = as.character(zap_labels(b1q1pt10)),
    stratum2_no = as.character(zap_labels(b1q1pt11)),
    hh_no = as.character(zap_labels(b1q1pt12)),
    hh_id = paste(year, fsu, su_no, subdiv_no, stratum2_no, hh_no, sep = "_"),
    
    sec_code = as.character(zap_labels(sector)),
    sector_label = if_else(sec_code == "1", "Rural", "Urban"),
    
    multiplier = as.numeric(mult)
  ) %>%
  select(hh_id, sector_label, multiplier) %>%
  distinct()

mpce_by_sector_2223 <- mpce_final_2223 %>%
  inner_join(sector_mult_map_2223, by = "hh_id")

mpce_summary_sector_2223 <- mpce_by_sector_2223 %>%
  group_by(sector_label) %>%
  summarise(
    Households = n(),
    Min = min(mpce_per_capita),
    Q1 = quantile(mpce_per_capita, 0.25),
    Median = median(mpce_per_capita),
    Mean = mean(mpce_per_capita),
    Q3 = quantile(mpce_per_capita, 0.75),
    Max = max(mpce_per_capita),
    .groups = "drop"
  )

# Calculate Weighted Mean MPCE per Sector
weighted_mpce_2223 <- mpce_by_sector_2223 %>%
  group_by(sector_label) %>%
  summarise(
    # Formula: Sum(MPCE * hh_size * multiplier) / Sum(hh_size * multiplier)
    Weighted_Mean_MPCE = sum(mpce_per_capita * hh_size * multiplier, na.rm = TRUE) / 
      sum(hh_size * multiplier, na.rm = TRUE),
    Total_Sample_HH = n(),
    .groups = "drop"
  )

print(weighted_mpce_2223)

exp22 <- exp_2223 %>% select(hh_id, hh_exp = item_exp)
exp23 <- exp_2324 %>% select(hh_id, hh_exp = item_exp)

attach_target <- function(hh, exp_tbl) {
  hh %>%
    left_join(exp_tbl, by = "hh_id") %>%
    mutate(
      w  = mult / 100,             
      pw = w * hh_size,            
      mpce = hh_exp / hh_size
    )
}

wquant <- function(x, w, p) {
  ok <- !is.na(x) & !is.na(w)
  x <- x[ok]; w <- w[ok]
  o <- order(x)
  cw <- cumsum(w[o]) / sum(w)
  x[o][which(cw >= p)[1]]
}

hh22 <- attach_target(hh22, exp22)
hh23 <- attach_target(hh23, exp23)

both <- bind_rows(
  hh22 %>% select(-any_of("selfemp_sector")),
  hh23 %>% select(-any_of("selfemp_sector"))
)

both %>%
  group_by(year, sector) %>%
  summarise(
    weighted_mpce = sum(w * hh_exp, na.rm = TRUE) / sum(w * hh_size, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  print()

#mashallah

#lets move on

#bottom 20% in each sector (rural and urban) and by year
both <- both %>%
  group_by(year, sector) %>%
  mutate(
    cut20 = wquant(mpce, pw, 0.20),
    bottom20 = factor(if_else(mpce <= cut20, "Poor", "NonPoor"), levels = c("Poor", "NonPoor"))
  ) %>%
  ungroup()

#=====================#Column grouping, structural checks and export======================

id_out       <- c("hh_id", "year", "state", "district", "w")
target_cols  <- c("mpce", "bottom20")
feature_cols <- c("sector", "hh_size", "any_econ_activity", "nco1", "nic2",
                  "main_income", "wage_sector", "casual_sector",
                  "hh_type", "land_owned", "land_type", "land_acres_log1p",
                  "has_dwelling", "dwelling_type", "cooking_fuel", "lighting",
                  "rent_rate_avail")
audit_cols   <- c("social_group", "religion")     # For post-estimation bias check only
leak_cols    <- c("ration_card", "pmgky")         # Welfare receipts (excluded to prevent leakage)

member_cols  <- intersect(names(both), c("n_members", "mean_age", "share_female", "educ"))

#verification
stopifnot(!any(c(audit_cols, leak_cols) %in% feature_cols))

final <- both %>%
  select(all_of(c(id_out, target_cols, feature_cols, audit_cols, leak_cols, member_cols)))

#missingness audit
final %>%
  summarise(across(everything(), ~ mean(is.na(.x)))) %>%
  pivot_longer(everything(), names_to = "column", values_to = "share_missing") %>%
  filter(share_missing > 0) %>%
  print(n = Inf)

#export
saveRDS(final, file.path(out_dir, "hces_household_model.rds"))
write_csv(final, file.path(out_dir, "hces_household_model.csv"))
saveRDS(extra22, file.path(out_dir, "extra_2022_23_features.rds"))

#data dictionary
dictionary <- tibble(
  variable = names(final),
  type     = map_chr(final, ~ class(.x)[1]),
  group    = case_when(
    names(final) %in% id_out       ~ "id / design",
    names(final) %in% target_cols  ~ "target",
    names(final) %in% feature_cols ~ "feature",
    names(final) %in% audit_cols   ~ "audit only",
    names(final) %in% leak_cols    ~ "excluded (leakage)",
    TRUE                           ~ "member feature"
  )
)
write_csv(dictionary, file.path(out_dir, "column_dictionary.csv"))

#=======================#Datasets and recipes for models===============================================

#Libraries
library(tidymodels)
library(xgboost)
library(glmnet)
library(shapviz)
library(ggplot2)
tidymodels_prefer()
set.seed(2024)

dev_mode = FALSE #set to true for dry runs

dict <- dictionary

#check 
feature_vars <- dict$variable[dict$group %in% c("feature", "member feature")]
stopifnot(!all(is.na(final$bottom20)))   

model_df <- final %>%
  filter(!is.na(w), !is.na(hh_size)) %>%
  mutate(
    bottom20_raw = as.character(bottom20),
    bottom20 = case_when(
      bottom20_raw %in% c("Poor", "poor", "1", "TRUE")       ~ "poor",
      bottom20_raw %in% c("NonPoor", "not_poor", "0", "FALSE") ~ "not_poor",
      TRUE ~ NA_character_
    ),
    bottom20 = factor(bottom20, levels = c("poor", "not_poor")),
    pw = w * hh_size,
    case_wts = hardhat::importance_weights(pw)
  ) %>%
  filter(!is.na(bottom20))

train_df <- model_df %>% filter(year == "2022-23")
test_df <- model_df %>% filter (year == "2023-24")
train_dev <- if (dev_mode) {
  slice_sample(train_df, prop = 0.30, by = bottom20) 
} else {
  train_df
}

#modify to exclude leakage, audit, household ID, etc. to not use them as predictors
train_mod <- train_dev %>% select(state, bottom20, case_wts, all_of(feature_vars))
test_mod <- test_df %>% select(state, bottom20, case_wts, all_of(feature_vars))

rec_formula <- reformulate(c(feature_vars, "state", "case_wts"), 
                           response = "bottom20")

#base recipe, used to create recipes for logit, EN and trees
base_rec <- recipe(bottom20 ~ ., data = train_mod) %>%
  update_role(state, new_role = "id") %>%
  step_novel(all_nominal_predictors()) %>%
  step_unknown(all_nominal_predictors(), new_level = "missing") %>%
  #merge predictors that are used by <1% of the households into others
  step_other(all_nominal_predictors(), threshold = 0.01) %>%
  step_impute_median(all_numeric_predictors())

#recipe for logit and EN
lin_rec <- base_rec %>%
  step_dummy(all_nominal_predictors()) %>% #dummy variables
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors()) #normalize to same scale
  

tree_rec <- base_rec %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>% #one column per variable
  step_zv(all_predictors())

#=======================Specify models, Cross-Validation====================================

logit_spec <- logistic_reg() %>% set_engine("glm")
enet_spec <- logistic_reg(penalty = tune(), mixture = tune()) %>%
  set_engine("glmnet")
tree_spec <- boost_tree(trees = tune(), tree_depth = tune(), learn_rate = 
                          tune(), min_n = tune()) %>%
  set_engine("xgboost") %>% set_mode("classification")

#helper function for workflow- make weights, recipe, spec into one object
make_wf <- function(rec, spec) {
  workflow() %>% add_case_weights(case_wts) %>% add_recipe(rec) %>%
    add_model(spec)
}

#k-fold cross-validation to tune parameters. here states are folds
folds <- group_vfold_cv(train_mod, group = state, v = 5)
#metrics to judge the quality of models are ROC AUC and PR AUC
mset  <- metric_set(roc_auc, pr_auc)

#grids are matrices to tune parameters
enet_grid <- grid_regular(penalty(range = c(-5, -1)), mixture(range = c(0, 1)),
                          levels = c(8, 3))     # mixture 0 = ridge, 1 = lasso
tree_grid  <- grid_space_filling(
  trees(range = c(100, 600)), tree_depth(range = c(2, 6)),
  learn_rate(range = c(-3, -1)), min_n(range = c(20, 200)), size = 15)   

#the actual cross-validation
logit_res <- fit_resamples(make_wf(lin_rec, logit_spec), folds, metrics = mset)
enet_res <- tune_grid(make_wf(lin_rec, enet_spec), resamples = folds, 
                      grid = enet_grid, metrics = mset)
tree_res  <- tune_grid(make_wf(tree_rec, tree_spec), resamples = folds,
                      grid = tree_grid, metrics = mset)

cv_summary <- bind_rows(
  collect_metrics(logit_res) %>% mutate(model = "logit"),
  show_best(enet_res, metric = "pr_auc", n = 1) %>% mutate(model = "elastic_net"),
  show_best(tree_res, metric = "pr_auc", n = 1) %>% mutate(model = "xgboost")
)

#export metric summary
print(cv_summary)
write_csv(cv_summary, file.path(out_dir, "cv_summary.csv"))

#=================================Final fitting and OOT testing==================

best_enet <- select_best(enet_res, metric = "pr_auc")
best_tree  <- select_best(tree_res,  metric = "pr_auc")

fit_logit <- make_wf(lin_rec, logit_spec) %>% fit(train_mod)
fit_enet  <- make_wf(lin_rec, enet_spec)  %>% 
  finalize_workflow(best_enet) %>% fit(train_mod)
fit_tree   <- make_wf(tree_rec, tree_spec) %>% 
  finalize_workflow(best_tree)  %>% fit(train_mod)

score <- function(fit, name) {
  predict(fit, test_mod, type = "prob") %>%
    transmute(model = name, hh_id = test_df$hh_id, .pred_poor)
}

audit_cols <- test_df %>%
  select(hh_id, state, sector, social_group, religion, bottom20, pw, case_wts)

#predictions in small sample
preds <- bind_rows(score(fit_logit, "logit"),
                   score(fit_enet, "elastic_net"),
                   score(fit_tree, "xgboost")) %>%
  left_join(audit_cols, by = "hh_id")

test_metrics <- preds %>%
  group_by(model) %>%
  summarise(
    roc_auc = roc_auc_vec(bottom20, .pred_poor, case_weights = case_wts),
    pr_auc  = pr_auc_vec(bottom20, .pred_poor, case_weights = case_wts),
    .groups = "drop"
  )
print(test_metrics)

#quota to be selected for the hypothetical scheme
#use weighted quantile function from earlier
quota <- 0.2
select_quota <- function(p, w, quota) {
  as.integer(p >= wquant(p, w, 1 - quota))
}

preds <- preds %>%
  group_by(model, sector) %>%
  mutate(selected = select_quota(.pred_poor, pw, quota)) %>%
  ungroup()

error_rates <- function(df) {
  df %>%
    summarise(
      poor_pop        = sum(pw[bottom20 == "poor"]),
      exclusion_error = sum(pw[bottom20 == "poor" & selected == 0]) / poor_pop,
      selected_pop    = sum(pw[selected == 1]),
      inclusion_error = sum(pw[bottom20 == "not_poor" & selected == 1]) / selected_pop,
      n_poor_hh       = sum(bottom20 == "poor"),
      .groups = "drop"
    )
}

overall <- preds %>% group_by(model) %>% error_rates()
print(overall)

#==========#Fairness Audit, check if certain religions or castes get excluded===========

by_group <- function(df, g) {
  df %>%
    group_by(model, group = as.character(.data[[g]])) %>%
    error_rates() %>%
    mutate(dimension = g, .before = 1)
}
audit <- map_dfr(c("social_group", "religion", "sector", "state"),
                 \(g) by_group(preds, g)) %>%
  mutate(small_n = n_poor_hh < 200)    # treat groups flagged TRUE as unreliable

write_csv(test_metrics, file.path(out_dir, "test_metrics.csv"))
write_csv(overall,      file.path(out_dir, "overall_errors.csv"))
write_csv(audit,        file.path(out_dir, "audit_errors.csv"))

audit %>%
  filter(model == "xgboost", dimension == "social_group", !small_n) %>%
  ggplot(aes(x = reorder(group, exclusion_error), y = exclusion_error)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Share of poor population excluded",
       title = "Exclusion error by state (XGBoost, 2023-24)",
       subtitle = "Quota targeting at 20% of the population within sector") +
  theme_minimal()
ggsave(file.path(out_dir, "exclusion_by_social_group.png"), width = 7, 
       height = 4)

audit %>%
  filter(model == "xgboost", dimension == "state", !small_n) %>%
  ggplot(aes(x = reorder(group, exclusion_error), y = exclusion_error)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Share of poor population excluded",
       title = "Exclusion error by state (XGBoost, 2023-24)",
       subtitle = "Quota targeting at 20% of the population within sector") +
  theme_minimal()
ggsave(file.path(out_dir, "exclusion_by_state.png"), width = 7, 
       height = 4)

audit %>%
  filter(model == "elastic_net", dimension == "religion", !small_n) %>%
  ggplot(aes(x = reorder(group, exclusion_error), y = exclusion_error)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL, y = "Share of poor population excluded",
       title = "Exclusion error by religion (Elastic Net, 2023-24)",
       subtitle = "Quota targeting at 20% of the population within sector") +
  theme_minimal()
ggsave(file.path(out_dir, "exclusion_by_religion_enet.png"), width = 7, 
       height = 4)

#===================SHAPviz for trees==============================================

tree_booster <- extract_fit_engine(fit_tree)
tree_recipe  <- extract_recipe(fit_tree)
x_sample <- bake(tree_recipe, new_data = slice_sample(test_mod, n = 5000),
                 all_predictors(), composition = "matrix")
sv <- shapviz(tree_booster, X_pred = x_sample, X = as.data.frame(x_sample))

png(file.path(out_dir, "shap_importance.png"), width = 900, height = 600)
print(sv_importance(sv, kind = "bar", max_display = 15))
dev.off()
# Dependence plot for one variable, e.g.:
# sv_dependence(sv, "hh_size")

#=======================Include excluded variables from 22-23==========================

extra22 <- readRDS(file.path(out_dir, "extra_2022_23_features.rds")) %>%
  mutate(
    across(c(wall, roof, floor, water_source, latrine_access, latrine_type),
           \(x) haven::as_factor(x)),
    water_time = as.numeric(haven::zap_labels(water_time))
  )
extra_cols <- setdiff(names(extra22), "hh_id")

full22 <- train_dev %>%
  select(hh_id, state, bottom20, case_wts, all_of(feature_vars)) %>%
  left_join(extra22, by = "hh_id")

split <- group_initial_split(full22, group = state, prop = 0.8)
tr <- training(split)
te <- testing(split)

tree_fixed <- finalize_model(tree_spec, best_tree)   # same hyper-parameters for both

fit_eval <- function(cols, name) {
  tr_s <- tr %>% select(state, bottom20, case_wts, all_of(cols))
  te_s <- te %>% select(state, bottom20, case_wts, all_of(cols))
  rec <- recipe(bottom20 ~ ., data = tr_s) %>%
    update_role(state, new_role = "id") %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_other(all_nominal_predictors(), threshold = 0.01) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_zv(all_predictors()) %>%
    step_dummy(all_nominal_predictors(), one_hot = TRUE)
  fit_ <- workflow() %>%
    add_case_weights(case_wts) %>% add_recipe(rec) %>% add_model(tree_fixed) %>%
    fit(tr_s)
  p <- predict(fit_, te_s, type = "prob")$.pred_poor
  d <- te %>%
    mutate(.pred_poor = p, pw = as.numeric(case_wts)) %>%
    group_by(sector) %>%
    mutate(selected = select_quota(.pred_poor, pw, quota)) %>%
    ungroup()
  tibble(
    features = name,
    roc_auc = roc_auc_vec(d$bottom20, d$.pred_poor, case_weights = d$case_wts),
    exclusion_error = sum(d$pw[d$bottom20 == "poor" & d$selected == 0]) /
      sum(d$pw[d$bottom20 == "poor"])
  )
}

info_value <- bind_rows(
  fit_eval(feature_vars, "common features only"),
  fit_eval(c(feature_vars, extra_cols), "common + dropped housing/sanitation")
)
print(info_value)
write_csv(info_value, file.path(out_dir, "info_value_dropped_variables.csv"))

