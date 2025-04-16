########################################
###         Cleaning Survey          ###
###           28/03/2025             ###
########################################

#rm(list = ls()) # Clear the environment and tables

# Load necessary libraries
pacman::p_load(tidyverse, haven, ggplot2, car, arrow, readxl, openxlsx, dplyr, readr, Hmisc, labelled, writexl)

#### 0- Define file paths based on the current working directory
if (grepl("brigittecastaneda", getwd())) {
  path <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/"
  inputs <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/Inputs/"
  outputs <- "/Users/brigittecastaneda/Library/CloudStorage/OneDrive-Universidaddelosandes/Col_survey_FFSR/Outputs/"
} else if (grepl("k.castaneda", getwd())) {
  path <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/"
  inputs <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/Inputs/"
  outputs <- "/Users/k.castaneda/OneDrive - Universidad de los andes/Col_survey_FFSR/Outputs/"
} else if (grepl("charlott", getwd())) {
  path <- "C:/Users/charlott/Dropbox (Personal)/Paper_Colombia_Experiment/Analysis/"
  inputs <- "C:/Users/charlott/Dropbox (Personal)/Paper_Colombia_Experiment/Analysis/Inputs/"
  outputs <- "C:/Users/charlott/Dropbox (Personal)/Paper_Colombia_Experiment/Analysis/Outputs/"
}

getwd() # Display the current working directory

#### 0- Load the survey database ----
setwd(inputs)
library(haven)
#survey <- read_sav("DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - TDS.sav")
#survey <- read_sav("DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - ZDS (2).sav")
#survey  <- read_sav("DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - ZDS II.sav")
survey  <- read_sav("DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - final.sav")
#mpios <- read_csv("mpios.csv")
survey <- read_sav("../Paper_Colombia_Experiment/Analysis/Inputs/DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - final.sav")

#### 1- Initial review of data -----
head(survey) # Display the first rows of the dataset
str(survey) # Display the structure of the dataset
colnames(survey)[1001:1259] # Display the columns names of the dataset (from columns 1001:1259)


# Count unique values in each column for categorical variables
sapply(survey, function(x) length(unique(x)))

# Count missing values in each column
colSums(is.na(survey)) # There are no missing values

### 1.1 Group municipalities (question q5) -----
# Identify columns starting with "dq5p1r"
columns <- grep("^dq5p1r", names(survey) , value = TRUE)

# Exclude the original columns listed in "columns"
survey <- survey[, !(names(survey) %in% columns)]

ggplot(survey, aes(x = qtime / 60)) +  # Convert to minutes
  geom_histogram(aes(y = ..density..), bins = 30, fill = "steelblue", color = "black", alpha = 0.7) +
  geom_density(color = "red", size = 1.2) +
  labs(title = "Distribution of qtime (in minutes)", x = "qtime (minutes)", y = "Density") +
  theme_minimal()

# Calculate summary statistics (in minutes)
qtime_summary <- data.frame(
  Min = min(survey$qtime, na.rm = TRUE) / 60,
  Max = max(survey$qtime, na.rm = TRUE) / 60,
  Median = median(survey$qtime, na.rm = TRUE) / 60,
  Mean = mean(survey$qtime, na.rm = TRUE) / 60
)

# Print summary statistics
print(qtime_summary)


########################################
###         Do all cleaning          ###
###         steps at once            ###
########################################

# Save original number of rows
n_before <- nrow(survey)

# Define qtime thresholds
low_qtime <- quantile(survey$qtime, 0.05, na.rm = TRUE)
high_qtime <- quantile(survey$qtime, 0.98, na.rm = TRUE)

# Define page time variables
page_time_vars <- c("pagetimeIntro_q42a", "pagetimeIntroq42b", 
                    "pagetimeIntroq42c", "pagetimeIntroq42d")

# Compute all page time outlier record IDs without modifying the dataset
outliers_list <- lapply(page_time_vars, function(var) {
  percentiles <- quantile(survey[[var]], probs = c(0.05, 0.98), na.rm = TRUE)
  survey %>%
    filter(.data[[var]] <= percentiles[1] | .data[[var]] >= percentiles[2]) %>%
    pull(record)
})

# Combine all page time outlier records
page_time_outliers <- unique(unlist(outliers_list))

# Final unified filter
survey <- survey %>%
  filter(qtime >= low_qtime,
         qtime <= high_qtime,
         q1 < 99,
         q1 > 17,
         !(record %in% page_time_outliers))

# Save number of rows after all filtering
n_after <- nrow(survey)

# Print summary
cat("Rows before filtering:", n_before, "\n")
cat("Rows after filtering:", n_after, "\n")
cat("Total rows removed:", n_before - n_after, "\n")

cat(n_after, "/", n_before, "=", round(n_after / n_before, 3), "\n") #3616 / 4095 = 0.883

# @Leonard and @Lotti check the 10% fastest respondents with 0 variability in how their answered the 9 FFSR questions, eventually kick them out/report back to Farah



########################################
###         All preprocessing        ###
###                steps             ###
########################################

#### 2- Reordering and renaming columns ------
# Define the new order of columns
# 1. Socio-demographics q1:q11
# 2. Vehicle use q12:q15 (in final table q12:q23)
# 3. Cooking q16 (q24)
# 4. Attention check q17 (q25)
# 5. Climate q18:q22 (q26:q30)
# 6. Political questions q23:q31 (q32:q34, q39, q40)
# 7. Trust in government *Not included in the pilot* (q38)
# 8. News sources q32 (q41)
# 9. Subsidy knowledge q33:q38 (q42:q47)
# 10. Fairness q39:q41 (q48:q51)
# 11. Protest salience *Not included in the pilot* (q52:q53)
# 12. Attention check *Not included in the pilot* (q54)
# 13. First stage q42 (q55)
# 14. Status quo q43 (q56)
# 15. Unconditional support - Partial q44 (q57)
# 16. Unconditional support - Complete q45 (q58)
# 17. Conditional support q46 (q59)
# 18. Treatments/controls "^HCELLSr"
# 19. Time per question "pagetime"

## 2.0 Create a separate database for time spent on each question
#time_per_question <- survey[, c(134:188, 247,262)]

## 2.1 Reorder and rename columns based on "Qnaming rcode xlsx"
selected_variables <- c( "date",
  "q1", "q2", "q3", "q4", "q5new", "urban", "q6", "q7", "q8", "q9", "q10", "q11",
  "q12", "q13", "q14a", "q14b", "q15br1", "q15br2", "q15br3", "q15br4", "q15br5",
  "q15br5oe", "q15c", "q15d", "q15e", "q15f", "q15g", "q15h", "q15i", "q16", "q17",
  "q18", "q19", "q20", "q21", "q22r1", "q22r2", "q22r3", "q23r1", "q23r2", "q23r3",
  "q23r4", "q23r5", "q23r6", "q23r7", "q23r8", "q23r9", "q23r10", "q23r11", "q23r12",
  "q23r13", "q23r14", "q23r15", "q24", "q25", "q26r1", "q26r2", "q26r3", "q26r4",
  "q26r5", "q26r6", "q26r7", "q27r1", "q27r2", "q27r3", "q27r4", "q27r5", "q27r6",
  "q27r7", "q28r1", "q28r2", "q28r3", "q28r4", "q28r5", "q28r6", "q28r7", "q29",
  "q30ar1", "q30ar2", "q30ar3", "q30ar4", "q30ar5", "q30ar6", "q30ar7", "q30ar8",
  "q30ar9", "q30b", "q31", "q32r1", "q32r2", "q32r3", "q32r4", "q32r5", "q32r6",
  "q32r7", "q32r8", "q32r9", "q32r10", "q32r11", "q32r12", "q32r13", "q33", "q34",
  "q35", "q36", "q37", "q38", "q39", "q40", "q41a", "q41b", "q41c", "q41dr1", "q41dr2",
  "q41dr3", "q41dr4", "q41dr5", "q41dr6", "q41e", "q42a", "q42b", "q42c", "q42d",
  "q43", "q44", "q45", "q46r1", "q46r2", "q46r3", "q46r4", "q46r5", "q46r6", "q46r7",
  "q46r8", "q46r9", "q46r10", "q47", "q48", "q49", "HCELLSr1", "HCELLSr2", "HCELLSr3",
  "HCELLSr4", "HCELLSr5", "HCELLSr6", "HCELLSr7", "HCELLSr8", "HCELLSr9"
)

survey <- survey[, selected_variables]

# Update column names to align with the survey design
column_names_survey <- c("date", "edad", "gnro", "etnia", "dept", "mncp", "urban", "prsns", "edu", "trbjo", "ingrso", "grp_ingrso", "estrto", "moto",
                         "moto_dias", "carro", "carro_dias", "carro_combus_gsln", "carro_combus_acpm", "carro_combus_gas", "carro_combus_elect",
                         "carro_combus_other", "carro_combus_other_ans", "precio_combus", "transpub", "transpub_dias", "taxi", "taxi_dias",
                         "bici", "bici_dias", "ccnr", "attn1", "cc_info", "cc_preocup", "cc_futuro", "cc_econ", "cc_imp_co2", "cc_imp_pers",
                         "cc_imp_equit", "pol_prtds_col_hum", "pol_prtds_liga", "pol_prtds_cende", "pol_prtds_lib", "pol_prtds_cons",
                         "pol_prtds_verdox", "pol_prtds_unpat", "pol_prtds_camra", "pol_prtds_mira", "pol_prtds_U", "pol_prtds_averd",
                         "pol_prtds_pactoh", "pol_prtds_notie", "pol_prtds_otro", "pol_prtds_nodic", "pol_pres", "izq_der", "prop_eschr_labrl",
                         "prop_eschr_pensl", "prop_eschr_fepc", "prop_eschr_salud", "prop_eschr_paz", "prop_eschr_energ", "prop_eschr_ning",
                         "prop_entd_labrl", "prop_entd_pensl", "prop_entd_fepc", "prop_entd_salud", "prop_entd_paz", "prop_entd_energ",
                         "prop_entd_ning", "prop_acrd_labrl", "prop_acrd_pensl", "prop_acrd_fepc", "prop_acrd_salud", "prop_acrd_paz",
                         "prop_acrd_energ", "prop_acrd_ning", "pais_gnrl", "pais_confianza_army", "pais_confianza_police", "pais_confianza_prsdnt",
                         "pais_confianza_gvnrs", "pais_confianza_mayors", "pais_confianza_parties", "pais_confianza_congrs", "pais_confianza_con_crt",
                         "pais_confianza_jst_crt", "pais_dmcrc", "pais_econ", "info_fnte_pernal", "info_fnte_perreg", "info_fnte_radnal",
                         "info_fnte_radlol", "info_fnte_pscall", "info_fnte_tvnal", "info_fnte_tvreg", "info_fnte_web", "info_fnte_redscl",
                         "info_fnte_vlls", "info_fnte_afic", "info_fnte_voz", "info_fnte_nose", "ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "gas_super",
                         "dsl_super", "benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto", "protestas", "protestas_lid_trans",
                         "protestas_lid_sindi", "protestas_lid_estu", "protestas_lid_campe", "protestas_lid_indi", "protestas_lid_nose", "attn2",
                         "frst_a", "frst_b", "frst_c", "frst_d", "ffsr_mnt", "ffsr_prcl", "ffsr_complet", "rr_lmpsm", "rr_pobre", "rr_afctds",
                         "rr_impuesto", "rr_deuda", "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst", "rr_mas", "rr_mas_si", "sesgo",
                         "T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", "C_no_frst")
colnames(survey) <- column_names_survey

# Updated column names have been aligned with the new survey design, including:
# "carro_combus", "precio_combus", "transpub", "transpub_dias", "taxi", "taxi_dias", "bici", "bici_dias" (q16:q23)
# "pais_confianza" (q38)
# "protestas" (q52)
# "protestas_lid" (q53)
# "attn2" (q54)

#### 3- Adjust variable formats --------

## 3.0 DATE FORMAT ----
head(survey)
# Convertir a formato de solo fecha (sin hora) YY-MM-DD
survey$date <- as.Date(survey$date)

## 3.1 SOCIO-DEMOGRAPHIC VARIABLES (q1:q12) ----

# Recoding 'edad' (age) into categories
survey <- survey %>%
  mutate(
    edad_c = case_when(
      edad < 18 ~ "Under 18", # drop them immediately (@Lotti and @Leonard)
      edad >= 18 & edad <= 24 ~ "18 - 24",
      edad >= 25 & edad <= 34 ~ "25 - 34",
      edad >= 35 & edad <= 49 ~ "35 - 49",
      edad >= 50 & edad <= 64 ~ "50 - 64",
      edad >= 65 ~ "65 and older",
      TRUE ~ NA_character_
    )
  )

# Updating labels for 'gnro' (gender)
survey <- survey %>%
  mutate(
    gnro = labelled(
      gnro,
      labels = c(
        "Male" = 1,
        "Female" = 2,
        "Other" = 3
      )
    )
  )

# Adding labels for 'prsns' (household size)
survey <- survey %>%
  mutate(
    prsns = labelled(
      prsns,
      labels = c(
        `1` = 1,
        `2` = 2,
        `3` = 3,
        `4` = 4,
        `5 or more` = 5
      )
    )
  )

# Adding labels for 'etnia' (ethnicity)
survey <- survey %>%
  mutate(
    etnia = labelled(
      etnia,
      labels = c(
        Indigenous = 1,
        Gypsy = 2,
        Raizal = 3,
        Palenquero = 4,
        `Afro-Colombian` = 5,
        None = 6
      )
    )
  )

# Adding labels for 'edu' (education level)
survey$edu <- ifelse(survey$edu == 5, 3, survey$edu) #uniforming High School with Secondary 
survey$edu <- ifelse(survey$edu == 6, 5, survey$edu)
survey <- survey %>%
  mutate(
    edu = labelled(
      edu,
      labels = c(
        `None` = 1,
        `Primary` = 2,
        `Secondary` = 3,
        `Technical` = 4,
        `University or Postgraduate` = 5
      )
    )
  )

# Adding labels for 'trbjo' (employment status)
survey <- survey %>%
  mutate(
    trbjo = labelled(
      trbjo,
      labels = c(
        `Full-time work` = 1,
        `Part-time work` = 2,
        `Studying` = 3,
        `Studying and working` = 4,
        `Housework` = 5,
        `Retired` = 6,
        `Looking for work` = 7,
        `Not looking for work` = 8
      )
    )
  )

# Adding labels for 'ingrso' (monthly income)
survey <- survey %>%
  mutate(
    ingrso = labelled(
      ingrso,
      labels = c(
        `Between 0 – 310,000 COP` = 1,
        `Between 310,001 – 600,000 COP` = 2,
        `Between 600,001 – 900,000 COP` = 3,
        `Between 900,001 – 1,100,000 COP` = 4,
        `Between 1,100,001 – 1,400,000 COP` = 5,
        `Between 1,400,001 – 1,600,000 COP` = 6,
        `Between 1,600,001 – 1,825,000 COP` = 7,
        `Between 1,825,001 – 2,600,000 COP` = 8,
        `Between 2,600,001 – 4,150,000 COP` = 9,
        `More than 4,150,000 COP` = 10
      )
    )
  )

# Adding labels for 'grp_ingrso' (income group)
survey <- survey %>%
  mutate(
    grp_ingrso = labelled(
      grp_ingrso,
      labels = c(
        `Low` = 1,
        `Lower-middle` = 2,
        `Middle` = 3,
        `Upper-middle` = 4,
        `High` = 5
      )
    )
  )

# Adding labels for 'estrto' (strata)
survey <- survey %>%
  mutate(
    estrto = labelled(
      estrto,
      labels = c(
        `Stratum 1` = 1,
        `Stratum 2` = 2,
        `Stratum 3` = 3,
        `Stratum 4` = 4,
        `Stratum 5` = 5,
        `Stratum 6` = 6
      )
    )
  )

## 3.2 VEHICLE USE ----

# Recode and label days of motorcycle and car use
recode_days_per_week <- function(column, label_description) {
  recoded_column <- case_when(
    column == 1 ~ 0,  # No days
    column == 2 ~ 1,  # 1 day per week
    column == 3 ~ 2,  # 2 days per week
    column == 4 ~ 3,  # 3 days per week
    column == 5 ~ 4,  # 4 days per week
    column == 6 ~ 5,  # 5 days per week
    column == 7 ~ 6,  # 6 days per week
    column == 8 ~ 7,  # 7 days per week
    TRUE ~ NA_real_   # Handle unexpected or missing values
  )
  recoded_column <- labelled(
    recoded_column,
    labels = c(
      "No days" = 0,
      "1 day per week" = 1,
      "2 days per week" = 2,
      "3 days per week" = 3,
      "4 days per week" = 4,
      "5 days per week" = 5,
      "6 days per week" = 6,
      "7 days per week" = 7
    )
  )
  var_label(recoded_column) <- label_description
  return(recoded_column)
}

# Apply the recoding to motorcycle and car usage days
survey <- survey %>%
  mutate(
    moto_dias = recode_days_per_week(
      moto_dias, 
      "q13: Average weekly motorcycle use by household"
    ),
    carro_dias = recode_days_per_week(
      carro_dias, 
      "q14b: Average weekly car use by household"
    )
  )

# Create a new variable 'days_per_week' with the maximum value between moto_dias and carro_dias
survey <- survey %>%
  mutate(
    days_per_week = pmax(moto_dias, carro_dias, na.rm = TRUE)
  )

# Add labels to the new variable 'days_per_week'
survey$days_per_week <- labelled(
  survey$days_per_week,
  labels = c(
    "No days" = 0,
    "1 day per week" = 1,
    "2 days per week" = 2,
    "3 days per week" = 3,
    "4 days per week" = 4,
    "5 days per week" = 5,
    "6 days per week" = 6,
    "7 days per week" = 7
  )
)
var_label(survey$days_per_week) <- "Maximum days of vehicle use (moto or car) per week"

# Assign binary labels for fuel type columns (carro_combus_)
fuel_columns <- c(
  "carro_combus_gsln", "carro_combus_acpm", 
  "carro_combus_gas", "carro_combus_elect", "carro_combus_other")

for (col in fuel_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `No` = 0,
      `Yes` = 1
    )
  )
}


# Recode binary variables: Replace '2' with '0'
binary_columns <- c("moto", "carro", "transpub", "taxi", "bici")

for (col in binary_columns) {
  original_label <- var_label(survey[[col]])
  original_value_labels <- val_labels(survey[[col]])
  
  survey[[col]] <- ifelse(survey[[col]] == 2, 0, survey[[col]])
  
  val_labels(survey[[col]]) <- c(
    "Yes" = 1,
    "No" = 0
  )
  
  var_label(survey[[col]]) <- original_label
}

# Recode labels for frequency-based variables (transpub_dias, taxi_dias, bici_dias)
frequency_columns <- c("transpub_dias", "taxi_dias", "bici_dias")
frequency_labels <- c(
  `Almost every day` = 1,
  `Several times a week` = 2,
  `Once or twice a week` = 3,
  `A couple of times a month` = 4,
  `Never` = 5
)

for (col in frequency_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = frequency_labels
  )
}

## 3.3 COOKING ----
# Update labels for 'ccnr' (cooking energy source)
survey <- survey %>%
  mutate(
    ccnr = labelled(
      ccnr,
      labels = c(
        Electricity = 1,
        `Natural Gas` = 2,
        `Propane Gas` = 3,
        Cocinol = 4,
        Firewood = 5,
        Coal = 6,
        Waste = 7,
        `Does not cook` = 8
      )
    )
  )

## 3.4 CLIMATE ----
# Update labels for climate-related variables: cc_info and cc_preocup
survey <- survey %>%
  mutate(
    cc_info = labelled(
      cc_info,
      labels = c(
        `Nothing` = 1,
        `Too little` = 2,
        `Moderate` = 3,
        `Some` = 4,
        `Greatly` = 5
      )
    ),
    cc_preocup = labelled(
      cc_preocup,
      labels = c(
        `Nothing` = 1,
        `Too little` = 2,
        `Moderate` = 3,
        `Some` = 4,
        `Greatly` = 5
      )
    )
  )

# Recode and relabel 'cc_futuro' (urgency of climate change action)
survey <- survey %>%
  mutate(
    cc_futuro = case_when(
      cc_futuro == 1 ~ 5,
      cc_futuro == 2 ~ 4,
      cc_futuro == 3 ~ 3,
      cc_futuro == 4 ~ 2,
      cc_futuro == 5 ~ 1,
      TRUE ~ NA_real_
    ),
    cc_futuro = labelled(
      cc_futuro,
      labels = c(
        "Very urgent" = 5,
        "Urgent in the future" = 4,
        "Nothing to do" = 3,
        "Will never be urgent" = 2,
        "Doesn't know" = 1
      )
    )
  )

# Recode and label 'cc_econ' (priority: climate change vs. economic growth)
survey <- survey %>%
  mutate(
    cc_econ = case_when(
      cc_econ == 1 ~ 5,
      cc_econ == 3 ~ 4,
      cc_econ == 5 ~ 3,
      cc_econ == 2 ~ 2,
      cc_econ == 4 ~ 1,
      TRUE ~ NA_real_
    ),
    cc_econ = labelled(
      cc_econ,
      labels = c(
        "More priority to climate change" = 5,
        "Equal priority to climate change and economy" = 4,
        "I don't know" = 3,
        "More priority to the economy" = 2,
        "Other:" = 1
      )
    )
  )

# Recode and update labels for 'cc_imp_' variables
climate_importance_columns <- c("cc_imp_co2", "cc_imp_pers", "cc_imp_equit")
climate_importance_labels <- c(
  `Not important` = 1,
  `Very little important` = 2,
  `Moderately important` = 3,
  `Somewhat important` = 4,
  `Very important` = 5
)

for (col in climate_importance_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = climate_importance_labels
  )
}

## 3.5 ATTENTION CHECK ----
# Recode and add labels to attention check variables
survey <- survey %>%
  mutate(
    attn1 = ifelse(attn1 == 5, 1, 0),
    attn1 = labelled(
      attn1,
      labels = c(
        "No" = 0,
        "Yes" = 1
      )
    )
  ) %>%
  mutate(
    attn2 = ifelse(attn2 == 3, 1, 0),
    attn2 = labelled(
      attn2,
      labels = c(
        "No" = 0,
        "Yes" = 1
      )
    )
  )


## 3.6 POLITICAL ----
# Update labels for 'pol_pres' (second-round voting)
survey <- survey %>%
  mutate(
    pol_pres = labelled(
      pol_pres,
      labels = c(
        `G Petro` = 1,
        `Rodolfo H` = 2,
        `Blank` = 3,
        `Did not vote` = 4,
        `Prefer not to say` = 5
      )
    )
  )

# Recode and label political party columns
political_party_columns <- c(
  "pol_prtds_col_hum", "pol_prtds_liga", "pol_prtds_cende", "pol_prtds_lib",
  "pol_prtds_cons", "pol_prtds_verdox", "pol_prtds_unpat", "pol_prtds_camra",
  "pol_prtds_mira", "pol_prtds_U", "pol_prtds_averd", "pol_prtds_pactoh",
  "pol_prtds_notie", "pol_prtds_otro", "pol_prtds_nodic"
)

for (col in political_party_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `No` = 0,
      `Yes` = 1
    )
  )
}

# Recode 'izq_der' (political spectrum) to numeric values and handle 'Apolitical' as NA
survey <- survey %>%
  mutate(
    izq_der = case_when(
      izq_der == 6 ~ NA_real_,  # Convert "Apolitical" to NA
      TRUE ~ izq_der  # Keep other values unchanged
    ),
    izq_der = labelled(
      izq_der,
      labels = c(
        "Far-left" = 1,
        "Left" = 2,
        "Centrist" = 3,
        "Right" = 4,
        "Far-right" = 5,
        "Apolitical" = NA  # Include NA in labels
      )
    )
  )

# Recode 'pais_gnrl' (general perception of the country) and convert "Prefer not to say" to NA
survey <- survey %>%
  mutate(
    pais_gnrl = case_when(
      pais_gnrl == 4 ~ NA_real_,  # Convert "Prefer not to say" to NA
      TRUE ~ pais_gnrl  # Keep other values unchanged
    ),
    pais_gnrl = labelled(
      pais_gnrl,
      labels = c(
        "Progressing" = 1,
        "Stagnated" = 2,
        "In decline" = 3,
        "Prefer not to say" = NA  # Include NA in labels
      )
    )
  )

# Recode labels for 'pais_dmcrc' and 'pais_econ' (democracy and economy perception)
democracy_economy_columns <- c("pais_dmcrc", "pais_econ")

for (col in democracy_economy_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      "Very bad" = 1,
      "Bad" = 2,
      "Neutral" = 3,
      "Good" = 4,
      "Very good" = 5
    )
  )
}

## 3.7 TRUST IN GOVERNMENT ----
# Calculate average trust in institutions and individuals, rounded to the nearest value
survey <- survey %>%
  mutate(
    # Calculate the mean for institutional trust
    pais_confianza_inst = round(rowMeans(
      dplyr::select(., pais_confianza_army, pais_confianza_police, pais_confianza_parties,
                    pais_confianza_congrs, pais_confianza_con_crt, pais_confianza_jst_crt),
      na.rm = TRUE  # Ignore missing values
    )),
    pais_confianza_inst = labelled(
      pais_confianza_inst,
      labels = c(
        "No trust" = 1,
        "Little trust" = 2,
        "Moderate trust" = 3,
        "Some trust" = 4,
        "Strong trust" = 5
      )
    ),
    # Calculate the mean for individual trust
    pais_confianza_indv = round(rowMeans(
      dplyr::select(., pais_confianza_prsdnt, pais_confianza_gvnrs, pais_confianza_mayors),
      na.rm = TRUE  # Ignore missing values
    )),
    pais_confianza_indv = labelled(
      pais_confianza_indv,
      labels = c(
        "No trust" = 1,
        "Little trust" = 2,
        "Moderate trust" = 3,
        "Some trust" = 4,
        "Strong trust" = 5
      )
    )
  )

# Update labels for all 'pais_confianza_' variables
trust_columns <- grep("^pais_confianza_", colnames(survey), value = TRUE)

for (col in trust_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `No trust at all` = 1,
      `Little trust` = 2,
      `Moderate trust` = 3,
      `Some trust` = 4,
      `Complete trust` = 5
    )
  )
}

## 3.8 NEWS, SUBSIDY KNOWLEDGE, PROTEST SALIENCE ----
# Update labels for selected columns related to news, subsidies, and protests
news_subsidy_protest_columns <- grep(
  "^(prop_eschr_|prop_entd_|prop_acrd_|info_fnte_|protestas_lid_)",
  colnames(survey),
  value = TRUE
)

for (col in news_subsidy_protest_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `No` = 0,
      `Yes` = 1
    )
  )
}

# Recode and label the 'protestas' variable
survey <- survey %>%
  mutate(
    protestas = labelled(
      protestas,
      labels = c(
        "Yes" = 1,
        "No" = 2,
        "Not sure" = 3
      )
    )
  )

## 3.9 SUBSIDY KNOWLEDGE (q33:q38) ----
# Recode and label subsidy-related variables
subsidy_columns <- c("ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "gas_super", "dsl_super")

for (col in subsidy_columns) {
  survey[[col]] <- case_when(
    survey[[col]] == 1 ~ 1,  # Yes
    survey[[col]] == 3 ~ 2,  # Don't know
    survey[[col]] == 2 ~ 3,  # No
    TRUE ~ NA_real_
  )
  
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      "Yes" = 1,
      "I don't know" = 2,
      "No" = 3
    )
  )
}

## 3.10 FAIRNESS TO ME (q39:q41) ----
# Recode and label fairness-related variables
fairness_columns <- c("benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto")

for (col in fairness_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `Nothing` = 1,
      `Very little` = 2,
      `Undecided` = 3,
      `Somewhat` = 4,
      `Greatly` = 5
    )
  )
}

## 3.11 BIAS ----
# Recode and label the 'sesgo' (bias) variable
survey <- survey %>%
  mutate(
    sesgo = labelled(
      sesgo,
      labels = c(
        `No bias` = 1,
        `Left-leaning` = 2,
        `Centrist` = 3,
        `Right-leaning` = 4,
        `Not sure` = 5
      )
    )
  )

## 3.12 CONDITIONAL SUPPORT (q46) ----
# Recode and label conditional support variables
conditional_support_columns <- c(
  "rr_lmpsm", "rr_pobre", "rr_afctds", "rr_impuesto", "rr_deuda",
  "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst"
)

for (col in conditional_support_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `Nothing` = 1,
      `Very little` = 2,
      `Undecided` = 3,
      `Somewhat` = 4,
      `Greatly` = 5
    )
  )
}

# Recode the variable rr_mas
survey <- survey %>%
  mutate(
    rr_mas = case_when(
      rr_mas == 2 ~ 0,  # Change value 2 to 0
      rr_mas == 1 ~ 1   # Keep value 1 as is
    ),
    rr_mas = labelled(
      rr_mas,
      labels = c(
        "No" = 0,  # Recode 2 (now 0) as "No"
        "Yes" = 1  # Keep 1 as "Yes"
      )
    )
  )

# Create new variable 'conditional_pooled' as the average of the conditional variables
survey <- survey %>%
  mutate(
    conditional = round(
      rowMeans(
        dplyr::select(., all_of(conditional_support_columns)), # Use '.' to reference the dataframe explicitly
        na.rm = TRUE  # Ignore missing values
      )
    )
  )

# Assign labels to the 'conditional' variable
survey <- survey %>%
  mutate(
    conditional = labelled(
      conditional,
      labels = c(
        "Strongly disagree" = 1,
        "Disagree" = 2,
        "Neutral" = 3,
        "Agree" = 4,
        "Strongly agree" = 5
      )
    )
  )

## 3.13 TREATMENT & CONTROL ----
# Recode and label treatment and control variables
treatment_control_columns <- c("T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", "C_no_frst")

for (col in treatment_control_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      `No` = 0,
      `Yes` = 1
    )
  )
}

## 3.14 FIRST STAGE QUESTION ----
# Recode and label 'frst_a'
survey <- survey %>%
  mutate(
    frst_a = ifelse(frst_a == 2, 1, 0),
    frst_a = labelled(
      frst_a,
      labels = c(
        "Incorrect" = 0,
        "Correct" = 1
      )
    )
  )

# Recode and label additional first stage and subsidy support variables
first_stage_columns <- c("frst_b", "frst_c", "frst_d", "ffsr_mnt", "ffsr_prcl", "ffsr_complet")

for (col in first_stage_columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      "Strongly disagree" = 1,
      "Somewhat disagree" = 2,
      "Neither agree nor disagree" = 3,
      "Somewhat agree" = 4,
      "Strongly agree" = 5
    )
  )
}

## 3.15 UNCONDITIONAL SUPPORT  ----
# Recode and label specific columns related to support variables
columns <- c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")
for (col in columns) {
  survey[[col]] <- labelled(
    survey[[col]],
    labels = c(
      "Strongly disagree" = 1,
      "Disagree" = 2,
      "Neutral" = 3,
      "Agree" = 4,
      "Strongly agree" = 5
    )
  )
}

#### 4- Transformations for outcome variables ------
## 4.1 TRUST IN PRESIDENT ----

# Binary recoding for trust (recode_2)
trust_columns <- c("pais_confianza_prsdnt", "pais_confianza_indv", "pais_confianza_inst")

for (col in trust_columns) {
  col_name <- paste0(col, "_recode_2")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(4, 5), 1, 
                                    ifelse(survey[[col]] %in% c(1, 2), 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "No trust (1-2)" = 0,
      "Trust (4-5)" = 1
    )
  )
}

# Binary recoding for extreme options (recode_2extrem)
for (col in trust_columns) {
  col_name <- paste0(col, "_recode_2extrem")
  survey[[col_name]] <- ifelse(survey[[col]] == 5, 1, ifelse(survey[[col]] == 1, 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "No trust (1)" = 0,
      "Absolute trust (5)" = 1
    )
  )
}

# Recoding to a 3-scale format (recode_3)
for (col in trust_columns) {
  col_name <- paste0(col, "_recode_3")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(5, 4), 2, ifelse(survey[[col]] == 3, 1, 0))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "No trust (1-2)" = 0,
      "Neutral (3)" = 1,
      "Trust (4-5)" = 2
    )
  )
}

## 4.2 STATUS QUO, UNCONDITIONAL SUPPORT ----
# Binary recoding for unconditional support (recode_2)
unconditional_support_columns <- c("ffsr_mnt", "ffsr_prcl", "ffsr_complet")

for (col in unconditional_support_columns) {
  col_name <- paste0(col, "_recode_2")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(4, 5), 1, 
                                    ifelse(survey[[col]] %in% c(1, 2), 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Disapproves (1-2)" = 0,
      "Supports (4-5)" = 1
    )
  )
}

# Binary recoding for extremes (recode_2extrem)
for (col in unconditional_support_columns) {
  col_name <- paste0(col, "_recode_2extrem")
  survey[[col_name]] <- ifelse(survey[[col]] == 5, 1, ifelse(survey[[col]] == 1, 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Strongly disapproves (1)" = 0,
      "Strongly supports (5)" = 1
    )
  )
}

# Recoding to a 3-scale format (recode_3)
for (col in unconditional_support_columns) {
  col_name <- paste0(col, "_recode_3")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(5, 4), 2, ifelse(survey[[col]] == 3, 1, 0))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Disapproves (1-2)" = 0,
      "Neutral (3)" = 1,
      "Supports (4-5)" = 2
    )
  )
}

## 4.3 CONDITIONAL SUPPORT ----
# Binary recoding for conditional support variables (recode_2)
conditional_support_columns <- c(
  "rr_lmpsm", "rr_pobre", "rr_afctds", "rr_impuesto", "rr_deuda",
  "rr_etransp", "rr_paz", "rr_edu", "rr_ncer", "rr_deforst"
)

for (col in conditional_support_columns) {
  col_name <- paste0(col, "_recode_2")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(4, 5), 1, 
                                    ifelse(survey[[col]] %in% c(1, 2), 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Disapproves (1-2)" = 0,
      "Supports (4-5)" = 1
    )
  )
}

# Binary recoding for extremes (recode_2extrem)
for (col in conditional_support_columns) {
  col_name <- paste0(col, "_recode_2extrem")
  survey[[col_name]] <- ifelse(survey[[col]] == 5, 1, ifelse(survey[[col]] == 1, 0, NA))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Strongly disapproves (1)" = 0,
      "Strongly supports (5)" = 1
    )
  )
}

# Recoding to a 3-scale format (recode_3)
for (col in conditional_support_columns) {
  col_name <- paste0(col, "_recode_3")
  survey[[col_name]] <- ifelse(survey[[col]] %in% c(5, 4), 2, ifelse(survey[[col]] == 3, 1, 0))
  survey[[col_name]] <- labelled(
    survey[[col_name]],
    labels = c(
      "Disapproves (1-2)" = 0,
      "Neutral (3)" = 1,
      "Supports (4-5)" = 2
    )
  )
}


# Create and recode the binary columns 'conditional_recode_2' and 'conditional_recode_2extrem'
survey <- survey %>%
  mutate(
    # Binary recoding for general support
    conditional_recode_2 = ifelse(conditional %in% c(4, 5), 1, ifelse(conditional %in% c(1, 2), 0, NA)), 
    conditional_recode_2 = labelled(
      conditional_recode_2,
      labels = c(
        "Disapproves (1-2)" = 0,
        "Supports (4-5)" = 1
      )
    ),
    
    # Binary recoding for extreme support
    conditional_recode_2extrem = ifelse(conditional == 5, 1, ifelse(conditional == 1, 0, NA)),
    conditional_recode_2extrem = labelled(
      conditional_recode_2extrem,
      labels = c(
        "Strongly disapproves (1)" = 0,
        "Strongly supports (5)" = 1
      )
    ),
    
    # Recoding to a 3-scale format (recode_3)
    conditional_recode_3 = ifelse(conditional >= 4, 2, ifelse(conditional == 3, 1, 0)),
    conditional_recode_3 = labelled(
      conditional_recode_3,
      labels = c(
        "Disapproves (1-2)" = 0,
        "Neutral (3)" = 1,
        "Supports (4-5)" = 2
      )
    )
  )

#### 5- Creating aggregated indices ------
# Create support categories combining partial and complete support
survey <- survey %>%
  mutate(
    support_cat_recode_2 = case_when(
      ffsr_prcl_recode_2 == 1 & ffsr_complet_recode_2 == 1 ~ 3,  # Both partial and complete support
      ffsr_prcl_recode_2 == 1 & ffsr_complet_recode_2 == 0 ~ 2,  # Partial support only
      ffsr_prcl_recode_2 == 0 & ffsr_complet_recode_2 == 1 ~ 1,  # Complete support only
      ffsr_prcl_recode_2 == 0 & ffsr_complet_recode_2 == 0 ~ 0,  # No support
      TRUE ~ NA_real_
    ),
    support_cat_recode_2 = labelled(
      support_cat_recode_2,
      labels = c(
        "Disapproves" = 0,
        "Complete support" = 1,
        "Partial support" = 2,
        "Partial and complete support" = 3
      )
    )
  )

# 5.2 Create the support category variable 'support_cat_extrem' with labels
survey <- survey %>%
  mutate(
    support_cat_extrem = case_when(
      ffsr_prcl_recode_2extrem == 1 & ffsr_complet_recode_2extrem == 1 ~ 3,  # Both (partial and complete support)
      ffsr_prcl_recode_2extrem == 1 & ffsr_complet_recode_2extrem == 0 ~ 2,  # Partial (only partial support)
      ffsr_prcl_recode_2extrem == 0 & ffsr_complet_recode_2extrem == 1 ~ 1,  # Complete (only complete support)
      ffsr_prcl_recode_2extrem == 0 & ffsr_complet_recode_2extrem == 0 ~ 0,  # None (no support)
      TRUE ~ NA_real_  # For values not contemplated, keeping numeric type
    ),
    support_cat_extrem = labelled(
      support_cat_extrem,
      labels = c(
        "Disapproves" = 0,
        "Complete support" = 1,
        "Partial support" = 2,
        "Partial and complete support" = 3
      )
    )
  )

# Create a detailed support category variable
survey <- survey %>%
  mutate(
    support_cat_detailed = case_when(
      ffsr_prcl_recode_3 == 2 & ffsr_complet_recode_3 == 2 ~ 5,  # Strong support for both partial and complete
      ffsr_prcl_recode_3 == 2 & ffsr_complet_recode_3 == 1 ~ 4,  # Strong partial, moderate complete
      ffsr_prcl_recode_3 == 1 & ffsr_complet_recode_3 == 2 ~ 4,  # Moderate partial, strong complete
      ffsr_prcl_recode_3 == 1 & ffsr_complet_recode_3 == 1 ~ 3,  # Moderate support for both
      ffsr_prcl_recode_3 == 2 & ffsr_complet_recode_3 == 0 ~ 2,  # Strong partial, no complete
      ffsr_prcl_recode_3 == 0 & ffsr_complet_recode_3 == 2 ~ 2,  # No partial, strong complete
      ffsr_prcl_recode_3 == 1 & ffsr_complet_recode_3 == 0 ~ 1,  # Moderate partial, no complete
      ffsr_prcl_recode_3 == 0 & ffsr_complet_recode_3 == 1 ~ 1,  # No partial, moderate complete
      ffsr_prcl_recode_3 == 0 & ffsr_complet_recode_3 == 0 ~ 0,  # No support for either
      TRUE ~ NA_real_
    ),
    support_cat_detailed = labelled(
      support_cat_detailed,
      labels = c(
        "Disapproves" = 0,
        "Moderate support" = 1,
        "Strong support for one" = 2,
        "Moderate support for both" = 3,
        "Strong for one, moderate for other" = 4,
        "Strong support for both" = 5
      )
    )
  )

#### 6 - Update variable labels ----
# Create a data frame with column names and their respective labels
column_labels <- data.frame(
  ColumnName = c("edad", "gnro", "etnia", "dept", "mncp", "prsns", "edu", "trbjo", "ingrso",
                 "grp_ingrso", "estrto", "moto", "moto_dias", "carro", "carro_dias", 
                 "carro_combus_gsln", "carro_combus_acpm", "carro_combus_gas", 
                 "carro_combus_elect", "carro_combus_other", "carro_combus_other_ans", 
                 "precio_combus", "transpub", "transpub_dias", "taxi", "taxi_dias", 
                 "bici", "bici_dias", "ccnr", "attn1", "cc_info", "cc_preocup", 
                 "cc_futuro", "cc_econ", "cc_imp_co2", "cc_imp_pers", "cc_imp_equit", 
                 "pol_prtds_col_hum", "pol_prtds_liga", "pol_prtds_cende", "pol_prtds_lib", 
                 "pol_prtds_cons", "pol_prtds_verdox", "pol_prtds_unpat", "pol_prtds_camra", 
                 "pol_prtds_mira", "pol_prtds_U", "pol_prtds_averd", "pol_prtds_pactoh", 
                 "pol_prtds_notie", "pol_prtds_otro", "pol_prtds_nodic", "pol_pres", 
                 "izq_der", "prop_eschr_labrl", "prop_eschr_pensl", "prop_eschr_fepc", 
                 "prop_eschr_salud", "prop_eschr_paz", "prop_eschr_energ", "prop_eschr_ning",
                 "prop_entd_labrl", "prop_entd_pensl", "prop_entd_fepc", "prop_entd_salud", 
                 "prop_entd_paz", "prop_entd_energ", "prop_entd_ning", "prop_acrd_labrl",
                 "prop_acrd_pensl", "prop_acrd_fepc", "prop_acrd_salud", "prop_acrd_paz", 
                 "prop_acrd_energ", "prop_acrd_ning", "pais_gnrl", "pais_confianza_army",
                 "pais_confianza_police", "pais_confianza_prsdnt", "pais_confianza_gvnrs",
                 "pais_confianza_mayors", "pais_confianza_parties", "pais_confianza_congrs",
                 "pais_confianza_con_crt", "pais_confianza_jst_crt", "pais_dmcrc", "pais_econ", 
                 "info_fnte_pernal", "info_fnte_perreg", "info_fnte_radnal", "info_fnte_radlol",
                 "info_fnte_pscall", "info_fnte_tvnal", "info_fnte_tvreg", "info_fnte_web", 
                 "info_fnte_redscl", "info_fnte_vlls", "info_fnte_afic", "info_fnte_voz", 
                 "info_fnte_nose", "ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "gas_super", "dsl_super", 
                 "benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto", "protestas", 
                 "protestas_lid_trans", "protestas_lid_sindi", "protestas_lid_estu", 
                 "protestas_lid_campe", "protestas_lid_indi", "protestas_lid_nose", "attn2", 
                 "frst_a", "frst_b", "frst_c", "frst_d", "ffsr_mnt", "ffsr_prcl", "ffsr_complet", 
                 "rr_lmpsm", "rr_pobre", "rr_afctds", "rr_impuesto", "rr_deuda", "rr_etransp", 
                 "rr_paz", "rr_edu", "rr_ncer", "rr_deforst", "rr_mas", "rr_mas_si", "sesgo", 
                 "T_A", "T_B", "T_C", "T_D", "C_A", "C_B", "C_C", "C_D", "C_no_frst"),
  ColumnLabel = c("age", "gender", "ethnicity", "department", "municipality", 
                  "people in the household", "educational level", "economic activity", 
                  "monthly income", "income group", "strata", "uses motorcycle", 
                  "days that uses a motorcycle", "uses car", "days that uses a car", 
                  "car with gasoline", "car with diesel", "car with natural gas", 
                  "electric car", "other car", "other car response", 
                  "diesel price per liter", "uses public transportation", 
                  "days public transportation", "uses taxi", "days taxi", "uses bicycle", 
                  "days bicycle", "energy in kitchen", "completed attention check 1", 
                  "information on climate change", "concern about climate change",
                  "urgency of action for climate change", "priority climate change over economy", 
                  "importance of reducing CO2", "importance of personal costs", 
                  "importance of equitable costs", "Colombia Humana Party", 
                  "LIGA Party", "Centro Democrático Party", "Liberal Party", 
                  "Conservative Party", "Green Oxygen Party", "Patriotic Union", 
                  "Change Party Radical", "Political Party Mira", "Party of the U", 
                  "Green Alliance Party", "Historical Pact", "Has no party", 
                  "Other", "Prefer not to say", "second round vote", "political spectrum", 
                  "listened to labor reform", "listened to pension reform", 
                  "listened to FFSSR", "listened to health reform", "listened to Total Peace", 
                  "listened to Energy Transition", "listened to none", 
                  "understands labor reform", "understands pension reform", 
                  "understands FFSSR", "understands health reform", "understands Total Peace", 
                  "understands Energy Transition", "understands none", 
                  "supports labor reform", "supports pension reform", "supports FFSSR", 
                  "supports health reform", "supports Total Peace", "supports Energy Transition", 
                  "supports none", "general situation", "trust in the army", 
                  "trust in the Police", "trust in the President", 
                  "trust in the Governors", "trust in the Mayors", 
                  "trust in the Political Parties", "trust in the Congress of the Republic", 
                  "trust in the Constitutional Court", "confidence in the Supreme Court of Justice", 
                  "democracy", "economic situation", 
                  "Informs by national newspapers (print or online)", 
                  "Informs by regional newspapers (print or online)", 
                  "Informs by national radio", "Informs by local radio", 
                  "Informs by parades", "Informs by national television channels", 
                  "Informs by regional television channels", "Informs by specialized articles", 
                  "Informs by social networks (Facebook)", "Informs by billboards", 
                  "Informs by posters", "Informs by word of mouth", "Informs I don't know/no answer", 
                  "knows about fuel subsidy", "knows about diesel subsidy", "knows about gasoline subsidy", 
                  "gasoline prices in supermarkets", "diesel prices in supermarkets", 
                  "own benefit", "right to benefit", "own effect", "effect on the poor", 
                  "effect on the rich", "there were protests", 
                  "led by Transporters", "led by Unions and workers", 
                  "led by Students", "led by Peasants and rural communities", 
                  "led by Communities indigenous", "I don't know who led the protests", 
                  "completed attention check 2", "first stage question to TA", "first stage question to TB", 
                  "first stage question to TC", "first stage question to TD", 
                  "support to maintain", "support to partially reduce", 
                  "support to eliminate", "conditional support to uniform transfers", 
                  "conditional support to transfers to poor", "conditional support to transfers to affected", 
                  "conditional support to reduce public debt", "conditional support to reduce taxes", 
                  "conditional support to public transport", "conditional support to roads", 
                  "conditional support to education", "conditional support to clean energy", 
                  "conditional support to the Amazon", "other type of conditional support", 
                  "other type of conditional support answer", "bias", "Treatment A", 
                  "Treatment B", "Treatment C", "Treatment D", "Control A", 
                  "Control B", "Control C", "Control D", "Control no first stage")
)

# Apply the labels to the columns of the DataFrame
for (i in 1:nrow(column_labels)) {
  if (column_labels$ColumnName[i] %in% colnames(survey)) {
    var_label(survey[[column_labels$ColumnName[i]]]) <- column_labels$ColumnLabel[i]
  }
}

#variables que faltaban 
# Create a data frame with the names of the additional columns and their respective labels
column_labels_faltantes <- data.frame(
  ColumnName = c(
    "conditional", "edad_c", "days_per_week", "pais_confianza_inst", "pais_confianza_indv",
    "pais_confianza_prsdnt_recode_2", "pais_confianza_indv_recode_2",
    "pais_confianza_inst_recode_2", "pais_confianza_prsdnt_recode_2extrem",
    "pais_confianza_indv_recode_2extrem", "pais_confianza_inst_recode_2extrem",
    "pais_confianza_prsdnt_recode_3", "pais_confianza_indv_recode_3",
    "pais_confianza_inst_recode_3", "ffsr_mnt_recode_2", "ffsr_prcl_recode_2",
    "ffsr_complet_recode_2", "ffsr_mnt_recode_2extrem", "ffsr_prcl_recode_2extrem",
    "ffsr_complet_recode_2extrem", "ffsr_mnt_recode_3", "ffsr_prcl_recode_3",
    "ffsr_complet_recode_3", "rr_lmpsm_recode_2", "rr_pobre_recode_2",
    "rr_afctds_recode_2", "rr_impuesto_recode_2", "rr_deuda_recode_2",
    "rr_etransp_recode_2", "rr_paz_recode_2", "rr_edu_recode_2",
    "rr_ncer_recode_2", "rr_deforst_recode_2", "rr_lmpsm_recode_2extrem",
    "rr_pobre_recode_2extrem", "rr_afctds_recode_2extrem", "rr_impuesto_recode_2extrem",
    "rr_deuda_recode_2extrem", "rr_etransp_recode_2extrem", "rr_paz_recode_2extrem",
    "rr_edu_recode_2extrem", "rr_ncer_recode_2extrem", "rr_deforst_recode_2extrem",
    "rr_lmpsm_recode_3", "rr_pobre_recode_3", "rr_afctds_recode_3",
    "rr_impuesto_recode_3", "rr_deuda_recode_3", "rr_etransp_recode_3",
    "rr_paz_recode_3", "rr_edu_recode_3", "rr_ncer_recode_3", "rr_deforst_recode_3",
    "support_cat_recode_2", "support_cat_extrem", "support_cat_detailed", 
    "conditional_recode_2", "conditional_recode_2extrem", "conditional_recode_3"
  ),
  ColumnLabel = c(
    "Conditional support (pooled)","Age Categories", "days per week vehicle use (motorcycle or car)",
    "trust in institutions", "trust in individuals (mayor, governor, president)",
    "Trust in institutions (trust 4-5 and do not trust  1-2)",
    "Trust in individuals (mayor, governor, president) (trust 4-5 and do not trust  1-2)",
    "Trust in the President (trust 4-5 and do not trust  1-2)",
    "Trust in institutions (trust 5, do not trust 1, NA dlc.)",
    "Trust in individuals (mayor, governor, president) (trust 5, do not trust 1, NA dlc.)",
    "Trust in the President (trust 5, do not trust 1, NA dlc.)",
    "Trust in institutions (trust 4-5, neutral 3, do not trust 1-2)",
    "Trust in individuals (mayor, governor, president) (trust 4-5, neutral 3, do not trust 1-2)",
    "Trust in the President (trust 4-5, neutral 3, do not trust 1-2)",
    "support to maintain (supports 4-5 and does not support  1-2)",
    "support to partially reduce (supports 4-5 and does not support  1-2)",
    "support to eliminate (supports 4-5 and does not support  1-2)",
    "support to maintain (supports 5, does not support 1, NA dlc.)",
    "support to partially reduce (supports 5, does not support 1, NA dlc.)",
    "support to eliminate (supports 5, does not support 1, NA dlc.)",
    "support to maintain (supports 4-5, neutral 3, does not support 1-2)",
    "support to partially reduce (supports 4-5, neutral 3, does not support 1-2)",
    "support to eliminate (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support to uniform transfers (supports 4-5 and does not support  1-2)",
    "conditional support to transfers to poor (supports 4-5 and does not support  1-2)",
    "Conditional support for transfers to affected people (supports 4-5 and does not support  1-2)",
    "Conditional support for reducing public debt (supports 4-5 and does not support  1-2)",
    "Conditional support for reducing taxes (supports 4-5 and does not support  1-2)",
    "Conditional support for public transport (supports 4-5 and does not support  1-2)",
    "Conditional support for roads (supports 4-5 and does not support  1-2)",
    "Conditional support for education (supports 4-5 and does not support  1-2)",
    "Conditional support for clean energy (supports 4-5 and does not support  1-2)",
    "Conditional support for the Amazon (supports 4-5 and does not support  1-2)",
    "Conditional support for uniform transfers (supports 5, does not support 1, NA dlc.)",
    "Conditional support for transfers to the poor (supports 5, does not support 1, NA dlc.)",
    "Conditional support to transfers to affected people (supports 5, does not support 1, NA dlc.)",
    "conditional support to reduce public debt (supports 5, does not support 1, NA dlc.)",
    "conditional support to reduce taxes (supports 5, does not support 1, NA dlc.)",
    "conditional support to public transport (supports 5, does not support 1, NA dlc.)",
    "conditional support to roads (supports 5, does not support 1, NA dlc.)",
    "conditional support to education (supports 5, does not support 1, NA dlc.)",
    "conditional support to clean energy (supports 5, does not support 1, NA dlc.)",
    "conditional support to the Amazon (supports 5, does not support 1, NA dlc.)",
    "conditional support to uniform transfers (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support to transfers to the poor (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for transfers to affected people (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for reducing public debt (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for reducing taxes (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for public transport (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for roads (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for education (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for clean energy (supports 4-5, neutral 3, does not support 1-2)",
    "conditional support for the Amazon (supports 4-5, neutral 3, does not support 1-2)",
    "Unconditional support index (supports 4-5 and does not support  1-2)",
    "Unconditional support index (supports 5, does not support 1, NA dlc.)",
    "Unconditional support index (supports 4-5, neutral 3, does not support 1-2)",
    "Conditional support pooled (supports 4-5 and does not support  1-2)",
    "Conditional support pooled (supports 5, does not support 1, NA dlc.)",
    "Conditional support pooled (supports 4-5, neutral 3, does not support 1-2)"
  )
)

# Assign the labels to the corresponding variables
for (i in 1:nrow(column_labels_faltantes)) {
  if (column_labels_faltantes$ColumnName[i] %in% colnames(survey)) {
    var_label(survey[[column_labels_faltantes$ColumnName[i]]]) <- column_labels_faltantes$ColumnLabel[i]
  }
}

# Verificar los labels actualizados
var_label(survey)



### 7 - creo la variable de edades (Farah) ###----
# Function to categorize ages
categorize_age <- function(age) {
  if (age >= 18 & age <= 33) {
    return("18-33 years")
  } else if (age >= 34 & age <= 53) {
    return("34-53 years")
  } else if (age >= 54) {
    return("54+ years")
  } else {
    return("Unknown")  # For ages below 18 or missing values
  }
}

# Apply the function to the 'edad' column to create the new variable
survey$age_category <- sapply(survey$edad, categorize_age)

# Create labels for the age_category variable
age_category_labels <- c(
  "18-33 years" = "18-33 years",
  "34-53 years" = "34-53 years",
  "54+ years" = "54+ years",
  "Unknown" = "Age Not Specified"
)

# Add a label for the variable age_category
attr(survey$age_category, "label") <- "Age Categories"


#### 9 - Export final dataset ----

setwd(outputs)

# Export the dataset in SPSS format
write_sav(survey, "../Paper_Colombia_Experiment/Analysis/Outputs/Intermediate.sav")
#write_sav(survey, "Survey_n139_labels.sav")
#write_sav(survey, "Survey_n3044_labels.sav")
#write_sav(survey, "Survey_final_qtime_age.sav") #Excluding observations with age greater than 74 and qtime less than 10 minutes or greater than 40 minutes.
#write_sav(survey, "Survey_final_pagetime42_qtime_age.sav") #Excluding observations with age greater than 74 and observations in the 5th and 95th percentiles for pagetime42 and qtime variables.


# Export the dataset in CSV format
#write.csv(survey, "Survey_n139_labels_eng.csv", row.names = FALSE)
#write.csv(survey, "Survey_n3044_labels_eng.csv", row.names = FALSE)
#write.csv(survey, "Survey_final_qtime_eng.csv", row.names = FALSE) #Excluding observations with age greater than 74 and qtime less than 10 minutes or greater than 40 minutes
#write.csv(survey, "Survey_final_pagetime42_qtime_age_eng.csv", row.names = FALSE) #Excluding observations with age greater than 74 and observations in the 5th and 95th percentiles for treatment time variables.


# Optional: Export variable labels to a text file
# variable_labels <- lapply(survey, function(x) attr(x, "labels"))
# labels_text <- lapply(names(variable_labels), function(var) {
#   lbls <- variable_labels[[var]]
#   if (!is.null(lbls)) {
#     paste(var, ":\n", paste(names(lbls), "=", lbls, collapse = "\n"), "\n")
#   } else {
#     paste(var, ":\nNo labels\n")
#   }
# })
# labels_combined <- paste(labels_text, collapse = "\n")
# writeLines(labels_combined, "Survey_n139_labels.txt")

#### 10 - Final checks ----

# Check unique values for the first 120 columns
for (col in colnames(survey)[1:12]) {
  cat("Unique values in column", col, ":\n")
  print(unique(survey[[col]]))
  cat("\n")  # Add a line break for readability
}


