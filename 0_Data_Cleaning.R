# Author: L. Missbach (leonard.missbach@pik-potsdam.de) based on script by Brigitte Castaneda and Charlotte Bez

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "ggsci", "fixest", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled", "tidymodels", "tidytext")

options(scipen=999)

# 0    Load the survey data ####

data_0 <- read_sav("Inputs/DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - final.sav")%>%
  # Create unique ID
  mutate(ID = 1:n())%>%
  select(ID, everything())

# 1    Transform data ####
# 1.1  Select important variables and rename ####

data_1.1 <- data_0 %>%
  select(-starts_with("dq5p1r"))%>%
  select(ID, "date",
         "q1", "q2", "q3", "q4", "q5new", "urban", "q6", "q7", "q8", "q9", "q10", "q11", "q12", "q13", "q14a", "q14b",
         "q15br1", "q15br2", "q15br3", "q15br4", "q15br5", "q15br5oe", "q15c", "q15d", "q15e", "q15f", "q15g", "q15h", "q15i", 
         "q16", "q17", "q18", "q19", "q20", "q21", "q22r1", "q22r2", "q22r3", 
         "q23r1", "q23r2", "q23r3", "q23r4", "q23r5", "q23r6", "q23r7", "q23r8", "q23r9", "q23r10", "q23r11", "q23r12", "q23r13", "q23r14", "q23r15",
         "q24", "q25", "q26r1", "q26r2", "q26r3", "q26r4", "q26r5", "q26r6", "q26r7",
         "q27r1", "q27r2", "q27r3", "q27r4", "q27r5", "q27r6", "q27r7",
         "q28r1", "q28r2", "q28r3", "q28r4", "q28r5", "q28r6", "q28r7",
         "q29", "q30ar1", "q30ar2", "q30ar3", "q30ar4", "q30ar5", "q30ar6", "q30ar7", "q30ar8", "q30ar9", "q30b", "q31", 
         "q32r1", "q32r2", "q32r3", "q32r4", "q32r5", "q32r6", "q32r7", "q32r8", "q32r9", "q32r10", "q32r11", "q32r12", "q32r13",
         "q33", "q34", "q35", "q36", "q37", "q38", "q39", "q40",
         "q41a", "q41b", "q41c", "q41dr1", "q41dr2", "q41dr3", "q41dr4", "q41dr5", "q41dr6", "q41e",
         "q42a", "q42b", "q42c", "q42d", "q43", "q44", "q45",
         "q46r1", "q46r2", "q46r3", "q46r4", "q46r5", "q46r6", "q46r7", "q46r8", "q46r9", "q46r10", "q47", "q48", "q49",
         "HCELLSr1", "HCELLSr2", "HCELLSr3", "HCELLSr4", "HCELLSr5", "HCELLSr6", "HCELLSr7", "HCELLSr8", "HCELLSr9")%>%
    rename(
      # Sociodemographics
      "edad" = "q1", "gnro" = "q2", "etnia" = "q3", "dept" = "q4", "mncp" = "q5new", "prsns" = "q6", "edu" = "q7", "trbjo" = "q8", "ingrso" = "q9", "grp_ingrso" = "q10", "estrto" = "q11",
      # Vehicle use
      "moto" = "q12", "moto_dias" = "q13", "carro" = "q14a", "carro_dias" = "q14b",
      "carro_combus_gsln" = "q15br1", "carro_combus_acpm" = "q15br2", "carro_combus_gas" = "q15br3", "carro_combus_elect" = "q15br4", "carro_combus_other" = "q15br5", "carro_combus_other_ans" = "q15br5oe", 
      "precio_combus" = "q15c", "transpub" = "q15d", "transpub_dias" = "q15e", "taxi" = "q15f", "taxi_dias" = "q15g", "bici" = "q15h", "bici_dias" = "q15i", 
      # Cooking fuels and attention check
      "ccnr" = "q16", "attn1" = "q17",
      # Climate change
      "cc_info" = "q18", "cc_preocup" = "q19", "cc_futuro" = "q20", "cc_econ" = "q21", "cc_imp_co2" = "q22r1", "cc_imp_pers" = "q22r2", "cc_imp_equit" = "q22r3", 
      # Political variables
      "pol_prtds_col_hum" = "q23r1",  "pol_prtds_liga"   = "q23r2",  "pol_prtds_cende" = "q23r3",  "pol_prtds_lib"   = "q23r4", "pol_prtds_cons"  = "q23r5",
      "pol_prtds_verdox"  = "q23r6",  "pol_prtds_unpat"  = "q23r7",  "pol_prtds_camra" = "q23r8",  "pol_prtds_mira"  = "q23r9", "pol_prtds_U"     = "q23r10",
      "pol_prtds_averd"   = "q23r11", "pol_prtds_pactoh" = "q23r12", "pol_prtds_notie" = "q23r13", "pol_prtds_otro" = "q23r14", "pol_prtds_nodic" = "q23r15",
      "pol_pres" = "q24", "izq_der" = "q25", 
      # Reforms
      "prop_eschr_labrl" = "q26r1", "prop_eschr_pensl" = "q26r2", "prop_eschr_fepc" = "q26r3", "prop_eschr_salud" = "q26r4", "prop_eschr_paz" = "q26r5", "prop_eschr_energ" = "q26r6", "prop_eschr_ning" = "q26r7",
      "prop_entd_labrl"  = "q27r1", "prop_entd_pensl"  = "q27r2", "prop_entd_fepc"  = "q27r3", "prop_entd_salud"  = "q27r4", "prop_entd_paz"  = "q27r5", "prop_entd_energ"  = "q27r6", "prop_entd_ning"  = "q27r7",
      "prop_acrd_labrl"  = "q28r1", "prop_acrd_pensl"  = "q28r2", "prop_acrd_fepc"  = "q28r3", "prop_acrd_salud"  = "q28r4", "prop_acrd_paz"  = "q28r5", "prop_acrd_energ"  = "q28r6", "prop_acrd_ning"  = "q28r7",
      "pais_gnrl" = "q29",
      # Trust in institutions
      "pais_army" = "q30ar1", "pais_police" = "q30ar2", "pais_prsdnt" = "q30ar3", "pais_gvnrs" = "q30ar4", "pais_mayors" = "q30ar5", "pais_parties" = "q30ar6", "pais_congrs" = "q30ar7", "pais_con_crt" = "q30ar8", "pais_jst_crt" = "q30ar9", 
      "pais_dmcrc" = "q30b", "pais_econ" = "q31",
      # Information reception
      "info_pernal" = "q32r1", "info_perreg" = "q32r2", "info_radnal" = "q32r3", "info_radlol" = "q32r4", "info_pscall" = "q32r5", "info_tvnal" = "q32r6",  "info_tvreg" = "q32r7", 
      "info_web" = "q32r8", "info_redscl" = "q32r9", "info_vlls" = "q32r10", "info_afic" = "q32r11", "info_voz" = "q32r12", "info_nose" = "q32r13",
      # Subsidies
      "ffsr_gnrl" = "q33", "ffsr_dsl" = "q34", "ffsr_gas" = "q35", "gas_super" = "q36", "dsl_super" = "q37",
      "benefic" = "q38", "derecho" = "q39", "yo_amnto" = "q40", "pobre_amnto" = "q41a", "rica_amnto" = "q41b",
      # Protests
      "protestas" = "q41c", "protestas_lid_trans" = "q41dr1", "protestas_lid_sindi" = "q41dr2", "protestas_lid_estu" = "q41dr3", "protestas_lid_campe" = "q41dr4", "protestas_lid_indi" = "q41dr5", "protestas_lid_nose" = "q41dr6", 
      "attn2" = "q41e",
      # First stage
      "frst_a" = "q42a", "frst_b" = "q42b", "frst_c" = "q42c", "frst_d" = "q42d",
      # Support
      "ffsr_mnt" = "q43", "ffsr_prcl" = "q44", "ffsr_complet" = "q45",
      # Conditional support
      "rr_lmpsm"   = "q46r1", "rr_pobre" = "q46r2", "rr_afctds" = "q46r3", "rr_impuesto" = "q46r4", "rr_deuda"   = "q46r5", 
      "rr_etransp" = "q46r6", "rr_paz"   = "q46r7", "rr_edu"    = "q46r8", "rr_ncer"     = "q46r9", "rr_deforst" = "q46r10", 
      "rr_mas" = "q47", "rr_mas_si" = "q48", "sesgo" = "q49",
      # Treatment groups
      "T_A" = "HCELLSr1", "T_B" = "HCELLSr2", "T_C" = "HCELLSr3", "T_D" = "HCELLSr4", 
      # Control groups
      "C_A" = "HCELLSr5", "C_B" = "HCELLSr6", "C_C" = "HCELLSr7", "C_D" = "HCELLSr8", "C_no_frst" = "HCELLSr9")

data_time <- data_0 %>%
  select(ID, qtime, pagetimeIntro_q42a:pagetimeIntroq42d)

# 1.2  Adjust individual variables ####

data_1.2 <- data_1.1 %>%
  # Sociodemographics
  mutate(date  = as.Date(date),
         gnro  = labelled(gnro, labels = c("Male" = 1, "Female" = 2, "Other" = 3)),
         prsns = labelled(prsns, labels = c("1" = 1, "2" = 2, "3" = 3, "4" = 4, "5 or more" = 5)),
         etnia = labelled(etnia, labels = c("Indigenous" = 1, "Gypsy" = 2, "Raizal" = 3, "Palenquero" = 4, "Afro-Colombian" = 5, "None" = 6)),
         edu   = ifelse(edu == 5,3,
                        ifelse(edu == 6,5,edu)),
         edu   = labelled(edu, labels = c("None" = 1, "Primary" = 2, "Secondary" = 3, "Technial" = 4, "University or Postgraduate" = 5)),
         trbjo = labelled(trbjo, labels = c("Full-time work" = 1, "Part-time work" = 2, "Studying" = 3, "Studying and working" = 4,
                                            "Housework" = 5, "Retired" = 6, "Looking for work" = 7, "Not looking for work" = 8)),
         ingrso = labelled(ingrso, labels = c("Between 0 – 310,000 COP" = 1,"Between 310,001 – 600,000 COP" = 2,"Between 600,001 – 900,000 COP" = 3,"Between 900,001 – 1,100,000 COP" = 4,"Between 1,100,001 – 1,400,000 COP" = 5,
                                              "Between 1,400,001 – 1,600,000 COP" = 6,"Between 1,600,001 – 1,825,000 COP" = 7,"Between 1,825,001 – 2,600,000 COP" = 8,"Between 2,600,001 – 4,150,000 COP" = 9,"More than 4,150,000 COP" = 10)),
         grp_ingrso = labelled(grp_ingrso, labels = c("Low" = 1,"Lower-middle" = 2,"Middle" = 3,"Upper-middle" = 4,"High" = 5)))%>%
  # Vehicle use
  mutate_at(vars("moto_dias", "carro_dias"), ~ case_when(
    . == 1 ~ 0,  # No days
    . == 2 ~ 1,  # 1 day per week
    . == 3 ~ 2,  # 2 days per week
    . == 4 ~ 3,  # 3 days per week
    . == 5 ~ 4,  # 4 days per week
    . == 6 ~ 5,  # 5 days per week
    . == 7 ~ 6,  # 6 days per week
    . == 8 ~ 7,  # 7 days per week
    TRUE ~ NA_real_))%>%
  mutate_at(vars("moto_dias", "carro_dias"), ~ labelled(., labels = c("No days" = 0,"1 day per week" = 1,"2 days per week" = 2,"3 days per week" = 3,
                                                                  "4 days per week" = 4,"5 days per week" = 5,"6 days per week" = 6,"7 days per week" = 7)))%>%
  mutate_at(vars("carro_combus_gsln", "carro_combus_acpm", "carro_combus_gas", "carro_combus_elect", "carro_combus_other"), ~ labelled(., labels = c("No" = 0, "Yes" = 1)))%>%
  mutate_at(vars("moto", "carro", "transpub", "taxi", "bici"), ~ ifelse(. == 2,0,.))%>%
  mutate_at(vars("moto", "carro", "transpub", "taxi", "bici"), ~ labelled (., labels = c("No" = 0, "Yes" = 1)))%>%
  mutate_at(vars("transpub_dias", "taxi_dias", "bici_dias"), ~ labelled(., labels = c("Almost every day" = 1, "Several times a week" = 2, "Once or twice a week" = 3, "A couple of times a month" = 4, "Never" = 5)))%>%
  # Cooking fuel
  mutate(ccnr = labelled(ccnr, labels = c("Electricity" = 1, "Natural gas" = 2, "Propane gas" = 3, "Cocinol" = 4, "Firewood" = 5, "Coal" = 6, "Waste" = 7, "No cooking" = 8)))%>%
  # Climate 
  mutate_at(vars(cc_info, cc_preocup), ~ labelled(., labels = c("Not at all" = 1, "Little" = 2, "Moderate" = 3, "Somewhat" = 4, "Greatly" = 5)))%>%
  mutate(cc_futuro = case_when(cc_futuro == 1 ~ 5,
                               cc_futuro == 2 ~ 4,
                               cc_futuro == 3 ~ 3,
                               cc_futuro == 4 ~ 2,
                               cc_futuro == 5 ~ 1,
                               TRUE ~ NA_real_))%>%
  mutate(cc_futuro = labelled(cc_futuro, labels = c("Very urgent" = 5,"Urgent in the future" = 4,"Nothing to do" = 3,"Will never be urgent" = 2,"Doesn't know" = 1)))%>%
  mutate(cc_econ = case_when(cc_econ == 1 ~ 5,
                             cc_econ == 3 ~ 4,
                             cc_econ == 5 ~ 3,
                             cc_econ == 2 ~ 2,
                             cc_econ == 4 ~ 1,
                             TRUE ~ NA_real_))%>%
  mutate(cc_econ = labelled(cc_econ, labels = c("More priority to climate change" = 5,"Equal priority to climate change and economy" = 4,"I don't know" = 3,"More priority to the economy" = 2,"Other:" = 1)))%>%
  mutate_at(vars("cc_imp_co2", "cc_imp_pers", "cc_imp_equit"), ~ labelled(., labels = c("Not important" = 1,"Very little important" = 2,"Moderately important" = 3,"Somewhat important" = 4,"Very important" = 5)))%>%
  # Political variables
  mutate(pol_pres = labelled(pol_pres, labels = c("G Petro" = 1, "Rodolfo H" = 2, "Blank" = 3, "Did not vote" = 4, "Prefer not to say" = 5)))%>%
  mutate_at(vars(starts_with("pol_prtds")), ~ labelled(., labels = c("No" = 0, "Yes" = 1)))%>%
  mutate(izq_der = labelled(izq_der, labels = c("Far-left" = 1,"Left" = 2,"Centrist" = 3,"Right" = 4,"Far-right" = 5,"Apolitical" = 6)))%>%
  mutate(pais_gnrl = labelled(pais_gnrl, labels = c("Progressing" = 1,"Stagnated" = 2,"In decline" = 3,"Prefer not to say" = 4)))%>%
  mutate_at(vars("pais_dmcrc", "pais_econ"), ~ labelled(., labels = c("Very bad" = 1,"Bad" = 2,"Neutral" = 3,"Good" = 4,"Very good" = 5)))%>%
  # Institutional trust
  mutate_at(vars("pais_army":"pais_jst_crt"), ~ labelled(., labels = c("No trust at all" = 1, "Little trust" = 2, "Moderate trust" = 3, "Some trust" = 4, "Complete trust" = 5)))%>%
  # Protests
  mutate(protestas = labelled(protestas, labels = c("Yes" = 1, "No" = 2,"Not sure" = 3)))%>%
  mutate_at(vars(starts_with("prop_eschr"), starts_with("prop_entd"), starts_with("prop_acrd"), starts_with("info_"), starts_with("protestas_lid")), ~ labelled(., labels = c("No" = 0, "Yes" = 1)))%>%
  mutate_at(vars("ffsr_gnrl":"dsl_super"), ~ labelled(., labels = c("Yes" = 1, "No" = 2, "I don't know" = 3)))%>%
  # Perceptions
  mutate_at(vars("benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto"), ~ labelled(., labels = c("Not at all" = 1, "Little" = 2,"Undecided" = 3,"Somewhat" = 4,"Very much" = 5)))%>%
  # Attention checks
  mutate(attn1 = labelled(attn1, labels = c("Right" = 5)),
         attn2 = labelled(attn2, labels = c("Wrong" = 1, "Wrong" = 2, "Right" = 3, "Wrong" = 4)))%>%
  # Bias
  mutate(sesgo = labelled(sesgo, labels = c("No bias" = 1, "Left-leaning" = 2, "Centrist" = 3, "Right-leaning" = 4, "Not sure" = 5)))%>%
  # Unconditional support
  mutate_at(vars("ffsr_mnt", "ffsr_prcl", "ffsr_complet"), ~ labelled(., labels = c("Strongly disagree" = 1,"Disagree" = 2,"Neutral" = 3,"Agree" = 4,"Strongly agree" = 5)))%>%
  # First stage
  mutate(frst_a = ifelse(frst_a == 2,1,0),
         frst_b = ifelse(frst_b %in% c(3,4,5),1,0),
         frst_c = ifelse(frst_c %in% c(3,4,5),1,0),
         frst_d = ifelse(frst_d %in% c(3,4,5),1,0))%>%
  mutate_at(vars(starts_with("frst_")), ~ labelled(., labels = c("Incorrect" = 0, "Correct" = 1)))%>%
  # Treatment and control
  mutate_at(vars("T_A":"C_no_frst"), ~ labelled(., labels = c("No" = 0, "Yes" = 1)))%>%
  # Conditional support
  mutate_at(vars("rr_lmpsm":"rr_deforst"), ~ labelled(., labels = c("Not at all" = 1, "Little" = 2,"Undecided" = 3,"Somewhat" = 4,"Very much" = 5)))%>%
  mutate(rr_mas = ifelse(rr_mas == 2,0,
                         ifelse(rr_mas == 1, 1, rr_mas)))%>%
  mutate(rr_mas = labelled(rr_mas, labels = c("No" = 0, "Yes" = 1)))
  
rm(data_1.1)  

# 1.3  Create new columns ####

data_1.3 <- data_1.2 %>%
  # Average institutional trust
  mutate(pais_inst = round(rowMeans(select(., pais_army, pais_police, pais_prsdnt, pais_gvnrs, pais_mayors, pais_parties, pais_congrs, pais_con_crt, pais_jst_crt), na.rm = TRUE)))%>%
  mutate(pais_inst = labelled(pais_inst, labels = c("No trust at all" = 1, "Little trust" = 2, "Moderate trust" = 3, "Some trust" = 4, "Complete trust" = 5)))%>%
  # Average trust in individuals
  mutate(pais_indv = round(rowMeans(select(., pais_prsdnt, pais_gvnrs, pais_mayors), na.rm = TRUE)))%>%
  mutate(pais_indv = labelled(pais_indv, labels = c("No trust at all" = 1, "Little trust" = 2, "Moderate trust" = 3, "Some trust" = 4, "Complete trust" = 5)))%>%
  # Conditional support pooled
  mutate(conditional = round(rowMeans(select(., "rr_lmpsm":"rr_deforst"), na.rm = TRUE)))%>%
  mutate(conditional = labelled(conditional, labels = c("Not at all" = 1, "Little" = 2,"Undecided" = 3,"Somewhat" = 4,"Very much" = 5)))%>%
  # Treatment variable
  mutate(treatment = ifelse(T_A == 1 | T_B == 1 | T_C == 1 | T_D == 1,1,0))%>%
  # Group variable
  mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
                        ifelse(T_B == 1 | C_B == 1, "B",
                               ifelse(T_C == 1 | C_C == 1, "C",
                                      ifelse(T_D == 1 | C_D == 1, "D", "Control")))))%>%
  mutate(Group = factor(Group, level = c("Control", "A", "B", "C", "D")))

rm(data_1.2)

# 1.4  Urban/rural and province information ####

UR_data <- read.xlsx("Urb_Rur_CEDE_150425.xlsx")%>%
  mutate(urban_01 = ifelse(indrural <= 0.5,1,0))%>%
  select(municipio, depto, urban_01)
Provinces <- read.xlsx("Provinces_Regions.xlsx")

data_1.4 <- data_1.3 %>%#
  mutate(municipio = haven::as_factor(mncp),
         depto     = haven::as_factor(dept))%>%
  left_join(UR_data)%>%
  left_join(Provinces, by = c("depto" = "labels"))%>%
  select(-municipio, -depto)%>%
  select(ID:mncp, urban_01, Province, everything())

rm(UR_data, Provinces, data_1.3)

# 2    Prepare cleaning ####

data_2 <- data_1.4 %>%
  # Respondents that are older than 80 or 99
  mutate(filter_1a = ifelse(edad > 80,1,0),
         filter_1b = ifelse(edad >= 99,1,0))%>%
  # Respondents that are too fast or too slow on aggregate
  left_join(data_time, by = "ID")%>%
  mutate(filter_2a = ifelse(qtime <= quantile(data_0$qtime, prob = 0.02),1,0),
         filter_2b = ifelse(qtime <= quantile(data_0$qtime, prob = 0.05),1,0), # Activated
         filter_2c = ifelse(qtime <= quantile(data_0$qtime, prob = 0.1), 1,0),
         filter_2d = ifelse(qtime >= quantile(data_0$qtime, prob = 0.9), 1,0),
         filter_2e = ifelse(qtime >= quantile(data_0$qtime, prob = 0.95),1,0),
         filter_2f = ifelse(qtime >= quantile(data_0$qtime, prob = 0.98),1,0), # Activated
         # Too fast or too slow on first stage question A
         filter_3a = ifelse(pagetimeIntro_q42a <= quantile(data_0$pagetimeIntro_q42a, prob = 0.02, na.rm = TRUE),1,0),
         filter_3b = ifelse(pagetimeIntro_q42a <= quantile(data_0$pagetimeIntro_q42a, prob = 0.05, na.rm = TRUE),1,0), # Activated
         filter_3c = ifelse(pagetimeIntro_q42a <= quantile(data_0$pagetimeIntro_q42a, prob = 0.1,  na.rm = TRUE),1,0),
         filter_3d = ifelse(pagetimeIntro_q42a >= quantile(data_0$pagetimeIntro_q42a, prob = 0.9,  na.rm = TRUE),1,0),
         filter_3e = ifelse(pagetimeIntro_q42a >= quantile(data_0$pagetimeIntro_q42a, prob = 0.95, na.rm = TRUE),1,0),
         filter_3f = ifelse(pagetimeIntro_q42a >= quantile(data_0$pagetimeIntro_q42a, prob = 0.98, na.rm = TRUE),1,0), # Activated
         # Too fast or too slow on first stage question B
         filter_3g = ifelse(pagetimeIntroq42b <= quantile(data_0$pagetimeIntroq42b, prob = 0.02, na.rm = TRUE),1,0),
         filter_3h = ifelse(pagetimeIntroq42b <= quantile(data_0$pagetimeIntroq42b, prob = 0.05, na.rm = TRUE),1,0), # Activated
         filter_3i = ifelse(pagetimeIntroq42b <= quantile(data_0$pagetimeIntroq42b, prob = 0.1,  na.rm = TRUE),1,0),
         filter_3j = ifelse(pagetimeIntroq42b >= quantile(data_0$pagetimeIntroq42b, prob = 0.9,  na.rm = TRUE),1,0),
         filter_3k = ifelse(pagetimeIntroq42b >= quantile(data_0$pagetimeIntroq42b, prob = 0.95, na.rm = TRUE),1,0),
         filter_3l = ifelse(pagetimeIntroq42b >= quantile(data_0$pagetimeIntroq42b, prob = 0.98, na.rm = TRUE),1,0), # Activated
         # Too fast or too slow on first stage question C
         filter_3m = ifelse(pagetimeIntroq42c <= quantile(data_0$pagetimeIntroq42c, prob = 0.02, na.rm = TRUE),1,0),
         filter_3n = ifelse(pagetimeIntroq42c <= quantile(data_0$pagetimeIntroq42c, prob = 0.05, na.rm = TRUE),1,0), # Activated
         filter_3o = ifelse(pagetimeIntroq42c <= quantile(data_0$pagetimeIntroq42c, prob = 0.1,  na.rm = TRUE),1,0),
         filter_3p = ifelse(pagetimeIntroq42c >= quantile(data_0$pagetimeIntroq42c, prob = 0.9,  na.rm = TRUE),1,0),
         filter_3q = ifelse(pagetimeIntroq42c >= quantile(data_0$pagetimeIntroq42c, prob = 0.95, na.rm = TRUE),1,0),
         filter_3r = ifelse(pagetimeIntroq42c >= quantile(data_0$pagetimeIntroq42c, prob = 0.98, na.rm = TRUE),1,0), # Activated
         # Too fast or too slow on first stage question D
         filter_3s = ifelse(pagetimeIntroq42d <= quantile(data_0$pagetimeIntroq42d, prob = 0.02, na.rm = TRUE),1,0),
         filter_3t = ifelse(pagetimeIntroq42d <= quantile(data_0$pagetimeIntroq42d, prob = 0.05, na.rm = TRUE),1,0), # Activated
         filter_3u = ifelse(pagetimeIntroq42d <= quantile(data_0$pagetimeIntroq42d, prob = 0.1,  na.rm = TRUE),1,0),
         filter_3v = ifelse(pagetimeIntroq42d >= quantile(data_0$pagetimeIntroq42d, prob = 0.9,  na.rm = TRUE),1,0),
         filter_3w = ifelse(pagetimeIntroq42d >= quantile(data_0$pagetimeIntroq42d, prob = 0.95, na.rm = TRUE),1,0),
         filter_3x = ifelse(pagetimeIntroq42d >= quantile(data_0$pagetimeIntroq42d, prob = 0.98, na.rm = TRUE),1,0), # Activated
         )%>%
  mutate_at(vars(starts_with("filter_3")), ~ ifelse(is.na(.),0,.))%>%
  # Respondents that answer attention check 2 wrongly
  mutate(filter_4 = ifelse(attn2 == 3,0,1))%>%
  # Respondents that give the same answer to all conditional FFSR options
  mutate(filter_5 = ifelse(rr_lmpsm == rr_pobre & rr_lmpsm == rr_afctds & rr_lmpsm == rr_impuesto & rr_lmpsm == rr_deuda & rr_lmpsm == rr_etransp & rr_lmpsm == rr_paz & rr_lmpsm == rr_edu & rr_lmpsm == rr_ncer & rr_lmpsm == rr_deforst,1,0))%>%
  select(-qtime, -pagetimeIntro_q42a, -pagetimeIntroq42b, -pagetimeIntroq42c, -pagetimeIntroq42d)%>%
  mutate(across(everything(), zap_label))

rm(data_time, data_1.4)

# 3    Save output ####

write_sav(data_2, "Outputs/Data_Cleaned.sav")

rm(data_2, data_0)