# Author: L. Missbach (leonard.missbach@pik-potsdam.de)

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "ggsci", "fixest", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled", "tidymodels")

options(scipen=999)

# 0.  Load data ####

data_0 <- read_sav("../X_Paper_Colombia_Experiment/Analysis/Inputs/DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - final.sav")

predictions_EXP <- read_csv("../2_Predictions/Colombia EXP/Predictions_Synthetic_Dataset_expenditures.csv")%>%
  rename(.pred_EXP = .pred)
predictions     <- read_csv("../2_Predictions/Colombia 65/Predictions_Synthetic_Dataset_65.csv")
# predictions_0 <- read_csv("../2_Predictions/Colombia 65/Predictions_Entire_Dataset_65.csv")

#quantile(read_csv("../Colombia EXP/Predictions_Entire_Dataset_expenditures.csv")$.pred, prob = 0.5)  # Median costs: 10,802,917

# 1. Renaming and cleaning (TBD) ####

# Update later

# data_0 <- read_sav("../Paper_Colombia_Experiment/Analysis/Inputs/Intermediate.sav")

data_0 <- read_sav("../X_Paper_Colombia_Experiment/Analysis/Outputs/Intermediate.sav")

# Basic data transformation
data_1 <- data_0 %>%
  mutate(ID = 1:n())%>%
  rename(status_quo                        = ffsr_mnt,
         unconditional_prcl                = ffsr_prcl,
         unconditional_complet             = ffsr_complet,
         unconditional_prcl_recode_2       = ffsr_prcl_recode_2,
         unconditional_prcl_recode_2extrem = ffsr_prcl_recode_2extrem,
         unconditional_prcl_recode_3       = ffsr_prcl_recode_3)%>%
  mutate(treatment = ifelse(rowSums(.[, grepl("^T_", names(.))] == 1) > 0, 1, 0))

data_1.1.1 <- data_1 %>%
  select(ID, conditional, conditional_recode_2, conditional_recode_2extrem, conditional_recode_3)%>%
  mutate(conditional_support = 1)%>%
  rename(support                = conditional,
         support_recode_2       = conditional_recode_2,
         support_recode_2extrem = conditional_recode_2extrem, 
         support_recode3        = conditional_recode_3)

data_1.1.2 <- data_1 %>%
  select(ID, unconditional_prcl, unconditional_prcl_recode_2, unconditional_prcl_recode_2extrem, unconditional_prcl_recode_3)%>%
  mutate(conditional_support = 0)%>%
  rename(support                = unconditional_prcl,
         support_recode_2       = unconditional_prcl_recode_2,
         support_recode_2extrem = unconditional_prcl_recode_2extrem, 
         support_recode3        = unconditional_prcl_recode_3)

data_1.1.3 <- data_1 %>%
  select(ID, rr_lmpsm:rr_deforst)%>%
  pivot_longer(-ID, names_to = "rr_", values_to = "not_rounded")%>%
  group_by(ID)%>%
  summarise(support = mean(not_rounded))%>%
  ungroup()%>%
  mutate(Type = "Not rounded")%>%
  mutate(conditional_support = 1)
  
data_1.1 <- bind_rows(data_1.1.1, data_1.1.2, data_1.1.3)%>%
  left_join(select(data_1, ID, starts_with("T_"), starts_with("C_"), treatment))%>%
  mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
                        ifelse(T_B == 1 | C_B == 1, "B",
                               ifelse(T_C == 1 | C_C == 1, "C",
                                      ifelse(T_D == 1 | C_D == 1, "D", "Control")))))%>%
  mutate(Group = factor(Group, level = c("Control", "A", "B", "C", "D")))

rm(data_1.1.1, data_1.1.2, data_1.1.3)

# 1.1 Individual cost treatment ####

# Income Group, Province

data_indiv <- data_1 %>%
  select(ID, ingrso, dept, urban, carro, moto, ccnr)%>%
  mutate(Income_Group = ifelse(ingrso %in% c(1,2,3),1,
                               ifelse(ingrso %in% c(4,5,6,7),2,
                                      ifelse(ingrso %in% c(8,9,10),3,NA))),
         Urban = ifelse(urban == 1, "Rural", "Urban"),
         Car   = ifelse(carro == 0, "No", "Yes"),
         Moto  = ifelse(moto  == 0, "No", "Yes"),
         CF    = ifelse(ccnr  == 1, "Electricity",
                        ifelse(ccnr == 2, "Gas", 
                               ifelse(ccnr == 3, "LPG",
                                      ifelse(ccnr == 4, "Kerosene", # TBD
                                             ifelse(ccnr == 5, "Firewood.Charcoal",
                                                    ifelse(ccnr %in% c(6,7,8), "Unknown", NA)))))),
         Province = ifelse(dept %in% c(4,6,12,14,19,20,27,29), "Atlántica",
                           ifelse(dept %in% c(3,10,21,33), "Oriental",
                                  ifelse(dept %in% c(13,11,22,31), "Pacifica", 
                                         ifelse(dept %in% c(2,5,7,8,15,18,23,25,26,28,30),"Central", 
                                                ifelse(dept %in% c(1,9,17,24,32), "Nuevo departamentos", NA))))))%>%
  select(ID, Income_Group, Province, Urban, Car, Moto, CF)

predictions_1 <- predictions %>%
  # left_join(predictions_EXP)
  mutate(Income_Group = ifelse(Income_Group_3_X2 == 1,2,
                               ifelse(Income_Group_3_X3 == 1,3,1)),
         CF    = ifelse(CF_Electricity == 1, "Electricity",
                        ifelse(CF_Firewood.Charcoal == 1, "Firewood.Charcoal",
                               ifelse(CF_Gas == 1, "Gas",
                                      ifelse(CF_Kerosene == 1, "Kerosene",
                                             ifelse(CF_LPG == 1, "LPG",
                                                    ifelse(CF_Unknown == 1, "Unknown", "Coal")))))),
         Province = ifelse(Province_Central == 1, "Central",
                           ifelse(Province_Nuevo.Departamentos == 1, "Nuevo departamentos",
                                  ifelse(Province_Oriental == 1, "Oriental",
                                         ifelse(Province_Pacífica == 1, "Pacifica", "Atlántica")))),
         Urban = ifelse(Urban_Urban == 1, "Urban", "Rural"),
         Car   = ifelse(Car_Yes == 1, "Yes", "No"),
         Moto  = ifelse(Motorcycle_Yes == 1, "Yes", "No"))%>%
  select(Income_Group, Province, Urban, Car, Moto, CF, .pred)%>%
  mutate(CF_2 = ifelse(CF %in% c("Firewood.Charcoal", "Unknown", "Coal"), "Solid",
                       ifelse(CF %in% c("Electricity", "Gas", "Kerosene"), "Liquid NA",
                              ifelse(CF %in% c("LPG"), "Liquid A", NA))))%>%
  group_by(Income_Group, Province, Urban, Car, Moto, CF_2)%>%
  mutate(.pred = mean(.pred))%>%
  ungroup()%>%
  mutate(Median_costs = ifelse(.pred > 0.0123214, "Above", "Below"))

predictions_2 <- left_join(data_indiv, predictions_1)

rm(data_indiv, predictions_1, predictions)

# 2. Main Analysis ####

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule Fixed Effects",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small", yesNo = c("Yes","No"))

dict_latex <- c(support = "Support (1-5)", "conditional_support" = "Conditional support", "conditional" = "Conditional", "unconditional_prcl" = "Unconditional",
                "T_A" = "Treatment A", "T_B" = "Treatment B", "T_C" = "Treatment C", "T_D" = "Treatment D",
                treatment = "Treatment",
                GroupA = "Group A", GroupB = "Group B", GroupC = "Group C", GroupD = "Group D", GroupControl = "Control",
                First_StageYes = "First Stage",
                "rr_pobre" = "Poor", "rr_afctds" = "Affected", "rr_deuda" = "Reduce debt", "rr_impuesto" = "Tax red.",
                "rr_lmpsm" = "LST", "rr_etransp" = "Transport", "rr_paz" = "Paz Total", "rr_edu" = "Education", "rr_ncer" = "Renewables", "rr_deforst" = "Deforestation")

# 2.1 H1 ####
# Conditional support is higher than unconditional support

# OLS
# support (1-5) over conditional_support (0/1) for a partial subsidy reform
# Conditional is an average over all answers

model_1.1 <- feols(support ~ conditional_support, data = filter(data_1.1, is.na(Type)))
model_1.2 <- feols(support ~ conditional_support, data = filter(data_1.1, !is.na(Type) | conditional_support == 0))
model_1.3 <- feols(support ~ conditional_support + treatment + Group, data = filter(data_1.1, is.na(Type)))
model_1.4 <- feols(support ~ conditional_support | ID, data = filter(data_1.1, is.na(Type)))

# tidy_1.1  <- tidy(model_1.1)%>%
#   mutate(type = "OLS")

etable(model_1.1, model_1.2, model_1.3, model_1.4, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H1.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 1: Support for a partial fossil fuel subsidy reform",  
       label = "tab:H1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!",
       extralines = list("-^Rounded (conditional)" = c("Yes", "No", "Yes", "Yes")),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from OLS regression REF on the support for a partial fossil fuel subsidy reform over whether such a reform would include compensation (conditional support) or not (unconditional support). The dependent variable expresses support on a five-point Lickert-scale. For conditional support, this variable is the rounded average support over ten different options for partial fossil fuel subsidy reform and one form of compensation. In Model (2), the dependent variable is the non-rounded average. Model (3) includes control variables for respondents receiving different first-stage questions (reference: control group) and for the treatment group. Model (4) includes a respondent-level fixed effect."))
)

rm(model_1.1, model_1.2, model_1.3, model_1.4)

# Logit
# support (0 (1-2)/1(4-5)) over conditional_support (0/1)
data_1.2 <- data_1.1 %>%
  filter(!is.na(support_recode_2))
model_1.2.1 <- feglm(support_recode_2 ~ conditional_support, family = binomial(link = "logit"), data = data_1.2)
# Probit
model_1.2.2 <- feglm(support_recode_2 ~ conditional_support, family = binomial(link = "probit"), data = data_1.2)
# tidy_1.2.1  <- tidy(model_1.2)%>%
#   mutate(type = "Logit")
# tidy_1.2.2  <- tidy(model_1.2)%>%
#   mutate(type = "Probit")

rm(data_1.2, model_1.2.1, model_1.2.2)

# support (0 (1-2)/1(4-5)) over conditional_support (0/1)
data_1.3 <- data_1.1 %>%
  filter(!is.na(support_recode_2extrem))
model_1.3.1 <- feglm(support_recode_2extrem ~ conditional_support, family = binomial(link = "logit"), data = data_1.3)
# Probit
model_1.3.2 <- feglm(support_recode_2extrem ~ conditional_support, family = binomial(link = "probit"), data = data_1.3)
# tidy_1.3.1  <- tidy(model_1.3.1)%>%
#   mutate(type = "Logit")
# tidy_1.3.2  <- tidy(model_1.3.2)%>%
#   mutate(type = "Probit")

rm(data_1.3, model_1.3.1, model_1.3.2)

# support (0(1-2)/1(3)/2(4-5))
# OLS
model_1.4 <- feols(support_recode3 ~ conditional_support, data = data_1.1)
tidy_1.4  <- tidy(model_1.4)%>%
  mutate(type = "OLS")
# model_1.4.2 <- feglm(support ~ conditional_support, family = (link = "probit"), data = data_1.4)
# tidy_1.2.2  <- tidy(model_1.2)%>%
#   mutate(type = "Probit")

rm(model_1.4, tidy_1.4)

# Quick visualization

data_1.X <- data_1.1 %>%
  filter(is.na(Type))%>%
  group_by(conditional_support, support)%>%
  summarise(number = n())%>%
  ungroup()%>%
  mutate(conditional_support = ifelse(conditional_support == 1, "Conditional", "Unconditional"))%>%
  mutate(conditional_support = factor(conditional_support, levels = c("Unconditional", "Conditional")))%>%
  mutate(support = case_when(support == 1 ~ "Strongly disagree",
                             support == 2 ~ "Disagree",
                             support == 3 ~ "Neutral",
                             support == 4 ~ "Agree",
                             support == 5 ~ "Strongly agree"))%>%
  mutate(support = factor(support, levels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))

P_0 <- ggplot(data_1.X)+
  geom_col(aes(x = conditional_support, y = number, group = conditional_support, fill = conditional_support), alpha = 0.7, colour = "black", width = 0.7)+
  facet_grid(. ~ support)+
  scale_fill_nejm(labels = c("Unconditional", "Conditional"), name = "Support for partial fossil fuel subsidy reform")+
  scale_y_continuous(expand = c(0,0))+
  theme_bw()+
  coord_cartesian(ylim = c(0,2000))+
  xlab("")+
  ylab("Survey respondents")+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/B_H1_S1.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_0)
dev.off()

rm(data_1.X, P_0)

# 2.2 H2 ####

# Additional information on the effects of removing fossil fuel subsidy will increase respondents unconditional and conditional support for FFSR.

data_1.2.1 <- data_1.1 %>%
  # Unconditional support
  filter(conditional_support == 0)%>%
  mutate(First_Stage = ifelse(Group == "Control", "No","Yes"))

model_1.2.1 <- feols(support ~ treatment + Group, data = data_1.2.1)
model_1.2.2 <- feols(support ~ treatment + First_Stage, data = data_1.2.1)

# tidy_1.2.1 <- tidy(model_1.2.1)%>%
#   mutate(Type         = "OLS",
#          type_support = "Unconditional")

data_1.2.2 <- data_1.1 %>%
  # Conditional support
  filter(conditional_support == 1)%>%
  mutate(First_Stage = ifelse(Group == "Control", "No","Yes"))

model_1.2.3 <- feols(support ~ treatment + Group, data = data_1.2.2)
model_1.2.4 <- feols(support ~ treatment + First_Stage, data = data_1.2.2)
# tidy_1.2.1 <- tidy(model_1.2.2)%>%
#   mutate(Type         = "OLS",
#          type_support = "Conditional")

etable(model_1.2.1, model_1.2.2, model_1.2.3, model_1.2.4, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2: Unconditional and conditional support for a partial fossil fuel subsidy reform",  
       label = "tab:H2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", headers = c("Unconditional", "Unconditional", "Conditional", "Conditional"), order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression REF on the unconditional and conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the fossil fuel subsidy reform. The dependent variable expresses support on a five-point Lickert-scale. For conditional support, this variable is the rounded average support over ten different options for partial fossil fuel subsidy reform and one form of compensation."))
)

rm(data_1.2.1, data_1.2.2, model_1.2.1, model_1.2.2, data_1.1)

# Prep for Sub-Hypotheses

data_2 <- data_1 %>%
  select(ID, starts_with("T_"), starts_with("C_"), treatment, 
         rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda, rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst,
         frst_a, frst_b, frst_c, frst_d,
         grp_ingrso)%>%
  mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
                        ifelse(T_B == 1 | C_B == 1, "B",
                               ifelse(T_C == 1 | C_C == 1, "C",
                                      ifelse(T_D == 1 | C_D == 1, "D", "Control")))))

# 2.2.1 H2.1 ####

# Treatment A information (!) will increase conditional support for targeted cash transfers and social protection

data_2.2.1 <- data_2 %>%
  filter(Group == "A")

model_2.2.1.1 <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_A,  data = data_2.2.1)
model_2.2.1.2 <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_A,  data = data_2.2.1)

etable(model_2.2.1.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.1.1.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.1: Conditional support for a partial fossil fuel subsidy reform (Treatment A - part I)",  
       label = "tab:H2.1.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform.")))

etable(model_2.2.1.2, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.1.2.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.1: Conditional support for a partial fossil fuel subsidy reform (Treatment A - part II)",  
       label = "tab:H2.1.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform.")))

rm(data_2.2.1, model_2.2.1.1, model_2.2.1.2)

# Including attention check

data_2.2.1.A <- data_2 %>%
  filter(Group == "A")%>%
  #filter(frst_a == 1)
  filter(frst_a == 1 | C_A == 1)

model_2.2.1.A <- feols(c(rr_pobre, rr_afctds, rr_deuda) ~ T_A,  data = data_2.2.1.A)

rm(data_2.2.1.A, model_2.2.1.A)

# Including splitting by being more or less affected than the median.

data_2.2.1.B <- data_2.2.1 %>%
  left_join(predictions_2)%>%
  mutate(log_pred = log(.pred))%>%
  mutate(T_weighted = T_A*log_pred)

model_2.2.1.B <- feols(c(rr_pobre, rr_afctds, rr_deuda) ~ T_A, fsplit = ~ Median_costs, data = data_2.2.1.B)
model_2.2.1.C <- feols(c(rr_pobre, rr_afctds, rr_deuda) ~ T_weighted, data = data_2.2.1.B)

ggplot(filter(data_2.2.1.B, T_A == 1))+
  geom_histogram(aes(x = .pred), bins = 50, colour = "black", fill = "grey")+
  theme_bw()

# Including splitting by being more or less affected than the median.

data_2.2.1.C <- data_2.2.1 %>%
  mutate(Group = ifelse(grp_ingrso %in% c(1,2), "Low and lower-middle",
                        ifelse(grp_ingrso %in% c(3), "Middle",
                               ifelse(grp_ingrso %in% c(4,5), "Upper-middle and high", NA))))

model_2.2.1.C <- feols(c(rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_A, fsplit = ~ Group, data = data_2.2.1.C)

# 2.2.2 H2.2 ####

# Treatment B information (!) will increase conditional support for uniform cash transfers and social protection

data_2.2.2 <- data_2 %>%
  filter(Group == "B")

model_2.2.2.1 <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_B,  data = data_2.2.2)
model_2.2.2.2 <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_B,  data = data_2.2.2)

etable(model_2.2.2.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.2.1.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.2: Conditional support for a partial fossil fuel subsidy reform (Treatment B - part I)",  
       label = "tab:H2.2.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform.")))

etable(model_2.2.2.2, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.2.2.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.2: Conditional support for a partial fossil fuel subsidy reform (Treatment B - part II)",  
       label = "tab:H2.2.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform.")))

rm(data_2.2.2, model_2.2.2.1, model_2.2.2.2)

# Including attention check

data_2.2.2.B <- data_2 %>%
  filter(Group == "B")%>%
  mutate(FRST_B = ifelse(frst_b %in% c(3,4,5), "Correct", "Not correct"))%>%
  #filter(FRST_B == "Correct")
  filter(FRST_B == "Correct" | C_B == 1)

model_2.2.2.B <- feols(c(rr_pobre, rr_afctds, rr_deuda) ~ T_B,  data = data_2.2.2.B)

rm(data_2.2.2.B, model_2.2.2.B)

# 2.2.3 H2.3 ####

data_2.2.3 <- data_2 %>%
  filter(Group == "C")

model_2.2.3.1 <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_C,  data = data_2.2.3)
model_2.2.3.2 <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_C,  data = data_2.2.3)

etable(model_2.2.3.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.3.1.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.3: Conditional support for a partial fossil fuel subsidy reform (Treatment C - part I)",  
       label = "tab:H2.3.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the efficiency of government resource use.")))

etable(model_2.2.3.2, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.3.2.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.3: Conditional support for a partial fossil fuel subsidy reform (Treatment C - part II)",  
       label = "tab:H2.3.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the efficiency of government resource use.")))

rm(data_2.2.3, model_2.2.3.1, model_2.2.3.2)

# Including attention check

data_2.2.3.C <- data_2 %>%
  filter(Group == "C")%>%
  mutate(FRST_C = ifelse(frst_c %in% c(3,4,5), "Correct", "Not correct"))%>%
  filter(FRST_C == "Correct")
  filter(FRST_C == "Correct" | C_C == 1)

model_2.2.3.C <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_C,  data = data_2.2.3.C)

rm(data_2.2.3.C, model_2.2.3.C)

# 2.2.4 H2.4 ####

data_2.2.4 <- data_2 %>%
  filter(Group == "D")

model_2.2.4.1 <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_D,  data = data_2.2.4)
model_2.2.4.2 <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_D,  data = data_2.2.4)

etable(model_2.2.4.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.4.1.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.4: Conditional support for a partial fossil fuel subsidy reform (Treatment D - part I)",  
       label = "tab:H2.4.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the environmental consequences of fossil fuel subsidies.")))

etable(model_2.2.4.2, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.4.2.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.4: Conditional support for a partial fossil fuel subsidy reform (Treatment D - part II)",  
       label = "tab:H2.4.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the environmental consequences of fossil fuel subsidies.")))

rm(data_2.2.4, model_2.2.4.1 , model_2.2.4.2)

data_2.2.4.D <- data_2 %>%
  filter(Group == "D")%>%
  mutate(FRST_D = ifelse(frst_d %in% c(3,4,5), "Correct", "Not correct"))%>%
  filter(FRST_D == "Correct")
  filter(FRST_D == "Correct" | C_D == 1)

model_2.2.4.D <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_D,  data = data_2.2.4.D)

rm(data_2.2.4.D, model_2.2.4.D)

# 2.3 H3 ####

# Additional information will increase unconditional and conditional support of respondents who do not endorse the incumbent president.

data_3 <- data_1 %>%
  select(ID, starts_with("T_"), starts_with("C_"), treatment, 
         conditional, unconditional_prcl, pol_pres)%>%
  mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
                        ifelse(T_B == 1 | C_B == 1, "B",
                               ifelse(T_C == 1 | C_C == 1, "C",
                                      ifelse(T_D == 1 | C_D == 1, "D", "Control")))))%>%
  mutate(Support = ifelse(pol_pres == 1, "Supporter",
                          ifelse(pol_pres == 2, "Non-supporter", NA)))%>%
  mutate(T_S = ifelse(Support == "Non-supporter" & treatment == 1,1,0))

# data_3.1 <- data_3 %>%
#   filter(Support == "Supporter")
# 
# model_3.1.1 <- feols(unconditional_prcl ~ treatment + Group, data = data_3.1)
# model_3.1.2 <- feols(conditional ~ treatment + Group, data = data_3.1)

# For supporters: No treatment effect.

# data_3.2 <- data_3 %>%
#   filter(Support == "Non-supporter")
# 
# model_3.2.1 <- feols(unconditional_prcl ~ treatment + Group, data = data_3.2)
# model_3.2.2 <- feols(conditional ~ treatment + Group, data = data_3.2)

model_test <- feols(unconditional_prcl ~ T_S + treatment + Group + Support, data = filter(data_3, !is.na(Support)))

# model_3.2 <- feols(conditional ~ T_S + Group + treatment + Support, data = data_3)
model_3.3 <- feols(c(unconditional_prcl, conditional) ~ treatment + Group, split = ~Support, data = filter(data_3, !is.na(Support)))

etable(model_3.3, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H3.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 3: Unconditional and conditional support for a partial fossil fuel subsidy reform among supporters and non-supporters of the government",  
       label = "tab:H3", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the unconditional and conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about fossil fuel subsidies, differentiated by whether respondets support the government or not.")))

rm(model_3.3)


# 3. Supplementary Analyses ####

# Unconditional support over perceived right to receive subsidies

data_3.1 <- count(data_1, derecho, unconditional_prcl)%>%
  mutate(derecho            = factor(derecho, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")))%>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels= c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))

P_3.1 <- ggplot(data_3.1, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ derecho, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.1$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S1.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.1)
dev.off()

rm(data_3.1, P_3.1)

# Unconditional support over perceived personal effects

data_3.2 <- count(data_1, yo_amnto, unconditional_prcl)%>%
  mutate(yo_amnto           = factor(yo_amnto, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")))%>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels= c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))

P_3.2 <- ggplot(data_3.2, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ yo_amnto, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.2$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S2.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.2)
dev.off()

rm(data_3.2, P_3.2)

# Awareness of Fossil Fuel Subsidy

data_3.3 <- data_1 %>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))%>%
  mutate(ffsr_gnrl          = factor(ffsr_gnrl,          labels = c("Yes", "I don't know", "No")),
         ffsr_dsl           = factor(ffsr_dsl,           labels = c("Yes", "I don't know", "No")),
         ffsr_gas           = factor(ffsr_gas,           labels = c("Yes", "I don't know", "No")),
         gas_super          = factor(gas_super,          labels = c("Yes", "I don't know", "No")),
         dsl_super          = factor(dsl_super,          labels = c("Yes", "I don't know", "No")))

data_3.3.1 <- count(data_3.3, unconditional_prcl, ffsr_gnrl)
data_3.3.2 <- count(data_3.3, unconditional_prcl, ffsr_dsl)%>%
  filter(!is.na(ffsr_dsl))
data_3.3.3 <- count(data_3.3, unconditional_prcl, ffsr_gas)%>%
  filter(!is.na(ffsr_gas))
data_3.3.4 <- count(data_3.3, unconditional_prcl, gas_super)
data_3.3.5 <- count(data_3.3, unconditional_prcl, dsl_super)

P_3.3.1 <- ggplot(data_3.3.1, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ ffsr_gnrl, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  coord_cartesian(ylim = c(0,max(data_3.3.1$n + 10)))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.3.2 <- ggplot(data_3.3.2, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ ffsr_dsl, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("¿Cree usted que, en este momento, en Colombia hay subsidios a los precios del diésel/ACPM?")+
  coord_cartesian(ylim = c(0,max(data_3.3.2$n + 10)))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.3.3 <- ggplot(data_3.3.3, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ ffsr_gas, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("¿Cree usted que, en este momento, en Colombia hay subsidios a los precios de la gasolina?")+
  coord_cartesian(ylim = c(0,max(data_3.3.3$n + 10)))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.3.4 <- ggplot(data_3.3.4, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ gas_super, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("¿Cree usted que si se aumentara el precio de la gasolina \n se verían afectados los precios de los alimentos en el supermercado?")+
  coord_cartesian(ylim = c(0,950))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.3.5 <- ggplot(data_3.3.5, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ dsl_super, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("¿Cree usted que si se aumentara el precio del diésel/ACPM  \n se verían afectados los precios de los alimentos en el supermercado")+
  coord_cartesian(ylim = c(0,950))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S3_%d.jpg", width = 13, height = 9, unit = "cm", res = 600)
print(P_3.3.1)
print(P_3.3.2)
print(P_3.3.3)
print(P_3.3.4)
print(P_3.3.5)
dev.off()

rm(data_3.3, P_3.3.1, P_3.3.2, P_3.3.3, data_3.3.1, data_3.3.2, data_3.3.3, data_3.3.4, data_3.3.5, P_3.3.4, P_3.3.5)

# Agreement with Fossil Fuel Subsidy Reform or Energy Transition

data_3.4 <- data_1 %>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))%>%
  mutate(prop_acrd_fepc     = factor(prop_acrd_fepc,     labels = c("Yes", "No")),
         prop_acrd_energ    = factor(prop_acrd_energ,    labels = c("Yes", "No")))

data_3.4.1 <- count(data_3.4, unconditional_prcl, prop_acrd_fepc)
data_3.4.2 <- count(data_3.4, unconditional_prcl, prop_acrd_energ)

P_3.4.1 <- ggplot(filter(data_3.4.1, !is.na(prop_acrd_fepc)), aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ prop_acrd_fepc, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("De las propuestas que usted entiende, ¿con cuáles está de acuerdo? \n Reforma de los subsidios a los combustibles")+
  coord_cartesian(ylim = c(0,250))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.4.2 <- ggplot(filter(data_3.4.2, !is.na(prop_acrd_energ)), aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ prop_acrd_energ, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  ggtitle("De las propuestas que usted entiende, ¿con cuáles está de acuerdo? Transición energética")+
  coord_cartesian(ylim = c(0,250))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S4_%d.jpg", width = 13, height = 9, unit = "cm", res = 600)
print(P_3.4.1)
print(P_3.4.2)
dev.off()

# Perception of personal benefit

data_3.5 <- count(data_1, benefic, unconditional_prcl)%>%
  mutate(benefic            = factor(benefic, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")))%>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels= c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))

P_3.5 <- ggplot(data_3.5, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ benefic)+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Cuánto considera que usted se ha beneficiado de los subsidios a los combustibles en los últimos 12 meses?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.5$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S5.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.5)
dev.off()

rm(data_3.5, P_3.5)

# Perception of financial impact

data_3.6 <- count(data_1, pobre_amnto, unconditional_prcl)%>%
  mutate(pobre_amnto        = factor(pobre_amnto, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")))%>%
  mutate(unconditional_prcl = factor(unconditional_prcl, labels= c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")))

P_3.6 <- ggplot(data_3.6, aes(x = unconditional_prcl, y = n))+
  facet_grid(. ~ pobre_amnto, labeller = labeller(group = labels))+
  geom_col(aes(fill = unconditional_prcl), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos \n por un posible aumento de los precios de los combustibles en el 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.6$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S6.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.6)
dev.off()

# Treatment effect A over yo_amnto / cc_imp_pers / cc_imp_equit

data_3.7 <- data_1 %>%
  mutate(yo_amnto           = factor(yo_amnto, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         cc_imp_pers        = factor(cc_imp_pers, labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")),
         cc_imp_equit       = factor(cc_imp_equit, labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")))

data_3.7.1 <- data_3.7 %>%
  mutate(Affected = ifelse(yo_amnto == "Nothing" | yo_amnto == "Very little", "Not affected",
                           ifelse(yo_amnto == "Somewhat" | yo_amnto == "Greatly", "Affected", NA)))%>%
  filter(!is.na(Affected))%>%
  filter(T_A == 1 | C_A == 1)

model_3.7.1 <- feols(c(unconditional_prcl, conditional) ~ T_A, split = ~ Affected, data = data_3.7.1)

data_3.7.2 <- data_3.7 %>%
  mutate(Impact = ifelse(cc_imp_pers == "Not importantg" | cc_imp_pers == "Very little important", "Not important",
                           ifelse(cc_imp_pers == "Somewhat important" | cc_imp_pers == "Very important", "Important", NA)))%>%
  filter(!is.na(Impact))%>%
  filter(T_A == 1 | C_A == 1)

model_3.7.2 <- feols(c(unconditional_prcl, conditional) ~ T_A, split = ~ Impact, data = data_3.7.2)

data_3.7.3 <- data_3.7 %>%
  mutate(Equity = ifelse(cc_imp_equit == "Not importantg" | cc_imp_equit == "Very little important", "Not important",
                         ifelse(cc_imp_equit == "Somewhat important" | cc_imp_equit == "Very important", "Important", NA)))%>%
  filter(!is.na(Equity))%>%
  filter(T_A == 1 | C_A == 1)

model_3.7.3 <- feols(c(unconditional_prcl, conditional) ~ T_A, split = ~ Equity, data = data_3.7.3)

etable(model_3.7.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TA_Affected.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment A in different sub-samples",  
       label = "tab:S1_TA_Affected", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform. The sample is split in various sub-samples.")))

etable(model_3.7.2, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TA_Impact.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment A in different sub-samples",  
       label = "tab:S1_TA_Impact", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform. The sample is split in various sub-samples.")))

etable(model_3.7.3, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TA_Equity.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment A in different sub-samples",  
       label = "tab:S1_TA_Equity", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform. The sample is split in various sub-samples.")))

data_3.7.1.1 <- count(data_3.7.1, Affected, T_A, unconditional_prcl)%>%
  group_by(Affected, T_A)%>%
  mutate(T_A_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_A_sum)

P_3.7.1 <- ggplot(data_3.7.1.1, aes(x = unconditional_prcl, y = share, group = interaction(T_A, unconditional_prcl), fill = factor(T_A)))+
  facet_grid(. ~ Affected)+
  geom_col(colour = "black", width = 0.70, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

data_3.7.2.1 <- count(data_3.7.2, Impact, T_A, unconditional_prcl)%>%
  group_by(Impact, T_A)%>%
  mutate(T_A_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_A_sum)

P_3.7.2 <- ggplot(data_3.7.2.1, aes(x = unconditional_prcl, y = share, group = interaction(T_A, unconditional_prcl), fill = factor(T_A)))+
  facet_grid(. ~ Impact)+
  geom_col(colour = "black", width = 0.75, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática deben ser bajos para mí y mi hogar")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

data_3.7.3.1 <- count(data_3.7.3, Equity, T_A, unconditional_prcl)%>%
  group_by(Equity, T_A)%>%
  mutate(T_A_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_A_sum)

P_3.7.3 <- ggplot(data_3.7.3.1, aes(x = unconditional_prcl, y = share, group = interaction(T_A, unconditional_prcl), fill = factor(T_A)))+
  facet_grid(. ~ Equity)+
  geom_col(colour = "black", width = 0.75, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática deben distribuirse equitativamente entre todos los hogares")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S7_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.7.1)
print(P_3.7.2)
print(P_3.7.3)
dev.off()

rm(data_3.7, data_3.7.1, data_3.7.1.1, data_3.7.2, data_3.7.2.1, data_3.7.3, data_3.7.3.1, model_3.7.1, model_3.7.2, model_3.7.3, P_3.7.1, P_3.7.2, P_3.7.3)

# Treatment effect B over pobre_amnto / cc_imp_equit

data_3.8 <- data_1 %>%
  mutate(pobre_amnto  = factor(pobre_amnto, labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         cc_imp_equit = factor(cc_imp_equit, labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")))

data_3.8.1 <- data_3.8 %>%
  mutate(Poor = ifelse(pobre_amnto == "Nothing" | pobre_amnto == "Very little", "Not Important",
                           ifelse(pobre_amnto == "Somewhat" | pobre_amnto == "Greatly", "Important", NA)))%>%
  filter(!is.na(Poor))%>%
  filter(T_B == 1 | C_B == 1)

model_3.8.1 <- feols(c(unconditional_prcl, conditional) ~ T_B, split = ~ Poor, data = data_3.8.1)

data_3.8.3 <- data_3.8 %>%
  mutate(Equity = ifelse(cc_imp_equit == "Not important" | cc_imp_equit == "Very little important", "Not important",
                         ifelse(cc_imp_equit == "Somewhat important" | cc_imp_equit == "Very important", "Important", NA)))%>%
  filter(!is.na(Equity))%>%
  filter(T_B == 1 | C_B == 1)

model_3.8.3 <- feols(c(unconditional_prcl, conditional) ~ T_B, split = ~ Equity, data = data_3.8.3)

etable(model_3.8.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TB_Poor.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment B in different sub-samples",  
       label = "tab:S1_TB_Poor", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform. The sample is split in various sub-samples.")))

etable(model_3.8.3, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TB_Equity.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment B in different sub-samples",  
       label = "tab:S1_TB_Equity", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform. The sample is split in various sub-samples.")))

data_3.8.1.1 <- count(data_3.8.1, Poor, T_B, unconditional_prcl)%>%
  group_by(Poor, T_B)%>%
  mutate(T_B_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_B_sum)

P_3.8.1 <- ggplot(data_3.8.1.1, aes(x = unconditional_prcl, y = share, group = interaction(T_B, unconditional_prcl), fill = factor(T_B)))+
  facet_grid(. ~ Poor)+
  geom_col(colour = "black", width = 0.70, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos \n por un posible aumento de los precios de los combustibles en el 65 %?")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

data_3.8.3.1 <- count(data_3.8.3, Equity, T_B, unconditional_prcl)%>%
  group_by(Equity, T_B)%>%
  mutate(T_B_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_B_sum)

P_3.8.3 <- ggplot(data_3.8.3.1, aes(x = unconditional_prcl, y = share, group = interaction(T_B, unconditional_prcl), fill = factor(T_B)))+
  facet_grid(. ~ Equity)+
  geom_col(colour = "black", width = 0.75, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática deben distribuirse equitativamente entre todos los hogares")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S8_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.8.1)
print(P_3.8.3)
dev.off()

rm(data_3.8, data_3.8.1, data_3.8.1.1, data_3.8.3, data_3.8.3.1, model_3.8.1, model_3.8.3, P_3.8.1, P_3.8.3)

# Treatment effect C over pais_confianza_prsdnt / pais_dmcrc

data_3.9 <- data_1 %>%
  mutate(pais_confianza_prsdnt = factor(pais_confianza_prsdnt, labels = c("No trust at all", "Little trust", "Moderate trust", "Some trust", "Complete trust")),
         pais_dmcrc            = factor(pais_dmcrc,            labels = c("Very bad", "Bad", "Neutral", "Good", "Very good")))

data_3.9.1 <- data_3.9 %>%
  mutate(President = ifelse(pais_confianza_prsdnt == "No trust at all" | pais_confianza_prsdnt == "Little trust", "No trust",
                       ifelse(pais_confianza_prsdnt == "Some trust" | pais_confianza_prsdnt == "Complete trust", "Trust", NA)))%>%
  filter(!is.na(President))%>%
  filter(T_C == 1 | C_C == 1)

model_3.9.1 <- feols(c(unconditional_prcl, conditional) ~ T_C, split = ~ President, data = data_3.9.1)

data_3.9.3 <- data_3.9 %>%
  mutate(Democracy = ifelse(pais_dmcrc == "Very bad" | pais_dmcrc == "Bad", "Bad",
                         ifelse(pais_dmcrc == "Good" | pais_dmcrc == "Very good", "Good", NA)))%>%
  filter(!is.na(Democracy))%>%
  filter(T_C == 1 | C_C == 1)

model_3.9.3 <- feols(c(unconditional_prcl, conditional) ~ T_C, split = ~ Democracy, data = data_3.9.3)

etable(model_3.9.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TC_President.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment C in different sub-samples",  
       label = "tab:S1_TC_President", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received  additional information about the efficiency of government resource use. The sample is split in various sub-samples.")))

etable(model_3.9.3, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/3_Supplementary/Table_S1_TC_Democracy.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Treatment effects of information treatment C in different sub-samples",  
       label = "tab:S1_TC_Democracy", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received  additional information about the efficiency of government resource use. The sample is split in various sub-samples.")))

data_3.9.1.1 <- count(data_3.9.1, President, T_C, unconditional_prcl)%>%
  group_by(President, T_C)%>%
  mutate(T_C_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_C_sum)

P_3.9.1 <- ggplot(data_3.9.1.1, aes(x = unconditional_prcl, y = share, group = interaction(T_C, unconditional_prcl), fill = factor(T_C)))+
  facet_grid(. ~ President)+
  geom_col(colour = "black", width = 0.70, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Cuanta confianza the president of the republic?")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

data_3.9.3.1 <- count(data_3.9.3, Democracy, T_C, unconditional_prcl)%>%
  group_by(Democracy, T_C)%>%
  mutate(T_C_sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/T_C_sum)

P_3.9.3 <- ggplot(data_3.9.3.1, aes(x = unconditional_prcl, y = share, group = interaction(T_C, unconditional_prcl), fill = factor(T_C)))+
  facet_grid(. ~ Democracy)+
  geom_col(colour = "black", width = 0.75, position = position_dodge(0.8))+
  theme_bw()+
  xlab("Unconditional support")+
  ylab("Survey respondents")+
  ggtitle("¿Cómo calificaría usted el funcionamiento de la democracia en Colombia?")+
  scale_fill_brewer(direction = -1, labels = c("Control group", "Treatment group"))+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,0.5))+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S9_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.9.1)
print(P_3.9.3)
dev.off()

rm(data_3.9, data_3.9.1, data_3.9.1.1, data_3.9.3, data_3.9.3.1, model_3.9.1, model_3.9.3, P_3.9.1, P_3.9.3)

# Revenue recycling options

data_3.10 <- data_1 %>%
  select(derecho, yo_amnto, pobre_amnto, rica_amnto, pais_confianza_prsdnt, carro, prop_acrd_paz,
         prop_acrd_energ, cc_info, cc_preocup, cc_futuro, cc_econ, cc_imp_co2, cc_imp_pers, cc_imp_equit,
         rr_lmpsm:rr_deforst)%>%
  mutate(derecho               = factor(derecho,               labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         yo_amnto              = factor(yo_amnto,              labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         pobre_amnto           = factor(pobre_amnto,           labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         pais_confianza_prsdnt = factor(pais_confianza_prsdnt, labels = c("No trust at all", "Little trust", "Moderate trust", "Some trust", "Complete trust")),
         prop_acrd_paz         = factor(prop_acrd_paz,         labels = c("No", "Yes")),
         prop_acrd_energ       = factor(prop_acrd_energ,       labels = c("No", "Yes")),
         cc_preocup            = factor(cc_preocup,            labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         cc_info               = factor(cc_info,               labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")),
         cc_imp_pers           = factor(cc_imp_pers,           labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")),
         cc_imp_equit          = factor(cc_imp_equit,          labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")),
         cc_imp_co2            = factor(cc_imp_co2,            labels = c("Not important", "Very little important", "Moderately important", "Somewhat important", "Very important")),
         cc_futuro             = factor(cc_futuro,             labels = c("Doesn't know", "Will never be urgent", "Nothing to do", "Urgent in the future", "Very urgent")),
         cc_econ               = factor(cc_econ,               labels = c("Other:", "More priority to the economy", "I don't know", "Equal priority to climate change and economy", "More priority to climate change")))%>%
  mutate_at(vars(starts_with("rr_")), ~ factor(., labels = c("Nothing", "Very little", "Undecided", "Somewhat", "Greatly")))

data_3.10.1 <- count(data_3.10, rr_lmpsm, derecho)
data_3.10.2 <- count(data_3.10, rr_lmpsm, yo_amnto)
data_3.10.3 <- count(data_3.10, rr_lmpsm, pobre_amnto)

data_3.10.4 <- count(data_3.10, rr_pobre, derecho)
data_3.10.5 <- count(data_3.10, rr_pobre, yo_amnto)
data_3.10.6 <- count(data_3.10, rr_pobre, pobre_amnto)

data_3.10.7 <- count(data_3.10, rr_afctds, derecho)
data_3.10.8 <- count(data_3.10, rr_afctds, yo_amnto)
data_3.10.9 <- count(data_3.10, rr_afctds, pobre_amnto)

data_3.10.10 <- count(data_3.10, rr_deuda,   pais_confianza_prsdnt)
data_3.10.11 <- count(data_3.10, rr_paz,     prop_acrd_paz)
data_3.10.12 <- count(data_3.10, rr_etransp, carro)

data_3.10.13 <- count(data_3.10, rr_ncer, prop_acrd_energ)
data_3.10.14 <- count(data_3.10, rr_ncer, cc_info)
data_3.10.15 <- count(data_3.10, rr_ncer, cc_preocup)
data_3.10.16 <- count(data_3.10, rr_ncer, cc_futuro)
data_3.10.17 <- count(data_3.10, rr_ncer, cc_econ)
data_3.10.18 <- count(data_3.10, rr_ncer, cc_imp_co2)
data_3.10.19 <- count(data_3.10, rr_ncer, cc_imp_pers)
data_3.10.20 <- count(data_3.10, rr_ncer, cc_imp_equit)

data_3.10.21 <- count(data_3.10, rr_deforst, prop_acrd_energ)
data_3.10.22 <- count(data_3.10, rr_deforst, cc_info)
data_3.10.23 <- count(data_3.10, rr_deforst, cc_preocup)
data_3.10.24 <- count(data_3.10, rr_deforst, cc_futuro)
data_3.10.25 <- count(data_3.10, rr_deforst, cc_econ)
data_3.10.26 <- count(data_3.10, rr_deforst, cc_imp_co2)
data_3.10.27 <- count(data_3.10, rr_deforst, cc_imp_pers)
data_3.10.28 <- count(data_3.10, rr_deforst, cc_imp_equit)

 
P_3.10.1 <- ggplot(data_3.10.1, aes(x = rr_lmpsm, y = n))+
  facet_grid(. ~ derecho)+
  geom_col(aes(fill = rr_lmpsm), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Lump sum)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.1$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.2 <- ggplot(data_3.10.2, aes(x = rr_lmpsm, y = n))+
  facet_grid(. ~ yo_amnto)+
  geom_col(aes(fill = rr_lmpsm), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Lump sum)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.2$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.3 <- ggplot(data_3.10.3, aes(x = rr_lmpsm, y = n))+
  facet_grid(. ~ pobre_amnto)+
  geom_col(aes(fill = rr_lmpsm), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Lump sum)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos \n por un posible aumento de los precios de los combustibles en el 65%?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.3$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.4 <- ggplot(data_3.10.4, aes(x = rr_pobre, y = n))+
  facet_grid(. ~ derecho)+
  geom_col(aes(fill = rr_pobre), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Poor)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.4$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.5 <- ggplot(data_3.10.5, aes(x = rr_pobre, y = n))+
  facet_grid(. ~ yo_amnto)+
  geom_col(aes(fill = rr_pobre), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Poor)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.5$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.6 <- ggplot(data_3.10.6, aes(x = rr_pobre, y = n))+
  facet_grid(. ~ pobre_amnto)+
  geom_col(aes(fill = rr_pobre), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Poor)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos \n por un posible aumento de los precios de los combustibles en el 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.6$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.7 <- ggplot(data_3.10.7, aes(x = rr_afctds, y = n))+
  facet_grid(. ~ derecho)+
  geom_col(aes(fill = rr_afctds), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Affected)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.7$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.8 <- ggplot(data_3.10.8, aes(x = rr_afctds, y = n))+
  facet_grid(. ~ yo_amnto)+
  geom_col(aes(fill = rr_afctds), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Affected)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.8$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.9 <- ggplot(data_3.10.9, aes(x = rr_afctds, y = n))+
  facet_grid(. ~ pobre_amnto)+
  geom_col(aes(fill = rr_afctds), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Affected)")+
  ylab("Survey respondents")+
  ggtitle("¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos \n por un posible aumento de los precios de los combustibles en el 65 %?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.9$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.10 <- ggplot(data_3.10.10, aes(x = rr_deuda, y = n))+
  facet_grid(. ~ pais_confianza_prsdnt)+
  geom_col(aes(fill = rr_deuda), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Public debt)")+
  ylab("Survey respondents")+
  ggtitle("¿Cuanta confianza the president?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.10$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.11 <- ggplot(data_3.10.11, aes(x = rr_paz, y = n))+
  facet_grid(. ~ prop_acrd_paz)+
  geom_col(aes(fill = rr_paz), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Paz)")+
  ylab("Survey respondents")+
  ggtitle("¿Esta de acuerdo con La Paz Total?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.11$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.12 <- ggplot(data_3.10.12, aes(x = rr_etransp, y = n))+
  facet_grid(. ~ carro)+
  geom_col(aes(fill = rr_etransp), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Transporte eléctrico)")+
  ylab("Survey respondents")+
  ggtitle("¿Usted o alguien en su hogar tiene un carro para uso del hogar?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.12$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.13 <- ggplot(data_3.10.13, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ prop_acrd_energ)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("¿Esta acuerdo con Transicion energetica?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.13$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.14 <- ggplot(data_3.10.14, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_info)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("¿En qué medida se considera usted informado/a sobre el cambio climático?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.14$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.15 <- ggplot(data_3.10.15, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_preocup)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("¿En qué medida le preocupa el cambio climático?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.15$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.16 <- ggplot(data_3.10.16, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_futuro)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("¿Con cuál de las siguientes frases está usted más de acuerdo? ")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.16$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.17 <- ggplot(data_3.10.17, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_econ)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("¿Con cuál de las siguientes frases está usted más de acuerdo?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.17$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.18 <- ggplot(data_3.10.18, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_imp_co2)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("Una política de lucha contra el cambio climático debería principalmente reducir las emisiones de CO2")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.18$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.19 <- ggplot(data_3.10.19, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_imp_pers)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática deben ser bajos para mí y mi hogar")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.19$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.20 <- ggplot(data_3.10.20, aes(x = rr_ncer, y = n))+
  facet_grid(. ~ cc_imp_equit)+
  geom_col(aes(fill = rr_ncer), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Energías renovables)")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática \n deben distribuirse equitativamente entre todos los hogares")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.20$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.21 <- ggplot(data_3.10.21, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ prop_acrd_energ)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("¿Esta acuerdo con Transicion energetica?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.21$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.22 <- ggplot(data_3.10.22, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_info)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("¿En qué medida se considera usted informado/a sobre el cambio climático?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.22$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.23 <- ggplot(data_3.10.23, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_preocup)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("¿En qué medida le preocupa el cambio climático?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.23$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 8),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.24 <- ggplot(data_3.10.24, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_futuro)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("¿Con cuál de las siguientes frases está usted más de acuerdo? ")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.24$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.25 <- ggplot(data_3.10.25, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_econ)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("¿Con cuál de las siguientes frases está usted más de acuerdo?")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.25$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.26 <- ggplot(data_3.10.26, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_imp_co2)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("Una política de lucha contra el cambio climático debería principalmente reducir las emisiones de CO2")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.26$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.27 <- ggplot(data_3.10.27, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_imp_pers)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática deben ser bajos para mí y mi hogar")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.27$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

P_3.10.28 <- ggplot(data_3.10.28, aes(x = rr_deforst, y = n))+
  facet_grid(. ~ cc_imp_equit)+
  geom_col(aes(fill = rr_deforst), colour = "black", width = 0.8)+
  theme_bw()+
  xlab("Conditional support (Deforestación)")+
  ylab("Survey respondents")+
  ggtitle("Los costos financieros de una política climática \n deben distribuirse equitativamente entre todos los hogares")+
  scale_fill_brewer(direction = -1)+
  scale_y_continuous(expand = c(0,0))+
  coord_cartesian(ylim = c(0,max(data_3.10.28$n + 10)))+
  theme(axis.text.x     = element_blank(),
        axis.ticks.x    = element_blank(),
        axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S10_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_3.10.1)
print(P_3.10.2)
print(P_3.10.3)
print(P_3.10.4)
print(P_3.10.5)
print(P_3.10.6)
print(P_3.10.7)
print(P_3.10.8)
print(P_3.10.9)
print(P_3.10.10)
print(P_3.10.11)
print(P_3.10.12)
print(P_3.10.13)
print(P_3.10.14)
print(P_3.10.15)
print(P_3.10.16)
print(P_3.10.17)
print(P_3.10.18)
print(P_3.10.19)
print(P_3.10.20)
print(P_3.10.21)
print(P_3.10.22)
print(P_3.10.23)
print(P_3.10.24)
print(P_3.10.25)
print(P_3.10.26)
print(P_3.10.27)
print(P_3.10.28)
dev.off()

rm(P_3.10.1,P_3.10.2,P_3.10.3,P_3.10.4,P_3.10.5,P_3.10.6,P_3.10.7,P_3.10.8,P_3.10.9,P_3.10.10,P_3.10.11,P_3.10.12,P_3.10.13,P_3.10.14,P_3.10.15,P_3.10.16,P_3.10.17,P_3.10.18,P_3.10.19,P_3.10.20,
   P_3.10.21,P_3.10.22,P_3.10.23,P_3.10.24,P_3.10.25,P_3.10.26,P_3.10.27,P_3.10.28,
   data_3.10.1,data_3.10.2,data_3.10.3,data_3.10.4,data_3.10.5,data_3.10.6,data_3.10.7,data_3.10.8,data_3.10.9,data_3.10.10,data_3.10.11,data_3.10.12,data_3.10.13,data_3.10.14,data_3.10.15,data_3.10.16,data_3.10.17,data_3.10.18,data_3.10.19,data_3.10.20,
   data_3.10.21,data_3.10.22,data_3.10.23,data_3.10.24,data_3.10.25,data_3.10.26,data_3.10.27,data_3.10.28)

data_3.11 <- data_1 %>%
  select(rr_lmpsm:rr_deforst)%>%
  rename("Lump sum transfers" = rr_lmpsm, "Transfers (poor)" = rr_pobre, "Transfers (most affected)" = rr_afctds,
         "Reduce taxes" = rr_impuesto, "Reduce external debt" = rr_deuda, "Electric transport" = rr_etransp, "Infrastructure investment" = rr_paz,
         "Education" = rr_edu, "Renewable energy" = rr_ncer, "Prevent deforestation" = rr_deforst)%>%
  summarise_all(~ mean(.))%>%
  pivot_longer(everything(),names_to = "names", values_to = "values")%>%
  mutate(names = factor(names, levels = c("Lump sum transfers", "Transfers (poor)", "Transfers (most affected)",
                                          "Reduce taxes", "Reduce external debt", "Electric transport", "Infrastructure investment",
                                          "Education", "Renewable energy", "Prevent deforestation")))

P_11 <- ggplot(data_3.11)+
  geom_col(aes(x = values, y = names), fill = "lightgrey", colour = "black")+
  theme_bw()+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))+
  coord_cartesian(xlim = c(1,5.5))+
  ylab("")+
  xlab("Average conditional support for partial fossil fuel subsidy removal")+
  theme(axis.text.y     = element_text(size = 7),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S11.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_11)
dev.off()

# In which direction do undecided non-supporters move with treatment?

data_3.12 <- data_3 %>%
  filter(Support == "Non-supporter")%>%
  filter(unconditional_prcl == 3)%>%
  group_by(treatment, conditional)%>%
  summarise(number = n())%>%
  ungroup()%>%
  group_by(treatment)%>%
  mutate(sum = sum(number))%>%
  ungroup()%>%
  mutate(share = number/sum)

P_12 <- ggplot(data_3.12)+
  geom_col(aes(x = conditional, group = factor(treatment), y = share, fill = factor(treatment)), width = 0.65, position = position_dodge(0.8), colour = "black")+
  theme_bw()+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))+
  #scale_x_discrete(breaks = c(1,2,3,4,5), labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))+
  coord_cartesian(ylim = c(0,0.6))+
  scale_fill_brewer(labels = c("Control", "Treatment"))+
  ggtitle("Non-supporters that show neutral support for unconditional fossil fuel subsidy removal")+
  scale_y_continuous(expand = c(0,0))+
  ylab("Number of respondents")+
  xlab("Average conditional support for partial fossil fuel subsidy removal")+
  theme(axis.text.y     = element_text(size = 7),
        axis.text.x     = element_text(size = 6),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S12.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_12)
dev.off()

data_2.2.1.B.1 <- count(data_2.2.1.B, Median_costs, T_A, rr_afctds)%>%
  group_by(Median_costs, T_A)%>%
  mutate(sum = sum(n))%>%
  ungroup()%>%
  mutate(share = n/sum)%>%
  mutate(Median_costs = ifelse(Median_costs == "Above", "Above median costs", "Below median costs"))

P_13 <- ggplot(data_2.2.1.B.1)+
  facet_grid(. ~ Median_costs)+
  geom_col(aes(x = rr_afctds, group = factor(T_A), y = share, fill = factor(T_A)), width =  0.65, position = position_dodge(0.8), colour = "black")+
  theme_bw()+
  scale_x_continuous(breaks = c(1,2,3,4,5), labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))+
  #scale_x_discrete(breaks = c(1,2,3,4,5), labels = c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree"))+
  coord_cartesian(ylim = c(0,0.4))+
  scale_fill_brewer(labels = c("Control", "Treatment"))+
  #ggtitle("Non-supporters that show neutral support for unconditional fossil fuel subsidy removal")+
  scale_y_continuous(expand = c(0,0))+
  ylab("Share of respondents")+
  xlab("Average conditional support for partial fossil fuel subsidy removal and transfers (most affected)")+
  theme(axis.text.y     = element_text(size = 7),
        axis.text.x     = element_text(size = 6),
        title           = element_text(size = 7),
        strip.text      = element_text(size = 6),
        axis.title      = element_text(size = 8),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_S13.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_13)
dev.off()

# 4. Figures ####

# Tests ####

test <- data_1.1 %>%
  filter(type_support == "conditional")

feols(support ~ T_D, data = data_1.1)
feols(support ~ conditional_support + T_D, data = data_1.1)
feols(support ~ T_A, data = filter(data_1.1, type_support == "conditional"))
feols(support ~ T_B, data = filter(data_1.1, type_support == "conditional"))
feols(support ~ T_C, data = filter(data_1.1, type_support == "conditional"))
feols(support ~ T_D, data = filter(data_1.1, type_support == "conditional"))
feols(support ~ T_D, data = filter(data_1.1, type_support == "conditional"& (treatment == 0 | T_D == 1)))
feols(support ~ T_D, data = filter(data_1.1, type_support == "unconditional_prcl"))

# Support (1-5) over conditional_support and treatment

model_2.1.1 <- feols(support ~ conditional_support + treatment, data = data_1.1)
tidy_2.1.1 <- tidy(model_2.1.1)%>%
  mutate(Type = "OLS")

model_2.1.2 <- feols(support ~ conditional_support + T_A + T_B + T_C + T_D, data = data_1.1)
tidy_2.1.2 <- tidy(model_2.1.2)%>%
  mutate(Type = "OLS")

model_2.1.2.1 <- feols(support ~ T_A, data = data_1.1)
model_2.1.2.2 <- feols(support ~ T_B, data = data_1.1)
model_2.1.2.3 <- feols(support ~ T_C, data = data_1.1)
model_2.1.2.4 <- feols(support ~ T_D, data = data_1.1)

etable(model_2.1.2, model_2.1.2.1, model_2.1.2.2, model_2.1.2.3, model_2.1.2.4, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.tex",
       digits = 3, replace = TRUE, fitstat = c("n", "r2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 1",  
       label = "tab:H2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!")

# Binary variable (0/1)
model_2.2.1 <- feglm(support ~ conditional_support + treatment, family = binomial(link = "logit"), data = data_1.2.1)
model_2.2.2 <- feglm(support ~ conditional_support + T_A + T_B + T_C + T_D, family = binomial(link = "logit"), data = data_1.2.1)
model_2.2.3 <- feglm(support ~ conditional_support + treatment, family = binomial(link = "probit"), data = data_1.2.1)
model_2.2.4 <- feglm(support ~ conditional_support + T_A + T_B + T_C + T_D, family = binomial(link = "probit"), data = data_1.2.1)

tidy_2.2.1 <- tidy(model_2.2.1)%>%
  mutate(Type = "Logit")
tidy_2.2.2 <- tidy(model_2.2.2)%>%
  mutate(Type = "Logit")
tidy_2.2.3 <- tidy(model_2.2.3)%>%
  mutate(Type = "Probit")
tidy_2.2.4 <- tidy(model_2.2.4)%>%
  mutate(Type = "Probit")

# Extreme classification (0/1)
model_2.3.1 <- feglm(support ~ conditional_support + treatment, family = binomial(link = "logit"), data = data_1.3.1)
model_2.3.2 <- feglm(support ~ conditional_support + T_A + T_B + T_C + T_D, family = binomial(link = "logit"), data = data_1.3.1)
model_2.3.3 <- feglm(support ~ conditional_support + treatment, family = binomial(link = "probit"), data = data_1.3.1)
model_2.3.4 <- feglm(support ~ conditional_support + T_A + T_B + T_C + T_D, family = binomial(link = "probit"), data = data_1.3.1)

tidy_2.3.1 <- tidy(model_2.3.1)%>%
  mutate(Type = "Logit")
tidy_2.3.2 <- tidy(model_2.3.2)%>%
  mutate(Type = "Logit")
tidy_2.3.3 <- tidy(model_2.3.3)%>%
  mutate(Type = "Probit")
tidy_2.3.4 <- tidy(model_2.3.4)%>%
  mutate(Type = "Probit")





# Receiving information will increase unconditional and conditional support for FFSR among respondents that do not endorse the incumbent president.
# Compare supporters and non-supporters.
# Compare treatment effects for all information treatments among both groups.

# 2.0 Supplementary Graphics ####

data_0.1 <- data_1 %>%
  mutate(IQ = ifelse(ingrso %in% c(1,2),"IQ1",
                     ifelse(ingrso %in% c(3,4),"IQ2",
                            ifelse(ingrso %in% c(5,6),"IQ3",
                                   ifelse(ingrso %in% c(7,8),"IQ4",
                                          ifelse(ingrso %in% c(9,10),"IQ5",NA))))))%>%
  select(ID, IQ, cc_info, cc_preocup, cc_futuro, cc_econ, ffsr_gnrl, ffsr_dsl, ffsr_gas, gas_super, dsl_super, benefic, conditional_recode_2)%>%
  filter(!is.na(conditional_recode_2))%>%
  mutate(Support = ifelse(conditional_recode_2 == 0, "Disapproves", "Supports"))

data_0.1.1 <- data_0.1 %>%
  group_by(IQ, cc_info)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.2 <- data_0.1 %>%
  group_by(IQ, cc_preocup)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.3 <- data_0.1 %>%
  group_by(IQ, cc_futuro)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.4 <- data_0.1 %>%
  group_by(IQ, cc_econ)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.5 <- data_0.1 %>%
  group_by(IQ, ffsr_gnrl)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.6 <- data_0.1 %>%
  filter(!is.na(ffsr_dsl))%>%
  group_by(IQ, ffsr_dsl)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.7 <- data_0.1 %>%
  filter(!is.na(ffsr_gas))%>%
  group_by(IQ, ffsr_gas)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.8 <- data_0.1 %>%
  
  group_by(IQ, gas_super)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.9 <- data_0.1 %>%
  group_by(IQ, dsl_super)%>%
  summarise(number = n())%>%
  ungroup()

data_0.1.10 <- data_0.1 %>%
  group_by(IQ, benefic)%>%
  summarise(number = n())%>%
  ungroup()

P_0.1 <- ggplot(data_0.1.1)+
  geom_col(aes(x = cc_info, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("cc_info")+
  theme()

P_0.2 <- ggplot(data_0.1.2)+
  geom_col(aes(x = cc_preocup, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("cc_preocup")+
  theme()

P_0.3 <- ggplot(data_0.1.3)+
  geom_col(aes(x = cc_futuro, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("cc_futuro")+
  theme()

P_0.4 <- ggplot(data_0.1.4)+
  geom_col(aes(x = cc_econ, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("cc_econ")+
  theme()

P_0.5 <- ggplot(data_0.1.5)+
  geom_col(aes(x = ffsr_gnrl, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("ffsr_gnrl")+
  theme()

P_0.6 <- ggplot(data_0.1.6)+
  geom_col(aes(x = ffsr_dsl, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("ffsr_dsl")+
  theme()

P_0.7 <- ggplot(data_0.1.7)+
  geom_col(aes(x = ffsr_gas, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("ffsr_gas")+
  theme()

P_0.8 <- ggplot(data_0.1.8)+
  geom_col(aes(x = gas_super, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("gas_super")+
  theme()

P_0.9 <- ggplot(data_0.1.9)+
  geom_col(aes(x = dsl_super, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("dsl_super")+
  theme()

P_0.10 <- ggplot(data_0.1.10)+
  geom_col(aes(x = benefic, y = number), colour = "black", fill = "#0072B5FF", alpha = 0.8)+
  facet_grid(. ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("benefic")+
  theme()

data_0.1.1 <- data_0.1 %>%
  group_by(IQ, Support)%>%
  summarise(mean_cc_info    = mean(cc_info),
            mean_cc_preocup = mean(cc_preocup),
            mean_cc_futuro  = mean(cc_futuro),
            mean_cc_econ    = mean(cc_econ),
            mean_ffsr_gnrl  = mean(ffsr_gnrl),
            mean_ffsr_dsl   = mean(ffsr_dsl),
            mean_ffsr_gas   = mean(ffsr_gas),
            mean_gas_super  = mean(gas_super),
            mean_dsl_super  = mean(dsl_super),
            mean_benefic    = mean(benefic),)%>%
  ungroup()%>%
  filter(!is.na(Support))

P_0.1 <- ggplot(data_0.1.1)+
  geom_col(aes(x = cc_info, y = number))+
  facet_grid(Support ~ IQ)+
  #scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("cc_info")+
  theme()

P_0.2 <- ggplot(data_0.1.2)+
  geom_col(aes(x = Support, y = mean_cc_info, group = Support, fill = Support), colour = "black", alpha = 1)+
  facet_grid(. ~ IQ)+
  theme_bw()+
  xlab("")+
  ylab("mean_cc_info")+
  theme(legend.position = "bottom",
        axis.text.x = element_blank())

jpeg("../Colombia_Survey_Experiment/Paper/Figures/0_Test/Test_Figures_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_0.1)
print(P_0.2)
print(P_0.3)
print(P_0.4)
print(P_0.5)
print(P_0.6)
print(P_0.7)
print(P_0.8)
print(P_0.9)
print(P_0.10)
# print(P_0.2)
dev.off()


