# Author: L. Missbach (leonard.missbach@pik-potsdam.de)

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "ggsci", "fixest", "ggpubr", "haven", "Hmisc", "kableExtra", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled", "tidymodels", "tidytext")

options(scipen=999)

# 0.    Load data ####

data_0 <- read_sav("Outputs/Data_Cleaned.sav")

predictions_EXP <- read_csv("../2_Predictions/Colombia EXP/Predictions_Synthetic_Dataset_expenditures.csv")%>%
  rename(.pred_EXP = .pred)
predictions     <- read_csv("../2_Predictions/Colombia 65/Predictions_Synthetic_Dataset_65.csv")

# 1.    Renaming and cleaning ####
# Basic data transformation
data_1 <- data_0 %>%
  # Basic filtering
  filter(filter_1b == 0 & filter_2b == 0 & filter_2f == 0 & filter_3b == 0 & filter_3f == 0 & filter_3h == 0 & filter_3l == 0 & filter_3n == 0 & filter_3r == 0 & filter_3t == 0 & filter_3x == 0)%>%
  #filter(if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b","filter_3f", "filter_3h", "filter_3l", "filter_3n","filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
  rename(status_quo                        = ffsr_mnt,
         unconditional_prcl                = ffsr_prcl,
         unconditional_complet             = ffsr_complet)

data_1.1.1 <- data_1 %>%
  select(ID, conditional)%>%
  mutate(conditional_support = 1)%>%
  rename(support = conditional)

data_1.1.2 <- data_1 %>%
  select(ID, unconditional_prcl)%>%
  mutate(conditional_support = 0)%>%
  rename(support = unconditional_prcl)

data_1.1.3 <- data_1 %>%
  select(ID, rr_lmpsm:rr_deforst)%>%
  pivot_longer(-ID, names_to = "rr_", values_to = "not_rounded")%>%
  group_by(ID)%>%
  summarise(support = mean(not_rounded))%>%
  ungroup()%>%
  mutate(Type = "Not rounded")%>%
  mutate(conditional_support = 1)
  
data_1.1 <- bind_rows(data_1.1.1, data_1.1.2, data_1.1.3)%>%
  left_join(select(data_1, ID, starts_with("T_"), starts_with("C_"), treatment, Group, starts_with("frst_")))

rm(data_1.1.1, data_1.1.2, data_1.1.3)

# 1.1   Individual cost treatment ####

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

# 2.    Main Analysis ####

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

hypotheses_0 <- data.frame()

# 2.1   H1 ####
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

tidy_1.4 <- tidy(model_1.4)%>%
  mutate(Type = "H1")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_1.4)

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

rm(model_1.1, model_1.2, model_1.3, model_1.4, tidy_1.4)

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

# 2.2   H2 ####

# Additional information on the effects of removing fossil fuel subsidy will increase respondents unconditional and conditional support for FFSR.

data_1.2.1 <- data_1.1 %>%
  # Unconditional support
  filter(conditional_support == 0)%>%
  mutate(First_Stage = ifelse(Group == 1, "No","Yes"))%>%
  mutate(Group = haven::as_factor(Group))%>%
  mutate_at(vars(frst_a:frst_d), ~ haven::as_factor(.))%>%
  mutate(frst_a = fct_na_value_to_level(frst_a, level = "Incorrect"))

model_1.2.1 <- feols(support ~ treatment + Group, data = data_1.2.1)
model_1.2.2 <- feols(support ~ treatment + First_Stage, data = data_1.2.1)

# tidy_1.2.1 <- tidy(model_1.2.1)%>%
#   mutate(Type         = "OLS",
#          type_support = "Unconditional")

tidy_1.2.1 <- tidy(model_1.2.1)%>%
  mutate(Type = "H2",
         Outcome = "Unconditional")%>%
  filter(term == "treatment")

data_1.2.2 <- data_1.1 %>%
  # Conditional support
  filter(conditional_support == 1)%>%
  filter(is.na(Type))%>%
  mutate(First_Stage = ifelse(Group == 1, "No","Yes"))%>%
  mutate(Group = haven::as_factor(Group))%>%
  mutate_at(vars(frst_a:frst_d), ~ haven::as_factor(.))%>%
  mutate(frst_a = fct_na_value_to_level(frst_a, level = "Incorrect"))

model_1.2.3 <- feols(support ~ treatment + Group, data = data_1.2.2)
model_1.2.4 <- feols(support ~ treatment + First_Stage, data = data_1.2.2)
# tidy_1.2.1 <- tidy(model_1.2.2)%>%
#   mutate(Type         = "OLS",
#          type_support = "Conditional")

tidy_1.2.3 <- tidy(model_1.2.3)%>%
  mutate(Type = "H2",
         Outcome = "Conditional")%>%
  filter(term == "treatment")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_1.2.1)%>%
  bind_rows(tidy_1.2.3)

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

# Inclusion in treatment group only those with correct answers in first stage question

data_1.2.3 <- data_1.2.1 %>%
  filter(treatment == 0 | frst_a == "Correct" | frst_b == "Correct" | frst_c == "Correct" | frst_d == "Correct")

model_1.2.5 <- feols(support ~ treatment + Group, data = data_1.2.3)
model_1.2.6 <- feols(support ~ treatment + First_Stage, data = data_1.2.3)

data_1.2.4 <- data_1.2.2 %>%
  filter(treatment == 0 | frst_a == "Correct" | frst_b == "Correct" | frst_c == "Correct" | frst_d == "Correct")

model_1.2.7 <- feols(support ~ treatment + Group, data = data_1.2.4)
model_1.2.8 <- feols(support ~ treatment + First_Stage, data = data_1.2.4)

etable(model_1.2.5, model_1.2.6, model_1.2.7, model_1.2.8, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2_Alternative.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2: Unconditional and conditional support for a partial fossil fuel subsidy reform among those with correct first-stage question",  
       label = "tab:H2_Alt", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", headers = c("Unconditional", "Unconditional", "Conditional", "Conditional"), order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression REF on the unconditional and conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the fossil fuel subsidy reform. The dependent variable expresses support on a five-point Lickert-scale. For conditional support, this variable is the rounded average support over ten different options for partial fossil fuel subsidy reform and one form of compensation. The treatment group is restricted to respondents that give the correct answer to the first-stage question, which proxies their understanding of the information provision."))
)

rm(data_1.2.1, data_1.2.2, model_1.2.1, model_1.2.2, data_1.1, tidy_1.2.1, tidy_1.2.3, model_1.2.4, model_1.2.5, model_1.2.6, model_1.2.7, model_1.2.8)

# Prep for Sub-Hypotheses

data_2 <- data_1 %>%
  select(ID, starts_with("T_"), starts_with("C_"), treatment, unconditional_prcl,
         rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda, rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst,
         frst_a, frst_b, frst_c, frst_d,
         grp_ingrso, Group, yo_amnto, pobre_amnto)%>%
  # mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
  #                       ifelse(T_B == 1 | C_B == 1, "B",
  #                              ifelse(T_C == 1 | C_C == 1, "C",
  #                                     ifelse(T_D == 1 | C_D == 1, "D", "Control")))))
  mutate(Group = haven::as_factor(Group))

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

model_2.2.1.3 <- feols(rr_pobre ~ T_A, data = data_2.2.1)
model_2.2.1.4 <- feols(rr_afctds ~ T_A, data = data_2.2.1)

tidy_2.2.1.3 <- tidy(model_2.2.1.3)%>%
  filter(term == "T_A")%>%
  mutate(Type    = "H2.1",
         Outcome = "rr_pobre")

tidy_2.2.1.4 <- tidy(model_2.2.1.4)%>%
  filter(term == "T_A")%>%
  mutate(Type    = "H2.1",
         Outcome = "rr_afctds")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_2.2.1.3)%>%
  bind_rows(tidy_2.2.1.4)

model_2.2.1.3 <- feols(unconditional_prcl ~ T_A,  data = data_2.2.1)


rm(data_2.2.1, model_2.2.1.1, model_2.2.1.2, model_2.2.1.3, model_2.2.1.4, tidy_2.2.1.3, tidy_2.2.1.4)

# Including first stage question

data_2.2.1.A <- data_2.2.1 %>%
  # Exclude wrong treatment group participants
  filter(treatment == 0 | frst_a == 1)

model_2.2.1.1.A <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_A,  data = data_2.2.1.A)
model_2.2.1.2.A <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_A,  data = data_2.2.1.A)

etable(model_2.2.1.1.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.1.1_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.1: Conditional support for a partial fossil fuel subsidy reform (Treatment A - part I)",  
       label = "tab:H2.1.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform.")))

etable(model_2.2.1.2.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.1.2_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.1: Conditional support for a partial fossil fuel subsidy reform (Treatment A - part II)",  
       label = "tab:H2.1.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about respondent-specific personal financial effects from a partial fossil fuel subsidy reform.")))

rm(data_2.2.1.A, model_2.2.1.1.A, model_2.2.1.2.A)

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

# Including splitting by being poorer and richer.

data_2.2.1.C <- data_2.2.1 %>%
  mutate(Group = ifelse(grp_ingrso %in% c(1,2), "Low and lower-middle",
                        ifelse(grp_ingrso %in% c(3), "Middle",
                               ifelse(grp_ingrso %in% c(4,5), "Upper-middle and high", NA))))

model_2.2.1.C <- feols(c(rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_A, fsplit = ~ Group, data = data_2.2.1.C)

# People that are more concerned about price increase

data_2.2.1.D <- data_2.2.1 %>%
  filter(yo_amnto > 3)
  
model_2.2.1.D <- feols(c(rr_lmpsm, rr_pobre, rr_afctds) ~ T_A,  data = data_2.2.1.D)

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

model_2.2.2.3 <- feols(rr_lmpsm ~ T_B, data = data_2.2.2)
model_2.2.2.4 <- feols(rr_afctds ~ T_B, data = data_2.2.2)

tidy_2.2.2.3 <- tidy(model_2.2.2.3)%>%
  filter(term == "T_B")%>%
  mutate(Type = "H2.2",
         Outcome = "rr_lmpsm")

tidy_2.2.2.4 <- tidy(model_2.2.2.4)%>%
  filter(term == "T_B")%>%
  mutate(Type = "H2.2",
         Outcome = "rr_afctds")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_2.2.2.3)%>%
  bind_rows(tidy_2.2.2.4)

rm(data_2.2.2, model_2.2.2.1, model_2.2.2.2, model_2.2.2.3, model_2.2.2.4, tidy_2.2.2.3, tidy_2.2.2.4)

# Including attention check

data_2.2.2.A <- data_2.2.2 %>%
  # Exclude wrong treatment group participants
  filter(treatment == 0 | frst_b %in% c(3,4,5))

model_2.2.2.1.A <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_B,  data = data_2.2.2.A)
model_2.2.2.2.A <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_B,  data = data_2.2.2.A)

etable(model_2.2.2.1.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.2.1_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.2: Conditional support for a partial fossil fuel subsidy reform (Treatment B - part I)",  
       label = "tab:H2.2.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform.")))

etable(model_2.2.2.2.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.2.2_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.2: Conditional support for a partial fossil fuel subsidy reform (Treatment B - part II)",  
       label = "tab:H2.2.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the distributional effects from a partial fossil fuel subsidy reform.")))

# Including splitted by being more concerned about hurting the poor

data_2.2.2.B <- data_2.2.2 %>%
  filter(pobre_amnto > 3)

model_2.2.2.1.B <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_B,  data = data_2.2.2.B)
model_2.2.2.2.B <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_B,  data = data_2.2.2.B)
model_2.2.2.3.B <- feols(unconditional_prcl ~ T_B,  data = data_2.2.2.B)

# Including splitted by perceived socioeconomic status
data_2.2.2.C <- data_2.2.2 %>%
  mutate(Group = ifelse(grp_ingrso %in% c(1,2), "Low and lower-middle",
                      ifelse(grp_ingrso %in% c(3), "Middle",
                             ifelse(grp_ingrso %in% c(4,5), "Upper-middle and high", NA))))

model_2.2.2.1.C <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_B, fsplit = ~ Group, data = data_2.2.2.C)

rm(data_2.2.2.A, model_2.2.2.1.A, model_2.2.2.2.A)

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

model_2.2.3.3 <- feols(rr_impuesto ~ T_C, data = data_2.2.3)
model_2.2.3.4 <- feols(rr_deuda    ~ T_C, data = data_2.2.3)
model_2.2.3.5 <- feols(rr_etransp  ~ T_C, data = data_2.2.3)
model_2.2.3.6 <- feols(rr_paz      ~ T_C, data = data_2.2.3)
model_2.2.3.7 <- feols(rr_edu      ~ T_C, data = data_2.2.3)

tidy_2.2.3.3 <- tidy(model_2.2.3.3)%>%
  filter(term == "T_C")%>%
  mutate(Type    = "H2.3",
         Outcome = "rr_impuesto")

tidy_2.2.3.4 <- tidy(model_2.2.3.4)%>%
  filter(term == "T_C")%>%
  mutate(Type    = "H2.3",
         Outcome = "rr_deuda")

tidy_2.2.3.5 <- tidy(model_2.2.3.5)%>%
  filter(term == "T_C")%>%
  mutate(Type    = "H2.3",
         Outcome = "rr_etransp")

tidy_2.2.3.6 <- tidy(model_2.2.3.6)%>%
  filter(term == "T_C")%>%
  mutate(Type    = "H2.3",
         Outcome = "rr_paz")

tidy_2.2.3.7 <- tidy(model_2.2.3.7)%>%
  filter(term == "T_C")%>%
  mutate(Type    = "H2.3",
         Outcome = "rr_edu")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_2.2.3.3, tidy_2.2.3.4, tidy_2.2.3.5, tidy_2.2.3.6, tidy_2.2.3.7)

rm(data_2.2.3, model_2.2.3.1, model_2.2.3.2, model_2.2.3.3, model_2.2.3.4, model_2.2.3.5, model_2.2.3.6, model_2.2.3.7, tidy_2.2.3.3, tidy_2.2.3.4, tidy_2.2.3.5, tidy_2.2.3.6, tidy_2.2.3.7)

# Including attention check

data_2.2.3.A <- data_2.2.3 %>%
  filter(treatment == 0 | frst_c == 1)

model_2.2.3.1.A <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_C,  data = data_2.2.3.A)
model_2.2.3.2.A <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_C,  data = data_2.2.3.A)

etable(model_2.2.3.1.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.3.1_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.3: Conditional support for a partial fossil fuel subsidy reform (Treatment C - part I)",  
       label = "tab:H2.3.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the efficiency of government resource use.")))

etable(model_2.2.3.2.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.3.2_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.3: Conditional support for a partial fossil fuel subsidy reform (Treatment C - part II)",  
       label = "tab:H2.3.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the efficiency of government resource use.")))

rm(data_2.2.3.A, model_2.2.3.1.A, model_2.2.3.2.A)

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

model_2.2.4.3 <- feols(rr_etransp ~ T_D, data = data_2.2.4)
model_2.2.4.4 <- feols(rr_paz     ~ T_D, data = data_2.2.4)
model_2.2.4.5 <- feols(rr_edu     ~ T_D, data = data_2.2.4)
model_2.2.4.6 <- feols(rr_ncer    ~ T_D, data = data_2.2.4)
model_2.2.4.7 <- feols(rr_deforst ~ T_D, data = data_2.2.4)

tidy_2.2.4.3 <- tidy(model_2.2.4.3)%>%
  filter(term == "T_D")%>%
  mutate(Type    = "H2.4",
         Outcome = "rr_etransp")

tidy_2.2.4.4 <- tidy(model_2.2.4.4)%>%
  filter(term == "T_D")%>%
  mutate(Type    = "H2.4",
         Outcome = "rr_paz")

tidy_2.2.4.5 <- tidy(model_2.2.4.5)%>%
  filter(term == "T_D")%>%
  mutate(Type    = "H2.4",
         Outcome = "rr_edu")

tidy_2.2.4.6 <- tidy(model_2.2.4.6)%>%
  filter(term == "T_D")%>%
  mutate(Type    = "H2.4",
         Outcome = "rr_ncer")

tidy_2.2.4.7 <- tidy(model_2.2.4.7)%>%
  filter(term == "T_D")%>%
  mutate(Type    = "H2.4",
         Outcome = "rr_deforst")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_2.2.4.3, tidy_2.2.4.4, tidy_2.2.4.5, tidy_2.2.4.6, tidy_2.2.4.7)

rm(data_2.2.4, model_2.2.4.1, model_2.2.4.2, model_2.2.4.3, model_2.2.4.4, model_2.2.4.5, model_2.2.4.6, model_2.2.4.7, tidy_2.2.4.3, tidy_2.2.4.4, tidy_2.2.4.5, tidy_2.2.4.6, tidy_2.2.4.7)

data_2.2.4.A <- data_2.2.4 %>%
  filter(treatment == 0 | frst_d %in% c(3,4,5))

model_2.2.4.1.A <- feols(c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda) ~ T_D,  data = data_2.2.4.A)
model_2.2.4.2.A <- feols(c(rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst) ~ T_D,  data = data_2.2.4.A)

etable(model_2.2.4.1.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.4.1_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.4: Conditional support for a partial fossil fuel subsidy reform (Treatment D - part I)",  
       label = "tab:H2.4.1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the environmental consequences of fossil fuel subsidies.")))

etable(model_2.2.4.2.A, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H2.4.2_Attention.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 2.4: Conditional support for a partial fossil fuel subsidy reform (Treatment D - part II)",  
       label = "tab:H2.4.2", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about the environmental consequences of fossil fuel subsidies.")))

rm(data_2.2.4.A, model_2.2.4.1.A, model_2.2.4.2.A)

# 2.3   H3 ####

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
model_3.3 <- feols(c(unconditional_prcl, conditional) ~ treatment + i(Group, ref = "Control"), split = ~Support, data = filter(data_3, !is.na(Support)))

etable(model_3.3, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H3.tex", fitstat = c("n", "r2"),
       digits = 3, digits.stats = 2, replace = TRUE,  style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 3: Unconditional and conditional support for a partial fossil fuel subsidy reform among supporters and non-supporters of the government",  
       label = "tab:H3", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!", order = c("(Intercept)","Treatment","Group", "Control"),
       notes = c("\\medskip \\textit{Note:}",
                 paste0("This table displays results from an OLS regression on the unconditional and conditional support for a partial fossil fuel subsidy reform over whether respondents received additional information about fossil fuel subsidies, differentiated by whether respondets support the government or not.")))

model_3.3.1 <- feols(unconditional_prcl ~ treatment + Group, data = filter(data_3, Support == "Non-supporter"))
model_3.3.2 <- feols(conditional ~ treatment + Group, data = filter(data_3, Support == "Non-supporter"))

tidy_3.3.1 <- tidy(model_3.3.1)%>%
  filter(term == "treatment")%>%
  mutate(Type    = "H3",
         Outcome = "Unconditional")

tidy_3.3.2 <- tidy(model_3.3.2)%>%
  filter(term == "treatment")%>%
  mutate(Type    = "H3",
         Outcome = "Conditional")

hypotheses_0 <- hypotheses_0 %>%
  bind_rows(tidy_3.3.1, tidy_3.3.2)

rm(model_3.3, model_3.3.1, model_3.3.2, tidy_3.3.1, tidy_3.3.2)

# 3.    Supplementary Analyses ####

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

# 4.    Figures ####
# 4.1   Descriptive statistics - part I ####

# Questions on perception of FFSR and unconditional removal
# Questions on climate policy (cc_imp_co2, cc_imp_pers, cc_imp_equit)

# Over left and right

data_4.1 <- data_1 %>%
  select(unconditional_prcl, conditional, cc_imp_co2, cc_imp_pers, cc_imp_equit, derecho, yo_amnto, pobre_amnto, rica_amnto, grp_ingrso, izq_der,
         rr_lmpsm:rr_deforst)%>%
  mutate(Position = ifelse(izq_der < 3, "Left", 
                           ifelse(izq_der > 3, "Right",
                                  ifelse(izq_der == 3, "Centrist", izq_der))))%>%
  mutate(Income = ifelse(grp_ingrso < 3, "Lower",
                         ifelse(grp_ingrso == 3, "Middle",
                                ifelse(grp_ingrso > 3, "Higher", NA))))%>%
  select(-grp_ingrso,-izq_der)%>%
  mutate_at(vars(starts_with("cc_")), ~ ifelse(. > 3, "Important", "Not important"))%>%
  mutate_at(vars(derecho:rica_amnto), ~ ifelse(. > 3, "Important", "Not important"))%>%
  mutate(unconditional_prcl = ifelse(unconditional_prcl > 3, "Agree", "Not agree"))%>%
  mutate(conditional        = ifelse(conditional > 3, "Agree", "Not agree"))%>%
  mutate_at(vars(starts_with("rr_")), ~ ifelse(. > 3, "Agree", "Not agree"))

# Over all respondents

data_4.1.1 <- data_4.1 %>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ifelse(. %in% c("Important", "Agree"),1,0))%>%
  summarise_at(vars(unconditional_prcl:rr_deforst), ~ sum(.))%>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ./nrow(data_4.1))%>%
  mutate(Type = "Overall")
  
# Over all income groups

data_4.1.2 <- data_4.1 %>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ifelse(. %in% c("Important", "Agree"),1,0))%>%
  group_by(Income)%>%
  summarise_at(vars(unconditional_prcl:rr_deforst), ~ sum(.))%>%
  ungroup()%>%
  left_join(summarise(group_by(data_4.1, Income),number = n()))%>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ./number)%>%
  select(-number)%>%
  mutate(Type = "Income")%>%
  rename(Type_1 = Income)

# Over left and right

data_4.1.3 <- data_4.1 %>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ifelse(. %in% c("Important", "Agree"),1,0))%>%
  filter(!is.na(Position))%>%
  group_by(Position)%>%
  summarise_at(vars(unconditional_prcl:rr_deforst), ~ sum(.))%>%
  ungroup()%>%
  left_join(summarise(group_by(filter(data_4.1, !is.na(Position)), Position),number = n()))%>%
  mutate_at(vars(unconditional_prcl:rr_deforst), ~ ./number)%>%
  select(-number)%>%
  mutate(Type = "Position")%>%
  rename(Type_1 = Position)

data_4.1.4 <- bind_rows(data_4.1.1, data_4.1.2, data_4.1.3)%>%
  pivot_longer(unconditional_prcl:rr_deforst, names_to = "variable", values_to = "values")%>%
  mutate(Type_1 = ifelse(is.na(Type_1),"",Type_1))%>%
  mutate(Type = factor(Type, levels = c("Overall", "Income", "Position")),
         Type_1 = factor(Type_1, levels = c("","Lower", "Middle", "Higher", "Left", "Centrist", "Right")))%>%
  mutate(Variable = case_when(variable == "yo_amnto"     ~ "Concern about personal impact from 65% fuel price hike",
                             variable == "rica_amnto"   ~ "Concern about high-income HH impact from 65% fuel price hike",
                             variable == "pobre_amnto"  ~ "Concern about low-income HH impact from 65% fuel price hike",
                             variable == "derecho"      ~ "Right to benefit from fuel subsidies",
                             variable == "cc_imp_pers"  ~ "Climate policy costs should be low for me and my HH.",
                             variable == "cc_imp_equit" ~ "Climate policy costs should be equally distributed across HH.",
                             variable == "cc_imp_co2"   ~ "Climate policy should primarily reduce CO2.",
                             variable == "unconditional_prcl" ~ "Unconditional support: FFSR despite 65% price increase",
                             variable == "conditional" ~ "Conditional support: FFSR despite 65% prince increase",
                             variable == "rr_lmpsm"    ~ "Equal cash transfers to all HH",
                             variable == "rr_pobre"    ~ "Cash transfers to poor HH",
                             variable == "rr_afctds"   ~ "Cash transfers to most affected HH",
                             variable == "rr_impuesto" ~ "Reduce public debt",
                             variable == "rr_deuda"    ~ "Reduce personal income taxes",
                             variable == "rr_etransp"  ~ "Invest in electric transport (e-buses or e-cars)",
                             variable == "rr_paz"      ~ "Improve rural roads",
                             variable == "rr_edu"      ~ "Improve education",
                             variable == "rr_ncer"     ~ "Invest in clean energy",
                             variable == "rr_deforst"  ~ "Protect the Amazon rainforest",))%>%
  mutate(Variable = factor(Variable, levels = c("Concern about high-income HH impact from 65% fuel price hike",
                                                "Concern about low-income HH impact from 65% fuel price hike",
                                                "Concern about personal impact from 65% fuel price hike",
                                                "Right to benefit from fuel subsidies",
                                                "Climate policy costs should be equally distributed across HH.",
                                                "Climate policy costs should be low for me and my HH.",
                                                "Climate policy should primarily reduce CO2.",
                                                "Conditional support: FFSR despite 65% prince increase",
                                                "Unconditional support: FFSR despite 65% price increase",
                                                "Protect the Amazon rainforest",
                                                "Invest in clean energy",
                                                "Improve education",
                                                "Improve rural roads",
                                                "Invest in electric transport (e-buses or e-cars)",
                                                "Reduce personal income taxes",
                                                "Reduce public debt",
                                                "Cash transfers to most affected HH",
                                                "Cash transfers to poor HH",
                                                "Equal cash transfers to all HH")))%>%

  
  # mutate(Variable = case_when(variable == "yo_amnto"     ~ "¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?",
  #                             variable == "rica_amnto"   ~ "¿Qué tanto le preocupan los efectos causados a las personas de altos ingresos por un posible aumento de los precios de los combustibles en el 65 %?",
  #                             variable == "pobre_amnto"  ~ "¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos por un posible aumento de los precios de los combustibles en el 65 %?",
  #                             variable == "derecho"      ~ "¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?",
  #                             variable == "cc_imp_pers"  ~ "Los costos financieros de una política climática deben ser bajos para mí y mi hogar.",
  #                             variable == "cc_imp_equit" ~ "Los costos financieros de una política climática deben distribuirse equitativamente entre todos los hogares.",
  #                             variable == "cc_imp_co2"   ~ "Una política de lucha contra el cambio climático debería principalmente reducir las emisiones de CO2.",
  #                             variable == "unconditional_prcl" ~ "El gobierno de Colombia debe REDUCIR PARCIALMENTE los subsidios a los combustibles (gasolina y diésel/ACPM) a pesar de que reducir los subsidios haga subir los precios de estos en un 65 %?"))%>%
  # mutate(Variable = factor(Variable, levels = c("¿Qué tanto considera que le afectaría un aumento de los precios de los combustibles de un 65 %?",
  #                                               "¿Qué tanto considera que su hogar tiene derecho a beneficiarse de los subsidios a los combustibles?",
  #                                               "¿Qué tanto le preocupan los efectos causados a las personas de bajos ingresos por un posible aumento de los precios de los combustibles en el 65 %?",
  #                                               "¿Qué tanto le preocupan los efectos causados a las personas de altos ingresos por un posible aumento de los precios de los combustibles en el 65 %?",
  #                                               "Una política de lucha contra el cambio climático debería principalmente reducir las emisiones de CO2.",
  #                                               "Los costos financieros de una política climática deben ser bajos para mí y mi hogar.",
  #                                               "Los costos financieros de una política climática deben distribuirse equitativamente entre todos los hogares.",
  #                                               "El gobierno de Colombia debe REDUCIR PARCIALMENTE los subsidios a los combustibles (gasolina y diésel/ACPM) a pesar de que reducir los subsidios haga subir los precios de estos en un 65 %?")))%>%
  mutate(label = paste0(round(values*100,0),"%"))%>%
  mutate(Type_2 = ifelse(Variable == "Unconditional support: FFSR despite 65% price increase" | Variable == "Conditional support: FFSR despite 65% prince increase", "",
                         ifelse(Variable %in% c("Climate policy costs should be low for me and my HH.",
                                                "Climate policy costs should be equally distributed across HH.",
                                                "Climate policy should primarily reduce CO2."), "Climate policy", 
                                ifelse(Variable %in% c("Protect the Amazon rainforest",
                                                       "Invest in clean energy",
                                                       "Improve education",
                                                       "Improve rural roads",
                                                       "Invest in electric transport (e-buses or e-cars)",
                                                       "Reduce personal income taxes",
                                                       "Reduce public debt",
                                                       "Cash transfers to most affected HH",
                                                       "Cash transfers to poor HH",
                                                       "Equal cash transfers to all HH",
                                                       "Conditional support: FFSR despite 65% prince increase"), "Conditional support", "Fossil fuel subsidy"))))%>%
  mutate(Type_2 = factor(Type_2, levels = c("Climate policy", "Fossil fuel subsidy", "", "Conditional support")))

levels(data_4.1.4$Variable) <- str_wrap(levels(data_4.1.4$Variable), width = 40)

P_4.1 <- ggplot(filter(data_4.1.4, Type_2 != "Conditional support"))+
  geom_point(aes(x = Type_1, y = Variable, fill = values), shape = 22, size = 10)+
  geom_text(aes(x = Type_1, y = Variable, label = label), size = 2.8)+
  facet_grid(Type_2 ~ Type, scales = "free", space = "free", switch = "y")+
  # scale_y_discrete(limits = rev(levels(data_4.1.4$Variable)))+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  scale_fill_distiller(limits = c(0.2,1), palette = "RdYlGn", direction = 1, guide = "none")+
  theme(strip.placement = "outside",
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 6),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

P_4.2 <- ggplot(filter(data_4.1.4, Type_2 == "Conditional support"))+
  geom_point(aes(x = Type_1, y = Variable, fill = values), shape = 22, size = 10)+
  geom_text(aes(x = Type_1, y = Variable, label = label), size = 2.8)+
  facet_grid(Type_2 ~ Type, scales = "free", space = "free", switch = "y")+
  # scale_y_discrete(limits = rev(levels(data_4.1.4$Variable)))+
  scale_x_discrete(position = "top")+
  theme_minimal()+
  scale_fill_distiller(limits = c(0.2,1), palette = "RdYlGn", direction = 1, guide = "none")+
  theme(strip.placement = "outside",
        axis.title = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(size = 6),
        panel.grid.major = element_blank(),
        panel.border = element_rect(color = "black", fill = NA))

jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A1_%d.jpg", width = 13, height = 10, unit = "cm", res = 600)
print(P_4.1)
print(P_4.2)
dev.off()

rm(data_4.1, data_4.1.1, data_4.1.2, data_4.1.3, data_4.1.4, P_4.1)

# 4.2   Hypothesis testing ####

hypotheses_1 <- hypotheses_0 %>%
  mutate(conf_low  = estimate - std.error*1.96,
         conf_high = estimate + std.error*1.96)%>%
  mutate(Outcome = ifelse(is.na(Outcome), "Compensatory policy", Outcome))%>%
  mutate(Outcome = ifelse(Outcome == "Conditional", "Conditional support",
                          ifelse(Outcome == "Unconditional", "Unconditional support", Outcome)))%>%
  mutate(Outcome = case_when(Outcome == "rr_lmpsm"    ~ "Uniform transfer",
                             Outcome == "rr_pobre"    ~ "Targeted transfer (poor)",
                             Outcome == "rr_afctds"   ~ "Targeted transfer (affected)",
                             Outcome == "rr_impuesto" ~ "Tax reduction",
                             Outcome == "rr_deuda"    ~ "Reduce public debt",
                             Outcome == "rr_etransp"  ~ "Electric transport",
                             Outcome == "rr_paz"      ~ "Road infrastructure",
                             Outcome == "rr_edu"      ~ "Public education",
                             Outcome == "rr_ncer"     ~ "Renewable energy",
                             Outcome == "rr_deforst"  ~ "Reduce deforestation",
                             .default = Outcome))%>%
  mutate(Outcome = factor(Outcome, levels = c("Reduce deforestation","Renewable energy","Electric transport", "Public education","Road infrastructure",  "Reduce public debt",
                                              "Tax reduction","Targeted transfer (affected)", "Targeted transfer (poor)", "Uniform transfer", "Conditional support", "Unconditional support", "Compensatory policy")))%>%
  mutate(Type = ifelse(Type == "H2.1", "A",
                       ifelse(Type == "H2.2", "B",
                              ifelse(Type == "H2.3", "C",
                                     ifelse(Type == "H2.4", "D", Type)))))%>%
  mutate(Type = factor(Type, levels = c("H1", "H2", "A", "B", "C", "D", "H3")))%>%
  mutate(Type_2 = ifelse(Outcome %in% c("Targeted transfer (affected)", "Targeted transfer (poor)", "Uniform transfer", "Tax reduction"), "Social protection",
                         ifelse(Outcome %in% c("Reduce public debt", "Road infrastructure", "Public education"), "Public goods",
                                ifelse(Outcome %in% c("Electric transport", "Renewable energy", "Reduce deforestation"), "Green spending", "Rest"))))%>%
  mutate(Type_2 = factor(Type_2, levels = c("Social protection", "Public goods", "Green spending", "Rest")))

P_4.2 <- ggplot(hypotheses_1)+
  geom_vline(aes(xintercept = 0))+
  facet_grid(Type ~ ., scales = "free_y", space = "free_y", switch = "y")+
  geom_errorbar(aes(xmin = conf_low, xmax = conf_high, y = Outcome), width = 0.5)+
  geom_point(aes(x = estimate, y = Outcome, fill = Type_2), shape = 22, size = 4, colour = "black", stroke = 0.5)+
  theme_bw()+
  xlab("Estimate")+
  scale_fill_manual(breaks = c("Social protection", "Public goods", "Green spending"), name = "Category",
                    values = c("#BC3C29FF", "#E18727FF", "#0072B5FF", "grey"))+
  theme(strip.placement = "outside",
        axis.title.y = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(size = 8),
        axis.text.x = element_text(size = 9),
        panel.border = element_rect(color = "black", fill = NA),
        legend.position = "bottom")

jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A2.jpg", width = 15.5, height = 20, unit = "cm", res = 600)
print(P_4.2)
dev.off()

# 5.    Descriptive statistics / ML-support ####

# Variation in which variables drive variation in outcomes?

data_5 <- data_1 %>%
  select(-ID, -date, -starts_with("filter"), -attn1, -attn2, -carro_combus_other_ans, -precio_combus)%>%
  mutate(prop_acrd_labrl = ifelse(is.na(prop_entd_labrl),2,
                                  ifelse(is.na(prop_acrd_labrl),3,prop_acrd_labrl)),
         prop_acrd_pensl = ifelse(is.na(prop_entd_pensl),2,
                                  ifelse(is.na(prop_acrd_pensl),3,prop_acrd_pensl)),
         prop_acrd_fepc = ifelse(is.na(prop_entd_fepc),2,
                                 ifelse(is.na(prop_acrd_fepc),3,prop_acrd_fepc)),
         prop_acrd_salud = ifelse(is.na(prop_entd_salud),2,
                                  ifelse(is.na(prop_acrd_salud),3,prop_acrd_salud)),
         prop_acrd_paz = ifelse(is.na(prop_entd_paz),2,
                                ifelse(is.na(prop_acrd_paz),3,prop_acrd_paz)),
         prop_acrd_energ = ifelse(is.na(prop_entd_energ),2,
                                  ifelse(is.na(prop_acrd_energ),3,prop_acrd_energ)),
         prop_acrd_ning = ifelse(is.na(prop_entd_ning),2,
                                 ifelse(is.na(prop_acrd_ning),3,prop_acrd_ning)))%>%
  select(-starts_with("prop_eschr"), -starts_with("prop_entd"), -starts_with("protestas_lid"))%>%
  select(edad:frst_d, pais_inst:pais_indv, everything())%>%
  # Convert to factors
  mutate_at(vars(gnro:pais_indv, sesgo:Group), ~ haven::as_factor(.))%>%
  # Get rid of NAs
  mutate(moto_dias  = replace(moto_dias, is.na(moto_dias), "No days"),
         carro_dias = replace(carro_dias, is.na(carro_dias), "No days"))%>%
  mutate_at(vars(starts_with("carro_combus_")), ~ replace(., is.na(.), "No"))%>%
  mutate_at(vars(transpub_dias, taxi_dias, bici_dias), ~ replace(., is.na(.), "Never"))%>%
  mutate_at(vars(starts_with("prop_acrd_")), ~ factor(., labels = c("No", "Yes", "Not heard", "Not understood")))%>%
  mutate(ffsr_dsl = ifelse(is.na(ffsr_dsl), ffsr_gnrl, ffsr_dsl),
         ffsr_gas = ifelse(is.na(ffsr_gas), ffsr_gnrl, ffsr_gas))%>%
  mutate_at(vars(ffsr_dsl, ffsr_gas), ~ factor(., labels = c("Yes", "I don't know", "No")))%>%
  mutate(ffsr_gnrl = haven::as_factor(ffsr_gnrl))%>%
  mutate(frst_a = fct_na_value_to_level(frst_a, level = "Incorrect"))

# 5.1   Support for unconditional FFSR ####

data_5.1 <- data_5 %>%
  select(-starts_with("rr_"),-mncp, -unconditional_complet, -status_quo, -conditional)%>%
  mutate(unconditional_prcl = haven::as_factor(unconditional_prcl))%>%
  # Based on first examination
  select(-bici_dias, -starts_with("carro_combus"), -ccnr, -Group, -grp_ingrso, -protestas, -taxi_dias, -urban, - bici, -carro, -cc_econ, -cc_futuro, -gas_super, -moto,
         -pais_congrs, -pais_parties, -prop_acrd_ning, -taxi, -transpub, -treatment)

data_5.1 <- data_5.1 %>%
  # Create noise parameter
  mutate(noise = rnorm(nrow(.),0,1))

data_5.2 <- data_5.1 %>%
  initial_split(prop = 0.8)

# Data for training
data_5.2.train <- data_5.2 %>%
  training()

# Data for testing
data_5.2.test <- data_5.2 %>%
  testing()

rm(data_5.1, data_5.2)

recipe_0 <- recipe(unconditional_prcl ~ .,
                   data = data_5.2.train)%>%
  # Deletes all columns with any NA
  step_filter_missing(all_predictors(), threshold = 0)%>%
  # Remove minimum number of columns such that correlations are less than 0.9
  step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
  # should have very few unique observations for factors
  # step_other(all_nominal(), -edad, -grp_ingrso, threshold = 0.03)%>%
  step_dummy(all_nominal(), -all_outcomes())

data_5.2.training <- recipe_0 %>%
  prep(training = data_5.2.train)%>%
  bake(new_data = NULL)

data_5.2.testing <- recipe_0 %>%
  prep(training = data_5.2.test)%>%
  bake(new_data = NULL) 

# Five-fold cross-validation

folds_1 <- vfold_cv(data_5.2.training, v = 5, strata = unconditional_prcl)

# Setup model to be tuned

# Tuning time: 80 minutes

model_brt <- boost_tree(
  trees         = 1000,
  tree_depth    = tune(), # maximum depth of tree
  learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
  # min_n       = tune(),
  mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
  # stop_iter   = tune(),
  # sample_size = tune()
)%>%
  set_mode("classification")%>%
  set_engine("xgboost")

# Create a tuning grid - 16 different models for the tuning space

grid_0 <- grid_latin_hypercube(
  tree_depth(),
  learn_rate(c(-3,-0.5)),# tuning parameters
  mtry(c(round((ncol(data_5.2.training)-1)/2,0), ncol(data_5.2.training)-1)),
  size = 20)%>%
  # default parameters
  bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_5.2.training)-1))

# Tune the model - cover the entire parameter space without running every combination

print("Start computing")

doParallel::registerDoParallel()

time_1 <- Sys.time()

model_brt_1 <- tune_grid(model_brt,
                         unconditional_prcl ~ .,
                         resamples = folds_1,
                         grid      = grid_0,
                         metrics   = metric_set(accuracy, mn_log_loss, f_meas))

time_2 <- Sys.time()

doParallel::stopImplicitCluster()

print("End computing")

# Collect metrics of tuned models

metrics_1 <- collect_metrics(model_brt_1)

model_brt_1.1 <- select_best(model_brt_1, metric = "accuracy")

metrics_1.1 <- metrics_1 %>%
  filter(.config == model_brt_1.1$.config[1])

# Fit best model after tuning
model_brt <- boost_tree(
  trees      = 1000,
  tree_depth = 5,     # metrics_1.1$tree_depth[1] # 5
  learn_rate = 0.007, # metrics_1.1$learn_rate[1] # 0.007
  mtry = 170          # metrics_1.1$mtry[1]       # 170
)%>%
  set_mode("classification")%>%
  set_engine("xgboost")

model_brt_2 <- model_brt %>%
  fit(unconditional_prcl ~ .,
      data = data_5.2.training)

predictions_0 <- augment(model_brt_2, new_data = data_5.2.testing) 

accuracy(predictions_0, truth = unconditional_prcl, estimate = .pred_class) # 0.40

# Class-wise accuracy
cw_accuracy <- predictions_0 %>%
  mutate(correct = ifelse(unconditional_prcl == .pred_class,1,0))%>%
  group_by(unconditional_prcl)%>%
  summarise(class_accuracy = mean(correct))%>%
  ungroup()

mn_log_loss(predictions_0, truth = unconditional_prcl, estimate = c(".pred_Strongly disagree", ".pred_Disagree", ".pred_Neutral", ".pred_Agree", ".pred_Strongly agree")) # 1.40

confusion <- conf_mat(predictions_0, truth = unconditional_prcl, estimate = .pred_class)

data_5.2.testing_matrix <- data_5.2.testing %>%
  select(-unconditional_prcl)%>%
  as.matrix()

data_5.2.training_matrix <- data_5.2.training %>%
  select(-unconditional_prcl)%>%
  as.matrix()

time_3 <- Sys.time()

shap_1 <- predict(extract_fit_engine(model_brt_2),
                  data_5.2.testing_matrix,
                  predcontrib = TRUE,
                  approxcontrib = FALSE)

time_4 <- Sys.time()

evaluate_SHAP <- function(shap_0, unconditional_prcl_0){
  shap_1.1 <- shap_0 %>%
    as_tibble()%>%
    summarise_all(~ mean(abs(.)))%>%
    select(-BIAS)%>%
    pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    arrange(desc(SHAP_contribution))%>%
    mutate(tot_contribution = sum(SHAP_contribution))%>%
    mutate(share_SHAP = SHAP_contribution/tot_contribution)%>%
    select(-tot_contribution)%>%
    mutate(unconditional_prcl = unconditional_prcl_0)
  
  shap_1.2 <- shap_1.1 %>%
    mutate(VAR_0 = case_when(grepl("dept_", variable) ~ "Department",
                             grepl("Province_", variable) ~ "Province",
                             grepl(".COP", variable) ~ "Income",
                             grepl("edad", variable) ~ "Age",
                             grepl("gnro", variable) ~ "Gender",
                             grepl("etnia_", variable) ~ "Ethnicity",
                             grepl("ccnr", variable) ~ "Cooking fuel",
                             grepl("edu_", variable) ~ "Education",
                             grepl("urban", variable) ~ "Urban",
                             grepl("trbjo_", variable) ~ "Occupation",
                             grepl("moto_dias", variable)     ~ "Motorcycle (days)",
                             grepl("carro_dias", variable)    ~ "Car (days)",
                             grepl("transpub_dias", variable) ~ "Public transport (days)",
                             grepl("taxi_dias", variable)     ~ "Taxi (days)",
                             grepl("bici_dias", variable)     ~ "Bicycle (days)",
                             
                             grepl("benefic", variable) ~ "Have you benefitted from FFS?",
                             grepl("Group_", variable) ~ variable,
                             variable %in% c("bici_Yes", "carro_Yes", "moto_Yes", "transpub_Yes", "taxi_Yes", "treatment", "noise") ~ variable,
                             grepl("carro_combus", variable) ~ "Car combustible",
                             grepl("cc_econ", variable)      ~ "cc_econ",
                             grepl("cc_futuro", variable)    ~ "cc_futuro",
                             grepl("cc_info", variable)      ~ "cc_info",
                             grepl("cc_preocup", variable)   ~ "cc_preocup",
                             grepl("cc_imp_co2", variable)   ~ "Climate policy should reduce emissions",
                             grepl("cc_imp_pers", variable)  ~ "Climate policy should incur low costs",
                             grepl("cc_imp_equit", variable) ~ "Climate policy should be equitable",
                             grepl("derecho", variable) ~ "Right to receive subsidy",
                             grepl("estrto", variable) ~ "estrto",
                             grepl("grp_ingrso", variable) ~ "Income group",
                             grepl("izq_der", variable) ~ "Left/right",
                             grepl("dsl_super", variable) ~ "dsl_super",
                             grepl("gas_super", variable) ~ "gas_super",
                             grepl("ffsr_dsl", variable)  ~ "Is diesel subsidized?",
                             grepl("ffsr_gas", variable)  ~ "ffsr_gas",
                             grepl("ffsr_gnrl", variable) ~ "Are fuels subsidized?",
                             grepl("pais_army",    variable) ~ "pais_army",
                             grepl("pais_con_crt", variable) ~ "pais_con_crt",
                             grepl("pais_congrs",  variable) ~ "pais_congrs",
                             grepl("pais_parties", variable) ~ "pais_parties",
                             grepl("pais_jst_crt", variable) ~ "pais_jst_crt",
                             grepl("pais_gvnrs",   variable) ~ "pais_gvnrs",
                             grepl("pais_mayors",  variable) ~ "pais_mayors",
                             grepl("pais_prsdnt",  variable) ~ "pais_prsdnt",
                             grepl("pais_police",  variable) ~ "pais_police",
                             grepl("pais_econ", variable)  ~ "What is the economic situation",
                             grepl("pais_dmcrc", variable) ~ "pais_dmcrc",
                             grepl("pais_gnrl", variable)  ~ "pais_gnrl",
                             grepl("pais_inst", variable) ~ "pais_inst",
                             grepl("pais_indv", variable) ~ "pais_indv",
                             grepl("treatment", variable) ~ "Treatment",
                             
                             grepl("yo_amnto", variable) ~ "Affected by price increase",
                             grepl("pobre_amnto", variable) ~ "Poor affected by price increase",
                             grepl("rica_amnto", variable)  ~ "Rich affected by price increase",
                             grepl("pol_pres", variable) ~ "Voted for president",
                             grepl("prop_acrd_labrl", variable) ~ "prop_acrd_labrl",
                             grepl("prop_acrd_pensl", variable) ~ "prop_acrd_pensl",
                             grepl("prop_acrd_fepc", variable)  ~ "Agreement with FFSR",
                             grepl("prop_acrd_salud", variable) ~ "prop_acrd_salud",
                             grepl("prop_acrd_paz", variable)   ~ "prop_acrd_paz",
                             grepl("prop_acrd_energ", variable) ~ "prop_acrd_energ",
                             grepl("prop_acrd_ning", variable)  ~ "prop_acrd_ning",
                             grepl("prsns", variable) ~ "Household size",
                             grepl("protestas", variable) ~ "Protests",
                             grepl("sesgo", variable) ~ "Bias",
                             grepl("info_afic", variable) ~ "info_afic",
                             grepl("info_nose", variable) ~ "info_nose",
                             grepl("info_pernal", variable) ~ "info_pernal",
                             grepl("info_perreg", variable) ~ "info_perreg",
                             grepl("info_pscall", variable) ~ "info_pscall",
                             grepl("info_radlol", variable) ~ "info_radlol",
                             grepl("info_radnal", variable) ~ "info_radnal",
                             grepl("info_redscl", variable) ~ "info_redscl",
                             grepl("info_tvnal", variable) ~ "info_tvnal",
                             grepl("info_tvreg", variable) ~ "info_tvreg",
                             grepl("info_vlls", variable) ~ "info_vlls",
                             grepl("info_voz", variable) ~ "info_voz",
                             grepl("info_web", variable) ~ "info_web",
                             grepl("frst_", variable) ~ "First-stage question",
                             grepl("pol_prtds", variable) ~ "Pol_Prtds",
                             grepl("T_A", variable) ~ "T_A",
                             grepl("T_B", variable) ~ "T_B",
                             grepl("T_C", variable) ~ "T_C",
                             grepl("T_D", variable) ~ "T_D",
                             grepl("C_A", variable) ~ "C_A",
                             grepl("C_B", variable) ~ "C_B",
                             grepl("C_C", variable) ~ "C_C",
                             grepl("C_D", variable) ~ "C_D",
                             .default = NA))%>%
    arrange(variable)%>%
    group_by(VAR_0)%>%
    summarise(share_SHAP = sum(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(share_SHAP))%>%
    mutate(unconditional_prcl = unconditional_prcl_0)
  
  list_0 <- list("shap_1.1" = shap_1.1, "shap_1.2" = shap_1.2)
  
  return(list_0)
}

shap_A_1 <- evaluate_SHAP(shap_1[[1]], "1")$shap_1.1
shap_A_2 <- evaluate_SHAP(shap_1[[2]], "2")$shap_1.1
shap_A_3 <- evaluate_SHAP(shap_1[[3]], "3")$shap_1.1
shap_A_4 <- evaluate_SHAP(shap_1[[4]], "4")$shap_1.1
shap_A_5 <- evaluate_SHAP(shap_1[[5]], "5")$shap_1.1

shap_B_1 <- evaluate_SHAP(shap_1[[1]], "1")$shap_1.2
shap_B_2 <- evaluate_SHAP(shap_1[[2]], "2")$shap_1.2
shap_B_3 <- evaluate_SHAP(shap_1[[3]], "3")$shap_1.2
shap_B_4 <- evaluate_SHAP(shap_1[[4]], "4")$shap_1.2
shap_B_5 <- evaluate_SHAP(shap_1[[5]], "5")$shap_1.2

shap_A <- bind_rows(shap_A_1, shap_A_2, shap_A_3, shap_A_4, shap_A_5)
shap_B <- bind_rows(shap_B_1, shap_B_2, shap_B_3, shap_B_4, shap_B_5)

rm(shap_A_1, shap_A_2, shap_A_3, shap_A_4, shap_A_5, shap_B_1, shap_B_2, shap_B_3, shap_B_4, shap_B_5)

# What is the single most important feature across level predictions?

shap_B_1 <- shap_B %>%
  group_by(VAR_0)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  mutate(unconditional_prcl = "0")

# Inspect characteristics that are below noise for every characteristic

shap_B_2 <- shap_B %>%
  bind_rows(shap_B_1)%>%
  mutate(help = ifelse(VAR_0 == "noise", share_SHAP,0))%>%
  group_by(unconditional_prcl)%>%
  mutate(help = sum(help))%>%
  ungroup()%>%
  # Lower than noise
  filter(share_SHAP < help)%>%
  group_by(VAR_0)%>%
  summarise(number = n())%>%
  ungroup()%>%
  arrange(desc(number))

shap_B_3 <- shap_B %>%
  bind_rows(shap_B_1)%>%
  arrange(unconditional_prcl, desc(share_SHAP))%>%
  group_by(unconditional_prcl)%>%
  mutate(number = 1:n())%>%
  ungroup()%>%
  mutate(VAR_0 = ifelse(number > 4, "Other features", VAR_0))%>%
  group_by(unconditional_prcl, VAR_0)%>%
  summarise(share_SHAP = sum(share_SHAP))%>%
  ungroup()%>%
  filter(VAR_0 != "Other features")%>%
  mutate(VAR_0 = reorder_within(VAR_0, share_SHAP, unconditional_prcl))%>%
  mutate(unconditional_prcl = case_when(unconditional_prcl == 1 ~ "Strongly disagree (Accuracy: 0.65)",
                                     unconditional_prcl == 2 ~ "Disagree (Accuracy: 0.18)",
                                     unconditional_prcl == 3 ~ "Neutral (Accuracy: 0.44)",
                                     unconditional_prcl == 4 ~ "Agree (Accuracy: 0.40)",
                                     unconditional_prcl == 5 ~ "Strongly agree (Accuracy: 0.11)",
                                     unconditional_prcl == 0 ~ "Overall (Accuracy: 0.4)"))%>%
  mutate(unconditional_prcl = factor(unconditional_prcl, levels = c("Overall (Accuracy: 0.4)",
                                                                    "Strongly disagree (Accuracy: 0.65)",
                                                                    "Disagree (Accuracy: 0.18)",
                                                                    "Neutral (Accuracy: 0.44)",
                                                                    "Agree (Accuracy: 0.40)",
                                                                    "Strongly agree (Accuracy: 0.11)")))%>%
  filter(unconditional_prcl %in% c("Strongly disagree (Accuracy: 0.65)","Neutral (Accuracy: 0.44)","Strongly agree (Accuracy: 0.11)"))

P_5.1 <- ggplot(shap_B_3)+
  geom_col(aes(x = VAR_0, y = share_SHAP), width = 0.75, fill = "#0072B5FF", colour = "black", alpha = 0.7, linewidth =  0.3)+
  facet_wrap(. ~ unconditional_prcl, scales = "free_y")+
  scale_x_discrete(labels = function(x) sub("__.*$", "", x))+  # Remove appended suffix
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), expand = c(0,0), breaks = c(0,0.05,0.1,0.15))+
  coord_flip(ylim = c(0,0.159))+
  theme_bw()+
  ylab("Feature importance (SHAP)")+
  xlab("Feature")+
  ggtitle("Predicting answers to \"The government of Colombia should partially reduce fossil fuel subsidies.\"")+
  theme(axis.text     = element_text(size = 4),
        title           = element_text(size = 6),
        strip.text      = element_text(size = 4.5),
        axis.title      = element_text(size = 5),
        legend.text     = element_text(size = 7),
        legend.position = "bottom",
        legend.title    = element_blank(),
        legend.key.size = unit(0.5, "cm"),
        axis.ticks = element_line(linewidth = 0.3),
        panel.grid.major = element_line(linewidth = 0.3),
        panel.grid.minor = element_line(linewidth = 0.1),
        plot.title.position = "plot")

jpeg("../Colombia_Survey_Experiment/Paper/Figures/3_Supplementary/Figure_Feature_Importance_US.jpg", width = 15.5, height = 5, unit = "cm", res = 600)
print(P_5.1)
dev.off()

rm(cw_accuracy, data_5, data_5.2.test, data_5.2.testing, data_5.2.train, data_5.2.training, data_5.2.testing_matrix, data_5.2.training_matrix,
   shap_A, shap_B, shap_B_1, shap_B_2, shap_B_3, time_3, time_4, evaluate_SHAP, P_5.1)

# 5.2   Changes in support from unconditional to conditional support ####

data_5.2 <- data_5 %>%
  select(-rr_mas, -rr_mas_si, -status_quo, -unconditional_complet)%>%
  mutate_at(vars(starts_with("rr_")), ~ . - unconditional_prcl)%>%
  rename_at(vars(starts_with("rr_")), ~ str_replace(., "$", "_dif"))%>%
  select(-unconditional_prcl, -C_no_frst, -conditional, -treatment, - starts_with("C_"), -mncp)

# 5.2.1 Tuning hyperparameters ####

for(i in c("rr_lmpsm_dif", "rr_pobre_dif", "rr_afctds_dif", "rr_impuesto_dif", "rr_deuda_dif",
            "rr_etransp_dif", "rr_paz_dif", "rr_edu_dif", "rr_ncer_dif", "rr_deforst_dif")){
  print(i)
  
  metrics_0 <- read.xlsx("Tuning_H2_Metrics.xlsx")
  
  data_5.2.1 <- data_5.2 %>%
    rename(var_0 = i)%>%
    select(- starts_with("rr_"))%>%
    mutate(var_0 = ifelse(var_0 < 0, "Lower support",
                          ifelse(var_0 > 0, "Higher support", "Neutral")))%>%
    mutate(var_0 = factor(var_0, levels = c("Lower support", "Neutral", "Higher support")))
  
  data_5.2.1 <- data_5.2.1 %>%
    # Create noise parameter
    mutate(noise = rnorm(nrow(.),0,1))
  
  data_5.2.2 <- data_5.2.1 %>%
    initial_split(prop = 0.8)
  
  # Data for training
  data_5.2.train <- data_5.2.2 %>%
    training()
  
  # Data for testing
  data_5.2.test <- data_5.2.2 %>%
    testing()
  
  rm(data_5.2.1, data_5.2.2)
  
  recipe_0 <- recipe(var_0 ~ .,
                     data = data_5.2.train)%>%
    # Deletes all columns with any NA
    step_filter_missing(all_predictors(), threshold = 0)%>%
    # Remove minimum number of columns such that correlations are less than 0.9
    step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
    # should have very few unique observations for factors
    # step_other(all_nominal(), -edad, -grp_ingrso, threshold = 0.03)%>%
    step_dummy(all_nominal(), -all_outcomes())
  
  data_5.2.training <- recipe_0 %>%
    prep(training = data_5.2.train)%>%
    bake(new_data = NULL)
  
  data_5.2.testing <- recipe_0 %>%
    prep(training = data_5.2.test)%>%
    bake(new_data = NULL) 
  
  folds_1 <- vfold_cv(data_5.2.training, v = 5, strata = var_0)
  
  # Setup model to be tuned
  
  # Tuning time: 80 minutes
  
  model_brt <- boost_tree(
    trees         = 1000,
    tree_depth    = tune(), # maximum depth of tree
    learn_rate    = tune(), # the higher the learning rate the faster - default 0.3
    # min_n       = tune(),
    mtry          = tune(), # fraction of features to be selected for each tree (0.5/0.7/1)
    # stop_iter   = tune(),
    # sample_size = tune()
  )%>%
    set_mode("classification")%>%
    set_engine("xgboost")
  
  # Create a tuning grid - 16 different models for the tuning space
  
  grid_0 <- grid_latin_hypercube(
    tree_depth(),
    learn_rate(c(-3,-0.5)),# tuning parameters
    mtry(c(round((ncol(data_5.2.training)-1)/2,0), ncol(data_5.2.training)-1)),
    size = 20)%>%
    # default parameters
    bind_rows(data.frame(tree_depth = 6, learn_rate = 0.3, mtry = ncol(data_5.2.training)-1))
  
  # Tune the model - cover the entire parameter space without running every combination
  
  print("Start computing")
  
  doParallel::registerDoParallel()
  
  time_1 <- Sys.time()
  
  model_brt_1 <- tune_grid(model_brt,
                           var_0 ~ .,
                           resamples = folds_1,
                           grid      = grid_0,
                           metrics   = metric_set(accuracy, mn_log_loss, f_meas))
  
  time_2 <- Sys.time()
  
  doParallel::stopImplicitCluster()
  
  print("End computing")
  
  # Collect metrics of tuned models
  
  metrics_1 <- collect_metrics(model_brt_1)
  
  model_brt_1.1 <- select_best(model_brt_1, metric = "accuracy")
  
  metrics_1.1 <- metrics_1 %>%
    filter(.config == model_brt_1.1$.config[1])%>%
    mutate(outcome = i)
  
  metrics_0 <- metrics_0 %>%
    bind_rows(metrics_1.1)
  
  write.xlsx(metrics_0, "Tuning_H2_Metrics.xlsx")
}

# 5.2.2 Fit best model after tuning ####

metrics_0 <- read.xlsx("Tuning_H2_Metrics.xlsx")

evaluate_SHAP <- function(shap_0, var_1){
  shap_1.1 <- shap_0 %>%
    as_tibble()%>%
    summarise_all(~ mean(abs(.)))%>%
    select(-BIAS)%>%
    pivot_longer(everything(), names_to = "variable", values_to = "SHAP_contribution")%>%
    arrange(desc(SHAP_contribution))%>%
    mutate(tot_contribution = sum(SHAP_contribution))%>%
    mutate(share_SHAP = SHAP_contribution/tot_contribution)%>%
    select(-tot_contribution)%>%
    mutate(var_0 = var_1)
  
  shap_1.2 <- shap_1.1 %>%
    mutate(VAR_0 = case_when(grepl("dept_", variable) ~ "Department",
                             grepl("mncp_", variable) ~ "Municipio",
                             grepl("Province_", variable) ~ "Province",
                             
                             grepl(".COP", variable) ~ "Income",
                             grepl("edad", variable) ~ "Age",
                             grepl("gnro", variable) ~ "Gender",
                             grepl("etnia_", variable) ~ "Ethnicity",
                             grepl("ccnr", variable) ~ "Cooking fuel",
                             grepl("edu_", variable) ~ "Education",
                             grepl("urban", variable) ~ "Urban",
                             grepl("trbjo_", variable) ~ "Occupation",
                             grepl("moto_dias", variable)     ~ "Motorcycle (days)",
                             grepl("carro_dias", variable)    ~ "Car (days)",
                             grepl("transpub_dias", variable) ~ "Public transport (days)",
                             grepl("taxi_dias", variable)     ~ "Taxi (days)",
                             grepl("bici_dias", variable)     ~ "Bicycle (days)",
                             
                             grepl("benefic", variable) ~ "Benefitted",
                             grepl("Group_", variable) ~ variable,
                             variable %in% c("bici_Yes", "carro_Yes", "moto_Yes", "transpub_Yes", "taxi_Yes", "treatment", "noise") ~ variable,
                             grepl("carro_combus", variable) ~ "Car combustible",
                             grepl("cc_econ", variable)      ~ "cc_econ",
                             grepl("cc_futuro", variable)    ~ "cc_futuro",
                             grepl("cc_info", variable)      ~ "cc_info",
                             grepl("cc_preocup", variable)   ~ "cc_preocup",
                             grepl("cc_imp_co2", variable)   ~ "Climate policy should reduce emissions",
                             grepl("cc_imp_pers", variable)  ~ "Climate policy should incur low costs",
                             grepl("cc_imp_equit", variable) ~ "Climate policy should be equitable",
                             grepl("derecho", variable) ~ "Right to receive subsidy",
                             grepl("estrto", variable) ~ "estrto",
                             grepl("grp_ingrso", variable) ~ "Income group",
                             grepl("izq_der", variable) ~ "Left/right",
                             grepl("dsl_super", variable) ~ "dsl_super",
                             grepl("gas_super", variable) ~ "gas_super",
                             grepl("ffsr_dsl", variable)  ~ "Is diesel subsidized?",
                             grepl("ffsr_gas", variable)  ~ "ffsr_gas",
                             grepl("ffsr_gnrl", variable) ~ "Are fuels subsidized?",
                             grepl("pais_army",    variable) ~ "pais_army",
                             grepl("pais_con_crt", variable) ~ "pais_con_crt",
                             grepl("pais_congrs",  variable) ~ "pais_congrs",
                             grepl("pais_parties", variable) ~ "pais_parties",
                             grepl("pais_jst_crt", variable) ~ "pais_jst_crt",
                             grepl("pais_gvnrs",   variable) ~ "pais_gvnrs",
                             grepl("pais_mayors",  variable) ~ "pais_mayors",
                             grepl("pais_prsdnt",  variable) ~ "pais_prsdnt",
                             grepl("pais_police",  variable) ~ "pais_police",
                             grepl("pais_econ", variable)  ~ "What is the economic situation",
                             grepl("pais_dmcrc", variable) ~ "pais_dmcrc",
                             grepl("pais_gnrl", variable)  ~ "pais_gnrl",
                             grepl("pais_inst", variable) ~ "pais_inst",
                             grepl("pais_indv", variable) ~ "pais_indv",
                             grepl("yo_amnto", variable) ~ "Affected by price increase",
                             grepl("pobre_amnto", variable) ~ "Poor affected by price increase",
                             grepl("rica_amnto", variable)  ~ "Rich affected by price increase",
                             grepl("pol_pres", variable) ~ "Voted for president",
                             grepl("prop_acrd_labrl", variable) ~ "prop_acrd_labrl",
                             grepl("prop_acrd_pensl", variable) ~ "prop_acrd_pensl",
                             grepl("prop_acrd_fepc", variable)  ~ "Agree with FFSR",
                             grepl("prop_acrd_salud", variable) ~ "prop_acrd_salud",
                             grepl("prop_acrd_paz", variable)   ~ "prop_acrd_paz",
                             grepl("prop_acrd_energ", variable) ~ "prop_acrd_energ",
                             grepl("prop_acrd_ning", variable)  ~ "prop_acrd_ning",
                             grepl("prsns", variable) ~ "Household size",
                             grepl("protestas", variable) ~ "Protests",
                             grepl("sesgo", variable) ~ "Bias",
                             grepl("info_afic", variable) ~ "info_afic",
                             grepl("info_nose", variable) ~ "info_nose",
                             grepl("info_pernal", variable) ~ "info_pernal",
                             grepl("info_perreg", variable) ~ "info_perreg",
                             grepl("info_pscall", variable) ~ "info_pscall",
                             grepl("info_radlol", variable) ~ "info_radlol",
                             grepl("info_radnal", variable) ~ "info_radnal",
                             grepl("info_redscl", variable) ~ "info_redscl",
                             grepl("info_tvnal", variable) ~ "info_tvnal",
                             grepl("info_tvreg", variable) ~ "info_tvreg",
                             grepl("info_vlls", variable) ~ "info_vlls",
                             grepl("info_voz", variable) ~ "info_voz",
                             grepl("info_web", variable) ~ "info_web",
                             grepl("frst_", variable) ~ "First",
                             grepl("pol_prtds", variable) ~ "Pol_Prtds",
                             grepl("_Yes", variable) ~ "Treatment",
                             .default = NA))%>%
    arrange(variable)%>%
    group_by(VAR_0)%>%
    summarise(share_SHAP = sum(share_SHAP))%>%
    ungroup()%>%
    arrange(desc(share_SHAP))%>%
    mutate(var_0 = var_1)
  
  list_0 <- list("shap_1.1" = shap_1.1, "shap_1.2" = shap_1.2)
  
  return(list_0)
}

shap_0_A <- data.frame()
shap_0_B <- data.frame()

for(i in c("rr_lmpsm_dif", "rr_pobre_dif", "rr_afctds_dif", "rr_impuesto_dif", "rr_deuda_dif",
           "rr_etransp_dif", "rr_paz_dif", "rr_edu_dif", "rr_ncer_dif", "rr_deforst_dif")){
  
  print(i)
  print(Sys.time())
  
  data_5.2.1 <- data_5.2 %>%
    rename(var_0 = i)%>%
    select(- starts_with("rr_"))%>%
    mutate(var_0 = ifelse(var_0 < 0, "Lower support",
                          ifelse(var_0 > 0, "Higher support", "Neutral")))%>%
    mutate(var_0 = factor(var_0, levels = c("Lower support", "Neutral", "Higher support")))
  
  data_5.2.1 <- data_5.2.1 %>%
    # Create noise parameter
    mutate(noise = rnorm(nrow(.),0,1))
  
  data_5.2.2 <- data_5.2.1 %>%
    initial_split(prop = 0.8)
  
  # Data for training
  data_5.2.train <- data_5.2.2 %>%
    training()
  
  # Data for testing
  data_5.2.test <- data_5.2.2 %>%
    testing()
  
  rm(data_5.2.1, data_5.2.2)
  
  recipe_0 <- recipe(var_0 ~ .,
                     data = data_5.2.train)%>%
    # Deletes all columns with any NA
    step_filter_missing(all_predictors(), threshold = 0)%>%
    # Remove minimum number of columns such that correlations are less than 0.9
    step_corr(all_numeric(), -all_outcomes(), threshold = 0.9)%>%
    # should have very few unique observations for factors
    # step_other(all_nominal(), -edad, -grp_ingrso, threshold = 0.03)%>%
    step_dummy(all_nominal(), -all_outcomes())
  
  data_5.2.training <- recipe_0 %>%
    prep(training = data_5.2.train)%>%
    bake(new_data = NULL)
  
  data_5.2.testing <- recipe_0 %>%
    prep(training = data_5.2.test)%>%
    bake(new_data = NULL)
  
  metrics_X <- metrics_0 %>%
    filter(outcome == i)
  
  model_brt <- boost_tree(
    trees      = 1000,
    tree_depth = metrics_X$tree_depth[1],    
    learn_rate = metrics_X$learn_rate[1],
    mtry       = metrics_X$mtry[1])%>%
    set_mode("classification")%>%
    set_engine("xgboost")
  
  model_brt_0 <- model_brt %>%
    fit(var_0 ~ .,
        data = data_5.2.training)
  
  predictions_0 <- augment(model_brt_0, new_data = data_5.2.testing) 
  
  accuracy(predictions_0, truth = var_0, estimate = .pred_class) # 0.59
  
  # Class-wise accuracy
  cw_accuracy <- predictions_0 %>%
    mutate(correct = ifelse(var_0 == .pred_class,1,0))%>%
    group_by(var_0)%>%
    summarise(class_accuracy = mean(correct))%>%
    ungroup()
  
  mn_log_loss(predictions_0, truth = var_0, estimate = c(".pred_Lower support", ".pred_Neutral", ".pred_Higher support")) # 1.44
  
  conf_mat(predictions_0, truth = var_0, estimate = .pred_class)
  
  data_5.2.testing_matrix <- data_5.2.testing %>%
    select(-var_0)%>%
    as.matrix()
  
  data_5.2.training_matrix <- data_5.2.training %>%
    select(-var_0)%>%
    as.matrix()
  
  time_3 <- Sys.time()
  
  shap_1 <- predict(extract_fit_engine(model_brt_0),
                    data_5.2.testing_matrix,
                    predcontrib = TRUE,
                    approxcontrib = FALSE)
  
  time_4 <- Sys.time()
  
  shap_A_1 <- evaluate_SHAP(shap_1[[1]], "Lower support")$shap_1.1
  shap_A_2 <- evaluate_SHAP(shap_1[[2]], "Neutral")$shap_1.1
  shap_A_3 <- evaluate_SHAP(shap_1[[3]], "Higher support")$shap_1.1
  
  shap_B_1 <- evaluate_SHAP(shap_1[[1]], "Lower support")$shap_1.2
  shap_B_2 <- evaluate_SHAP(shap_1[[2]], "Neutral")$shap_1.2
  shap_B_3 <- evaluate_SHAP(shap_1[[3]], "Higher support")$shap_1.2
  
  shap_A <- bind_rows(shap_A_1, shap_A_2, shap_A_3)%>%
    mutate(Outcome = i)
  shap_B <- bind_rows(shap_B_1, shap_B_2, shap_B_3)%>%
    mutate(Outcome = i)
  
  rm(shap_A_1, shap_A_2, shap_A_3, shap_B_1, shap_B_2, shap_B_3)
  
  shap_0_A <- shap_0_A %>%
    bind_rows(shap_A)
  
  shap_0_B <- shap_0_B %>%
    bind_rows(shap_B)
  
}

# What is the single most important feature across level predictions?

shap_1_B <- shap_0_B %>%
  arrange(Outcome, var_0, desc(share_SHAP))%>%
  select(Outcome, var_0, VAR_0, share_SHAP)%>%
  filter(share_SHAP >0)

write.xlsx(shap_1_B, "ML_H2_Outcome.xlsx")

shap_B_1 <- shap_B %>%
  group_by(VAR_0)%>%
  summarise(share_SHAP = mean(share_SHAP))%>%
  ungroup()%>%
  mutate(var_0 = "0")

# Inspect characteristics that are below noise for every characteristic

shap_B_2 <- shap_B %>%
  bind_rows(shap_B_1)%>%
  mutate(help = ifelse(VAR_0 == "noise", share_SHAP,0))%>%
  group_by(var_0)%>%
  mutate(help = sum(help))%>%
  ungroup()%>%
  # Lower than noise
  filter(share_SHAP < help)%>%
  group_by(VAR_0)%>%
  summarise(number = n())%>%
  ungroup()%>%
  arrange(desc(number))

# 5.3   Descriptive statistics in general ####

# 6     Specification charts ####

data_6 <- data_1 %>%
  select(ID,edad, gnro, etnia, Province, edu, ingrso, grp_ingrso, moto, carro, transpub, ccnr, cc_info, cc_preocup, cc_econ,
         cc_imp_co2, cc_imp_pers, cc_imp_equit, pol_pres, izq_der, prop_acrd_fepc, prop_acrd_paz, prop_acrd_energ,
         pais_gnrl, pais_army, pais_police, pais_prsdnt, pais_dmcrc, pais_econ,
         ffsr_gnrl, ffsr_dsl, ffsr_gas, benefic, derecho, yo_amnto, pobre_amnto, rica_amnto, conditional, unconditional_prcl,
         T_A:T_D, C_A:C_D, treatment,
         rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda, rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst)%>%
  mutate(Group = ifelse(T_A == 1 | C_A == 1, "A",
                        ifelse(T_B == 1 | C_B == 1, "B",
                               ifelse(T_C == 1 | C_C == 1, "C",
                                      ifelse(T_D == 1 | C_D == 1, "D", "Control")))))%>%
  mutate(Group = factor(Group, level = c("Control", "A", "B", "C", "D")))%>%
  # select(-(T_A:C_D))%>%
  # Individual adjustment and change to factor
  mutate(edad  = factor(binning(edad, bins = 3, method = "quantile"), labels = c("18-36", "37-55", "56-95")),
         etnia = factor(ifelse(etnia == 6, "White", "Non-white")),
         edu = factor(ifelse(edu < 3, "Primary or none",
                             ifelse(edu < 5, "Secondary or technial", "University or postgraduate"))),
         ingrso = factor(ifelse(ingrso < 4, "0 - 900,000 COP",
                                ifelse(ingrso < 8, "900,001 - 1,825,000 COP", "> 1,825,001 COP"))),
         grp_ingrso = factor(ifelse(grp_ingrso < 3, "Low and lower-middle",
                                    ifelse(grp_ingrso < 4, "Middle", "Upper-middle and high"))),
         ccnr = factor(ifelse(ccnr == 1, "Electricity",
                              ifelse(ccnr < 5, "Natural gas and liquid fuels", "Solid fuels"))),
         cc_info = factor(ifelse(cc_info < 3, "Not informed",
                                 ifelse(cc_info == 3, "NA", "Informed"))),
         cc_preocup = factor(ifelse(cc_preocup < 3, "Not worried",
                                    ifelse(cc_preocup == 3, "NA", "Worried"))),
         cc_econ = factor(ifelse(cc_econ == 2, "Priority to economy",
                                 ifelse(cc_econ == 5, "Priority to CC",
                                        ifelse(cc_econ == 4, "Equal priority", "NA")))),
         pol_pres = factor(ifelse(pol_pres == 1, "Yes",
                                  ifelse(pol_pres > 3, "NA", "No"))),
         izq_der = factor(ifelse(izq_der < 2, "Right-wing",
                                 ifelse(izq_der == 3, "Center",
                                        ifelse(izq_der == 4 | izq_der == 5, "Left-wing", "NA")))))%>%
  mutate_at(vars(cc_imp_co2, cc_imp_pers, cc_imp_equit), 
            ~ factor(ifelse(. < 3, "Disagree", ifelse(. == 3, "Neutral", "Agree"))))%>%
  mutate_at(vars(pais_army, pais_police, pais_prsdnt), 
            ~ factor(ifelse(. < 3, "Low trust", ifelse(. == 3, "Moderate trust", "High trust"))))%>%
  mutate_at(vars(pais_dmcrc, pais_econ), ~ factor(ifelse(. < 3, "Bad",
                                                         ifelse(. > 3, "Good", "NA"))))%>%
  mutate_at(vars(benefic, derecho, yo_amnto, pobre_amnto, rica_amnto), 
            ~ factor(ifelse(. < 3, "Little to not at all",
                            ifelse(. > 3, "Somewhat to greatly", "NA"))))%>%
  # Convert to factors
  mutate_at(vars(gnro, moto:transpub, prop_acrd_fepc:prop_acrd_energ, pais_gnrl, Province),
            ~ haven::as_factor(.))%>%
  mutate(ffsr_dsl = ifelse(is.na(ffsr_dsl), ffsr_gnrl, ffsr_dsl),
         ffsr_gas = ifelse(is.na(ffsr_gas), ffsr_gnrl, ffsr_gas))%>%
  mutate_at(vars(ffsr_dsl, ffsr_gas), ~ factor(., labels = c("Yes", "I don't know", "No")))%>%
  mutate(ffsr_gnrl = haven::as_factor(ffsr_gnrl))%>%
  select(ID, Group, treatment, conditional, unconditional_prcl, T_A:T_D, everything())

# 6.1   H1 ####

data_6.1 <- data_6 %>%
  select(-c(rr_lmpsm, rr_pobre, rr_afctds, rr_impuesto, rr_deuda, rr_etransp, rr_paz, rr_edu, rr_ncer, rr_deforst))%>%
  pivot_longer(c(conditional, unconditional_prcl), values_to = "support", names_to = "Type")%>%
  mutate(conditional = ifelse(Type == "conditional",1,0))%>%
  select(-(C_A:C_D))


# to be included: frst_a, frst_b, frst_c, frst_d
# ffsr_mnt, ffsr_prcl, ffsr_complet for H2

data_6.1.0 <- data.frame()

colnames_0 <- colnames(select(data_6, -c(ID:unconditional_prcl),-c(C_A:rr_deforst), -c(T_A:T_D)))

for(var_0 in colnames_0){
  print(var_0)
  # Delete observations first
  
  data_6.1.1 <- data_6.1
  
  if(var_0 == "gnro")                 {data_6.1.1 <- filter(data_6.1.1, gnro != "Other")}
  if(var_0 == "cc_info")              {data_6.1.1 <- filter(data_6.1.1, cc_info != "NA")}
  if(var_0 == "cc_preocup")           {data_6.1.1 <- filter(data_6.1.1, cc_preocup != "NA")}
  if(var_0 == "cc_econ")              {data_6.1.1 <- filter(data_6.1.1, cc_econ != "NA")}
  if(var_0 == "cc_imp_co2")           {data_6.1.1 <- filter(data_6.1.1, cc_imp_co2 != "Neutral")}
  if(var_0 == "cc_imp_pers")          {data_6.1.1 <- filter(data_6.1.1, cc_imp_pers != "Neutral")}
  if(var_0 == "cc_imp_equit")         {data_6.1.1 <- filter(data_6.1.1, cc_imp_equit != "Neutral")}
  if(var_0 == "pol_pres")             {data_6.1.1 <- filter(data_6.1.1, pol_pres != "NA")}
  if(var_0 == "izq_der")              {data_6.1.1 <- filter(data_6.1.1, izq_der != "NA" & !is.na(izq_der))}
  if(var_0 == "pais_army")  {data_6.1.1 <- filter(data_6.1.1, pais_army != "Moderate trust")}
  if(var_0 == "pais_police"){data_6.1.1 <- filter(data_6.1.1, pais_police != "Moderate trust")}
  if(var_0 == "pais_prsdnt"){data_6.1.1 <- filter(data_6.1.1, pais_prsdnt != "Moderate trust")}
  if(var_0 == "prop_acrd_fepc")       {data_6.1.1 <- filter(data_6.1.1, !is.na(prop_acrd_fepc))}
  if(var_0 == "prop_acrd_paz")        {data_6.1.1 <- filter(data_6.1.1, !is.na(prop_acrd_paz))}
  if(var_0 == "prop_acrd_energ")      {data_6.1.1 <- filter(data_6.1.1, !is.na(prop_acrd_energ))}
  if(var_0 == "pais_gnrl")            {data_6.1.1 <- filter(data_6.1.1, !is.na(pais_gnrl) & pais_gnrl != "Prefer not to say")}
  if(var_0 == "pais_dmcrc")           {data_6.1.1 <- filter(data_6.1.1, pais_dmcrc != "NA")}
  if(var_0 == "pais_econ")            {data_6.1.1 <- filter(data_6.1.1, pais_econ != "NA" & pais_econ != "Prefer not to say")}
  if(var_0 == "ffsr_gnrl")            {data_6.1.1 <- filter(data_6.1.1, ffsr_gnrl != "I don't know")}
  if(var_0 == "ffsr_dsl")             {data_6.1.1 <- filter(data_6.1.1, ffsr_dsl != "I don't know")}
  if(var_0 == "ffsr_gas")             {data_6.1.1 <- filter(data_6.1.1, ffsr_gas != "I don't know")}
  if(var_0 == "benefic")              {data_6.1.1 <- filter(data_6.1.1, benefic != "NA")}
  if(var_0 == "derecho")              {data_6.1.1 <- filter(data_6.1.1, derecho != "NA")}
  if(var_0 == "yo_amnto")             {data_6.1.1 <- filter(data_6.1.1, yo_amnto != "NA")}
  if(var_0 == "pobre_amnto")          {data_6.1.1 <- filter(data_6.1.1, pobre_amnto != "NA")}
  if(var_0 == "rica_amnto")           {data_6.1.1 <- filter(data_6.1.1, rica_amnto != "NA")}
  
  
  data_6.1.1 <- data_6.1.1 %>%
    rename(var_interest = any_of(var_0))
  
  model <- feols(support ~ conditional | ID, data = data_6.1.1, split = ~ var_interest)
  
  summary_model <- summary(model)
  
  data_6.1.2 <- data.frame()
  
  for(i in 1:length(summary_model)){
    
    data_6.1.2.1 <- data.frame(number = i,
                               name   = names(model)[i],
                               coefficient = unname(summary(model[[i]])$coefficients),
                               std_error   = unname(summary(model[[i]])$se)) 
    
    data_6.1.2 <- data_6.1.2 %>%
      bind_rows(data_6.1.2.1)
    
  }
  
  data_6.1.2 <- data_6.1.2 %>%
    mutate(Term = var_0)%>%
    select(Term, everything())
  
  data_6.1.0 <- data_6.1.0 %>%
    bind_rows(data_6.1.2)
    
}


rm(data_6.1.2, data_6.1.2.1, data_6.1.1, colnames_0, model, summary_model)

data_6.1.3 <- data_6.1.0 %>%
  mutate(conf_low  = coefficient - 1.96*std_error,
         conf_high = coefficient + 1.96*std_error)%>%
  mutate(Term = factor(Term, levels = c("edad", "gnro", "etnia", "Province", "edu", "ingrso", "grp_ingrso", "moto", "carro", "transpub", "ccnr", "cc_info", "cc_preocup", "cc_econ",
                                        "cc_imp_co2", "cc_imp_pers", "cc_imp_equit", "pol_pres", "izq_der", "prop_acrd_fepc", "prop_acrd_paz", "prop_acrd_energ",
                                        "pais_gnrl", "pais_army", "pais_police", "pais_prsdnt", "pais_dmcrc", "pais_econ",
                                        "ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto")))%>%
  mutate(name = case_when(Term == "edad"         ~ str_replace(name, "^", "Age: "),
                          Term == "gnro"         ~ str_replace(name, "^", "Gender: "),
                          Term == "etnia"        ~ str_replace(name, "^", "Ethnicity: "),
                          Term == "Province"     ~ str_replace(name, "^", "Province: "),
                          Term == "edu"          ~ str_replace(name, "^", "Education: "),
                          Term == "ingrso"       ~ str_replace(name, "^", "Income: "),
                          Term == "grp_ingrso"   ~ str_replace(name, "^", "Perc. income group: "),
                          Term == "moto"         ~ str_replace(name, "^", "Owns a motorcycle: "),
                          Term == "carro"        ~ str_replace(name, "^", "Owns a car: "),
                          Term == "transpub"     ~ str_replace(name, "^", "Uses public transport: "),
                          Term == "ccnr"         ~ str_replace(name, "^", "Cooking fuel: "),
                          Term == "cc_info"      ~ str_replace(name, "^", "Informed about CC: "),
                          Term == "cc_preocup"   ~ str_replace(name, "^", "Worried about CC: "),
                          Term == "cc_econ"      ~ str_replace(name, "^", "CC vs. economy: "),
                          Term == "cc_imp_co2"   ~ str_replace(name, "^", "Climate policy - effective: "),
                          Term == "cc_imp_pers"  ~ str_replace(name, "^", "Climate policy - low costs: "),
                          Term == "cc_imp_equit" ~ str_replace(name, "^", "Climate policy - equity: "),
                          
                          Term == "pol_pres"        ~ str_replace(name, "^", "Voted for president: "),
                          Term == "izq_der"         ~ str_replace(name, "^", "Political position: "),
                          Term == "prop_acrd_fepc"  ~ str_replace(name, "^", "Agree with FFSR: "),
                          Term == "prop_acrd_paz"   ~ str_replace(name, "^", "Agree with paz total: "),
                          Term == "prop_acrd_energ" ~ str_replace(name, "^", "Agree with energy transition: "),
                          Term == "pais_gnrl"       ~ str_replace(name, "^", "Perc. country situation: "),
                          Term == "pais_army"       ~ str_replace(name, "^", "Trust in army: "),
                          Term == "pais_police"     ~ str_replace(name, "^", "Trust in police: "),
                          Term == "pais_prsdnt"     ~ str_replace(name, "^", "Trust in democracy: "),
                          Term == "pais_dmcrc"      ~ str_replace(name, "^", "State of democracy: "),
                          Term == "pais_econ"       ~ str_replace(name, "^", "State of economy: "),
                          Term == "ffsr_gnrl"       ~ str_replace(name, "^", "Thinks: fuels are subsidised: "),
                          Term == "ffsr_dsl"        ~ str_replace(name, "^", "Thinks: diesel is subsidised: "),
                          Term == "ffsr_gas"        ~ str_replace(name, "^", "Thinks: gasoline is subsidised : "),
                          Term == "benefic"         ~ str_replace(name, "^", "Thinks: benefit from subsidy: "),
                          Term == "derecho"         ~ str_replace(name, "^", "Thinks: right for subsidy: "),
                          Term == "yo_amnto"        ~ str_replace(name, "^", "Concern cost on oneself: "),
                          Term == "pobre_amnto"     ~ str_replace(name, "^", "Concern costs on poor: "),
                          Term == "rica_amnto"      ~ str_replace(name, "^", "Concern costs on rich: "),
                          TRUE ~ name))%>%
  mutate(Row = ifelse(Term %in% c("edad", "gnro", "etnia", "Province", "edu", "ingrso", "grp_ingrso", "moto", "carro", "transpub", "ccnr", "cc_info", "cc_preocup", "cc_econ",
                                  "cc_imp_co2", "cc_imp_pers", "cc_imp_equit"),1,0))

data_6.1.3.1 <- data_6.1.3 %>%
  filter(Row == 1)%>%
  mutate(name = factor(name))%>%
  mutate(name = fct_relevel(name, levels(name)[c(1:3,5,4,6:22,24,25,23,26:length(levels(name))) ]))

data_6.1.3.2 <- data_6.1.3 %>%
  filter(Row == 0)%>%
  mutate(name = factor(name))%>%
  mutate(name = fct_relevel(name, levels(name)[c(1:15,17,16,18:length(levels(name))) ]))


functions_labels <- function(x) {
  lapply(x, function(lbl) {
    parts <- strsplit(lbl, ":", fixed = TRUE)[[1]]
    # first part is before first colon, second part is everything else
    first <- parts[1]
    rest <- paste(parts[-1], collapse = ":")
    bquote(bold(.(paste0(first, ":"))) * .(rest))
  })
}

P_6.1.1 <- ggplot(data_6.1.3.1, aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 1.1535, ymax = 1.257), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-0.1,2.2))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))

P_6.1.2 <- ggplot(data_6.1.3.2, aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 1.1535, ymax = 1.257), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-0.1,2.2))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))


jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A_H1_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_6.1.1)
print(P_6.1.2)
dev.off()

rm(P_6.1.1, P_6.1.2, data_6.1.3.1, data_6.1.3.2)

# 6.2   H2 ####

data_6.2 <- data_6 %>%
  mutate(mean_social = (rr_lmpsm + rr_pobre + rr_afctds + rr_impuesto + rr_deuda)/5,
         mean_public = (rr_etransp + rr_paz + rr_edu + rr_ncer + rr_deforst)/5)%>%
  select(ID:unconditional_prcl, mean_social, mean_public, rr_lmpsm:rr_deforst, everything())

feols(mean_social ~ T_A, data = filter(data_6.2, Group == "A")) # -0.177 bis 0.095
feols(mean_social ~ T_B, data = filter(data_6.2, Group == "B")) # -0.181 bis 0.081
feols(mean_public ~ T_C, data = filter(data_6.2, Group == "C")) # -0.157 bis 0.090
feols(mean_public ~ T_D, data = filter(data_6.2, Group == "D")) #  0.005 bis 0.25

data_6.2.0 <- data.frame()

colnames_0 <- colnames(select(data_6.2, -c(ID:T_D), -c(C_A:C_D)))

for(var_0 in colnames_0){
  print(var_0)
  # Delete observations first
  
  data_6.2.1 <- data_6.2
  
  if(var_0 == "gnro")                 {data_6.2.1 <- filter(data_6.2.1, gnro != "Other")}
  if(var_0 == "Province")             {data_6.2.1 <- filter(data_6.2.1, Province != "Nuevo departamentos")}
  if(var_0 == "ccnr")                 {data_6.2.1 <- filter(data_6.2.1, ccnr != "Solid fuels")}
  if(var_0 == "cc_info")              {data_6.2.1 <- filter(data_6.2.1, cc_info != "NA")}
  if(var_0 == "cc_preocup")           {data_6.2.1 <- filter(data_6.2.1, cc_preocup != "NA")}
  if(var_0 == "cc_econ")              {data_6.2.1 <- filter(data_6.2.1, cc_econ != "NA")}
  if(var_0 == "cc_imp_co2")           {data_6.2.1 <- filter(data_6.2.1, cc_imp_co2 != "Neutral")}
  if(var_0 == "cc_imp_pers")          {data_6.2.1 <- filter(data_6.2.1, cc_imp_pers != "Neutral")}
  if(var_0 == "cc_imp_equit")         {data_6.2.1 <- filter(data_6.2.1, cc_imp_equit != "Neutral")}
  if(var_0 == "pol_pres")             {data_6.2.1 <- filter(data_6.2.1, pol_pres != "NA")}
  if(var_0 == "izq_der")              {data_6.2.1 <- filter(data_6.2.1, izq_der != "NA" & !is.na(izq_der))}
  if(var_0 == "pais_army")            {data_6.2.1 <- filter(data_6.2.1, pais_army != "Moderate trust")}
  if(var_0 == "pais_police")          {data_6.2.1 <- filter(data_6.2.1, pais_police != "Moderate trust")}
  if(var_0 == "pais_prsdnt")          {data_6.2.1 <- filter(data_6.2.1, pais_prsdnt != "Moderate trust")}
  if(var_0 == "prop_acrd_fepc")       {data_6.2.1 <- filter(data_6.2.1, !is.na(prop_acrd_fepc))}
  if(var_0 == "prop_acrd_paz")        {data_6.2.1 <- filter(data_6.2.1, !is.na(prop_acrd_paz))}
  if(var_0 == "prop_acrd_energ")      {data_6.2.1 <- filter(data_6.2.1, !is.na(prop_acrd_energ))}
  if(var_0 == "pais_gnrl")            {data_6.2.1 <- filter(data_6.2.1, !is.na(pais_gnrl))}
  if(var_0 == "pais_dmcrc")           {data_6.2.1 <- filter(data_6.2.1, pais_dmcrc != "NA" & pais_dmcrc != "Prefer not to say")}
  if(var_0 == "pais_econ")            {data_6.2.1 <- filter(data_6.2.1, pais_econ != "NA" & pais_econ != "Prefer not to say")}
  if(var_0 == "ffsr_gnrl")            {data_6.2.1 <- filter(data_6.2.1, ffsr_gnrl != "I don't know")}
  if(var_0 == "ffsr_dsl")             {data_6.2.1 <- filter(data_6.2.1, ffsr_dsl != "I don't know")}
  if(var_0 == "ffsr_gas")             {data_6.2.1 <- filter(data_6.2.1, ffsr_gas != "I don't know")}
  if(var_0 == "benefic")              {data_6.2.1 <- filter(data_6.2.1, benefic != "NA")}
  if(var_0 == "derecho")              {data_6.2.1 <- filter(data_6.2.1, derecho != "NA")}
  if(var_0 == "yo_amnto")             {data_6.2.1 <- filter(data_6.2.1, yo_amnto != "NA")}
  if(var_0 == "pobre_amnto")          {data_6.2.1 <- filter(data_6.2.1, pobre_amnto != "NA")}
  if(var_0 == "rica_amnto")           {data_6.2.1 <- filter(data_6.2.1, rica_amnto != "NA")}
  
  data_6.2.1 <- data_6.2.1 %>%
    rename(var_interest = any_of(var_0))
  
  # H2.1
  model_H21 <- feols(mean_social ~ T_A, data = filter(data_6.2.1, Group == "A"), split = ~ var_interest)
  # H2.2
  model_H22 <- feols(mean_social ~ T_B, data = filter(data_6.2.1, Group == "B"), split = ~ var_interest)
  # H2.3
  model_H23 <- feols(mean_public ~ T_C, data = filter(data_6.2.1, Group == "C"), split = ~ var_interest)
  # H2.4
  model_H24 <- feols(mean_public ~ T_D, data = filter(data_6.2.1, Group == "D"), split = ~ var_interest)
  
  data_6.2.X <- data.frame()
  
  for(j in c("A", "B", "C", "D")){
    
    if(j == "A"){
      summary_model_H21 <- summary(model_H21)
      
      data_6.2.2 <- data.frame()
      
      for(i in 1:length(summary_model_H21)){
        data_6.2.2.1 <- data.frame(number = i,
                                   name        = names(model_H21)[i],
                                   coefficient = unname(summary(model_H21[[i]])$coefficients["T_A"]),
                                   std_error   = unname(summary(model_H21[[i]])$se["T_A"])) 
        
        data_6.2.2 <- data_6.2.2 %>%
          bind_rows(data_6.2.2.1)
      }
      
      data_6.2.2 <- data_6.2.2 %>%
        mutate(Hypothesis = "H2.1")
      
      data_6.2.X <- data_6.2.X %>%
        bind_rows(data_6.2.2)
    }
    
    if(j == "B"){
      summary_model_H22 <- summary(model_H22)
      
      data_6.2.2 <- data.frame()
      
      for(i in 1:length(summary_model_H22)){
        data_6.2.2.1 <- data.frame(number = i,
                                   name        = names(model_H22)[i],
                                   coefficient = unname(summary(model_H22[[i]])$coefficients["T_B"]),
                                   std_error   = unname(summary(model_H22[[i]])$se["T_B"])) 
        
        data_6.2.2 <- data_6.2.2 %>%
          bind_rows(data_6.2.2.1)
      }
      
      data_6.2.2 <- data_6.2.2 %>%
        mutate(Hypothesis = "H2.2")
      
      data_6.2.X <- data_6.2.X %>%
        bind_rows(data_6.2.2)
    }
    
    if(j == "C"){
      summary_model_H23 <- summary(model_H23)
      
      data_6.2.2 <- data.frame()
      
      for(i in 1:length(summary_model_H23)){
        data_6.2.2.1 <- data.frame(number = i,
                                   name        = names(model_H23)[i],
                                   coefficient = unname(summary(model_H23[[i]])$coefficients["T_C"]),
                                   std_error   = unname(summary(model_H23[[i]])$se["T_C"])) 
        
        data_6.2.2 <- data_6.2.2 %>%
          bind_rows(data_6.2.2.1)
      }
      
      data_6.2.2 <- data_6.2.2 %>%
        mutate(Hypothesis = "H2.3")
      
      data_6.2.X <- data_6.2.X %>%
        bind_rows(data_6.2.2)
    }
    
    if(j == "D"){
      summary_model_H24 <- summary(model_H24)
      
      data_6.2.2 <- data.frame()
      
      for(i in 1:length(summary_model_H24)){
        data_6.2.2.1 <- data.frame(number = i,
                                   name        = names(model_H24)[i],
                                   coefficient = unname(summary(model_H24[[i]])$coefficients["T_D"]),
                                   std_error   = unname(summary(model_H24[[i]])$se["T_D"])) 
        
        data_6.2.2 <- data_6.2.2 %>%
          bind_rows(data_6.2.2.1)
      }
      
      data_6.2.2 <- data_6.2.2 %>%
        mutate(Hypothesis = "H2.4")
      
      data_6.2.X <- data_6.2.X %>%
        bind_rows(data_6.2.2)
    }
    
  }
  
  data_6.2.X <- data_6.2.X %>%
    mutate(Term = var_0)%>%
    select(Term, everything())
  
  data_6.2.0 <- data_6.2.0 %>%
    bind_rows(data_6.2.X)
  
}

rm(summary_model_H21, summary_model_H22, summary_model_H23, summary_model_H24,
   model_H21, model_H22, model_H23, model_H24, data_6.2.X, data_6.2.2, data_6.2.1, data_6.2, data_6.2.2.1, colnames_0, var_0, i, j)

data_6.2.3 <- data_6.2.0 %>%
  mutate(conf_low  = coefficient - 1.96*std_error,
         conf_high = coefficient + 1.96*std_error)%>%
  mutate(Term = factor(Term, levels = c("edad", "gnro", "etnia", "Province", "edu", "ingrso", "grp_ingrso", "moto", "carro", "transpub", "ccnr", "cc_info", "cc_preocup", "cc_econ",
                                        "cc_imp_co2", "cc_imp_pers", "cc_imp_equit", "pol_pres", "izq_der", "prop_acrd_fepc", "prop_acrd_paz", "prop_acrd_energ",
                                        "pais_gnrl", "pais_army", "pais_police", "pais_prsdnt", "pais_dmcrc", "pais_econ",
                                        "ffsr_gnrl", "ffsr_dsl", "ffsr_gas", "benefic", "derecho", "yo_amnto", "pobre_amnto", "rica_amnto")))%>%
  mutate(name = case_when(Term == "edad"         ~ str_replace(name, "^", "Age: "),
                          Term == "gnro"         ~ str_replace(name, "^", "Gender: "),
                          Term == "etnia"        ~ str_replace(name, "^", "Ethnicity: "),
                          Term == "Province"     ~ str_replace(name, "^", "Province: "),
                          Term == "edu"          ~ str_replace(name, "^", "Education: "),
                          Term == "ingrso"       ~ str_replace(name, "^", "Income: "),
                          Term == "grp_ingrso"   ~ str_replace(name, "^", "Perc. income group: "),
                          Term == "moto"         ~ str_replace(name, "^", "Owns a motorcycle: "),
                          Term == "carro"        ~ str_replace(name, "^", "Owns a car: "),
                          Term == "transpub"     ~ str_replace(name, "^", "Uses public transport: "),
                          Term == "ccnr"         ~ str_replace(name, "^", "Cooking fuel: "),
                          Term == "cc_info"      ~ str_replace(name, "^", "Informed about CC: "),
                          Term == "cc_preocup"   ~ str_replace(name, "^", "Worried about CC: "),
                          Term == "cc_econ"      ~ str_replace(name, "^", "CC vs. economy: "),
                          Term == "cc_imp_co2"   ~ str_replace(name, "^", "Climate policy - effective: "),
                          Term == "cc_imp_pers"  ~ str_replace(name, "^", "Climate policy - low costs: "),
                          Term == "cc_imp_equit" ~ str_replace(name, "^", "Climate policy - equity: "),
                          
                          Term == "pol_pres"        ~ str_replace(name, "^", "Voted for president: "),
                          Term == "izq_der"         ~ str_replace(name, "^", "Political position: "),
                          Term == "prop_acrd_fepc"  ~ str_replace(name, "^", "Agree with FFSR: "),
                          Term == "prop_acrd_paz"   ~ str_replace(name, "^", "Agree with paz total: "),
                          Term == "prop_acrd_energ" ~ str_replace(name, "^", "Agree with energy transition: "),
                          Term == "pais_gnrl"       ~ str_replace(name, "^", "Perc. country situation: "),
                          Term == "pais_army"       ~ str_replace(name, "^", "Trust in army: "),
                          Term == "pais_police"     ~ str_replace(name, "^", "Trust in police: "),
                          Term == "pais_prsdnt"     ~ str_replace(name, "^", "Trust in democracy: "),
                          Term == "pais_dmcrc"      ~ str_replace(name, "^", "State of democracy: "),
                          Term == "pais_econ"       ~ str_replace(name, "^", "State of economy: "),
                          Term == "ffsr_gnrl"       ~ str_replace(name, "^", "Thinks: fuels are subsidised: "),
                          Term == "ffsr_dsl"        ~ str_replace(name, "^", "Thinks: diesel is subsidised: "),
                          Term == "ffsr_gas"        ~ str_replace(name, "^", "Thinks: gasoline is subsidised : "),
                          Term == "benefic"         ~ str_replace(name, "^", "Thinks: benefit from subsidy: "),
                          Term == "derecho"         ~ str_replace(name, "^", "Thinks: right for subsidy: "),
                          Term == "yo_amnto"        ~ str_replace(name, "^", "Concern cost on oneself: "),
                          Term == "pobre_amnto"     ~ str_replace(name, "^", "Concern costs on poor: "),
                          Term == "rica_amnto"      ~ str_replace(name, "^", "Concern costs on rich: "),
                          TRUE ~ name))%>%
  mutate(Row = ifelse(Term %in% c("edad", "gnro", "etnia", "Province", "edu", "ingrso", "grp_ingrso", "moto", "carro", "transpub", "ccnr", "cc_info", "cc_preocup", "cc_econ",
                                  "cc_imp_co2", "cc_imp_pers", "cc_imp_equit"),1,0))

data_6.2.3.1 <- data_6.2.3 %>%
  filter(Row == 1)%>%
  mutate(name = factor(name))%>%
  mutate(name = fct_relevel(name, levels(name)[c(1:3,5,4,6:21,23,24,22,25:length(levels(name))) ]))

data_6.2.3.2 <- data_6.2.3 %>%
  filter(Row == 0)%>%
  mutate(name = factor(name))%>%
  mutate(name = fct_relevel(name, levels(name)[c(1:16,18,17,19:length(levels(name))) ]))

functions_labels <- function(x) {
  lapply(x, function(lbl) {
    parts <- strsplit(lbl, ":", fixed = TRUE)[[1]]
    # first part is before first colon, second part is everything else
    first <- parts[1]
    rest <- paste(parts[-1], collapse = ":")
    bquote(bold(.(paste0(first, ":"))) * .(rest))
  })
}

# H2.1
P_6.2.1 <- ggplot(filter(data_6.2.3.1, Hypothesis == "H2.1"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.177, ymax = 0.095), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1.5,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1.5,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))

P_6.2.2 <- ggplot(filter(data_6.2.3.2, Hypothesis == "H2.1"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.177, ymax = 0.095), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1.5,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1.5,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))


jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A_H2.1_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_6.2.1)
print(P_6.2.2)
dev.off()

# H2.2
P_6.2.3 <- ggplot(filter(data_6.2.3.1, Hypothesis == "H2.2"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.181, ymax = 0.081), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-2.5,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-2.5,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))

P_6.2.4 <- ggplot(filter(data_6.2.3.2, Hypothesis == "H2.2"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.181, ymax = 0.081), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1.5,1))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1.5,1,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))


jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A_H2.2_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_6.2.3)
print(P_6.2.4)
dev.off()

# H2.3
P_6.2.5 <- ggplot(filter(data_6.2.3.1, Hypothesis == "H2.3"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.157, ymax = 0.090), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))

P_6.2.6 <- ggplot(filter(data_6.2.3.2, Hypothesis == "H2.3"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = -0.157, ymax = 0.090), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1.5,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1.5,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))


jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A_H2.3_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_6.2.5)
print(P_6.2.6)
dev.off()

# H2.4
P_6.2.7 <- ggplot(filter(data_6.2.3.1, Hypothesis == "H2.4"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.005, ymax = 0.25), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1.5,1.5))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1.5,1.5,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))

P_6.2.8 <- ggplot(filter(data_6.2.3.2, Hypothesis == "H2.4"), aes(x = name))+
  geom_rect(aes(xmin = 0, xmax = Inf, ymin = 0.005, ymax = 0.25), fill = "lightgrey")+
  geom_hline(aes(yintercept = 0), linewidth = 0.3)+
  facet_grid(. ~ Term, scales = "free", space = "free")+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high), linewidth = 0.2, width = 0.7)+
  geom_point(aes(y = coefficient), size = 1, shape = 16)+
  coord_cartesian(ylim = c(-1,1))+
  theme_bw()+
  ylab("Coefficient") +
  xlab("")+
  scale_x_discrete(labels = functions_labels)+
  scale_y_continuous(breaks = c(seq(-1,1,0.5)))+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, size = 5),
        axis.text.y = element_text(size = 6),
        axis.title  = element_text(size = 7),
        strip.text = element_blank(),
        axis.ticks = element_line(linewidth = 0.2),
        axis.title.x = element_blank(),
        panel.grid.major = element_line(linewidth = 0.2),
        panel.spacing = unit(0.1, "lines"))


jpeg("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_A_H2.4_%d.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_6.2.7)
print(P_6.2.8)
dev.off()


rm(P_6.2.1, P_6.2.2, P_6.2.3, P_6.2.4, P_6.2.5, P_6.2.6, P_6.2.7, P_6.2.8, data_6.2.3.1, data_6.2.3.2)

# 7     Robustness checks ####

# 7.1   H1 ####

data_7.1.1 <- data_0 %>%
  select(ID, conditional)%>%
  mutate(conditional_support = 1)%>%
  rename(support = conditional)

data_7.1.2 <- data_0 %>%
  select(ID, ffsr_prcl)%>%
  mutate(conditional_support = 0)%>%
  rename(support = ffsr_prcl)

data_7.1.3 <- data_0 %>%
  select(ID, rr_lmpsm:rr_deforst)%>%
  pivot_longer(-ID, names_to = "rr_", values_to = "not_rounded")%>%
  group_by(ID)%>%
  summarise(support = mean(not_rounded))%>%
  ungroup()%>%
  mutate(Type = "Not rounded")%>%
  mutate(conditional_support = 1)

data_7.1 <- bind_rows(data_7.1.1, data_7.1.2, data_7.1.3)%>%
  mutate(Type = ifelse(is.na(Type), "rounded", Type))%>%
  left_join(select(data_0, -conditional, -ffsr_prcl), by = "ID")

rm(data_7.1.1, data_7.1.2, data_7.1.3)

# Idea is that many specifications are conducted with and without various cleaning steps and with and without various fixed effects

combinations_H1 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H1")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

prepare_specification_chart <- function(data_7.1_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.1.1 <- feols(support ~ conditional_support, data = data_7.1_input)
  # Treatment and group control
  model_7.1.2 <- feols(support ~ conditional_support + Group + treatment, data = data_7.1_input)
  # ID
  model_7.1.3 <- feols(support ~ conditional_support | ID, data = data_7.1_input)
  
  tidy_7.1.1  <- tidy(model_7.1.1)%>%
    filter(term == "conditional_support")%>%
    mutate(FE       = "None")
  
  tidy_7.1.2  <- tidy(model_7.1.2)%>%
    filter(term == "conditional_support")%>%
    mutate(FE       = "Group + treatment")
  
  tidy_7.1.3  <- tidy(model_7.1.3)%>%
    filter(term == "conditional_support")%>%
    mutate(FE       = "ID")
  
  data_7.1_output <- tidy_7.1.1 %>%
    bind_rows(tidy_7.1.2)%>%
    bind_rows(tidy_7.1.3)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.1_output)
  
}

# Baseline

data_7.1_temp <- filter(data_7.1, Type == "rounded" & if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.1.1.0 <- prepare_specification_chart(data_7.1_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.1_temp)

# Various filters

data_7.1.1.1 <- data.frame()

for(i in c(2:20)){
  combinations_H1.1 <- combinations_H1 %>%
    filter(Specification == i)
  
  data_7.1_temp <- filter(data_7.1, Type == "rounded" & if_all(all_of(combinations_H1.1$filter), ~ .x == 0))
  data_7.1.1.1_temp <- prepare_specification_chart(data_7.1_temp,paste(sub("filter_","", combinations_H1.1$filter), collapse = "_"),"Rounded","Standard")
  
  data_7.1.1.1 <- data_7.1.1.1 %>%
    bind_rows(data_7.1.1.1_temp)
}

rm(combinations_H1, combinations_H1.1, data_7.1.1.1_temp)

# Different outcomes 

data_7.1_temp <- data_7.1 %>%
  filter(Type == "rounded")%>%
  mutate(support_2 = ifelse(support %in% c(1,2),0,
                            ifelse(support %in% c(4,5),1, NA)),
         support_3 = ifelse(support %in% c(1,2),1,
                            ifelse(support %in% c(3),2,
                                   ifelse(support %in% c(4,5),3,NA))))

data_7.1_temp_2 <- data_7.1_temp %>%
  mutate(NAs = ifelse(is.na(support_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(support = support_2)

data_7.1.1.2 <- prepare_specification_chart(data_7.1_temp_2,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Binary")

data_7.1_temp_3 <- data_7.1_temp %>%
  mutate(support = support_3)

data_7.1.1.3 <- prepare_specification_chart(data_7.1_temp_3,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Three levels")

# Not rounded

data_7.1_temp_4 <- filter(data_7.1, (Type == "Not rounded" | conditional_support == 0) & if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.1.1.4 <- prepare_specification_chart(data_7.1_temp_4,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")

# No filter

data_7.1_temp_5 <- filter(data_7.1, Type == "rounded")
data_7.1.1.5 <- prepare_specification_chart(data_7.1_temp_5,"No filter","Rounded","Standard")

data_7.1.X <- data_7.1.1.0 %>%
  bind_rows(data_7.1.1.1)%>%
  bind_rows(data_7.1.1.2)%>%
  bind_rows(data_7.1.1.3)%>%
  bind_rows(data_7.1.1.4)%>%
  bind_rows(data_7.1.1.5)

rm(data_7.1.1.0, data_7.1.1.1, data_7.1.1.2, data_7.1.1.3, data_7.1.1.4, data_7.1_temp_2, data_7.1_temp_3, data_7.1_temp, data_7.1_temp_4, data_7.1_temp_5, data_7.1.1.5)
  
data_7.1.0 <- data_7.1.X %>%
  mutate(Type     = ifelse(is.na(Type), "Other", 
                           ifelse(FE == "None", "Baseline", "Other")))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.1.1 <- ggplot(data_7.1.0, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.1.0$seq[data_7.1.0$Type == "Baseline"] - 0.5), xmax = (data_7.1.0$seq[data_7.1.0$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.1,1.4))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(0,0.25,0.5,0.75,1,1.25))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Group + treatment" = 0,
                 "IID" = 0,
                 "None" = 1,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.1.0_cat <- data_7.1.0 %>%
  select(ID, seq, seq_fa, FE, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded             = ifelse(Rounded == "Rounded",1,0),
         "Standard"          = ifelse(Outcome == "Standard",1,0),
         "Binary"            = ifelse(Outcome == "Binary",1,0),
         "Three levels"      = ifelse(Outcome == "Three levels",1,0),
         "IID"                = ifelse(FE == "ID",1,0),
         "None"              = ifelse(FE == "None",1,0),
         "Group + treatment" = ifelse(FE == "Group + treatment",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome, -FE)%>%
  pivot_longer("Rounded":"Group + treatment", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "None", "Group + treatment", "IID")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("None", "Group + treatment", "IID"), "Controls", NA))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Controls")))%>%
  mutate(Value_2 = factor(Value_2))

P_7.1.2 <- ggplot(data = data_7.1.0_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.1.0$seq[data_7.1.0$Type == "Baseline"] - 0.5), xmax = (data_7.1.0$seq[data_7.1.0$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.1 <- ggarrange(P_7.1.1, P_7.1.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H1.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.1)
dev.off()

rm(P_7.1, P_7.1.1, P_7.1.2, interest_df, data_7.1, data_7.1.0, data_7.1.0_cat, data_7.1.X)

# 7.2   H2 ####

# treatment effects on conditional and unconditional support

data_7.2.1 <- data_0 %>%
  mutate(First_Stage = ifelse(Group == 1, "No","Yes"))%>%
  mutate(Group = haven::as_factor(Group))%>%
  mutate(conditional_not_rounded = (rr_lmpsm+rr_pobre+rr_afctds+rr_impuesto+rr_deuda+rr_etransp+rr_paz+rr_edu+rr_ncer+rr_deforst)/10)

prepare_specification_chart <- function(data_7.2_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.2.1.1 <- feols(ffsr_prcl ~ treatment + Group, data = data_7.2_input)
  # First - stage control
  model_7.2.1.2 <- feols(ffsr_prcl ~ treatment + First_Stage, data = data_7.2_input)
  # No control
  model_7.2.1.3 <- feols(ffsr_prcl ~ treatment, data = data_7.2_input)
  
  # Raw
  model_7.2.1.4 <- feols(conditional ~ treatment + Group, data = data_7.2_input)
  # First - stage control
  model_7.2.1.5 <- feols(conditional ~ treatment + First_Stage, data = data_7.2_input)
  # No control
  model_7.2.1.6 <- feols(conditional ~ treatment, data = data_7.2_input)
  
  tidy_7.2.1.1  <- tidy(model_7.2.1.1)%>%
    filter(term == "treatment")%>%
    mutate(FE = "Group")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.2.1.2  <- tidy(model_7.2.1.2)%>%
    filter(term == "treatment")%>%
    mutate(FE = "First_Stage")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.2.1.3  <- tidy(model_7.2.1.3)%>%
    filter(term == "treatment")%>%
    mutate(FE = "None")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.2.1.4  <- tidy(model_7.2.1.4)%>%
    filter(term == "treatment")%>%
    mutate(FE = "Group")%>%
    mutate(Type_1 = "Conditional")
  
  tidy_7.2.1.5  <- tidy(model_7.2.1.5)%>%
    filter(term == "treatment")%>%
    mutate(FE = "First_Stage")%>%
    mutate(Type_1 = "Conditional")
  
  tidy_7.2.1.6  <- tidy(model_7.2.1.6)%>%
    filter(term == "treatment")%>%
    mutate(FE = "None")%>%
    mutate(Type_1 = "Conditional")
  
  data_7.2_output <- tidy_7.2.1.1 %>%
    bind_rows(tidy_7.2.1.2)%>%
    bind_rows(tidy_7.2.1.3)%>%
    bind_rows(tidy_7.2.1.4)%>%
    bind_rows(tidy_7.2.1.5)%>%
    bind_rows(tidy_7.2.1.6)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.2_output)
  
}

# Baseline

data_7.2_temp <- filter(data_7.2.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.2.1.0 <- prepare_specification_chart(data_7.2_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.2_temp)

# Various filtering criteria

combinations_H2 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H2")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.2.1.1 <- data.frame()

for(i in c(2:20)){
  combinations_H2.1 <- combinations_H2 %>%
    filter(Specification == i)
  
  data_7.2_temp <- filter(data_7.2.1, if_all(all_of(combinations_H2.1$filter), ~ .x == 0))
  data_7.2.1.1_temp <- prepare_specification_chart(data_7.2_temp,paste(sub("filter_","", combinations_H2.1$filter), collapse = "_"),"Rounded","Standard")
  
  data_7.2.1.1 <- data_7.2.1.1 %>%
    bind_rows(data_7.2.1.1_temp)
}

rm(combinations_H2, combinations_H2.1, data_7.2.1.1_temp)

# Standard, binary, Three levels
data_7.2.1_temp <- data_7.2.1 %>%
  mutate(ffsr_prcl_2 = ifelse(ffsr_prcl %in% c(1,2),0,
                            ifelse(ffsr_prcl %in% c(4,5),1, NA)),
         ffsr_prcl_3 = ifelse(ffsr_prcl %in% c(1,2),1,
                            ifelse(ffsr_prcl %in% c(3),2,
                                   ifelse(ffsr_prcl %in% c(4,5),3,NA))))

data_7.2.1_temp_2 <- data_7.2.1_temp %>%
  mutate(NAs = ifelse(is.na(ffsr_prcl_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(ffsr_prcl = ffsr_prcl_2)

data_7.2.1.2 <- prepare_specification_chart(data_7.2.1_temp_2,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Binary")

data_7.2.1_temp_3 <- data_7.2.1_temp %>%
  mutate(ffsr_prcl = ffsr_prcl_3)

data_7.2.1.3 <- prepare_specification_chart(data_7.2.1_temp_3,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Three levels")

# No filter
data_7.2.1.4 <- prepare_specification_chart(data_7.2.1,"No filter","Rounded","Standard")

# Not rounded
data_7.2.1_temp_5 <- filter(data_7.2.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-conditional)%>%
  rename(conditional = conditional_not_rounded)
data_7.2.1.5 <- prepare_specification_chart(data_7.2.1_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")

data_7.2.X <- data_7.2.1.0 %>%
  bind_rows(data_7.2.1.1)%>%
  bind_rows(data_7.2.1.2)%>%
  bind_rows(data_7.2.1.3)%>%
  bind_rows(data_7.2.1.4)%>%
  bind_rows(data_7.2.1.5)

rm(data_7.2.1.0, data_7.2.1.1, data_7.2.1.2, data_7.2.1.3, data_7.2.1.4, data_7.2.1.5, data_7.2.1_temp, data_7.2.1_temp_2, data_7.2.1_temp_3, data_7.2.1_temp_5)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type     = ifelse(is.na(Type), "Other", 
                           ifelse(FE == "Group", "Baseline", "Other")))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)

data_7.2.0.1 <- data_7.2.0 %>%
  filter(Type_1 == "Unconditional")%>%
  filter(Rounded != "Not rounded")%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")
  
P_7.2.1 <- ggplot(data_7.2.0.1, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.17,0.17))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Group" = 1,
                 "First-Stage" = 0,
                 "None" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0.1_cat <- data_7.2.0.1 %>%
  select(ID, seq, seq_fa, FE, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded             = ifelse(Rounded == "Rounded",1,0),
         "Standard"          = ifelse(Outcome == "Standard",1,0),
         "Binary"            = ifelse(Outcome == "Binary",1,0),
         "Three levels"      = ifelse(Outcome == "Three levels",1,0),
         "First-Stage"       = ifelse(FE == "First_Stage",1,0),
         "None"              = ifelse(FE == "None",1,0),
         "Group"             = ifelse(FE == "Group",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome, -FE)%>%
  pivot_longer("Rounded":"Group", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "None", "Group", "First-Stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("None", "Group", "First-Stage"), "Controls", NA))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Controls")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0.1_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2_Unconditional.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

data_7.2.0.2 <- data_7.2.0 %>%
  filter(Type_1 == "Conditional")%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.3 <- ggplot(data_7.2.0.2, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0.2$seq[data_7.2.0.2$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.2$seq[data_7.2.0.2$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.17,0.17))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.15,-0.1,-0.05,0,0.05,0.1,0.15))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Group" = 1,
                 "First-Stage" = 0,
                 "None" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0.2_cat <- data_7.2.0.2 %>%
  select(ID, seq, seq_fa, FE, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded             = ifelse(Rounded == "Rounded",1,0),
         "Standard"          = ifelse(Outcome == "Standard",1,0),
         "Binary"            = ifelse(Outcome == "Binary",1,0),
         "Three levels"      = ifelse(Outcome == "Three levels",1,0),
         "First-Stage"       = ifelse(FE == "First_Stage",1,0),
         "None"              = ifelse(FE == "None",1,0),
         "Group"             = ifelse(FE == "Group",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome, -FE)%>%
  pivot_longer("Rounded":"Group", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "None", "Group", "First-Stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("None", "Group", "First-Stage"), "Controls", NA))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Controls")))%>%
  mutate(Value_2 = factor(Value_2))

P_7.2.4 <- ggplot(data = data_7.2.0.2_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0.2$seq[data_7.2.0.2$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.2$seq[data_7.2.0.2$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.3, P_7.2.4, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2_Conditional.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

# 7.2.1 H2.1 ####

data_7.2.1 <- data_1 %>%
  mutate(mean_social         = (rr_lmpsm + rr_pobre + rr_afctds + rr_impuesto + rr_deuda)/5,
         mean_public         = (rr_etransp + rr_paz + rr_edu + rr_ncer + rr_deforst)/5)%>%
  mutate(mean_social_rounded = round(mean_social),
         mean_public_rounded = round(mean_public))%>%
  mutate(frst_a              = ifelse(is.na(frst_a),0,frst_a))%>%
  mutate(first_stage_correct = ifelse(frst_a == 1 | frst_b == 1 | frst_c == 1 | frst_d == 1,1,0))%>%
  mutate(Group = haven::as_factor(Group))

prepare_specification_chart <- function(data_7.2_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.2.1.1 <- feols(mean_social ~ T_A, data = filter(data_7.2_input, Group == "A"))
  # Correct first-stage
  model_7.2.1.2 <- feols(mean_social ~ T_A, data = filter(data_7.2_input, Group == "A" & (first_stage_correct == 1 | treatment == 0)))
  
  tidy_7.2.1.1  <- tidy(model_7.2.1.1)%>%
    filter(term == "T_A")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "All")
  
  tidy_7.2.1.2  <- tidy(model_7.2.1.2)%>%
    filter(term == "T_A")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "Correct")
  
  data_7.2_output <- tidy_7.2.1.1 %>%
    bind_rows(tidy_7.2.1.2)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.2_output)
  
}

# Baseline

data_7.2_temp <- filter(data_7.2.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.2.1.0 <- prepare_specification_chart(data_7.2_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.2_temp)

# Various filtering criteria

combinations_H2.1 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H2.1")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.2.1.1 <- data.frame()

for(i in c(2:20)){
  combinations_H2.1.1 <- combinations_H2.1 %>%
    filter(Specification == i)
  
  data_7.2_temp <- filter(data_7.2.1, if_all(all_of(combinations_H2.1.1$filter), ~ .x == 0))
  data_7.2.1.1_temp <- prepare_specification_chart(data_7.2_temp,paste(sub("filter_","", combinations_H2.1.1$filter), collapse = "_"),"Not rounded","Standard")
  
  data_7.2.1.1 <- data_7.2.1.1 %>%
    bind_rows(data_7.2.1.1_temp)
}

rm(combinations_H2.1, combinations_H2.1.1, data_7.2.1.1_temp)

# No filter
data_7.2.1.2 <- prepare_specification_chart(data_7.2.1,"No filter","Not rounded","Standard")

# Rounded
data_7.2.1_temp_1 <- filter(data_7.2.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-mean_social)%>%
  rename(mean_social = mean_social_rounded)
data_7.2.1.3 <- prepare_specification_chart(data_7.2.1_temp_1,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")

rm(data_7.2.1_temp_1)

# Include control group

data_7.2.1_temp_2 <- filter(data_7.2.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  # filter(Group == "A" | Group == "B" | Group == "Control")%>%
  mutate(Control = as.character(Group))

# Raw
# Controlling for treatment is not necessary because the control group is by definition zero for the treatment
model_7.2.1.4a <- feols(mean_social ~ Control + T_A + T_B + T_C + T_D, data = data_7.2.1_temp_2)
# Correct first-stage
model_7.2.1.4b <- feols(mean_social ~ Control + T_A + T_B + T_C + T_D, data = filter(data_7.2.1_temp_2, (T_A == 0 | frst_a == 1)))

tidy_7.2.1.4a  <- tidy(model_7.2.1.4a)%>%
  filter(term == "T_A")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "All")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

tidy_7.2.1.4b  <- tidy(model_7.2.1.4b)%>%
  filter(term == "T_A")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "Correct")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

rm(data_7.2.1_temp_2, model_7.2.1.4a, model_7.2.1.4b)

# Multiple levels: Standard, binary, Three levels
data_7.2.1_temp_3 <- data_7.2.1 %>%
  mutate(mean_social_2 = ifelse(mean_social <= 2,0,
                              ifelse(mean_social >= 4,1, NA)),
         mean_social_3 = ifelse(mean_social <= 2.5,1,
                              ifelse(mean_social > 2.5 & mean_social <= 3.5,2,
                                     ifelse(mean_social > 3.5,3,NA))))

data_7.2.1_temp_4 <- data_7.2.1_temp_3 %>%
  mutate(NAs = ifelse(is.na(mean_social_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(mean_social = mean_social_2)

data_7.2.1.5 <- prepare_specification_chart(data_7.2.1_temp_4,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Binary")

data_7.2.1_temp_5 <- data_7.2.1_temp_3 %>%
  mutate(mean_social = mean_social_3)

data_7.2.1.6 <- prepare_specification_chart(data_7.2.1_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Three levels")

rm(data_7.2.1_temp_3, data_7.2.1_temp_4, data_7.2.1_temp_5)

data_7.2.X <- data_7.2.1.0 %>%
  bind_rows(data_7.2.1.1)%>%
  bind_rows(data_7.2.1.2)%>%
  bind_rows(data_7.2.1.3)%>%
  bind_rows(tidy_7.2.1.4a)%>%
  bind_rows(tidy_7.2.1.4b)%>%
  bind_rows(data_7.2.1.5)%>%
  bind_rows(data_7.2.1.6)

rm(data_7.2.1.0, data_7.2.1.1, data_7.2.1.2, data_7.2.1.3, data_7.2.1.5, data_7.2.1.6, tidy_7.2.1.4a, tidy_7.2.1.4b)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type = ifelse(is.na(Type), "Other", Type))%>%
  mutate(Type = ifelse(Type == "Baseline" & First_Stage == "All", Type, "Other"))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.1 <- ggplot(data_7.2.0, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.35,0.35))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0,
                 "All groups"                       = 0,
                 "Subsample"                        = 1,
                 "All observations"                 = 1,
                 "Correct first stage"              = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0_cat <- data_7.2.0 %>%
  select(ID, seq, seq_fa, Control_Group, First_Stage, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded               = ifelse(Rounded == "Rounded",1,0),
         "Standard"            = ifelse(Outcome == "Standard",1,0),
         "Binary"              = ifelse(Outcome == "Binary",1,0),
         "Three levels"        = ifelse(Outcome == "Three levels",1,0),
         "All groups"          = ifelse(Control_Group == "All",1,0),
         "Subsample"           = ifelse(Control_Group == "Same first-stage",1,0),
         "All observations"    = ifelse(First_Stage == "All",1,0),
         "Correct first stage" = ifelse(First_Stage == "Correct",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome)%>%
  pivot_longer("Rounded":"Correct first stage", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "All groups", "Subsample",
                                                "All observations", "Correct first stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("All groups", "Subsample"), "Sample", 
                                      ifelse(Variable %in% c("All observations", "Correct first stage"), "1st stage", NA)))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Sample", "1st stage")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2.1.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

# 7.2.2 H2.2 ####

data_7.2.2 <- data_1 %>%
  mutate(mean_social         = (rr_lmpsm + rr_pobre + rr_afctds + rr_impuesto + rr_deuda)/5,
         mean_public         = (rr_etransp + rr_paz + rr_edu + rr_ncer + rr_deforst)/5)%>%
  mutate(mean_social_rounded = round(mean_social),
         mean_public_rounded = round(mean_public))%>%
  mutate(frst_a              = ifelse(is.na(frst_a),0,frst_a))%>%
  mutate(first_stage_correct = ifelse(frst_a == 1 | frst_b == 1 | frst_c == 1 | frst_d == 1,1,0))%>%
  mutate(Group = haven::as_factor(Group))

prepare_specification_chart <- function(data_7.2_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.2.2.1 <- feols(mean_social ~ T_B, data = filter(data_7.2_input, Group == "B"))
  # Correct first-stage
  model_7.2.2.2 <- feols(mean_social ~ T_B, data = filter(data_7.2_input, Group == "B" & (first_stage_correct == 1 | treatment == 0)))
  
  tidy_7.2.2.1  <- tidy(model_7.2.2.1)%>%
    filter(term == "T_B")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "All")
  
  tidy_7.2.2.2  <- tidy(model_7.2.2.2)%>%
    filter(term == "T_B")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "Correct")
  
  data_7.2_output <- tidy_7.2.2.1 %>%
    bind_rows(tidy_7.2.2.2)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.2_output)
  
}

# Baseline

data_7.2_temp <- filter(data_7.2.2, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.2.2.0 <- prepare_specification_chart(data_7.2_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.2_temp)

# Various filtering criteria

combinations_H2.2 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H2.2")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.2.2.1 <- data.frame()

for(i in c(2:20)){
  combinations_H2.2.1 <- combinations_H2.2 %>%
    filter(Specification == i)
  
  data_7.2_temp <- filter(data_7.2.2, if_all(all_of(combinations_H2.2.1$filter), ~ .x == 0))
  data_7.2.2.1_temp <- prepare_specification_chart(data_7.2_temp,paste(sub("filter_","", combinations_H2.2.1$filter), collapse = "_"),"Not rounded","Standard")
  
  data_7.2.2.1 <- data_7.2.2.1 %>%
    bind_rows(data_7.2.2.1_temp)
}

rm(combinations_H2.2, combinations_H2.2.1, data_7.2.2.1_temp)

# No filter
data_7.2.2.2 <- prepare_specification_chart(data_7.2.2,"No filter","Not rounded","Standard")

# Rounded
data_7.2.2_temp_1 <- filter(data_7.2.2, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-mean_social)%>%
  rename(mean_social = mean_social_rounded)
data_7.2.2.3 <- prepare_specification_chart(data_7.2.2_temp_1,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")

rm(data_7.2.2_temp_1)

# Include control group

data_7.2.2_temp_2 <- filter(data_7.2.2, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  mutate(Control = as.character(Group))

# Raw
# Controlling for treatment is not necessary because the control group is by definition zero for the treatment
model_7.2.2.4a <- feols(mean_social ~ Control + T_A + T_B + T_C + T_D, data = data_7.2.2_temp_2)
# Correct first-stage
model_7.2.2.4b <- feols(mean_social ~ Control + T_A + T_B + T_C + T_D, data = filter(data_7.2.2_temp_2, (T_B == 0 | frst_b == 1)))

tidy_7.2.2.4a  <- tidy(model_7.2.2.4a)%>%
  filter(term == "T_B")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "All")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

tidy_7.2.2.4b  <- tidy(model_7.2.2.4b)%>%
  filter(term == "T_B")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "Correct")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

rm(data_7.2.2_temp_2, model_7.2.2.4a, model_7.2.2.4b)

# Multiple levels: Standard, binary, Three levels
data_7.2.2_temp_3 <- data_7.2.2 %>%
  mutate(mean_social_2 = ifelse(mean_social <= 2,0,
                                ifelse(mean_social >= 4,1, NA)),
         mean_social_3 = ifelse(mean_social <= 2.5,1,
                                ifelse(mean_social > 2.5 & mean_social <= 3.5,2,
                                       ifelse(mean_social > 3.5,3,NA))))

data_7.2.2_temp_4 <- data_7.2.2_temp_3 %>%
  mutate(NAs = ifelse(is.na(mean_social_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(mean_social = mean_social_2)

data_7.2.2.5 <- prepare_specification_chart(data_7.2.2_temp_4,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Binary")

data_7.2.2_temp_5 <- data_7.2.2_temp_3 %>%
  mutate(mean_social = mean_social_3)

data_7.2.2.6 <- prepare_specification_chart(data_7.2.2_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Three levels")

rm(data_7.2.2_temp_3, data_7.2.2_temp_4, data_7.2.2_temp_5)

data_7.2.X <- data_7.2.2.0 %>%
  bind_rows(data_7.2.2.1)%>%
  bind_rows(data_7.2.2.2)%>%
  bind_rows(data_7.2.2.3)%>%
  bind_rows(tidy_7.2.2.4a)%>%
  bind_rows(tidy_7.2.2.4b)%>%
  bind_rows(data_7.2.2.5)%>%
  bind_rows(data_7.2.2.6)

rm(data_7.2.2.0, data_7.2.2.1, data_7.2.2.2, data_7.2.2.3, data_7.2.2.5, data_7.2.2.6, tidy_7.2.2.4a, tidy_7.2.2.4b)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type = ifelse(is.na(Type), "Other", Type))%>%
  mutate(Type = ifelse(Type == "Baseline" & First_Stage == "All", Type, "Other"))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.1 <- ggplot(data_7.2.0, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.35,0.35))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.3,-0.2,-0.1,0,0.1,0.2,0.3))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0,
                 "All groups"                       = 0,
                 "Subsample"                        = 1,
                 "All observations"                 = 1,
                 "Correct first stage"              = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0_cat <- data_7.2.0 %>%
  select(ID, seq, seq_fa, Control_Group, First_Stage, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded               = ifelse(Rounded == "Rounded",1,0),
         "Standard"            = ifelse(Outcome == "Standard",1,0),
         "Binary"              = ifelse(Outcome == "Binary",1,0),
         "Three levels"        = ifelse(Outcome == "Three levels",1,0),
         "All groups"          = ifelse(Control_Group == "All",1,0),
         "Subsample"           = ifelse(Control_Group == "Same first-stage",1,0),
         "All observations"    = ifelse(First_Stage == "All",1,0),
         "Correct first stage" = ifelse(First_Stage == "Correct",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome)%>%
  pivot_longer("Rounded":"Correct first stage", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "All groups", "Subsample",
                                                "All observations", "Correct first stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("All groups", "Subsample"), "Sample", 
                                      ifelse(Variable %in% c("All observations", "Correct first stage"), "1st stage", NA)))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Sample", "1st stage")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2.2.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

# 7.2.3 H2.3 ####

data_7.2.4 <- data_1 %>%
  mutate(mean_social         = (rr_lmpsm + rr_pobre + rr_afctds + rr_impuesto + rr_deuda)/5,
         mean_public         = (rr_etransp + rr_paz + rr_edu + rr_ncer + rr_deforst)/5)%>%
  mutate(mean_social_rounded = round(mean_social),
         mean_public_rounded = round(mean_public))%>%
  mutate(frst_a              = ifelse(is.na(frst_a),0,frst_a))%>%
  mutate(first_stage_correct = ifelse(frst_a == 1 | frst_b == 1 | frst_c == 1 | frst_d == 1,1,0))%>%
  mutate(Group = haven::as_factor(Group))

prepare_specification_chart <- function(data_7.2_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.2.4.1 <- feols(mean_public ~ T_D, data = filter(data_7.2_input, Group == "D"))
  # Correct first-stage
  model_7.2.4.2 <- feols(mean_public ~ T_D, data = filter(data_7.2_input, Group == "D" & (first_stage_correct == 1 | treatment == 0)))
  
  tidy_7.2.4.1  <- tidy(model_7.2.4.1)%>%
    filter(term == "T_D")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "All")
  
  tidy_7.2.4.2  <- tidy(model_7.2.4.2)%>%
    filter(term == "T_D")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "Correct")
  
  data_7.2_output <- tidy_7.2.4.1 %>%
    bind_rows(tidy_7.2.4.2)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.2_output)
  
}

# Baseline

data_7.2_temp <- filter(data_7.2.4, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.2.4.0 <- prepare_specification_chart(data_7.2_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.2_temp)

# Various filtering criteria

combinations_H2.4 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H2.4")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.2.4.1 <- data.frame()

for(i in c(2:20)){
  combinations_H2.4.1 <- combinations_H2.4 %>%
    filter(Specification == i)
  
  data_7.2_temp <- filter(data_7.2.4, if_all(all_of(combinations_H2.4.1$filter), ~ .x == 0))
  data_7.2.4.1_temp <- prepare_specification_chart(data_7.2_temp,paste(sub("filter_","", combinations_H2.4.1$filter), collapse = "_"),"Not rounded","Standard")
  
  data_7.2.4.1 <- data_7.2.4.1 %>%
    bind_rows(data_7.2.4.1_temp)
}

rm(combinations_H2.4, combinations_H2.4.1, data_7.2.4.1_temp)

# No filter
data_7.2.4.2 <- prepare_specification_chart(data_7.2.4,"No filter","Not rounded","Standard")

# Rounded
data_7.2.4_temp_1 <- filter(data_7.2.4, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-mean_public)%>%
  rename(mean_public = mean_public_rounded)
data_7.2.4.3 <- prepare_specification_chart(data_7.2.4_temp_1,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")

rm(data_7.2.4_temp_1)

# Include control group

data_7.2.4_temp_2 <- filter(data_7.2.4, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  # filter(Group == "A" | Group == "B" | Group == "Control")%>%
  mutate(Control = as.character(Group))

# Raw
# Controlling for treatment is not necessary because the control group is by definition zero for the treatment
model_7.2.4.4a <- feols(mean_public ~ Control + T_A + T_B + T_C + T_D, data = data_7.2.4_temp_2)
# Correct first-stage
model_7.2.4.4b <- feols(mean_public ~ Control + T_A + T_B + T_C + T_D, data = filter(data_7.2.4_temp_2, (T_D == 0 | frst_d == 1)))

tidy_7.2.4.4a  <- tidy(model_7.2.4.4a)%>%
  filter(term == "T_D")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "All")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

tidy_7.2.4.4b  <- tidy(model_7.2.4.4b)%>%
  filter(term == "T_D")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "Correct")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

rm(data_7.2.4_temp_2, model_7.2.4.4a, model_7.2.4.4b)

# Multiple levels: Standard, binary, Three levels
data_7.2.4_temp_3 <- data_7.2.4 %>%
  mutate(mean_public_2 = ifelse(mean_public <= 2,0,
                                ifelse(mean_public >= 4,1, NA)),
         mean_public_3 = ifelse(mean_public <= 2.5,1,
                                ifelse(mean_public > 2.5 & mean_public <= 3.5,2,
                                       ifelse(mean_public > 3.5,3,NA))))

data_7.2.4_temp_4 <- data_7.2.4_temp_3 %>%
  mutate(NAs = ifelse(is.na(mean_public_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(mean_public = mean_public_2)

data_7.2.4.5 <- prepare_specification_chart(data_7.2.4_temp_4,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Binary")

data_7.2.4_temp_5 <- data_7.2.4_temp_3 %>%
  mutate(mean_public = mean_public_3)

data_7.2.4.6 <- prepare_specification_chart(data_7.2.4_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Three levels")

rm(data_7.2.4_temp_3, data_7.2.4_temp_4, data_7.2.4_temp_5)

data_7.2.X <- data_7.2.4.0 %>%
  bind_rows(data_7.2.4.1)%>%
  bind_rows(data_7.2.4.2)%>%
  bind_rows(data_7.2.4.3)%>%
  bind_rows(tidy_7.2.4.4a)%>%
  bind_rows(tidy_7.2.4.4b)%>%
  bind_rows(data_7.2.4.5)%>%
  bind_rows(data_7.2.4.6)

rm(data_7.2.4.0, data_7.2.4.1, data_7.2.4.2, data_7.2.4.3, data_7.2.4.5, data_7.2.4.6, tidy_7.2.4.4a, tidy_7.2.4.4b)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type = ifelse(is.na(Type), "Other", Type))%>%
  mutate(Type = ifelse(Type == "Baseline" & First_Stage == "All", Type, "Other"))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.1 <- ggplot(data_7.2.0, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.15,0.65))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0,
                 "All groups"                       = 0,
                 "Subsample"                        = 1,
                 "All observations"                 = 1,
                 "Correct first stage"              = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0_cat <- data_7.2.0 %>%
  select(ID, seq, seq_fa, Control_Group, First_Stage, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded               = ifelse(Rounded == "Rounded",1,0),
         "Standard"            = ifelse(Outcome == "Standard",1,0),
         "Binary"              = ifelse(Outcome == "Binary",1,0),
         "Three levels"        = ifelse(Outcome == "Three levels",1,0),
         "All groups"          = ifelse(Control_Group == "All",1,0),
         "Subsample"           = ifelse(Control_Group == "Same first-stage",1,0),
         "All observations"    = ifelse(First_Stage == "All",1,0),
         "Correct first stage" = ifelse(First_Stage == "Correct",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome)%>%
  pivot_longer("Rounded":"Correct first stage", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "All groups", "Subsample",
                                                "All observations", "Correct first stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("All groups", "Subsample"), "Sample", 
                                      ifelse(Variable %in% c("All observations", "Correct first stage"), "1st stage", NA)))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Sample", "1st stage")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2.4.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

# 7.2.4 H2.4 ####

data_7.2.3 <- data_1 %>%
  mutate(mean_social         = (rr_lmpsm + rr_pobre + rr_afctds + rr_impuesto + rr_deuda)/5,
         mean_public         = (rr_etransp + rr_paz + rr_edu + rr_ncer + rr_deforst)/5)%>%
  mutate(mean_social_rounded = round(mean_social),
         mean_public_rounded = round(mean_public))%>%
  mutate(frst_a              = ifelse(is.na(frst_a),0,frst_a))%>%
  mutate(first_stage_correct = ifelse(frst_a == 1 | frst_b == 1 | frst_c == 1 | frst_d == 1,1,0))%>%
  mutate(Group = haven::as_factor(Group))

prepare_specification_chart <- function(data_7.2_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.2.3.1 <- feols(mean_public ~ T_C, data = filter(data_7.2_input, Group == "C"))
  # Correct first-stage
  model_7.2.3.2 <- feols(mean_public ~ T_C, data = filter(data_7.2_input, Group == "C" & (first_stage_correct == 1 | treatment == 0)))
  
  tidy_7.2.3.1  <- tidy(model_7.2.3.1)%>%
    filter(term == "T_C")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "All")
  
  tidy_7.2.3.2  <- tidy(model_7.2.3.2)%>%
    filter(term == "T_C")%>%
    mutate(Control_Group   = "Same first-stage")%>%
    mutate(First_Stage     = "Correct")
  
  data_7.2_output <- tidy_7.2.3.1 %>%
    bind_rows(tidy_7.2.3.2)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.2_output)
  
}

# Baseline

data_7.2_temp <- filter(data_7.2.3, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.2.3.0 <- prepare_specification_chart(data_7.2_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.2_temp)

# Various filtering criteria

combinations_H2.3 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H2.3")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.2.3.1 <- data.frame()

for(i in c(2:20)){
  combinations_H2.3.1 <- combinations_H2.3 %>%
    filter(Specification == i)
  
  data_7.2_temp <- filter(data_7.2.3, if_all(all_of(combinations_H2.3.1$filter), ~ .x == 0))
  data_7.2.3.1_temp <- prepare_specification_chart(data_7.2_temp,paste(sub("filter_","", combinations_H2.3.1$filter), collapse = "_"),"Not rounded","Standard")
  
  data_7.2.3.1 <- data_7.2.3.1 %>%
    bind_rows(data_7.2.3.1_temp)
}

rm(combinations_H2.3, combinations_H2.3.1, data_7.2.3.1_temp)

# No filter
data_7.2.3.2 <- prepare_specification_chart(data_7.2.3,"No filter","Not rounded","Standard")

# Rounded
data_7.2.3_temp_1 <- filter(data_7.2.3, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-mean_public)%>%
  rename(mean_public = mean_public_rounded)
data_7.2.3.3 <- prepare_specification_chart(data_7.2.3_temp_1,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")

rm(data_7.2.3_temp_1)

# Include control group

data_7.2.3_temp_2 <- filter(data_7.2.3, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  # filter(Group == "A" | Group == "B" | Group == "Control")%>%
  mutate(Control = as.character(Group))

# Raw
# Controlling for treatment is not necessary because the control group is by definition zero for the treatment
model_7.2.3.4a <- feols(mean_public ~ Control + T_A + T_B + T_C + T_D, data = data_7.2.3_temp_2)
# Correct first-stage
model_7.2.3.4b <- feols(mean_public ~ Control + T_A + T_B + T_C + T_D, data = filter(data_7.2.3_temp_2, (T_C == 0 | frst_c == 1)))

tidy_7.2.3.4a  <- tidy(model_7.2.3.4a)%>%
  filter(term == "T_C")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "All")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

tidy_7.2.3.4b  <- tidy(model_7.2.3.4b)%>%
  filter(term == "T_C")%>%
  mutate(Control_Group   = "All")%>%
  mutate(First_Stage     = "Correct")%>%
  mutate(Filter  = "1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x",
         Rounded = "Not rounded",
         Outcome = "Standard")

rm(data_7.2.3_temp_2, model_7.2.3.4a, model_7.2.3.4b)

# Multiple levels: Standard, binary, Three levels
data_7.2.3_temp_3 <- data_7.2.3 %>%
  mutate(mean_public_2 = ifelse(mean_public <= 2,0,
                                ifelse(mean_public >= 4,1, NA)),
         mean_public_3 = ifelse(mean_public <= 2.5,1,
                                ifelse(mean_public > 2.5 & mean_public <= 3.5,2,
                                       ifelse(mean_public > 3.5,3,NA))))

data_7.2.3_temp_4 <- data_7.2.3_temp_3 %>%
  mutate(NAs = ifelse(is.na(mean_public_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(mean_public = mean_public_2)

data_7.2.3.5 <- prepare_specification_chart(data_7.2.3_temp_4,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Binary")

data_7.2.3_temp_5 <- data_7.2.3_temp_3 %>%
  mutate(mean_public = mean_public_3)

data_7.2.3.6 <- prepare_specification_chart(data_7.2.3_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Three levels")

rm(data_7.2.3_temp_3, data_7.2.3_temp_4, data_7.2.3_temp_5)

data_7.2.X <- data_7.2.3.0 %>%
  bind_rows(data_7.2.3.1)%>%
  bind_rows(data_7.2.3.2)%>%
  bind_rows(data_7.2.3.3)%>%
  bind_rows(tidy_7.2.3.4a)%>%
  bind_rows(tidy_7.2.3.4b)%>%
  bind_rows(data_7.2.3.5)%>%
  bind_rows(data_7.2.3.6)

rm(data_7.2.3.0, data_7.2.3.1, data_7.2.3.2, data_7.2.3.3, data_7.2.3.5, data_7.2.3.6, tidy_7.2.3.4a, tidy_7.2.3.4b)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type = ifelse(is.na(Type), "Other", Type))%>%
  mutate(Type = ifelse(Type == "Baseline" & First_Stage == "All", Type, "Other"))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.1 <- ggplot(data_7.2.0, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.25,0.45))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.2,-0.1,0,0.1,0.2,0.3,0.4))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0,
                 "All groups"                       = 0,
                 "Subsample"                        = 1,
                 "All observations"                 = 1,
                 "Correct first stage"              = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0_cat <- data_7.2.0 %>%
  select(ID, seq, seq_fa, Control_Group, First_Stage, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded               = ifelse(Rounded == "Rounded",1,0),
         "Standard"            = ifelse(Outcome == "Standard",1,0),
         "Binary"              = ifelse(Outcome == "Binary",1,0),
         "Three levels"        = ifelse(Outcome == "Three levels",1,0),
         "All groups"          = ifelse(Control_Group == "All",1,0),
         "Subsample"           = ifelse(Control_Group == "Same first-stage",1,0),
         "All observations"    = ifelse(First_Stage == "All",1,0),
         "Correct first stage" = ifelse(First_Stage == "Correct",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome)%>%
  pivot_longer("Rounded":"Correct first stage", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "All groups", "Subsample",
                                                "All observations", "Correct first stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("All groups", "Subsample"), "Sample", 
                                      ifelse(Variable %in% c("All observations", "Correct first stage"), "1st stage", NA)))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Sample", "1st stage")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] - 0.5), xmax = (data_7.2.0$seq[data_7.2.0$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H2.3.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()


# 7.3   H3 ####



data_7.3.1 <- data_0 %>%
  mutate(First_Stage = ifelse(Group == 1, "No","Yes"))%>%
  mutate(Group = haven::as_factor(Group))%>%
  mutate(conditional_not_rounded = (rr_lmpsm+rr_pobre+rr_afctds+rr_impuesto+rr_deuda+rr_etransp+rr_paz+rr_edu+rr_ncer+rr_deforst)/10)%>%
  # Filter for those supporting Rodolfo H
  filter(pol_pres == 2)

prepare_specification_chart <- function(data_7.3_input, filter_0, rounded_0, outcome_0){
  
  # Raw
  model_7.3.1.1 <- feols(ffsr_prcl ~ treatment + Group, data = data_7.3_input)
  # First - stage control
  model_7.3.1.2 <- feols(ffsr_prcl ~ treatment + First_Stage, data = data_7.3_input)
  # No control
  model_7.3.1.3 <- feols(ffsr_prcl ~ treatment, data = data_7.3_input)
  
  # Raw
  model_7.3.1.4 <- feols(conditional ~ treatment + Group, data = data_7.3_input)
  # First - stage control
  model_7.3.1.5 <- feols(conditional ~ treatment + First_Stage, data = data_7.3_input)
  # No control
  model_7.3.1.6 <- feols(conditional ~ treatment, data = data_7.3_input)
  
  tidy_7.3.1.1  <- tidy(model_7.3.1.1)%>%
    filter(term == "treatment")%>%
    mutate(FE = "Group")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.3.1.2  <- tidy(model_7.3.1.2)%>%
    filter(term == "treatment")%>%
    mutate(FE = "First_Stage")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.3.1.3  <- tidy(model_7.3.1.3)%>%
    filter(term == "treatment")%>%
    mutate(FE = "None")%>%
    mutate(Type_1 = "Unconditional")
  
  tidy_7.3.1.4  <- tidy(model_7.3.1.4)%>%
    filter(term == "treatment")%>%
    mutate(FE = "Group")%>%
    mutate(Type_1 = "Conditional")
  
  tidy_7.3.1.5  <- tidy(model_7.3.1.5)%>%
    filter(term == "treatment")%>%
    mutate(FE = "First_Stage")%>%
    mutate(Type_1 = "Conditional")
  
  tidy_7.3.1.6  <- tidy(model_7.3.1.6)%>%
    filter(term == "treatment")%>%
    mutate(FE = "None")%>%
    mutate(Type_1 = "Conditional")
  
  data_7.3_output <- tidy_7.3.1.1 %>%
    bind_rows(tidy_7.3.1.2)%>%
    bind_rows(tidy_7.3.1.3)%>%
    bind_rows(tidy_7.3.1.4)%>%
    bind_rows(tidy_7.3.1.5)%>%
    bind_rows(tidy_7.3.1.6)%>%
    mutate(Filter  = filter_0,
           Rounded = rounded_0,
           Outcome = outcome_0)
  
  return(data_7.3_output)
  
}

# Baseline

data_7.3_temp <- filter(data_7.3.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))
data_7.3.1.0 <- prepare_specification_chart(data_7.3_temp,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Standard")%>%
  mutate(Type = "Baseline")

rm(data_7.3_temp)

# Various filtering criteria

combinations_H3 <- read.xlsx("Filter_Combinations.xlsx", sheet = "H3")%>%
  pivot_longer(-Specification, names_to = "filter", values_to = "values")%>%
  filter(values == 1)%>%
  filter(Specification != 1)

data_7.3.1.1 <- data.frame()

for(i in c(2:20)){
  combinations_H3.1 <- combinations_H3 %>%
    filter(Specification == i)
  
  data_7.3_temp <- filter(data_7.3.1, if_all(all_of(combinations_H3.1$filter), ~ .x == 0))
  data_7.3.1.1_temp <- prepare_specification_chart(data_7.3_temp,paste(sub("filter_","", combinations_H3.1$filter), collapse = "_"),"Rounded","Standard")
  
  data_7.3.1.1 <- data_7.3.1.1 %>%
    bind_rows(data_7.3.1.1_temp)
}

rm(combinations_H3, combinations_H3.1, data_7.3.1.1_temp)

# Standard, binary, Three levels
data_7.3.1_temp <- data_7.3.1 %>%
  mutate(ffsr_prcl_2 = ifelse(ffsr_prcl %in% c(1,2),0,
                              ifelse(ffsr_prcl %in% c(4,5),1, NA)),
         ffsr_prcl_3 = ifelse(ffsr_prcl %in% c(1,2),1,
                              ifelse(ffsr_prcl %in% c(3),2,
                                     ifelse(ffsr_prcl %in% c(4,5),3,NA))))

data_7.3.1_temp_2 <- data_7.3.1_temp %>%
  mutate(NAs = ifelse(is.na(ffsr_prcl_2),1,0))%>%
  group_by(ID)%>%
  mutate(NAs = sum(NAs))%>%
  ungroup()%>%
  arrange(ID)%>%
  filter(NAs == 0)%>%
  mutate(ffsr_prcl = ffsr_prcl_2)

data_7.3.1.2 <- prepare_specification_chart(data_7.3.1_temp_2,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Binary")

data_7.3.1_temp_3 <- data_7.3.1_temp %>%
  mutate(ffsr_prcl = ffsr_prcl_3)

data_7.3.1.3 <- prepare_specification_chart(data_7.3.1_temp_3,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Rounded","Three levels")

# No filter
data_7.3.1.4 <- prepare_specification_chart(data_7.3.1,"No filter","Rounded","Standard")

# Not rounded
data_7.3.1_temp_5 <- filter(data_7.3.1, if_all(c("filter_1b", "filter_2b", "filter_2f", "filter_3b", "filter_3f", "filter_3h", "filter_3l", "filter_3n", "filter_3r", "filter_3t", "filter_3x"), ~ .x == 0))%>%
  select(-conditional)%>%
  rename(conditional = conditional_not_rounded)
data_7.3.1.5 <- prepare_specification_chart(data_7.3.1_temp_5,"1b_2b_2f_3b_3f_3h_3l_3n_3r_3t_3x","Not rounded","Standard")

data_7.2.X <- data_7.3.1.0 %>%
  bind_rows(data_7.3.1.1)%>%
  bind_rows(data_7.3.1.2)%>%
  bind_rows(data_7.3.1.3)%>%
  bind_rows(data_7.3.1.4)%>%
  bind_rows(data_7.3.1.5)

rm(data_7.3.1.0, data_7.3.1.1, data_7.3.1.2, data_7.3.1.3, data_7.3.1.4, data_7.3.1.5, data_7.3.1_temp, data_7.3.1_temp_2, data_7.3.1_temp_3, data_7.2.1_temp_5)

data_7.2.0 <- data_7.2.X %>%
  mutate(Type     = ifelse(is.na(Type), "Other", 
                           ifelse(FE == "Group", "Baseline", "Other")))%>%
  mutate(conf_low = estimate - 1.96*std.error,
         conf_high = estimate + 1.96*std.error)%>%
  mutate("Age < 81"                         = ifelse(str_detect(Filter, "1a"),1,0),
         "Age < 99"                         = ifelse(str_detect(Filter, "1b"),1,0),
         "Remove fastest 2%"                = ifelse(str_detect(Filter, "2a"),1,0),
         "Remove fastest 5%"                = ifelse(str_detect(Filter, "2b"),1,0),
         "Remove fastest 10%"               = ifelse(str_detect(Filter, "2c"),1,0),
         "Remove slowest 10%"               = ifelse(str_detect(Filter, "2d"),1,0),
         "Remove slowest 5%"                = ifelse(str_detect(Filter, "2e"),1,0),
         "Remove slowest 2%"                = ifelse(str_detect(Filter, "2f"),1,0),
         "Remove fastest 2% first-stage"    = ifelse(str_detect(Filter, c("3a")) | str_detect(Filter, c("3g")) | str_detect(Filter, c("3m")) | str_detect(Filter, c("3s")),1,0),
         "Remove fastest 5% first-stage"    = ifelse(str_detect(Filter, c("3b")) | str_detect(Filter, c("3h")) | str_detect(Filter, c("3n")) | str_detect(Filter, c("3t")),1,0),
         "Remove fastest 10% first-stage"   = ifelse(str_detect(Filter, c("3c")) | str_detect(Filter, c("3i")) | str_detect(Filter, c("3o")) | str_detect(Filter, c("3u")),1,0),
         "Remove slowest 10% first-stage"   = ifelse(str_detect(Filter, c("3d")) | str_detect(Filter, c("3j")) | str_detect(Filter, c("3p")) | str_detect(Filter, c("3v")),1,0),
         "Remove slowest 5% first-stage"    = ifelse(str_detect(Filter, c("3e")) | str_detect(Filter, c("3k")) | str_detect(Filter, c("3q")) | str_detect(Filter, c("3w")),1,0),
         "Remove slowest 2% first-stage"    = ifelse(str_detect(Filter, c("3f")) | str_detect(Filter, c("3l")) | str_detect(Filter, c("3r")) | str_detect(Filter, c("3x")),1,0),
         "Attention check correct"          = ifelse(str_detect(Filter, "4"),1,0),
         "Variation in conditional support" = ifelse(str_detect(Filter, "5"),1,0))%>%
  mutate(ID = 1:n())%>%
  arrange(estimate, ID)

data_7.2.0.1 <- data_7.2.0 %>%
  filter(Type_1 == "Unconditional")%>%
  filter(Rounded != "Not rounded")%>%
  mutate(seq = 1:n())%>%
  mutate(seq_fa = as_factor(seq))%>%
  mutate(name = "Coefficient")

P_7.2.1 <- ggplot(data_7.2.0.1, aes(x = seq))+
  scale_x_discrete()+
  annotate("rect", xmin = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] + 0.5), ymin = -0.5, ymax = 1.5, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_hline(aes(yintercept = 0))+
  geom_errorbar(aes(ymin = conf_low, ymax = conf_high, colour = Type), width = 0.7, linewidth = 0.2)+
  geom_point(aes(y = estimate, colour = Type), size = 1.2, shape = 16)+
  facet_grid(name ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  coord_cartesian(ylim = c(-0.15,0.55))+
  theme_bw()+
  guides(colour = "none")+
  xlab("")+ylab("")+
  scale_colour_manual(values = c("#6F99ADFF", "black"))+
  scale_fill_manual(values   = c("grey", "#6F99ADFF"))+
  scale_size_manual(values   = c(0.2, 0.35))+
  scale_y_continuous(expand = c(0,0), breaks = c(-0.1,0,0.1,0.2,0.3,0.4,0.5))+
  theme(axis.text.x      = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 7), 
        axis.title  = element_blank(), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text = element_text(size = 7), 
        strip.text.y = element_text(size = 7), 
        panel.grid.major = element_line(size = 0.3), 
        axis.ticks = element_line(size = 0.4),
        legend.text = element_text(size = 7), 
        legend.title = element_text(size = 7), 
        plot.margin = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border = element_rect(size = 0.3))

interest_df <- c("Age < 81" = 0,
                 "Age < 99" = 1,
                 "Attention check correct" = 0,
                 "Binary" = 0,
                 "Group" = 1,
                 "First-Stage" = 0,
                 "None" = 0,
                 "Remove fastest 10%"               = 0,
                 "Remove fastest 10% first-stage"   = 0,
                 "Remove fastest 2%"                = 0,
                 "Remove fastest 2% first-stage"    = 0,
                 "Remove fastest 5%"                = 1,
                 "Remove fastest 5% first-stage"    = 1,
                 "Remove slowest 10%"               = 0,
                 "Remove slowest 10% first-stage"   = 0,
                 "Remove slowest 2%"                = 1,
                 "Remove slowest 2% first-stage"    = 1,
                 "Remove slowest 5%"                = 0,
                 "Remove slowest 5% first-stage"    = 0,
                 "Rounded"                          = 1,
                 "Standard"                         = 1,
                 "Three levels"                     = 0,
                 "Variation in conditional support" = 0)%>%
  as_tibble(rownames = "Variable")%>%
  rename(value_interest = value)

data_7.2.0.1_cat <- data_7.2.0.1 %>%
  select(ID, seq, seq_fa, FE, Rounded, Outcome, Type:"Variation in conditional support", -conf_low, -conf_high)%>%
  mutate(Rounded             = ifelse(Rounded == "Rounded",1,0),
         "Standard"          = ifelse(Outcome == "Standard",1,0),
         "Binary"            = ifelse(Outcome == "Binary",1,0),
         "Three levels"      = ifelse(Outcome == "Three levels",1,0),
         "First-Stage"       = ifelse(FE == "First_Stage",1,0),
         "None"              = ifelse(FE == "None",1,0),
         "Group"             = ifelse(FE == "Group",1,0))%>%
  select(ID, seq, seq_fa, Type, everything(), -Outcome, -FE)%>%
  pivot_longer("Rounded":"Group", names_to = "Variable", values_to = "Value")%>%
  mutate(Value = ifelse(Type == "Baseline" & Value == 1,2,Value))%>%
  # For different colours
  left_join(interest_df)%>%
  mutate(Value_2 = ifelse(Value == 2, 1, # Main Specification
                          ifelse(Value == 1 & value_interest == 1, 2, # Criterion identical with main specification - both fulfilled
                                 ifelse(Value == 1 & value_interest != 1, 3, # Criterion not fulfilled while fulfilled in main specification
                                        ifelse(Value == 0 & value_interest == 0,4, # Criterion identical with main specification - both not fulfilled
                                               ifelse(Value == 0 & value_interest != 0, 5, 6))))))%>% # Criterion fulfilled while not fulfilled in main specificataion 
  mutate(Variable = factor(Variable, levels = c("Standard", "Binary", "Three levels", # "Rounded",
                                                "Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                                "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                                "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                                "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage",
                                                "None", "Group", "First-Stage")))%>%
  mutate(Group = ifelse(Variable %in% c("Standard", "Binary", "Three levels", "Rounded"), "Outcome",
                        ifelse(Variable %in% c("Variation in conditional support", "Attention check correct", "Age < 81", "Age < 99",
                                               "Remove fastest 2%", "Remove fastest 5%", "Remove fastest 10%", "Remove slowest 2%", "Remove slowest 5%", "Remove slowest 10%",
                                               "Remove fastest 2% first-stage", "Remove fastest 5% first-stage", "Remove fastest 10% first-stage",
                                               "Remove slowest 2% first-stage", "Remove slowest 5% first-stage", "Remove slowest 10% first-stage"), "Filter",
                               ifelse(Variable %in% c("None", "Group", "First-Stage"), "Controls", NA))))%>%
  mutate(Group = factor(Group, levels = c("Outcome", "Filter", "Controls")))%>%
  mutate(Value_2 = factor(Value_2))%>%
  filter(!is.na(Group))

P_7.2.2 <- ggplot(data = data_7.2.0.1_cat, aes(x = seq_fa, y = reorder(Variable, desc(Variable))))+
  scale_x_discrete()+
  scale_y_discrete(expand = c(0,0.75))+
  annotate("rect", xmin = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] - 0.5), xmax = (data_7.2.0.1$seq[data_7.2.0.1$Type == "Baseline"] + 0.5), ymin = 0, ymax = Inf, fill = "grey90", colour = "lightgrey", size = 0.1)+
  geom_point(aes(colour = Value_2, fill = Value_2), shape = 22, size = 1.2, stroke = 0.2)+
  facet_grid(Group ~ .,
             scales = "free_y",
             space  = "free_y",
             switch = "y")+
  scale_colour_manual(values = c("black", "black", "black", "gray60", "black"))+
  scale_fill_manual(values = c("#6F99ADFF","gray40", "gray40", "grey85", "#E18727FF"))+
  # facet_grid(Group ~ .,
  #            scales = "free_y",
  #            space  = "free_y",
  #            switch = "y")+
  theme_bw()+
  theme(axis.text.x = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        strip.placement = "outside",
        axis.text.y = element_text(size = 5), 
        axis.title  = element_text(size = 6), 
        plot.title = element_text(size = 7), 
        legend.position = "bottom", 
        strip.text      = element_text(size = 6),
        axis.ticks      = element_line(size = 0.2),
        legend.text     = element_text(size = 7), 
        legend.title    = element_text(size = 7), 
        plot.margin     = unit(c(0.1,0.1,0,0), "cm"), 
        panel.border    = element_rect(size = 0.3))+
  ylab("")+xlab("")+
  guides(fill = "none", colour = "none")

P_7.2 <- ggarrange(P_7.2.1, P_7.2.2, nrow = 2, align = "hv", heights = c(0.8,1))

png("../Colombia_Survey_Experiment/Paper/Figures/2_Appendix/Figure_B_H3.png", width = 6, height = 7, unit = "in", res = 400)
print(P_7.2)
dev.off()

# 9 Supplementary Graphics ####

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


