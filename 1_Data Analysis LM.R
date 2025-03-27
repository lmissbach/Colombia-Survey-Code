# Author: L. Missbach (leonard.missbach@pik-potsdam.de)

if(!require("pacman")) install.packages("pacman")

p_load("arrow", "ggsci", "fixest", "haven", "Hmisc", "openxlsx", "rattle", "scales", "tidyverse", "sjlabelled", "tidymodels")

options(scipen=999)

# 0.  Load data ####

data_0 <- read_sav("../Paper_Colombia_Experiment/Analysis/Inputs/DE_124764_Brandenb_TU_Mcc_Berlin_Kolumbien  Ad hoc - final.sav")

# 1. Renaming and cleaning (TBD) ####

# Update later

data_0 <- read_sav("../Paper_Colombia_Experiment/Analysis/Inputs/Intermediate.sav")

treatment_vars <- c("T_A", "T_B", "T_C", "T_D")
frstst_vars <- c("frst_a", "frst_b", "frst_c", "frst_d")
control_vars <- c("C_A", "C_B", "C_C", "C_D", "C_no_frst")

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

# Survey_H1_long & Survey_H2_long
data_1.1 <- data_1 %>%
  select(ID, conditional, unconditional_prcl)%>%
  pivot_longer(-ID, names_to = "type_support", values_to = "support")%>%
  mutate(conditional_support = ifelse(type_support == "conditional",1,0))%>%
  left_join(select(data_1, ID, all_of(treatment_vars), all_of(control_vars), treatment))

# Survey_H1_recode_2 & Survey_H2_recode_2
data_1.2 <- data_1 %>%
  select(ID, unconditional_prcl_recode_2, conditional_recode_2)%>%
  pivot_longer(-ID, names_to = "type_support", values_to = "support")%>%
  mutate(conditional_support = ifelse(type_support == "conditional_recode_2",1,0))%>%
  left_join(select(data_1, ID, all_of(treatment_vars), all_of(control_vars), treatment))

# Survey_H1_recode_2extrem & Survey_H2_recode_2extrem
data_1.3 <- data_1 %>%
  select(ID, unconditional_prcl_recode_2extrem, conditional_recode_2extrem)%>%
  pivot_longer(-ID, names_to = "type_support", values_to = "support")%>%
  mutate(conditional_support = ifelse(type_support == "conditional_recode_2extrem",1,0))%>%
  left_join(select(data_1, ID, all_of(treatment_vars), all_of(control_vars), treatment))

# Survey_H1_recode_3 & Survey_H2_recode_3
data_1.4 <- data_1 %>%
  select(ID, unconditional_prcl_recode_3, conditional_recode_3)%>%
  pivot_longer(-ID, names_to = "type_support", values_to = "support")%>%
  mutate(conditional_support = ifelse(type_support == "conditional_recode_3",1,0))%>%
  left_join(select(data_1, ID, all_of(treatment_vars), all_of(control_vars), treatment))

# 2. Analysis ####

tex.style <- style.tex(model.title = "", fixef.title = "\\midrule",
                       stats.title = "\\midrule", model.format = "",
                       fontsize = "small")

dict_latex <- c(support = "Support (1-5)", "conditional_support" = "Conditional support",
                "T_A" = "Treatment A", "T_B" = "Treatment B", "T_C" = "Treatment C", "T_D" = "Treatment D")


# 2.0 Graphics ####

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

# 2.1 H1 ####
# Conditional support is higher than unconditional support

# OLS
# support (1-5) over conditional_support (0/1)

model_1.1 <- feols(support ~ conditional_support, data = data_1.1)
tidy_1.1  <- tidy(model_1.1)%>%
  mutate(type = "OLS")

etable(model_1.1, tex = TRUE, dict = dict_latex,
       file = "../Colombia_Survey_Experiment/Paper/Tables/Table_H1.tex",
       digits = 3, replace = TRUE, fitstat = c("n", "r2"), style.tex = tex.style, se.row = TRUE, tpt = TRUE,
       title = "Hypothesis 1",  
       label = "tab:H1", 
       # adjustbox = "width = 1\\textwidth, max height = 0.95\\textheight, center", 
       placement = "htbp!")

# Logit
# support (0 (1-2)/1(4-5)) over conditional_support (0/1)
data_1.2.1 <- data_1.2 %>%
  filter(!is.na(support))
model_1.2.1 <- feglm(support ~ conditional_support, family = binomial(link = "logit"), data = data_1.2.1)
# Probit
model_1.2.2 <- feglm(support ~ conditional_support, family = binomial(link = "probit"), data = data_1.2.1)
tidy_1.2.1  <- tidy(model_1.2)%>%
  mutate(type = "Logit")
tidy_1.2.2  <- tidy(model_1.2)%>%
  mutate(type = "Probit")

# support (0 (1-2)/1(4-5)) over conditional_support (0/1)
data_1.3.1 <- data_1.3 %>%
  filter(!is.na(support))
model_1.3.1 <- feglm(support ~ conditional_support, family = binomial(link = "logit"), data = data_1.3.1)
# Probit
model_1.3.2 <- feglm(support ~ conditional_support, family = binomial(link = "probit"), data = data_1.3.1)
tidy_1.3.1  <- tidy(model_1.3.1)%>%
  mutate(type = "Logit")
tidy_1.3.2  <- tidy(model_1.3.2)%>%
  mutate(type = "Probit")

# support (0(1-2)/1(3)/2(4-5))
# OLS
model_1.4.1 <- feols(support ~ conditional_support, data = data_1.4)
tidy_1.4.1  <- tidy(model_1.4.1)%>%
  mutate(type = "OLS")
# model_1.4.2 <- feglm(support ~ conditional_support, family = (link = "probit"), data = data_1.4)
# tidy_1.2.2  <- tidy(model_1.2)%>%
#   mutate(type = "Probit")

# Quick visualization

data_1.X <- data_1.1 %>%
  group_by(type_support, support)%>%
  summarise(number = n())%>%
  ungroup()

P_0 <- ggplot(data_1.X)+
  geom_col(aes(x = type_support, y = number, group = type_support, fill = type_support), alpha = 0.7)+
  facet_grid(. ~ support)+
  scale_fill_nejm(labels = c("Conditional", "Unconditional"), name = " ")+
  theme_bw()+
  xlab("")+
  theme(axis.text.x = element_blank())

jpeg("../Colombia_Survey_Experiment/Paper/Figures/0_Test/Density_Conditional.jpg", width = 15, height = 10, unit = "cm", res = 600)
print(P_0)
dev.off()

# 2.2 H2 ####

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
       label = "tab:H1", 
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



