#Lukas Cincikas 2021
#A collection of various parameter graphs for the naltrexone study. Individual values are taken from .txt output files.

library(rstan)
library(patchwork)

data_control_alcohol_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(-1.281435, -2.509639, -0.7862308),
  lower_bound = c(0.4421553, 2.845272, 0.052656),
  upper_bound = c(-2.967002, -8.079226, -1.656406)
)

data_control_alcohol_2 <- data.frame(
  parameter = c("Side bias", "Delay aversion", "Previous outcome", "Odds sensitivity"),
  mean = c(0.05519839, 0.002694728, 0.02364208, 0.1308009),
  lower_bound = c(0.1406517, 0.03968826, 0.1871594, 0.5476102),
  upper_bound = c(-0.02742524, -0.0353849, -0.1394677, -0.2833208)
)

data_control_poly_1 <- data.frame(
  parameter = c("Bet consistency"),
  mean = c(-5.034968),
  lower_bound = c(0.4334),
  upper_bound = c(-10.4868)
)

data_control_poly_1_col <- data.frame(
  parameter = c("Colour consistency", "Loss/gain sensitivity"),
  mean = c(-2.503876, -1.147968),
  lower_bound = c(-1.000371, -0.3981),
  upper_bound = c(-4.0591, -1.965494)
)

data_control_poly_2 <- data.frame(
  parameter = c("Side bias", "Odds sensitivity"),
  mean = c(0.02391672, 0.04783284),
  lower_bound = c(0.09866658, 0.4278),
  upper_bound = c(-0.048465, -0.3056343)
)

data_control_poly_2_col <- data.frame(
  parameter = c("Delay aversion", "Previous outcome"),
  mean = c(0.05362031, 0.2114642),
  lower_bound = c(0.1033, 0.3879),
  upper_bound = c(0.008075647, 0.0372774)
)

data_alcohol_poly_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity", "Odds sensitivity"),
  mean = c(1.2224, 2.5253, 0.3617, 0.082968),
  lower_bound = c(-0.18266, -2.5712, -0.26059, -0.3781),
  upper_bound = c(2.71566, 7.548, 1.0547, 0.54028)
)

data_alcohol_poly_2 <- data.frame(
  parameter = c("Side bias", "Previous outcome"),
  mean = c(0.03128, -0.1878),
  lower_bound = c(-0.03115, -0.3896),
  upper_bound = c(0.09406, 0.006728588)
)

data_alcohol_poly_2_col <- data.frame(
  parameter = c("Delay aversion"),
  mean = c(-0.050926),
  lower_bound = c(-0.09943),
  upper_bound = c(-0.005993)
)

data_naltrexone_1 <- data.frame(
  parameter = c("Side bias", "Previous outcome", "Odds sensitivity"),
  mean = c(0.00997, -0.068033, 0.044687),
  lower_bound = c(0.057113, 0.0078057, 0.15477),
  upper_bound = c(-0.038299, -0.14356, -0.06587)
)

data_naltrexone_1_col <- data.frame(
  parameter = c("Delay aversion"),
  mean = c(0.01585),
  lower_bound = c(0.030881),
  upper_bound = c(0.00119)
)

data_naltrexone_2 <- data.frame(
  parameter = c("Bet consistency"),
  mean = c(2.1668),
  lower_bound = c(4.2633),
  upper_bound = c(0.1087)
)

data_naltrexone_3 <- data.frame(
  parameter = c("Colour consistency"),
  mean = c(0.165968),
  lower_bound = c(0.5882355),
  upper_bound = c(-0.2648)
)

data_naltrexone_3_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(-0.2331),
  lower_bound = c(-0.006263),
  upper_bound = c(-0.47517)
)

graph_control_alcohol_1 <-
  ggplot(data_control_alcohol_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=0.5) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
    panel.grid = element_line(colour="white"),
    axis.text = element_text(size=11, face = "bold"),
    axis.ticks = element_line(size=1),
    text = element_text(size=14, face = "bold"))

graph_control_alcohol_2 <-
  ggplot(data_control_alcohol_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("NALT study Alcohol-Control group posterior mean differences (±95% HDI)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=0.5) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
    panel.grid = element_line(colour="white"),
    axis.text = element_text(size=11, face = "bold"),
    axis.ticks = element_line(size=1),
    text = element_text(size=14, face = "bold"))


graph_control_poly_1 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_1_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data_control_poly_1) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_1_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold")
  )

graph_control_poly_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_2_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data_control_poly_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_2_col, colour="magenta") +
  xlab("") +
  ylab("NALT study Substance-Control group posterior mean differences (±95% HDI)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold")
  )

graph_alcohol_poly_1 <-
  ggplot(data_alcohol_poly_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold")
  )

graph_alcohol_poly_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_alcohol_poly_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_alcohol_poly_2_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data_alcohol_poly_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_alcohol_poly_2_col, colour="magenta") +
  xlab("") +
  ylab("NALT study Alcohol-Substance group posterior mean differences (±95% HDI)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold")
  )

graph_naltrexone_1 <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_1_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data_naltrexone_1) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_1_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_naltrexone_2 <-
  ggplot(data_naltrexone_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_naltrexone_3 <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_3, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_3_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data_naltrexone_3) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_3_col, colour="magenta") +
  xlab("") +
  ylab("Naltrexone condition posterior mean differences (±95% HDI)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


#Displays means of a few parameters for the groups
data_means <- data.frame(
  group = c("Loss/gain (control)", "Loss/gain (alcohol)", "Loss/gain (substance)", "Colour consistency (control)", "Colour consistency (alcohol)", "Colour consistency (substance)", "Previous outcome (control)", "Previous outcome (alcohol)", "Previous outcome (substance)"),
  mean = c(2.1794, 1.5254, 1.0976, 5.0044, 3.6385, 2.68, 0.2257, 0.3348, 0.4259),
  lower_bound = c(1.5246, 0.9893, 0.7142, 3.747, 2.576, 1.843, 0.114, 0.1769, 0.2743),
  upper_bound = c(3.0015, 2.237, 1.5928, 6.37, 4.956, 3.669, 0.3469, 0.49946, 0.58593)
)

graph_means <-
  ggplot(data_means, aes(x = group, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Group (placebo) parameter means") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


#Makes graphs with drug use comparison between the poly groups of the two studies
data_drugs_CUD <- data.frame(
  drug = c("Stimulants", "Opiates", "Alcohol"),
  proportions = c(100, 57.1, 10.7)
)

data_drugs_poly <- data.frame(
  drug = c("Stimulants", "Opiates", "Alcohol"),
  proportions = c(81.5, 55.6, 74.1)
)

graph_drugs_CUD <-
  ggplot(data_drugs_CUD, aes(x = drug, y = proportions)) +
  geom_col() +
  xlab("CUD group of ATX study") +
  ylab("Propotion of group") +
  ylim(0, 100) +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_drugs_poly <-
  ggplot(data_drugs_poly, aes(x = drug, y = proportions)) +
  geom_col() +
  xlab("Substance group of naltrexone study") +
  ylab("Propotion of group") +
  ylim(0, 100) +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#----------------------------------------------------------------------------------------
#This Section is for PLACEBO condition comparisons
#----------------------------------------------------------------------------------------

#PLACEBO comparison of CONTROLS and COMBINED SUBSTANCE groups

data_control_substance <- data.frame(
  parameter = c("Side bias", "Delay aversion", "Previous outcome"),
  mean = c(-0.0201, 0.0311, 0.155),
  lower_bound = c(0.0688, 0.0664, 0.313),
  upper_bound = c(-0.107, -0.0057, -0.00149)
)

data_control_substance_2 <- data.frame(
  parameter = c("Bet consistency", "Odds sensitivity"),
  mean = c(-3.08, -0.0246),
  lower_bound = c( 1.69, 0.327),
  upper_bound = c( -8.25, -0.374)
)

data_control_substance_2_col <- data.frame(
  parameter = c("Colour consistency", "Loss/gain sensitivity"),
  mean = c(-1.85, -0.868),
  lower_bound = c(-0.38, -0.0844),
  upper_bound = c(-3.4, -1.75)
)

graph_control_substance <-
  ggplot(data_control_substance, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_substance_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_substance_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_substance_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of difference between control and combined substance groups on PLACEBO") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#PLACEBO comparison of CONTROLS and ALCOHOL

data_control_alcohol_placebo <- data.frame(
  parameter = c("Side bias", "Delay aversion", "Previous outcome"),
  mean = c(-0.0377, 0.00664, 0.109),
  lower_bound = c(0.0682, 0.0467, 0.31),
  upper_bound = c(-0.142, -0.0335, -0.082)
)

data_control_alcohol_placebo_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity", "Odds sensitivity"),
  mean = c(-1.37, -2.85, -0.654, -0.0119),
  lower_bound = c(0.468, 2.67, 0.305, 0.442),
  upper_bound = c(-3.14, -8.6, -1.64, -0.459)
)

graph_control_alcohol_placebo <-
  ggplot(data_control_alcohol_placebo, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_alcohol_placebo_2 <-
  ggplot(data_control_alcohol_placebo_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of Control and Alcohol groups (placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#PLACEBO comparison of CONTROL and POLYSUBSTANCE groups

data_control_poly_placebo <- data.frame(
  parameter = c("Side bias"),
  mean = c(-0.00244),
  lower_bound = c(0.0875),
  upper_bound = c(-0.0925)
)

data_control_poly_placebo_col <- data.frame(
  parameter = c("Delay aversion", "Previous outcome"),
  mean = c(0.0552, 0.2),
  lower_bound = c(0.104, 0.395),
  upper_bound = c(0.0106, 0.0111)
)

data_control_poly_placebo_2 <- data.frame(
  parameter = c("Bet consistency", "Odds sensitivity"),
  mean = c(-3.31, -0.0372),
  lower_bound = c(2.42, 0.367),
  upper_bound = c(-9.07, -0.425)
)

data_control_poly_placebo_2_col <- data.frame(
  parameter = c("Colour consistency", "Loss/gain sensitivity"),
  mean = c(-2.32, -1.08),
  lower_bound = c(-0.751, -0.254),
  upper_bound = c(-3.95, -1.98)
)

graph_control_poly_placebo <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_placebo, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_placebo_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_placebo) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_placebo_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_poly_placebo_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_placebo_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_placebo_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_placebo_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_placebo_2_col, colour="magenta") +
  xlab("") +
  ylab("95% of Control and Polysubstance groups (On placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#PLACEBO comparison of ALCOHOL and POLYSUBSTANCE groups

data_alcohol_poly_placebo <- data.frame(
  parameter = c("Side bias", "Previous outcome"),
  mean = c(-0.0353, -0.0911),
  lower_bound = c(-0.121, -0.313),
  upper_bound = c(0.0504, 0.134)
)

data_alcohol_poly_placebo_col <- data.frame(
  parameter = c("Delay aversion"),
  mean = c( -0.0488),
  lower_bound = c(-0.0996),
  upper_bound = c(-0.000709)
)

data_alcohol_poly_placebo_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity", "Odds sensitivity"),
  mean = c(0.958, 0.454, 0.428, 0.0253),
  lower_bound = c(-0.512, -4.95, -0.309, -0.455),
  upper_bound = c(2.51, 5.71, 1.24, 0.505)
)

graph_alcohol_poly_placebo <-
  ggplot() +
  geom_point(size=3.5, data=data_alcohol_poly_placebo, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_alcohol_poly_placebo_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_alcohol_poly_placebo) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_alcohol_poly_placebo_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_alcohol_poly_placebo_2 <-
  ggplot(data_alcohol_poly_placebo_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of Alcohol and Polysubstance groups (on placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#----------------------------------------------------------------------------------------
#This Section is for NALTREXONE condition comparisons
#----------------------------------------------------------------------------------------

data_control_alcohol_naltrexone <- data.frame(
  parameter = c("Delay aversion", "Previous outcome"),
  mean = c(0.00125, 0.0618),
  lower_bound = c(-0.0419, -0.121),
  upper_bound = c(0.0445, 0.246)
)

data_control_alcohol_naltrexone_col <- data.frame(
  parameter = c("Side bias"),
  mean = c(-0.148),
  lower_bound = c(-0.256),
  upper_bound = c(-0.0435)
)

data_control_alcohol_naltrexone_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Odds sensitivity"),
  mean = c(1.2, 2.17, -0.274),
  lower_bound = c(-0.647, -4.52, -0.713),
  upper_bound = c(3.00, 8.86, 0.158)
)

data_control_alcohol_naltrexone_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(0.918),
  lower_bound = c(0.123),
  upper_bound = c(1.79)
)

graph_control_alcohol_naltrexone <-
  ggplot() +
  geom_point(size=3.5, data=data_control_alcohol_naltrexone, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_alcohol_naltrexone_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_alcohol_naltrexone) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_alcohol_naltrexone_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_alcohol_naltrexone_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_alcohol_naltrexone_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_alcohol_naltrexone_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_alcohol_naltrexone_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_alcohol_naltrexone_2_col, colour="magenta") +
  xlab("") +
  ylab("95% of Control and Alcohol groups (On naltrexone)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))



data_control_poly_naltrexone <- data.frame(
  parameter = c("Side bias"),
  mean = c(-0.0503),
  lower_bound = c(-0.144),
  upper_bound = c(0.0438)
)

data_control_poly_naltrexone_col <- data.frame(
  parameter = c("Delay aversion", "Previous outcome"),
  mean = c(-0.0518, -0.223),
  lower_bound = c(-0.107, -0.42),
  upper_bound = c(-0.00125, -0.0326)
)

data_control_poly_naltrexone_2 <- data.frame(
  parameter = c("Odds sensitivity"),
  mean = c(-0.133),
  lower_bound = c(-0.535),
  upper_bound = c(0.235)
)

data_control_poly_naltrexone_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity", "Colour consistency", "Bet consistency"),
  mean = c(1.21, 2.68, 6.76),
  lower_bound = c(0.471, 1.11, 0.757),
  upper_bound = c(2.05, 4.29, 12.9)
)

graph_control_poly_naltrexone <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_naltrexone, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_naltrexone_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_naltrexone) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_naltrexone_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_poly_naltrexone_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_poly_naltrexone_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_poly_naltrexone_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_naltrexone_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_poly_naltrexone_2_col, colour="magenta") +
  xlab("") +
  ylab("95% of Control and Polysubstance groups (On naltrexone)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))



data_alcohol_poly_naltrexone <- data.frame(
  parameter = c(),
  mean = c(),
  lower_bound = c(),
  upper_bound = c()
)

data_alcohol_poly_naltrexone_col <- data.frame(
  parameter = c("Side bias", "Delay aversion", "Previous outcome"),
  mean = c(0.0979, -0.053, -0.285),
  lower_bound = c(0.0081, -0.108, -0.503),
  upper_bound = c(0.185, -0.00232, -0.0707)
)

data_alcohol_poly_naltrexone_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity", "Odds sensitivity"),
  mean = c(1.49, 4.6, 0.296, 0.141),
  lower_bound = c(-0.0154, -1.18, -0.283, -0.332),
  upper_bound = c(3.08, 10.5, 0.929, 0.619)
)

data_alcohol_poly_naltrexone_2_col <- data.frame(
  parameter = c(),
  mean = c(),
  lower_bound = c(),
  upper_bound = c()
)

graph_alcohol_poly_naltrexone <-
  ggplot() +
  geom_point(size=3.5, data=data_alcohol_poly_naltrexone_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_alcohol_poly_naltrexone_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_alcohol_poly_naltrexone_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_alcohol_poly_naltrexone_2, aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_alcohol_poly_naltrexone_2) +
  xlab("") +
  ylab("95% of Alcohol and Polysubstance groups (On naltrexone)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


data_control_substance_naltrexone <- data.frame(
  parameter = c("Delay aversion", "Previous outcome"),
  mean = c(-0.0253, -0.0805),
  lower_bound = c(-0.0655, -0.233),
  upper_bound = c(0.0158, 0.0757)
)

data_control_substance_naltrexone_col <- data.frame(
  parameter = c("Side bias"),
  mean = c(-0.0992),
  lower_bound = c(-0.19),
  upper_bound = c(-0.0109)
)

data_control_substance_naltrexone_2 <- data.frame(
  parameter = c("Odds sensitivity", "Bet consistency"),
  mean = c(-0.203, 4.46),
  lower_bound = c(-0.541, -1.05),
  upper_bound = c(0.133, 10.3)
)

data_control_substance_naltrexone_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity", "Colour consistency"),
  mean = c(1.07, 1.94),
  lower_bound = c(0.377, 0.458),
  upper_bound = c(1.88, 3.51)
)

graph_control_substance_naltrexone <-
  ggplot() +
  geom_point(size=3.5, data=data_control_substance_naltrexone, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_substance_naltrexone_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_naltrexone) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_naltrexone_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_control_substance_naltrexone_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_substance_naltrexone_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_substance_naltrexone_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_naltrexone_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_substance_naltrexone_2_col, colour="magenta") +
  xlab("") +
  ylab("95% of Control and combined substance groups (on naltrexone)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#----------------------------------------------------------------------------------------
#This Section is for CONDITION comparisons
#----------------------------------------------------------------------------------------

data_naltrexone_control <- data.frame(
  parameter = c("Side bias", "Previous outcome"),
  mean = c(-0.0696, -0.0186),
  lower_bound = c(0.0138, 0.0914),
  upper_bound = c(-0.153, -0.129)
)

data_naltrexone_control_col <- data.frame(
  parameter = c("Delay aversion"),
  mean = c(0.0197),
  lower_bound = c(0.04),
  upper_bound = c(0.00134)
)

data_naltrexone_control_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity", "Odds sensitivity"),
  mean = c(0.229, 3.09, -0.101, -0.107),
  lower_bound = c(1.04, 6.99, 0.402, 0.101),
  upper_bound = c(-0.57, -0.678, -0.641, -0.314)
)

data_naltrexone_control_2_col <- data.frame(
  parameter = c(),
  mean = c(),
  lower_bound = c(),
  upper_bound = c()
)

graph_naltrexone_control <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_control, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_control_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_control) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_control_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_naltrexone_control_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_control_2, aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_control_2) +
  xlab("") +
  ylab("95% HDI of drug condition in Control group") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


data_naltrexone_alcohol <- data.frame(
  parameter = c("Delay aversion"),
  mean = c(0.0118),
  lower_bound = c(0.042),
  upper_bound = c(-0.0177)
)

data_naltrexone_alcohol_col <- data.frame(
  parameter = c("Side bias", "Previous outcome"),
  mean = c(0.116, -0.189),
  lower_bound = c(0.214, -0.0341),
  upper_bound = c(0.0159, -0.347)
)

data_naltrexone_alcohol_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Odds sensitivity"),
  mean = c(0.398, 3.78, 0.178),
  lower_bound = c(1.24, 7.89, 0.384),
  upper_bound = c(-0.463, -0.145, -0.0352)
)

data_naltrexone_alcohol_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(-0.365),
  lower_bound = c(-0.0149),
  upper_bound = c(-0.797)
)

graph_naltrexone_alcohol <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_alcohol, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_alcohol_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_alcohol) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_alcohol_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_naltrexone_alcohol_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_alcohol_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_alcohol_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_alcohol_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_alcohol_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of drug condition in Alcohol group") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))



data_naltrexone_poly <- data.frame(
  parameter = c("Delay aversion", "Side bias", "Previous outcome"),
  mean = c(0.016, -0.0169, 0.00404),
  lower_bound = c(0.0431, 0.0491, 0.123),
  upper_bound = c(-0.00896, -0.0849, -0.114)
)

data_naltrexone_poly_col <- data.frame(
  parameter = c(),
  mean = c(),
  lower_bound = c(),
  upper_bound = c()
)

data_naltrexone_poly_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Odds sensitivity"),
  mean = c(-0.13, -0.365, 0.063),
  lower_bound = c(0.392, 2.43, 0.223),
  upper_bound = c(-0.661, -3.31, -0.0885)
)

data_naltrexone_poly_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(-0.233),
  lower_bound = c(-0.00688),
  upper_bound = c(-0.505)
)

graph_naltrexone_poly <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_poly, aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_poly) +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_naltrexone_poly_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_naltrexone_poly_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_naltrexone_poly_2_col, colour="magenta", aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_poly_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_naltrexone_poly_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of drug condition in the Polysubstance group") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))