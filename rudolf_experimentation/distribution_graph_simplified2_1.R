#Lukas Cincikas 2021
#collection of graphs for the atomoxetine study. Individual values are taken from .txt output files.

library(rstan)
library(patchwork)

data_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(-5.416, -16.207, -2.699),
  lower_bound = c(-3.914, -10.531, -1.776),
  upper_bound = c(-6.816, -22.526, -3.808)
)

data_2 <- data.frame(
  parameter = c("Previous outcome", "Odds sensitivity"),
  mean = c(0.448, 0.697),
  lower_bound = c(0.663, 1.185),
  upper_bound = c(0.244, 0.255)
)

data_2_additional <- data.frame(
  parameter = c("Side bias", "Delay aversion"),
  mean = c(0.0666, 0.0704),
  lower_bound = c(0.132, 0.113),
  upper_bound = c(0.00146, 0.0315)
)

data_3 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency"),
  mean = c(0.155, 2.482),
  lower_bound = c(0.693, 5.163),
  upper_bound = c(-0.363, -0.047)
)

data_3_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(0.447),
  lower_bound = c(0.792),
  upper_bound = c(0.137)
)

data_4 <- data.frame(
  parameter = c("Side bias", "Delay aversion", "Previous outcome", "Odds sensitivity"),
  mean = c(-0.0152, -0.00889, 0.0402, 0.0158),
  lower_bound = c(0.044, 0.01, 0.143, 0.185),
  upper_bound = c(-0.0737, -0.0276, -0.0622, -0.147)
)

graph_1 <-
  ggplot(data_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), colour="magenta") +
  xlab("") +
  ylab("") +
  ylim(-24, 0) +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        legend.position = "none"
        )

graph_2 <-
  ggplot(data_2, aes(x = parameter, y = mean)) +
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
        axis.ticks = element_line(size=1)
        )

graph_2_additional <-
  ggplot(data_2_additional, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), colour="magenta") +
  xlab("") +
  ylab("ATX study group posterior mean differences (±95% HDI, CUD-control)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

graph_3 <-
  ggplot() +
  geom_point(size=3.5, data=data_3, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_3_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_3) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_3_col, colour="magenta") +
  xlab("") +
  ylab("") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
    panel.grid = element_line(colour="white"),
    axis.text = element_text(size=11, face = "bold"),
    axis.ticks = element_line(size=1)
  )

graph_4 <-
  ggplot(data_4, aes(x = parameter, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Atomoxetine condition group posterior mean differences (±95% HDI, ATX-placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
    panel.grid = element_line(colour="white"),
    axis.text = element_text(size=11, face = "bold"),
    axis.ticks = element_line(size=1),
    text = element_text(size=14, face = "bold"))

data_c <- data.frame(
  group = c("CUD (placebo)", "CUD (ATX)"),
  mean = c(0.4972, 0.4581),
  lower_bound = c(0.4478, 0.4108),
  upper_bound = c(0.5469, 0.5055)
)

data_c_col <- data.frame(
  group = c("Control (placebo)", "Control (ATX)"),
  mean = c(0.4067, 0.4154),
  lower_bound = c(0.3356, 0.3443),
  upper_bound = c(0.4788, 0.4903)
)

graph_c <-
  ggplot() +
  geom_point(size=3.5, data=data_c, aes(x = group, y = mean)) +
  geom_point(size=3.5, data=data_c_col, aes(x = group, y = mean), colour="magenta") +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound), data=data_c) +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound), data=data_c_col, colour="magenta") +
  xlab("") +
  ylab("Side bias") +
  geom_hline(yintercept = 0.5, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

data_theta <- data.frame(
  group = c("Control (placebo)", "Control (ATX)", "CUD (placebo)", "CUD (ATX)"),
  mean = c(0.2113414, 0.2166372, 0.6245831, 0.6997534),
  lower_bound = c(0.1306561, 0.1412251, 0.4089937, 0.4845647),
  upper_bound = c(0.2976413, 0.2966527, 0.847476, 0.9215298)
)

graph_theta <-
  ggplot() +
  geom_point(size=3.5, data=data_theta, aes(x = group, y = mean), colour="magenta") +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound), data=data_theta, colour="magenta") +
  xlab("") +
  ylab("Previous outcome effect") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

data_rho <- data.frame(
  group = c("Control (placebo)", "Control (ATX)", "CUD (placebo)", "CUD (ATX)"),
  mean = c(3.050044, 3.568554, 0.4224067, 0.7979415),
  lower_bound = c(2.21025, 2.610866, 0.2481344, 0.4613199),
  upper_bound = c(4.09986, 4.776793, 0.670333, 1.302454)
)

graph_rho <-
  ggplot(data_rho, aes(x = group, y = mean)) +
  geom_point(size=3.5) +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Loss/gain sensitivity") +
#  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  ylim(0,5) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


#Group differences on placebo

data_placebo_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(-5.08, -12.39, -2.64),
  lower_bound = c(-3.40, -6.56, -1.79),
  upper_bound = c(-6.59, -18.67, -3.73)
)

data_placebo_2 <- data.frame(
  parameter = c("Previous outcome", "Odds sensitivity"),
  mean = c(0.414, 0.797),
  lower_bound = c(0.6457, 1.298),
  upper_bound = c(0.188, 0.336)
)

data_placebo_3 <- data.frame(
  parameter = c("Colour bias", "Delay aversion"),
  mean = c(0.0901, 0.063),
  lower_bound = c(0.177, 0.111),
  upper_bound = c(0.002, 0.0197)
)

graph_placebo_1 <-
  ggplot(data_placebo_1, aes(x = parameter, y = mean)) +
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
        axis.ticks = element_line(size=1)
  )

graph_placebo_2 <-
  ggplot(data_placebo_2, aes(x = parameter, y = mean)) +
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
        text = element_text(size=14, face = "bold")
  )

graph_placebo_3 <-
  ggplot(data_placebo_3, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), colour="magenta") +
  xlab("") +
  ylab("95% HDI of group differences on placebo (CUD-control)") +
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

#Atomoxetine effect on control group specifically

data_control_1 <- data.frame(
  parameter = c("Colour bias", "Previous outcome", "Odds sensitivity"),
  mean = c(0.0086, 0.0054, 0.113),
  lower_bound = c(0.106, 0.0907, 0.24),
  upper_bound = c(-0.0902, -0.079, -0.0169)
)

data_control_1_col <- data.frame(
  parameter = c("Delay aversion"),
  mean = c(-0.0165),
  lower_bound = c(-0.00179),
  upper_bound = c(-0.0323)
)

data_control_2 <- data.frame(
  parameter = c("Colour consistency", "Loss/gain sensitivity"),
  mean = c(0.502, 0.515),
  lower_bound = c(1.46, 1.13),
  upper_bound = c(-0.418, -0.045)
)

data_control_2_col <- data.frame(
  parameter = c("Bet consistency"),
  mean = c(6.23),
  lower_bound = c(10.86),
  upper_bound = c(1.77)
)

graph_control_1 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_1_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_1) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_1_col, colour="magenta") +
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

graph_control_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_2_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of atomoxetine effect on controls (ATX-placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))

#Atomoxetine effects on CUD group

data_CUD_1 <- data.frame(
  parameter = c("Colour bias", "Delay aversion", "Previous outcome", "Odds sensitivity"),
  mean = c(-0.039, -0.00119, 0.0735, -0.0776),
  lower_bound = c(0.025, 0.0331, 0.259, 0.0243),
  upper_bound = c(-0.105, -0.0354, -0.112, -0.386)
)

data_CUD_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency"),
  mean = c(-0.198, -1.29),
  lower_bound = c(0.297, 0.95),
  upper_bound = c(-0.709, -3.64)
)

data_CUD_2_col <- data.frame(
  parameter = c("Loss/gain sensitivity"),
  mean = c(0.371),
  lower_bound = c(0.718),
  upper_bound = c(0.133)
)

graph_CUD_1 <-
  ggplot() +
  geom_point(size=3.5, data=data_CUD_1, aes(x = parameter, y = mean)) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_CUD_1) +
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

graph_CUD_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_CUD_2, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_CUD_2_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_CUD_2) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_CUD_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of atomoxetine effect on CUD (ATX-placebo)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))


#------------------------------------------------------------------------------------------------------
#Comparing control groups across datasets
#------------------------------------------------------------------------------------------------------

data_control_comparison_1 <- data.frame(
  parameter = c("Delay aversion", "Previous outcome", "Odds sensitivity"),
  mean = c(-0.0047, 0.0332, -0.11),
  lower_bound = c(-0.039, -0.103, -0.408),
  upper_bound = c(0.0288, 0.177, 0.184)
)

data_control_comparison_1_col <- data.frame(
  parameter = c("Colour bias"),
  mean = c(-0.0996),
  lower_bound = c(-0.197),
  upper_bound = c(-0.0029)
)

data_control_comparison_2 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(2.02, 3.49, 0.597),
  lower_bound = c(-0.0087, -4.30, -0.712),
  upper_bound = c(3.92, 11.87, 1.86)
)

graph_control_comparison_1 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_comparison_1, aes(x = parameter, y = mean)) +
  geom_point(size=3.5, data=data_control_comparison_1_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_comparison_1) +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_comparison_1_col, colour="magenta") +
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

graph_control_comparison_2 <-
  ggplot() +
  geom_point(size=3.5, data=data_control_comparison_2, aes(x = parameter, y = mean)) +
  #  geom_point(size=3.5, data=data_control_comparison_2_col, aes(x = parameter, y = mean), colour="magenta") +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_comparison_2) +
  #  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound), data=data_control_comparison_2_col, colour="magenta") +
  xlab("") +
  ylab("95% HDI of differences between control groups (ATX controls - NAL controls)") +
  geom_hline(yintercept = 0, linetype = "dashed", size=1) +
  theme(text = element_text(size=20)) +
  coord_flip() +
  theme_minimal() +
  theme(axis.line = element_line(size=0.5, colour="black"),
        panel.grid = element_line(colour="white"),
        axis.text = element_text(size=11, face = "bold"),
        axis.ticks = element_line(size=1),
        text = element_text(size=14, face = "bold"))
