#Lukas Cincikas 2021
# Old graph script, left for reference only.

library(rstan)

data_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(5.416, 16.207, 2.699),
  lower_bound = c(3.914, 10.531, 1.776),
  upper_bound = c(6.816, 22.526, 3.808)
)

data_2 <- data.frame(
  parameter = c("Previous outcome effect", "Risk adjustment"),
  mean = c(-0.448, -0.697),
  lower_bound = c(-0.663, -1.185),
  upper_bound = c(-0.244, -0.255)
)

data_2_additional <- data.frame(
  parameter = c("Side bias", "Impulsivity"),
  mean = c(-0.0666, -0.0704),
  lower_bound = c(-0.132, -0.113),
  upper_bound = c(-0.00146, -0.0315)
)

data_3 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(-0.155, -2.482, -0.447),
  lower_bound = c(-0.693, -5.163, -0.792),
  upper_bound = c(0.363, 0.047, -0.137)
)

data_4 <- data.frame(
  parameter = c("Side bias", "Impulsivity", "Previous outcome effect", "Risk adjustment"),
  mean = c(0.0152, 0.00889, -0.0402, -0.0158),
  lower_bound = c(-0.044, -0.01, -0.143, -0.185),
  upper_bound = c(0.0737, 0.0276, 0.0622, 0.147)
)

graph_1 <-
  ggplot(data_1, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of group means (Control-CUD) difference") +
  ylim(0, 24) +
  geom_hline(yintercept = 0, colour = "purple") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_2 <-
  ggplot(data_2, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of group means (Control-CUD) difference") +
  geom_hline(yintercept = 0, colour = "purple") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_2_additional <-
  ggplot(data_2_additional, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of group means (Control-CUD) difference") +
  geom_hline(yintercept = 0, colour = "purple") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_3 <-
  ggplot(data_3, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of group means (Placebo-atomoxetine) difference") +
  geom_hline(yintercept = 0, colour = "purple") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_4 <-
  ggplot(data_4, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of group means (Placebo-atomoxetine) difference") +
  geom_hline(yintercept = 0, colour = "purple") +
  theme(text = element_text(size=20)) +
  coord_flip()

data_c <- data.frame(
  group = c("Control (placebo)", "Control (ATX)", "CUD (placebo)", "CUD (ATX)"),
  mean = c(0.4064, 0.4157, 0.4972, 0.4579),
  lower_bound = c(0.3352, 0.343, 0.4485, 0.4099),
  upper_bound = c(0.4792, 0.4900, 0.5461, 0.5058)
)

graph_c <-
  ggplot(data_c, aes(x = group, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Side bias") +
  ylim(0.25,0.75) +
  theme(text = element_text(size=20)) +
  coord_flip()

data_theta <- data.frame(
  group = c("Control (placebo)", "Control (ATX)", "CUD (placebo)", "CUD (ATX)"),
  mean = c(0.1445, 0.1546, 0.692, 0.7515),
  lower_bound = c(0.03487, 0.05333, 0.4855, 0.5352),
  upper_bound = c(0.2588, 0.2649, 0.903, 0.9637)
)

graph_theta <-
  ggplot(data_theta, aes(x = group, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Previous outcome effect") +
  theme(text = element_text(size=20)) +
  coord_flip()

data_rho <- data.frame(
  group = c("Control (placebo)", "Control (ATX)", "CUD (placebo)", "CUD (ATX)"),
  mean = c(1.371, 1.787, 0.3132, 0.6161),
  lower_bound = c(1.0656, 1.381, 0.1864, 0.3675),
  upper_bound = c(1.7545, 2.2874, 0.4939, 0.9783)
)

graph_rho <-
  ggplot(data_rho, aes(x = group, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=group, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("Loss/gain sensitivity") +
  theme(text = element_text(size=20)) +
  coord_flip()
