#Lukas Cincikas 2021
#Old graph script, left for reference only.

library(rstan)

data_1 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(5.435, 10.435, 1.114),
  lower_bound = c(3.965, 5.575, 0.701),
  upper_bound = c(6.779, 15.47, 1.578)
)

data_2 <- data.frame(
  parameter = c("Side bias", "Impulsivity", "Previous outcome effect"),
  mean = c(-0.066, -0.074, -0.572),
  lower_bound = c(-0.132, -0.117, -0.779),
  upper_bound = c(-0.00252, -0.0349, -0.368)
)

data_3 <- data.frame(
  parameter = c("Colour consistency", "Bet consistency", "Loss/gain sensitivity"),
  mean = c(-0.15, -1.081, -0.35951),
  lower_bound = c(-0.271, -3.171, -0.53),
  upper_bound = c(0.374, 0.945, -0.213456)
)

data_4 <- data.frame(
  parameter = c("Side bias", "Impulsivity", "Previous outcome effect"),
  mean = c(0.015, 0.00774, -0.0348),
  lower_bound = c(-0.04324, -0.01188, -0.1568),
  upper_bound = c(0.07499, 0.02742, 0.09007)
)

graph_1 <-
ggplot(data_1, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of difference (Control-CUD) group means") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_2 <-
  ggplot(data_2, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of difference (Control-CUD) group means") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_3 <-
  ggplot(data_3, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of difference (Placebo-atomoxetine) group means") +
  theme(text = element_text(size=20)) +
  coord_flip()

graph_4 <-
  ggplot(data_4, aes(x = parameter, y = mean)) +
  geom_point() +
  geom_errorbar(aes(x=parameter, ymin=lower_bound, ymax=upper_bound)) +
  xlab("") +
  ylab("95% HDI of difference (Placebo-atomoxetine) group means") +
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
