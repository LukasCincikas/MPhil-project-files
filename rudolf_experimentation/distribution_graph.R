#Lukas Cincikas 2021
#Old script that creates violin graphs of parameter distributions. Left for reference only.

library(rstan)
library(viridis)
library(hrbrthemes)
library(extrafont)
setwd("C:/Users/Lukas Cincikas/Documents/R/MPhil project files/rudolf_experimentation/fitcache")
fit <- readRDS("fit_real_data_M12_loss_chasing_3C.rds")
data <- extract(fit)

data_rho_means <- data.frame(
  name = c(rep("Control+placebo",12000), rep("Control+atomoxetine",12000), rep("CUD+placebo",12000), rep("CUD+atomoxetine",12000)),
  value = c(data$group_mean_rho[,1], data$group_mean_rho[,2], data$group_mean_rho[,3], data$group_mean_rho[,4]),
  cutoff_lower = c(rep(1.066,12000), rep(1.381,12000), rep(0.1864,12000), rep(0.3675,12000)),
  cutoff_upper = c(rep(1.755,12000), rep(2.287,12000), rep(0.4939,12000), rep(0.9783,12000))
)

#ggplot(data_rho_means, aes(x=name, y=value, fill=name)) + geom_violin()

data_rho_group_effect <- data.frame(
  name = rep("Control-CUD means",12000),
  value = data$overall_group_effect_rho
)

data_rho_condition_effect <- data.frame(
  name = rep("Placebo-atomoxetine means",12000),
  value = data$overall_condition_effect_rho
)

windowsFonts("Roboto Condensed" = windowsFont("Roboto Condensed"))

ggplot(data_rho_means, aes(x=name, y=value, fill=name, color=name)) +
  geom_violin(width=1, size=0.2) +
  scale_fill_viridis(discrete=TRUE) +
  scale_color_viridis(discrete=TRUE) +
  theme_ipsum(base_family = "Roboto Condensed") +
  theme(
    legend.position="none"
  ) +
  coord_flip() + # This switches X and Y axis and allows to get the horizontal version
  xlab("") +
  ylab("Rho")

#  geom_errorbar(aes(x=name, ymin=cutoff_lower, ymax=cutoff_upper), color="white") +
