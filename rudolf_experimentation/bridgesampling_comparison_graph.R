# Lukas Cincikas 2021
# Creates a graph comparing model fit using items in the environment. To be used in conjunction with bridgesampling_comparison.R

library(ggplot2)

in_order <- NA

for(i in 1:length(model_comparison_nal$index)) { #Replace 'model_comparison_nal' with appropriate item as needed
  model_comparison_nal$order[i] <- i
  in_order[i] <- toString(i)
}

model_comparison_plot <- 
  ggplot(model_comparison_nal, aes(x=model_name, y=log_posterior_p_model)) +
  geom_col(fill="black") +
  scale_y_reverse() +
#  scale_x_discrete(limits = in_order) +
  theme_minimal() +
  ylab("Log posterior of model likelihood") +
  xlab("Model comparison for naltrexone dataset") +
  theme(axis.line.y = element_line(size=0.5, colour="black"),
        axis.ticks.y = element_line(size=0.5),
        axis.text = element_text(size=11, face = "bold"),
        axis.title = element_text(size=11, face = "bold")
        ) +
  geom_hline(yintercept = 0, size=0.5)

#  theme(axis.text.x = element_text(angle=60, vjust=0.5))
