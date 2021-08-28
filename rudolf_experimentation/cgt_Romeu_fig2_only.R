mkRomeuFig2 <- function(d)
{
  n_locations <- 10
  working <- copy(d)
  working[, ascending := ifelse(ascending_bets, "ascending", "descending")]
  working[, red_to_blue_ratio := paste0(n_red, ":", n_locations - n_red)]
  by_subject <- (
    working %>%
      group_by(group_name, subject_name, ascending, red_to_blue_ratio) %>%
      summarize(
        average_colour_choice = mean(chose_red),
        average_bet_ratio = mean(proportion_staked),
        .groups = "drop"
        # https://stackoverflow.com/questions/62140483/how-to-interpret-dplyr-message-summarise-regrouping-output-by-x-override
      ) %>%
      as.data.table()
  )
  
  #Converts strings into numbers
  by_subject_length <- length(by_subject$group_name)
  for(j in 1:by_subject_length) {
    string <- str_sub(by_subject$subject_name[j], 9)
    convert_string <- strtoi(string)
    by_subject$subject_number[j] <- convert_string
  }
  
  #Creates the 4 separate groups to differentiate by condition
  for(i in 1:by_subject_length) {
    if ((by_subject$subject_number[i] %% 2) == 1 && by_subject$group_name[i] == "group_1") {
      by_subject$group_name[i] <- 1
    } else if ((by_subject$subject_number[i] %% 2) == 0 && by_subject$group_name[i] == "group_1") {
      by_subject$group_name[i] <- 2
    } else if ((by_subject$subject_number[i] %% 2) == 1 && by_subject$group_name[i] == "group_2") {
      by_subject$group_name[i] <- 3
    } else {
      by_subject$group_name[i] <- 4
    }
  }
  
  by_group <- (
    by_subject %>%
      group_by(group_name, ascending, red_to_blue_ratio) %>%
      summarize(
        mean_colour_choice = mean(average_colour_choice),
        sem_colour_choice = miscstat$sem(average_colour_choice),
        mean_bet_ratio = mean(average_bet_ratio),
        sem_bet_ratio = miscstat$sem(average_bet_ratio),
        .groups = "drop"
      ) %>%
      as.data.table()
  )
  
  for(k in 1:length(by_group$group_name)) {
    if (by_group$group_name[k] == 1) {
      by_group$CUD_group[k] <- 1
      by_group$condition[k] <- 1
    } else if (by_group$group_name[k] == 2) {
      by_group$CUD_group[k] <- 1
      by_group$condition[k] <- 2
    } else if (by_group$group_name[k] == 3) {
      by_group$CUD_group[k] <- 2
      by_group$condition[k] <- 1
    } else {
      by_group$CUD_group[k] <- 2
      by_group$condition[k] <- 2
    }
  }
  
  theme <- theme_bw()
  fig_a <- (
    ggplot(
      by_group,
      aes(
        x = red_to_blue_ratio,
        y = mean_colour_choice,
        colour = factor(CUD_group),
        group = group_name,
        linetype = factor(condition),
        shape = factor(condition)
      )
    ) +
      theme +
      scale_colour_manual(name = "Group", values = c("black", "orange")) +
      geom_line() +
      geom_errorbar(aes(
        ymin = mean_colour_choice - sem_colour_choice,
        ymax = mean_colour_choice + sem_colour_choice
      )) +
      geom_point() +
      facet_grid(. ~ ascending) +
      xlab("Red:blue colour ratio") +
      ylab("Colour choice (0 = blue, 1 = red)") +
      ylim(0, 1)
  )
  fig_b <- (
    ggplot(
      by_group,
      aes(
        x = red_to_blue_ratio,
        y = mean_bet_ratio,
        colour = factor(CUD_group),
        group = group_name,
        linetype = factor(condition),
        shape = factor(condition)
      )
    ) +
      theme +
      scale_colour_manual(name = "Group", values = c("black", "orange")) +
      geom_line() +
      geom_errorbar(aes(
        ymin = mean_bet_ratio - sem_bet_ratio,
        ymax = mean_bet_ratio + sem_bet_ratio
      )) +
      geom_point() +
      facet_grid(. ~ ascending) +
      xlab("Red:blue colour ratio") +
      ylab("Bet size (proportion of points)") +
      ylim(0, 1)
  )
  f <- (fig_a / fig_b + plot_layout(guides = "collect"))
  return(f)
}

#plot_layout(guides = "collect")

mock_fig_2x2g <<- mkRomeuFig2(mock_data_2x2g)
mock_fig_2x2g