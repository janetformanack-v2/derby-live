plot_blocker_stats <- function(data, team_color) {
  data |> 
    # transform
    select(
      team,
      blocker,
      n_jams,
      percent_jams_lead,
      percent_jams_penalty,
      n_points_sum
    ) |> 
    tidyr::pivot_longer(
      cols = matches("^n_|^percent_"),
      names_to = "what",
      values_to = "value"
    ) |> 
    mutate(
      what = case_match(
        what,
        "n_jams" ~ "Number of Jams",
        "n_points_sum" ~ "Total points (diff)",
        "percent_jams_lead" ~ "Percent Lead",
        "percent_jams_penalty" ~ "Percent Penalty"
      ) |> 
        factor(
          levels = c(
            "Number of Jams",
            "Total points (diff)",
            "Percent Lead",
            "Percent Penalty"
          )
        )
    ) |>
    arrange(
      what,
      desc(value)
    ) |> 
    group_by(
      what
    ) |> 
    mutate(
      rank = row_number()
    ) |> 
    ungroup() |> 
    filter(
      rank <= 10
    ) |> 
    # group_by(
    #   team,
    #   what,
    #   value
    # ) |>
    # reframe(
    #   blocker = paste(unique(blocker), collapse = ",")
    # ) |>
    # plot
    ggplot(
      aes(
        x = value,
        y = forcats::fct_reorder(
          as.factor(blocker),
          value
        ),
        group = what,
        color = team,
        fill = team
      )
    ) +
    facet_wrap(
      ~ what,
      ncol = 2,
      nrow = 2,
      scales = "free"
    ) +
    geom_vline(
      xintercept = 0,
      col = "red",
      linewidth = 2
    ) +
    geom_bar(
      stat = "identity"
    ) +
    # geom_point(
    #   shape = 21,
    #   col = "white",
    #   size = 7,
    #   alpha = 0.7,
    #   stroke = 2
    # ) +
    labs(
      title = "",
      x = "",
      y = ""
    ) +
    # scale_y_continuous(
    #   limits = c(0, max(scores_now()) + 10),
    #   breaks = scales::breaks_pretty()
    # ) +
    scale_x_continuous(
      # breaks = scales::breaks_pretty(),
      n.breaks = 4
    ) +
    theme_minimal(20) +
    scale_fill_manual(
      # values = c("#B22EF0", "grey10")
      values = team_color
    ) +
    scale_color_manual(
      values = team_color
      # values = c("#B22EF0", "grey10")
    ) +
    theme(
      legend.position = "none",
      plot.title = element_blank(),
      legend.title = element_blank(),
      axis.title.y = element_blank(),
      axis.text = element_text(size = 16),
      axis.title.x = element_blank()
    )
}