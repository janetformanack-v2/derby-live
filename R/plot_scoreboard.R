plot_scoreboard <- function(data, 
                            team_colors) {
  
  max_score <- max(data$cum_points)
  
  data |> 
    ggplot(
      aes(
        x = jam_number,
        y = cum_points,
        color = team,
        fill = team
      )
    ) +
    geom_line(
      linewidth = 3,
      alpha = 0.8
    ) +
    geom_point(
      shape = 21,
      col = "white",
      size = 5,
      alpha = 0.7,
      stroke = 2
    ) +
    labs(
      x = "Jam Number",
      y = ""
    ) +
    scale_y_continuous(
      limits = c(0, max_score + 10),
      breaks = scales::breaks_pretty()
    ) +
    scale_x_continuous(
      breaks = scales::breaks_pretty()
    ) +
    theme_minimal(24) +
    scale_fill_manual(
      values = team_colors
    ) +
    scale_color_manual(
      values = team_colors
    ) +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    )
}