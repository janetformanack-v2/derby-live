plot_jammer_stats <- function(data) {
  data |> 
    ggplot(
      aes(
        x = value,
        y = jammer,
        color = team,
        fill = team
      )
    ) +
    facet_wrap(
      ~ what,
      ncol = 2,
      nrow = 2
      # scales = "free_x"
    ) +
    geom_bar(
      stat = "identity"
    ) +
    geom_point(
      shape = 21,
      col = "white",
      size = 7,
      alpha = 0.7,
      stroke = 2
    ) +
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
      breaks = scales::breaks_pretty()
    ) +
    theme_bw(24) +
    scale_fill_manual(
      values = c("#B22EF0", "grey10")
    ) +
    scale_color_manual(
      values = c("#B22EF0", "grey10")
    ) +
    theme(
      legend.position = "none",
      legend.title = element_blank()
    )
}