theme_bw <- theme_bw() +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank())

big_trend_theme <- theme_bw() +
  theme(axis.title.x = element_blank(),
        text = element_text(size=11),
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        plot.title = element_text(size = 12))