# library(ggplot2)
# library(ggforce)
p <- ggplot2::ggplot() +
  ggplot2::theme_void(base_size = 12) +
  ggplot2::theme(
    panel.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    plot.background = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.key = ggplot2::element_rect(fill = "transparent", colour = NA),
    legend.background = ggplot2::element_rect(fill = "transparent", colour = NA)
  ) +
  ggplot2::geom_polygon(
    data = data.frame(
      x = 1 + c(rep(-sqrt(3)/2, 2), 0, rep(sqrt(3)/2, 2), 0),
      y = 1 + c(0.5, -0.5, -1, -0.5, 0.5, 1)
    )[c(1, 1:6), ],
    mapping = ggplot2::aes(x = x, y = y),
    size = 1.2,
    fill = "goldenrod1",
    colour = "goldenrod4"
  ) +
  ggforce::geom_regon(
    ggplot2::aes(
      x0 = c(1, 1.06, 1.06),
      y0 = c(1, 1.05, 1),
      sides = 3,
      angle = c(0.12, -0.3, -0.65),
      r = c(0.90, 0.82, 0.85),
      fill = c("A", "B", "C"),
      alpha = c("A", "B", "C")
    ),
    radius = ggplot2::unit(0.08, 'npc'),
    expand = ggplot2::unit(0.015, 'npc')
  ) +
  ggplot2::annotate(geom = "text", x = 1, y = 1, label = "NACHO", size = 7, colour = "grey25") +
  ggplot2::scale_alpha_manual(values = c(0.95, 0.90, 0.80)*0.9) +
  ggplot2::scale_fill_manual(values = c("#c64544", "#e39c33", "#f2f659")) +
  ggplot2::coord_fixed() +
  ggplot2::theme(legend.position = "none") +
  ggplot2::scale_x_continuous(expand = ggplot2::expand_scale(add = 0.02)) +
  ggplot2::scale_y_continuous(expand = ggplot2::expand_scale(add = 0.02))
ggplot2::ggsave(
  filename = "./inst/logo/nacho_hex_r.png",
  plot = p,
  bg = "transparent",
  width = 1.73,
  height = 2,
  units = "in",
  dpi = 300
)


# devtools::install_github("GuangchuangYu/hexSticker")
# library(hexSticker)
hexSticker::sticker(
  subplot = "./inst/logo/Nacho_logo_notext.png",
  package = "NACHO",
  p_size = 24,
  p_y = 0.93,
  p_color = "grey25",
  s_x = 1,
  s_y = 1.02,
  s_width = 0.8,
  s_height = 0.8,
  h_fill = "goldenrod1",
  h_color = "goldenrod4",
  filename = "./inst/logo/nacho_hex.png"
)
