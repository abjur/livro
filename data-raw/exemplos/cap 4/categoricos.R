# visualizações de variáveis categóricas

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

consumo <- abjData::consumo |>
  dplyr::filter(dec_val == "Reformou" |
                  dec_val == "Parcial" |
                  dec_val == "Não reformou")

# gráfico de barras (univariado) ------------------------------------------

bar_vert1 <- consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = dec_val, x = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::ylab("Decisão de segunda instância") +
  ggplot2::xlab("Quantidade")


bar_hor1 <- consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = dec_val, y = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::xlab("Decisão de segunda instância") +
  ggplot2::ylab("Quantidade")

gridExtra::grid.arrange(bar_vert1, bar_hor1,top=grid::textGrob("Gráficos de barras na vertical e na horizontal"))


# gráfico de barras (univariado prop) -------------------------------------
consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(
    prop = n / sum(n),
    dec_val = stringr::str_replace_all(dec_val, "/", "/ \n"),
    group = 1
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = dec_val, y = ..prop.., group = 1) +
  ggplot2::geom_bar(fill = cores_abj[1], stat = 'count')
  ggplot2::ylab("Decisão de segunda instância") +
  ggplot2::xlab("Quantidade")

# gráfico de barras (bivariado 1) ------------------------------------------

bar_vert1 <- consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = dec_val, x = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::ylab("Decisão de segunda instância") +
  ggplot2::xlab("Quantidade")

bar_vert2 <- consumo |>
  dplyr::count(dec_val, tipo_litigio) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = dec_val, fill = tipo_litigio, x= n) +
  ggplot2::geom_col() +
  ggplot2::labs(
    x = "Decisão de segunda instância",
    y = "Quantidade"
  ) +
  ggplot2::theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  ggplot2::guides(fill=guide_legend("Tipo de litígio", nrow=2, byrow=TRUE))


gridExtra::grid.arrange(
  bar_vert1,
  bar_vert2,
  top=grid::textGrob("Gráficos de barras na vertical quebrado em uma segunda variável"),
  nrow = 1
  )

# gráfico de barras (bivariado 2) ------------------------------------------

bar_hor <- consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = dec_val, y = n) +
  ggplot2::geom_col(fill = cores_abj[1]) +
  ggplot2::labs(
    x = "Decisão de segunda instância",
    y = "Quantidade"
  ) +
  ggplot2::theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  ggplot2::guides(fill=guide_legend("Tipo de litígio", nrow=2, byrow=TRUE))



bar_hor2 <- consumo |>
  dplyr::count(dec_val, tipo_litigio) |>
  dplyr::mutate(prop = n / sum(n)) |>
  dplyr::mutate(dec_val = stringr::str_replace_all(dec_val, "/", "/ \n")) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = dec_val, fill = tipo_litigio, y= n) +
  ggplot2::geom_col(position = 'dodge') +
  ggplot2::labs(
    x = "Decisão de segunda instância",
    y = "Quantidade"
  ) +
  ggplot2::theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  ggplot2::guides(fill=guide_legend("Tipo de litígio", nrow=2, byrow=TRUE))


gridExtra::grid.arrange(bar_hor, bar_hor2,top=grid::textGrob("Gráficos de barras na horizontal quebrado em uma segunda variável"))


# gráficos com highlights 1 -------------------------------------------------

consumo |>
  dplyr::count(dec_val) |>
  dplyr::mutate(
    prop = n / sum(n),
    dec_val = stringr::str_replace_all(dec_val, "/", "/ \n"),
    highlight = dplyr::case_when(
      dec_val == "Reformou" | dec_val == "Não reformou" ~ TRUE,
      TRUE ~ FALSE
      )
    ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = dec_val, y = n, fill = highlight) +
  ggplot2::geom_col() +
  ggplot2::scale_fill_manual(values = c("TRUE"=cores_abj[1], "FALSE"="gray"), guide = "none") +
  ggplot2::labs(
    title = "Gráfico de barras com comparação entre os grupos 'Reformou' e 'Não reformou'",
    x = "Decisão de segunda instância",
    y = "Quantidade"
  )

# gráficos com highlights 2 -------------------------------------------------

consumo1 <- consumo |>
  dplyr::count(dec_val, tipo_litigio) |>
  dplyr::transmute(
    dec_val = stringr::str_replace_all(dec_val, "/", "/ \n"),
    tipo_litigio,
    n,
    prop = n / sum(n)
  )

subset_consumo1 <- consumo1 |>
  dplyr::mutate(
    highlight = dplyr::if_else(
      condition = dec_val == "Não reformou" &
        (tipo_litigio == "nPF-nPF" | tipo_litigio == "PF-nPF"),
      true = n,
      false = NA_integer_
    )
  )

ggplot2::ggplot() +
  ggplot2::geom_col(
    data = consumo1,
    ggplot2::aes(x = dec_val, y= n, group = tipo_litigio),
    fill = "grey",
    alpha = .6,
    position = 'dodge') +
  ggplot2::geom_col(
    data = subset_consumo1,
    ggplot2::aes(x = dec_val, y = highlight, fill = tipo_litigio),
    position = 'dodge') +
  ggplot2::labs(
    x = "Decisão de segunda instância",
    y = "Quantidade"
  ) +
  ggplot2::theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  ggplot2::guides(fill=guide_legend("Tipo de litígio", nrow=2, byrow=TRUE))



# gráficos com highlights 3 -------------------------------------------------

consumo2 <- consumo |>
  dplyr::count(dec_val, tipo_litigio) |>
  dplyr::transmute(
    dec_val = stringr::str_replace_all(dec_val, "/", "/ \n"),
    tipo_litigio,
    n,
    prop = n / sum(n)
  )

subset_consumo2 <- consumo1 |>
  dplyr::mutate(
    highlight = dplyr::if_else(
      condition = tipo_litigio == "PF-nPF",
      true = n,
      false = NA_integer_
    )
  )

ggplot2::ggplot() +
  ggplot2::geom_col(
    data = consumo2,
    ggplot2::aes(x = dec_val, y= n, group = tipo_litigio),
    fill = "grey",
    alpha = .6,
    position = 'dodge') +
  ggplot2::geom_col(
    data = subset_consumo2,
    ggplot2::aes(x = dec_val, y = highlight, fill = tipo_litigio),
    position = 'dodge') +
  ggplot2::labs(
    x = "Decisão de segunda instância",
    y = "Quantidade"
  ) +
  ggplot2::theme(
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top"),
    legend.box.just = "right",
    legend.margin = margin(6, 6, 6, 6),
    legend.background = element_rect(fill = "white", color = "black")
  ) +
  ggplot2::guides(fill=guide_legend("Tipo de litígio", nrow=2, byrow=TRUE))




