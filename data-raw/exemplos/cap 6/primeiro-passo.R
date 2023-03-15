# primeiro passo


# tabela de frequencia vira prob ------------------------------------------

abjData::consumo |>
  dplyr::mutate(
    dec_val = dplyr::case_when(
      dec_val %in% c("Parcial", "Reformou") ~ "Procedente (Parcial ou Total)",
      TRUE ~ "Improcedente"
    )
  ) |>
  dplyr::count(dec_val) |>
  dplyr::mutate(
    prop = n/sum(n),
    prop = formattable::percent(prop)
  )

# histograma vira prob ----------------------------------------------------
tibble::tibble(
  id = 1:365,
  taxa_procedencia = rnorm(365, mean = 0.5, sd = 0.1)
) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262") +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::scale_x_continuous(limits = c(0,1)) +
  ggplot2::scale_y_continuous(limits = c(0,60)) +
  gganimate::transition_reveal(taxa_procedencia)

