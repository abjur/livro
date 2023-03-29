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

# histograma vira prob ----------------------------------------------------
set.seed(100)
tibble::tibble(
  id = 1:365,
  taxa_procedencia = rnorm(365, mean = 0.5, sd = 0.1)
) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262", bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 22, label = "Valor real"), color = "red") +
  ggplot2::scale_x_continuous(limits = c(0,1))

set.seed(100)
tibble::tibble(
  id = 1:365,
  taxa_procedencia = rnorm(365, mean = 0.5, sd = 0.1)
) |>
  dplyr::summarise(media = round(mean(taxa_procedencia), 2))


set.seed(100)
tibble::tibble(
  id = 1:365,
  taxa_procedencia = rnorm(365, mean = 0.5, sd = 0.1)
) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262", bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 22, label = "Valor real"), color = "red") +
  ggplot2::scale_x_continuous(limits = c(0,1)) +
  gganimate::transition_reveal(along = id)

tibble::tibble(
  decisao = sample(
    x = c("P", "I"),
    size = 100,
    replace = TRUE,
    prob = c(.5, .5))
  ) |>
  dplyr::count(decisao) |>
  dplyr::mutate(prop = n/sum(n)) |>
  dplyr::filter(decisao == "P") |>
  dplyr::pull(prop)

