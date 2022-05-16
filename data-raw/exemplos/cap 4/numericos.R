# Visualizações das variáveis quantitativas

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

consumo <- abjData::consumo


# histograma --------------------------------------------------------------

consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = log10(tempo)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 5) +
  ggplot2::labs(
    title = "Histograma simples",
    x = "Tempo (em dias)",
    y = "Quantidade de casos"
  )

# histograma com bins diferentes --------------------------------------------------------------

bin_grande <- consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 5) +
  ggplot2::labs(
    title = "Histograma com 5 intervalos",
    x = "Tempo (em dias)",
    y = "Quantidade de casos"
  )


bin_padrao <- consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 30) +
  ggplot2::labs(
    title = "Histograma com 30 intervalos",
    x = "Tempo (em dias)",
    y = "Quantidade de casos"
  )


bin_pequeno <- consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 200) +
  ggplot2::labs(
    title = "Histograma com 200 intervalos",
    x = "Tempo (em dias)",
    y = "Quantidade de casos"
  )

gridExtra::grid.arrange(bin_grande, bin_padrao, bin_pequeno,
                        top=grid::textGrob("O mesmo histograma com intervalos distintos para as barras"),
                        nrow = 1)


# histograma com valor ----------------------------------------------------

options(scipen = 999)
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 30) +
  ggplot2::labs(
    title = "Histograma de valores",
    x = "Valor (em reais)",
    y = "Quantidade de casos"
  )


# histograma com valor filtrando outliers ---------------------------------

consumo |>
  dplyr::filter(valor < quantile(valor, 0.90)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 30) +
  ggplot2::labs(
    title = "Histograma de valores (filtrando os 10% maiores processos)",
    x = "Valor (em reais)",
    y = "Quantidade de casos"
  )


# tabela valores originais e valores transformados em log10 ---------------

options(scipen=999)
set.seed()
consumo |>
  dplyr::sample_n(10, cluster) |>
  dplyr::transmute(
    id_processo,
    valor,
    valor_em_log = log10(valor)
  ) |>
  knitr::kable()

# histograma com valor em log ---------------------------------------------
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = log10(valor)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::labs(
    title = "Histograma de valores (com transformação em log de base 10)",
    x = "Valor em log (em reais)",
    y = "Quantidade de casos"
  )
