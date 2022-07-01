# Visualizações das variáveis quantitativas

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

consumo <- abjData::consumo

format_reais <- function(valor) {
  real <- paste("R$", format(valor, decimal.mark = ",", big.mark = ".", nsmall = 2))
  return(real)
}

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

patchwork::wrap_plots(
  bin_grande, bin_padrao, bin_pequeno,
  nrow = 3
) +
  patchwork::plot_annotation(title = "O mesmo histograma com intervalos distintos para as barras")

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


# histograma em escala log com valores em log ---------------------------------------------
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = log10(valor)) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::labs(
    title = "Histograma de valores (com transformação em log de base 10)",
    x = "Valor em escala logarítmica (valores em log)",
    y = "Quantidade de casos"
  )


# histograma em escala log com valores reais ---------------------------------------------
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60) +
  ggplot2::labs(
    title = "Histograma de valores (com transformação em log de base 10)",
    x = "Valor em escala logarítmica (valores em reais)",
    y = "Quantidade de casos"
  ) +
  ggplot2::scale_x_log10()


# Tabela com valores principais pro boxplot -----------------------------------------------------------------

set.seed(909)
sub_set_consumo <- consumo |>
  dplyr::sample_n(30)

sub_set_consumo |>
  dplyr::summarise(
    `valor máximo` = max(valor),
    q3 = quantile(valor, 0.75),
    `mediana` = quantile(valor, 0.5),
    q1 = quantile(valor, 0.25),
    `valor mínimo` = min(valor),
    iqr = q3-q1,
    bigode_superior = q3 + (3/2 * iqr),
    bigode_inferior = q1 - (3/2 * iqr),
    `último ponto superior` = max(valor[valor < bigode_superior]),
    `último ponto inferior` = min(valor[valor > bigode_inferior]),
    pontos_superiores = length(valor[valor > bigode_superior]),
    pontos_inferiores = length(valor[valor < bigode_inferior])
  ) |>
  dplyr::mutate(
    dplyr::across(
      .fns = ~round(.x, 2)
    )
  ) |>
  dplyr::rename(
    `quartil superior` = q3,
    `quartil inferior` = q1,
    `IQR` = iqr,
    `quartil superior + 3/2 IQR` = bigode_superior,
    `quartil inferior - 3/2 IQR` = bigode_inferior,
    `quantidade de pontos superiores` = pontos_superiores,
    `quantidade de pontos inferiores` = pontos_inferiores

  ) |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = "valores") |>
  tidyr::pivot_longer(!rowname, names_to = "medidas", values_to = "valores") |>
  tidyr::pivot_wider(names_from = "rowname", values_from = "valores") |>
  dplyr::transmute(
    medidas,
    `parte do boxplot` = dplyr::case_when(
      medidas == "quartil inferior" |
        medidas == "mediana" |
        medidas == "quartil superior" ~ "centro",

      medidas == "valor máximo" |
        medidas == "valor mínimo" |
        stringr::str_detect(medidas, "último") |
        stringr::str_detect(medidas, "IQR") ~ "bigodes",
      TRUE ~ "pontos"
    ),
    valores = dplyr::case_when(
      !stringr::str_detect(medidas, "quantidade de pontos") ~ format_reais(valores),
      TRUE ~ paste0(valores, " pontos")
    )
  ) |>
  knitr::kable()

resumo_valores <- sub_set_consumo |>
  dplyr::summarise(
    max = max(valor),
    q3 = quantile(valor, 0.75),
    mediana = quantile(valor, 0.5),
    q1 = quantile(valor, 0.25),
    min = min(valor),
    iqr = q3-q1,
    bigode_superior = q3 + (3/2 * iqr),
    bigode_inferior = q1 - (3/2 * iqr),
    ponto_superior = max(valor[valor < bigode_superior]),
    ponto_inferior = min(valor[valor > bigode_inferior]),
    n_pontos_superiores = length(valor[valor > bigode_superior]),
    n_pontos_inferiores = length(valor[valor < bigode_inferior])
  ) |>
  dplyr::mutate(
    dplyr::across(
      .fns = ~round(.x, 2)
    )
  )

# boxplot valor -----------------------------------------------------------

options(scipen = 999)
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_boxplot() +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )



# tabela do boxplot completo ----------------------------------------------

consumo |>
  dplyr::summarise(
    `valor máximo` = max(valor),
    q3 = quantile(valor, 0.75),
    `mediana` = quantile(valor, 0.5),
    q1 = quantile(valor, 0.25),
    `valor mínimo` = min(valor),
    iqr = q3-q1,
    bigode_superior = q3 + (3/2 * iqr),
    bigode_inferior = q1 - (3/2 * iqr),
    `último ponto superior` = max(valor[valor < bigode_superior]),
    `último ponto inferior` = min(valor[valor > bigode_inferior]),
    pontos_superiores = length(valor[valor > bigode_superior]),
    pontos_inferiores = length(valor[valor < bigode_inferior])
  ) |>
  dplyr::mutate(
    dplyr::across(
      .fns = ~round(.x, 2)
    )
  ) |>
  dplyr::rename(
    `quartil superior` = q3,
    `quartil inferior` = q1,
    `IQR` = iqr,
    `quartil superior + 3/2 IQR` = bigode_superior,
    `quartil inferior - 3/2 IQR` = bigode_inferior,
    `quantidade de pontos superiores` = pontos_superiores,
    `quantidade de pontos inferiores` = pontos_inferiores

  ) |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = "valores") |>
  tidyr::pivot_longer(!rowname, names_to = "medidas", values_to = "valores") |>
  tidyr::pivot_wider(names_from = "rowname", values_from = "valores") |>
  dplyr::transmute(
    medidas,
    `parte do boxplot` = dplyr::case_when(
      medidas == "quartil inferior" |
        medidas == "mediana" |
        medidas == "quartil superior" ~ "centro",

      medidas == "valor máximo" |
        medidas == "valor mínimo" |
        stringr::str_detect(medidas, "último") |
        stringr::str_detect(medidas, "IQR") ~ "bigodes",
      TRUE ~ "pontos"
    ),
    valores = dplyr::case_when(
      !stringr::str_detect(medidas, "quantidade de pontos") ~ format_reais(valores),
      TRUE ~ paste0(valores, " pontos")
    )
  ) |>
  knitr::kable()


# boxplot valores em log --------------------------------------------------


options(scipen = 999)
consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_boxplot() +
  ggplot2::scale_x_log10(breaks=10^c(3,4,5,6,7,8)) +
  ggplot2::theme(
    axis.title.y = ggplot2::element_blank(),
    axis.text.y = ggplot2::element_blank(),
    axis.ticks.y = ggplot2::element_blank()
  )


# tabela boxplot valores em log -------------------------------------------

consumo |>
  dplyr::mutate(
    valores_sem_log = valor,
    valor = log10(valor)
  ) |>
  dplyr::summarise(
    `valor máximo` = max(valor),
    q3 = quantile(valor, 0.75),
    `mediana` = quantile(valor, 0.5),
    q1 = quantile(valor, 0.25),
    `valor mínimo` = min(valor),
    iqr = q3-q1,
    bigode_superior = q3 + (3/2 * iqr),
    bigode_inferior = q1 - (3/2 * iqr),
    `último ponto superior` = max(valor[valor < bigode_superior]),
    `último ponto inferior` = min(valor[valor > bigode_inferior]),
    pontos_superiores = length(valor[valor > bigode_superior]),
    pontos_inferiores = length(valor[valor < bigode_inferior])
  ) |>
  dplyr::mutate(
    dplyr::across(
      .fns = ~round(.x, 2)
    )
  ) |>
  dplyr::rename(
    `quartil superior` = q3,
    `quartil inferior` = q1,
    `IQR` = iqr,
    `quartil superior + 3/2 IQR` = bigode_superior,
    `quartil inferior - 3/2 IQR` = bigode_inferior,
    `quantidade de pontos superiores` = pontos_superiores,
    `quantidade de pontos inferiores` = pontos_inferiores

  ) |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = "valores") |>
  tidyr::pivot_longer(!rowname, names_to = "medidas", values_to = "valores") |>
  tidyr::pivot_wider(names_from = "rowname", values_from = "valores") |>
  dplyr::transmute(
    medidas,
    `parte do boxplot` = dplyr::case_when(
      medidas == "quartil inferior" |
        medidas == "mediana" |
        medidas == "quartil superior" ~ "centro",

      medidas == "valor máximo" |
        medidas == "valor mínimo" |
        stringr::str_detect(medidas, "último") |
        stringr::str_detect(medidas, "IQR") ~ "bigodes",
      TRUE ~ "pontos"
    ),
    `valores em log` = dplyr::case_when(
      !stringr::str_detect(medidas, "quantidade de pontos") ~ as.character(valores),
      TRUE ~ paste0(valores, " pontos")
    )
  ) |>
  knitr::kable()



# histograma com explicativa categórica -----------------------------------

consumo |>
  dplyr::filter(
    dec_val == "Não reformou" |
      dec_val == "Reformou" |
      dec_val == "Parcial"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo, fill = dec_val) +
  ggplot2::geom_histogram() +
  gghighlight::gghighlight() +
  ggplot2::facet_wrap(~ dec_val)

# boxplot com explicativa categórica -----------------------------------

consumo |>
  dplyr::filter(
    dec_val == "Não reformou" |
      dec_val == "Reformou" |
      dec_val == "Parcial"
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = tempo, y = dec_val, fill = dec_val) +
  ggplot2::geom_boxplot(show.legend = FALSE)


# scatterplot -------------------------------------------------------------

p_positivo <- readr::read_csv("~/Downloads/obsCIEE.csv") |>
  dplyr::filter(ano == min(ano)) |>
  dplyr::mutate(
    congestionamento = baixados / (novos + pendentes),
    novos_por_100k = novos/pop * 100000
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = novos_por_100k, x = idhm) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method=lm, se=FALSE, color = cores_abj[1]) +
  ggplot2::labs(
    title = "Gráfico de dispersão com relação positiva",
    y = "Casos novos por 100 mil habitantes",
    x = "IDH"
  )


p_negativo <- readr::read_csv("~/Downloads/obsCIEE.csv") |>
  dplyr::filter(ano == min(ano)) |>
  dplyr::mutate(
    congestionamento = baixados / (novos + pendentes),
    novos_por_100k = novos/pop * 100000
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(y = novos_por_100k, x = t_analf18m) +
  ggplot2::geom_point() +
  ggplot2::geom_smooth(method=lm, se=FALSE, color = cores_abj[1]) +
  ggplot2::labs(
    title = "Gráfico de dispersão com relação negativa",
    y = "Casos novos por 100 mil habitantes",
    x = "Taxa de analfabetismo - 18 anos ou mais"
  )

gridExtra::grid.arrange(p_positivo, p_negativo, ncol = 2)


# time series -------------------------------------------------------------
norte <- c("AM", "TO", "AP", "AC", "RR", "RO")
nordeste <- c("MA", "BA", "PE", "SE", "AL", "RN", "PB", "CE", "PI", "PA")
centro_oeste <- c("GO", "MT", "MS", "DF")
sudeste <- c("SP", "MG", "RJ", "ES")
sul <- c("RS", "PR", "SC")

readr::read_csv("~/Downloads/obsCIEE.csv") |>
  dplyr::mutate(
    regiao = dplyr::case_when(
      tribunal_uf %in% norte ~ "Norte",
      tribunal_uf %in% nordeste ~ "Nordeste",
      tribunal_uf %in% centro_oeste ~ "Centro Oeste",
      tribunal_uf %in% sudeste ~ "Sudeste",
      tribunal_uf %in% sul ~ "Sul"
    )
  ) |>
  dplyr::group_by(regiao, ano) |>
  dplyr::summarise(
    novos = sum(novos),
    pop = sum(pop)
  ) |>
  dplyr::ungroup() |>
  dplyr::mutate(
    novos_por_100k = novos/pop * 100000
  ) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = ano, y = novos_por_100k, color = regiao) +
  ggplot2::geom_point() +
  ggplot2::geom_line()
