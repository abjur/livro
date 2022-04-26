# Exemplo do "Por que fazer gráficos?"
# Aqui, vamos apresentar três formas de apresentar os dados:
# 1) os dados tabulados, 2) as medidas de resumo e 3) o gráfico
# Para tanto, vamos usar a base de consumo

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

consumo <- abjData::consumo


# 1) dados tabulados ---------------------------------------------------------

set.seed(20)
da <- consumo |>
  dplyr::sample_n(100) |>
  dplyr::select(valor)

da2 <- cbind(
  da[1:20, ],
  da[21:40, ],
  da[41:60, ],
  da[61:80, ],
  da[81:100, ])

htmlTable::htmlTable(da2,
                     header = c("", "", "valor", "", ""),
                     rnames = FALSE
                     )

# 2) medidas de resumo ----------------------------------------------------

da_resumo <- da |>
  dplyr::summarise(
    `média` = mean(valor),
    `desvio padrão` = sd(valor),
    `mínimo` = min(valor),
    `quantil inferior` = quantile(valor, 0.25),
    `mediana` = median(valor),
    `quantil superior` = quantile(valor, 0.75),
    `máximo` = max(valor)
    ) |>
  tibble::rownames_to_column() |>
  dplyr::mutate(rowname = "valores") |>
  tidyr::pivot_longer(!rowname, names_to = "medidas", values_to = "valores") |>
  tidyr::pivot_wider(names_from = "rowname", values_from = "valores")

da_resumo |>
  knitr::kable(
    caption = "Medidas de resumo"
  )

# representação gráfica ---------------------------------------------------

options(scipen = 999)
da |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 60)
