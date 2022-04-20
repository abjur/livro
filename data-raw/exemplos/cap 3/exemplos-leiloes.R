# exemplos leilões
leiloes <- abjData::leiloes


# exemplo medidas resumo categoricas --------------------------------------

abjData::leiloes |>
  dplyr::count(modalidade) |>
  dplyr::mutate(
    acu = cumsum(n),
    prop = n/sum(n),
    porc = scales::percent(prop),
    prop_acu = cumsum(prop),
  )


# exemplo calculando os 3 valores de dispersão ----------------------------

# a base que vamos usar
q1 <- quantile(leiloes$valor_avaliacao_inicial, 0.25, na.rm=TRUE)
q3 <- quantile(leiloes$valor_avaliacao_inicial, 0.75, na.rm=TRUE)

set.seed(100)
da <- leiloes |>
  dplyr::filter(
    valor_avaliacao_inicial > q1,
    valor_avaliacao_inicial < q3
  ) |>
  dplyr::sample_n(10) |>
  dplyr::select(descricao, valor_avaliacao_inicial) |>
  dplyr::mutate(diferenca = round(valor_avaliacao_inicial - mean(valor_avaliacao_inicial),2)) |>
  dplyr::arrange(valor_avaliacao_inicial) |>
  dplyr::mutate(descricao = paste("bem", 1:10))

mean(da$valor_avaliacao_inicial)
mad(da$valor_avaliacao_inicial)
var(da$valor_avaliacao_inicial)
sd(da$valor_avaliacao_inicial)


# exemplo dos quantis empíricos -------------------------------------------
# aqui vamos usar a base completa

min(leiloes$valor_avaliacao_inicial, na.rm = TRUE)

max(leiloes$valor_avaliacao_inicial, na.rm = TRUE)

quantile(leiloes$valor_avaliacao_inicial, 0.25, na.rm = TRUE)

quantile(leiloes$valor_avaliacao_inicial, 0.5, na.rm = TRUE)

quantile(leiloes$valor_avaliacao_inicial, 0.75, na.rm = TRUE)

IQR(leiloes$valor_avaliacao_inicial, na.rm = TRUE)

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

leiloes |>
  dplyr::filter(!is.na(valor_avaliacao_inicial)) |>
  ggplot2::ggplot(ggplot2::aes(x = valor_avaliacao_inicial)) +
  ggplot2::geom_histogram(fill = cores_abj[1])
