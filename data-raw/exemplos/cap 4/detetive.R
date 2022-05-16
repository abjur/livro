# Gráficos do detetive

cores_abj <- c("#102C68", "#AFCA0A", "#575756")

consumo <- abjData::consumo


p_sem_log <- consumo |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::ggtitle("Distribuição do valor de causa")


p_com_log <- consumo |>
  dplyr::mutate(valor_log = log(valor)) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = valor_log) +
  ggplot2::geom_histogram(fill = cores_abj[1], bins = 50) +
  ggplot2::scale_x_log10(labels = scales::label_number_si(), breaks = 10^c(1:10)) +
  ggplot2::ggtitle("Distribuição do valor de causa (com log)")


