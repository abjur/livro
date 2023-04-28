# primeiro passo
cores_abj <-  viridis::viridis(2, 1, .2, .8)
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



# primeira amostra de 100 processos ---------------------------------------
set.seed(99)
resultado <- rbinom(100, 1, 0.46)
mean(resultado)
sd(resultado) / sqrt(100)

# histograma vira prob ----------------------------------------------------
set.seed(99)
days <- 365
da <- tibble::tibble(
  id = 0,
  taxa_procedencia = 0
)
for(day in seq_along(1:days)) {
  da <- da |>
    dplyr::add_row(
      id = day,
      taxa_procedencia = mean(rbinom(100, 1, 0.5))
    )
}

da |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262", bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 22, label = "Valor real"), color = "red") +
  ggplot2::scale_x_continuous(limits = c(0,1))

da |>
  dplyr::summarise(
    media = round(mean(taxa_procedencia), 2),
    sd = round(sd(taxa_procedencia), 2)
  )

da |>
  dplyr::mutate(
    cor = dplyr::case_when(
      (taxa_procedencia < (mean(taxa_procedencia) + 2*sd(taxa_procedencia)))
      & (taxa_procedencia > (mean(taxa_procedencia) - 2*sd(taxa_procedencia))) ~ TRUE,
      TRUE ~ FALSE
  )) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia, fill = cor) +
  ggplot2::geom_histogram(bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 22, label = "Valor real"), color = "red") +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = mean(taxa_procedencia) - 2*sd(taxa_procedencia)), color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = mean(taxa_procedencia) - 2*sd(taxa_procedencia) + .02, y = 22, label = round(mean(taxa_procedencia) - 2*sd(taxa_procedencia), 2)), color = "red", linetype = 2) +
  ggplot2::geom_vline(mapping = ggplot2::aes(xintercept = mean(taxa_procedencia) + 2*sd(taxa_procedencia)), color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = mean(taxa_procedencia) + 2*sd(taxa_procedencia) + .02, y = 22, label = round(mean(taxa_procedencia) + 2*sd(taxa_procedencia), 2)), color = "red", linetype = 2) +
  ggplot2::scale_x_continuous(limits = c(0,1)) +
  ggplot2::scale_fill_manual(values = cores_abj)


# poisson -----------------------------------------------------------------

tibble::tibble(
  resultado = resultado
) |>
  ggplot2::ggplot() +
  ggplot2::aes(x = resultado) +
  ggplot2::geom_bar(fill = cores_abj[1]) +
  ggplot2::geom_vline(xintercept = mean(resultado), color = 'red', linetype = 2) +
  ggplot2::scale_x_continuous(limits = c(-0.5,1.5), breaks = c(0,mean(resultado),1)) +
  ggplot2::scale_y_continuous(limits = c(0,55), breaks = c(0,10,20,30,40,50,60))


da |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262", bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::scale_x_continuous(limits = c(0,1))

