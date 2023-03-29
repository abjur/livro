da <- tibble::tibble(
  dia = 0,
  taxa_procedencia = 0
)

set.seed(100)

animation::saveHTML({

  for(i in 1:10000) {
    prop <- tibble::tibble(
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

    da <- da |>
      dplyr::add_row(
        dia = i,
        taxa_procedencia = prop
      )

    # plot <- da |>
    #   ggplot2::ggplot() +
    #   ggplot2::aes(x = taxa_procedencia) +
    #   ggplot2::geom_histogram(fill = "#233262", bins = 100) +
    #   ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
    #   ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 50, label = "Valor real"), color = "red") +
    #   ggplot2::scale_x_continuous(limits = c(0,1)) +
    #   ggplot2::scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60))
    #
    # print(plot)
  }
}, img.name = "tlc", imgdir = "data-raw/exemplos/cap 6/", htmlfile = "tlc.html", autobrowse = FALSE, outdir = getwd())

da |>
  ggplot2::ggplot() +
  ggplot2::aes(x = taxa_procedencia) +
  ggplot2::geom_histogram(fill = "#233262", bins = 100) +
  ggplot2::geom_vline(xintercept = .5, color = "red", linetype = 2) +
  ggplot2::geom_text(mapping = ggplot2::aes(x = .55, y = 50, label = "Valor real"), color = "red") +
  ggplot2::scale_x_continuous(limits = c(0,1))
  ggplot2::scale_y_continuous(limits = c(0,60), breaks = c(0,10,20,30,40,50,60))

da |>
  dplyr::summarise(
    media = mean(taxa_procedencia),
    mediana = median(taxa_procedencia),
    desvio_padrao = sd(taxa_procedencia)
  )

tibble::tibble(
  decisao = sample(
    x = c(1, 0),
    size = 100,
    replace = TRUE,
    prob = c(.5, .5))
) |>
  dplyr::mutate(
    media = mean(decisao),
    desvio2 = (decisao-media)^2,
    dp = mean(desvio2)
  )
  dplyr::count(decisao) |>
  dplyr::mutate(prop = n/sum(n))
  dplyr::filter(decisao == "P") |>
  dplyr::pull(prop)
