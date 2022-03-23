id_pretendente <- c(1082L:1092L)
etnias <- c("A", "B", "I", "N", "P", "S")
tp_etnia <- sample(x = etnias, size = 11, replace = TRUE)

etnia_long <- data.frame(id_pretendente, tp_etnia)

etnia_wide <- etnia_long |>
  tidyr::pivot_wider(
    names_from = tp_etnia,
    values_from = tp_etnia) |>
  dplyr::transmute(
    id_pretendente,
    tp_etnia,
    A = dplyr::if_else(is.na(A), 0, 1),
    B = dplyr::if_else(is.na(B), 0, 1),
    I = dplyr::if_else(is.na(I), 0, 1),
    N = dplyr::if_else(is.na(N), 0, 1),
    P = dplyr::if_else(is.na(P), 0, 1)
  )
