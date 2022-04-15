library(patchwork)

# base de distribuicoes -----------------------------------------------------------
set.seed(1)
populacao <- c(0:20)
normal <- rnorm(10000)
direita <- rbeta(10000,2,5)
esquerda <- rbeta(10000,5,2)
uniforme <- runif(10000)

distribuicoes <- data.frame(normal, direita, esquerda, uniforme)

cores_abj <- c("#102C68", "#AFCA0A", "#575756")
# distribuicao normal -----------------------------------------------------
p_normal <- ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = normal) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::ggtitle("Distribuição Normal") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

# distribuicoes assimetricas ----------------------------------------------
p_direita <- ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = direita) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::ggtitle("", subtitle = "Esquerda") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

p_esquerda <- ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = esquerda) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::ggtitle("Distribuições Assimétricas", subtitle = "Direita") +
  ggplot2::theme(
    axis.title.x = ggplot2::element_blank()
  )

# p_assimetrica <- gridExtra::grid.arrange(
#   p_direita,
#   p_esquerda,
#   ncol = 2, nrow = 1,
#   top = "Distribuições Assiḿetricas")

# distribuicao uniforme --------------------------------------------------
p_uniforme <- ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = uniforme) +
  ggplot2::geom_histogram(fill = cores_abj[1]) +
  ggplot2::ggtitle("Distribuição Uniforme") +
  ggplot2::xlab("x")

# todos os graficos -------------------------------------------------------

# gridExtra::grid.arrange(
#   p_normal,
#   p_assimetrica,
#   p_uniforme
# )

p_normal /
  (p_esquerda + p_direita) /
  p_uniforme



