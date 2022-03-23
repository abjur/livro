
# base de distribuicoes -----------------------------------------------------------
normal <- rnorm(10000)
direita <- rbeta(10000,2,5)
esquerda <- rbeta(10000,5,2)
uniforme <- runif(10000)

distribuicoes <- data.frame(normal, direita, esquerda, uniforme)

cores_abj <- c("#102C68", "#AFCA0A", "#575756")
# distribuicao normal -----------------------------------------------------
ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = normal) +
  ggplot2::geom_histogram(fill = cores_abj[1])

# distribuicoes assimetricas ----------------------------------------------
ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = direita) +
  ggplot2::geom_histogram(fill = cores_abj[1])

ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = esquerda) +
  ggplot2::geom_histogram(fill = cores_abj[1])

# distribuicao uniforme --------------------------------------------------
ggplot2::ggplot(distribuicoes) +
  ggplot2::aes(x = uniforme) +
  ggplot2::geom_histogram(fill = cores_abj[1])

# todos os graficos -------------------------------------------------------


