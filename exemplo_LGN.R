set.seed(123)

library(ggplot2)

n <- 10000
moeda <- rbinom(n, 1, 0.5)

prop <- cumsum(moeda) / (1:n)

df <- data.frame(
  lancamentos = 1:n,
  proporcao = prop
)

ggplot(df, aes(x = lancamentos, y = proporcao)) +
  geom_line(color = "blue") +
  geom_hline(yintercept = 0.5, color = "red", linetype = "dashed") +
  labs(
    title = "Lei dos Grandes Números",
    x = "Número de lançamentos",
    y = "Proporção de Caras"
  ) +
  theme_minimal()

# Teormea Central do Limite
set.seed(123)

