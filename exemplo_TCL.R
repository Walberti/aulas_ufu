library(ggplot2)

n_amostra <- 64
simulacoes <- 10000

# Simulações das médias amostrais
medias <- replicate(
  simulacoes,
  mean(rexp(n_amostra, rate = 1/10))
)

df <- data.frame(medias = medias)

# Gráfico
ggplot(df, aes(x = medias)) +
  geom_histogram(
    aes(y = after_stat(density)),
    bins = 40,
    fill = "lightblue",
    color = "white"
  ) +
  stat_function(
    fun = dnorm,
    args = list(mean = mean(medias), sd = sd(medias)),
    color = "red",
    linewidth = 1
  ) +
  labs(
    title = "Teorema Central do Limite",
    x = "Médias amostrais",
    y = "Densidade"
  ) +
  theme_minimal()
