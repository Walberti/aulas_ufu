library(ggplot2)

set.seed(123)
n <- 10000
moeda <- rbinom(n, 1, 0.5)

prop <- cumsum(moeda) / (1:n)

df <- data.frame(lancamentos = 1:n, proporcao = prop)

ggplot(df, aes(x = lancamentos, y = proporcao)) +
  geom_line(color = "blue", linewidth = 1) +
  geom_hline(yintercept = 0.5, color = "red", linewidth=1)+
  labs(title = "Lei dos Grandes Números",
    x = "Número de lançamentos", y = "Proporção de Caras") +
  theme_bw()+
  theme(text = element_text(size=10, face="bold", color="black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size=12))


