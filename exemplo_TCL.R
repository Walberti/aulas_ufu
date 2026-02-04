library(ggplot2)

n_amostra <- 64
simulacoes <- 10000

# Simulações das médias amostrais
medias <- replicate(simulacoes, mean(rexp(n_amostra, rate = 1/10)))

df <- data.frame(medias = medias)

# Gráfico
ggplot(df, aes(x = medias)) +
  geom_histogram(aes(y = after_stat(density)), bins = 40,
                 fill = "blue", color = "white") +
  stat_function( fun = dnorm, args = list(mean = mean(medias), sd = sd(medias)),
    color = "red", linewidth = 1.2) +
  labs(title = "Teorema Central do Limite", 
       x = "Médias amostrais",y = "Densidade") +
  theme_bw()+
  theme(text = element_text(size=10, face="bold", color="black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
        legend.title = element_text(size=12))
