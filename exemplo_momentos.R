library(tidyquant)
library(tidyverse)

# Baixar dados da VALE3 (Yahoo Finance)
vale3 <- tq_get("VALE3.SA",
                from = "2015-01-01",
                to   = Sys.Date())

# Calcular retorno diário (log-retorno)
vale3_retornos <- vale3 %>%
  tq_transmute(select     = adjusted,
               mutate_fun = periodReturn,
               period     = "daily",
               type       = "log",
               col_rename = "retorno")

# Gráfico de frequência dos retornos
ggplot(vale3_retornos, aes(x = retorno)) +
  geom_histogram(bins = 80,
                 fill = "blue",
                 color = "black",
                 alpha = 0.8) +
  labs(title = "Distribuição dos Retornos Diários da VALE3",
       subtitle = "Log-retornos diários",
       x = "Retorno diário", y = "Frequência") +
  theme_bw()+
  theme(text = element_text(size=10, face="bold", color="black"),
        axis.text.y = element_text(size = 12, face = "bold", color = "black"),
        axis.text.x = element_text(size = 12, face = "bold", color = "black"),
      legend.title = element_text(size=12))