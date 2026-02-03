library(tidyquant)
library(tidyverse)

# Baixar dados da VALE3 (Yahoo Finance)
vale3 <- tq_get("VALE3.SA",
                from = "2015-01-01",
                to   = Sys.Date())

# Calcular retorno diário (log-retorno)
vale3_retornos <- vale3 %>%
  tq_transmute(
    select     = adjusted,
    mutate_fun = periodReturn,
    period     = "daily",
    type       = "log",
    col_rename = "retorno"
  )

# Gráfico de frequência dos retornos
ggplot(vale3_retornos, aes(x = retorno)) +
  geom_histogram(
    bins = 50,
    fill = "steelblue",
    color = "white",
    alpha = 0.8
  ) +
  labs(
    title = "Distribuição dos Retornos Diários da VALE3",
    subtitle = "Log-retornos diários",
    x = "Retorno diário",
    y = "Frequência"
  ) +
  theme_minimal()
