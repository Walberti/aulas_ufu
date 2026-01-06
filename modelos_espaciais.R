rm(list=ls())
library(sf)
library(spdep)
library(spatialreg)
library(tidyverse)

setwd("~/pacotes_r/04/dados_amazonia")
amz <- readRDS("dados_amazonia.rds")

rook_p <- poly2nb(amz, queen=F)
rook_b <- nb2listw(rook_p, style="W", zero.policy = TRUE)

m_ols <- lm(desm ~ pasto, data = amz)
summary(m_ols)

moran.test(residuals(m_ols), rook_b, zero.policy = TRUE)

lm.RStests(m_ols, rook_b,
  test = c("LMlag", "LMerr", "RLMlag", "RLMerr"),
  zero.policy = TRUE)


m_sem <- errorsarlm(desm ~ pasto, data = amz, 
                    listw = rook_b, 
                    zero.policy = TRUE )
summary(m_sem)

m_sar <- lagsarlm(desm ~ pasto, data = amz,
                  listw = rook_b,
                  zero.policy = TRUE)
summary(m_sar)

m_sdm <- lagsarlm(desm ~ pasto, data = amz,
                  listw = rook_b,
                  type = "mixed",
                  zero.policy = TRUE)
summary(m_sdm)

AIC(m_ols, m_sem, m_sar, m_sdm)
logLik(m_ols)
logLik(m_sem)
logLik(m_sar)
logLik(m_sdm)


imp_sar <- impacts(m_sar, listw = queen_b, R = 1000)
summary(imp_sar, zstats = TRUE)

imp_sdm <- impacts(m_sdm, listw = queen_b, R = 1000)
summary(imp_sdm, zstats = TRUE)


