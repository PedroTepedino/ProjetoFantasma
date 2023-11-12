source("leituraLimpesaDados.R")

library(stats)

table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
chisq.test()
