source("leituraLimpesaDados.R")


table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
  addmargins()
  
table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
chisq.test()

table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
  chisq.test(correct=T)
