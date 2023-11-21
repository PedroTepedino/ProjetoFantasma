source("leituraLimpesaDados.R")


table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
  addmargins()
  
table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
chisq.test()

table(devolucao$Motivo.devolucao, devolucao$Brand) %>%
  chisq.test(correct=T)


devolucao_freq <- 
devolucao %>%
  filter(!is.na(Motivo.devolucao), !is.na(Brand)) %>%
  group_by(Brand, Motivo.devolucao) %>%
  summarise(freq = n()) %>%
  mutate( freq_relativa = freq %>% {. / sum(.)} %>% percent()) 


porcentagens <- devolucao_freq$freq_relativa %>% str_replace( "\\.", ",") 
legendas <- str_squish(str_c(devolucao_freq$freq , " (", porcentagens , ")"))

devolucao_freq %>%
ggplot() +
  aes(x = fct_reorder (Brand , freq , .desc = T), y = freq ,
    fill = Motivo.devolucao , label = legendas
  ) +
  ylim(0,43) + 
  coord_flip() +
  geom_col( position = position_dodge2(preserve = "single", padding = 0)) +
  geom_text(
    position = position_dodge(width = .9),
    vjust = 0.5, hjust = -0.1,
    size = 3
  ) +
  labs(x = " Marca ", y = " Frequência ", fill = " Motivo da Devolução ") +
  theme_estat () 
ggsave("barrasFreqMarcaDevolucao.pdf", width = 158, height = 93, units = "mm")

