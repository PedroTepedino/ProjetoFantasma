source("leituraLimpesaDados.R")

#Price

vendas %>%
  filter(!is.na(Price)) %>%
  {summary(.$Price)}

quadro_resumo_preco <- vendas %>%
  filter(!is.na(Price)) %>%
  summarize (Média = round(mean(Price),2),
             `Desvio Padrão ` = round(sd(Price),2),
             `Variância ` = round(var(Price),2),
             `Mínimo ` = round(min(Price),2),
             `1º Quartil ` = round( quantile (Price , probs = .25),2),
             Mediana = round( quantile (Price , probs = .5),2),
             `3º Quartil ` = round( quantile (Price , probs = .75),2),
             `Máximo ` = round(max(Price),2)) %>% 
  t() %>% 
  as.data.frame() %>%
  rename("Preço" = V1)
xtable::xtable(quadro_resumo_preco)

vendas %>%
  filter(!is.na(Price)) %>%
ggplot() +
  aes(x= factor(""), y=Price) +
geom_boxplot (fill=c("#A11D21"), width = 0.5) +
  guides (fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Preço dos produtos")+
  theme_estat()
ggsave("boxPlotPrice.pdf", width = 158, height = 93, units = "mm")


vendas %>%
  filter(!is.na(Price)) %>%
  ggplot() +
  aes(x = Price) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Preço", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("preco_hist.pdf", width = 158, height = 93, units = "mm")

vendas %>%
  filter(!is.na(Price)) %>%
  {shapiro.test(.$Price)}



#Rating

quadro_resumo_avaliacao <- vendas %>%
  filter(!is.na(Rating)) %>%
  summarize (Média = round(mean(Rating),2),
             `Desvio Padrão ` = round(sd(Rating),2),
             `Variância ` = round(var(Rating),2),
             `Mínimo ` = round(min(Rating),2),
             `1º Quartil ` = round( quantile (Rating , probs = .25),2),
             Mediana = round( quantile (Rating , probs = .5),2),
             `3º Quartil ` = round( quantile (Rating , probs = .75),2),
             `Máximo ` = round(max(Rating),2)) %>% 
  t() %>% 
  as.data.frame() %>%
  rename("Avaliação" = V1)
xtable::xtable(quadro_resumo_avaliacao)

vendas %>%
  filter(!is.na(Rating)) %>%
  ggplot() +
  aes(x= factor(""), y=Rating) +
  geom_boxplot (fill=c("#A11D21"), width = 0.5) +
  guides (fill=FALSE) +
  stat_summary(fun="mean", geom="point", shape=23, size=3, fill="white")+
  labs(x="", y="Avaliação dos produtos")+
  theme_estat()
ggsave("boxPlotRating.pdf", width = 158, height = 93, units = "mm")

vendas %>%
  filter(!is.na(Rating)) %>%
ggplot() +
  aes(x = Rating) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 0.25) +
  labs(x = "Avaliação", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("avaliacao_hist.pdf", width = 158, height = 93, units = "mm")

vendas %>%
  filter(!is.na(Rating)) %>%
  {shapiro.test(.$Rating)}


# Price X Rating

quadro_resumo <- bind_cols( quadro_resumo_preco, quadro_resumo_avaliacao)
xtable::xtable(quadro_resumo)

vendas %>%
ggplot() +
  aes(x = Price, y = Rating) +
  labs(x = "Preço", y = "Avaliação") + 
  geom_point(colour = "#A11D21", size = 3) + 
  theme_estat()
ggsave("AvaliacaoPreco.pdf", width = 158, height = 93, units = "mm")

vendas %>%
  filter(!is.na(Price), !is.na(Rating)) %>%
  {cor.test(.$Price, .$Rating, method = "pearson")}
