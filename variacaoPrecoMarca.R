source("leituraLimpesaDados.R")
library('nortest')
library('agricolae')


vendas$Brand %>% is.na() %>% sum()
vendas$Price %>% is.na() %>% sum()

vendas %>%
  filter(!is.na(Price)) %>%
  filter(!is.na(Brand)) %>%
  group_by(Brand) %>%
  summarize(mean = mean(Price), 
            sd = sd(Price))

quadro_resumo <- vendas %>%
  filter(!is.na(Price), !is.na(Brand)) %>%
  group_by( Brand ) %>% # caso mais de uma categoria
  summarize (Média = round(mean(Price),2),
             `Desvio Padrão ` = round(sd(Price),2),
             `Variância ` = round(var(Price),2),
             `Mínimo ` = round(min(Price),2),
             `1º Quartil ` = round( quantile (Price , probs = .25),2),
             Mediana = round( quantile (Price , probs = .5),2),
             `3º Quartil ` = round( quantile (Price , probs = .75),2),
             `Máximo ` = round(max(Price),2)) %>% 
  t() %>% 
  as.data.frame() 

xtable::xtable(quadro_resumo)

vendas %>% 
  filter(!is.na(Price)) %>%
  ggplot() +
  aes(x = Price) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Preço", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("priceHist.pdf", width = 158, height = 93, units = "mm")

vendas %>% 
  filter(!is.na(Brand), !is.na(Price)) %>%
  ggplot() +
  aes(x = Brand , y = Price) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marca", y = "Preço") +
  theme_estat()
ggsave("brandPriceBoxplot.pdf", width = 158, height = 93, units = "mm")


vendas %>% 
  filter(!is.na(Price)) %>%
  {.$Price} %>%
  shapiro.test()

for ( brand in unique(vendas$Brand) )
{
  print(brand)
  (vendas %>% 
      filter(!is.na(Price), !is.na(Brand)) %>%
      filter(Brand == brand))$Price %>%
    shapiro.test() %>%
    print()
}

# os dados se distribuem normalmente
# tambem se distribuem normalmente por categoria

vendas %>%
  filter(!is.na(Price), !is.na(Brand)) %>%
  bartlett.test(Price ~ Brand, data = . ) 


# como o quantil critico eh menor que a estatistica do teste, nao rejeitamos h0, 
# logo, as variancias sao homogeneas

vendas %>% 
  filter(!is.na(Brand), !is.na(Price)) %>%
  group_by(Brand) %>%
  summarise(mean = mean(Price))

model <- aov(Price ~ Brand, data=vendas)
summary(model)
