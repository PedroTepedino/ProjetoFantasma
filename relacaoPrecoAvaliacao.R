source("leituraLimpesaDados.R")

summary(vendas$Price)
summary(vendas$Rating)

vendas %>%
  filter(!is.na(Price), !is.na(Rating))

ggplot(vendas) +
  aes(x = Price, y = Rating) +
  labs(x = "Preço", y = "Avaliação") + 
  geom_point(colour = "#A11D21", size = 3) + 
  theme_estat()

ggsave("AvaliacaoPreco.pdf", width = 158, height = 93, units = "mm")

hist(vendas$Price)
hist(vendas$Rating)

vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
ggplot() +
  aes(x = Price) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Preço", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("preco_hist.pdf", width = 158, height = 93, units = "mm")

vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
ggplot() +
  aes(x = Price) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 7) +
  labs(x = "Avaliação", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("avaliacao_hist.pdf", width = 158, height = 93, units = "mm")


vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
  {cor(.$Price, .$Rating)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
  {shapiro.test(.$Price)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
  {shapiro.test(.$Rating)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating), !is.na(Product.ID)) %>%
  {chisq.test(.$Price, .$Rating)}
