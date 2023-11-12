source("leituraLimpesaDados.R")

summary(vendas$Price)
summary(vendas$Rating)

vendas %>%
  filter(!is.na(Price), !is.na(Rating))

ggplot(vendas) +
  aes(x = Price, y = Rating) +
  geom_point(colour = "#A11D21", size = 3) + 
  theme_estat()

hist(vendas$Price)
hist(vendas$Rating)

vendas %>%
  filter(!is.na(Price), !is.na(Rating)) %>%
  {cor(.$Price, .$Rating)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating)) %>%
  {shapiro.test(.$Price)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating)) %>%
  {shapiro.test(.$Rating)}

vendas %>%
  filter(!is.na(Price), !is.na(Rating)) %>%
  {chisq.test(.$Price, .$Rating)}
