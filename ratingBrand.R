source("leituraLimpesaDados.R")

vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
  {shapiro.test(.$Rating)}

vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
  {hist(.$Rating)}

vendas %>%
  filter(!is.na(Brand), !is.na(Rating)) %>%
ggplot() +
  aes(x = Rating) +
  geom_histogram(colour = "white", fill = "#A11D21", binwidth = 0.25) +
  labs(x = "Avaliação", y = " Frequência Absoluta ") +
  theme_estat ()
ggsave("rating_hist.pdf", width = 158, height = 93, units = "mm")

vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
ggplot() +
  aes(x = Brand , y = Rating) +
  geom_boxplot(fill = c("#A11D21"), width = 0.5) +
  stat_summary(fun = "mean", geom = "point", shape = 23, size = 3, fill = "white") +
  labs(x = "Marca", y = "Avaliação") +
  theme_estat()
ggsave("brandRatingBoxplot.pdf", width = 158, height = 93, units = "mm")

vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
  bartlett.test(Rating ~ Brand, data = . )

vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
  group_by(Brand) %>%
  summarise(mean = mean(Rating))

model <-
  vendas %>% 
  filter(!is.na(Brand), !is.na(Rating)) %>%
  aov(Rating ~ Brand, data=.)
summary(model)

vendas %>%
  filter(!is.na(Rating), !is.na(Brand)) %>%
  bartlett.test(Rating ~ Brand, data = . ) 
