source("leituraLimpesaDados.R")


vendas$Brand %>% is.na() %>% sum()
vendas$Price %>% is.na() %>% sum()

vendas %>%
  filter(!is.na(Price)) %>%
  filter(!is.na(Brand)) %>%
  group_by(Brand) %>%
  summarize(count = n(), 
            mean = mean(Price), 
            var = var(Price), 
            sd = sd(Price))


hist(vendas$Price)

count(vendas, Brand)
for ( brand in unique(vendas$Brand))
{
  hist(filter(vendas, !is.na(Price), !is.na(Brand), Brand == brand)$Price)
}
# Todos os precos parecem normalmente distribuidos pelos histogramas
# TODO: Talvez fazer teste de normalidade depois



