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

vendas %>%
  filter(!is.na(Price)) %>%
  filter(!is.na(Brand)) %>%
  filter(Brand == Adidas) %>%
  group_by(Brand) %>%
  hist(Price)
