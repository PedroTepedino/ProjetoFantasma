library(tidyverse)
library(lubridate)
vendas_raw <- read.csv("vendas.csv")
devolucao_raw <- read.csv("devolucao.csv")

# remove colunas que estao duplicadas
vendas <- select(vendas_raw, -c(X, ...1.x, ...1.y, Motivo.devoluÃ.Ã.o))
# set data collumn
vendas$Data.Venda <- mdy(vendas$Data.Venda)


is.na(vendas$Price) %>% sum() 
# 10 linhas com NA no preco. Algum erro no sistema talvez

# vendas exeto onde os precos sao NA
vendas_price_na <- filter(vendas, !is.na(vendas$Price))

# faturamentos totais
vendas_price_na$Price %>% sum()

# total por categoria
vendas_price_na %>% 
  group_by(Category) %>%
  summarize(total = sum(Price))

# faturamento total por mes
vendas_price_na %>%
  group_by(month = month(floor_date(Data.Venda, "month"))) %>%
  summarize(total = sum(Price))

# faturamento total por mes por categoria
vendas_price_na %>%
  group_by(Category, month = month(floor_date(Data.Venda, "month"))) %>%
  summarize(total = sum(Price))

# faturamento acumulado total por mes
vendas_price_na %>%
  group_by(month = month(floor_date(Data.Venda, "month"))) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(month)) %>%
  mutate(accumutalive = cumsum(total))
  
# faturamento acumulado por categoria por mes
vendas_price_na %>%
  group_by(Category, month = month(floor_date(Data.Venda, "month"))) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(month)) %>%
  filter(!is.na(Category)) %>%
  mutate(accumutalive = cumsum(total)) %>%
  print(n = 50)



