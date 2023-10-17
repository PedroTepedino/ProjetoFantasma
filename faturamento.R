source("leituraLimpesaDados.R")

is.na(vendas$Price) %>% sum() 
# 10 linhas com NA no preco. Algum erro no sistema talvez

# vendas exeto onde os precos sao NA
vendas_price_na <- filter(vendas, !is.na(vendas$Price))
vendas_price_na <- filter(vendas_price_na, !is.na(vendas_price_na$Category))

# faturamento total
vendas_price_na$Price %>% sum()

# total por categoria
vendas_price_na %>% 
  group_by(Category) %>%
  summarize(total = sum(Price))

# faturamento total por mes por categoria
faturamento_categoria <-
vendas_price_na %>%
  group_by(Category, month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(Category))

# faturamento acumulado
faturamento_categoria <-
  faturamento_categoria %>%
  filter(!is.na(month)) %>%
  mutate(accumulative = cumsum(total)) 

# media e desvio padrao do faturamento por categoria
faturamento_categoria %>%
  group_by(Category) %>%
  summarise(mean = mean(total), sd = sd(total))

# media e desvio padrao do faturamento por mes
faturamento_categoria %>%
  group_by(month) %>%
  summarise(mean = mean(total), sd = sd(total))


# Plots
ggplot(faturamento_categoria) +
  aes(x=month, y=total, group=Category, colour=Category) +
  geom_line(linewidth=1) +
  labs(x="Mes", y="Faturamento Mensal", colour="Categoria") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()
ggsave("faturamento_mes_categoria.pdf", width = 158, height = 93, units = "mm")

ggplot(faturamento_categoria) +
  aes(x=month, y=accumulative, group=Category, colour=Category) +
  geom_line(linewidth=1) +
  labs(x="Mes", y="Faturamento Mensal") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()
ggsave("faturamento_mes_categoria_acumulado.pdf", width = 158, height = 93, units = "mm")
