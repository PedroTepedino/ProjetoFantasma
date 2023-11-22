source("leituraLimpesaDados.R")

is.na(vendas$Price) %>% sum() 
# 10 linhas com NA no preco. Algum erro no sistema talvez

# vendas exeto onde os precos sao NA
vendas_price_na <- filter(vendas, !is.na(vendas$Price))
vendas_price_na <- filter(vendas_price_na, !is.na(vendas_price_na$Category))

# faturamento total
vendas_price_na$Price %>% sum()

# total por categoria
vendas %>%
  distinct(Product.ID, .keep_all = T) %>%
  filter(!is.na(Price)) %>%
  group_by(Category) %>%
  summarize(total = sum(Price)) %>%
  mutate(perc = {total / sum(total)} %>% percent()) %>%
  {.$total %>% sum()}


# faturamento total por mes por categoria
faturamento_categoria <-
vendas %>%
  distinct(Product.ID, .keep_all = T) %>%
  filter(!is.na(Price), !is.na(Category)) %>%
  group_by(Category, month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price)) 


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

# Aparentemente meu sistema operacional nao quis colocar os meses com letra maiuscula, esse foi o jeito...
month_abreviation <- c( "Dez", "Jan", "Fev", "Mar", "Abr", "Mai", "Jun", "Jul", "Ago", "Set", "Out", "Nov")
# Por algum motivo pior ainda, ele nao colocou na ordem certa
#TODO: Arrumar um jeito melhor de concertar isso no futuro

vendas %>%
  distinct(Product.ID, .keep_all = T) %>%
  filter(!is.na(Price), !is.na(Category)) %>%
  group_by(Category, month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(month)) %>%
ggplot() +
  aes(x=month, y=total, group=Category, colour=Category) +
  geom_line(linewidth=1) +
  labs(x="Mês", y="Faturamento Mensal", colour="Categoria") +
  scale_y_continuous(breaks = seq(0, 4000, 500)) +
  scale_x_date(date_breaks = "1 month", date_labels = month_abreviation) +
  theme_estat()
ggsave("faturamento_mes_categoria.pdf", width = 158, height = 93, units = "mm")

