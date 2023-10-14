library(tidyverse)
library(lubridate)

############################
# tema da estat

cores_estat <- c("#A11D21", "#003366", "#CC9900", "#663333", "#FF6600
", "#CC9966", "#999966", "#006606", "#008091", "#041835", "#666666
")

theme_estat <- function(...) {
  theme <- ggplot2:: theme_bw() +
    ggplot2:: theme(
      axis.title.y = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.title.x = ggplot2:: element_text(colour = "black",
                                            size = 12),
      axis.text = ggplot2:: element_text(colour = "black", size
                                         = 9.5),
      panel.border = ggplot2:: element_blank (),
      axis.line = ggplot2:: element_line(colour = "black"),
      legend.position = "top",
      ...
    )
  return(
    list(
      theme ,
      scale_fill_manual(values = cores_estat),
      scale_colour_manual(values = cores_estat)
    )
  )
}
###############

vendas_raw <- read.csv("vendas.csv")
devolucao_raw <- read.csv("devolucao.csv")

# remove colunas que estao duplicadas
vendas <- select(vendas_raw, -c(X, ...1.x, ...1.y, Motivo.devolução))
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
faturamento_total_mes <-
  vendas_price_na %>%
  group_by(month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price))

 # faturamento total por mes por categoria
faturamento_total_mes_categoria <-
vendas_price_na %>%
  group_by(Category, month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price))

# faturamento acumulado total por mes
faturamento_acumulado_total_mes <- 
vendas_price_na %>%
  group_by(month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(month)) %>%
  mutate(accumulative = cumsum(total))
  
# faturamento acumulado por categoria por mes
faturamento_acumulado_categoria_mes <-
vendas_price_na %>%
  group_by(Category, month = floor_date(Data.Venda, "month")) %>%
  summarize(total = sum(Price)) %>%
  filter(!is.na(month)) %>%
  filter(!is.na(Category)) %>%
  mutate(accumulative = cumsum(total)) 


# Plots
ggplot(faturamento_total_mes) +
  aes(x=month , y=total , group=1) +
  geom_line(linewidth=1,colour="#A11D21") + 
  geom_point(colour="#A11D21", size=2) +
  labs(x="Mes", y="Faturamento Mensal") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()

ggplot(faturamento_acumulado_total_mes) +
  aes(x=month , y=accumulative, group=1) +
  geom_line(linewidth=1,colour="#A11D21") + 
  geom_point(colour="#A11D21", size=2) +
  labs(x="Mes", y="Faturamento Mensal") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()

ggplot(faturamento_total_mes_categoria) +
  aes(x=month, y=total, group=Category, colour=Category) +
  geom_line(linewidth=1) +
  labs(x="Mes", y="Faturamento Mensal") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()

ggplot(faturamento_total_mes_categoria) +
  aes(x=month, y=total, group=Category, colour=Category) +
  geom_line(linewidth=1) +
  labs(x="Mes", y="Faturamento Mensal") +
  scale_x_date(date_breaks = "1 month", date_labels = "%b") +
  theme_estat()
