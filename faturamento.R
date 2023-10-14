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
