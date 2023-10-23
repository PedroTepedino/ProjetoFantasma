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

vendas <- vendas %>% distinct()

vendas$Brand <- vendas$Brand %>% as.factor()
