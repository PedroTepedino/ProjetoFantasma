source("leituraLimpesaDados.R")
library('nortest')
library('agricolae')


vendas$Brand %>% is.na() %>% sum()
vendas$Price %>% is.na() %>% sum()

vendas %>%
  filter(!is.na(Price)) %>%
  filter(!is.na(Brand)) %>%
  group_by(Brand) %>%
  summarize(mean = mean(Price), 
            sd = sd(Price))


hist(vendas$Price)

count(vendas, Brand)
for ( brand in unique(vendas$Brand))
{
  hist(filter(vendas, !is.na(Price), !is.na(Brand), Brand == brand)$Price)
}
# Todos os precos parecem normalmente distribuidos pelos histogramas
# TODO: Talvez fazer teste de normalidade depois

vendas %>% 
  filter(!is.na(Price), !is.na(Brand)) %>%
  group_by(Brand) %>%
  summarize(count = n())

vendas %>% 
  filter(!is.na(Price), !is.na(Brand)) %>%
  {.$Price} %>%
  sort() %>%
  shapiro.test()

for ( brand in unique(vendas$Brand) )
{
  print(brand)
  (vendas %>% 
      filter(!is.na(Price), !is.na(Brand)) %>%
      filter(Brand == brand))$Price %>%
    shapiro.test() %>%
    print()
}

# os dados se distribuem normalmente
# tambem se distribuem normalmente por categoria

vendas %>%
  filter(!is.na(Price), !is.na(Brand)) %>%
  bartlett.test(Price ~ Brand, data = . ) 

qchisq(p=.05, df=4, lower.tail=FALSE)



# como o quantil critico eh menor que a estatistica do teste, nao rejeitamos h0, 
# logo, as variancias sao homogeneas

vendas %>% 
  filter(!is.na(Brand), !is.na(Price)) %>%
  group_by(Brand) %>%
  summarise(mean = mean(Price))

model <- aov(Price ~ Brand, data=vendas)
summary(model)


