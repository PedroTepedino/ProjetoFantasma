source("leituraLimpesaDados.R")
library(scales)

unique(vendas$Category)
unique(vendas$Color)

vendas %>% 
  filter(Category %in% c("Moda Feminina", "Moda Masculina")) %>%
  filter(!is.na(Color)) %>%
  droplevels() %>%
  {table(.$Category, .$Color)} %>%
  chisq.test()

vendas %>%
  filter(Category %in% c("Moda Feminina", "Moda Masculina")) %>%
  filter(!is.na(Color)) %>%
  droplevels() %>%
  group_by( Color, Category) %>%
  summarise(freq = n()) %>%
  mutate(
    relative_freq = freq %>% 
           percent() %>%
          str_replace("\\.", ",")
      ) 

df_frequency <- vendas %>%
  filter(Category %in% c("Moda Feminina", "Moda Masculina")) %>%
  filter(!is.na(Color)) %>%
  droplevels() %>%
  group_by(Category) %>%
  mutate(count = n()) %>% 
  group_by(Category, Color) %>%
  summarise(freq = n(), count = mean(count)) %>%
  mutate(relative_freq = (freq / count) %>% percent())

percents <- df_frequency$relative_freq %>% str_replace ("\\.", ",")
legends <- str_squish (str_c(df_frequency$freq , " (", percents , ")"))
legends <- str_squish (str_c(df_frequency$freq ))

ggplot(df_frequency) +
  aes(x = fct_reorder(Category, freq, .desc = T), y = freq, fill = Color, label = legends) +
  geom_col(position = position_dodge2( preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = -0.5, hjust = 0.5, size = 3 ) +
  labs(x = " Categoria ", y = " Frequência ") + 
  theme_estat()
ggsave("color_frequency_category.pdf", width = 158 , height = 93, units = "mm")
