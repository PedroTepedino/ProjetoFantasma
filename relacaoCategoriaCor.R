source("leituraLimpesaDados.R")

vendas %>% 
  distinct(Product.ID, .keep_all = T) %>%
  filter(Category %in% c("Moda Feminina", "Moda Masculina")) %>%
  filter(!is.na(Color), !is.na(Product.ID)) %>%
  droplevels() %>%
  {table(.$Category, .$Color)} %>%
  chisq.test()

df_frequency <- vendas %>%
  distinct(Product.ID, .keep_all = T) %>%
  filter(Category %in% c("Moda Feminina", "Moda Masculina")) %>%
  filter(!is.na(Color), !is.na(Product.ID)) %>%
  droplevels() %>%
  group_by( Color, Category) %>%
  summarise(freq = n()) %>%
  mutate( relative_freq = {freq / sum(freq)} %>% 
            percent() %>%
            str_replace("\\.", ",")
  )

percents <- df_frequency$relative_freq %>% str_replace ("\\.", ",")
legends <- str_squish (str_c(df_frequency$freq , " (", percents , ")"))

ggplot(df_frequency) +
  aes(x = fct_reorder(Color, freq, .desc = T), y = freq, fill = Category, label = legends) +
  ylim(0, 75) +
  geom_col(position = position_dodge2( preserve = "single", padding = 0)) +
  geom_text(position = position_dodge(width = .9), vjust = 0.5, hjust = -0.1, size = 3 ) +
  labs(x = " Cor ", y = " Frequência ", fill = " Categoria ") + 
  coord_flip() +
  theme_estat()
ggsave("color_frequency_category.pdf", width = 158 , height = 93, units = "mm")
