# Descendo no nível de estados, para os estados SP, MG e RS
# Análise estado de Minas Gerais
# Carregando um shapefile do estado de Minas Gerais
# com um tipo de encoding que suporte os caracteres utilizados

shp_mg <- readOGR(dsn = "shapefile_mg", 
                  layer = "mg_shapefile")

# Explorando a base de dados do shapefile de Minas Gerais
shp_mg@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualizando o shapefile de forma espacial:
tm_shape(shp = shp_mg) + 
  tm_borders()

# Visualizando em um mapa
tmap_mode("view")

tm_shape(shp = shp_mg) + 
  tm_borders()

tmap_mode("plot")

# Caso a intenção seja a de identificar cada cidade envolvido na plotagem, a
# função tm_text() pode ser uma solução, caso não haja muitos polígonos 
# envolvidos
tm_shape(shp = shp_mg) + 
  tm_borders() +
  tm_text(text = "NM_MUN", size = 0.4)

# Transformando o objeto shp_sc num data frame:
shp_mg_df <- tidy(shp_mg, region = "CD_MUN") %>% 
  rename(CD_MUN = id) %>%
  left_join(shp_mg@data,
            by = "CD_MUN") %>%
  rename(cidade = NM_MUN)

# Cria um dataframe sem cidades duplicadas para extrair CD_MUN
shp_mg_df_nodup <- shp_mg_df[!duplicated(shp_mg_df[c('CD_MUN')]), ]
# Para preparar o arquivo itens vendidos...com os códigos dos municípios
write_xlsx(shp_mg_df_nodup, "cidades_mg.xlsx")

# Importando a base de dados para plotar com o mapa
dados_vendas <- read_excel("ITENS VENDIDOS NO PERIODO.XLS")

# Filtrando para apenas resultados de Minas Gerais
dados_vendas_mg <- filter(dados_vendas, dados_vendas$UF == "MG")

# Somando o montante de cada cidade
dados_vendas_mg <- group_by(dados_vendas_mg, CD_CIDADE) %>%
  summarize(VL_FINAL = sum(VL_FINAL))

dados_vendas_mg <- dados_vendas_mg %>% rename(Montante = VL_FINAL)

# Adicionando informações de vendas no estado de Minas Gerais:
shp_mg_dados <- merge(x = shp_mg,
                      y = dados_vendas_mg,
                      by.x = "CD_MUN",
                      by.y = "CD_CIDADE")

# Plotando espacialmente a variável de interesse:
tm_shape(shp = shp_mg_dados) + 
  tm_fill(col = "Montante", 
          style = "quantile", 
          n = 3, 
          palette = "viridis", 
          legend.hist = TRUE) +
  tm_borders(alpha = 0.8) +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Para uma aproximação da plotagem feita anteriormente com o pacote ggplot2:
shp_mg_df <- tidy(shp_mg_dados, region = "CD_MUN") %>% 
  rename(CD_MUN = id) %>% 
  left_join(shp_mg_dados@data, by = "CD_MUN")

# Criando as faixas do gradiente do Montante por quartis
shp_mg_df[is.na(shp_mg_df)] = 0
summary(shp_mg_df$Montante)

# a) Esquema de cores padrão, viridis:
shp_mg_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Montante),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Montante") +
  scale_fill_viridis_c(option = "viridis") +
  theme_bw()

# Usando o ggplotly para fazer um gráfico dinâmico
ggplotly(
  shp_mg_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Montante, 
                     text = paste("Cidade:", NM_MUN)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         title = "Distribuição de Vendas Estado de Minas Gerais") +
    scale_fill_viridis_c(option = "viridis") +
    theme_bw()
)

# Salvando o gráfico para visualizações
saveWidget(ggplotly(
  shp_mg_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Montante, 
                     text = paste("Cidade:", NM_MUN)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         title = "Distribuição de Vendas Estado de Minas Gerais") +
    scale_fill_viridis_c(option = "viridis") +
    theme_bw()
), file = "vMGplot.html", title = "Montante de Vendas - Estado de Minas Gerais")
