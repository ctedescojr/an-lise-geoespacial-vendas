# Descendo no nível de estados, para os estados SP, MG e RS
# Análise estado de São Paulo
# Carregando um shapefile do estado de São Paulo

shp_sp <- readOGR(dsn = "shapefile_sp", 
                  layer = "sp_shapefile")

# Explorando a base de dados do shapefile de São Paulo
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Corrigindo erros de encoding
# o shapefile com um tipo de encoding que suporte os caracteres utilizados
shp_sp <- readOGR(dsn = "shapefile_sp", 
                      layer = "sp_shapefile",
                      encoding = "latin1")

# Explorando a base de dados do shapefile shp_sp recarregado:
shp_sp@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)


# Visualizando o shapefile de forma espacial:
tm_shape(shp = shp_sp) + 
  tm_borders()

# Visualizando em um mapa
tmap_mode("view")

tm_shape(shp = shp_sp) + 
  tm_borders()

tmap_mode("plot")

# Caso a intenção seja a de identificar cada cidade envolvido na plotagem, a
# função tm_text() pode ser uma solução, caso não haja muitos polígonos 
# envolvidos
tm_shape(shp = shp_sp) + 
  tm_borders() +
  tm_text(text = "NM_MUNICIP", size = 0.4)

# Transformando o objeto shp_sc num data frame:
shp_sp_df <- tidy(shp_sp, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id) %>%
  left_join(shp_sp@data,
            by = "CD_GEOCMU") %>%
  rename(cidade = NM_MUNICIP)

# Para preparar o arquivo itens vendidos...com os códigos dos municípios
write_xlsx(shp_sp_df, "cidades_sp.xlsx")

# Importando a base de dados para plotar com o mapa
dados_vendas <- read_excel("ITENS VENDIDOS NO PERIODO.XLS")

# Filtrando para apenas resultados de São Paulo
dados_vendas_sp <- filter(dados_vendas, dados_vendas$UF == "SP")

# Somando o montante de cada cidade
dados_vendas_sp <- group_by(dados_vendas_sp, CD_CIDADE) %>%
  summarize(VL_FINAL = sum(VL_FINAL))

dados_vendas_sp <- dados_vendas_sp %>% rename(Montante = VL_FINAL)

# Adicionando informações de vendas no estado de São Paulo:
shp_sp_dados <- merge(x = shp_sp,
                          y = dados_vendas_sp,
                          by.x = "CD_GEOCMU",
                          by.y = "CD_CIDADE")

# Plotando espacialmente a variável de interesse:
tm_shape(shp = shp_sp_dados) + 
  tm_fill(col = "Montante", 
          style = "quantile", 
          n = 6, 
          palette = "viridis", 
          legend.hist = TRUE) +
  tm_borders(alpha = 0.8) +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Para uma aproximação da plotagem feita anteriormente com o pacote ggplot2:
shp_sp_df <- tidy(shp_sp_dados, region = "CD_GEOCMU") %>% 
  rename(CD_GEOCMU = id) %>% 
  left_join(shp_sp_dados@data, by = "CD_GEOCMU")

# Criando as faixas do gradiente do Montante por quartis
shp_sp_df[is.na(shp_sp_df)] = 0
summary(shp_sp_df$Montante)

# a) Esquema de cores padrão, viridis:
shp_sp_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Montante),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Montante") +
  scale_fill_viridis_c(option = "viridis") +
  theme_bw()

# ggplot2, então temos que replicar as faixas estabelecidas pelo tmap:
shp_sp_df <- shp_sp_df %>% 
  mutate(montante_bands = ifelse(test = Montante <= 566, 
                                 yes = "0.00 to 566",
                                 no = ifelse(test = Montante > 566 & Montante <= 1638,
                                             yes = "567 to 1638",
                                             no = ifelse(test = Montante > 1638 & Montante <= 2910,
                                                         yes = "1639 to 2910",
                                                         no = ifelse(test = Montante > 2910 & Montante <= 4488,
                                                                     yes = "2911 to 4488",
                                                                     no = ifelse(test = Montante > 4488 & Montante <= 6131,
                                                                                 yes = "4489 to 6131",
                                                                                 no = ifelse(test = Montante > 6131 & Montante <= 9594,
                                                                                             yes = "6132 to 9594",
                                                                                             no = "9595 to 118663")))))),
         montante_bands = factor(montante_bands,
                                 levels = c("0.00 to 566",
                                            "567 to 1638",
                                            "1639 to 2910",
                                            "2911 to 4488",
                                            "4489 to 6131",
                                            "6132 to 9594",
                                            "9595 to 118663")))



shp_sp_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = montante_bands),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Montante") +
  scale_fill_viridis_d(option = "viridis") +
  theme_bw()

shp_sp_df <- rename(shp_sp_df, Cidade = NM_MUNICIP, Vendas = montante_bands)

ggplotly(
  shp_sp_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Vendas, 
                     label= Montante, text = paste("Cidade:", Cidade)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Montante",
         title = "Distribuição de Vendas Estado de São Paulo" ) +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
)

# Fazendo a plotagem de forma espacial e interativa e salvando o arquivo
saveWidget(ggplotly(
  shp_sp_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Vendas, 
                     label= Montante, text = paste("Cidade:", Cidade)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         fill = "Montante",
         title = "Distribuição de Vendas no Estado de São Paulo" ) +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
), file = "vSPplot.html", title = "Montante de Vendas - Estado de São Paulo")
