# Carregando um shapefile do Brasil
# com um tipo de encoding que suporte os caracteres utilizados
shp_brasil <- readOGR(dsn = "shapefile_brasil", 
                      layer = "brasil_shapefile",
                      encoding = "UTF8")

# Explorando a base de dados do shapefile brasileiro
shp_brasil@data %>% 
  kable() %>%
  kable_styling(bootstrap_options = "striped", 
                full_width = TRUE, 
                font_size = 12)

# Visualizando o shapefile de forma espacial:
tm_shape(shp = shp_brasil) + 
  tm_borders()

# Visualizando em um mapa
tmap_mode("view")

tm_shape(shp = shp_brasil) + 
  tm_borders()

tmap_mode("plot")

# Caso a intenção seja a de identificar cada estado envolvido na plotagem, a
# função tm_text() pode ser uma solução, caso não haja muitos polígonos 
# envolvidos
tm_shape(shp = shp_brasil) + 
  tm_borders() +
  tm_text(text = "NM_UF", size = 0.4)

# Quando há uma grande quantidade de polígonos, sugiro a utilização conjunta das
# funcionalidades do pacote plotly e ggplot2:

# Passo 1: Transformando nosso shapefile num data frame:
shp_brasil_df <- tidy(shp_brasil, region = "CD_UF") %>%
  rename(CD_UF = id) %>%
  mutate(regiao = substr(x = group, start = 1, stop = 1),
         regiao = factor(regiao,
                         levels = c("1", "2", "3", "4", "5"),
                         labels = c("Norte", "Nordeste", "Sudeste", "Sul", "Centro-oeste")),
         estado_cod = substr(x = group, start = 1, stop = 2)) %>%
  left_join(shp_brasil@data,
            by = "CD_UF") %>%
  rename(estado = NM_UF)

# Passo 2: Fazendo a plotagem de forma espacial e interativa:
ggplotly(
  shp_brasil_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat,
                     group = group, 
                     fill = regiao, 
                     label = estado),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         title = 'Regiões Brasileiras',
         fill = "State") +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
)

# Importando a base de dados para plotar com o mapa
dados_vendas <- read_excel("ITENS VENDIDOS NO PERIODO.XLS")
dados_vendas_estados <- group_by(dados_vendas, CD_UF) %>%
  summarize(VL_FINAL = sum(VL_FINAL))

dados_vendas_estados <- dados_vendas_estados %>% rename(Montante = VL_FINAL)

# Adicionando informações de vendas o Brasil:
shp_brasil_dados <- merge(x = shp_brasil,
                          y = dados_vendas_estados,
                          by.x = "CD_UF",
                          by.y = "CD_UF")

# Plotando espacialmente a variável de interesse:
tm_shape(shp = shp_brasil_dados) + 
  tm_fill(col = "Montante", 
          style = "quantile", 
          n = 3, 
          palette = "viridis", 
          legend.hist = TRUE) +
  tm_borders(alpha = 0.8) +
  tm_compass() +
  tm_layout(legend.outside = TRUE)

# Para uma aproximação da plotagem feita anteriormente com o pacote ggplot2:
shp_brasil_df <- tidy(shp_brasil_dados, region = "CD_UF") %>% 
  rename(CD_UF = id) %>% 
  left_join(shp_brasil_dados@data, by = "CD_UF")

# Criando as faixas do gradiente do Montante por quartis
shp_brasil_df[is.na(shp_brasil_df)] = 0
summary(shp_brasil_df$Montante)

# a) Esquema de cores padrão, viridis:
shp_brasil_df %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat, group = group, fill = Montante),
               color = "black") +
  labs(x = "Longitude",
       y = "Latitude",
       fill = "Montante") +
  scale_fill_viridis_c(option = "viridis") +
  theme_bw()

# ggplot2, então temos que replicar as faixas estabelecidas pelo tmap:
shp_brasil_df <- shp_brasil_df %>% 
  mutate(montante_bands = ifelse(test = Montante <= 203.3, 
                                yes = "0.00 to 203.3",
                                no = ifelse(test = Montante > 203.3 & Montante <= 2310.1,
                                            yes = "203.4 to 2310.1",
                                            no = ifelse(test = Montante > 2310.1 & Montante <= 12892.4,
                                                        yes = "2310.2 to 12892.4",
                                                        no = "12892.5 to 421340.4"))),
         montante_bands = factor(montante_bands,
                                levels = c("0.00 to 203.3",
                                           "203.4 to 2310.1",
                                           "2310.2 to 12892.4",
                                           "12892.5 to 421340.4")))

shp_brasil_df <- rename(shp_brasil_df, Estado = NM_UF, Vendas = montante_bands)

# Fazendo a plotagem de forma espacial e interativa:
ggplotly(
  shp_brasil_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Vendas, 
                     label= Montante, text = paste("Estado:", Estado)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         title = "Distribuição de Vendas no Território Nacional",
         fill = "Montante") +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
)

# Salvando o gráfico interativo em um arquivo para visualização:
saveWidget(ggplotly(
  shp_brasil_df %>% 
    ggplot() +
    geom_polygon(aes(x = long, 
                     y = lat, 
                     group = group, 
                     fill = Vendas, 
                     label= Montante, text = paste("Estado:", Estado)),
                 color = "black") +
    labs(x = "Longitude",
         y = "Latitude",
         title = "Distribuição de Vendas no Território Nacional",
         fill = "Montante") +
    scale_fill_viridis_d(option = "viridis") +
    theme_bw()
), file = "vBRplot.html", title = "Montante de Vendas - Brasil")

