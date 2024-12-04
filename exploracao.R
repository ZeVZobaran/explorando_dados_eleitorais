
library(tidyverse)
library(gridExtra)
library(stringr)
library(FactoMineR)
library(reshape2)
library(sf)
library(broom)
library(gridExtra)
library(stringr)
library(png)
library(gridGraphics)

tse_18 = read_csv('./dados/dados_formatados/full_tse_2018.csv')

tse_18_votos_1t <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select('ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0)

colnames(tse_18_votos_1t) <- c(
  'Álvaro Dias', 'Cabo_Daciolo', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
  'Henrique_Meirelles', 'Jair_Bolsonaro', 'João_Amoedo', 'João_Goulart', 'Eymael', 'Marina_Silva',
  'Vera_Lucia', 'Nulos_AS', 'Brancos', 'Nulos'
)

tse_18_votos_1t_pc <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select(ELEITORES, 'ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0) %>%
  mutate_at(vars(-ELEITORES), funs(100*./ ELEITORES)) %>%
  select(-ELEITORES)

colnames(tse_18_votos_1t_pc) <- c(
  'Álvaro_Dias', 'Cabo_Daciolo', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
  'Henrique_Meirelles', 'Jair_Bolsonaro', 'João_Amoedo', 'João_Goulart', 'Eymael', 'Marina_Silva',
  'Vera_Lucia', 'Nulos_AS', 'Brancos', 'Nulos'
)

# Mapas de performance absoluta e relativa
########
# Dados geolocalizados
# Dados por municipio, em % do total para cada candidato

tse_18_votos_1t_pc_munic <- tse_18 %>%
  filter(NR_TURNO == 1) %>%
  select('NM_MUNICIPIO', 'ELEITORES', 'ALVARO FERNANDES DIAS':'VOTO NULO') %>%
  replace(is.na(.), 0) %>%
  group_by(NM_MUNICIPIO) %>%
  summarise_all(sum) %>%
  mutate_at(vars(-ELEITORES, -NM_MUNICIPIO), funs(100*./ ELEITORES)) %>%
  select(-ELEITORES)

colnames(tse_18_votos_1t_pc_munic) <- c(
  'NM_MUNICIP', 'Álvaro_Dias', 'Cabo_Daciolo', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
  'Henrique_Meirelles', 'Jair_Bolsonaro', 'João_Amoedo', 'João_Goulart', 'Eymael', 'Marina_Silva',
  'Vera_Lucia', 'Nulas_AS', 'Brancos', 'Nulos'
)  # Renomando o códigos dos municipios para bater com o .shp do IBGE

# Loadando o shapefile
#AVISO! O github não permite arquivos do tamanho desse shapefile aparentemente.
#Baixe ele manualmente do IBGE em:
#https://www.ibge.gov.br/geociencias/organizacao-do-territorio/15774-malhas.html?edicao=24048&t=downloads
#municipio_2018 > brasil > BR > br_municipios.zip
shp_munics <- st_read('dados/geoloc/BRMUE250GC_SIR.shp')

# Cria a coluna com o nome do candidato que teve a melhor perfomance no municipio
# E adiciona as infos da shapefile
# Nesses cortamos os brancos e nulos
geoloc_1t_18 <- tse_18_votos_1t_pc_munic %>%
  mutate(
    'Melhor_Performance' = apply(.[2:16], 1, function(x) names(x)[which.max(x)])
  ) %>%
  mutate_at(vars(Melhor_Performance), funs(str_replace(., "_", " "))) %>%
  inner_join(shp_munics, 'NM_MUNICIP')

# Cria a coluna com o nome do candidato que teve a melhor perfomance relativa no municipio
# (maior z-score) e adiciona as infos da shapefile

geoloc_1t_18_z_score <- tse_18_votos_1t_pc_munic %>%
  select(
    'NM_MUNICIP', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin',
    'Jair_Bolsonaro', 'João_Amoedo', 'Henrique_Meirelles', 'Marina_Silva'
  ) %>%  # Só mantemos quem teve +1%
  mutate_at(vars(-NM_MUNICIP), funs((. - mean(.))/var(.))) %>%
  mutate(
    'Melhor_Performance' = apply(.[2:8], 1, function(x) names(x)[which.max(x)])
  ) %>%
  mutate_at(vars(Melhor_Performance), funs(str_replace(., "_", " "))) %>%
  inner_join(shp_munics, 'NM_MUNICIP')

# Tibble com as cores de cada cand. para manter consistência nos gráficos
cores_aux = tibble('candidatos' = colnames(geoloc_1t_18_z_score))  %>%
  mutate_at(vars(candidatos), funs(str_replace(., "_", " ")))

cores = tibble('candidatos' = cores_aux$candidatos[2:8]) %>%
  arrange(candidatos) %>%
  mutate('cores' = c('orange', 'firebrick', 'royalblue',
                     'aquamarine4', 'yellowgreen', 'blueviolet', 'cyan'
  ))

# Gráficos de capa: melhor performance absoluta e melhor performance relativa de cada candidato
graph_melhor_performance <- function(df, tipo, cores=cores){
  subtitulo = 'candidato mais votado em cada município'
  titulo = 'Absoluta'  # Default - abs. Se rel, muda
  if (tipo == 'rel'){
    subtitulo = 'candidato com maior z-score em cada município'
    titulo = 'Relativa'
  }
  vetor_cores = subset(cores, candidatos %in% df$Melhor_Performance) %>%
    select(cores)
  graph <- ggplot() +
    geom_sf(
      data = df, aes(
        geometry = geometry, fill = Melhor_Performance
      ), color = NA
    ) +
    scale_fill_manual(values = vetor_cores$cores) + 
    coord_sf() +
    theme_minimal() +
    theme(axis.title.x=element_blank(),  # Remove os eixos e gridlines
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, family='Verdana', face='bold'),
          legend.title = element_text(size = 11, family='Verdana'),
          plot.subtitle = element_text(size = 11, family='Verdana'),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    labs(x = "", y = "", fill='') +
    labs(title =  titulo,
         subtitle = subtitulo)
  # Salvando
  dir.create(file.path(getwd(), 'graficos\\mapas', tipo), showWarnings=FALSE)
  ggsave(
    file.path(getwd(), 'graficos\\mapas', tipo, 'melhor_performance.png', sep=""),
    graph
  )
  return(graph)
}

graphs_performance = list(graph_melhor_performance(geoloc_1t_18, tipo='abs', cores=cores),
                          graph_melhor_performance(geoloc_1t_18_z_score, tipo='rel', cores=cores))

# Monta a tabela com % de votos totais de cada candidato importante
pc_votos_tot <- tse_18_votos_1t %>%
  summarise_all(funs(sum)) %>%
  replace(is.na(.), 0) %>%
  mutate(votos = rowSums(.[,-1])) %>%
  mutate_at(vars(-votos), funs(100*./votos)) %>%
  select(-votos) %>%
  select('Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin',
         'Henrique_Meirelles', 'Jair_Bolsonaro', 'João_Amoedo', 
         'Marina_Silva') %>%
  mutate_all(funs(round(., digits=1)))
colnames(pc_votos_tot)  <- str_replace(colnames(pc_votos_tot), "_", "\n")
rownames(pc_votos_tot) = 'Total de  \nvotos (%)'

tabela <- gridExtra::tableGrob(pc_votos_tot, theme=ttheme_minimal(
  base_size = 11, base_family = 'Verdana'
)) #TODO talvez um gráfico de pizza aqui?

## Monta os dois gráficos e a tabela
melhores_perf = grid.arrange(grobs = list(graphs_performance[[1]], graphs_performance[[2]],
                                          grid::nullGrob(), tabela, grid::nullGrob()),
                             layout_matrix = matrix(c(1, 1, 2, 2, 3, 4, 4, 5),
                                                    nrow=2,
                                                    byrow=TRUE),
                             bottom=grid::textGrob(
                               'TSE',
                               gp = grid::gpar(fontsize = 10, family='Verdana', col='grey42'),
                               x=0.93,
                             ),
                             top=grid::textGrob(
                               'Performance no 1º Turno de 2018\n',
                               gp = grid::gpar(fontsize = 15, family='Verdana', fontface='bold')
                             ))
ggsave(file.path(getwd(), 'graficos\\mapas', 'melhor_performance.png', sep=""),
       melhores_perf)


# Exploratórios 1: Performance absoluta por municipio - heatmap
# Exploratórios 2: Performance relativa por municipio - heatmap
# Feitos juntos por serem análogos

# Recria os dfs, mantendo brancos e nulos, e deixa eles tidy
geoloc_1t_18_nb <- tse_18_votos_1t_pc_munic %>%
  select(
    'NM_MUNICIP', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
    'Jair_Bolsonaro', 'João_Amoedo', 'Henrique_Meirelles', 'Marina_Silva',
    'Nulos', 'Brancos'
  ) %>%  # Só mantemos quem teve +1%
  mutate(
    'Melhor_Performance' = apply(.[2:11], 1, function(x) names(x)[which.max(x)])
  ) %>%
  inner_join(shp_munics, 'NM_MUNICIP')

geoloc_1t_18_z_score_nb <- tse_18_votos_1t_pc_munic %>%
  select(
    'NM_MUNICIP', 'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
    'Jair_Bolsonaro', 'João_Amoedo', 'Henrique_Meirelles', 'Marina_Silva',
    'Nulos', 'Brancos'
  ) %>%  # Só mantemos quem teve +1%
  mutate_at(vars(-NM_MUNICIP), funs((. - mean(.))/var(.))) %>%
  mutate(
    'Melhor_Performance' = apply(.[2:11], 1, function(x) names(x)[which.max(x)])
  ) %>%
  inner_join(shp_munics, 'NM_MUNICIP')

# Função para tydificar
retidy_elec <- function(df){
  aux_colnames = df %>%
    select(-NM_MUNICIP, -geometry, -Melhor_Performance, -CD_GEOCMU) %>%
    colnames(.)
  tidy <- df %>%
    pivot_longer(aux_colnames, names_to = 'candidatos', values_to='votos')
}

tidy_1t_abs = retidy_elec(geoloc_1t_18_nb)
tidy_1t_rel = retidy_elec(geoloc_1t_18_z_score_nb)

# Função geradora dos gráficos para cada candidato
plot_facets <- function(candidato, df, tipo){
  candidato_titulo = candidato  # Se branco ou nulo, não precisa de ajuste
  if((candidato != 'Nulos')&(candidato != 'Brancos')){
    candidato_titulo = gsub('_', ' ', candidato)
  } # Se não, tiramos o _ para o display no gráfico
  print(paste('Grafico', tipo, candidato_titulo))
  df_graph <- df %>% filter(candidatos==candidato)
  midpoint = 0
  if (tipo=='abs'){
    midpoint = (max(df_graph$votos) + min(df_graph$votos))/2
  }  # Define o midpoint. Se tipo == rel, deixa zerado
  dir.create(file.path(getwd(), 'graficos\\mapas', tipo), showWarnings=FALSE)
  # Cria a pasta para salvar o graf
  graph <- ggplot() +
    geom_sf(
      data = df_graph, aes(
        geometry = geometry, fill = votos
      ), color = NA) +
    coord_sf() +
    scale_fill_gradient2(low = "#EBFDED", high = "#000000", mid = "#007AAA", space = "Lab",
                         midpoint=midpoint, name="") +
    theme_minimal() +
    theme(axis.title.x=element_blank(),  # Remove os eixos e gridlines
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          plot.title = element_text(size = 12, family='Verdana'),
          plot.margin = unit(c(0, 0, 0, 0), "cm")) +
    labs(title =  candidato_titulo)
  ggsave(
    file.path(getwd(), 'graficos\\mapas', tipo, paste(candidato, '.png', sep="")),
    graph
  )
  return(graph)
}  # Ref de paletas: https://www.r-bloggers.com/2014/01/gmt-standard-color-palettes/
# Usadas as da Ocean

# Gera todos os gráficos para candidatos individuais
graphs_abs = map(unique(tidy_1t_abs$candidatos), plot_facets, tidy_1t_abs, 'abs')
graphs_rel = map(unique(tidy_1t_rel$candidatos), plot_facets, tidy_1t_rel, 'rel')

# Monta os gráficos em grid com título e frufrus
gen_grid <- function(graphs, tipo){
  subtitulo = 'percentual de votos totais'
  tipo_titulo = 'Absoluta'
  filename = file.path(getwd(), 'graficos\\mapas', tipo, 'full_grid.png')
  if (tipo == 'rel'){
    subtitulo = 'z-score do total de votos'
    tipo_titulo = 'Relativa'
  }
  subtitulo = grid::textGrob(subtitulo,
                             gp = grid::gpar(fontsize = 12, family='Verdana'), x=0.01, hjust=0)
  titulo = grid::textGrob(paste('\nPerformance ', tipo_titulo,
                                ' no 1º Turno de 2018 por Município',
                                sep = ''),
                          gp = grid::gpar(fontsize = 15, fontface = 'bold', family='Verdana'), x=0.01, hjust=0)
  
  grid_graphs = grid.arrange(arrangeGrob(
    grobs = list(graphs[[1]], graphs[[2]], graphs[[3]], graphs[[4]],
                 graphs[[5]], graphs[[6]], graphs[[7]], graphs[[8]],
                 graphs[[9]], grid::nullGrob(), graphs[[10]], grid::nullGrob()),
    nrow=4,  # Add 2 nulls para centralizar o ultimo graf
    bottom=grid::textGrob(
      'TSE',
      gp = grid::gpar(fontsize = 10, family='Verdana', col='grey42'),
      x=0.93,
    )
  ))  # Gera o grid de gráficos com a anotação de fonte do TSE
  margin <- unit(0.3, "line")
  grid = grid.arrange(titulo, subtitulo, grid_graphs,
                      heights = grid::unit.c(grid::grobHeight(titulo) + 1.5*margin,
                                             grid::grobHeight(subtitulo) + 1.5*margin,
                                             grid::unit(1, "null")))
  ggsave(filename,
         grid,
         width=210, height=297, units = 'mm'
  )
  return(grid)
}

full_grid_abs = gen_grid(graphs_abs, 'abs')
full_grid_rel = gen_grid(graphs_rel, 'rel')

# Exploratórios 3: Análise de correlação
# Análise de correlação
##########
# matriz de correlação
cor_18 <- tse_18_votos_1t_pc %>%
  select(
    'Ciro_Gomes', 'Fernando_Haddad', 'Geraldo_Alckmin', 'Guilherme_Boulos',
    'Jair_Bolsonaro', 'João_Amoedo', 'Henrique_Meirelles', 'Marina_Silva',
    'Nulos', 'Brancos'
  ) 
colnames(cor_18) <- str_replace(colnames(cor_18), "_", " ")
cor_18 <- cor(cor_18)

# heatmap de correlação
# Zera os valores do  triângulo inferior
get_upper_tri<-function(cormat){
  cormat[lower.tri(cormat)] <- 0
  return(cormat)
}

melted_cor <- melt(get_upper_tri(cor_18))

cor_graph <- ggplot(data = melted_cor, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile(color = NA) +
  scale_fill_gradient2(low = "firebrick", high = "royalblue", mid = "white",
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, size = 11, hjust = 1, color='black', family='Verdana'),
        axis.text.y = element_text(vjust = 1, size = 11, hjust = 1, color='black', family='Verdana'),
        plot.title = element_text(size = 13, family='Verdana', face='bold'),
        legend.title = element_text(size = 11, family='Verdana'),
        plot.subtitle = element_text(size = 11, family='Verdana'),
        plot.caption = element_text(size = 10, family='Verdana', color='grey42')
  ) +
  labs(x = "", y = "") +
  labs(title =  "Correlação dos eleitorados de cada candidato\nno 1º turno de 2018") +
  labs(subtitle = 'por percentual de votos obtidos em cada seção eleitoral') +
  labs(caption = 'TSE')


ggsave(filename = file.path(getwd(), 'graficos', 'compilados', 'correlacao.png'),
       cor_graph)

# Exploratórios 4: Análise PCA

cores_boulos_nb = tibble(candidatos=c('Guilherme Boulos', 'Nulos', 'Brancos'),
                         cores=c('lightcoral', 'gray46', 'cornsilk3'))
cores_full = cores %>%
  bind_rows(cores_boulos_nb) %>%
  arrange(candidatos)

# Lê e converte em grobs os logos dos partidos
logos_aux = list.files('./graficos/logotipos') %>%
  map2(rep(file.path(getwd(), 'graficos', 'logotipos'), length(.)),
       ., file.path) %>%
  map(., readPNG) %>%
  map(., rasterGrob, interpolate=FALSE) %>%
  tibble()
colnames(logos_aux) = 'logos'

sq_unit = 1 # Metade do tamanho da diagonal dos logos (na última mutate do pipe abaixo fica clar) 
pca_pc <- PCA(tse_18_votos_1t_pc)  # Análise PCA por percentuais -- A certa!
pca_coords <- as.tibble(pca_pc$var$coord)  %>%   # COordenadas PCA
  mutate_all(funs(.*10)) %>%
  mutate(candidatos = str_replace(rownames(pca_pc$var$coord), "_", " ")) %>%
  filter(candidatos %in% c('Ciro Gomes', 'Fernando Haddad', 'Geraldo Alckmin', 'Guilherme Boulos',
                           'Jair Bolsonaro', 'João Amoedo', 'Henrique Meirelles',
                           'Marina Silva', 'Nulos', 'Brancos')) %>%
  mutate(ordem=seq(0, length(candidatos)-1, by=1)) %>%  # Coluna para podermos preservar a ordem
  arrange(candidatos) %>%
  left_join(cores_full, by='candidatos') %>%
  # Tricky: os logos estão em ordem alfabética, salvos como os nomes dos candidatos. Se não for assim, da errado
  bind_cols(logos_aux) %>%
  arrange(ordem) %>%
  mutate(xmin = Dim.1 - sq_unit,
         xmax = Dim.1 + sq_unit,
         ymin = Dim.2 - sq_unit,
         ymax = Dim.2 + sq_unit)


# Tem um migué horrível pra conseguir ordenar os candidatos na ordem certa
# Você consegue encontrar ele? Uma dica: adicionar um candidato ao gráfico seria um desastre!
pca_final = ggplot(data=pca_coords, aes(x=Dim.1, y=Dim.2)) +
  geom_point(size=0.1) +
  geom_hline(yintercept=0) +
  geom_vline(xintercept=0) +
  coord_cartesian(ylim=c(-10, 10), expand=FALSE) +
  scale_x_continuous(limits=c(-10, 10), breaks=seq(-10, 10, 1), labels=c(-10, '', '', '', '', -5,
                                                                         '', '', '', '', 0,
                                                                         '', '', '', '', 5,
                                                                         '', '', '', '', 10)) +
  scale_y_continuous(limits=c(-10, 10), breaks=seq(-10, 10, 1), labels=c(-10, '', '', '', '', -5,
                                                                         '', '', '', '', 0,
                                                                         '', '', '', '', 5,
                                                                         '', '', '', '', 10)) +
  # Cada logo é adicionado manualmente mesmo, na raça
  annotation_custom(pca_coords$logos[[1]], xmin=pca_coords$xmin[[1]], xmax=pca_coords$xmax[[1]], ymin=pca_coords$ymin[[1]], ymax=pca_coords$ymax[[1]]) +
  annotation_custom(pca_coords$logos[[2]], xmin=pca_coords$xmin[[2]], xmax=pca_coords$xmax[[2]], ymin=pca_coords$ymin[[2]], ymax=pca_coords$ymax[[2]]) +
  annotation_custom(pca_coords$logos[[3]], xmin=pca_coords$xmin[[3]], xmax=pca_coords$xmax[[3]], ymin=pca_coords$ymin[[3]], ymax=pca_coords$ymax[[3]]) +
  annotation_custom(pca_coords$logos[[4]], xmin=pca_coords$xmin[[4]], xmax=pca_coords$xmax[[4]], ymin=pca_coords$ymin[[4]], ymax=pca_coords$ymax[[4]]) +
  # MDB fica pequeno, aumentamos na marra
  annotation_custom(pca_coords$logos[[5]], xmin=pca_coords$xmin[[5]]-0.5, xmax=pca_coords$xmax[[5]]+0.5, ymin=pca_coords$ymin[[5]]-0.5, ymax=pca_coords$ymax[[5]]+0.5) +
  # PSL fica grande, diminuimos na marra
  annotation_custom(pca_coords$logos[[6]], xmin=pca_coords$xmin[[6]]+0.4, xmax=pca_coords$xmax[[6]]-0.4, ymin=pca_coords$ymin[[6]]+0.4, ymax=pca_coords$ymax[[6]]-0.4) +
  annotation_custom(pca_coords$logos[[7]], xmin=pca_coords$xmin[[7]], xmax=pca_coords$xmax[[7]], ymin=pca_coords$ymin[[7]], ymax=pca_coords$ymax[[7]]) +
  annotation_custom(pca_coords$logos[[8]], xmin=pca_coords$xmin[[8]], xmax=pca_coords$xmax[[8]], ymin=pca_coords$ymin[[8]], ymax=pca_coords$ymax[[8]]) +
  # Brancos e nulos são grandes demais; cortamos em 0.5
  annotation_custom(pca_coords$logos[[9]], xmin=pca_coords$xmin[[9]]+0.4, xmax=pca_coords$xmax[[9]]-0.4, ymin=pca_coords$ymin[[9]]+0.4, ymax=pca_coords$ymax[[9]]-0.4) +
  annotation_custom(pca_coords$logos[[10]], xmin=pca_coords$xmin[[10]]+0.4, xmax=pca_coords$xmax[[10]]-0.4, ymin=pca_coords$ymin[[10]]+0.4, ymax=pca_coords$ymax[[10]]-0.4) +
  theme_minimal() +
  theme(axis.title.x=element_text(size = 11, family='Verdana', color='black'),
        axis.text.x=element_text(size = 11, family='Verdana', color='black'),
        axis.title.y=element_text(size = 11, family='Verdana', color='black'),
        axis.text.y=element_text(size = 11, family='Verdana', color='black'),
        plot.title =element_text(size = 12, family='Verdana', face='bold'),
        legend.title = element_text(size = 11, family='Verdana'),
        plot.subtitle = element_text(size = 11, family='Verdana',),
        plot.caption = element_text(size = 9, family='Verdana', color='grey42'),
        plot.margin = unit(c(0.5, 1, 1, 1), "cm")) +
  labs(y = '2º fator (12% das diferenças)', x = "1º fator (21% das diferenças)", fill='', color='', legend='') +
  labs(title =  expression(~italic("Political Compass")~'Brasileiro'),
       subtitle = 'principais componentes do eleitorado de cada candidato no 1º turno de 2018\n',
       caption = 'TSE')

ggsave(filename = file.path(getwd(), 'graficos', 'compilados', 'political_compass_br_misterio.png'),
  pca_final,
  width=8, height=8, units='in')
pca_final <- pca_final + 
  labs(y = 'prevalência entre o eleitorado "informado"', x = "prevalência na região nordeste", fill='', color='', legend='') +
  theme(axis.title.x=element_text(size = 11, family='Verdana', color='black'),
        axis.text.x=element_text(size = 11, family='Verdana', color='black'),
        axis.title.y=element_text(size = 11, family='Verdana', color='black'),
        axis.text.y=element_text(size = 11, family='Verdana', color='black'))
        
ggsave(filename = file.path(getwd(), 'graficos', 'compilados', 'political_compass_br_explicado.png'),
       pca_final,
       width=8, height=8, units='in')
