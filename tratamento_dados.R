library(tidyverse)
library(stringr)

#Baixe os dados
dir_perfil_elet_2018 <- './dados/dados_brutos_tse/perfil_eleitorado/planilhas/perfil_eleitorado_2018.csv'
perfil_2018_raw <- read_csv2(dir_perfil_elet_2018, locale = locale(encoding = "latin1"))

idade_est <- function (x) {
  #  Auxiliar para estimar a idade média de cada grupo
  #  O TSE reparte em grupos de cinco anos para idade > 20, e de ano em ano para 16 => idade => 20
  ifelse(
    x>20, x + 2, ifelse(
      x < 0, NA, x
      )
    )
}
na_para_zero <- function(x) {
  ifelse(is.na(x), 0, x)
}


perfil_2018 <- perfil_2018_raw %>%
  mutate(
    IDADE_EST_MIN = as.numeric(CD_FAIXA_ETARIA%/%100),
    IDADE_EST_MED = idade_est(IDADE_EST_MIN)
    ) %>%
  filter(SG_UF != "ZZ") %>%
  select(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED, QT_ELEITORES_PERFIL) %>%
  arrange(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED, QT_ELEITORES_PERFIL) %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, DS_GENERO, IDADE_EST_MED) %>%
  summarise(ELEITORES = sum(QT_ELEITORES_PERFIL, na.rm = TRUE)) %>%  # Consolida quantidade de eleitores
  spread(key = DS_GENERO, value = ELEITORES) %>%  # Cria colunas específicas para genero
  rename(NAO_INFORMADO = `NÃO INFORMADO` ) %>%
  mutate(
    NAO_INFORMADO = na_para_zero(NAO_INFORMADO),
    MASCULINO = na_para_zero(MASCULINO),
    FEMININO = na_para_zero(FEMININO),
    ELEITORES = FEMININO + MASCULINO + NAO_INFORMADO
  ) %>% # Recompõe o total de eleitores
  mutate(IDADE_EST_MED = weighted.mean(IDADE_EST_MED, ELEITORES, na.rm = TRUE)) %>%
  group_by(SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, IDADE_EST_MED) %>%
  summarise(
    ELEITORES = sum(ELEITORES, na.rm = TRUE),
    FEMININO = sum(FEMININO, na.rm = TRUE),
    MASCULINO = sum(MASCULINO, na.rm = TRUE)
    )



dir_votos_2018 <- 'C:/Users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_brutos_tse/votacao_presidente/planilhas/votacao_secao_2018_BR.csv'
votos_2018_raw <- read_csv2(dir_votos_2018, locale = locale(encoding = "latin1"))
colnames((votos_2018))

votos_2018 <- votos_2018_raw %>%
  select(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL, QT_VOTOS) %>%
  arrange(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL) %>%
  group_by(NR_TURNO, SG_UF, CD_MUNICIPIO, NM_MUNICIPIO, NR_ZONA, NM_VOTAVEL) %>%
  summarise(VOTOS = sum(QT_VOTOS)) %>%
  spread(key = NM_VOTAVEL, value = VOTOS) # Cada candidato como uma variavel

ELEITORES = tibble(rowSums(votos_2018[6:21], na.rm = TRUE))
colnames(ELEITORES) = 'ELEITORES'

votos_2018 <- votos_2018 %>%
  bind_cols(ELEITORES)


teste_2018 <- votos_2018 %>%
  group_by(CD_MUNICIPIO) %>%
  filter(NR_TURNO == 1)

write_csv(
  votos_2018,
  'C:/Users/josez/Desktop/Economia/Politica Brasil/explorando_dados_eleitorais/dados/dados_formatados/full_tse_2018.csv'
)





