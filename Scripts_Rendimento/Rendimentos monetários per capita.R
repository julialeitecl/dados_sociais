##### RENDIMENTOS MONETÁRIOS (TRABALHO, TRANSFERENCIAS E OUTRAS RENDAS) ----
# 1. Carregando bibliotecas ---------------------------------
ifelse(!require(dplyr),install.packages("dplyr"),require(dplyr))
ifelse(!require(openxlsx),install.packages("openxlsx"),require(openxlsx))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))

# 2. Carregamento dos cadernos de microdados -----------------

# Caminho dos arquivos extraídos a partir do script "Leitura dos Microdados - R"
setwd("...")

# Carregamento dos cadernos de microdados 
Aluguel = readRDS("ALUGUEL_ESTIMADO.rds")
Morador = readRDS("MORADOR.rds")
Cad_coletiva = readRDS("CADERNETA_COLETIVA.rds")
Desp_coletiva = readRDS("DESPESA_COLETIVA.rds")
Desp_individual = readRDS("DESPESA_INDIVIDUAL.rds")
Rend = readRDS("RENDIMENTO_TRABALHO.rds")
Outros_rend = readRDS("OUTROS_RENDIMENTOS.rds")

Despesas = data.frame("UF" = c(Aluguel$UF, Cad_coletiva$UF, Desp_coletiva$UF,
                               Desp_individual$UF, Outros_rend$UF, Rend$UF),
                      "ESTRATO" = c(Aluguel$ESTRATO_POF, Cad_coletiva$ESTRATO_POF,
                                    Desp_coletiva$ESTRATO_POF, Desp_individual$ESTRATO_POF,
                                    Outros_rend$ESTRATO_POF, Rend$ESTRATO_POF),
                      "SIT_REG" = c(Aluguel$TIPO_SITUACAO_REG, Cad_coletiva$TIPO_SITUACAO_REG,
                                    Desp_coletiva$TIPO_SITUACAO_REG, Desp_individual$TIPO_SITUACAO_REG,
                                    Outros_rend$TIPO_SITUACAO_REG, Rend$TIPO_SITUACAO_REG),
                      "COD_UPA" = c(Aluguel$COD_UPA, Cad_coletiva$COD_UPA, Desp_coletiva$COD_UPA,
                                    Desp_individual$COD_UPA, Outros_rend$COD_UPA, Rend$COD_UPA),
                      "NUM_DOM" = c(Aluguel$NUM_DOM, Cad_coletiva$NUM_DOM, Desp_coletiva$NUM_DOM,
                                    Desp_individual$NUM_DOM, Outros_rend$NUM_DOM, Rend$NUM_DOM),
                      "NUM_UC" = c(Aluguel$NUM_UC, Cad_coletiva$NUM_UC, Desp_coletiva$NUM_UC,
                                   Desp_individual$NUM_UC, Outros_rend$NUM_DOM, Rend$NUM_UC),
                      "QUADRO" = c(Aluguel$QUADRO, Cad_coletiva$QUADRO, Desp_coletiva$QUADRO,
                                   Desp_individual$QUADRO, Outros_rend$QUADRO, Rend$QUADRO),
                      "V9001" = c(Aluguel$V9001, Cad_coletiva$V9001, Desp_coletiva$V9001,
                                  Desp_individual$V9001, Outros_rend$V9001, Rend$V9001),
                      "V9002" = c(Aluguel$V9002, sample(NA, 789995, replace = T), 
                                  Desp_coletiva$V9002, Desp_individual$V9002, 
                                  
                                  sample(NA, 206108, replace = T), sample(NA, 97075, replace = T)),
                      "V9011" = c(Aluguel$V9011, sample(NA, 789995, replace = T), Desp_coletiva$V9011, 
                                  Desp_individual$V9011, Outros_rend$V9011, Rend$V9011),
                      "V8000_DEF" = c(Aluguel$V8000_DEFLA, Cad_coletiva$V8000_DEFLA, 
                                      Desp_coletiva$V8000_DEFLA, Desp_individual$V8000_DEFLA, 
                                      sample(NA, 206108, replace = T), sample(NA, 97075, replace = T)),
                      "ANUALIZADOR" = c(Aluguel$FATOR_ANUALIZACAO, Cad_coletiva$FATOR_ANUALIZACAO, 
                                        Desp_coletiva$FATOR_ANUALIZACAO, Desp_individual$FATOR_ANUALIZACAO, 
                                        Outros_rend$FATOR_ANUALIZACAO, Rend$FATOR_ANUALIZACAO),
                      "PESO_FINAL" = c(Aluguel$PESO_FINAL, Cad_coletiva$PESO_FINAL, 
                                       Desp_coletiva$PESO_FINAL, Desp_individual$PESO_FINAL, 
                                       Outros_rend$PESO_FINAL, Rend$PESO_FINAL),
                      "V1904_DEF" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                      Desp_coletiva$V1904_DEFLA, sample(NA, 1836032, replace = T),
                                      sample(NA, 206108, replace = T), sample(NA, 97075, replace = T)),
                      "V8501_DEF" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                      sample(NA, 478572, replace = T), sample(NA, 1836032, replace = T),
                                      Outros_rend$V8501_DEFLA, sample(NA, 97075, replace = T)),
                      "V531112_DEF" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                        sample(NA, 478572, replace = T), sample(NA, 1836032, replace = T),
                                        sample(NA, 206108, replace = T), Rend$V531112_DEFLA),
                      "V531122_DEF" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                        sample(NA, 478572, replace = T), sample(NA, 1836032, replace = T),
                                        sample(NA, 206108, replace = T), Rend$V531122_DEFLA),
                      "V531132_DEF" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                        sample(NA, 478572, replace = T), sample(NA, 1836032, replace = T),
                                        sample(NA, 206108, replace = T), Rend$V531132_DEFLA),
                      "V8500_DEFLA" = c(sample(NA, 48935, replace = T), sample(NA, 789995, replace = T),
                                        sample(NA, 478572, replace = T), sample(NA, 1836032, replace = T), 
                                        Outros_rend$V8500_DEFLA, Rend$V8500_DEFLA)
)%>% 
  mutate("PRODUTO_5D" = round(V9001 / 100),
         "RENDIMENTO" = case_when(
           # DO TRABALHO, TRANSFERENCIAS E OUTRAS RENDAS
           QUADRO %in% c(53:54) ~ ((V8500_DEFLA*V9011*ANUALIZADOR*PESO_FINAL)/12),
           QUADRO %in% c(55:57) ~ ((V8500_DEFLA*ANUALIZADOR*PESO_FINAL)/12),
           TRUE ~ NA))
Despesas_2 <- Despesas
Moradores = unique(Morador[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_moradores" = round(sum(PESO_FINAL)))
Moradores = unique(Moradores[, c("UF", "N_moradores")])
Despesas = left_join(Despesas, Moradores, by = "UF")

tradutor_rend <-
  readxl::read_excel("C:/Users/NOVO/Desktop/pesquisa_de_orcamentos_familiares/Suporte/Tradutor_Rendimento.xls")
tradutor_rend$Codigo <- as.numeric(tradutor_rend$Codigo)
Despesas = left_join(Despesas, tradutor_rend, by = c("PRODUTO_5D" = "Codigo"))

Despesas = Despesas |> drop_na(RENDIMENTO)

# 5. GERACAO DE RESULTADOS ---- 
# Antes de rodar a próxima etapa, certifique-se de ter memória suficiente
# Gerando um df para cada nível de despesa, para UFs e Brasil

# Despesa monetária e não monetária média mensal familiar - por UF
geral0 = Despesas %>% 
  group_by(UF,Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_moradores, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral1 = Despesas %>% 
  group_by(UF,Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_moradores, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral2 = Despesas %>% 
  group_by(UF,Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_moradores, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral3 = Despesas %>% 
  group_by(UF,Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_moradores, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR0 = Despesas %>%
  group_by(Nivel_1,Descricao_1) |>
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(Moradores$N_moradores), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR1 = Despesas %>% 
  group_by(Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(Moradores$N_moradores), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR2 = Despesas %>% 
  group_by(Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(Moradores$N_moradores), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR3 = Despesas %>% 
  group_by(Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(Moradores$N_moradores), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

# 5.1. Exportando para excel arquivos na mesma pasta de trabalho
setwd("C:/Users/NOVO/Desktop/pesquisa_de_orcamentos_familiares/Resultados")
library(writexl)
sheets_UF <- list("descricao0" = geral0, 
                  "descricao1" = geral1,
                  "descricao2" = geral2,
                  "descricao3" = geral3)
write_xlsx(sheets_UF, "UF_rendas_pc.xlsx")

sheets <- list("descricao0" = geralBR0, 
               "descricao1" = geralBR1,
               "descricao2" = geralBR2,
               "descricao3" = geralBR3) 
write_xlsx(sheets, "BR_rendas_pc.xlsx")

# Criando variável binária IDOSO (1 se o morador for idoso, 0 do contrário)
Morador <- Morador %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))

Moradores_idosos <- Morador[which(Morador$IDOSO==1),]
N_Moradores_idosos <- unique(Moradores_idosos[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_idosos" = round(sum(PESO_FINAL)))
N_Moradores_idosos = unique(N_Moradores_idosos[, c("UF", "N_idosos")])

# 6.2. Filtro de famílias com idosos --------
Moradores_idosos$Chave <- paste(Moradores_idosos$UF, Moradores_idosos$ESTRATO_POF, Moradores_idosos$TIPO_SITUACAO_REG, Moradores_idosos$COD_UPA, Moradores_idosos$NUM_DOM, Moradores_idosos$NUM_UC)
chaves_unicas_idosos <- unique(Moradores_idosos$Chave)

Despesas_2$ChaveID <- paste(Despesas_2$UF, Despesas_2$ESTRATO, Despesas_2$SIT_REG, Despesas_2$COD_UPA, Despesas_2$NUM_DOM, Despesas_2$NUM_UC)
Despesas_idosos <- Despesas_2 %>% filter(ChaveID %in% chaves_unicas_idosos)
Despesas_idosos = left_join(Despesas_idosos, N_Moradores_idosos, by = "UF")

Despesas_idosos = left_join(Despesas_idosos, tradutor_rend, by = c("PRODUTO_5D" = "Codigo"))

# 7. GERACAO DE RESULTADOS ---------
# Antes de rodar a próxima etapa, certifique-se de ter memória suficiente
# Gerando um df para cada nível de despesa, para UFs e Brasil
# Despesa monetária e não monetária média mensal familiar - por UF
idosos0 = Despesas_idosos %>% 
  group_by(UF,Nivel_0,Descricao_0) %>% 
  filter(UF == 21) |>
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos1 = Despesas_idosos %>% 
  group_by(UF,Nivel_1,Descricao_1) %>% 
  filter(UF == 21) |>
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos2 = Despesas_idosos %>% 
  group_by(UF,Nivel_2,Descricao_2) %>% 
  filter(UF == 21) |>
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos3 = Despesas_idosos %>% 
  group_by(UF,Nivel_3,Descricao_3) %>% 
  filter(UF == 21) |>
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / N_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR0 = Despesas_idosos %>% 
  group_by(Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(N_Moradores_idosos$N_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR1 = Despesas_idosos %>% 
  group_by(Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(N_Moradores_idosos$N_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR2 = Despesas_idosos %>% 
  group_by(Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(N_Moradores_idosos$N_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR3 = Despesas_idosos %>% 
  group_by(Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(RENDIMENTO, na.rm = T) / sum(N_Moradores_idosos$N_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

## 7.1. Exportando para excel
setwd("...")
library(writexl)
sheets <- list("descricao0" = idososBR0, 
               "descricao1" = idososBR1,
               "descricao2" = idososBR2,
               "descricao3" = idososBR3)
write_xlsx(sheets, "PC_BR_renda_idosos.xlsx")

sheets_UF <- list("descricao0" = idosos0, 
                  "descricao1" = idosos1,
                  "descricao2" = idosos2,
                  "descricao3" = idosos3) 
write_xlsx(sheets_UF, "PC_UF_renda_idosos.xlsx")
