###++++++++++++++++++++++++++++++++++++++++++++++++++++++###
### LIMPEZA E TRATAMENTO DOS MICRODADOS DA POF 2017-2018 ###
###++++++++++++++++++++++++++++++++++++++++++++++++++++++###

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

# 3. Manipulacao dos cadernos necessarios para calculos de despesas -----------------

# 3.1. Criacao da tabela de despesas gerais = Despesas --------
# Variavel de Despesas Mensais com bens e servicos = DESP_M
# Variavel de despesas mensais com INSS = INSS_M
# Variavel Contribuicao para a Previdencia Publica Mensal = CPP_M
# Variavel do Imposto de Renda Pessoa Fisica Mensal = IRPF_M
# Variavel Imposto sobre Serviços de Qualquer Natureza Mensal = ISSQN_M
# variavel de Deducoes sobre rendimentos = DEDUCOES

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
                                        sample(NA, 206108, replace = T), Rend$V531132_DEFLA)) %>% 
  mutate("DESP_M" = case_when(QUADRO == 0 | QUADRO == 10 | QUADRO == 19 | QUADRO == 44 | QUADRO == 47 | QUADRO == 48 | QUADRO == 49 | QUADRO == 50 ~ round(((V8000_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                              QUADRO %in% c(63:69) | QUADRO %in% c(6:9) | QUADRO %in% c(11:18) | QUADRO %in% c(21:43) | QUADRO == 45 | QUADRO == 46 | QUADRO == 51 ~ round(((V8000_DEF * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                              TRUE ~ NA),
         "INSS_M" = case_when(QUADRO %in% c(6:19) ~ round(((V1904_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                              TRUE ~ NA),
         "CPP_M" = case_when(QUADRO == 53 ~ round(((V531112_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                             TRUE ~ NA),
         "IRPF_M" = case_when(QUADRO == 53 ~ round(((V531122_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                              TRUE ~ NA),
         "ISSQN_M" = case_when(QUADRO == 53 ~ round(((V531132_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                               TRUE ~ NA),
         "DEDUCOES" = case_when(QUADRO == 54 ~ round(((V8501_DEF * V9011 * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                                QUADRO %in% c(55:57) ~ round(((V8501_DEF * ANUALIZADOR * PESO_FINAL) / 12), digits = 2),
                                TRUE ~ NA),
         "PRODUTO_5D" = round(V9001 / 100))
rm(Aluguel, Cad_coletiva, Desp_coletiva, Desp_individual, Outros_rend, Rend)

# Guardando df para trabalhar com idosos posteriormente
Despesas_2 <- Despesas

# 4. Manipulacao do caderno de moradores de famílias em geral ---------

# Com esse passo cria-se o numero de UC's expandido, necessario para as contas
# de despesas medias mensais por familias

Familia = unique(Morador[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_Familias" = round(sum(PESO_FINAL)))

# 4.1. Relacionamento da base de despesas com a de familias em geral ------------

# Este passo serve para os calculos das depesas medias mensais por familias EM GERAL
Familia = unique(Familia[, c("UF", "N_Familias")])
Despesas = left_join(Despesas, Familia, by = "UF")

# Variavel de Despesas Medias Mensais das Familias = DMMF
Despesas = Despesas %>% mutate("DMMF" = round(DESP_M / N_Familias, digits = 2))

# Retirando variáveis que não serão mais utilizadas
Despesas = subset(Despesas, select = -c(ESTRATO,SIT_REG,COD_UPA,NUM_DOM,NUM_UC,QUADRO,V9001,
                                        V9011,ANUALIZADOR,PESO_FINAL))

# 4.2. Relacionamento do tradutor de despesas com as despesas por produtos ----------

# caso deseje um df mais leve e por nível de descrição, usar esse tradutor
tradutor_despesa <-
   readxl::read_excel("S:/DESRE/BOLETIM SOCIAL/15. IDOSO/Dados/POF/Tradutores/Tradutor_Despesa_Geral.xls")

# caso deseje todas as despesas detalhadas de uma vez (testar resultados ao usar essa planilha, conferir com SIDRA)
# indice_despesa <-
#   readxl::read_excel("S:/DESRE/BOLETIM SOCIAL/15. IDOSO/Dados/POF/Memória de cálculo/Indice_Despesa_editado.xlsx")

Despesas = left_join(Despesas, tradutor_despesa, by = c("PRODUTO_5D" = "Codigo"))

# 4.3. Calculo de variavel das Despesas Mensais --------

# Concomitantimente, calculando as Despesas Mensais com bens e servicos 2 = DESP_M2
# Essa var e criada para considerar somente as contas da tabela de despesa geral,
# conforme tradutor de despesas

Despesas = Despesas %>% 
  mutate(DESP_M2 = case_when(Variavel == "V1904_DEFLA" ~ INSS_M,
                             Variavel == "V531112_DEFLA" ~ CPP_M,
                             Variavel == "V531122_DEFLA" ~ IRPF_M,
                             Variavel == "V531132_DEFLA" ~ ISSQN_M,
                             Variavel == "V8000_DEFLA" ~ DESP_M,
                             Variavel == "V8501_DEFLA" ~ DEDUCOES,
                             TRUE ~ NA)) %>% 
  drop_na(DESP_M2)

# 5. GERACAO DE RESULTADOS ---- 
# Antes de rodar a próxima etapa, certifique-se de ter memória suficiente
# Gerando um df para cada nível de despesa, para UFs e Brasil

# Despesa monetária e não monetária média mensal familiar - por UF
geral0 = Despesas %>% 
  group_by(UF,Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral1 = Despesas %>% 
  group_by(UF,Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral2 = Despesas %>% 
  group_by(UF,Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral3 = Despesas %>% 
  group_by(UF,Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geral4 = Despesas %>% 
  group_by(UF,Nivel_4,Descricao_4) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR0 = Despesas %>% 
  group_by(Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familia$N_Familias), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR1 = Despesas %>% 
  group_by(Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familia$N_Familias), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR2 = Despesas %>% 
  group_by(Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familia$N_Familias), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR3 = Despesas %>% 
  group_by(Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familia$N_Familias), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

geralBR4 = Despesas %>% 
  group_by(Nivel_4,Descricao_4) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familia$N_Familias), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

# 5.1. Exportando para excel arquivos na mesma pasta de trabalho
setwd("...")
library(writexl)
sheets_UF <- list("descricao0" = geral0, 
               "descricao1" = geral1,
               "descricao2" = geral2,
               "descricao3" = geral3,
               "descricao4" = geral4) 
write_xlsx(sheets_UF, "UF_despesas.xlsx")

sheets <- list("descricao0" = geralBR0, 
                  "descricao1" = geralBR1,
                  "descricao2" = geralBR2,
                  "descricao3" = geralBR3,
                  "descricao4" = geralBR4) 
write_xlsx(sheets, "BR_despesas.xlsx")

rm(geral, geralBR, Despesas)

# 6. Manipulacao do caderno de moradores de famílias com idosos ------

# 6.1. Dados de população e domicílio ------- 
# Este passo serve para filtrar moradores idosos
# Criando variável binária IDOSO (1 se o morador for idoso, 0 do contrário)
Morador <- Morador %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))

# Filtrando novo dataframe somente com idosos
Moradores_idosos <- Morador[which(Morador$IDOSO==1),]

# Selecionando somente idosos e agrupando por chaves únicas de identificação,
# somando pesos para obter população total de idosos por UF e organizando por UF única
N_Moradores_idosos <- unique(Moradores_idosos[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_idosos" = round(sum(PESO_FINAL)))
N_Moradores_idosos = unique(N_Moradores_idosos[, c("UF", "N_idosos")])

# Repetindo o mesmo processo para selecionar somente UCs com idosos  
Familias_idosos = unique(Moradores_idosos[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_Familias_com_idosos" = round(sum(PESO_FINAL)))
Familias_idosos = unique(Familias_idosos[, c("UF", "N_Familias_com_idosos")])

# Repetindo o mesmo processo para selecionar moradores em geral 
Moradores = unique(Morador[, c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "COD_INFORMANTE", "PESO_FINAL")]) %>% 
  group_by(UF) %>% 
  mutate("N_moradores" = round(sum(PESO_FINAL)))
Moradores = unique(Moradores[, c("UF", "N_moradores")])

## exportando para excel
# setwd("S:/DESRE/BOLETIM SOCIAL/15. IDOSO/Dados/POF/Resultados/BRUTOS")
# write.xlsx(N_Moradores_idosos,"moradores_idosos.xlsx")
# write.xlsx(Familias_idosos,"familias_com_idosos.xlsx")
# write.xlsx(Moradores,"moradores_geral.xlsx")
# write.xlsx(Familia,"familias_geral.xlsx")


# 6.2. Filtro de famílias com idosos --------
Moradores_idosos$Chave <- paste(Moradores_idosos$UF, Moradores_idosos$ESTRATO_POF, Moradores_idosos$TIPO_SITUACAO_REG, Moradores_idosos$COD_UPA, Moradores_idosos$NUM_DOM, Moradores_idosos$NUM_UC)
chaves_unicas_dom_idosos <- unique(Moradores_idosos$Chave)

Despesas_2$ChaveID <- paste(Despesas_2$UF, Despesas_2$ESTRATO, Despesas_2$SIT_REG, Despesas_2$COD_UPA, Despesas_2$NUM_DOM, Despesas_2$NUM_UC)
Despesas_idosos <- Despesas_2 %>% filter(ChaveID %in% chaves_unicas_dom_idosos)
rm(Despesas_2)
Despesas_idosos = left_join(Despesas_idosos, Familias_idosos, by = "UF")

rm(Morador,Moradores,Moradores_idosos,N_Moradores_idosos,chaves_unicas_dom_idosos)

# Repetindo passos realizados com df "Despesas"
Despesas_idosos = Despesas_idosos %>% 
  mutate("DMMF_idosos" = round(DESP_M / N_Familias_com_idosos, digits = 2),
  )

Despesas_idosos = subset(Despesas_idosos, select = -c(ESTRATO,SIT_REG,COD_UPA,NUM_DOM,NUM_UC,QUADRO,V9001,
                                                      V9011,ANUALIZADOR,PESO_FINAL) )

Despesas_idosos = left_join(Despesas_idosos, tradutor_despesa, by = c("PRODUTO_5D" = "Codigo"))

Despesas_idosos = Despesas_idosos %>% 
  mutate(DESP_M2 = case_when(Variavel == "V1904_DEFLA" ~ INSS_M,
                             Variavel == "V531112_DEFLA" ~ CPP_M,
                             Variavel == "V531122_DEFLA" ~ IRPF_M,
                             Variavel == "V531132_DEFLA" ~ ISSQN_M,
                             Variavel == "V8000_DEFLA" ~ DESP_M,
                             Variavel == "V8501_DEFLA" ~ DEDUCOES,
                             TRUE ~ NA)) %>% 
  drop_na(DESP_M2)

# 7. GERACAO DE RESULTADOS ---------
# Antes de rodar a próxima etapa, certifique-se de ter memória suficiente
# Gerando um df para cada nível de despesa, para UFs e Brasil

# Despesa monetária e não monetária média mensal familiar - por UF
idosos0 = Despesas_idosos %>% 
  group_by(UF,Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias_com_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos1 = Despesas_idosos %>% 
  group_by(UF,Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias_com_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos2 = Despesas_idosos %>% 
  group_by(UF,Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias_com_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos3 = Despesas_idosos %>% 
  group_by(UF,Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias_com_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idosos4 = Despesas_idosos %>% 
  group_by(UF,Nivel_4,Descricao_4) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / N_Familias_com_idosos, digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR0 = Despesas_idosos %>% 
  group_by(Nivel_0,Descricao_0) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familias_idosos$N_Familias_com_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR1 = Despesas_idosos %>% 
  group_by(Nivel_1,Descricao_1) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familias_idosos$N_Familias_com_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR2 = Despesas_idosos %>% 
  group_by(Nivel_2,Descricao_2) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familias_idosos$N_Familias_com_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR3 = Despesas_idosos %>% 
  group_by(Nivel_3,Descricao_3) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familias_idosos$N_Familias_com_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

idososBR4 = Despesas_idosos %>% 
  group_by(Nivel_4,Descricao_4) %>% 
  summarise(Valor = round(sum(DESP_M2, na.rm = T) / sum(Familias_idosos$N_Familias_com_idosos), digits = 2)) %>% 
  unique() %>% 
  drop_na() 

## 7.1. Exportando para excel
setwd("...")
library(writexl)
sheets <- list("descricao0" = idososBR0, 
               "descricao1" = idososBR1,
               "descricao2" = idososBR2,
               "descricao3" = idososBR3,
               "descricao4" = idososBR4) 
write_xlsx(sheets, "BR_despesas_idosos.xlsx")

sheets_UF <- list("descricao0" = idosos0, 
               "descricao1" = idosos1,
               "descricao2" = idosos2,
               "descricao3" = idosos3,
               "descricao4" = idosos4) 
write_xlsx(sheets_UF, "UF_despesas_idosos.xlsx")
