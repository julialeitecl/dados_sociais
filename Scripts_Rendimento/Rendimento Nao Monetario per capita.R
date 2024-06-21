ifelse(!require(dplyr),install.packages("dplyr"),require(dplyr))
ifelse(!require(openxlsx),install.packages("openxlsx"),require(openxlsx))
ifelse(!require(srvyr),install.packages("srvyr"),require(srvyr))
ifelse(!require(tidyverse),install.packages("tidyverse"),require(tidyverse))

setwd("C:/Users/NOVO/Desktop/pesquisa_de_orcamentos_familiares/Dados")

Aluguel = readRDS("ALUGUEL_ESTIMADO.rds")
Morador = readRDS("MORADOR.rds")
Cad_coletiva = readRDS("CADERNETA_COLETIVA.rds")
Desp_coletiva = readRDS("DESPESA_COLETIVA.rds")
Desp_individual = readRDS("DESPESA_INDIVIDUAL.rds")
Rend = readRDS("RENDIMENTO_TRABALHO.rds")
Outros_rend = readRDS("OUTROS_RENDIMENTOS.rds")

# BRASIL ----

despesa_coletiva <- Desp_coletiva

desp_coletiva <- 
  transform( 
    subset( despesa_coletiva,
            V9002 >= 7 
    ) , # [2]
    valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )  # [1] 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

caderneta_coletiva <- Cad_coletiva
cad_coletiva <- 
  transform( 
    subset( caderneta_coletiva,
            V9002 >= 7
    ) ,  # [2]
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(caderneta_coletiva)

despesa_individual <- Desp_individual

desp_individual <-
  transform( 
    subset( despesa_individual,
            V9002 >= 7
    ) , # [2]
    valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    ) # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(despesa_individual)

junta_nao_monet <- 
  rbind( desp_coletiva , 
         cad_coletiva , 
         desp_individual
  )

parte1 <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=junta_nao_monet, sum )
names(parte1) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "soma_nao_monet" )

aluguel_estimado <- Aluguel

alu_estimado <- 
  transform( aluguel_estimado,
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(aluguel_estimado)


aluguel_estimado <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=alu_estimado, sum )
names(aluguel_estimado) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma1" )

despesa_coletiva <- 
  transform( despesa_coletiva ,
             codigo = trunc(V9001/100)
  )

codigos_subtracao <- 
  transform( 
    subset( despesa_coletiva,
            V9002 <= 6 & 
              (
                (codigo >= 8001 & codigo <= 8024) | 
                  (codigo >= 8026 & codigo <= 8068) | 
                  codigo == 8999 |
                  codigo == 10006 |
                  codigo == 10011 |
                  (codigo >= 12005 & codigo <= 12008) | 
                  (codigo >= 12010 & codigo <= 12015) | 
                  (codigo >= 12017 & codigo <= 12020) | 
                  (codigo >= 12023 & codigo <= 12025) | 
                  (codigo >= 12027 & codigo <= 12036) | 
                  codigo == 12999
              )
    ) ,
    valor_mensal = ifelse( QUADRO == 10 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(despesa_coletiva)

cod_subtracao <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC , data=codigos_subtracao, sum )
names(cod_subtracao) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma2" )

parte2 <-
  subset(
    transform(
      merge( aluguel_estimado , cod_subtracao , all.x = T , all.y = T ) ,
      dif = ifelse( is.na(soma1) , 0 , soma1 ) - ifelse( is.na(soma2) , 0 , soma2 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "dif")] ,
    dif > 0
  )

merge_duas_partes <-
  transform(
    merge( parte1 , parte2 , all.x = T , all.y = T ) ,
    soma = ifelse( is.na(soma_nao_monet) , 0 , soma_nao_monet ) + ifelse( is.na(dif) , 0 , dif )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma")]

soma_final <- sum(merge_duas_partes$soma)

morador_uc <- 
  unique( 
    Morador [ ,
              c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC", "COD_INFORMANTE",
                 "PESO_FINAL"
              ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

renda_nao_monet_BR <-
  data.frame(
    soma_final = soma_final ,
    soma_familia = soma_familia ,
    media = round( soma_final / soma_familia , 2 )
  )

# MARANHÃO ----
Morador_MA = Morador[which(Morador$UF==21),]
Morador_MA$Chave = paste(Morador_MA$UF, Morador_MA$ESTRATO_POF, Morador_MA$TIPO_SITUACAO_REG, Morador_MA$COD_UPA, Morador_MA$NUM_DOM, Morador_MA$NUM_UC)
chaves_unicas_MA <- unique(Morador_MA$Chave)

despesa_coletiva <- Desp_coletiva

desp_coletiva <- 
  transform( 
    subset( despesa_coletiva,
            V9002 >= 7 
    ) , # [2]
    valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )  # [1] 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

caderneta_coletiva <- Cad_coletiva
cad_coletiva <- 
  transform( 
    subset( caderneta_coletiva,
            V9002 >= 7
    ) ,  # [2]
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(caderneta_coletiva)

despesa_individual <- Desp_individual

desp_individual <-
  transform( 
    subset( despesa_individual,
            V9002 >= 7
    ) , # [2]
    valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    ) # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(despesa_individual)

junta_nao_monet <- 
  rbind( desp_coletiva , 
         cad_coletiva , 
         desp_individual
  )

junta_nao_monet$ChaveID <- paste(paste(junta_nao_monet$UF, junta_nao_monet$ESTRATO_POF, junta_nao_monet$TIPO_SITUACAO_REG, junta_nao_monet$COD_UPA, junta_nao_monet$NUM_DOM, junta_nao_monet$NUM_UC))
junta_nao_monet_MA <- junta_nao_monet |> filter(ChaveID %in% chaves_unicas_MA)

parte1 <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=junta_nao_monet_MA , sum )
names(parte1) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "soma_nao_monet" )

aluguel_estimado <- Aluguel

alu_estimado <- 
  transform( aluguel_estimado,
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(aluguel_estimado)

alu_estimado$ChaveID <- paste(paste(alu_estimado$UF, alu_estimado$ESTRATO_POF, alu_estimado$TIPO_SITUACAO_REG, alu_estimado$COD_UPA, alu_estimado$NUM_DOM, alu_estimado$NUM_UC))
alu_estimado_MA <- alu_estimado |> filter(ChaveID %in% chaves_unicas_MA)

aluguel_estimado <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=alu_estimado_MA, sum )
names(aluguel_estimado) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma1" )

despesa_coletiva <- 
  transform( despesa_coletiva ,
             codigo = trunc(V9001/100)
  )

despesa_coletiva$ChaveID <- paste(paste(despesa_coletiva$UF, despesa_coletiva$ESTRATO_POF, despesa_coletiva$TIPO_SITUACAO_REG, despesa_coletiva$COD_UPA, despesa_coletiva$NUM_DOM, despesa_coletiva$NUM_UC))
despesa_coletiva_MA <- despesa_coletiva |> filter(ChaveID %in% chaves_unicas_MA)

codigos_subtracao_MA <- 
  transform( 
    subset( despesa_coletiva_MA,
            V9002 <= 6 & 
              (
                (codigo >= 8001 & codigo <= 8024) | 
                  (codigo >= 8026 & codigo <= 8068) | 
                  codigo == 8999 |
                  codigo == 10006 |
                  codigo == 10011 |
                  (codigo >= 12005 & codigo <= 12008) | 
                  (codigo >= 12010 & codigo <= 12015) | 
                  (codigo >= 12017 & codigo <= 12020) | 
                  (codigo >= 12023 & codigo <= 12025) | 
                  (codigo >= 12027 & codigo <= 12036) | 
                  codigo == 12999
              )
    ) ,
    valor_mensal = ifelse( QUADRO == 10 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(despesa_coletiva)

cod_subtracao <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC , data=codigos_subtracao_MA, sum )
names(cod_subtracao) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma2" )

parte2 <-
  subset(
    transform(
      merge( aluguel_estimado , cod_subtracao , all.x = T , all.y = T ) ,
      dif = ifelse( is.na(soma1) , 0 , soma1 ) - ifelse( is.na(soma2) , 0 , soma2 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "dif")] ,
    dif > 0
  )

merge_duas_partes <-
  transform(
    merge( parte1 , parte2 , all.x = T , all.y = T ) ,
    soma = ifelse( is.na(soma_nao_monet) , 0 , soma_nao_monet ) + ifelse( is.na(dif) , 0 , dif )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma")]

soma_final <- sum(merge_duas_partes$soma)

morador_uc <- 
  unique( 
    Morador [ ,
              c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE",
                 "PESO_FINAL"
              ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC

morador_uc$ChaveID <- paste(paste(morador_uc$UF, morador_uc$ESTRATO_POF, morador_uc$TIPO_SITUACAO_REG, morador_uc$COD_UPA, morador_uc$NUM_DOM, morador_uc$NUM_UC))
morador_uc_MA <- morador_uc |> filter(ChaveID %in% chaves_unicas_MA)

soma_familia <- sum( morador_uc_MA$PESO_FINAL )

renda_nao_monet_MA <-
  data.frame(
    soma_final = soma_final ,
    soma_familia = soma_familia ,
    media = round( soma_final / soma_familia , 2 )
  )

# BRASIL IDOSOS ----
Morador <- Morador %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))
Moradores_idosos <- Morador[which(Morador$IDOSO==1),]
Moradores_idosos$Chave <- paste(Moradores_idosos$UF, Moradores_idosos$ESTRATO_POF, Moradores_idosos$TIPO_SITUACAO_REG, 
                                Moradores_idosos$COD_UPA, Moradores_idosos$NUM_DOM, Moradores_idosos$NUM_UC)
chaves_unicas_dom_idosos <- unique(Moradores_idosos$Chave)

despesa_coletiva <- Desp_coletiva

desp_coletiva <- 
  transform( 
    subset( despesa_coletiva,
            V9002 >= 7 
    ) , # [2]
    valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )  # [1] 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

caderneta_coletiva <- Cad_coletiva
cad_coletiva <- 
  transform( 
    subset( caderneta_coletiva,
            V9002 >= 7
    ) ,  # [2]
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(caderneta_coletiva)

despesa_individual <- Desp_individual

desp_individual <-
  transform( 
    subset( despesa_individual,
            V9002 >= 7
    ) , # [2]
    valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    ) # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(despesa_individual)

junta_nao_monet <- 
  rbind( desp_coletiva , 
         cad_coletiva , 
         desp_individual
  )

junta_nao_monet$ChaveID <- paste(junta_nao_monet$UF, junta_nao_monet$ESTRATO_POF, junta_nao_monet$TIPO_SITUACAO_REG, 
                                 junta_nao_monet$COD_UPA, junta_nao_monet$NUM_DOM, junta_nao_monet$NUM_UC)
junta_nao_monet_MA <- junta_nao_monet |> filter(ChaveID %in% chaves_unicas_dom_idosos)

parte1 <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=junta_nao_monet_MA, sum )
names(parte1) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "soma_nao_monet" )

aluguel_estimado <- Aluguel

alu_estimado <- 
  transform( aluguel_estimado,
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(aluguel_estimado)

alu_estimado$ChaveID <- paste(paste(alu_estimado$UF, alu_estimado$ESTRATO_POF, alu_estimado$TIPO_SITUACAO_REG, alu_estimado$COD_UPA, alu_estimado$NUM_DOM, alu_estimado$NUM_UC))
alu_estimado_MA <- alu_estimado |> filter(ChaveID %in% chaves_unicas_dom_idosos)

alu_estimado <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=alu_estimado_MA, sum )
names(alu_estimado) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma1" )

despesa_coletiva <- 
  transform( despesa_coletiva ,
             codigo = trunc(V9001/100)
  )

despesa_coletiva$ChaveID <- paste(despesa_coletiva$UF, despesa_coletiva$ESTRATO_POF, despesa_coletiva$TIPO_SITUACAO_REG, 
                                  despesa_coletiva$COD_UPA, despesa_coletiva$NUM_DOM, despesa_coletiva$NUM_UC)
despesa_coletiva_idosos <- despesa_coletiva |> filter(ChaveID %in% chaves_unicas_dom_idosos)

codigos_subtracao <- 
  transform( 
    subset( despesa_coletiva_idosos,
            V9002 <= 6 & 
              (
                (codigo >= 8001 & codigo <= 8024) | 
                  (codigo >= 8026 & codigo <= 8068) | 
                  codigo == 8999 |
                  codigo == 10006 |
                  codigo == 10011 |
                  (codigo >= 12005 & codigo <= 12008) | 
                  (codigo >= 12010 & codigo <= 12015) | 
                  (codigo >= 12017 & codigo <= 12020) | 
                  (codigo >= 12023 & codigo <= 12025) | 
                  (codigo >= 12027 & codigo <= 12036) | 
                  codigo == 12999
              )
    ) ,
    valor_mensal = ifelse( QUADRO == 10 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(despesa_coletiva)

cod_subtracao <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC , data=codigos_subtracao, sum )
names(cod_subtracao) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma2" )

parte2 <-
  subset(
    transform(
      merge( alu_estimado , cod_subtracao , all.x = T , all.y = T ) ,
      dif = ifelse( is.na(soma1) , 0 , soma1 ) - ifelse( is.na(soma2) , 0 , soma2 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "dif")] ,
    dif > 0
  )

merge_duas_partes <-
  transform(
    merge( parte1 , parte2 , all.x = T , all.y = T ) ,
    soma = ifelse( is.na(soma_nao_monet) , 0 , soma_nao_monet ) + ifelse( is.na(dif) , 0 , dif )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma")]

soma_final <- sum(merge_duas_partes$soma)

morador_uc <- 
  unique( 
    Moradores_idosos [ ,
                       c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                          "COD_INFORMANTE","PESO_FINAL"
                       ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

renda_nao_monet_BR_idosos <-
  data.frame(
    soma_final = soma_final ,
    soma_familia = soma_familia ,
    media = round( soma_final / soma_familia , 2 )
  )

# MARANHÃO IDOSOS ----
Morador_MA = Morador[which(Morador$UF==21),]
Morador_MA <- Morador_MA %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))
Moradores_idosos_MA <- Morador_MA[which(Morador_MA$IDOSO==1),]
Moradores_idosos_MA$Chave <- paste(Moradores_idosos_MA$UF, Moradores_idosos_MA$ESTRATO_POF, Moradores_idosos_MA$TIPO_SITUACAO_REG, Moradores_idosos_MA$COD_UPA, Moradores_idosos_MA$NUM_DOM, Moradores_idosos_MA$NUM_UC)
chaves_unicas_dom_idosos_MA <- unique(Moradores_idosos_MA$Chave)

despesa_coletiva <- Desp_coletiva

desp_coletiva <- 
  transform( 
    subset( despesa_coletiva,
            V9002 >= 7 
    ) , # [2]
    valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )  # [1] 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

caderneta_coletiva <- Cad_coletiva
cad_coletiva <- 
  transform( 
    subset( caderneta_coletiva,
            V9002 >= 7
    ) ,  # [2]
    valor_mensal=(V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(caderneta_coletiva)

despesa_individual <- Desp_individual

desp_individual <-
  transform( 
    subset( despesa_individual,
            V9002 >= 7
    ) , # [2]
    valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    ) # [1]
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal" ) ]

rm(despesa_individual)

junta_nao_monet <- 
  rbind( desp_coletiva , 
         cad_coletiva , 
         desp_individual
  )

junta_nao_monet$ChaveID <- paste(paste(junta_nao_monet$UF, junta_nao_monet$ESTRATO_POF, junta_nao_monet$TIPO_SITUACAO_REG, junta_nao_monet$COD_UPA, junta_nao_monet$NUM_DOM, junta_nao_monet$NUM_UC))
junta_nao_monet_idosos_MA <- junta_nao_monet |> filter(ChaveID %in% chaves_unicas_dom_idosos_MA)

parte1 <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=junta_nao_monet_idosos_MA , sum )
names(parte1) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC", "soma_nao_monet" )

aluguel_estimado <- Aluguel

alu_estimado <- 
  transform( aluguel_estimado,
             valor_mensal=(V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(aluguel_estimado)

alu_estimado$ChaveID <- paste(paste(alu_estimado$UF, alu_estimado$ESTRATO_POF, alu_estimado$TIPO_SITUACAO_REG, alu_estimado$COD_UPA, alu_estimado$NUM_DOM, alu_estimado$NUM_UC))
alu_estimado_idosos_MA <- alu_estimado |> filter(ChaveID %in% chaves_unicas_dom_idosos_MA)

aluguel_estimado <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC, data=alu_estimado_idosos_MA, sum )
names(aluguel_estimado) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma1" )

despesa_coletiva <- 
  transform( despesa_coletiva ,
             codigo = trunc(V9001/100)
  )

despesa_coletiva$ChaveID <- paste(paste(despesa_coletiva$UF, despesa_coletiva$ESTRATO_POF, despesa_coletiva$TIPO_SITUACAO_REG, despesa_coletiva$COD_UPA, despesa_coletiva$NUM_DOM, despesa_coletiva$NUM_UC))
despesa_coletiva_idosos_MA <- despesa_coletiva |> filter(ChaveID %in% chaves_unicas_dom_idosos_MA)

codigos_subtracao_idosos_MA <- 
  transform( 
    subset( despesa_coletiva_idosos_MA,
            V9002 <= 6 & 
              (
                (codigo >= 8001 & codigo <= 8024) | 
                  (codigo >= 8026 & codigo <= 8068) | 
                  codigo == 8999 |
                  codigo == 10006 |
                  codigo == 10011 |
                  (codigo >= 12005 & codigo <= 12008) | 
                  (codigo >= 12010 & codigo <= 12015) | 
                  (codigo >= 12017 & codigo <= 12020) | 
                  (codigo >= 12023 & codigo <= 12025) | 
                  (codigo >= 12027 & codigo <= 12036) | 
                  codigo == 12999
              )
    ) ,
    valor_mensal = ifelse( QUADRO == 10 ,
                           (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12 ,
                           (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
    )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "valor_mensal"  ) ]

rm(despesa_coletiva)

cod_subtracao <- aggregate(valor_mensal ~ UF + ESTRATO_POF + TIPO_SITUACAO_REG + COD_UPA + NUM_DOM + NUM_UC , data=codigos_subtracao_idosos_MA, sum )
names(cod_subtracao) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma2" )

parte2 <-
  subset(
    transform(
      merge( aluguel_estimado , cod_subtracao , all.x = T , all.y = T ) ,
      dif = ifelse( is.na(soma1) , 0 , soma1 ) - ifelse( is.na(soma2) , 0 , soma2 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "dif")] ,
    dif > 0
  )

merge_duas_partes <-
  transform(
    merge( parte1 , parte2 , all.x = T , all.y = T ) ,
    soma = ifelse( is.na(soma_nao_monet) , 0 , soma_nao_monet ) + ifelse( is.na(dif) , 0 , dif )
  )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA", "NUM_DOM", "NUM_UC" , "soma")]

soma_final <- sum(merge_duas_partes$soma)

morador_uc <- 
  unique( 
    Moradores_idosos_MA [ ,
              c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC","COD_INFORMANTE",
                 "PESO_FINAL"
              ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
    ] ) # Apenas um registro por UC

morador_uc$ChaveID <- paste(paste(morador_uc$UF, morador_uc$ESTRATO_POF, morador_uc$TIPO_SITUACAO_REG, morador_uc$COD_UPA, morador_uc$NUM_DOM, morador_uc$NUM_UC))
morador_uc_idosos_MA <- morador_uc |> filter(ChaveID %in% chaves_unicas_dom_idosos_MA)

soma_familia <- sum( morador_uc_idosos_MA$PESO_FINAL )

renda_nao_monet_idosos_MA <-
  data.frame(
    soma_final = soma_final ,
    soma_familia = soma_familia ,
    media = round( soma_final / soma_familia , 2 )
  )

