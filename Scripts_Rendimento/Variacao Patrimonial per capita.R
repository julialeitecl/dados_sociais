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

#  BRASIL -----
outros_rendimentos <- Outros_rend

outros_rend <-
  transform( outros_rendimentos,
             codigo = trunc( V9001/100 ) ,
             valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8500_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
             ) 
  ) [ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "codigo" , "valor_mensal" ) ]

rm(outros_rendimentos)

codigos <-
  subset( outros_rend ,
          codigo == 55008 |
            codigo == 55010 | 
            codigo == 55016 |
            codigo == 55020 |
            codigo == 55021 |
            codigo == 55022 |
            codigo == 55023 |
            codigo == 55024 |
            codigo == 55025 |
            codigo == 55026 |
            codigo == 55035 |
            codigo == 55037 |
            codigo == 55044 |
            codigo == 55053 |
            codigo == 55061 
  ) 

parte1 <- sum( codigos$valor_mensal )

cod57001 <-
  subset( outros_rend ,
          codigo == 57001
  ) 
arquivo57001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57001 ,
                           sum )
names(arquivo57001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57001" )


cod56001 <-
  subset( outros_rend ,
          codigo == 56001
  ) 
arquivo56001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+ COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56001 ,
                           sum )
names(arquivo56001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56001" )


merge1 <-
  subset(
    transform(
      merge( arquivo57001 ,
             arquivo56001 , 
             all.x = T , 
             all.y = T ) ,
      dif1 = ifelse( is.na(soma57001) , 0 , soma57001 ) - ifelse( is.na(soma56001) , 0 , soma56001 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif1")] ,
    dif1 > 0
  )
arquivo1 <- sum(merge1$dif1)

cod57002 <-
  subset( outros_rend ,
          codigo == 57002
  ) 
arquivo57002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57002 ,
                           sum )
names(arquivo57002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma57002" )


cod56002 <-
  subset( outros_rend ,
          codigo == 56002
  ) 
arquivo56002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56002 ,
                           sum )
names(arquivo56002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma56002" )


merge2 <-
  subset(
    transform(
      merge( arquivo57002 ,
             arquivo56002 , 
             all.x = T , 
             all.y = T ) ,
      dif2 = ifelse( is.na(soma57002) , 0 , soma57002 ) - ifelse( is.na(soma56002) , 0 , soma56002 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif2")] ,
    dif2 > 0
  )
arquivo2 <- sum(merge2$dif2)

cod57003 <-
  subset( outros_rend ,
          codigo == 57003
  ) 
arquivo57003 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57003 ,
                           sum )
names(arquivo57003) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57003" )


cod56003 <-
  subset( outros_rend ,
          codigo == 56003
  ) 
arquivo56003 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56003 ,
                           sum )
names(arquivo56003) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56003" )


merge3 <-
  subset(
    transform(
      merge( arquivo57003 ,
             arquivo56003 , 
             all.x = T , 
             all.y = T ) ,
      dif3 = ifelse( is.na(soma57003) , 0 , soma57003 ) - ifelse( is.na(soma56003) , 0 , soma56003 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif3")] ,
    dif3 > 0
  )
arquivo3 <- sum(merge3$dif3)

cod57004 <-
  subset( outros_rend ,
          codigo == 57004
  ) 
arquivo57004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57004 ,
                           sum )
names(arquivo57004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57004" )


cod56004 <-
  subset( outros_rend ,
          codigo == 56004
  ) 
arquivo56004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56004 ,
                           sum )
names(arquivo56004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56004" )


merge4 <-
  subset(
    transform(
      merge( arquivo57004 ,
             arquivo56004 , 
             all.x = T , 
             all.y = T ) ,
      dif4 = ifelse( is.na(soma57004) , 0 , soma57004 ) - ifelse( is.na(soma56004) , 0 , soma56004 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif4")] ,
    dif4 > 0
  )
arquivo4 <- sum(merge4$dif4)

parte2 <- arquivo1+arquivo2+arquivo3+arquivo4

soma_final <- parte1+parte2

morador_uc <- 
  unique(Morador[ ,
                  c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                     "COD_INFORMANTE","PESO_FINAL"
                  ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
  ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

media_final <- 
  data.frame( media_mensal = soma_final / soma_familia )

# MARANHAO ----
Morador_MA = Morador[which(Morador$UF==21),]
Morador_MA$Chave = paste(Morador_MA$UF, Morador_MA$ESTRATO_POF, Morador_MA$TIPO_SITUACAO_REG, Morador_MA$COD_UPA, Morador_MA$NUM_DOM, Morador_MA$NUM_UC)
chaves_unicas_MA <- unique(Morador_MA$Chave)

outros_rendimentos <- Outros_rend

outros_rend <-
  transform( outros_rendimentos,
             codigo = trunc( V9001/100 ) ,
             valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8500_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
             ) 
  ) [ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "codigo" , "valor_mensal" ) ]

rm(outros_rendimentos)

outros_rend$ChaveID <- paste(paste(outros_rend$UF, outros_rend$ESTRATO_POF, outros_rend$TIPO_SITUACAO_REG, outros_rend$COD_UPA, outros_rend$NUM_DOM, outros_rend$NUM_UC))
outros_rend_MA <- outros_rend |> filter(ChaveID %in% chaves_unicas_MA)

codigos <-
  subset( outros_rend_MA ,
          codigo == 55008 |
            codigo == 55010 | 
            codigo == 55016 |
            codigo == 55020 |
            codigo == 55021 |
            codigo == 55022 |
            codigo == 55023 |
            codigo == 55024 |
            codigo == 55025 |
            codigo == 55026 |
            codigo == 55035 |
            codigo == 55037 |
            codigo == 55044 |
            codigo == 55053 |
            codigo == 55061 
  ) 

parte1 <- sum( codigos$valor_mensal )

cod57001 <-
  subset( outros_rend_MA ,
          codigo == 57001
  ) 
arquivo57001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57001 ,
                           sum )
names(arquivo57001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57001" )


cod56001 <-
  subset( outros_rend_MA ,
          codigo == 56001
  ) 
arquivo56001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+ COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56001 ,
                           sum )
names(arquivo56001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56001" )


merge1 <-
  subset(
    transform(
      merge( arquivo57001 ,
             arquivo56001 , 
             all.x = T , 
             all.y = T ) ,
      dif1 = ifelse( is.na(soma57001) , 0 , soma57001 ) - ifelse( is.na(soma56001) , 0 , soma56001 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif1")] ,
    dif1 > 0
  )
arquivo1 <- sum(merge1$dif1)

cod57002 <-
  subset( outros_rend_MA ,
          codigo == 57002
  ) 
arquivo57002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57002 ,
                           sum )
names(arquivo57002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma57002" )


cod56002 <-
  subset( outros_rend_MA ,
          codigo == 56002
  ) 
arquivo56002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56002 ,
                           sum )
names(arquivo56002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma56002" )


merge2 <-
  subset(
    transform(
      merge( arquivo57002 ,
             arquivo56002 , 
             all.x = T , 
             all.y = T ) ,
      dif2 = ifelse( is.na(soma57002) , 0 , soma57002 ) - ifelse( is.na(soma56002) , 0 , soma56002 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif2")] ,
    dif2 > 0
  )
arquivo2 <- sum(merge2$dif2)


cod56003 <-
  subset( outros_rend_MA ,
          codigo == 56003
  ) 
arquivo56003 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56003 ,
                           sum )
names(arquivo56003) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56003" )

#cod57003 vazia

merge3 <-
  subset(
    transform(arquivo56003,
              dif3 = ifelse( is.na(soma56003) , 0 , soma56003 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif3")] ,
    dif3 > 0
  )
arquivo3 <- sum(merge3$dif3)

cod57004 <-
  subset( outros_rend_MA ,
          codigo == 57004
  ) 
arquivo57004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57004 ,
                           sum )
names(arquivo57004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57004" )


cod56004 <-
  subset( outros_rend_MA ,
          codigo == 56004
  ) 
arquivo56004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56004 ,
                           sum )
names(arquivo56004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56004" )


merge4 <-
  subset(
    transform(
      merge( arquivo57004 ,
             arquivo56004 , 
             all.x = T , 
             all.y = T ) ,
      dif4 = ifelse( is.na(soma57004) , 0 , soma57004 ) - ifelse( is.na(soma56004) , 0 , soma56004 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif4")] ,
    dif4 > 0
  )
arquivo4 <- sum(merge4$dif4)

parte2 <- arquivo1+arquivo2+arquivo3+arquivo4

soma_final <- parte1+parte2

morador_uc <- 
  unique(Morador_MA[ ,
                     c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                        "COD_INFORMANTE" ,"PESO_FINAL"
                     ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
  ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

media_final_MA <- 
  data.frame( media_mensal = soma_final / soma_familia )

# BRASIL IDOSOS ----
Morador <- Morador %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))
Moradores_idosos <- Morador[which(Morador$IDOSO==1),]
Moradores_idosos$Chave <- paste(Moradores_idosos$UF, Moradores_idosos$ESTRATO_POF, Moradores_idosos$TIPO_SITUACAO_REG, Moradores_idosos$COD_UPA, Moradores_idosos$NUM_DOM, Moradores_idosos$NUM_UC)
chaves_unicas_dom_idosos <- unique(Moradores_idosos$Chave)

outros_rendimentos <- Outros_rend

outros_rend <-
  transform( outros_rendimentos,
             codigo = trunc( V9001/100 ) ,
             valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8500_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
             ) 
  ) [ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "codigo" , "valor_mensal" ) ]

rm(outros_rendimentos)

outros_rend$ChaveID <- paste(paste(outros_rend$UF, outros_rend$ESTRATO_POF, outros_rend$TIPO_SITUACAO_REG, outros_rend$COD_UPA, outros_rend$NUM_DOM, outros_rend$NUM_UC))
outros_rend_idosos <- outros_rend |> filter(ChaveID %in% chaves_unicas_dom_idosos)

codigos <-
  subset( outros_rend_idosos ,
          codigo == 55008 |
            codigo == 55010 | 
            codigo == 55016 |
            codigo == 55020 |
            codigo == 55021 |
            codigo == 55022 |
            codigo == 55023 |
            codigo == 55024 |
            codigo == 55025 |
            codigo == 55026 |
            codigo == 55035 |
            codigo == 55037 |
            codigo == 55044 |
            codigo == 55053 |
            codigo == 55061 
  ) 

parte1 <- sum( codigos$valor_mensal )

cod57001 <-
  subset( outros_rend_idosos ,
          codigo == 57001
  ) 
arquivo57001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57001 ,
                           sum )
names(arquivo57001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57001" )


cod56001 <-
  subset( outros_rend_idosos ,
          codigo == 56001
  ) 
arquivo56001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+ COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56001 ,
                           sum )
names(arquivo56001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56001" )


merge1 <-
  subset(
    transform(
      merge( arquivo57001 ,
             arquivo56001 , 
             all.x = T , 
             all.y = T ) ,
      dif1 = ifelse( is.na(soma57001) , 0 , soma57001 ) - ifelse( is.na(soma56001) , 0 , soma56001 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif1")] ,
    dif1 > 0
  )
arquivo1 <- sum(merge1$dif1)

cod57002 <-
  subset( outros_rend_idosos ,
          codigo == 57002
  ) 
arquivo57002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57002 ,
                           sum )
names(arquivo57002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma57002" )


cod56002 <-
  subset( outros_rend_idosos ,
          codigo == 56002
  ) 
arquivo56002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56002 ,
                           sum )
names(arquivo56002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma56002" )


merge2 <-
  subset(
    transform(
      merge( arquivo57002 ,
             arquivo56002 , 
             all.x = T , 
             all.y = T ) ,
      dif2 = ifelse( is.na(soma57002) , 0 , soma57002 ) - ifelse( is.na(soma56002) , 0 , soma56002 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif2")] ,
    dif2 > 0
  )
arquivo2 <- sum(merge2$dif2)

cod57003 <-
  subset( outros_rend ,
          codigo == 57003
  ) 
arquivo57003 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57003 ,
                           sum )
names(arquivo57003) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57003" )


cod56003 <-
  subset( outros_rend ,
          codigo == 56003
  ) 
arquivo56003 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56003 ,
                           sum )
names(arquivo56003) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56003" )


merge3 <-
  subset(
    transform(
      merge( arquivo57003 ,
             arquivo56003 , 
             all.x = T , 
             all.y = T ) ,
      dif3 = ifelse( is.na(soma57003) , 0 , soma57003 ) - ifelse( is.na(soma56003) , 0 , soma56003 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif3")] ,
    dif3 > 0
  )
arquivo3 <- sum(merge3$dif3)

cod57004 <-
  subset( outros_rend ,
          codigo == 57004
  ) 
arquivo57004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57004 ,
                           sum )
names(arquivo57004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57004" )


cod56004 <-
  subset( outros_rend_idosos ,
          codigo == 56004
  ) 
arquivo56004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56004 ,
                           sum )
names(arquivo56004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56004" )


merge4 <-
  subset(
    transform(
      merge( arquivo57004 ,
             arquivo56004 , 
             all.x = T , 
             all.y = T ) ,
      dif4 = ifelse( is.na(soma57004) , 0 , soma57004 ) - ifelse( is.na(soma56004) , 0 , soma56004 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif4")] ,
    dif4 > 0
  )
arquivo4 <- sum(merge4$dif4)

parte2 <- arquivo1+arquivo2+arquivo3+arquivo4

soma_final <- parte1+parte2

morador_uc <- 
  unique(Moradores_idosos[ ,
                           c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                              "COD_INFORMANTE", "PESO_FINAL"
                           ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
  ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

media_final_br_idosos <- 
  data.frame( media_mensal = soma_final / soma_familia )

# MARANHAO IDOSOS ----
Morador_MA = Morador[which(Morador$UF==21),]
Morador_MA <- Morador_MA %>% 
  mutate(IDOSO = 
           case_when(V0403>=60 ~ 1,  
                     TRUE  ~ 0))
Moradores_idosos_ma <- Morador_MA[which(Morador_MA$IDOSO==1),]
Moradores_idosos_ma$Chave <- paste(Moradores_idosos_ma$UF, Moradores_idosos_ma$ESTRATO_POF, Moradores_idosos_ma$TIPO_SITUACAO_REG, Moradores_idosos_ma$COD_UPA, Moradores_idosos_ma$NUM_DOM, Moradores_idosos_ma$NUM_UC)
chaves_unicas_dom_idosos_ma <- unique(Moradores_idosos_ma$Chave)

outros_rendimentos <- Outros_rend

outros_rend <-
  transform( outros_rendimentos,
             codigo = trunc( V9001/100 ) ,
             valor_mensal = ifelse( QUADRO==54,
                                    (V8500_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8500_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12 
             ) 
  ) [ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "codigo" , "valor_mensal" ) ]

rm(outros_rendimentos)

outros_rend$ChaveID <- paste(paste(outros_rend$UF, outros_rend$ESTRATO_POF, outros_rend$TIPO_SITUACAO_REG, outros_rend$COD_UPA, outros_rend$NUM_DOM, outros_rend$NUM_UC))
outros_rend_idosos_ma <- outros_rend |> filter(ChaveID %in% chaves_unicas_dom_idosos_ma)

codigos <-
  subset( outros_rend_idosos_ma ,
          codigo == 55008 |
            codigo == 55010 | 
            codigo == 55016 |
            codigo == 55020 |
            codigo == 55021 |
            codigo == 55022 |
            codigo == 55023 |
            codigo == 55024 |
            codigo == 55025 |
            codigo == 55026 |
            codigo == 55035 |
            codigo == 55037 |
            codigo == 55044 |
            codigo == 55053 |
            codigo == 55061 
  ) 

parte1 <- sum( codigos$valor_mensal )

cod57001 <-
  subset( outros_rend_idosos_ma ,
          codigo == 57001
  ) 
arquivo57001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57001 ,
                           sum )
names(arquivo57001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57001" )


cod56001 <-
  subset( outros_rend_idosos_ma ,
          codigo == 56001
  ) 
arquivo56001 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+ COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56001 ,
                           sum )
names(arquivo56001) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56001" )


merge1 <-
  subset(
    transform(
      merge( arquivo57001 ,
             arquivo56001 , 
             all.x = T , 
             all.y = T ) ,
      dif1 = ifelse( is.na(soma57001) , 0 , soma57001 ) - ifelse( is.na(soma56001) , 0 , soma56001 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif1")] ,
    dif1 > 0
  )
arquivo1 <- sum(merge1$dif1)

cod57002 <-
  subset( outros_rend_idosos_ma ,
          codigo == 57002
  ) 
arquivo57002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57002 ,
                           sum )
names(arquivo57002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma57002" )


cod56002 <-
  subset( outros_rend_idosos_ma ,
          codigo == 56002
  ) 
arquivo56002 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56002 ,
                           sum )
names(arquivo56002) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE", "soma56002" )


merge2 <-
  subset(
    transform(
      merge( arquivo57002 ,
             arquivo56002 , 
             all.x = T , 
             all.y = T ) ,
      dif2 = ifelse( is.na(soma57002) , 0 , soma57002 ) - ifelse( is.na(soma56002) , 0 , soma56002 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif2")] ,
    dif2 > 0
  )
arquivo2 <- sum(merge2$dif2)

#cod57003 e cod56003 vazia
arquivo3 <- 0

cod57004 <-
  subset( outros_rend_idosos_ma ,
          codigo == 57004
  ) 
arquivo57004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod57004 ,
                           sum )
names(arquivo57004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma57004" )


cod56004 <-
  subset( outros_rend_idosos_ma ,
          codigo == 56004
  ) 
arquivo56004 <- aggregate( valor_mensal ~ UF+ESTRATO_POF+TIPO_SITUACAO_REG+COD_UPA + NUM_DOM + NUM_UC + COD_INFORMANTE , 
                           data = cod56004 ,
                           sum )
names(arquivo56004) <- c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "soma56004" )


merge4 <-
  subset(
    transform(
      merge( arquivo57004 ,
             arquivo56004 , 
             all.x = T , 
             all.y = T ) ,
      dif4 = ifelse( is.na(soma57004) , 0 , soma57004 ) - ifelse( is.na(soma56004) , 0 , soma56004 )
    )[ , c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG", "COD_UPA" , "NUM_DOM" , "NUM_UC" , "COD_INFORMANTE" , "dif4")] ,
    dif4 > 0
  )
arquivo4 <- sum(merge4$dif4)

parte2 <- arquivo1+arquivo2+arquivo3+arquivo4

soma_final <- parte1+parte2

morador_uc <- 
  unique(Moradores_idosos_ma[ ,
                              c( "UF","ESTRATO_POF","TIPO_SITUACAO_REG","COD_UPA","NUM_DOM","NUM_UC",
                                 "COD_INFORMANTE", "PESO_FINAL"
                              ) # Apenas vari?veis com informa??es das UC's no arquivo "MORADOR.rds"
  ] ) # Apenas um registro por UC

soma_familia <- sum( morador_uc$PESO_FINAL )

media_final_ma_idosos <- 
  data.frame( media_mensal = soma_final / soma_familia )

