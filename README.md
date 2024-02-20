<h1 align="center">Pesquisa de Orçamentos Familiares (POF)</h1>
<p align="center"><i>Repositório para documentação do tratamento de microdados da Pesquisa de Orçamentos Familiares (POF) 2017-2018 do IBGE.</i></p>

# Sobre esse projeto
Este repositório conta com a descrição dos procedimentos de extração dos dados e tratamento estatístico da POF 2017-2018.

Para exemplificar a criação de informações filtradas, buscou-se as despesas de famílias que possuíam idosos na sua composição.

## Sobre a POF 2017-2018
As informações oficiais sobre a POF e sobre os microdados estão no site do IBGE: https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&t=microdados

Para esse trabalho, foram utilizados as pastas da pesquisa 2017-2018 como base para as devidas modificações de interesse.

No repositório, apenas os arquivos necessários para o objetivo final (despesas de famílias com idosos) foram adicionados.

## Procedimentos
1. Baixar material em https://www.ibge.gov.br/estatisticas/sociais/saude/24786-pesquisa-de-orcamentos-familiares-2.html?=&t=microdados
-	Dados (contém os microdados em formato ".txt"; para informações de despesas, apenas alguns cadernos serão utilizados)
-	Documentação (arquivos "Manual do Agente de Pesquisa" e "Dicionários de váriaveis" usados para suporte)
-	Tradutores das Tabelas (<code style="color : red">ATENÇÃO</code>: arquivo "Tradutor_Despesa_Geral" contém erros que foram corrigidos neste repositório; utilizar o corrigido)
-	Programas de Leitura (arquivo "Leitura dos microdados – R")
  
2. Abrir pasta “Dados” para transformar arquivos em formato “.txt” para “.Rds” (para a memória de cálculo do R) 
-	O script está em: Programa de leituras > R > Leitura dos microdados – R; também está na pasta "Suporte" deste repositório
-	Copiar caminho da pasta “Dados” para substituir dentro de “setwd(...)” no script "Despesas"

3. Abrir script “Despesas” (criado para este trabalho)

4. Substituir no script “Despesas” os caminhos dos dados transformados em “.Rds” e o caminho onde serão salvos os dados em Excel

### Observações
Obs1: caso sejam necessários novos filtros de famílias por características dos moradores, editar de acordo com o caderno de “Morador”, alterando as variáveis e os parâmetros na linha 238. A saber:
`Morador <- Morador %>% mutate(IDOSO = case_when(V0403>=60 ~ 1,  TRUE  ~ 0))`

Obs2: Usar para tradutor de despesas o arquivo “Tradutor_Despesa_Geral_corrigido”, (na mesma pasta do atual arquivo), para evitar erros de escrita do arquivo original do IBGE

## Tecnologias utilizadas
<p display="inline-block">
  <img width="48" src="https://www.r-project.org/logo/Rlogo.png" alt="R-logo"/>
  <img width="85" src="https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png" alt="RStudio-logo"/>
</p>

## Autores do projeto
**Júlia Cristina Lucas Leite** - https://github.com/julialeitecl / https://www.linkedin.com/in/juliacristinaleite/
**Carlos Henrique Candido de Sousa** - https://github.com/caique-codes / https://www.linkedin.com/in/carlos-henrique-432060a3
