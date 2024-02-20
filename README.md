<h1 align="center">Indicadores Sociais</h1>
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
-	Dados (contém os microdados em formato ".txt")
-	Documentação (arquivos "Manual do Agente de Pesquisa" e "Dicionários de váriaveis" usados para suporte)
-	Tradutores das Tabelas (<code style="color : red">ATENÇÃO</code>: arquivo "Tradutor_Despesa_Geral" contém erros que foram corrigidos neste repositório; utilizar o corrigido)
-	Programas de Leitura (arquivo "Leitura dos microdados – R")
  
2. Abrir pasta “Dados” para transformar arquivos em formato “.txt” para “.Rds” (para a memória de cálculo do R) 
-	O script está em: Programa de leituras > R > Leitura dos microdados – R; também está na pasta "Suporte" deste repositório
-	Copiar caminho da pasta “Dados” para substituir dentro de “setwd(...)” no script "Despesas"

3. Abrir script “Despesas” (criado pela autora do trabalho)

## Tecnologias utilizadas
<p display="inline-block">
  <img width="48" src="https://www.r-project.org/logo/Rlogo.png" alt="R-logo"/>
  <img width="70" src="https://www.rstudio.com/wp-content/uploads/2018/10/RStudio-Logo-Flat.png" alt="RStudio-logo"/>
  <img width="48" scr="[https://cdn-dynmedia-1.microsoft.com/is/image/microsoftcorp/Icon_Excel_36x36_2x?fmt=png-alpha](https://1000logos.net/wp-content/uploads/2020/08/Microsoft-Excel-Logo-500x313.png)https://1000logos.net/wp-content/uploads/2020/08/Microsoft-Excel-Logo-500x313.png"/>
</p>
