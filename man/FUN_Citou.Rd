% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FUN_Citou.R
\name{FUN_Citou}
\alias{FUN_Citou}
\title{FUN_Citou}
\usage{
FUN_Citou(
  TABELA,
  DICIONARIO,
  lista_variaveis,
  adc_labels = TRUE,
  debug = FALSE
)
}
\arguments{
\item{TABELA}{Banco de dados a ser analisado.}

\item{DICIONARIO}{Dicionário correspondente à TABELA.}

\item{lista_variaveis}{Variáveis que serão pivotadas.}

\item{adc_labels}{Se adc_labels=TRUE, adicionar uma coluna com o label.}

\item{debug}{Se debug=TRUE, printa cada iteração de variáveis.}
}
\description{
Pivota e depois calcula a frequência simples (ponderada e não ponderada)
 das variáveis indicadas em 'lista_variaveis' (MRG).
 Código 1 = Citou; código diferente de 1 = não citou.
}
\details{
Consute o livro para mais detalhes e exemplos.
}
\examples{

#Passando 'outros' (v10) para do tipo citou/não citou
library(IPpackage)
library(dplyr)
library(tidyr)
TABELA = IPpackage::IPpackage_exemplo \%>\%
 dplyr::mutate(
   v10_1_o = dplyr::case_when(
     base::is.na(v4) & base::is.na(v10_1) ~ NA,
     base::is.na(v10_1) ~ 2,
     !base::is.na(v10_1) ~ 1
   ),
   v10_2_o = dplyr::case_when(
     base::is.na(v4) & base::is.na(v10_2) ~ NA,
     base::is.na(v10_2) ~ 2,
     !base::is.na(v10_2) ~ 1
   ),
   v10_outros = dplyr::case_when(
     base::is.na(v10_1_o) & base::is.na(v10_2_o) ~ NA,
     v10_1_o == 1 | v10_2_o == 1 ~ 1,
     TRUE ~ 2
   )
 )
TABELA \%>\% dplyr::count(v4,v10_1,v10_1_o)
TABELA \%>\% dplyr::count(v4,v10_2,v10_2_o)
TABELA \%>\% dplyr::count(v4,v10_1_o,v10_2_o,v10_outros)

DICIONARIO = IPpackage::IPpackage_dicionario \%>\%
 base::rbind(
   tibble::tibble(
     opcao_cod = base::rep(c(1, 2), 3),
     opcao_label = base::rep(c("Citou", "Não citou"), 3),
     opcao_variavel = c(
       base::rep("v10_outros", 2),
       base::rep("v10_1_o", 2),
       base::rep("v10_2_o", 2)
     ),
     pergunta_enunciado = c(
       base::rep("v10_outros - Citou (Outros)", 2),
       base::rep("v10_1_o - Citou (Outros)", 2),
       base::rep("v10_2_o - Citou (Outros)", 2)
     )
   )
 )

# MRG e Isolada
IPpackage::FUN_Citou(
 TABELA = TABELA,
 DICIONARIO = DICIONARIO,
 lista_variaveis = base::list(
   "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
   "v10_1_o" = "v10_1_o"
   ),
 adc_labels = TRUE
)

# Warning1: Código a mais encontrado em TABELA (diferente de 1, 2)
IPpackage::FUN_Citou(
 TABELA = TABELA,
 DICIONARIO = DICIONARIO,
 lista_variaveis = base::list(
   "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_1", "v11"),
   "v10_1" = "v10_1"
   ),
 adc_labels = TRUE
)

# Warning2: Enunciado de alguma variável faltando
IPpackage::FUN_Citou(
 TABELA = TABELA,
 DICIONARIO = DICIONARIO\%>\%
   dplyr::mutate(
     pergunta_enunciado = base::ifelse(
       opcao_variavel \%in\% c("v5","v11"), NA, pergunta_enunciado
       )
   ),
 lista_variaveis = base::list(
   "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
   "v5" = "v5",
   "v11" = "v11"
 ),
 adc_labels = TRUE
)

# Todos os warnings
IPpackage::FUN_Citou(
 TABELA = TABELA,
 DICIONARIO = DICIONARIO\%>\%
   dplyr::mutate(
     pergunta_enunciado = base::ifelse(
       opcao_variavel \%in\% c("v5","v11"), NA, pergunta_enunciado
     )
   ),
 lista_variaveis = base::list(
   "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_1", "v11"),
   "v10_1" = "v10_1",
   "v5" = "v5",
   "v11" = "v11"
 ),
 adc_labels = TRUE
)

}
