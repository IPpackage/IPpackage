% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/func_processamento.R
\name{func_processamento}
\alias{func_processamento}
\title{func_processamento}
\usage{
func_processamento(
  TABELA_splits,
  DICIONARIO,
  isoladas = NA,
  mrg = NA,
  citou = NA,
  medias = NA,
  cruza_SPLITS = TRUE,
  cruza_isoladas = NA,
  cruza_mrg = NA,
  cruza_media = NA,
  cruza_citou = NA
)
}
\arguments{
\item{TABELA_splits}{Banco de dados a ser analisado (tipo nest agrupado pelos splits).}

\item{DICIONARIO}{Dicionário correspondente à TABELA_splits.}

\item{isoladas}{vetor com as variáveis para as quais as frequências devem ser calculadas.}

\item{mrg}{Lista das variáveis que são do tipo MRGs (metodologia: pivota).}

\item{citou}{Lista das variáveis e MRGs que são do tipo Citou/Não Citou}

\item{medias}{Lista das variáveis e MRGs que devem ter sua média calculada (metodologia: pivota).}

\item{cruza_SPLITS}{Parâmetro para selecionar só alguns splits pro cruzamento. Se TRUE, calcula para todos os splits.}

\item{cruza_isoladas}{Lista dos cruzamentos entre variáveis isoladas.}

\item{cruza_mrg}{Lista dos cruzamentos envolvendo MRGs (metodologia: pivota).}

\item{cruza_media}{Lista dos cruzamentos envolvendo variáveis e/ou MRGs do tipo média.}

\item{cruza_citou}{Lista dos cruzamentos envolvendo variáveis e/ou MRGs do tipo Citou/Não Citou.}
}
\description{
Esta função de processamento é utilizada para calcular frequências,
médias e cruzamentos.
}
\details{
Consute o livro para mais detalhes e exemplos.
}
\examples{

# Pacotes---------------------------------------------------------------------
library(IPpackage)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

# Importando banco de Dados---------------------------------------------------
split = IPpackage::IPpackage_split
split_peso = IPpackage::IPpackage_split_peso
TABELA = IPpackage::IPpackage_exemplo \%>\%
 dplyr::select(-peso)

#Passando 'outros' (v10) para do tipo citou/não citou-------------------------
TABELA = TABELA \%>\%
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

# Dividindo o banco por split e adicionando peso------------------------------
all_splits = split$split_id \%>\%
 base::unique()

for( i in 1:length(all_splits) )
{# Start: rodando para cada split

 # Código para filtrar o split no R
 filtro = split \%>\%
   dplyr::filter(split_id == all_splits[i]) \%>\%
   dplyr::select(filtro_r) \%>\%
   dplyr::pull()

 # Usando o códifo para filtrar a TABELA
 x1 = TABELA \%>\%
   dplyr::filter(
     base::eval(
       base::parse(
         text = filtro
         )
       )
     ) \%>\%
   dplyr::mutate(id = as.numeric(id))

 # Usando o códifo para filtrar a split_peso
 x2 = split_peso \%>\%
   dplyr::filter(split_id == all_splits[i]) \%>\%
   dplyr::mutate(splits = split_id) \%>\%
   dplyr::select(questionario_id, peso, splits) \%>\%
   dplyr::mutate(questionario_id = as.numeric(questionario_id))

 # Colocando o peso na Tabela
 x = dplyr::left_join(
   x1,
   x2,
   by = c("id" = "questionario_id")
 ) \%>\%
   base::unique() \%>\%
   dplyr::mutate(splits = base::as.character(splits))

 # Printando se tudo bateu ou não
 base::cat(
   base::paste0(
     all_splits[i],
     ": Nenhum peso = NA? ",
     x \%>\%
       dplyr::filter(base::is.na(peso)) \%>\%
       base::nrow() == 0,
     " | nº linhas iguais = ",
     base::nrow(x1) == base::nrow(x2),
     " | ",
     split \%>\%
       dplyr::filter(split_id == all_splits[i]) \%>\%
       dplyr::select(split_nome) \%>\%
       dplyr::pull() \%>\%
       base::unique(),
     " | n = ",
     base::nrow(x),
     "| sum(peso) = ",
     IPpackage::round_excel(base::sum(x$peso), 2),
     " [", i, "/", base::length(all_splits), "]\n"
   ) \%>\%
     stringr::str_replace_all("TRUE", "\033[32mTRUE\033[0m") \%>\%
     stringr::str_replace_all("FALSE", "\033[31mFALSE\033[0m")
 )

 # Nestando o Banco de dados
 x = x \%>\%
   dplyr::group_nest(splits)

 if(i == 1)
 {# Start: armazenando o banco nest

   TABELA_split = x

 }else{

   TABELA_split = base::rbind(TABELA_split, x)

 }# End: armazenando o banco nest

}# End: rodando para cada split

base::rm(x, x1, x2, i, all_splits, filtro) \%>\%
 base::suppressWarnings()

#Rodando----------------------------------------------------------------------

processamento = IPpackage::func_processamento(
   TABELA_splits = TABELA_split,
   DICIONARIO = DICIONARIO,
   isoladas = colnames(TABELA),
   mrg = list(
     "v3mrg"=c("v3_1","v3_2","v3_3"),
     "v19mrg"=c("v19_1","v19_2")
     ),
   citou = base::list(
     # MRG Citou
     "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
     # Isolada
     "v4" = "v4"
   ),
   medias = base::list(
     # Isolada
     "v2" = "v2",
     # MRG (pivotando)
     "v17mrg" = c("v17", "v18"),
     # Isolada
     "v17" = "v17",
     # Isolada
     "v18" = "v18"
   ),
   cruza_SPLITS = TRUE,
   cruza_isoladas = base::list(
     # v15 vs v16 fechando na v15
     base::list("v15", "v16", "v15")
     # v15 vs v16 fechando na v16
     ,base::list("v15", "v16", "v16")
   ),
   cruza_mrg = base::list(
     # Isolada vs MRG fechando na Isolada
     base::list(
       base::list("v1"="v1"),
       base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
       "v1"
     ),
     # Isolada vs MRG fechando no MRG
     base::list(
       base::list("v1"="v1"),
       base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
       "v3mrg"
     ),
     # MRG vs MRG fechando no primeiro MRG
     base::list(
       base::list("v19mrg"=c("v19_1","v19_2")),
       base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
       "v19mrg"
     ),
     # MRG vs MRG fechando no segundo MRG
     base::list(
       base::list("v19mrg"=c("v19_1","v19_2")),
       base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
       "v3mrg"
     )
   ),
   cruza_media = base::list(
     # Categoria MRG (v15mrg - v15, v16) vs Média MRG (v17mrg - v17, v18)
     base::list(
       "v15mrg" = c("v15", "v16"),
       "v17mrg" = c("v17", "v18")
     ),
     # Categoria Isolada (v15) vs Média MRG (v17mrg - v17, v18):
     base::list(
       "v15" = "v15",
       "v17mrg" = c("v17", "v18")
     ),
     # Categoria MRG (v15mrg - v15, v16) vs Média Isolada
     base::list(
       "v15mrg" = c("v15", "v16"),
       "v17" = "v17"
     ),
     base::list(
       "v15mrg" = c("v15", "v16"),
       "v18" = "v18"
     ),
     # Categoria Isolada vs Média Isolada
     base::list(
       "v15" = "v15",
       "v17" = "v17"
     ),
     base::list(
       "v15" = "v15",
       "v18" = "v18"
     )
   ),
   cruza_citou = base::list(
     #MRG vs MRG Ci. (Divide pela base - quem falou pelo menos um cód em v15mrg)
     base::list(
       base::list(
         "v15mrg" = c("v15", "v16")),
       base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
                              "v11")),
       base::list("v15mrg")
     ),
     #MRG vs MRG Citou (Divide pela soma de n - soma 100\%)
     base::list(
       base::list("v15mrg" = c("v15", "v16")),
       base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
                              "v11")),
       base::list("v4mrg")
     ),
     #Isolada vs MRG Citou (Divide pela base - quem falou cód em v15)
     base::list(
       base::list("v15" = "v15"),
       base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
                              "v11")),
       base::list("v15")
     ),
     #Isolada vs MRG Citou (Divide pela soma de n - soma 100\%)
     base::list(
       base::list("v15" = "v15"),
       base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
                              "v11")),
       base::list("v4mrg")
     ),
     #MRG vs Citou I. (Divide pela base - quem falou pelo menos um cód em v15mrg)
     base::list(
       base::list(
         "v15mrg" = c("v15", "v16")),
       base::list("v4" = "v4"),
       base::list("v15mrg")
     ),
     #MRG vs Citou Isolada (Divide pela soma de n - soma 100\%)
     base::list(
       base::list("v15mrg" = c("v15", "v16")),
       base::list("v4" = "v4"),
       base::list("v4")
     ),
     #Isolada vs Citou I. (Divide pela base - quem falou cód em v15)
     base::list(
       base::list("v15" = "v15"),
       base::list("v4" = "v4"),
       base::list("v15")
     ),
     #Isolada vs Citou Isolada (Divide pela soma de n - soma 100\%)
     base::list(
       base::list("v15" = "v15"),
       base::list("v4" = "v4"),
       base::list("v4")
     )
   )
)

#Média das Médias-------------------------------------------------------------
IPpackage::PosProce_meanofmean(
 arquivo_medias=processamento[["Medias"]]
 ,variaveis_recalcular=list(
   "v17mrg" = c("v17", "v18")
 )
 ,tipo="Medias"
)

IPpackage::PosProce_meanofmean(
 arquivo_medias=processamento[["Cruz_M"]]
 ,variaveis_recalcular=list(
   "v17mrg" = c("v17", "v18")
 )
 ,tipo="Cruz_M"
)

}
