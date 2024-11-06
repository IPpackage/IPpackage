#' @title func_processamento
#' @name func_processamento
#'
#' @description Esta função de processamento é utilizada para calcular frequências,
#' médias e cruzamentos.
#'
#' @param TABELA_splits Banco de dados a ser analisado (tipo nest agrupado pelos splits).
#' @param DICIONARIO Dicionário correspondente à TABELA_splits.
#' @param isoladas vetor com as variáveis para as quais as frequências devem ser calculadas.
#' @param mrg Lista das variáveis que são do tipo MRGs (metodologia: pivota).
#' @param citou Lista das variáveis e MRGs que são do tipo Citou/Não Citou
#' @param medias Lista das variáveis e MRGs que devem ter sua média calculada (metodologia: pivota).
#' @param cruza_SPLITS Parâmetro para selecionar só alguns splits pro cruzamento. Se TRUE, calcula para todos os splits.
#' @param cruza_isoladas Lista dos cruzamentos entre variáveis isoladas.
#' @param cruza_mrg Lista dos cruzamentos envolvendo MRGs (metodologia: pivota).
#' @param cruza_media Lista dos cruzamentos envolvendo variáveis e/ou MRGs do tipo média.
#' @param cruza_citou Lista dos cruzamentos envolvendo variáveis e/ou MRGs do tipo Citou/Não Citou.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import cli
#' @importFrom tidyr unnest
#' @importFrom tidyr replace_na
#' @importFrom purrr map
#' @importFrom purrr map2
#' @importFrom purrr pluck
#' @importFrom lubridate seconds_to_period
#' @importFrom lubridate second
#'
#' @examples
#'
#'# Pacotes---------------------------------------------------------------------
#'library(IPpackage)
#'library(purrr)
#'library(dplyr)
#'library(tidyr)
#'library(stringr)
#'
#'# Importando banco de Dados---------------------------------------------------
#'split = IPpackage::IPpackage_split
#'split_peso = IPpackage::IPpackage_split_peso
#'TABELA = IPpackage::IPpackage_exemplo %>%
#'  dplyr::select(-peso)
#'
#'#Passando 'outros' (v10) para do tipo citou/não citou-------------------------
#'TABELA = TABELA %>%
#'  dplyr::mutate(
#'    v10_1_o = dplyr::case_when(
#'      base::is.na(v4) & base::is.na(v10_1) ~ NA,
#'      base::is.na(v10_1) ~ 2,
#'      !base::is.na(v10_1) ~ 1
#'    ),
#'    v10_2_o = dplyr::case_when(
#'      base::is.na(v4) & base::is.na(v10_2) ~ NA,
#'      base::is.na(v10_2) ~ 2,
#'      !base::is.na(v10_2) ~ 1
#'    ),
#'    v10_outros = dplyr::case_when(
#'      base::is.na(v10_1_o) & base::is.na(v10_2_o) ~ NA,
#'      v10_1_o == 1 | v10_2_o == 1 ~ 1,
#'      TRUE ~ 2
#'    )
#'  )
#'TABELA %>% dplyr::count(v4,v10_1,v10_1_o)
#'TABELA %>% dplyr::count(v4,v10_2,v10_2_o)
#'TABELA %>% dplyr::count(v4,v10_1_o,v10_2_o,v10_outros)
#'
#'DICIONARIO = IPpackage::IPpackage_dicionario %>%
#'  base::rbind(
#'    tibble::tibble(
#'      opcao_cod = base::rep(c(1, 2), 3),
#'      opcao_label = base::rep(c("Citou", "Não citou"), 3),
#'      opcao_variavel = c(
#'        base::rep("v10_outros", 2),
#'        base::rep("v10_1_o", 2),
#'        base::rep("v10_2_o", 2)
#'      ),
#'      pergunta_enunciado = c(
#'        base::rep("v10_outros - Citou (Outros)", 2),
#'        base::rep("v10_1_o - Citou (Outros)", 2),
#'        base::rep("v10_2_o - Citou (Outros)", 2)
#'      )
#'    )
#'  )
#'
#'# Dividindo o banco por split e adicionando peso------------------------------
#'all_splits = split$split_id %>%
#'  base::unique()
#'
#'for( i in 1:length(all_splits) )
#'{# Start: rodando para cada split
#'
#'  # Código para filtrar o split no R
#'  filtro = split %>%
#'    dplyr::filter(split_id == all_splits[i]) %>%
#'    dplyr::select(filtro_r) %>%
#'    dplyr::pull()
#'
#'  # Usando o códifo para filtrar a TABELA
#'  x1 = TABELA %>%
#'    dplyr::filter(
#'      base::eval(
#'        base::parse(
#'          text = filtro
#'          )
#'        )
#'      ) %>%
#'    dplyr::mutate(id = as.numeric(id))
#'
#'  # Usando o códifo para filtrar a split_peso
#'  x2 = split_peso %>%
#'    dplyr::filter(split_id == all_splits[i]) %>%
#'    dplyr::mutate(splits = split_id) %>%
#'    dplyr::select(questionario_id, peso, splits) %>%
#'    dplyr::mutate(questionario_id = as.numeric(questionario_id))
#'
#'  # Colocando o peso na Tabela
#'  x = dplyr::left_join(
#'    x1,
#'    x2,
#'    by = c("id" = "questionario_id")
#'  ) %>%
#'    base::unique() %>%
#'    dplyr::mutate(splits = base::as.character(splits))
#'
#'  # Printando se tudo bateu ou não
#'  base::cat(
#'    base::paste0(
#'      all_splits[i],
#'      ": Nenhum peso = NA? ",
#'      x %>%
#'        dplyr::filter(base::is.na(peso)) %>%
#'        base::nrow() == 0,
#'      " | nº linhas iguais = ",
#'      base::nrow(x1) == base::nrow(x2),
#'      " | ",
#'      split %>%
#'        dplyr::filter(split_id == all_splits[i]) %>%
#'        dplyr::select(split_nome) %>%
#'        dplyr::pull() %>%
#'        base::unique(),
#'      " | n = ",
#'      base::nrow(x),
#'      "| sum(peso) = ",
#'      IPpackage::round_excel(base::sum(x$peso), 2),
#'      " [", i, "/", base::length(all_splits), "]\n"
#'    ) %>%
#'      stringr::str_replace_all("TRUE", "\033[32mTRUE\033[0m") %>%
#'      stringr::str_replace_all("FALSE", "\033[31mFALSE\033[0m")
#'  )
#'
#'  # Nestando o Banco de dados
#'  x = x %>%
#'    dplyr::group_nest(splits)
#'
#'  if(i == 1)
#'  {# Start: armazenando o banco nest
#'
#'    TABELA_split = x
#'
#'  }else{
#'
#'    TABELA_split = base::rbind(TABELA_split, x)
#'
#'  }# End: armazenando o banco nest
#'
#'}# End: rodando para cada split
#'
#'base::rm(x, x1, x2, i, all_splits, filtro) %>%
#'  base::suppressWarnings()
#'
#'#Rodando----------------------------------------------------------------------
#'
#'processamento = IPpackage::func_processamento(
#'    TABELA_splits = TABELA_split,
#'    DICIONARIO = DICIONARIO,
#'    isoladas = colnames(TABELA),
#'    mrg = list(
#'      "v3mrg"=c("v3_1","v3_2","v3_3"),
#'      "v19mrg"=c("v19_1","v19_2")
#'      ),
#'    citou = base::list(
#'      # MRG Citou
#'      "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
#'      # Isolada
#'      "v4" = "v4"
#'    ),
#'    medias = base::list(
#'      # Isolada
#'      "v2" = "v2",
#'      # MRG (pivotando)
#'      "v17mrg" = c("v17", "v18"),
#'      # Isolada
#'      "v17" = "v17",
#'      # Isolada
#'      "v18" = "v18"
#'    ),
#'    cruza_SPLITS = TRUE,
#'    cruza_isoladas = base::list(
#'      # v15 vs v16 fechando na v15
#'      base::list("v15", "v16", "v15")
#'      # v15 vs v16 fechando na v16
#'      ,base::list("v15", "v16", "v16")
#'    ),
#'    cruza_mrg = base::list(
#'      # Isolada vs MRG fechando na Isolada
#'      base::list(
#'        base::list("v1"="v1"),
#'        base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'        "v1"
#'      ),
#'      # Isolada vs MRG fechando no MRG
#'      base::list(
#'        base::list("v1"="v1"),
#'        base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'        "v3mrg"
#'      ),
#'      # MRG vs MRG fechando no primeiro MRG
#'      base::list(
#'        base::list("v19mrg"=c("v19_1","v19_2")),
#'        base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'        "v19mrg"
#'      ),
#'      # MRG vs MRG fechando no segundo MRG
#'      base::list(
#'        base::list("v19mrg"=c("v19_1","v19_2")),
#'        base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'        "v3mrg"
#'      )
#'    ),
#'    cruza_media = base::list(
#'      # Categoria MRG (v15mrg - v15, v16) vs Média MRG (v17mrg - v17, v18)
#'      base::list(
#'        "v15mrg" = c("v15", "v16"),
#'        "v17mrg" = c("v17", "v18")
#'      ),
#'      # Categoria Isolada (v15) vs Média MRG (v17mrg - v17, v18):
#'      base::list(
#'        "v15" = "v15",
#'        "v17mrg" = c("v17", "v18")
#'      ),
#'      # Categoria MRG (v15mrg - v15, v16) vs Média Isolada
#'      base::list(
#'        "v15mrg" = c("v15", "v16"),
#'        "v17" = "v17"
#'      ),
#'      base::list(
#'        "v15mrg" = c("v15", "v16"),
#'        "v18" = "v18"
#'      ),
#'      # Categoria Isolada vs Média Isolada
#'      base::list(
#'        "v15" = "v15",
#'        "v17" = "v17"
#'      ),
#'      base::list(
#'        "v15" = "v15",
#'        "v18" = "v18"
#'      )
#'    ),
#'    cruza_citou = base::list(
#'      #MRG vs MRG Ci. (Divide pela base - quem falou pelo menos um cód em v15mrg)
#'      base::list(
#'        base::list(
#'          "v15mrg" = c("v15", "v16")),
#'        base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
#'                               "v11")),
#'        base::list("v15mrg")
#'      ),
#'      #MRG vs MRG Citou (Divide pela soma de n - soma 100%)
#'      base::list(
#'        base::list("v15mrg" = c("v15", "v16")),
#'        base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
#'                               "v11")),
#'        base::list("v4mrg")
#'      ),
#'      #Isolada vs MRG Citou (Divide pela base - quem falou cód em v15)
#'      base::list(
#'        base::list("v15" = "v15"),
#'        base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
#'                               "v11")),
#'        base::list("v15")
#'      ),
#'      #Isolada vs MRG Citou (Divide pela soma de n - soma 100%)
#'      base::list(
#'        base::list("v15" = "v15"),
#'        base::list("v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros",
#'                               "v11")),
#'        base::list("v4mrg")
#'      ),
#'      #MRG vs Citou I. (Divide pela base - quem falou pelo menos um cód em v15mrg)
#'      base::list(
#'        base::list(
#'          "v15mrg" = c("v15", "v16")),
#'        base::list("v4" = "v4"),
#'        base::list("v15mrg")
#'      ),
#'      #MRG vs Citou Isolada (Divide pela soma de n - soma 100%)
#'      base::list(
#'        base::list("v15mrg" = c("v15", "v16")),
#'        base::list("v4" = "v4"),
#'        base::list("v4")
#'      ),
#'      #Isolada vs Citou I. (Divide pela base - quem falou cód em v15)
#'      base::list(
#'        base::list("v15" = "v15"),
#'        base::list("v4" = "v4"),
#'        base::list("v15")
#'      ),
#'      #Isolada vs Citou Isolada (Divide pela soma de n - soma 100%)
#'      base::list(
#'        base::list("v15" = "v15"),
#'        base::list("v4" = "v4"),
#'        base::list("v4")
#'      )
#'    )
#')
#'
#'#Média das Médias-------------------------------------------------------------
#'IPpackage::PosProce_meanofmean(
#'  arquivo_medias=processamento[["Medias"]]
#'  ,variaveis_recalcular=list(
#'    "v17mrg" = c("v17", "v18")
#'  )
#'  ,tipo="Medias"
#')
#'
#'IPpackage::PosProce_meanofmean(
#'  arquivo_medias=processamento[["Cruz_M"]]
#'  ,variaveis_recalcular=list(
#'    "v17mrg" = c("v17", "v18")
#'  )
#'  ,tipo="Cruz_M"
#')
#'
#' @export
#'

func_processamento <- function(
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
{# Start: func_processamento

  {# Start: armazenar resultados e definições iniciais

    `%nin%` = base::Negate(`%in%`)
    start_tempo <- base::proc.time()
    out <- base::vector("list", length = 7)
    base::names(out) <- c("Freq", "Base", "Medias", "Cruz", "Cruz_M", "Log", "Labels")

  }# End: armazenar resultados e definições iniciais

  # base::cat("\n\n------------------------------------------------ ISOLADAS ------------------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" VARI\u00C1VEIS - ISOLADAS "))

  # Abas "Freq" e "Base"
  if ( base::all(base::is.na(isoladas)) )
  {# Start: rodando as ISOLADAS

    df_isoladas <- tibble::tibble(
      "splits"        = base::character(),
      "variavel"      = base::character(),
      "codigo"        = base::character(),
      "n"             = base::numeric(),
      "n_peso"        = base::numeric(),
      "pct"           = base::numeric(),
      "pct_peso"      = base::numeric(),
      "total"         = base::numeric(),
      "total_peso"    = base::numeric(),
      "n_base"        = base::numeric(),
      "n_base_peso"   = base::numeric(),
      "pct_base"      = base::numeric(),
      "pct_base_peso" = base::numeric()
    )

    log_isoladas <- tibble::tibble(
      splits = base::character(),
      log = base::list()
    )

    base::warning("Vari\u00E1veis 'isoladas' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_isoladas <- TABELA_splits %>%
      dplyr::mutate(
        data = purrr::map2(
          data,
          splits,
          .f = function(x, y) {

            IPpackage::FUN_isoladas(
              TABELA = x,
              DICIONARIO = DICIONARIO,
              variaveis = isoladas,
              adc_labels = TRUE
            )

          },
          .progress = base::list(
            clear = FALSE,
            name = "ISOLADAS",
            format = "Isoladas: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      dplyr::mutate(
        res = purrr::map(
          data,
          .f = ~ purrr::pluck(., 1)
        )
      ) %>%
      dplyr::mutate(
        log = purrr::map(
          data,
          .f = ~ purrr::pluck(., 2)
        )
      )

    log_isoladas <- df_isoladas %>%
      dplyr::select(splits, log)

    df_isoladas <- df_isoladas %>%
      dplyr::select(-c(log, data)) %>%
      dplyr::mutate(
        res = purrr::map(
          res,
          .f = ~ purrr::map(
            .,
            .f = ~ IPpackage::func_freqToDF(.)
          ),
          .progress = base::list(
            clear = FALSE,
            name = "Isoladas",
            format = "Isoladas to DF: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      tidyr::unnest(res) %>%
      tidyr::unnest(res) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ tidyr::replace_na(., 0)
        )
      )
    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")

  }# End: rodando as ISOLADAS

  # base::cat("\n\n-------------------------------------------------- MRG --------------------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" VARI\u00C1VEIS - MRG "))

  if ( base::all(base::is.na(mrg)) )
  {# Start: rodando MRG

    df_mrg <- tibble::tibble(
      "splits"        = base::character(),
      "variavel"      = base::character(),
      "codigo"        = base::character(),
      "n"             = base::numeric(),
      "n_peso"        = base::numeric(),
      "pct"           = base::numeric(),
      "pct_peso"      = base::numeric(),
      "total"         = base::numeric(),
      "total_peso"    = base::numeric(),
      "n_base"        = base::numeric(),
      "n_base_peso"   = base::numeric(),
      "pct_base"      = base::numeric(),
      "pct_base_peso" = base::numeric()
    )

    log_mrg <- tibble::tibble(
      splits = base::character(),
      log = base::list()
    )

    base::warning("Vari\u00E1veis 'mrg' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_mrg <- TABELA_splits %>%
      dplyr::mutate(
        data = purrr::map2(
          data,
          splits,
          .f = function(x, y) {

            IPpackage::FUN_MRG(
              TABELA = x,
              DICIONARIO = DICIONARIO,
              lista_variaveis = mrg,
              adc_labels = TRUE
            )

          },
          .progress = base::list(
            clear = FALSE,
            name = "MRG",
            format = "MRG: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      dplyr::mutate(
        res = purrr::map(
          data,
          .f = ~ purrr::pluck(., 1)
        )
      ) %>%
      dplyr::mutate(
        log = purrr::map(
          data,
          .f = ~ purrr::pluck(., 2)
        )
      )

    log_mrg <- df_mrg %>%
      dplyr::select(splits, log)

    df_mrg <- df_mrg %>%
      dplyr::select(-c(log, data)) %>%
      dplyr::mutate(
        res = purrr::map(
          res,
          .f = ~ purrr::map(
            .,
            .f = ~ IPpackage::func_freqToDF(.)
          ),
          .progress = base::list(
            clear = FALSE,
            name = "MRG",
            format = "MRG to DF: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      tidyr::unnest(res) %>%
      tidyr::unnest(res) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ tidyr::replace_na(., 0)
        )
      )
    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")

  }# End: rodando MRG

  # base::cat("\n\n-------------------------------------------------- CITOU --------------------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" VARI\u00C1VEIS - CITOU "))

  if ( base::all(base::is.na(citou)) )
  {# Start: CITOU

    df_citou <- tibble::tibble(
      "splits"        = base::character(),
      "variavel"      = base::character(),
      "codigo"        = base::character(),
      "n"             = base::numeric(),
      "n_peso"        = base::numeric(),
      "pct"           = base::numeric(),
      "pct_peso"      = base::numeric(),
      "total"         = base::numeric(),
      "total_peso"    = base::numeric(),
      "n_base"        = base::numeric(),
      "n_base_peso"   = base::numeric(),
      "pct_base"      = base::numeric(),
      "pct_base_peso" = base::numeric()
    )

    log_citou <- tibble::tibble(
      splits = base::character(),
      log = base::list()
    )

    base::warning("Vari\u00E1veis 'citou' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_citou <- TABELA_splits %>%
      dplyr::mutate(
        data = purrr::map2(
          data,
          splits,
          .f = function(x, y) {

            IPpackage::FUN_Citou(
              TABELA = x,
              DICIONARIO = DICIONARIO,
              lista_variaveis = citou,
              adc_labels = TRUE
            )

          },
          .progress = base::list(
            clear = FALSE,
            name = "CITOU",
            format = "Citou: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      dplyr::mutate(
        res = purrr::map(
          data,
          .f = ~ purrr::pluck(., 1)
        )
      ) %>%
      dplyr::mutate(
        log = purrr::map(
          data,
          .f = ~ purrr::pluck(., 2)
        )
      )

    log_citou <- df_citou %>%
      dplyr::select(splits, log)

    df_citou <- df_citou %>%
      dplyr::select(-c(log, data)) %>%
      dplyr::mutate(
        res = purrr::map(
          res,
          .f = ~ purrr::map(
            .,
            .f = ~ IPpackage::func_freqToDF(.)
          ),
          .progress = base::list(
            clear = FALSE,
            name = "Citou",
            format = "Citou to DF: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )
      ) %>%
      tidyr::unnest(res) %>%
      tidyr::unnest(res) %>%
      dplyr::mutate(
        dplyr::across(
          dplyr::where(base::is.numeric),
          ~ tidyr::replace_na(., 0)
        )
      )
    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")

  }# End: CITOU

  {# Start: ajustando saída de CITOU

    dfs <- dplyr::bind_rows(
      df_isoladas,
      df_mrg,
      df_citou
    )

    out$Freq <- dfs %>%
      dplyr::select(splits, variavel, codigo, n, n_peso, pct, pct_peso)

    out$Base <- dfs %>%
      dplyr::select(splits, variavel, total, total_peso, n_base, n_base_peso, pct_base, pct_base_peso) %>%
      dplyr::distinct() %>%
      dplyr::left_join(
        y = DICIONARIO %>%
          dplyr::select(dplyr::any_of(c("opcao_variavel", "pergunta_enunciado", "descbase", "regrasbase"))) %>%
          dplyr::distinct(),
        by = dplyr::join_by(variavel == opcao_variavel)
      ) %>%
      dplyr::select(dplyr::any_of(c(
        "splits", "variavel", "n_base", "n_base_peso", "pct_base", "pct_base_peso",
        "regrasbase", "descbase", "pergunta_enunciado"
      )))

    out$Labels <- dfs %>%
      dplyr::left_join(
        y = DICIONARIO %>%
          dplyr::mutate(
            dplyr::across(
              opcao_cod,
              base::as.character
            )
          ) %>%
          dplyr::select(
            dplyr::any_of(
              c("opcao_variavel", "pergunta_enunciado")
            )
          ) %>%
          dplyr::distinct(),
        by = c("variavel" = "opcao_variavel")
      ) %>%
      base::unique()

  }# End: ajustando saída de CITOU

  # base::cat("\n\n-------------------------------------------------- MEDIAS --------------------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" VARI\u00C1VEIS - M\u00C9DIAS "))

  # Aba "Medias"
  if ( base::all(base::is.na(medias)) )
  {# Start: MEDIAS

    medias_normal <- tibble::tibble(
      "splits"             = base::character(),
      "variavel"           = base::character(),
      "media"              = base::numeric(),
      "media_peso"         = base::numeric(),
      "ma_p_splits_SupInf" = base::numeric(),
      "ma_p_splits_Sup"    = base::numeric(),
      "ma_p_splits_Inf"    = base::numeric(),
      "ma_p_splits_SemOut" = base::numeric(),
      "n_base"             = base::numeric(),
      "n_base_peso"        = base::numeric(),
      "desvp"              = base::numeric(),
      "desvp_peso"         = base::numeric(),
      "erro"               = base::numeric(),
      "erro_peso"          = base::numeric(),
      "min"                = base::numeric(),
      "Q1"                 = base::numeric(),
      "mediana"            = base::numeric(),
      "Q3"                 = base::numeric(),
      "max"                = base::numeric(),
      "cv"                 = base::numeric()
    )

    medias_aparadas <- tibble::tibble(
      splits = base::character(),
      variavel = base::character(),
      media = base::numeric(),
      media_peso = base::numeric(),
      n = base::numeric(),
      n_peso = base::numeric(),
      desvp = base::numeric(),
      desvp_peso = base::numeric(),
      erro = base::numeric(),
      erro_peso = base::numeric()
    )

    log_medias_normal <- tibble::tibble(
      splits = base::character(),
      log = base::list()
    )

    log_medias_aparadas <- tibble::tibble(
      splits = base::character(),
      log = base::list()
    )

    base::warning("Vari\u00E1vel 'm\u00E9dia' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    medias_normal = TABELA_splits %>%

      dplyr::mutate(
        data = purrr::map2(
          data,
          splits,
          .f = function(x, y) {

            IPpackage::FUN_media(
              TABELA = x,
              lista_variaveis = medias
            )

          },
          .progress = base::list(
            clear = FALSE,
            name = "M\u00C9DIAS",
            format = "M\u00E9dias: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}"
          )
        )) %>%
      dplyr::mutate(
        res = purrr::map(
          data,
          .f = ~ purrr::pluck(., 1)
        )
      ) %>%
      dplyr::mutate(
        log = purrr::map(
          data,
          .f = ~ purrr::pluck(., 2)
        )
      )

    log_medias_normal <- medias_normal %>%
      dplyr::select(splits, log)

    medias_normal <- medias_normal %>%
      dplyr::select(-c(log, data)) %>%
      tidyr::unnest(res) %>%
      base::data.frame()

    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")

  }# End: MEDIAS

  out$Medias <- medias_normal

  # base::cat("\n\n-------------------------------------------------- CRUZAMENTOS - ISOLADAS ----------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" CRUZAMENTOS - ISOLADAS "))

  TABELA_splits_CRUZAMENTOS <- TABELA_splits %>%
    dplyr::filter(
      base::eval(
        base::parse(
          text = cruza_SPLITS)
      )
    )

  if ( base::all(base::is.na(cruza_isoladas)) )
  {# Start: rodando Cruzamentos - Isoladas

    df_cruzaisoladas <- tibble::tibble(
      "splits"   = base::character(),
      "varlin"   = base::character(),
      "varcol"   = base::character(),
      "codlin"   = base::factor(),
      "codcol"   = base::factor(),
      "n"        = base::numeric(),
      "n_peso"   = base::numeric(),
      "pct"      = base::numeric(),
      "pct_peso" = base::numeric()
    )

    base::warning("Vari\u00E1veis 'cruza_isoladas' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_cruzaisoladas <- base::vector(
      "list",
      length = base::length(cruza_isoladas)
    )

    for ( i in base::seq_along(df_cruzaisoladas) )
    {# Start: rodando - Isoladas

      df_cruzaisoladas[[i]] <- TABELA_splits_CRUZAMENTOS %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_cruzaVar(
              .,
              DICIONARIO = DICIONARIO,
              var1 = cruza_isoladas[[i]][[1]],
              var2 = cruza_isoladas[[i]][[2]],
              var_fecha = cruza_isoladas[[i]][[3]]
            )
          )
        ) %>%
        tidyr::unnest(data)

    }# End: rodando - Isoladas

    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")
  }# Start: rodando Cruzamentos - Isoladas

  # base::cat("\n\n-------------------------------------------------- CRUZAMENTOS - MRG -------------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" CRUZAMENTOS - MRG "))

  if ( base::all(base::is.na(cruza_mrg)) )
  {# Start: rodando Cruzamentos - MRG

    df_cruzamrg <- tibble::tibble(
      "splits"   = base::character(),
      "varlin"   = base::character(),
      "varcol"   = base::character(),
      "codlin"   = base::factor(),
      "codcol"   = base::factor(),
      "n"        = base::numeric(),
      "n_peso"   = base::numeric(),
      "pct"      = base::numeric(),
      "pct_peso" = base::numeric()
    )

    base::warning("Vari\u00E1veis 'cruza_mrg' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_cruzamrg <- base::vector(
      "list",
      length = base::length(cruza_mrg)
    )

    for ( i in base::seq_along(df_cruzamrg) )
    {# Start: rodando - MRG

      df_cruzamrg[[i]] <- TABELA_splits_CRUZAMENTOS %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_cruzaMRG(
              .,
              DICIONARIO = DICIONARIO,
              var1 = cruza_mrg[[i]][[1]],
              varMRG = cruza_mrg[[i]][[2]],
              var_fecha = cruza_mrg[[i]][[3]]
            )
          )
        ) %>%
        tidyr::unnest(data)

    }# End: rodando - MRG

    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")
  }# End: rodando Cruzamentos - MRG

  # base::cat("\n\n-------------------------------------------------- CRUZAMENTOS - CITOU -----------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" CRUZAMENTOS - CITOU "))

  if ( base::all(base::is.na(cruza_citou)) )
  {# Start: rodando Cruzamentos - Citou
    df_cruzacitou <- tibble::tibble(
      "splits"   = base::character(),
      "varlin"   = base::character(),
      "varcol"   = base::character(),
      "codlin"   = base::factor(),
      "codcol"   = base::factor(),
      "n"        = base::numeric(),
      "n_peso"   = base::numeric(),
      "pct"      = base::numeric(),
      "pct_peso" = base::numeric()
    )
    base::warning("Vari\u00E1veis 'cruza_citou' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")
  } else {

    df_cruzacitou <- base::vector(
      "list",
      length = base::length(cruza_citou)
    )

    for ( i in base::seq_along(df_cruzacitou) )
    {# Start: rodando - Citou

      df_cruzacitou[[i]] <- TABELA_splits_CRUZAMENTOS %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_cruzaCitou(
              ., DICIONARIO = DICIONARIO,
              var1 = cruza_citou[[i]][[1]],
              varCitou = cruza_citou[[i]][[2]],
              var_fecha = cruza_citou[[i]][[3]]
            )
          )
        ) %>%
        tidyr::unnest(data)

    }# End: rodando - Citou

    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")
  }# End: rodando Cruzamentos - Citou

  out$Cruz <- dplyr::bind_rows(
    df_cruzaisoladas,
    df_cruzamrg,
    df_cruzacitou
  )

  # base::cat("\n\n-------------------------------------------------- CRUZAMENTOS - MEDIAS -----------------------------------------\n\n")
  cli::cli_h1(text = cli::style_underline(" CRUZAMENTOS - M\u00C9DIAS "))

  if ( base::all(base::is.na(cruza_media)) )
  {# Start: rodando Cruzamentos - Médias

    df_cruzamedia <- tibble::tibble(
      "splits"             = base::character(),
      "varmedia"           = base::character(),
      "varcol"             = base::character(),
      "codcol"             = base::factor(),
      "media"              = base::numeric(),
      "media_peso"         = base::numeric(),
      "ma_p_splits_SupInf" = base::numeric(),
      "ma_p_splits_Sup"    = base::numeric(),
      "ma_p_splits_Inf"    = base::numeric(),
      "ma_p_splits_SemOut" = base::numeric(),
      "n_base"             = base::numeric(),
      "n_base_peso"        = base::numeric(),
      "desvp"              = base::numeric(),
      "desvp_peso"         = base::numeric(),
      "erro"               = base::numeric(),
      "erro_peso"          = base::numeric(),
      "min"                = base::numeric(),
      "Q1"                 = base::numeric(),
      "mediana"            = base::numeric(),
      "Q3"                 = base::numeric(),
      "max"                = base::numeric(),
      "cv"                 = base::numeric()
    )

    base::warning("Vari\u00E1veis 'cruza_media' n\u00E3o fornecidas")
    cli::symbol$line %>% cli::col_br_yellow() %>% base::cat(., "\n")

  } else {

    df_cruzamedia <- base::vector(
      "list",
      length = base::length(cruza_media)
    )

    cli::cli_progress_bar(
      format = "Cruza M\u00E9dias: {cli::pb_bar} {cli::pb_percent} ({cli::pb_current}/{cli::pb_total}) | [{cli::pb_elapsed_clock}] | ETA: {cli::pb_eta}",
      total = base::length(cruza_media),
      clear = FALSE,
    )
    for ( i in base::seq_along(df_cruzamedia) )
    {# Start: rodando - Médias

      df_cruzamedia[[i]] <- TABELA_splits_CRUZAMENTOS %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_cruzaMedia(
              .,
              DICIONARIO = DICIONARIO,
              var1 = cruza_media[[i]][1],
              ind = cruza_media[[i]][2])
          )
        ) %>%
        tidyr::unnest(data)

      cli::cli_progress_update()

    }# End: rodando - Médias

    cli::symbol$tick %>% cli::col_br_green() %>% base::cat(., "\n")
  }# End: rodando Cruzamentos - Médias

  out$Cruz_M <- dplyr::bind_rows(df_cruzamedia)

  # Logs
  out$Log <- dplyr::bind_rows(
    log_isoladas,
    log_mrg,
    log_citou,
    log_medias_normal
  ) %>%
    tidyr::unnest(log) %>%
    tidyr::unnest(log)

  # Tempo
  finish_tempo <- ( base::proc.time() - start_tempo )
  # base::cat("A Fun\u00E7\u00E3o inteira demorou (em minutos): ", finish_tempo[3] / 60, "\n\n")
  periodo_segundos <- lubridate::seconds_to_period(finish_tempo[[3]])
  cli::boxx(
    label = cli::col_br_white(c(
      "Este Processamento inteiro demorou:",
      base::sprintf(
        "%02d:%02d:%02d",
        periodo_segundos@hour,
        periodo_segundos@minute,
        base::floor(lubridate::second(periodo_segundos))
      )
    )),
    background_col = "darkblue",
    padding = c(0, 10, 0, 10),
    margin = 0,
    border_style = "none",
    float = "center",
    align = "center"
  ) %>% base::print()
  cat("\n")


  base::return(out)

}# End: func_processamento

