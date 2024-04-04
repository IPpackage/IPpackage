#' @title FUN_media
#' @name FUN_media
#'
#' @description Pivota e depois calcula a média (ponderada e não ponderada) das variáveis indicadas em 'lista_variaveis'.
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param lista_variaveis Lista das variáveis que serão pivotadas.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_c
#' @importFrom Hmisc wtd.var
#' @importFrom Hmisc wtd.quantile
#'
#' @examples
#'
#'library(dplyr)
#'library(tidyr)
#'library(IPpackage)
#'
#'# Normal
#'IPpackage::FUN_media(
#'  TABELA = IPpackage::IPpackage_exemplo
#'  ,lista_variaveis = list(
#'    "v2" = "v2",
#'    "v17mrg" = c("v17", "v18"),
#'    "v17" = "v17",
#'    "v18" = "v18"
#'  )
#')
#'
#'# warning1 (ao forçar virar numérica, algo virou NA)
#'IPpackage::FUN_media(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = ifelse(id == 4, "texto", v17))
#'  ,lista_variaveis = list(
#'    "v2" = "v2",
#'    "v17mrg" = c("v17", "v18"),
#'    "v17" = "v17",
#'    "v18" = "v18"
#'  )
#')
#'
#'# warning2 (converteu em númerico sem problemas)
#'IPpackage::FUN_media(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = as.character(v17))
#'  ,lista_variaveis=list(
#'    "v2" = "v2",
#'    "v17mrg" = c("v17", "v18"),
#'    "v17" = "v17",
#'    "v18" = "v18"
#'  )
#')
#'
#'# warning3 (se a variável só tiver NA)
#'IPpackage::FUN_media(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = NA)
#'  ,lista_variaveis = list(
#'    "v2" = "v2",
#'    "v17mrg" = c("v17", "v18"),
#'    "v17" = "v17",
#'    "v18" = "v18"
#'  )
#')
#'
#'# Todos os warnings
#'IPpackage::FUN_media(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(
#'      v2 = NA
#'      ,v17 = ifelse(id == 4, "texto", v17)
#'      ,v18 =as.character(v18)
#'    )
#'  ,lista_variaveis = list(
#'    "v2" = "v2",
#'    "v17mrg" = c("v17", "v18"),
#'    "v17" = "v17",
#'    "v18" = "v18"
#'  )
#')
#'
#' @export
#'
#'

FUN_media <- function(
    TABELA,
    lista_variaveis
)
{# Start: FUN_media

  {# Start: armazenar resultados e definições iniciais

    Tempo_Inicio <- base::Sys.time()
    `%nin%`= base::Negate(`%in%`)
    out <- base::vector("list", length = base::length(lista_variaveis))
    Log_media <- base::list()
    all_warning<-c()

  }# End: armazenar resultados e definições iniciais


  for ( i in base::seq_along(out) )
  {# Start: Executando para cada variável

    erro = 0
    TABELA2 = TABELA

    for( t in 1:base::length(lista_variaveis[[i]]) )
    {# Start: Verificando se todas as variáveis são numéricas

      variavel <- lista_variaveis[[i]][t]

      if( !base::is.numeric(TABELA2[[variavel]]) )
      {# Start: Se não for numérica

        antes <- base::sum(base::is.na(TABELA2[[variavel]]))

        TABELA2[[variavel]] <- base::as.numeric(TABELA2[[variavel]]) %>%
          base::suppressWarnings()

        depois <- base::sum(base::is.na(TABELA2[[variavel]]))

        if( depois != antes )
        {# Start: warning1 (ao forçar virar numérica, algo virou NA)

          msg = stringr::str_c(
            "Vari\u{00E1}vel ",
            variavel,
            " n\u{00E3}o \u00E9 num\u{00E9}rica. Na tentativa de for\u{00E7}ar a convers\u{00E3}o ",
            depois-antes,
            " NA(s) foram gerados. Por tanto, a m\u{00E9}dia n\u{00E3}o foi executada"
          )

          erro = erro+1

          all_warning = c(all_warning,"\n",msg)

          if( base::length(Log_media) == 0 )
          {# Start: Colocando msg do warning1 no 'Log_media'

            Log_media[[1]] <- tibble::tibble(
              Variavel = variavel,
              `Problema`= msg,
              Status = "nao rodou"
            )

          }else{

            Log_media[[1]] <- dplyr::bind_rows(
              Log_media[[1]],
              tibble::tibble(
                Variavel = variavel,
                `Problema`= msg,
                Status = "nao rodou"
              )
            )

          }# End: Colocando msg do warning1 no 'Log_media'

        }# End: warning1 (ao forçar virar numérica, algo virou NA)
        else
        {# Start: warning2 (converteu em númerico sem problemas)

          if( base::sum(base::is.na(TABELA2[[variavel]])) != base::nrow(TABELA2) )
          {# Start: verificando se ocorreu o warning2

            msg = stringr::str_c(
              "Vari\u{00E1}vel ",
              variavel,
              " n\u{00E3}o \u00E9 num\u{00E9}rica. Convers\u{00E3}o conclu\u{00ED}da sem a cria\u{00E7}\u{00E3}o de nenhum NA. Por tanto, a m\u{00E9}dia foi executada com a vari\u{00E1}vel convertida"
            )

            all_warning = c(all_warning,"\n",msg)

            if( base::length(Log_media) == 0 )
            {# Start: Colocando msg do warning2 no 'Log_media'

              Log_media[[1]] <- tibble::tibble(
                Variavel = variavel,
                `Problema`= msg,
                Status = "rodou"
              )

            }else{

              Log_media[[1]] <- dplyr::bind_rows(
                Log_media[[1]],
                tibble::tibble(
                  Variavel = variavel,
                  `Problema`= msg,
                  Status = "rodou"
                )
              )

            }# End: Colocando msg do warning2 no 'Log_media'

          }# End: verificando se ocorreu o warning2

        }# End: warning2 (converteu em númerico sem problemas)

      }# End: Se não for numérica

    }# End: Verificando se todas as variáveis são numéricas

    if( erro == 0 )
    {# Start: se não houver erro, executar

      tabela_base=TABELA2 %>%
        dplyr::select(peso, dplyr::all_of(lista_variaveis[[i]])) %>%
        dplyr::filter(base::rowSums(base::is.na(.[, -1])) != base::ncol(.) - 1)

      out[[i]] <- TABELA2 %>%
        dplyr::select(peso, dplyr::all_of(lista_variaveis[[i]])) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(lista_variaveis[[i]]),
          names_to = "Variavel",
          values_to = "Respostas"
        ) %>%
        dplyr::select(-c("Variavel")) %>%
        {
          if ( c(tibble::tibble(.) %>% tidyr::drop_na() %>% base::nrow()) == 0 )
          {# Start: Se não tenho dados para calcular a média

            tibble::tibble(.) %>%
              dplyr::summarise(
                "media" = NA,
                "media_peso" = NA,
                "n_base" = base::nrow(tabela_base),
                "n_base_peso" = base::sum(tabela_base$peso,na.rm=T),
                "desvp" = stats::sd(x = .[[2]], na.rm = TRUE),
                "desvp_peso" = base::sqrt(Hmisc::wtd.var(x = .[[2]], weights = .[[1]], na.rm = TRUE)) %>% base::suppressWarnings(),
                "erro" = desvp / base::sqrt(n_base),
                "erro_peso" = desvp_peso / base::sqrt(n_base_peso),
                "min" = NA,
                "Q1" = NA,
                "mediana" = stats::median(x = .[[2]], na.rm = TRUE),
                "Q3" = NA,
                "max" = NA,
                "cv" = (100 * stats::sd(x = .[[2]], na.rm = TRUE)) / base::mean(x = .[[2]], na.rm = TRUE)
              )

          }# End: Se não tenho dados para calcular a média
          else
          {# Start: Se tenho dados para calcular a média

            tibble::tibble(.) %>%
              dplyr::summarise(
                "media" = base::mean(x = .[[2]], na.rm = TRUE),
                "media_peso" = stats::weighted.mean(x = .[[2]], w = .[[1]], na.rm = TRUE),
                "n_base" = base::nrow(tabela_base),
                "n_base_peso" = base::sum(tabela_base$peso,na.rm=T),
                "desvp" = stats::sd(x = .[[2]], na.rm = TRUE),
                "desvp_peso" = base::sqrt(Hmisc::wtd.var(x = .[[2]], weights = .[[1]], na.rm = TRUE)) %>% base::suppressWarnings(),
                "erro" = desvp / base::sqrt(n_base),
                "erro_peso" = desvp_peso / base::sqrt(n_base_peso),
                "min" = min(.[[2]], na.rm = TRUE),
                "Q1" = Hmisc::wtd.quantile(x = .[[2]], probs = 0.25),
                "mediana" = stats::median(x = .[[2]], na.rm = TRUE),
                "Q3" = Hmisc::wtd.quantile(x = .[[2]], probs = 0.75),
                "max" = base::max(.[[2]], na.rm = TRUE),
                "cv" = (100 * stats::sd(x = .[[2]], na.rm = TRUE)) / base::mean(x = .[[2]], na.rm = TRUE)
              )

          }# End: Se tenho dados para calcular a média

        } %>%
        dplyr::mutate(
          "variavel" = base::names(lista_variaveis[i]),
          .before = 1
        )# %>%
        # dplyr::mutate(
        #   dplyr::across(
        #     .cols = c("media", "media_peso", "n_base", "n_base_peso", "desvp", "desvp_peso", "erro", "erro_peso"),
        #     ~base::ifelse(base::is.na(.x), 0, .x)
        #   )
        # )

    }# End: se não houver erro, executar

    for(t in 1:base::length(lista_variaveis[[i]]))
    {# Start: warning3 (se a variável só tiver NA)

      variavel <- lista_variaveis[[i]][t]

      if(base::sum(base::is.na(TABELA2[[variavel]])) == base::nrow(TABELA2) )
      {# Start: verificando se ocorreu o warning3

        msg = stringr::str_c(
          "Vari\u{00E1}vel ",
          variavel,
          " s\u00F3 tem NA"
        )

        all_warning = c(all_warning,"\n",msg)

        if( base::length(Log_media) == 0 )
        {# Start: Colocando msg do warning3 no 'Log_media'

          Log_media[[1]] <- tibble::tibble(
            Variavel = variavel,
            `Problema`= msg,
            Status = "rodou"
          )

        }else{

          Log_media[[1]] <- dplyr::bind_rows(
            Log_media[[1]],
            tibble::tibble(
              Variavel = variavel,
              `Problema`= msg,
              Status = "rodou"
            )
          )

        }# End: Colocando msg do warning3 no 'Log_media'

      }# End: verificando se ocorreu o warning2

    }# End: warning3 (se a variável só tiver NA)

  }# End: Executando para cada variável

  if( base::length(all_warning) > 0 )
  {# Start: tiver algum warning, printar

    base::warning(all_warning, call. = FALSE)

  }# End: tiver algum warning, printar

  # Retorna os resultados e os registros de mrg (Log_media)
  return(
    base::list(
      Resultado_media = dplyr::bind_rows(out),
      Log_media = base::unique(Log_media)
    )
  )

}# End: FUN_media
