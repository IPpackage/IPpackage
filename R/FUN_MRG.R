#' @title FUN_MRG
#' @name FUN_MRG
#'
#' @description Pivota e depois calcula a frequência simples (ponderada e não ponderada) das variáveis indicadas em 'lista_variaveis' (MRG).
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param lista_variaveis Variáveis que serão pivotadas (vetor).
#' @param adc_labels Se adc_labels=TRUE, adicionar uma coluna com o label.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#' @import stringr
#' @importFrom purrr map_df
#'
#' @examples
#'
#'#MRG sem erro
#'FUN_MRG(
#'  TABELA=IPpackage::IPpackage_exemplo,
#'  DICIONARIO=IPpackage::IPpackage_dicionario,
#'  lista_variaveis=list("v3mrg"=c("v3_1","v3_2","v3_3")),
#'  adc_labels = TRUE
#')
#'
#'library(dplyr)
#'#MRG sem a variável no DICIONARIO
#'FUN_MRG(
#'  TABELA=IPpackage::IPpackage_exemplo,
#'  DICIONARIO=IPpackage::IPpackage_dicionario%>%dplyr::mutate(opcao_cod=ifelse(
#'    opcao_variavel=="v3mrg",NA,opcao_cod)),
#'  lista_variaveis=list("v3mrg"=c("v3_1","v3_2","v3_3")),
#'  adc_labels = TRUE
#')
#'#MRG com a variável no DICIONARIO, mas faltando Label
#'FUN_MRG(
#'  TABELA=IPpackage::IPpackage_exemplo,
#'  DICIONARIO=IPpackage::IPpackage_dicionario%>%
#'    dplyr::filter(opcao_variavel!="v3mrg"),
#'  lista_variaveis=list("v3mrg"=c("v3_1","v3_2","v3_3")),
#'  adc_labels = TRUE
#')
#'
#' @export
#'
#'


FUN_MRG <- function(
    TABELA,
    DICIONARIO,
    lista_variaveis,
    adc_labels = TRUE
)
{# Start: FUN_MRG

  {# Start: armazenar resultados e definições iniciais

    Tempo_Inicio<-base::Sys.time()
    `%nin%` = base::Negate(`%in%`)
    out <- base::vector("list", length = base::length(lista_variaveis))
    base <- base::vector("list", length = base::length(lista_variaveis))
    vars_nomes <- base::names(lista_variaveis)
    Log_MRG<-base::list()
    all_warning<-c()

  }# End: armazenar resultados e definições iniciais


  for ( i in base::seq_along(out) )
    {# Start: Executando para cada variável

    erro=0

    if( erro==0 )
      {# Start: se não houver erro, executar

      # Seleciona os rótulos do MRG do dicionário
      labels <- DICIONARIO %>%
        dplyr::filter(opcao_variavel == vars_nomes[i]) %>%
        dplyr::select(opcao_cod, opcao_label) %>%
        dplyr::rename(!!vars_nomes[i] := "opcao_cod") %>%
        dplyr::rename(!!stringr::str_c(vars_nomes[i], "_label") := "opcao_label") %>%
        dplyr::mutate(dplyr::across(vars_nomes[i], base::as.character)) %>%
        dplyr::arrange(base::match(.[[1]], stringr::str_sort(.[[1]], numeric = T))) %>%
        dplyr::arrange(.[[1]] %in% "-88") %>%
        dplyr::arrange(.[[1]] %in% "-99")

      # Calcula a base de dados para O MRG atual
      base[[i]] <- TABELA %>%
        dplyr::select(dplyr::all_of(lista_variaveis[[i]]), peso) %>%
        dplyr::mutate("n_base" = base::ifelse(
          test = base::rowSums(!base::is.na(dplyr::across(dplyr::all_of(lista_variaveis[[i]])))) > 0, 1, 0
          )
        ) %>%
        dplyr::mutate("n_base" = base::sum(`n_base`)) %>%
        dplyr::mutate("pct_base" = (100 * `n_base`) / base::nrow(.)) %>%
        dplyr::mutate("n_base_peso" = base::ifelse(
          test = base::rowSums(
            !base::is.na(dplyr::across(dplyr::all_of(lista_variaveis[[i]])))
            ) > 0, peso, 0
          )
        ) %>%
        dplyr::mutate("n_base_peso" = base::sum(`n_base_peso`)) %>%
        dplyr::mutate("pct_base_peso" = (100 * `n_base_peso`) / base::sum(peso)) %>%
        dplyr::distinct(n_base, pct_base, n_base_peso, pct_base_peso)

      # Calcula a tabela de frequência para o MRG atual
      out[[i]] <- TABELA %>%
        dplyr::select(dplyr::all_of(lista_variaveis[[i]]), peso) %>%
        tidyr::pivot_longer(
          cols = dplyr::all_of(lista_variaveis[[i]]),
          names_to = "Variavel",
          values_to = "Respostas"
        ) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of("Respostas")), .drop = FALSE) %>%
        dplyr::summarise("n" = n(), "n_peso" = base::sum(peso), .groups = "drop") %>%
        dplyr::filter(!base::is.na(.[[1]])) %>%
        dplyr::mutate("pct" = (100 * n / base::sum(n))) %>%
        dplyr::mutate("pct_peso" = (100 * n_peso / base::sum(n_peso))) %>%
        dplyr::rename(!!vars_nomes[i] := "Respostas") %>%
        dplyr::mutate(dplyr::across(vars_nomes[i], base::as.character)) %>%
        dplyr::full_join(labels,by=vars_nomes[i]) %>%
        dplyr::mutate(dplyr::across(c("n", "n_peso", "pct", "pct_peso"), ~ base::ifelse(base::is.na(.x), 0, .x))) %>%
        dplyr::arrange(base::match(.[[1]], stringr::str_sort(.[[1]], numeric = T))) %>%
        dplyr::arrange(.[[1]] %in% "-88") %>%
        dplyr::arrange(.[[1]] %in% "-99") %>%
        dplyr::mutate(dplyr::across(c("n", "n_peso", "pct", "pct_peso"), base::as.double)) %>%
        purrr::map_df(.f = ~ c(., base::ifelse(
          base::is.numeric(.), base::sum(., na.rm = TRUE), "Total"
          ))
        ) %>%
        base::rbind(
          base::list(
            "Base",
            base[[i]]$n_base,
            base[[i]]$n_base_peso,
            base[[i]]$pct_base,
            base[[i]]$pct_base_peso,
            "Base"
            )
          )

      {# Start: Identificando e colocando os warnings no 'Log_MRG'

        if( base::any(base::is.na(out[[i]]%>%dplyr::pull(6))) )
        {# Start: Motivo para algum warning

          if ( vars_nomes[i] %nin% c(DICIONARIO %>% dplyr::distinct(opcao_variavel) %>% dplyr::pull()) )
          {# Start: warning1 ("Variável var não presente no dicionário")

            msg=stringr::str_c(
              "Vari\u00E1vel ",
              vars_nomes[i],
              " n\u00E3o presente no dicion\u00E1rio"
            )

            all_warning=c(all_warning,"\n",msg)

            if( base::length(Log_MRG) == 0 )
            {# Start: Colocando msg do warning1 no 'Log_MRG'

              Log_MRG[[1]] <- tibble::tibble(
                Variavel = vars_nomes[i],
                `Problema` = msg,
                Status = "rodou"
              )

            }else{

              Log_MRG[[1]] <- dplyr::bind_rows(
                Log_MRG[[1]],
                tibble::tibble(
                  Variavel=vars_nomes[i],
                  `Problema`=msg,
                  Status="rodou")
              )

            }# End: Colocando msg do warning1 no 'Log_MRG'
          }
          else
          {# Start: warning2 (Variável var possui label não presente no dicionário. Código(s) []")

            all_cods_falt=base::as.character(out[[i]][which(base::is.na(out[[i]][,6])),1]%>%dplyr::pull())

            if( base::length(all_cods_falt) > 5 )
            {# Start: Limitando o número de códigos a serem inseridos no warning

              all_cods_falt = all_cods_falt[1:5]
              all_cods_falt[6] = "..."

            }# End: Limitando o número de códigos a serem inseridos no warning

            msg=stringr::str_c(
              "Vari\u00E1vel ",
              vars_nomes[i],
              " possui label n\u00E3o presente no dicion\u00E1rio. C\u00F3digo(s) [",
              base::paste(all_cods_falt,collapse = ","),
              "]"
            )

            all_warning=c(all_warning,"\n",msg)

            if( base::length(Log_MRG) == 0 )
            {# Start: Colocando msg do warning2 no 'Log_MRG'

              Log_MRG[[1]] <- tibble::tibble(
                Variavel = vars_nomes[i],
                `Problema`= msg,
                Status = "rodou"
              )

            }else{

              Log_MRG[[1]] <- dplyr::bind_rows(
                Log_MRG[[1]],
                tibble::tibble(
                  Variavel = vars_nomes[i],
                  `Problema` = msg,
                  Status = "rodou"
                )
              )

            }# End: Colocando msg do warning2 no 'Log_MRG'

          }# End: warning2 (Variável var possui label não presente no dicionário. Código(s) []")

        }# End: Motivo para algum warning

      }# End: Identificando e colocando os warnings no 'Log_MRG'

      if( adc_labels == FALSE )
      {# Start: se não quiser adicionar o Label na saída

        #Remove a coluna com o Label
        out[[i]]<-out[[i]] %>%
          dplyr::select(-base::ncol(.))

      }# End: se não quiser adicionar o Label na saída

    }# End: se não houver erro, executar

  }# End: Executando para cada variável

  if( base::length(all_warning) > 0 )
  {# Start: tiver algum warning, printar

    base::warning(all_warning, call. = FALSE)

  }# End: tiver algum warning, printar

  # Retorna os resultados e os registros de mrg (Log_MRG)
  return(base::list(Resultado_MRG = out, Log_MRG = Log_MRG))

}# End: FUN_MRG

