#' @title FUN_isoladas
#' @name FUN_isoladas
#'
#' @description Determinar a frequência simples das 'variaveis', tanto ponderada quanto não ponderada.
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param variaveis Variáveis para as quais as frequências devem ser calculadas (vetor).
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
#' #Uma variável
#' IPpackage::FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=c("v1"),
#'   adc_labels = TRUE
#' )
#'
#' #Mais de uma variável
#' IPpackage::FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=c("v1","v2"),
#'   adc_labels = TRUE
#' )
#'
#' #Todas as variáveis
#' IPpackage::FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=colnames(IPpackage::IPpackage_exemplo),
#'   adc_labels = TRUE
#' )
#'
#' @export
#'
#'

FUN_isoladas <- function(
    TABELA,
    DICIONARIO,
    variaveis,
    adc_labels = TRUE
)
{ #Start: FUN_isoladas

  {# Start: armazenar resultados e definições iniciais

    `%nin%` = base::Negate(`%in%`)
    Tempo_Inicio<-base::Sys.time()
    out <- base::vector("list", length = base::length(variaveis))
    base <- base::vector("list", length = base::length(variaveis))
    Log_isoladas<-base::list()
    all_warning<-c()
    erro=0

  }# End: armazenar resultados e definições iniciais

  if( erro==0 )
  {# Start: se não houver erro, executar

    for ( i in base::seq_along(out) )
    {# Start: Executando para cada variável

      var <- variaveis[i]

      # Seleciona os rótulos da variável do dicionário
      labels <- DICIONARIO %>%
        dplyr::filter(opcao_variavel == var) %>%
        dplyr::select(opcao_cod, opcao_label) %>%
        dplyr::rename(!!var := "opcao_cod") %>%
        dplyr::rename(!!stringr::str_c(var, "_label") := "opcao_label") %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(var), base::as.character)) %>%
        dplyr::arrange(
          base::match(.[[1]], stringr::str_sort(.[[1]], numeric = TRUE))
        ) %>%
        dplyr::arrange(.[[1]] %in% c("-66","-666")) %>%
        dplyr::arrange(.[[1]] %in% c("-77","-777")) %>%
        dplyr::arrange(.[[1]] %in% c("-88","-888")) %>%
        dplyr::arrange(.[[1]] %in% c("-99","-999"))

      # Calcula a base de dados para a variável atual
      base[[i]] <- TABELA %>%
        dplyr::select(dplyr::all_of(var), peso) %>%
        dplyr::mutate("n_base" = base::ifelse(
          test = base::rowSums(!base::is.na(dplyr::across(dplyr::all_of(var)))) > 0, 1, 0)
        ) %>%
        dplyr::mutate("n_base" = base::sum(`n_base`)) %>%
        dplyr::mutate("pct_base" = (100 * `n_base`) / base::nrow(.)) %>%
        dplyr::mutate("n_base_peso" = base::ifelse(
          test = base::rowSums(!base::is.na(dplyr::across(dplyr::all_of(var)))) > 0, peso, 0)
        ) %>%
        dplyr::mutate("n_base_peso" = base::sum(`n_base_peso`)) %>%
        dplyr::mutate("pct_base_peso" = (100 * `n_base_peso`) / base::sum(peso)) %>%
        dplyr::distinct(n_base, pct_base, n_base_peso, pct_base_peso)

      # Calcula a tabela de frequência para a variável atual
      out[[i]] <- TABELA %>%
        dplyr::select(dplyr::all_of(var), peso) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of(var)), .drop = FALSE) %>%
        dplyr::summarise("n" = dplyr::n(), "n_peso" = base::sum(peso), .groups = "drop") %>%
        dplyr::filter(!base::is.na(.[[1]])) %>%
        dplyr::mutate("pct" = (100 * n / base::sum(n)), "pct_peso" = (100 * n_peso / base::sum(n_peso))) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(var), base::as.character)) %>%
        dplyr::full_join(labels, by = var) %>%
        dplyr::mutate(dplyr::across(c("n", "n_peso", "pct", "pct_peso"), ~ base::ifelse(base::is.na(.x), 0, .x))) %>%
        dplyr::arrange(base::match(.[[1]], stringr::str_sort(.[[1]], numeric = TRUE))) %>%
        dplyr::arrange(.[[1]] %in% c("-66","-666")) %>%
        dplyr::arrange(.[[1]] %in% c("-77","-777")) %>%
        dplyr::arrange(.[[1]] %in% c("-88","-888")) %>%
        dplyr::arrange(.[[1]] %in% c("-99","-999")) %>%
        dplyr::mutate(dplyr::across(c("n", "n_peso", "pct", "pct_peso"), base::as.double)) %>%
        purrr::map_df(.f = ~ c(
          ., base::ifelse(base::is.numeric(.), base::sum(., na.rm = TRUE), "Total"))
        ) %>%
        base::rbind(
          base::list("Base", base[[i]]$n_base, base[[i]]$n_base_peso, base[[i]]$pct_base, base[[i]]$pct_base_peso, "Base")
        )

      {# Start: Identificando e colocando os warnings no 'Log_isoladas'

        if( base::any(base::is.na(out[[i]]%>%dplyr::pull(6))) )
        {# Start: Motivo para algum warning

          if ( var %nin% c(DICIONARIO %>% dplyr::distinct(opcao_variavel) %>% dplyr::pull()) )
          {# Start: warning1 ("Variável var não presente no dicionário")

            msg=stringr::str_c("Vari\u00E1vel ", var, " n\u00E3o presente no dicion\u00E1rio")
            all_warning=c(all_warning,"\n",msg)


            if( base::length(Log_isoladas)==0 )
            {# Start: Colocando msg do warning1 no 'Log_isoladas'

              Log_isoladas[[1]]<-tibble::tibble(
                Variavel=var,
                `Problema`=msg,
                Status="rodou"
              )

            }else{

              Log_isoladas[[1]]<-dplyr::bind_rows(
                Log_isoladas[[1]],
                tibble::tibble(
                  Variavel=var,
                  `Problema`=msg,
                  Status="rodou"
                )
              )

            }# End: Colocando msg do warning1 no 'Log_isoladas'

            base::rm(msg)

          }# End: warning1 ("Variável var não presente no dicionário")
          else
          {# Start: warning2 (Variável var possui label não presente no dicionário. Código(s) []")

            all_cods_falt=base::as.character(out[[i]][which(base::is.na(out[[i]][,6])),1]%>%dplyr::pull())

            if( base::length(all_cods_falt)>5 )
            {# Start: Limitando o número de códigos a serem inseridos no warning

              all_cods_falt=all_cods_falt[1:5]
              all_cods_falt[6]="..."

            }# End: Limitando o número de códigos a serem inseridos no warning

            msg=stringr::str_c("Vari\u00E1vel ", var, " possui label n\u00E3o presente no dicion\u00E1rio. C\u00F3digo(s) [",base::paste(all_cods_falt,collapse = ","),"]")
            all_warning=c(all_warning,"\n",msg)

            if( base::length(Log_isoladas)==0 )
            {# Start: Colocando msg do warning2 no 'Log_isoladas'

              Log_isoladas[[1]]<-tibble::tibble(
                Variavel=var,
                `Problema`=msg,
                Status="rodou"
              )

            }else{

              Log_isoladas[[1]]<-dplyr::bind_rows(
                Log_isoladas[[1]],
                tibble::tibble(
                  Variavel=var,
                  `Problema`=msg,
                  Status="rodou")
              )

            }# End: Colocando msg do warning2 no 'Log_isoladas'

            base::rm(all_cods_falt,msg)%>%base::suppressWarnings()

          }# End: warning2 (Variável var possui label não presente no dicionário. Código(s) []")

        }# End: Motivo para algum warning

        }# End: Identificando e colocando os warnings no 'Log_isoladas'


      if( adc_labels==FALSE )
      {# Start: se não quiser adicionar o Label na saída

        #Remove a coluna com o Label
        out[[i]]<-out[[i]]%>%
          dplyr::select(-base::ncol(.))

      }# End: se não quiser adicionar o Label na saída

    }# End: Executando para cada variável

  }# End: se não houver erro, executar

  if( base::length(all_warning)>0 )
  {# Start: tiver algum warning, printar

    base::warning(all_warning, call. = FALSE)

  }# End: tiver algum warning, printar

  # Retorna os resultados e os registros de variáveis isoladas (Log_isoladas)
  base::return(list(Resultado_isoladas=out,Log_isoladas=Log_isoladas))

}#End: FUN_isoladas
