#' @title FUN_MRG
#' @name FUN_MRG
#'
#' @description Pivota e depois calcula a frequência simples (ponderada e não ponderada) das variáveis indicas em 'lista_variaveis' (MRG).
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param lista_variaveis Variáveis que serão pivotadas (vetor).
#' @param adc_labels Se adc_labels=TRUE, adicionar uma coluna com o label.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @importFrom purrr map_df
#'
#' @examples
#'
#' #Uma variável
#' FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=c("v1"),
#'   adc_labels = TRUE
#' )
#'
#' #Mais de uma variável
#' FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=c("v1","v2"),
#'   adc_labels = TRUE
#' )
#'
#' #Todas as variáveis
#' FUN_isoladas(
#'   TABELA=IPpackage::IPpackage_exemplo,
#'   DICIONARIO=IPpackage::IPpackage_dicionario,
#'   variaveis=colnames(IPpackage::IPpackage_exemplo),
#'   adc_labels = TRUE
#' )
#'
#' @export
#'
#'


FUN_MRG <- function(
    TABELA,
    DICIONARIO,
    lista_variaveis,
    adc_labels = TRUE)
  {# Start: FUN_MRG
  Tempo_Inicio<-base::Sys.time()
  `%nin%` = base::Negate(`%in%`)

  out <- base::vector("list", length = base::length(lista_variaveis))
  base <- base::vector("list", length = base::length(lista_variaveis))
  vars_nomes <- base::names(lista_variaveis)

  Log_MRG<-base::list()

  # calcular a tabela de frequência
  for (i in base::seq_along(out)) {
    #Error
    erro=0

    #Run
    if(erro==0){
      labels <- DICIONARIO %>%
        dplyr::filter(opcao_variavel == vars_nomes[i]) %>%
        # distinct() %>%
        dplyr::select(opcao_cod, opcao_label) %>%
        dplyr::rename(!!vars_nomes[i] := "opcao_cod") %>%
        dplyr::rename(!!str_c(vars_nomes[i], "_label") := "opcao_label") %>%
        dplyr::mutate(dplyr::across(vars_nomes[i], as.character)) %>%
        dplyr::arrange(match(.[[1]], str_sort(.[[1]], numeric = T))) %>%
        dplyr::arrange(.[[1]] %in% "-88") %>%
        dplyr::arrange(.[[1]] %in% "-99")

      base[[i]] <- TABELA %>%
        dplyr::select(all_of(lista_variaveis[[i]]), peso) %>%
        dplyr::mutate("n_base" = ifelse(test = rowSums(!is.na(dplyr::across(all_of(lista_variaveis[[i]])))) > 0, 1, 0)) %>%
        dplyr::mutate("n_base" = sum(`n_base`)) %>%
        dplyr::mutate("pct_base" = (100 * `n_base`) / nrow(.)) %>%
        dplyr::mutate("n_base_peso" = ifelse(test = rowSums(!is.na(dplyr::across(all_of(lista_variaveis[[i]])))) > 0, peso, 0)) %>%
        dplyr::mutate("n_base_peso" = sum(`n_base_peso`)) %>%
        dplyr::mutate("pct_base_peso" = (100 * `n_base_peso`) / sum(peso)) %>%
        dplyr::distinct(n_base, pct_base, n_base_peso, pct_base_peso)

      out[[i]] <- TABELA %>%
        dplyr::select(all_of(lista_variaveis[[i]]), peso) %>%
        tidyr::pivot_longer(
          cols = all_of(lista_variaveis[[i]]),
          names_to = "Variavel",
          values_to = "Respostas"
        ) %>%
        # dplyr::mutate(dplyr::across(all_of("Respostas"), ~ factor(., levels = labels[[1]]))) %>%
        dplyr::group_by(dplyr::across(all_of("Respostas")), .drop = FALSE) %>%
        dplyr::summarise("n" = n(), "n_peso" = sum(peso), .groups = "drop") %>%
        dplyr::filter(!is.na(.[[1]])) %>%
        dplyr::mutate("pct" = (100 * n / sum(n))) %>%
        dplyr::mutate("pct_peso" = (100 * n_peso / sum(n_peso))) %>%
        # dplyr::rename_with(
        #   .cols = 1,
        #   .fn = ~ str_c(lista_variaveis[[i]][[1]], "mrg") %>% str_replace("_\\d+", "")
        # ) %>%
        dplyr::rename(!!vars_nomes[i] := "Respostas") %>%
        dplyr::mutate(dplyr::across(vars_nomes[i], as.character)) %>%
        full_join(labels,by=vars_nomes[i]) %>%
        dplyr::mutate(across(c("n", "n_peso", "pct", "pct_peso"), ~ ifelse(is.na(.x), 0, .x))) %>%
        dplyr::arrange(match(.[[1]], str_sort(.[[1]], numeric = T))) %>%
        dplyr::arrange(.[[1]] %in% "-88") %>%
        dplyr::arrange(.[[1]] %in% "-99") %>%
        dplyr::mutate(across(c("n", "n_peso", "pct", "pct_peso"), as.double)) %>%
        purrr::map_df(.f = ~ c(., ifelse(is.numeric(.), sum(., na.rm = TRUE), "Total"))) %>%
        rbind(base::list("Base", base[[i]]$n_base, base[[i]]$n_base_peso, base[[i]]$pct_base, base[[i]]$pct_base_peso, "Base"))

      # print(paste0(base::names(lista_variaveis)[i]," [Variavel MRG ",i,"/",base::length(lista_variaveis),"]"))

      if(any(is.na(out[[i]]%>%dplyr::pull(6)))){

        if (vars_nomes[i] %nin% c(DICIONARIO %>% dplyr::distinct(opcao_variavel) %>% dplyr::pull()) ) {
          warning(str_c("Variavel ", vars_nomes[i], " nao presente no dicionario"))
          if(base::length(Log_MRG)==0){Log_MRG[[1]]<-tibble::tibble(Variavel=vars_nomes[i],`Problema`=str_c("Variavel ", vars_nomes[i], " nao presente no dicionario"),Status="rodou")}else{Log_MRG[[1]]<-dplyr::bind_rows(Log_MRG[[1]],tibble::tibble(Variavel=vars_nomes[i],`Problema`=str_c("Variavel ", vars_nomes[i], " nao presente no dicionario"),Status="rodou"))  }
        }else{
          msg=str_c("Variavel ", vars_nomes[i], " possui label nao presente no dicionario. Codigo(s) [",paste(as.character(out[[i]][which(is.na(out[[i]][,6])),1]%>%dplyr::pull()),collapse = ","),"]")
          warning(msg)
          if(base::length(Log_MRG)==0){Log_MRG[[1]]<-tibble::tibble(Variavel=vars_nomes[i],`Problema`=msg,Status="rodou")}else{Log_MRG[[1]]<-dplyr::bind_rows(Log_MRG[[1]],tibble::tibble(Variavel=vars_nomes[i],`Problema`=msg,Status="rodou"))  }
        }

      }

      if(adc_labels==FALSE){out[[i]]<-out[[i]]%>%dplyr::select(-ncol(.))}

    }

  }

  # print(paste0("A funcao levou ",Time_Difference(Sys.time(),Tempo_Inicio)," para calcular as frequencias (variaveis MRG's)"))
  # cat("\n")

  #Se tenho nenhum warning, criar o tibble composto só por NA
  #if(base::length(Log_MRG)==0){Log_MRG[[1]]<-tibble::tibble(Variavel=NA,`Problema`=NA,Status=NA)}


  return(base::list(Resultado_MRG=out,Log_MRG=Log_MRG))
}# End: FUN_MRG
