#' @title FUN_Citou
#' @name FUN_Citou
#'
#' @description Pivota e depois calcula a frequência simples (ponderada e não ponderada)
#'  das variáveis indicadas em 'lista_variaveis' (MRG).
#'  Código 1 = Citou; código diferente de 1 = não citou.
#'
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param lista_variaveis Variáveis que serão pivotadas.
#' @param adc_labels Se adc_labels=TRUE, adicionar uma coluna com o label.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom stringr str_c
#' @importFrom purrr map_df
#'
#' @examples
#'
#'#Passando 'outros' (v10) para do tipo citou/não citou
#'library(IPpackage)
#'library(dplyr)
#'library(tidyr)
#'TABELA = IPpackage::IPpackage_exemplo %>%
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
#'# MRG e Isolada
#'IPpackage::FUN_Citou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  lista_variaveis = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
#'    "v10_1_o" = "v10_1_o"
#'    ),
#'  adc_labels = TRUE
#')
#'
#'# Warning1: Código a mais encontrado em TABELA (diferente de 1, 2)
#'IPpackage::FUN_Citou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  lista_variaveis = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_1", "v11"),
#'    "v10_1" = "v10_1"
#'    ),
#'  adc_labels = TRUE
#')
#'
#'# Warning2: Enunciado de alguma variável faltando
#'IPpackage::FUN_Citou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO%>%
#'    dplyr::mutate(
#'      pergunta_enunciado = base::ifelse(
#'        opcao_variavel %in% c("v5","v11"), NA, pergunta_enunciado
#'        )
#'    ),
#'  lista_variaveis = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11"),
#'    "v5" = "v5",
#'    "v11" = "v11"
#'  ),
#'  adc_labels = TRUE
#')
#'
#'# Todos os warnings
#'IPpackage::FUN_Citou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO%>%
#'    dplyr::mutate(
#'      pergunta_enunciado = base::ifelse(
#'        opcao_variavel %in% c("v5","v11"), NA, pergunta_enunciado
#'      )
#'    ),
#'  lista_variaveis = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_1", "v11"),
#'    "v10_1" = "v10_1",
#'    "v5" = "v5",
#'    "v11" = "v11"
#'  ),
#'  adc_labels = TRUE
#')
#'
#' @export
#'
#'

FUN_Citou <- function(
  TABELA,
  DICIONARIO,
  lista_variaveis,
  adc_labels = TRUE
)
{# Start: FUN_Citou

  {# Start: armazenar resultados e definições iniciais

    Tempo_Inicio <- base::Sys.time()
    `%nin%` = base::Negate(`%in%`)
    out <- base::vector("list", length = base::length(lista_variaveis))
    base <- base::vector("list", length = base::length(lista_variaveis))
    vars_nomes <- base::names(lista_variaveis)
    Log_citou <- base::list()
    all_warning <- c()

  }# End: armazenar resultados e definições iniciais

  # calcular a tabela de frequência
  for ( i in base::seq_along(out) )
    {# Start: Executando para cada variável

    erro = 0

    if( erro == 0 )
      {# Start: se não houver erro, executar

      # Seleciona os rótulos do MRG do dicionário
      labels <- DICIONARIO %>%
        dplyr::filter(opcao_variavel %in% lista_variaveis[[i]]) %>%
        dplyr::filter(opcao_cod == 1) %>%
        dplyr::select(opcao_variavel, pergunta_enunciado) %>%
        dplyr::rename(!!vars_nomes[i] := "opcao_variavel") %>%
        dplyr::rename(!!stringr::str_c(vars_nomes[i], "_label") := "pergunta_enunciado")

      # Calcula a base de dados para o MRG atual
      base[[i]] <- TABELA %>%
        dplyr::select(dplyr::all_of(lista_variaveis[[i]]), peso) %>%
        dplyr::mutate("n_base" = base::ifelse(
          test = base::rowSums(!base::is.na(dplyr::across(dplyr::all_of(lista_variaveis[[i]])))) > 0, 1, 0
          )
        ) %>%
        dplyr::mutate("n_base" = base::sum(`n_base`)) %>%
        dplyr::mutate("pct_base" = (100 * `n_base`) / base::nrow(.)) %>%
        dplyr::mutate("n_base_peso" = base::ifelse(
          test = base::rowSums(!base::is.na(dplyr::across(dplyr::all_of(lista_variaveis[[i]])))) > 0, peso, 0
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
        dplyr::filter(Respostas == 1) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of("Variavel"), ~ base::factor(., levels = labels[[1]]))) %>%
        dplyr::group_by(dplyr::across(dplyr::all_of("Variavel")), .drop = F) %>%
        dplyr::summarise("n" = n(), "n_peso" = base::sum(peso), .groups = "drop") %>%
        dplyr::mutate("pct" = (100 * n / base[[i]]$n_base)) %>%
        dplyr::mutate("pct_peso" = (100 * n_peso / base[[i]]$n_base_peso)) %>%
        dplyr::rename(!!vars_nomes[i] := "Variavel") %>%
        dplyr::mutate(dplyr::across(vars_nomes[i], as.character )) %>%
        purrr::map_df(.f = ~ c(., base::ifelse(base::is.numeric(.), base::sum(., na.rm = TRUE), "Total"))) %>%
        base::rbind(base::list("Base", base[[i]]$n_base, base[[i]]$n_base_peso, base[[i]]$pct_base, base[[i]]$pct_base_peso)) %>%
        dplyr::left_join(x = ., y = labels , by = vars_nomes[i]) %>%
        dplyr::mutate(dplyr::across(6, ~ replace(., .data[[vars_nomes[i]]] == "Total", "Total"))) %>%
        dplyr::mutate(dplyr::across(6, ~ replace(., .data[[vars_nomes[i]]] == "Base", "Base"))) %>%
        dplyr::mutate(dplyr::across(c("n","n_peso","pct","pct_peso"), ~ base::ifelse(base::is.na(.x), 0, .x)))

      {# Start: Identificando e colocando os warnings no 'Log_citou'

        {# Start: warning1 ("Váriavel vars_nomes: código a mais encontrado em TABELA.")

          #Verificando valor a mais em TABELA (que não seja 1 nem 2)
          values_TABELA <- TABELA %>%
            dplyr::select(dplyr::all_of(lista_variaveis[[i]])) %>%
            tidyr::pivot_longer(cols = dplyr::all_of(lista_variaveis[[i]])) %>%
            dplyr::mutate(value = base::as.factor(value)) %>%
            dplyr::select(value) %>%
            dplyr::reframe(levels(value)) %>%
            dplyr::pull()

          if( base::any(values_TABELA %nin% c("1","2")) )
          {# Start: Se warning1

            all_cods_falt=values_TABELA[which(values_TABELA %nin% c("1", "2"))]

            if( base::length(all_cods_falt)>5 )
            {# Start: Limitando o número de códigos a serem inseridos no warning

              all_cods_falt = all_cods_falt[1:5]
              all_cods_falt[6] = "..."

            }# End: Limitando o número de códigos a serem inseridos no warning


            msg = stringr::str_c(
              "Vari\u00E1vel ",
              vars_nomes[i],
              ": c\u00F3digo a mais encontrado em TABELA. C\u00F3digo [",
              base::paste(all_cods_falt, collapse = ","),
              "] foi tratado como n\u00E3o/n\u00E3o citou"
            )

            all_warning = c(all_warning,"\n",msg)

            if( base::length(Log_citou) == 0 )
            {# Start: Colocando msg do warning1 no 'Log_citou'

              Log_citou[[1]] <- tibble::tibble(
                Variavel=vars_nomes[i],
                `Problema`= msg,
                Status = "rodou")

            }else{

              Log_citou[[1]] <- dplyr::bind_rows(
                Log_citou[[1]],
                tibble::tibble(
                  Variavel = vars_nomes[i],
                  `Problema`= msg,
                  Status="rodou")
              )

            }# End: Colocando msg do warning1 no 'Log_citou'

          }# End: Se warning1

        }# End: warning1 ("Váriavel vars_nomes: código a mais encontrado em TABELA.")

        {# Start: warning2 ("Variável vars_nomes[i] possui label não presente no dicionário")

          #Verificar se todas as variáveis possuem enunciado
          if( base::any(base::is.na(out[[i]] %>% dplyr::pull(6))) )
          {# Start: Se warning2

            msg = stringr::str_c(
              "Vari\u00E1vel ",
              vars_nomes[i],
              " possui label n\u00E3o presente no dicion\u00E1rio. Enunciado(s) [",
              base::paste(out[[i]][which(base::is.na(out[[i]][[stringr::str_c(vars_nomes[i], "_label")]])),1] %>% pull(),collapse = ",")
              ,"] ausente(s)."
            )

            all_warning = c(all_warning, "\n", msg)

            if( base::length(Log_citou) == 0 )
            {# Start: Colocando msg do warning2 no 'Log_citou'

              Log_citou[[1]] <- tibble::tibble(
                Variavel = vars_nomes[i],
                `Problema`= msg,
                Status = "rodou"
              )

            }else{

              Log_citou[[1]] <- dplyr::bind_rows(
                Log_citou[[1]],
                tibble::tibble(
                  Variavel = vars_nomes[i],
                  `Problema`= msg,
                  Status = "rodou"
                )
              )

            }# End: Colocando msg do warning2 no 'Log_citou'

          }# End: Se warning2

        }# End: warning2 ("Variável vars_nomes[i] possui label não presente no dicionário")

      }# End: Identificando e colocando os warnings no 'Log_citou'

      if(adc_labels == FALSE)
      {# Start: Removendo label se adc_labels=FALSE

        out[[i]] <- out[[i]] %>%
          dplyr::select(-base::ncol(.))

      }# End: Removendo label se adc_labels=FALSE

    }# End: se não houver erro, executar

  }# End: Executando para cada variável


  if( base::length(all_warning) > 0 )
  {# Start: tiver algum warning, printar

    base::warning(all_warning, call. = FALSE)

  }# End: tiver algum warning, printar

  # Retorna os resultados e os registros de mrg citou (Log_citou)
  return(base::list(Resultado_Citou = out, Log_citou = Log_citou))

}# End: FUN_Citou
