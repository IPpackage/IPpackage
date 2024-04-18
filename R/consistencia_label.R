#' @title consistencia_label
#' @name consistencia_label
#'
#' @description A função foi desenvolvida para avaliar se todas as variáveis possuem
#'  Label e Enunciado no Dicionário e se existe algum código repetido.
#'
#' @param log_consistenia_label Se a variável em questão for a primeira a ser testada,
#' 'log_consistenia_label' é definida como 'NULL'; no entanto, se um arquivo no formato de
#' saída dessa função já existir, 'log_consistenia_label' será o seu data.frame correspondente
#' @param xdf Banco de dados
#' @param xdicionario Dicionário com as colunas 'opcao_cod', 'opcao_label',
#' 'opcao_variavel', 'pergunta_enunciado'
#' @param vars Variável ou, no caso de um MRG, vetor de variáveis
#' @param var_mrg_nome 'NA' caso não se trate de um MRG; caso contrário, nome do MRG
#' @param mrg_citou 'TRUE' para indicar se tratar de MRG do tipo Citou;
#' caso contrário, 'FALSE'.
#' @param pode_falta 'TRUE' para indicar a possibilidade de labels ausentes;
#' 'FALSE' para indicar a ausência de tolerância para labels não fornecidas.
#'
#' @param show 'TRUE' para imprimir a frequência com Label e Enunciado;
#' 'FALSE', caso contrário.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#'
#' #Sem exemplo
#' @export
#'
#'
consistencia_label <- function(
    log_consistenia_label = NULL,
    xdf,
    xdicionario,
    vars,
    var_mrg_nome = NA,
    mrg_citou = FALSE,
    pode_falta = FALSE,
    show = FALSE
)
{# Start: consistencia_label

  if ( base::is.null(log_consistenia_label) )
  {# Start: se ainda não existe log_consistenia_label, criar

    log_consistenia_label = base::data.frame(
      var = NA,
      resultado = NA,
      descricao = NA
    )

  }# End: se ainda não existe log_consistenia_label, criar

  if ( base::length(vars) > 1 ) {# Start: definindo nome_usar

    if ( base::is.na(var_mrg_nome) ) {# Start: definindo nome_usar

      nome_usar = base::paste0("MRG-", vars[1])
      message(
        base::paste0(
          "Como nome = NULL, atribuimos o nome '",
          nome, "'")
      )

    } else {

      nome_usar = var_mrg_nome

    }# Start: definindo nome_usar

  }else{

    nome_usar = vars

  }# End: definindo nome_usar

  rodar = base::lapply(vars, base::list)
  rodar = stats::setNames(rodar, vars)

  if( base::length(vars) > 1 )
  {# Start: armazenando todas as variáveis que serão rodadas

    rodar[[base::length(rodar)+1]] <- base::list(vars)
    names(rodar)[base::length(rodar)] = nome_usar
    rodar <- c(utils::tail(rodar, 1), utils::head(rodar, -1))

  }# End: armazenando todas as variáveis que serão rodadas

  for( i in 1:base::length(rodar) )
  {# Start: rodando para cada variável

    nome = base::names(rodar)[i]
    variaveis = rodar[[i]][[1]]

    {# Start: Verificando se tem label faltando

      if( mrg_citou == TRUE )
      {# Start: rodando 'mrg_citou'

        fim = dplyr::left_join(
          xdf %>%
            dplyr::select(dplyr::all_of(variaveis)) %>%
            tidyr::pivot_longer(dplyr::everything()) %>%
            dplyr::mutate(
              name = base::as.character(name),
              value = base::as.character(value)
            ),
          xdicionario %>%
            dplyr::filter(opcao_variavel %in% dplyr::all_of(variaveis)) %>%
            dplyr::mutate(
              opcao_variavel = base::as.character(opcao_variavel),
              opcao_cod = base::as.character(opcao_cod)
            ),
          by = c(
            "name" = "opcao_variavel",
            "value" = "opcao_cod"
          )
        ) %>%
          dplyr::group_by(name) %>%
          dplyr::count(value, opcao_label, pergunta_enunciado) %>%
          dplyr::mutate(mrg_nome = nome) %>%
          dplyr::left_join(
            xdicionario %>%
              dplyr::filter(opcao_variavel %in% c(names(rodar)[i])) %>%
              dplyr::mutate(
                opcao_variavel = base::as.character(opcao_variavel),
                opcao_cod = base::as.character(opcao_cod)
              ) %>%
              dplyr::select(opcao_variavel, pergunta_enunciado) %>%
              dplyr::rename(pergunta_enunciado_mrg = pergunta_enunciado)
            ,
            by = c('mrg_nome' = 'opcao_variavel')
          ) %>%
          base::suppressWarnings()

        # Filtrando label faltando
        x = fim %>%
          dplyr::filter(
            (
              base::is.na(opcao_label) |
                base::is.na(pergunta_enunciado) |
                base::is.na(pergunta_enunciado_mrg)
            ) &
              (
                !base::is.na(value)
              )
          ) %>%
          base::suppressWarnings()

      }# End: rodando 'mrg_citou'

      if( mrg_citou == FALSE )
      {# Start: rodando se não for 'mrg_citou'

        procurar = nome

        fim = xdf %>%
          dplyr::select(dplyr::all_of(variaveis)) %>%
          tidyr::pivot_longer(dplyr::everything()) %>%
          dplyr::count(value) %>%
          dplyr::mutate(name = procurar) %>%
          dplyr::relocate(name, .before = value) %>%
          dplyr::mutate(
            name = base::as.character(name),
            value = base::as.character(value)
          ) %>%
          {# Start: colocando o label

            freq = tibble::tibble(.)

            dic_filtro = xdicionario %>%
              dplyr::filter(opcao_variavel %in% c(procurar)) %>%
              dplyr::mutate(
                opcao_variavel = base::as.character(opcao_variavel),
                opcao_cod = base::as.character(opcao_cod)
              )

            so_na_dicionario = base::any(
              base::is.na(
                dic_filtro %>%
                  dplyr::select(opcao_label)
              )
            ) &
              dic_filtro %>%
              dplyr::select(pergunta_enunciado, opcao_variavel) %>%
              dplyr::select(pergunta_enunciado) %>%
              base::unique() %>%
              base::nrow() == 1

            if( so_na_dicionario == TRUE )
            {# Start: se só tenho NA no dicionário

              freq = freq %>%
                dplyr::left_join(
                  dic_filtro %>% dplyr::select(opcao_cod, opcao_label, opcao_variavel),
                  by = c("name" = "opcao_variavel", "value" = "opcao_cod")
                ) %>%
                dplyr::left_join(
                  dic_filtro %>% dplyr::select(pergunta_enunciado, opcao_variavel) %>% unique(),
                  by = c("name" = "opcao_variavel")
                ) %>% base::suppressWarnings()

            }# End: se só tenho NA no dicionário
            else
            {# Start: juntando frquência com dicionário

              freq = freq %>%
                dplyr::left_join(
                  dic_filtro,
                  by = c(
                    "name" = "opcao_variavel",
                    "value" = "opcao_cod"
                  )
                ) %>%
                base::suppressWarnings()

            }# End: juntando frquência com dicionário

            freq

          }# End: colocando o label

        # Filtrando label faltando
        x = fim %>%
          dplyr::filter(
            (
              base::is.na(opcao_label) | base::is.na(pergunta_enunciado)
            )
            &
              (
                !base::is.na(value)
              )
          ) %>%
          base::suppressWarnings()

      }# End: rodando se não for 'mrg_citou'

      if( base::nrow(x) > 0 )
      {# Start: resultado da verificação 'tem label faltando ou não'

        if( pode_falta == FALSE )
        {# Start: salvando o resultado caso possa faltar e caso não possa

          mensagem = base::cat(
            base::paste0(
              "\033[1;31m[", nome, "] var x label ERRO (falta label)\033[0m\n"
            )
          )
          resultado = "error"
          descricao = "Falta Label"
          cor = "vermelho"

        }else{

          mensagem = base::cat(
            base::paste0(
              "\033[1;33m[", nome, "] var x label OK (faltou mas pode faltar)\033[0m\n"
            )
          )
          resultado = "ok"
          descricao = "Faltou Label [pode faltar = TRUE]"
          cor = "amarelo"

        }# End: salvando o resultado caso possa faltar e caso não possa

      }else{

        mensagem = base::cat(
          base::paste0(
            "\033[1;32m[", nome, "] var x label OK\033[0m\n"
          )
        )
        resultado = "ok"
        descricao = "-"
        cor = "verde"

      }# End: resultado da verificação 'tem label faltando ou não'

    }# End: Verificando se tem label faltando

    {# Start: verificando se tem código duplicado

      procurar = nome

      dic_filtrado = xdicionario %>%
        dplyr::filter(opcao_variavel %in% c(procurar)) %>%
        dplyr::mutate(
          opcao_variavel = base::as.character(opcao_variavel),
          opcao_cod = base::as.character(opcao_cod)
        )

      tenho_duplicata = dic_filtrado %>%
        dplyr::filter(base::duplicated(opcao_cod))

      cor_duplicado = "verde"

      if( base::nrow(tenho_duplicata) > 0 )
      {# Start: se tenho código duplicado
        codigos_duplicados = tenho_duplicata %>%
          dplyr::select(opcao_cod) %>%
          dplyr::pull()

        mensagem = base::cat(
          base::paste0(
            "\033[1;31m[", nome, "] Label ERRO (opcao_cod duplicados - ",
            base::paste0(codigos_duplicados, collapse = ", "),
            ")\033[0m\n"
          )
        )

        resultado = "error"

        if( descricao == "-" )
        {# Start: formatando 'descricao'

          descricao = base::paste0(
            "opcao_cod duplicados - ",
            base::paste0(codigos_duplicados, collapse = ", ")
          )

        }else{

          descricao =
            base::paste0(
              descricao,
              base::paste0(
                "; opcao_cod duplicados - ",
                base::paste0(codigos_duplicados, collapse = ", ")
              )
            )

        }# End: formatando 'descricao'

        cor_duplicado = "vermelho"

      }# End: se tenho código duplicado

    }# End: verificando se tem código duplicado

    # Salvando log
    log_consistenia_label <-
      base::rbind(
        log_consistenia_label,
        base::data.frame(
          var = nome,
          resultado = resultado,
          descricao = descricao
        )
      )

    log_consistenia_label = log_consistenia_label %>%
      tidyr::drop_na()

    if( show == TRUE )
    {# Start: printando o resultado

      if( resultado != "error" )
      {# Start: se não teve erro

        base::cat("\033[3m labels\033[0m\n")
        IPpackage::print_cor(
          fim %>%
            base::unique(),
          cor
        )

      }# End: se não teve erro

      if( resultado == "error" )
      {# Start: se teve erro

        base::cat("\033[3m erros\033[0m\n")

        if( nrow(x) > 0 )
        {# Start: se tenho erro de label faltante

          IPpackage::print_cor(
            x %>%
              base::unique(),
            cor
          )

        }# End: se tenho erro de label faltante

        if( base::nrow(tenho_duplicata) > 0 )
        {# Start: se tenho erro de código duplicado

          IPpackage::print_cor(
            dic_filtrado %>%
              dplyr::filter(opcao_cod %in% c(codigos_duplicados)),
            cor_duplicado
          )

        }# End: se tenho erro de código duplicado

      }# End: se teve erro

    }# End: printando o resultado

  }# End: rodando para cada variável

  # Retornando o resultado
  base::return(log_consistenia_label)

}# End: consistencia_label
