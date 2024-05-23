#' @title FUN_cruzaCitou
#' @name FUN_cruzaCitou
#'
#' @description Pivota e depois calcula a frequência simples (ponderada e não ponderada)
#'  das variáveis indicadas em 'varCitou' (Código 1 = Citou; código diferente de 1 = não citou)
#'  cruzada pela variável indicada em 'ind'.
#'  O parâmetro 'var_fecha' define a variável na coluna.
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param var1 Variável que será cruzada pela 'varCitou'.
#' @param varCitou Variáveis do tipo citou/não citou.
#' @param var_fecha Variável na coluna.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#' @importFrom tibble tibble
#' @importFrom stringr str_c
#' @importFrom purrr map
#'
#' @examples
#'
#'library(IPpackage)
#'library(dplyr)
#'library(tidyr)
#'library(tibble)
#'library(stringr)
#'library(purrr)
#'#Passando 'outros' (v10) para do tipo citou/não citou
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
#'      opcao_cod = base::rep(c(1, 2),3),
#'      opcao_label = base::rep(c("Citou", "Não citou"),3),
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
#'
#'#Rodando a função-------------------------------------------------------------
#'
#'#1. Categoria MRG (v15mrg - v15, v16) vs Citou MRG (v4mrg - v4 a v11):----------
#'
#'##Fechando na v15mrg (Divide pela base - quem falou pelo menos um cód em v15mrg)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = base::list(
#'    "v15mrg" = c("v15", "v16")
#'  ),
#'  varCitou = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11")
#'  ),
#'  var_fecha = "v15mrg"
#') %>%
#'  base::data.frame()
#'
#'##Fechando na v4mrg (Divide pela soma de n - soma 100%)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = base::list(
#'    "v15mrg" = c("v15", "v16")
#'  ),
#'  varCitou = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11")
#'  ),
#'  var_fecha = "v4mrg"
#') %>%
#'  base::data.frame()
#'
#'#2.Categoria Isolada (v15) vs Citou MRG (v4mrg - v4 a v11):---------------------
#'
#'##Fechando na v15 (Divide pela base - quem falou cód em v15)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = "v15",
#'  varCitou = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11")
#'  ),
#'  var_fecha = "v15"
#') %>%
#'  base::data.frame()
#'
#'##Fechando na v4mrg (Divide pela soma de n - soma 100%)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = "v15",
#'  varCitou = base::list(
#'    "v4mrg" = c("v4", "v5", "v6", "v7", "v8", "v9", "v10_outros", "v11")
#'  ),
#'  var_fecha = "v4mrg"
#') %>%
#'  base::data.frame()
#'
#'#3.Categoria MRG (v15mrg - v15, v16) vs Citou Isolada v4:-----------------------
#'
#'##Fechando na v15mrg (Divide pela base - quem falou pelo menos um cód em v15mrg)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = base::list("v15mrg" = c("v15", "v16")),
#'  varCitou = base::list("v4" = "v4"),
#'  var_fecha = "v15mrg"
#') %>%
#'  base::data.frame()
#'
#'##Fechando na v4 (Divide pela soma de n - soma 100%)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = base::list("v15mrg" = c("v15", "v16")),
#'  varCitou = base::list("v4" = "v4"),
#'  var_fecha = "v4"
#') %>%
#'  base::data.frame()
#'
#'#4.Categoria Isolada v15 vs Citou Isolada v4:-----------------------------------
#'
#'##Fechando na v15 (Divide pela base - quem falou cód em v15)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = "v15",
#'  varCitou = base::list("v4" = "v4"),
#'  var_fecha = "v15"
#') %>%
#'  base::data.frame()
#'
#'## Fechando na v4 (Divide pela soma de n - soma 100%)
#'FUN_cruzaCitou(
#'  TABELA = TABELA,
#'  DICIONARIO = DICIONARIO,
#'  var1 = "v15",
#'  varCitou = base::list("v4" = "v4"),
#'  var_fecha = "v4"
#') %>%
#'  base::data.frame()
#'
#' @export
#'


FUN_cruzaCitou <-function(
    TABELA,
    DICIONARIO,
    var1,
    varCitou,
    var_fecha
)
{# Start: FUN_cruzaCitou

  {# Start: armazenar resultados e definições iniciais

    `%nin%` = base::Negate(`%in%`)

    nome_var1 = var1
    if( base::is.list(nome_var1) )
    {
      nome_var1 = base::names(nome_var1)
    }

    nome_varCitou = varCitou
    if( base::is.list(nome_varCitou) )
    {
      nome_varCitou = base::names(varCitou)
    }


  }# End: armazenar resultados e definições iniciais



  if( var_fecha == nome_var1 )
  {# Start: calcular a frequência fechando em 'var1'

    if( !base::all(base::is.na(TABELA %>%
                               dplyr::select(dplyr::all_of(var1[[1]])) %>%
                               dplyr::pull() %>% base::unique() ) )
    )
    {# Start: só calcula se tem valores para tal

      if ( base::length(var1[[1]]) > 1 )
      {# Start: se 'var1' for MRG, calcular a base


        all_cods = DICIONARIO %>%
          dplyr::filter(opcao_variavel == nome_var1) %>%
          dplyr::pull(opcao_cod)

        tabela_base = TABELA %>%
          dplyr::select(dplyr::all_of(c(var1[[1]], varCitou[[1]])), peso) %>%
          dplyr::filter(rowSums(is.na(.)) != base::ncol(.) -1 )

        a = tibble::tibble()

        for( tb in 1:base::length(all_cods) )
        {# Start: calculando a base para cada código de 'var1'

          b = tabela_base %>%
            dplyr::rowwise() %>%
            dplyr::filter(
              base::any(
                dplyr::c_across(dplyr::all_of(var1[[1]])) == all_cods[tb])
            ) %>%
            dplyr::filter(
              base::any(
                !base::is.na(dplyr::c_across(dplyr::all_of(varCitou[[1]]))))
            ) %>%
            dplyr::ungroup() %>%
            dplyr::summarise(
              variavel = nome_var1,
              codigo = all_cods[tb],
              n_base = base::nrow(.),
              n_base_peso = base::sum(peso)
            )

          if( base::nrow(a) == 0 )
          {# Start: armazendo o resultado

            a = b

          }else{

            a = base::rbind(a, b)

          }# End: armazendo o resultado

        }# End: calculando a base para cada código de 'var1'

        tabela_base = a %>%
          dplyr::mutate(codigo = base::as.factor(codigo))
        base::rm(a, b, all_cods, tb)

      }# End: se 'var1' for MRG, calcular a base

      {# Start: ajustando o banco

        x = TABELA %>%
          dplyr::select(dplyr::all_of(c(var1[[1]], varCitou[[1]])), peso) %>%
          dplyr::mutate(
            dplyr::across(
              dplyr::all_of(var1[[1]]),
              ~ base::factor(
                .,
                levels = DICIONARIO %>%
                  dplyr::filter(opcao_variavel == nome_var1) %>%
                  dplyr::pull(opcao_cod)
              )
            )
          ) %>%
          tidyr::pivot_longer(
            cols = var1[[1]]
            ,values_to = "var1"
          )

      }# End: ajustando o banco

      {# Start: calculando

       x = x %>%
          dplyr::group_nest(var1) %>%
          dplyr::filter(!base::is.na(.[[1]])) %>%
          dplyr::mutate(
            data = purrr::map(
              data,
              .f = ~IPpackage::FUN_Citou(., DICIONARIO, varCitou, adc_labels = F)[["Resultado_Citou"]]
            )
          ) %>%
          tidyr::unnest(data) %>%
          tidyr::unnest(data) %>%
          dplyr::filter(.[[2]] %nin% c("Total", "Base")) %>%
          dplyr::rename(
            "codlin" = nome_varCitou,
            "codcol" = "var1"
          ) %>%
          dplyr::mutate(
            "varlin" = nome_varCitou,
            "varcol" = nome_var1
          ) %>%
          dplyr::relocate(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso) %>%
          dplyr::left_join(
            y = DICIONARIO %>%
              dplyr::filter(opcao_variavel == nome_varCitou) %>%
              dplyr::mutate(
                dplyr::across(
                  opcao_cod,
                  base::as.factor
                )
              ),
            by = c(
              "varlin" = "opcao_variavel",
              "codlin" = "opcao_cod"
            )
          ) %>%
          dplyr::rename(
            "lablin" = "opcao_label",
            "titulolin" = "pergunta_enunciado"
          )

        if ( base::length(var1[[1]]) > 1 )
        {# Start: se 'var1' for MRG, trocar pct para dividir pela base certa

          x = x %>%
            dplyr::left_join(
              tabela_base,
              by = c(
                "varcol" = "variavel"
                ,"codcol" = "codigo"
              )
            ) %>%
            dplyr::mutate(
              pct = n / n_base * 100,
              pct_peso = n_peso / n_base_peso * 100
            ) %>%
            dplyr::select(-c(n_base,n_base_peso))

        }# End: se 'var1' for MRG, trocar pct para pegar a base certa

      }# End: calculando

      {# Start: Ajustando saída

        x = x %>%
          dplyr::left_join(
          y = DICIONARIO %>%
            dplyr::filter(opcao_variavel == nome_var1) %>%
            dplyr::mutate(
              dplyr::across(
                opcao_cod,
                base::as.factor
              )
            ),
          by = c(
            "varcol" = "opcao_variavel",
            "codcol" = "opcao_cod"
          )
        ) %>%
          dplyr::rename(
            "labcol" = "opcao_label",
            "titulocol" = "pergunta_enunciado"
          ) %>%
          dplyr::relocate(labcol, .after = lablin) %>%
          dplyr::mutate(
            codcol = base::as.factor(codcol)
          )

      }# End: Ajustando saída

      out = x
      base::rm(x)

    }else{

      out = tibble::tibble(
        varlin = NA,
        varcol = NA,
        codlin = NA,
        codcol = NA,
        n = NA,
        n_peso = NA,
        pct = NA,
        pct_peso = NA,
        lablin = NA,
        labcol = NA,
        titulolin = NA,
        titulocol = NA
      )

    }# End: só calcula se tem valores para tal

  }# End: calcular a frequência fechando em 'var1'

  if( var_fecha == nome_varCitou )
  {# Start: se fechar pela 'varCitou'

    {# Start: ajustando banco de dados

      x = TABELA %>%
        dplyr::select(dplyr::all_of(c(var1[[1]], varCitou[[1]])), peso) %>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(var1[[1]]),
            ~ base::factor(
              .,
              levels = DICIONARIO %>%
                dplyr::filter(opcao_variavel == nome_var1) %>%
                dplyr::pull(opcao_cod))
          )
        ) %>%
        tidyr::pivot_longer(
          cols = varCitou[[1]],
          names_to = "x"
        ) %>%
        dplyr::filter(value%in%c(1)) %>%
        tidyr::drop_na() %>%
        tidyr::pivot_longer(
          cols = var1[[1]],
          values_to = nome_var1
        ) %>%
        dplyr::select(-name)

    }# End: ajustando banco de dados

    {# Start: Calculando

      a = x
      b = a %>%
        dplyr::group_by(x) %>%
        dplyr::count(value, base::eval(base::parse(text = nome_var1))) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::rename(
          codcol = 2,
          codlin = 3,
          n = 4,
          pct = 5
        )
      c = a %>%
        dplyr::group_by(x) %>%
        dplyr::count(value, base::eval(base::parse(text = nome_var1)), wt = peso) %>%
        dplyr::mutate(pct = n / sum(n) * 100) %>%
        dplyr::rename(
          codcol = 2,
          codlin = 3,
          n_peso = 4,
          pct_peso = 5
        )
      d = dplyr::left_join(
        b,
        c,
        by = c("codcol", "codlin", "x")
      ) %>%
        dplyr::ungroup(x)

      x = d %>%
        dplyr::mutate(
          varlin = nome_var1,
          varcol = nome_varCitou
        )

      base::rm(a, b, c, d)

    }# End: Calculando

    {# Start: Ajustando saída

      x = x %>%
        dplyr::left_join(
          y = DICIONARIO %>%
            dplyr::filter(opcao_variavel == nome_var1) %>%
            dplyr::mutate(
              dplyr::across(
                opcao_cod, base::as.factor
              )
            ) %>%
            dplyr::rename(
              titulolin = pergunta_enunciado,
              lablin = opcao_label
            ),
          by = c(
            "varlin" = "opcao_variavel",
            "codlin" = "opcao_cod"
          )
        ) %>%
        dplyr::left_join(
          y = DICIONARIO %>%
            dplyr::filter(opcao_variavel == nome_varCitou) %>%
            dplyr::mutate(
              dplyr::across(
                opcao_cod,
                base::as.factor
              )
            ) %>%
            dplyr::rename(
              titulocol = pergunta_enunciado,
              labcol = opcao_label
            ),
          by = c(
            "varcol" = "opcao_variavel",
            "x" = "opcao_cod"
          )
        ) %>%
        dplyr::mutate(codcol = x) %>%
        dplyr::select(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso,
                      lablin, labcol, titulolin, titulocol
        ) %>%
        dplyr::mutate(codcol = base::as.factor(codcol))

    }# End: Ajustando saída

    if( base::all(base::is.na(x$titulocol) & base::is.na(x$labcol)) )
    {# Start: se ficou sem labcol e titulocol pq é citou

     x = x %>%
        dplyr::select(-c(labcol, titulocol)) %>%
        dplyr::left_join(
          DICIONARIO %>%
            dplyr::filter(opcao_variavel %in% c(nome_varCitou) ) %>%
            dplyr::select(-c(opcao_cod, opcao_label)) %>%
            base::unique() %>%
            dplyr::rename(titulocol = pergunta_enunciado),
          by = c("varcol" = "opcao_variavel")
        ) %>%
        dplyr::left_join(
          DICIONARIO %>%
            dplyr::filter(opcao_variavel %in% c(varCitou[[1]]) ) %>%
            dplyr::select(-c(opcao_cod, opcao_label)) %>%
            base::unique() %>%
            dplyr::rename(labcol = pergunta_enunciado),
          by = c("codcol" = "opcao_variavel")
        ) %>%
        dplyr::select(
          varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso, lablin,
          labcol, titulolin, titulocol
        )




    }# End: se ficou sem labcol e titulocol pq é citou

    out <- x
    base::rm(x)


  }# End: se fechar pela 'varCitou'


  #Retornando o resultado
  base::return(out)

}# End: FUN_cruzaCitou
