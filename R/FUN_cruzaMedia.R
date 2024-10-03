#' @title FUN_cruzaMedia
#' @name FUN_cruzaMedia
#'
#' @description Pivota e depois calcula a média (ponderada e não ponderada) da
#' variável indicada em 'ind' cruzada pela variável indicada em 'var1'.
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param var1 Variável categórica (pode ser lista - MRG).
#' @param ind Variável numérica (pode ser lista - MRG).

#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @importFrom tidyr unnest
#' @importFrom tidyr pivot_longer
#' @importFrom purrr map
#'
#' @examples
#'
#'library(dplyr)
#'library(tidyr)
#'library(purrr)
#'library(IPpackage)
#'
#'#Rodando a função-------------------------------------------------------------
#'
#'# Categoria MRG (v15mrg - v15, v16) vs Média MRG (v17mrg - v17, v18):
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v15mrg" = c("v15", "v16")),
#'  ind = base::list("v17mrg" = c("v17", "v18"))
#') %>%
#'  base::data.frame()
#'
#'# Categoria Isolada (v15) vs Média MRG (v17mrg - v17, v18):
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v15",
#'  ind = base::list("v17mrg" = c("v17", "v18"))
#') %>%
#'  base::data.frame()
#'
#'# Categoria MRG (v15mrg - v15, v16) vs Média Isolada (v17):
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v15mrg" = c("v15", "v16")),
#'  ind = "v17"
#') %>%
#'  base::data.frame()
#'
#'# Categoria Isolada (v16) vs Média Isolada (v17):
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v16",
#'  ind = "v17"
#') %>%
#'  base::data.frame()
#'
#'#Warnings---------------------------------------------------------------------
#'
#'# warning1 (ao forçar virar numérica, algo virou NA)
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = ifelse(id == 4, "texto", v17)),
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v16",
#'  ind = "v17"
#')
#'
#'# warning2 (converteu em númerico sem problemas)
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = as.character(v17)),
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v16",
#'  ind = "v17"
#')
#'
#'# warning3 (se a variável só tiver NA)
#'IPpackage::FUN_cruzaMedia(
#'  TABELA = IPpackage::IPpackage_exemplo %>%
#'    dplyr::mutate(v17 = NA),
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v16",
#'  ind = "v17"
#')
#'
#' @export
#'
#'

FUN_cruzaMedia <- function(
    TABELA,
    DICIONARIO,
    var1,
    ind
)
{# Start: FUN_cruzaMedia

  if ( base::length(var1[[1]]) == 1 )
  {# Start: Se não for um MRG

    {# Start: arrumando banco de dados

      x = TABELA %>%
        dplyr::select(dplyr::all_of(c(var1[[1]], ind[[1]], "peso")))%>%
        dplyr::mutate(
          dplyr::across(
            dplyr::all_of(var1[[1]]),
            ~ base::factor(
              .,
              levels = DICIONARIO %>%
                dplyr::filter(opcao_variavel == var1) %>%
                dplyr::pull(opcao_cod)
            )
          )
        ) %>%
        dplyr::rename("var1" = base::colnames(.)[1])

    }# End: arrumando banco de dados

    {# Start: calculando as médias

      x = x %>%
        dplyr::group_nest(var1) %>%
        dplyr::filter(!base::is.na(.[[1]])) %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_media(.,ind)[["Resultado_media"]]
          )
        ) %>%
        tidyr::unnest(data)

    }# End: calculando as médias

    {# Start: formatando os dados calculados

      if ( c(tibble::tibble(x) %>% base::nrow()) == 0 )
      {# Start: Definindo as colunas iniciais

        x = tibble::tibble(
          "var1" = base::factor(),
          "variavel" = base::character(),
          "media" = base::numeric(),
          "media_peso" = base::numeric(),
          "ma_p_splits_SupInf" = base::numeric(),
          "ma_p_splits_Sup" = base::numeric(),
          "ma_p_splits_Inf" = base::numeric(),
          "ma_p_splits_SemOut" = base::numeric(),
          "n_base" = base::numeric(),
          "n_base_peso" = base::numeric(),
          "desvp" = base::numeric(),
          "desvp_peso" = base::numeric(),
          "erro" = base::numeric(),
          "erro_peso" = base::numeric(),
          "min" = base::numeric(),
          "Q1" = base::numeric(),
          "mediana" = base::numeric(),
          "Q3" = base::numeric(),
          "max" = base::numeric(),
          "cv" = base::numeric()
        )

      } else {

        if( !base::any(base::colnames(x) == "variavel") )
        {# Start: Se não tem coluna "variavel", colocar

          x = x %>%
            dplyr::mutate(
              variavel = ind[[1]]
            )

        }# End: Se não tem coluna "variavel", colocar


      }# End: Definindo as colunas iniciais

      x = x %>%
        dplyr::rename(
          "codcol" = var1,
          "varmedia" = variavel
        ) %>%
        dplyr::mutate("varcol" = var1[[1]]) %>%
        dplyr::select(
          varmedia, varcol, codcol,
          media, media_peso,
          ma_p_splits_SupInf, ma_p_splits_Sup, ma_p_splits_Inf,ma_p_splits_SemOut,
          n_base, n_base_peso,
          desvp, desvp_peso,
          erro, erro_peso,
          min, Q1, mediana, Q3, max,
          cv
        ) %>%
        base::suppressWarnings() %>%
        dplyr::left_join(
          y = DICIONARIO %>%
            dplyr::filter(opcao_variavel == var1) %>%
            dplyr::select(opcao_cod, opcao_label, opcao_variavel, pergunta_enunciado) %>%
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
        )

    }# End: formatando os dados calculados

    #Resultado
    out = x
    base::rm(x)

  }# End: Se não for um MRG
  else
  {# Star: Se for um MRG

    {# Start: arrumando banco de dados

      x <- TABELA %>%
        dplyr::select(dplyr::all_of(c(var1[[1]], ind[[1]], "peso"))) %>%
        tidyr::pivot_longer(
          cols = {{var1}}[[1]],
          values_to = base::names(var1)
        ) %>%
        dplyr::select(-name) %>%
        dplyr::mutate(
          dplyr::across(
            base::names(var1),
            ~ base::factor(
              .,
              levels = DICIONARIO %>%
                dplyr::filter(opcao_variavel == base::names(var1)) %>%
                dplyr::pull(opcao_cod)
            )
          )
        )

    }# End: arrumando banco de dados

    {# Start: calculando as médias

      x = x %>%
        dplyr::group_nest(dplyr::across(base::names(var1))) %>%
        dplyr::filter(!base::is.na(.[[1]])) %>%
        dplyr::rename("var1" = base::colnames(.)[1]) %>%
        dplyr::mutate(
          data = purrr::map(
            data,
            .f = ~ IPpackage::FUN_media(.,ind)[["Resultado_media"]]
          )
        ) %>%
        tidyr::unnest(data)

    }# End: calculando as médias


    {# Start: formatando os dados calculados

      if( !base::any(base::colnames(x) == "variavel") )
      {# Start: Se não tem coluna "variavel", colocar

        x = x %>%
          dplyr::mutate(
            variavel = ind[[1]]
          )

      }# End: Se não tem coluna "variavel", colocar

      x = x %>%
        dplyr::rename(
          "codcol" = var1,
          "varmedia" = variavel
        ) %>%
        dplyr::mutate("varcol" = base::names(var1)) %>%
        dplyr::select(
          varmedia, varcol, codcol,
          media, media_peso,
          ma_p_splits_SupInf, ma_p_splits_Sup, ma_p_splits_Inf,ma_p_splits_SemOut,
          n_base, n_base_peso,
          desvp, desvp_peso,
          erro, erro_peso,
          min, Q1, mediana, Q3, max,
          cv
        ) %>%
        base::suppressWarnings() %>%
        dplyr::left_join(
          y = DICIONARIO %>%
            dplyr::filter(opcao_variavel == base::names(var1)) %>%
            dplyr::select(opcao_cod, opcao_label, opcao_variavel, pergunta_enunciado) %>%
            dplyr::mutate(dplyr::across(opcao_cod, as.factor)),
          by = c(
            "varcol" = "opcao_variavel",
            "codcol" = "opcao_cod"
          )
        ) %>%
        dplyr::rename(
          "labcol" = "opcao_label",
          "titulocol" = "pergunta_enunciado"
        )
    }# End: formatando os dados calculados

    #Resultado
    out = x
    base::rm(x)


  }# End: Se for um MRG

  #Retornando o resultado
  base::return(out)

}# End: FUN_cruzaMedia

