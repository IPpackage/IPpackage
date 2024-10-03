#' @title FUN_cruzaMRG
#' @name FUN_cruzaMRG
#'
#' @description Calcula duas possibilidades de frequências cruzadas: 'var vs mrg' ou 'mrg vs mrg'.
#' O parâmetro 'var_fecha' define a variável na coluna.
#'
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param var1 Primeira variável do cruzamento. Pode ser MRG ou variável isolada (lista).
#' @param varMRG Segunda variável do cruzamento. Tem que ser um MRG (lista).
#' @param var_fecha Variável na coluna.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_subset

#' @examples
#'
#'library(dplyr)
#'library(tidyr)
#'library(stringr)
#'library(IPpackage)
#'
#'# Var isolada vs mrg fechando na var isolada----------------------------------
#'IPpackage::FUN_cruzaMRG(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v1"="v1"),
#'  varMRG = base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'  var_fecha = "v1"
#')
#'
#'# Var isolada vs mrg fechando no mrg------------------------------------------
#'IPpackage::FUN_cruzaMRG(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v1"="v1"),
#'  varMRG = base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'  var_fecha = "v3mrg"
#')
#'# mrg vs mrg------------------------------------------------------------------
#'
#'# Fechando em 'v3mrg
#'IPpackage::FUN_cruzaMRG(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v19mrg"=c("v19_1","v19_2")),
#'  varMRG = base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'  var_fecha = "v3mrg"
#')
#'
#'# Fechando em 'v19mrg
#'IPpackage::FUN_cruzaMRG(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = base::list("v19mrg"=c("v19_1","v19_2")),
#'  varMRG = base::list("v3mrg" = c("v3_1","v3_2","v3_3")),
#'  var_fecha = "v19mrg"
#')
#'
#' @export
#'

FUN_cruzaMRG <- function(
    TABELA,
    DICIONARIO,
    var1,
    varMRG,
    var_fecha
)
{# Start: FUN_cruzaMRG

  if( base::class(var1) == "list" )
  {# Start: Definindo 'nome_var1'

    nome_var1 = base::names(var1)
    var1 = var1[[1]]

  }else{

    nome_var1 <- var1

  }# End: Definindo 'nome_var1'

  var_naofecha <- c(nome_var1, base::names(varMRG)) %>%
    stringr::str_subset(base::paste0(var_fecha,"$"), negate = T)

  TABELA %>%
    dplyr::select(dplyr::all_of(c(var1, varMRG[[1]])), peso)%>%
    tidyr::pivot_longer(
      cols = {{varMRG}}[[1]],
      values_to = base::names(varMRG)
    ) %>%
    {# Strat: analisando de 'var1' é ou não mrg

      a = tibble::tibble(.)

      if( base::length(var1) > 1 )
      {# Start: Se 'var1' for mrg

        a = a %>%
          tidyr::pivot_longer(
          cols = var1,
          values_to = nome_var1,
          names_to = "var1"
        )
        a = a %>%
          dplyr::select(-var1)
      }# End: Se 'var1' for mrg

      a

    } %>%
    dplyr::select(-name) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(nome_var1),
        ~ base::factor(
          .,
          levels = DICIONARIO %>%
            dplyr::filter(opcao_variavel == nome_var1) %>%
            dplyr::pull(opcao_cod)
          )
        )
      ) %>%
    dplyr::mutate(
      dplyr::across(
        base::names(varMRG),
        ~ base::factor(
          .,
          levels = DICIONARIO %>%
            dplyr::filter(opcao_variavel == base::names(varMRG)) %>%
            dplyr::pull(opcao_cod)
          )
        )
      ) %>%
    dplyr::filter(!base::is.na(.[[1]]), !base::is.na(.[[2]])) %>%
    dplyr::filter(!base::is.na(.[[3]])) %>%
    dplyr::group_by(
      dplyr::across(
        dplyr::all_of(c(nome_var1, base::names(varMRG)))
      )
      ,.drop = T
    ) %>%
    summarise(
      "n" = dplyr::n(),
      "n_peso" = base::sum(peso),
      .groups = "drop"
    ) %>%
    dplyr::group_by(dplyr::across(dplyr::all_of(var_fecha))) %>%
    dplyr::mutate(
      "pct" = (100 * n / base::sum(n)),
      "pct_peso" = (100 * n_peso / base::sum(n_peso))
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      "varlin" = var_naofecha,
      "varcol" = var_fecha
      ) %>%
    dplyr::rename(
      "codlin" = var_naofecha,
      "codcol" = var_fecha
      ) %>%
    dplyr::relocate(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso) %>%
    dplyr::left_join(
      y = DICIONARIO %>%
        dplyr::filter(opcao_variavel == var_naofecha) %>%
        dplyr::select(opcao_cod, opcao_label, opcao_variavel, pergunta_enunciado) %>%
        dplyr::mutate(dplyr::across(opcao_cod, as.factor)),
      by = c("varlin" = "opcao_variavel", "codlin" = "opcao_cod")
    ) %>%
    dplyr::rename(
      "lablin" = "opcao_label",
      "titulolin" = "pergunta_enunciado"
      ) %>%
    dplyr::left_join(
      y = DICIONARIO %>%
        dplyr::filter(opcao_variavel == var_fecha) %>%
        dplyr::select(opcao_cod, opcao_label, opcao_variavel, pergunta_enunciado) %>%
        dplyr::mutate(dplyr::across(opcao_cod, base::as.factor)),
      by = c("varcol" = "opcao_variavel", "codcol" = "opcao_cod")
    ) %>%
    dplyr::rename(
      "labcol" = "opcao_label",
      "titulocol" = "pergunta_enunciado"
      ) %>%
    dplyr::relocate(labcol, .after = lablin)

}# End: FUN_cruzaMRG

