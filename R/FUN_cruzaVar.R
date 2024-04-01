#' @title FUN_cruzaVar
#' @name FUN_cruzaVar
#'
#' @description Calcula a frequência cruzada entre duas variáveis (ponderada e não ponderada).
#' O parâmetro 'var_fecha' define a variável na coluna.
#'
#' @param TABELA Banco de dados a ser analisado.
#' @param DICIONARIO Dicionário correspondente à TABELA.
#' @param var1 Primeira variável do cruzamento.
#' @param var2 Segunda variável do cruzamento.
#' @param var_fecha Variável na coluna.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#'
#' @examples
#'
#'library(dplyr)
#'library(IPpackage)
#'
#'# Variável na coluna: v1
#'IPpackage::FUN_cruzaVar(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v1",
#'  var2 = "v10",
#'  var_fecha = "v1"
#')
#'
#'# Variável na coluna: v10
#'IPpackage::FUN_cruzaVar(
#'  TABELA = IPpackage::IPpackage_exemplo,
#'  DICIONARIO = IPpackage::IPpackage_dicionario,
#'  var1 = "v1",
#'  var2 = "v10",
#'  var_fecha = "v10"
#')
#'
#'
#' @export
#'
#'

FUN_cruzaVar <- function(
    TABELA,
    DICIONARIO,
    var1,
    var2,
    var_fecha
)
{# Start: FUN_cruzaVar

  {# Start: armazenar resultados e definições iniciais

    `%nin%` = base::Negate(`%in%`)

    var_naofecha <- c(var1, var2) %>%
      base::subset(. %nin% var_fecha)

  }# End: armazenar resultados e definições iniciais


  # Start: Cálculo do cruzamento
  TABELA %>%
    dplyr::select(dplyr::all_of(c(var1, var2)), peso) %>%
    dplyr::filter(dplyr::if_any(.cols = -peso,.fns = ~ !base::is.na(.x))) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(var1),
        ~ base::factor(
          .,
          levels = DICIONARIO %>%
            dplyr::filter(opcao_variavel == var1) %>%
            dplyr::pull(opcao_cod)
        )
      )
    ) %>%
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(var2),
        ~ base::factor(
          .,
          levels = DICIONARIO %>%
            dplyr::filter(opcao_variavel == var2) %>%
            dplyr::pull(opcao_cod)
        )
      )
    ) %>%
    dplyr::filter(!base::is.na(.[[1]]), !base::is.na(.[[2]])) %>%
    dplyr::filter(!base::is.na(.[[3]])) %>%
    dplyr::group_by(dplyr::across(c({{var1}}, {{var2}})), .drop = T) %>%
    dplyr::summarise(
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
    dplyr::mutate("varlin" = var_naofecha, "varcol" = var_fecha) %>%
    dplyr::rename("codlin" = var_naofecha, "codcol" = var_fecha) %>%
    dplyr::relocate(varlin, varcol, codlin, codcol, n, n_peso, pct, pct_peso) %>%
    dplyr::left_join(
      y = DICIONARIO %>%
        dplyr::filter(opcao_variavel == var_naofecha) %>%
        dplyr::mutate(dplyr::across(opcao_cod, as.factor)),
      by = c("varlin" = "opcao_variavel", "codlin" = "opcao_cod")
    ) %>%
    dplyr::rename("lablin" = "opcao_label", "titulolin" = "pergunta_enunciado") %>%
    dplyr::left_join(
      y = DICIONARIO %>%
        dplyr::filter(opcao_variavel == var_fecha) %>%
        dplyr::mutate(dplyr::across(opcao_cod, as.factor)),
      by = c("varcol" = "opcao_variavel", "codcol" = "opcao_cod")
    ) %>%
    dplyr::rename("labcol" = "opcao_label", "titulocol" = "pergunta_enunciado") %>%
    dplyr::relocate(labcol, .after = lablin)


}# End: FUN_cruzaVar
