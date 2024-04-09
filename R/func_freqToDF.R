#' @title func_freqToDF
#' @name func_freqToDF
#'
#' @description Falta escrever.
#'
#' @param TABELA Falta escrever.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#'
#' @examples
#'
#'#Falta escrever.
#'
#' @export
#'
#'


func_freqToDF <- function(
    TABELA
)
{# Start: func_freqToDF

  `%nin%` = base::Negate(`%in%`)

  TABELA %>%
    dplyr::rename_with(
      .fn = ~ "opcao_label",
      .cols = dplyr::ends_with("_label")
      ) %>%
    dplyr::mutate("variavel" = base::colnames(.)[1]) %>%
    dplyr::rename("codigo" = 1) %>%
    dplyr::mutate("total" = n[base::which(.[[1]] == "Total")]) %>%
    dplyr::mutate("total_peso" = n_peso[base::which(.[[1]] == "Total")]) %>%
    dplyr::mutate("n_base" = n[base::which(.[[1]] == "Base")]) %>%
    dplyr::mutate("n_base_peso" = n_peso[base::which(.[[1]] == "Base")]) %>%
    dplyr::mutate("pct_base" = pct[base::which(.[[1]] == "Base")]) %>%
    dplyr::mutate("pct_base_peso" = pct_peso[base::which(.[[1]] == "Base")]) %>%
    dplyr::filter(.[[1]] %nin% c("Total", "Base")) %>%
    dplyr::select(
      variavel, codigo, n, n_peso, pct, pct_peso, total, total_peso, n_base,
      n_base_peso, pct_base, pct_base_peso, opcao_label
    )

}# End: func_freqToDF
