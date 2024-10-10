#' @title substitui_prefixo_variavel
#' @name substitui_prefixo_variavel
#'
#' @description Esta função recebe um vetor contendo o(s) nome(s) de variável(is) que
#' começam com um valor especificado ('trocar_esse'). Ela substitui esse prefixo
#' inicial por um novo valor ('por_esse') e adiciona um caractere de preenchimento
#' ('add_p_n_digitos') até que o nome tenha o número total de dígitos especificado
#' ('n_digitos').
#'
#' O uso mais comum desta função é substituir o prefixo 'v' ('trocar_esse' = 'v')
#' por 'P' ('por_esse' = 'P'), preenchendo com zeros ('add_p_n_digitos' = '0')
#' para que os nomes tenham exatamente 3 dígitos ('n_digitos' = 3).
#'
#' Além disso, os nomes podem ser convertidos para letras maiúsculas
#' ('maiusculo' = TRUE).
#'
#'
#' @param variavel Vetor contendo o(s) nome(s) das variáveis que devem ser
#' modificadas
#' @param trocar_esse String que especifica o prefixo que será substituído no(s)
#'  nome(s) da(s) variável(is). O padrão é 'v'
#' @param por_esse String que será usada para substituir o valor especificado em
#' 'trocar_esse'. O padrão é 'P'
#' @param n_digitos Número inteiro que define o total de dígitos desejado para
#' os nomes das variáveis após a substituição. O padrão é 3
#' @param add_p_n_digitos Caractere utilizado para preencher até que
#' o nome da variável tenha o número de dígitos especificado em 'n_digitos'.
#' O padrão é '0'
#' @param maiusculo Booleano que indica se os nomes das variáveis devem ser
#' convertidos para letras maiúsculas após a substituição. O padrão é TRUE.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import stringr
#'
#' @examples
#'
#' # Utilizando o padrão
#' IPpackage::substitui_prefixo_variavel(
#'   c("v1", "v152", "v1521", "v5_3", "v5r1","v5mrg", "v2_1_asdfqwe_32")
#' )
#'
#' # Modificando todos os parâmetros
#' IPpackage::substitui_prefixo_variavel(
#'   variavel = c("i1", "i152", "i1521", "i5_3", "i5r1","i5mrg", "i2_1_asdfqwe_32"),
#'   trocar_esse = "i",
#'   por_esse = "G",
#'   n_digitos = 5,
#'   add_p_n_digitos = "X",
#'   maiusculo = FALSE
#' )
#'
#' @export
#'
#'

substitui_prefixo_variavel <- function(
    variavel,
    trocar_esse = "v",
    por_esse = "P",
    n_digitos = 3,
    add_p_n_digitos = "0",
    maiusculo = TRUE
    )
{# Start: substitui_prefixo_variavel

  # Extrai a parte numérica após o "trocar_esse" (ou o início da string se não houver "trocar_esse")
  parte_numerica <- base::sub(
    pattern = base::paste0(trocar_esse,"?(\\d+).*"),
    replacement = "\\1",
    x = variavel
  )

  # Adiciona o caractere especificado para preencher à esquerda até atingir n_digitos
  parte_numerica <- stringr::str_pad(
    string = parte_numerica,
    width = n_digitos,
    side = "left",
    pad = add_p_n_digitos
  )

  # Substitui "v" por "P" e concatena com a parte numérica e o restante da string
  var_final = base::paste0(
    por_esse,
    parte_numerica,
    base::sub(base::paste0(trocar_esse,"\\d+"), "", variavel)
    )

  if( maiusculo == TRUE )
  {# Start: Colocando em maíusculo

    var_final = stringr::str_to_upper(var_final)

  }# End: Colocando em maíusculo

  var_final

}# End: substitui_prefixo_variavel
