#' @title round_excel
#' @name round_excel
#'
#' @description Arredonda um número (ou vetor contendo números) com saída semelhante ao Excel.
#'
#' @param y número ou vetor contendo números
#' @param digits número de casas decimais
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @export
#'
round_excel <- function (y, digits=0){
  x<-ifelse(y<0,-(y),y)
  posneg = sign(x)
  z = trunc(abs(x) * 10 ^ (digits + 1)) / 10
  z = floor(z * posneg + 0.5) / 10 ^ digits
  z<-ifelse(y<0,-(z),z)
  return(z)
}
