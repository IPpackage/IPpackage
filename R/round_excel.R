#' @title round_excel
#' @name round_excel
#'
#' @description  Muitas vezes, precisamos representar os resultados de um Excel
#' em diferentes formatos (tabelas em PNG, gráficos, etc.), mas esbarramos em um
#' pequeno problema: R e Excel utilizam metodologias diferentes de arredondamento.
#' Por isso, o que no Excel vira 4.5, por exemplo, as saídas do R retornam 4.4
#'
#' Para resolver esse problema, desenvolvemos a função IPpackage::round_excel, que
#' arredonda os números seguindo a mesma metodologia que o Excel.
#'
#' Obs.: a função funciona tanto para números quanto para vetores de números.
#'
#' @param y número ou vetor contendo números
#' @param digits número de casas decimais
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @examples
#'
#' ##Número; nenhuma casa decimal
#' IPpackage::round_excel(4.45)
#' IPpackage::round_excel(4.45,0)
#' ##Número; 5 casas decimais
#' IPpackage::round_excel(4.45,5)
#' ##Vetor; 1 casa decimal
#' IPpackage::round_excel(c(4.45,-4.45,855.55,4,NA,86.542),1)
#' ##Coluna de um data.frame
#' set.seed(42)
#' df=data.frame(casa=c(rep('A',5),rep('B',5)),renda_familiar=runif(10,0,5000));df[1,2]<-NA;df[5,2]<-1
#' #Arredondando a renda em 2 casas decimais
#' df$rf_arred=IPpackage::round_excel(df$renda_familiar,2)
#' df$rf_arred2=round_excel_decimais(df$renda_familiar,2,",")
#' df
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
