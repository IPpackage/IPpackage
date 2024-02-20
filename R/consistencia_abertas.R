#' @title consistencia_abertas
#' @name consistencia_abertas
#'
#' @description A função foi criada com o propósito de verificar se todos os textos
#' presentes em 'var_orig' foram codificados na 'var_codificada'.
#'
#' @param log_consistencia_aberta Se a variável em questão for a primeira a ser testada,
#' 'log_consistencia_aberta' é definida como 'NULL'; no entanto, se um arquivo no formato de
#' saída dessa função já existir, 'log_consistencia_aberta' será o seu data.frame correspondente
#' @param xdf Banco de dados
#' @param var_id Variável no Banco de Dados com o 'id' do entrevistado
#' @param var_codificada Variável com a codificação da 'var_orig'
#' @param var_orig Se 'var_orig' for igual a NULL, a função concatenará 'var_codificada'
#' com '_orig'. Se a variável original não for essa concatenação, por favor, especifique-a aqui
#' @param show 'TRUE' para mostrar 'var_id', 'var_codificada' e 'var_orig' em caso de erro -serão
#' exibidas apenas as variáveis indicadas e as linhas com erros; 'FALSE' para não
#' imprimir as linhas, mesmo na presença de erros.
#' @param obs Colocar essa observação em 'descricao'.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr

#' @examples
#'
#' data=IPpackage::IPpackage_exemplo
#'
#' ##Tudo OK
#' log_consistencia_aberta=IPpackage::consistencia_abertas(
#'   log_consistencia_aberta=NULL,xdf=data,var_id="id",var_codificada="v14",
#'   var_orig=NA,show=FALSE,obs=NA)
#'
#' ##ERRO
#' data$v14[3]=NA
#' log_consistencia_aberta=IPpackage::consistencia_abertas(
#'   log_consistencia_aberta=log_consistencia_aberta,xdf=data,var_id="id",var_codificada="v14",
#'   var_orig=NA,show=FALSE,obs=NA)
#'
#' log_consistencia_aberta=IPpackage::consistencia_abertas(
#'   log_consistencia_aberta=log_consistencia_aberta,xdf=data,var_id="id",var_codificada="v14",
#'   var_orig=NA,show=TRUE,obs=NA)
#'
#' log_consistencia_aberta=IPpackage::consistencia_abertas(
#'   log_consistencia_aberta=log_consistencia_aberta,xdf=data,var_id="id",var_codificada="v14",
#'   var_orig=NA,show=TRUE,obs="observação")
#'
#' @export
#'
#'

consistencia_abertas=function(log_consistencia_aberta=NULL,xdf,var_id,var_codificada,var_orig=NA,show=FALSE,obs=NA){
  if (base::is.null(log_consistencia_aberta)) {
    log_consistencia_aberta = base::data.frame(var_codificada = NA,var_orig=NA ,resultado = NA,descricao = NA)
  }

  if(base::is.na(var_orig)){
    var_orig=base::paste0(var_codificada,"_orig")
  }
  x=xdf%>%
    dplyr::select(dplyr::all_of(var_id),dplyr::all_of(var_codificada),dplyr::all_of(var_orig))%>%
    dplyr::filter(
      base::is.na(.data[[var_codificada]])&!base::is.na(.data[[var_orig]])
    )
  if(base::nrow(x)>0){
    descricao=paste0("[",var_codificada,"] var x var original ","ERRO (",base::nrow(x)," casos)")
    resultado="error"
    cat(paste0("\033[1;31m",descricao,"\033[0m\n"))
    cor="vermelho"
  }else{
    descricao="-"
    resultado="OK"
    cat(paste0("\033[1;32m[",var_codificada,"] var x var original OK\033[0m\n"))
    cor="verde"
  }
  if(!base::is.na(obs)){
    descricao=obs
    cor="amarelo"
  }

  log_consistencia_aberta <- base::rbind(log_consistencia_aberta, base::data.frame(var_codificada = var_codificada,var_orig=var_orig, resultado = resultado,descricao = descricao))
  log_consistencia_aberta=log_consistencia_aberta%>%tidyr::drop_na()
  if(show==TRUE){
    base::cat("\033[3m erros\033[0m\n")
    IPpackage::print_cor(x,cor)
  }

  return(log_consistencia_aberta)
}


