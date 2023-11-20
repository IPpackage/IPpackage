#' @title print_cor
#' @name print_cor
#'
#' @description Imprimir um data.frame com uma cor específica
#'
#' @param df Data.frame ou vetor
#' @param cor cor ='preto', 'vermelho', 'verde', 'amarelo', 'azul', 'roxo', 'ciano'
#'  ou 'branco'
#' @param negrito negrito = 'TRUE' ou 'FALSE'
#' @param italico 'italico = TRUE' ou 'FALSE'
#' @param borrado borrado = 'TRUE' ou 'FALSE'
#' @param sublinhado sublinhado = 'TRUE' ou 'FALSE'
#' @param data.frame data.frame = Imprimir no formato de data.frame. 'TRUE' ou 'FALSE'
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @examples
#'
#' IPpackage::print_cor(df="--------------show--------------",cor="verde",negrito=TRUE,
#'           italico=FALSE,borrado=FALSE,sublinhado=FALSE,data.frame=FALSE)
#' IPpackage::print_cor(df=c("a","b"),cor="ciano",negrito=TRUE,
#'           italico=TRUE,borrado=TRUE,sublinhado=TRUE,data.frame=FALSE)
#' IPpackage::print_cor(df=head(IPpackage::IPpackage_exemplo),cor="vermelho",data.frame=TRUE)
#'
#' @export
#'
#'

print_cor <- function(df, cor, negrito = FALSE, italico = FALSE,borrado=FALSE,sublinhado=FALSE,data.frame=TRUE) {
  if(data.frame==TRUE){
    df = as.data.frame(df)
  }

  cor_codigo <- base::switch(
    cor,
    preto = "\033[30m",
    vermelho = "\033[31m",
    verde = "\033[32m",
    amarelo = "\033[33m",
    azul = "\033[34m",
    roxo = "\033[35m",
    ciano = "\033[36m",
    branco = "\033[37m",
    "\033[39m"  # Cor padrão caso não seja uma opção válida
  )
  if(negrito==TRUE){
    cor_codigo=base::sub("\\[", "[1;", cor_codigo)
  }
  if(borrado==TRUE){
    cor_codigo=base::sub("\\[", "[2;", cor_codigo)
  }
  if(italico==TRUE){
    cor_codigo=base::sub("\\[", "[3;", cor_codigo)
  }
  if(sublinhado==TRUE){
    cor_codigo=base::sub("\\[", "[4;", cor_codigo)
  }

  if(data.frame==TRUE){
    cat(cor_codigo)
    print(df)
    cat("\033[0m")
  }else{
    cat(paste0(cor_codigo,df,"\033[0m"))
  }
}


