#' @title round_excel_decimais
#' @name round_excel_decimais
#'
#' @description Ah acaso em que, além de precisarmos manter a metodologia de
#' arredondamento do Excel, precisamos imprimir o mesmo número de casas decimais.
#' Nesses casos, a função IPpackage::round_excel_decimais se apresenta como uma ótima solução.
#'
#' Tanto a função padrão do R (round) quanto a IPpackage::round_excel (do pacote)
#' suprimem os zeros à direta: 1.0000 vira 1 em ambas
#'
#' Utilizando a função round_excel_decimais, além de mantermos o número de casas
#' decimais, podemos personalizar o termo de separação (vírgula, ponto, ponto e vírgula,…).
#'
#' Obs.: a função funciona tanto para números quanto para vetores de números.
#'
#' @param y número ou vetor contendo números
#' @param n_depois_ponto número de casas decimais
#' @param ponto separador decimal a ser retornado
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import stringr
#' @import dplyr
#'
#' @examples
#'
#' ##Número; nenhuam casa decimal; vírgula e ponto
#' IPpackage::round_excel_decimais(4.45,0,",")
#' IPpackage::round_excel_decimais(4.45,0,".")
#' ##Número; 5 casas decimais; vírgula e ponto
#' IPpackage::round_excel_decimais(4.45,5,",")
#' IPpackage::round_excel_decimais(4.45,5,".")
#' ##Vetor; 4 casa decimal; vírgula e ponto
#' IPpackage::round_excel_decimais(c(4.45,-4.45,855.5555454,4,NA,86.542),4,",")
#' IPpackage::round_excel_decimais(c(4.45,-4.45,855.5555454,4,NA,86.542),4,".")
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
round_excel_decimais<-function(y,n_depois_ponto=0,ponto=","){
  xxx<-c()
  for(i in 1:length(y)){
    x=y[i]
    if(!is.na(x)){
      x=IPpackage::round_excel(as.numeric(x),n_depois_ponto)%>%as.character()
      if(n_depois_ponto>0){
        if(!stringr::str_detect(x,"[.]")){
          xx=paste0(x,".",paste0(rep(0,n_depois_ponto),collapse = ""))
        }else{
          o_q_tem=sub(".*[.]","",x)
          n_tem=stringr::str_length(o_q_tem)
          if(n_tem<n_depois_ponto){
            xx=paste0(sub("[.].*","",x),".",o_q_tem,paste0(rep(0,n_depois_ponto-n_tem),collapse = ""))
          }else{
            xx=x
          }
        }
      }else{
        xx=x
      }
      if(ponto!="."&n_depois_ponto!=0){xx=stringr::str_replace(xx,"[.]",ponto)}
    }else{xx=x}
    if(i==1){xxx<-xx}else{xxx[i]<-xx}
    #print(xxx)
  }
  rm(x,i,y,xx,n_depois_ponto,o_q_tem,n_tem)%>%suppressWarnings()
  return(xxx)
}
