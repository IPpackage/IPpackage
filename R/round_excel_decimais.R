#' @title round_excel_decimais
#' @name round_excel_decimais
#'
#' @description Muitas vezes, precisamos representar os resultados de um Excel em
#'  diferentes formas: tabelas em PNG, gráficos,... mas esbarramos com um pequeno
#'  problema: R e Excel utilizam métodos diferentes de arredondamento. Então, o
#'  que no excel vida 0.5, por exemplo, as saídas do R retornam 0.6.
#'
#'  Para resolver esse problema, desenvolvemos a função round_excel, que arredonda
#'  os números seguindo a mesma metodologia que o Excel.
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
