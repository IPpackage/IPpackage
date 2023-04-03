#' @title consistencia
#' @name consistencia
#'
#' @description  x
#'
#' @param log_consistenia log_consistenia
#' @param x x
#' @param vars vars
#' @param nome nome
#' @param regra regra
#' @param pode_falta pode_falta
#' @param show show
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#'
#' data=IPpackage::IPpackage_exemplo
#' ##Tudo OK
#' log_consistenia=consistencia(log_consistenia=NULL,x=data,vars="v1",regra="100",
#' pode_falta=FALSE,show=c("id","v1"))
#' log_consistenia=consistencia(log_consistenia=log_consistenia,x=data,vars="v2",
#' regra="v1%nin%c(1)",pode_falta=FALSE,show=c("id","v1","v2"))
#' log_consistenia=consistencia(log_consistenia=log_consistenia,x=data,
#' vars=paste0("v3_",1:3),regra="v1%nin%c(1)",pode_falta=TRUE,show=c("id","v1","v2")
#' ,nome="G003")
#' log_consistenia=consistencia(log_consistenia=log_consistenia,x=data,
#' vars=paste0("v",4:11),regra="100",pode_falta=TRUE,show=c("id","v1","v2"))
#' ##Com Erro
#' data$v1[3]=NA;data$v1[4]<-1
#' log_consistenia=consistencia(log_consistenia=NULL,x=data,vars="v1",regra="100",
#' pode_falta=FALSE,show=c("id","v1","v2"))
#' log_consistenia=consistencia(log_consistenia=log_consistenia,x=data,vars="v2",
#' regra="v1%nin%c(1)",pode_falta=FALSE,show=c("id","v1","v2"))
#' log_consistenia=consistencia(log_consistenia=log_consistenia,x=data,
#' vars=paste0("v3_",1:3),regra="v1%nin%c(1)",pode_falta=TRUE,show=c("id","v1",
#' paste0("v3_",1:3)),nome="G003")
#'
#' @export
#'
#'

consistencia<-function(log_consistenia=NULL,x,vars,nome=NULL,regra,pode_falta=FALSE,show=FALSE){
  `%nin%` = Negate(`%in%`)
  if(is.null(log_consistenia)){log_consistenia=data.frame(var=NA,entrou=NA,base=NA,resultado=NA,descricao=NA,regra=NA)}
  #MRG
  {
    consistencia_mrg<-function(log_consistenia,x,vars,regra,nome,show=show){
      if(regra!="100"){
        bd=x%>%dplyr::mutate(base=ifelse(eval(parse(text=regra)),1,0),base=ifelse(is.na(base),0,base),
                             entrou=ifelse(eval(parse(text =paste0("!is.na(",vars,")",collapse = "|") )),1,0)
        )
        if(all(bd$entrou==bd$base)){
          log_consistenia<-rbind(log_consistenia,
                                 bd%>%dplyr::summarise(var=nome,entrou=sum(entrou),base=sum(base),resultado="OK",descricao="-",regra=regra)
          )
        }else{
          #entrou e nao deveria
          e_nd=bd%>%dplyr::filter(entrou%in%c(1)&base%in%c(0))%>%nrow()
          #nao entrou e deveria
          ne_d=bd%>%dplyr::filter(entrou%in%c(0)&base%in%c(1))%>%nrow()
          if(ne_d>0&e_nd==0){d=paste0(ne_d," nao entrou e deveria")}
          if(ne_d==0&e_nd>0){d=paste0(e_nd," entrou e nao deveria")}
          if(ne_d>00&e_nd>0){d=paste0(ne_d," nao entrou e deveria;",e_nd," entrou e nao deveria")}
          log_consistenia<-rbind(log_consistenia,
                                 bd%>%dplyr::summarise(var=nome,entrou=sum(entrou),base=sum(base),resultado="Erro",descricao=d,regra=regra)
          )
          rm(e_nd,ne_d,d)%>%suppressWarnings()
        }

      }else{
        bd=x%>%dplyr::mutate(base=1,
                             entrou=ifelse(eval(parse(text =paste0("!is.na(",vars,")",collapse = "|") )),1,0))
        #nao entrou e deveria
        ne_d=bd%>%dplyr::filter(entrou%in%c(0)&base%in%c(1))%>%nrow()
        if(ne_d>0){d=paste0(ne_d," nao entrou e deveria")}else{d="-"}

        log_consistenia<-rbind(log_consistenia,
                               bd%>%dplyr::summarise(var=nome,entrou=sum(entrou),base=sum(base),resultado=ifelse(all(base==entrou),"OK","Erro"),descricao=d,regra=regra)
        )
        rm(ne_d,d)%>%suppressWarnings()

      }
      print(log_consistenia[nrow(log_consistenia),])
      #mostrar o banco de dados com os erros e vars que quero
      if(all(show!=FALSE)&log_consistenia[nrow(log_consistenia),4]!="OK"){
        message("--------------show-------------")
        print(bd%>%dplyr::select(all_of(show),entrou,base)%>%dplyr::filter(entrou!=base)%>%data.frame())
      }
      return(log_consistenia%>%unique())
    }
    if(length(vars)>1){
      message("--------------MRG--------------")
      if(is.null(nome)){nome=paste0("MRG-",vars[1]);message(paste0("Como nome=NULL, atribuimos o nome '",nome,"'"))}
      log_consistenia<-rbind(log_consistenia,consistencia_mrg(log_consistenia,x,vars,regra,nome,show))
      for(i in 1:nrow(log_consistenia)){
        if(all(is.na(log_consistenia[i,]))){log_consistenia<-log_consistenia[-i,]}
      }

    }
  }
  #Isoladas
  {
    for(v in 1:length(vars)){
      var=vars[v]
      message(paste0("--------------Var: ",var,"-------------"))

      if(regra=="100"){
        entrou=sum(!is.na(x[[var]]))
        base=nrow(x)
        bd=x%>%dplyr::mutate(base=1,entrou=ifelse(!is.na(eval(parse(text =var))),1,0))
        if(entrou==base){resultado="OK";descricao="-"}else{
          resultado="Erro"
          descricao=paste0(x%>%dplyr::filter(eval(parse(text =paste0("is.na(",var,")"))))%>%nrow()," nao entrou e deveria")
        }
        log_consistenia<-rbind(log_consistenia,data.frame(var,entrou,base,resultado=resultado,descricao=descricao,regra=regra))
      }else{
        bd=x%>%dplyr::mutate(base=ifelse(eval(parse(text =regra)),1,0),entrou=ifelse(!is.na(eval(parse(text =var))),1,0))

        x$x_entrou_x=ifelse(!is.na(x[[var]]),1,0)
        x<-x%>%dplyr::mutate(x_base_x=ifelse(eval(parse(text =regra)),1,0));x$x_base_x<-ifelse(is.na(x$x_base_x),0,x$x_base_x)
        entrou=sum(x$x_entrou_x)
        base=sum(x$x_base_x)
        #resultado
        if(all(x$x_entrou_x==x$x_base_x)){
          resultado="OK";descricao="-"
        }else{
          xx<-x[which(x$x_entrou_x!=x$x_base_x),which(colnames(x)%in%c("x_entrou_x","x_base_x"))]
          #xx<-x%>%dplyr::filter(c(x$x_entrou_x)!=c(x$x_base_x))%>%dplyr::select(x_entrou_x,x_base_x)
          resultado="Erro"
          n_entrou_menos=paste(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%nrow(),"nao entrou e deveria")
          n_entrou_mais=paste(xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%nrow(),"entrou e nao deveria")

          if(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%nrow()!=0&xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%nrow()!=0){
            descricao=paste0(n_entrou_menos,";",n_entrou_mais)
          }else{
            if(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%nrow()!=0){descricao=n_entrou_menos}
            if(xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%nrow()!=0){descricao=n_entrou_mais}
          }
        }
        log_consistenia<-rbind(log_consistenia,data.frame(var,entrou,base,resultado=resultado,descricao=descricao,regra=regra))
      }
      for(i in 1:nrow(log_consistenia)){
        if(all(is.na(log_consistenia[i,]))){log_consistenia<-log_consistenia[-i,]}
      }
      #Se puder faltar
      if(pode_falta==TRUE){
        if(log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="resultado")]=="Erro"){
          if(log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("naoentrouedeveria")){#&!log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("entrouenaodeveria")){
            log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]<-paste0(log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]," [Faz parte de um MRG ou e 'outros', pode faltar]")
            if(!log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("entrouenaodeveria")){
              log_consistenia[nrow(log_consistenia),which(colnames(log_consistenia)=="resultado")]<-"OK"
            }
            bd=bd%>%dplyr::mutate(entrou=ifelse(entrou==0&base==1,1,entrou))
          }
        }
      }

      # #ordem
      # if(arrange==TRUE){
      #   x<-log_consistenia
      #   ordem<-seq(1,nrow(x),by=0.1)
      #   x$ordem<-NA
      #   for(i in 1:length(ordem)){
      #     x$ordem[which(
      #       x$var%>%stringr::str_remove_all("v")%>%stringr::str_replace_all("_",".")%>%as.numeric()==round(ordem[i],1)
      #     )]<-ordem[i]
      #   }
      #   log_consistenia<-x%>%dplyr::arrange(ordem)%>%dplyr::select(-ordem);rm(x)
      # }
      print(log_consistenia[nrow(log_consistenia),])
      #mostrar o banco de dados com os erros e vars que quero
      if(all(show!=FALSE)&log_consistenia[nrow(log_consistenia),4]!="OK"){
        message("--------------show-------------")
        print(bd%>%dplyr::select(dplyr::all_of(show),entrou,base)%>%dplyr::filter(entrou!=base)%>%data.frame())
      }
    }

  }
  return(log_consistenia)
  rm(log_consistenia,x,vars,regra,pode_falta,show,v,vars,var,nome,x_base_x,x_entrou_x,)%>%suppressWarnings()
}





