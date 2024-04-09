#' @title consistencia
#' @name consistencia
#'
#' @description A função foi desenvolvida para avaliar se todos os indivíduos
#' designados para responder à variável/MRG cumpriram com essa atribuição.
#'
#' @param log_consistenia Se a variável em questão for a primeira a ser testada,
#' 'log_consistenia' é definida como 'NULL'; no entanto,  se um arquivo no formato de
#' saída dessa função já existir,  'log_consistenia' será o seu data.frame correspondente
#' @param x Banco de dados
#' @param vars Variável ou,  no caso de um MRG,  vetor de variáveis
#' @param nome Se vars for um vetor -MRG,  este será o nome a ser atribuído a esse
#' conjunto de variáveis
#' @param regra Regra de entrada. Obs.: se a base for 100 porcento,  atribua o valor '100'
#' @param pode_falta 'TRUE' para indicar a possibilidade de respostas ausentes;
#' 'FALSE' para indicar a ausência de tolerância para respostas não fornecidas.
#' @param show Indicar as variáveis a serem impressas em caso de erro -serão
#' exibidas apenas as variáveis indicadas e as linhas com erros; 'FALSE' para não
#' imprimir as linhas,  mesmo na presença de erros.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import stringr
#'
#' @examples
#'
#'data=IPpackage::IPpackage_exemplo
#'
#'##Tudo OK
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = NULL,
#'  x = data,
#'  vars = "v1",
#'  regra = "100",
#'  pode_falta = FALSE,
#'  show = c("id", "v1")
#')
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = log_consistenia,
#'  x = data,
#'  vars = "v2",
#'  regra = "v1%nin%c(1)",
#'  pode_falta = FALSE,
#'  show = c("id", "v1", "v2")
#')
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = log_consistenia,
#'  x = data,
#'  vars = paste0("v3_", 1:3),
#'  regra = "v1%nin%c(1)",
#'  pode_falta = TRUE,
#'  show = c("id", "v1", "v2"),
#'  nome = "G003"
#'  )
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = log_consistenia,
#'  x = data,
#'  vars = c("v4",  "v5",  "v6",  "v7",  "v8",  "v9",  "v10_1",  "v10_2",  "v11"),
#'  regra = "v1%nin%c(1)",
#'  pode_falta = TRUE,
#'  show = c("id", "v1", "v2")
#' )
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = NULL,
#'  x = data,
#'  vars = "v1",
#'  regra = "100",
#'  pode_falta = FALSE,
#'  show = c("id", "v1", "v2")
#' )
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = log_consistenia,
#'  x = data,
#'  vars = "v2",
#'  regra = "v1%nin%c(1)",
#'  pode_falta = FALSE,
#'  show = c("id", "v1", "v2")
#' )
#'
#'log_consistenia = IPpackage::consistencia(
#'  log_consistenia = log_consistenia,
#'  x = data, vars = paste0("v3_", 1:3),
#'  regra = "v1%nin%c(1)",
#'  pode_falta = TRUE,
#'  show = c("id", "v1", paste0("v3_", 1:3)),
#'  nome = "G003"
#'  )
#'
#' @export
#'
#'

consistencia<-function(log_consistenia=NULL,x,vars,nome=NULL,regra,pode_falta=FALSE,show=FALSE){
  `%nin%` = base::Negate(`%in%`)
  if(base::is.null(log_consistenia)){log_consistenia=base::data.frame(var=NA,entrou=NA,base=NA,resultado=NA,descricao=NA,regra=NA)}
  #MRG
  {
    consistencia_mrg<-function(log_consistenia,x,vars,regra,nome,show=show){
      if(regra!="100"){
        bd=x%>%dplyr::mutate(base=base::ifelse(base::eval(base::parse(text=regra)),1,0),base=base::ifelse(base::is.na(base),0,base),
                             entrou=base::ifelse(base::eval(base::parse(text =base::paste0("!base::is.na(",vars,")",collapse = "|") )),1,0)
        )
        if(base::all(bd$entrou==bd$base)){
          log_consistenia<-base::rbind(log_consistenia,
                                       bd%>%dplyr::summarise(var=nome,entrou=base::sum(entrou),base=base::sum(base),resultado="OK",descricao="-",regra=regra)
          )
        }else{
          #entrou e nao deveria
          e_nd=bd%>%dplyr::filter(entrou%in%c(1)&base%in%c(0))%>%base::nrow()
          #nao entrou e deveria
          ne_d=bd%>%dplyr::filter(entrou%in%c(0)&base%in%c(1))%>%base::nrow()
          if(ne_d>0&e_nd==0){d=base::paste0(ne_d," nao entrou e deveria")}
          if(ne_d==0&e_nd>0){d=base::paste0(e_nd," entrou e nao deveria")}
          if(ne_d>00&e_nd>0){d=base::paste0(ne_d," nao entrou e deveria;",e_nd," entrou e nao deveria")}
          log_consistenia<-base::rbind(log_consistenia,
                                       bd%>%dplyr::summarise(var=nome,entrou=base::sum(entrou),base=base::sum(base),resultado="Erro",descricao=d,regra=regra)
          )
          rm(e_nd,ne_d,d)%>%base::suppressWarnings()
        }

      }else{
        bd=x%>%dplyr::mutate(base=1,
                             entrou=base::ifelse(base::eval(base::parse(text =base::paste0("!base::is.na(",vars,")",collapse = "|") )),1,0))
        #nao entrou e deveria
        ne_d=bd%>%dplyr::filter(entrou%in%c(0)&base%in%c(1))%>%base::nrow()
        if(ne_d>0){d=base::paste0(ne_d," nao entrou e deveria")}else{d="-"}

        log_consistenia<-base::rbind(log_consistenia,
                                     bd%>%dplyr::summarise(var=nome,entrou=base::sum(entrou),base=base::sum(base),resultado=base::ifelse(base::all(base==entrou),"OK","Erro"),descricao=d,regra=regra)
        )
        rm(ne_d,d)%>%base::suppressWarnings()

      }

      if(log_consistenia$resultado[base::nrow(log_consistenia)]=="Erro"){
        IPpackage::print_cor(log_consistenia[base::nrow(log_consistenia),],cor="vermelho",data.frame=TRUE)
      }else{
        IPpackage::print_cor(log_consistenia[base::nrow(log_consistenia),],cor="verde",data.frame=TRUE)
      }
      #mostrar o banco de dados com os erros e vars que quero
      if(base::all(show!=FALSE)&log_consistenia[base::nrow(log_consistenia),4]!="OK"){
        IPpackage::print_cor("--------------show--------------\n",cor="ciano",negrito=TRUE,data.frame=FALSE)

        base::print(bd%>%dplyr::select(dplyr::all_of(show),entrou,base)%>%dplyr::filter(entrou!=base)%>%base::data.frame())

      }
      return(log_consistenia%>%unique())
    }
    if(length(vars)>1){
      if(base::is.null(nome)){nome=base::paste0("MRG-",vars[1]);message(base::paste0("Como nome=NULL, atribuimos o nome '",nome,"'"))}
      p=base::paste0("==============MRG - ",nome,"==============\n")
      IPpackage::print_cor(p,cor="ciano",negrito=TRUE,data.frame=FALSE)

      log_consistenia<-consistencia_mrg(log_consistenia,x,vars,regra,nome,show)
      for(i in 1:base::nrow(log_consistenia)){
        if(base::all(base::is.na(log_consistenia[i,]))){log_consistenia<-log_consistenia[-i,]}
      }
      #Se é MRG, pode faltar nas vars isoladas
      pode_falta<-TRUE
    }

  }
  #Isoladas
  {
    for(v in 1:length(vars)){
      var=vars[v]
      p=base::paste0("==============Var - ",var,"==============\n")
      IPpackage::print_cor(p,cor="amarelo",negrito=TRUE,data.frame=FALSE)

      if(regra=="100"){
        entrou=base::sum(!base::is.na(x[[var]]))
        base=base::nrow(x)
        bd=x%>%dplyr::mutate(base=1,entrou=base::ifelse(!base::is.na(base::eval(base::parse(text =var))),1,0))
        if(entrou==base){resultado="OK";descricao="-"}else{
          resultado="Erro"
          descricao=base::paste0(x%>%dplyr::filter(base::eval(base::parse(text =base::paste0("base::is.na(",var,")"))))%>%base::nrow()," nao entrou e deveria")
        }
        log_consistenia<-base::rbind(log_consistenia,base::data.frame(var,entrou,base,resultado=resultado,descricao=descricao,regra=regra))
      }else{
        bd=x%>%dplyr::mutate(base=base::ifelse(base::eval(base::parse(text =regra)),1,0),entrou=base::ifelse(!base::is.na(base::eval(base::parse(text =var))),1,0))

        x$x_entrou_x=base::ifelse(!base::is.na(x[[var]]),1,0)
        x<-x%>%dplyr::mutate(x_base_x=base::ifelse(base::eval(base::parse(text =regra)),1,0));x$x_base_x<-base::ifelse(base::is.na(x$x_base_x),0,x$x_base_x)
        entrou=base::sum(x$x_entrou_x)
        base=base::sum(x$x_base_x)
        #resultado
        if(base::all(x$x_entrou_x==x$x_base_x)){
          resultado="OK";descricao="-"
        }else{
          xx<-x[which(x$x_entrou_x!=x$x_base_x),which(colnames(x)%in%c("x_entrou_x","x_base_x"))]
          #xx<-x%>%dplyr::filter(c(x$x_entrou_x)!=c(x$x_base_x))%>%dplyr::select(x_entrou_x,x_base_x)
          resultado="Erro"
          n_entrou_menos=paste(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%base::nrow(),"nao entrou e deveria")
          n_entrou_mais=paste(xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%base::nrow(),"entrou e nao deveria")

          if(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%base::nrow()!=0&xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%base::nrow()!=0){
            descricao=base::paste0(n_entrou_menos,";",n_entrou_mais)
          }else{
            if(xx%>%dplyr::filter(x_entrou_x==0&x_base_x==1)%>%base::nrow()!=0){descricao=n_entrou_menos}
            if(xx%>%dplyr::filter(x_entrou_x==1&x_base_x==0)%>%base::nrow()!=0){descricao=n_entrou_mais}
          }
        }
        log_consistenia<-base::rbind(log_consistenia,base::data.frame(var,entrou,base,resultado=resultado,descricao=descricao,regra=regra))
      }
      for(i in 1:base::nrow(log_consistenia)){
        if(base::all(base::is.na(log_consistenia[i,]))){log_consistenia<-log_consistenia[-i,]}
      }
      #Se puder faltar
      if(pode_falta==TRUE){
        if(log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="resultado")]=="Erro"){
          if(log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("naoentrouedeveria")){#&!log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("entrouenaodeveria")){
            log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]<-base::paste0(log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]," [pode faltar = TRUE]")
            if(!log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="descricao")]%>%stringr::str_remove_all(" ")%>%stringr::str_detect("entrouenaodeveria")){
              log_consistenia[base::nrow(log_consistenia),which(colnames(log_consistenia)=="resultado")]<-"OK"
            }
            bd=bd%>%dplyr::mutate(entrou=base::ifelse(entrou==0&base==1,1,entrou))
          }
        }
      }

      # #ordem
      # if(arrange==TRUE){
      #   x<-log_consistenia
      #   ordem<-seq(1,base::nrow(x),by=0.1)
      #   x$ordem<-NA
      #   for(i in 1:length(ordem)){
      #     x$ordem[which(
      #       x$var%>%stringr::str_remove_all("v")%>%stringr::str_replace_base::all("_",".")%>%base::as.numeric()==round(ordem[i],1)
      #     )]<-ordem[i]
      #   }
      #   log_consistenia<-x%>%dplyr::arrange(ordem)%>%dplyr::select(-ordem);rm(x)
      # }

      if(log_consistenia$resultado[base::nrow(log_consistenia)]=="Erro"){
        IPpackage::print_cor(log_consistenia[base::nrow(log_consistenia),],cor="vermelho",data.frame=TRUE)
      }else{
        IPpackage::print_cor(log_consistenia[base::nrow(log_consistenia),],cor="verde",data.frame=TRUE)
      }

      #mostrar o banco de dados com os erros e vars que quero
      if(base::all(show!=FALSE)&log_consistenia[base::nrow(log_consistenia),4]!="OK"){
        IPpackage::print_cor("--------------show--------------\n",cor="amarelo",negrito=TRUE,data.frame=FALSE)
        base::print(bd%>%dplyr::select(dplyr::all_of(show),entrou,base)%>%dplyr::filter(entrou!=base)%>%base::data.frame())
      }
    }

  }
  return(log_consistenia)
  rm(log_consistenia,x,vars,regra,pode_falta,show,v,vars,var,nome,x_base_x,x_entrou_x,i,resultado,entrou,descricao,base)%>%base::suppressWarnings()
}
