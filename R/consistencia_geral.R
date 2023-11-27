#' @title consistencia_geral
#' @name consistencia_geral
#'
#' @description Aprimoramos esse processo ao consolidar todas as informações
#' relevantes sobre a consistência em uma planilha do Excel, que posteriormente
#' utilizamos como referência para executar a verificação de consistência por meio
#' desta função.
#'
#' @param caminho caminho completo do Excel - de 'C:/' à '.xlsx'
#' @param banco_de_dados Banco de dados
#' @param DADOS_dicionario Dicionário com as colunas 'opcao_cod', 'opcao_label',
#' 'opcao_variavel' e 'pergunta_enunciado'
#' @param aba_principal Nome da aba/sheet que contém informações cruciais, como
#' regras, variáveis e nomes das variáveis
#' @param coluna_nome_variavel Nome da coluna em 'aba_principal' que contém o nome
#' das variáveis ou do MRG
#' @param coluna_variaveis Nome da coluna em 'aba_principal' que contém a variável
#' ou variáveis -em caso de MRG
#' @param coluna_pode_faltar Nome da coluna em 'aba_principal' que indica a
#' possibilidade de ausência de respondentes
#' @param coluna_regra Nome da coluna em 'aba_principal' que contém informações
#' sobre a regra da base
#' @param coluna_pode_faltar_label Nome da coluna em 'aba_principal' que indica a
#' possibilidade de ausência ou presença de labels
#' @param coluna_e_mrg_ou_nao Nome da coluna em 'aba_principal' que informa se se
#' trata ou não de um MRG
#' @param aba_abertas Nome da aba/sheet que contém variáveis abertas - 'NULL' caso
#' não queira analisar as abertas
#' @param coluna_aberta_id Nome da coluna em 'aba_abertas' que indica qual coluna
#' em 'banco_de_dados' possui o 'id' - 'NULL' caso não queira analisar as abertas
#' @param coluna_aberta_codificada Nome da coluna em 'aba_abertas' que contém
#' informações sobre a variável codificada - 'NULL' caso não queira analisar as abertas
#' @param coluna_aberta_original Nome da coluna em 'aba_abertas' que contém
#' informações sobre a variável original - 'NULL' caso não queira analisar as abertas
#' @param coluna_aberta_obs Nome da coluna em 'aba_abertas' que indica se deseja
#' ou não adicionar uma observação à consistência das variáveis abertas - 'NULL'
#' caso não queira analisar as abertas
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import stringr
#' @import openxlsx
#'
#' @examples
#'
#' # log_consistenia=IPpackage::consistencia_geral(
#' #   caminho="C:/i/Innovare Pesquisa/Projetos - Documentos/2023.009.00 - CAIXA
#' #IMAGEM 2023/Estatística/Processamentos/Auxílio finalização/2023.009.02 - Caixa
#' #Imagem 2023 - Labels.xlsx"
#' #   ,banco_de_dados=base::readRDS(file = "C:/SSD Externo/Work/InnovarePesquisa/
#' #Projects/43---2023.009.00 - CAIXA IMAGEM 2023/00.Drive/Banco de dados/02.final.rds")
#' #   ,DADOS_dicionario=openxlsx::read.xlsx("C:/SSD Externo/Work/InnovarePesquisa/
#' #Projects/43---2023.009.00 - CAIXA IMAGEM 2023/Output/caixa_imagem_2023_Dicionário.xlsx")
#' #   ,aba_principal="Labels perguntas"
#' #   ,coluna_nome_variavel="Variável"
#' #   ,coluna_variaveis="vars"
#' #   ,coluna_pode_faltar="pd_falt_cons"
#' #   ,coluna_regra="C_regra"
#' #   ,coluna_pode_faltar_label="pd_falt_labl"
#' #   ,coluna_e_mrg_ou_nao="obs"
#' #   ,aba_abertas=NULL
#' #   ,coluna_aberta_id=NULL
#' #   ,coluna_aberta_codificada=NULL
#' #   ,coluna_aberta_original=NULL
#' #   ,coluna_aberta_obs=NULL
#' # )
#'
#' @export
#'
#'

consistencia_geral=function(
    caminho,
    banco_de_dados,
    DADOS_dicionario,
    aba_principal,
    coluna_nome_variavel,
    coluna_variaveis,
    coluna_pode_faltar,
    coluna_regra,
    coluna_pode_faltar_label,
    coluna_e_mrg_ou_nao,
    aba_abertas=NULL,
    coluna_aberta_id=NULL,
    coluna_aberta_codificada=NULL,
    coluna_aberta_original=NULL,
    coluna_aberta_obs=NULL
){
  #Consistência de 'entrou/ não entrou'
  {
    label=openxlsx::read.xlsx(caminho,sheet=aba_principal)
    if(base::any(base::colnames(label)==coluna_nome_variavel)){base::colnames(label)[base::colnames(label)==coluna_nome_variavel]<-"coluna_nome_variavel"}
    if(base::any(base::colnames(label)==coluna_variaveis)){base::colnames(label)[base::colnames(label)==coluna_variaveis]<-"coluna_variaveis"}
    if(base::any(base::colnames(label)==coluna_pode_faltar)){base::colnames(label)[base::colnames(label)==coluna_pode_faltar]<-"coluna_pode_faltar"}
    if(base::any(base::colnames(label)==coluna_regra)){base::colnames(label)[base::colnames(label)==coluna_regra]<-"coluna_regra"}

    if(base::any(base::colnames(label)==coluna_pode_faltar_label)){base::colnames(label)[base::colnames(label)==coluna_pode_faltar_label]<-"coluna_pode_faltar_label"}
    if(base::any(base::colnames(label)==coluna_e_mrg_ou_nao)){base::colnames(label)[base::colnames(label)==coluna_e_mrg_ou_nao]<-"coluna_e_mrg_ou_nao"}
    label=label%>%dplyr::select(coluna_nome_variavel,coluna_variaveis,coluna_pode_faltar,coluna_regra,coluna_pode_faltar_label,coluna_e_mrg_ou_nao)


    all_vars=label$coluna_nome_variavel%>%unique()
    all_vars=all_vars[!base::is.na(all_vars)]
    #i=base::which(all_vars=="v7mrg")

    log_consistenia=base::data.frame(var=NA,entrou=NA,base=NA,resultado=NA,descricao=NA,regra=NA)
    log_consistenia_label=base::data.frame(var=NA,resultado=NA,descricao=NA)
    for(i in 1:base::length(all_vars)){
      #for(i in 543:base::length(all_vars)){
      nome_var=all_vars[i]
      vars=base::unlist(base::strsplit(label%>%dplyr::filter(`coluna_nome_variavel`==nome_var)%>%dplyr::select("coluna_variaveis")%>%dplyr::pull(),","))%>%stringr::str_trim()
      if(any(vars%in%c(""))){
        vars=vars[-which(vars%in%c(""))]
      }
      regra=label%>%dplyr::filter(coluna_nome_variavel==nome_var)%>%dplyr::select("coluna_regra")%>%dplyr::pull()
      if(!base::is.na(regra)){
        if(regra%>%stringr::str_detect("&amp;")){
          regra=regra%>%stringr::str_replace_all("&amp;","&")
        }
      }
      falta_resp=label%>%dplyr::filter(coluna_nome_variavel==nome_var)%>%dplyr::select("coluna_pode_faltar")%>%dplyr::pull()

      var_mrg_nome=label%>%dplyr::filter(coluna_nome_variavel==nome_var)%>%dplyr::select("coluna_e_mrg_ou_nao")%>%dplyr::pull()%>%stringr::str_detect("mrg")
      if((var_mrg_nome==FALSE)&(base::length(vars)>1)){
        #if(nome_var%>%stringr::str_detect('imrg|mmrg')){
        var_mrg_nome=TRUE
        #}
      }
      if(var_mrg_nome==TRUE){var_mrg_nome=nome_var}else{var_mrg_nome=NA}
      mrg_citou=label%>%dplyr::filter(coluna_nome_variavel==nome_var)%>%dplyr::select("coluna_e_mrg_ou_nao")%>%dplyr::pull()%>%stringr::str_detect("mrg_citou")
      pode_falta=label%>%dplyr::filter(coluna_nome_variavel==nome_var)%>%dplyr::select("coluna_pode_faltar_label")%>%dplyr::pull()

      #consistência das variáveis
      if(base::any(base::is.na(vars))|base::is.na(regra)|base::is.na(falta_resp)){
        x=base::data.frame(var=nome_var,entrou=NA,base=NA,resultado=NA,descricao=NA,regra=regra)
      }else{
        x=IPpackage::consistencia(
          log_consistenia=NULL,
          x=banco_de_dados,
          vars=vars,
          nome=nome_var,
          regra=regra,
          pode_falta = falta_resp,
          show = FALSE
        )%>%base::suppressWarnings()
        x=x[1,]
      }
      if(all(base::is.na(log_consistenia))){log_consistenia=x}else{log_consistenia=base::rbind(log_consistenia,x)}
      base::rm(x)
      log_consistenia=log_consistenia%>%unique()
      #consistência dos labels
      {
        if(base::any(base::is.na(vars))|base::is.na(regra)|base::is.na(falta_resp)){
          y=base::data.frame(var=nome_var,resultado=NA,descricao=NA)
        }else{
          y<-IPpackage::consistencia_label(
            log_consistenia=NULL,
            xdf=banco_de_dados,
            xdicionario=DADOS_dicionario,
            vars=vars,
            var_mrg_nome=var_mrg_nome,
            mrg_citou=mrg_citou,
            pode_falta = pode_falta,
            show = FALSE)%>%base::data.frame()%>%base::suppressWarnings()
          y<-y[1,]
        }

      }
      if(all(base::is.na(log_consistenia_label))){log_consistenia_label=y}else{log_consistenia_label=base::rbind(log_consistenia_label,y)}
      base::rm(y)
      log_consistenia_label=log_consistenia_label%>%unique()

    }


  }

  #Abertas
  {
    if(base::is.null(aba_abertas)|base::is.null(coluna_aberta_id)|base::is.null(coluna_aberta_codificada)|base::is.null(coluna_aberta_original)|base::is.null(coluna_aberta_obs)){
      log_consistencia_aberta=base::data.frame(var_codificada="-",var_orig="-",resultado="-",descricao="-")
    }else{
      questao_aberta=openxlsx::read.xlsx(caminho,sheet=aba_abertas)
      if(base::any(base::colnames(questao_aberta)==coluna_aberta_id)){base::colnames(questao_aberta)[base::colnames(questao_aberta)==coluna_aberta_id]<-"coluna_aberta_id"}
      if(base::any(base::colnames(questao_aberta)==coluna_aberta_codificada)){base::colnames(questao_aberta)[base::colnames(questao_aberta)==coluna_aberta_codificada]<-"coluna_aberta_codificada"}
      if(base::any(base::colnames(questao_aberta)==coluna_aberta_original)){base::colnames(questao_aberta)[base::colnames(questao_aberta)==coluna_aberta_original]<-"coluna_aberta_original"}
      if(base::any(base::colnames(questao_aberta)==coluna_aberta_obs)){base::colnames(questao_aberta)[base::colnames(questao_aberta)==coluna_aberta_obs]<-"coluna_aberta_obs"}

      log_consistencia_aberta = base::data.frame(var_codificada = NA,var_orig=NA ,resultado = NA,descricao = NA)
      i=base::which(questao_aberta$coluna_aberta_codificada=="v15")
      for(i in 1:base::length(questao_aberta$coluna_aberta_codificada)){
        log_consistencia_aberta<-IPpackage::consistencia_abertas(
          log_consistencia_aberta=log_consistencia_aberta,
          xdf=banco_de_dados,
          var_id=questao_aberta$coluna_aberta_id[i],
          var_codificada=questao_aberta$coluna_aberta_codificada[i],
          var_orig=questao_aberta$coluna_aberta_original[i],
          show=FALSE,
          obs=questao_aberta$coluna_aberta_obs[i])%>%base::suppressWarnings()

      }
    }

  }
  #Juntando abertas com a consistencia
  {
    y=log_consistencia_aberta
    base::colnames(y)=base::paste0(base::colnames(y),"_aberta")
    x=dplyr::left_join(
      log_consistenia%>%dplyr::mutate(var=base::as.character(var))
      ,y%>%dplyr::mutate(var_codificada_aberta=base::as.character(var_codificada_aberta))
      ,by=c("var"="var_codificada_aberta")
    )%>%
      dplyr::mutate(
        dplyr::across(.cols=base::colnames(y)[-base::which(base::colnames(y)=="var_codificada_aberta")],~base::ifelse(base::is.na(.),"-",.))
      )%>%unique()%>%base::suppressWarnings()
    log_consistenia=x
    base::rm(x,y)
  }
  #Juntando label com a consistencia
  {
    y=log_consistenia_label
    base::colnames(y)=base::paste0(base::colnames(y),"_label")
    x=dplyr::left_join(
      log_consistenia%>%dplyr::mutate(var=base::as.character(var))
      ,y%>%dplyr::mutate(var_label=base::as.character(var_label))
      ,by=c("var"="var_label")
    )%>%
      dplyr::mutate(
        dplyr::across(.cols=base::colnames(y)[-base::which(base::colnames(y)=="var_label")],~base::ifelse(base::is.na(.),"-",.))
      )%>%unique()%>%base::suppressWarnings()
    log_consistenia=x
    base::rm(x,y)
  }
  return(log_consistenia)
}
