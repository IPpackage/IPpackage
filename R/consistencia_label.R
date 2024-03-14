#' @title consistencia_label
#' @name consistencia_label
#'
#' @description A função foi desenvolvida para avaliar se todas as variáveis possuem
#'  Label e Enunciado no Dicionário.
#'
#' @param log_consistenia_label Se a variável em questão for a primeira a ser testada,
#' 'log_consistenia_label' é definida como 'NULL'; no entanto, se um arquivo no formato de
#' saída dessa função já existir, 'log_consistenia_label' será o seu data.frame correspondente
#' @param xdf Banco de dados
#' @param xdicionario Dicionário com as colunas 'opcao_cod', 'opcao_label',
#' 'opcao_variavel', 'pergunta_enunciado'
#' @param vars Variável ou, no caso de um MRG, vetor de variáveis
#' @param var_mrg_nome 'NA' caso não se trate de um MRG; caso contrário, nome do MRG
#' @param mrg_citou 'TRUE' para indicar se tratar de MRG do tipo Citou;
#' caso contrário, 'FALSE'.
#' @param pode_falta 'TRUE' para indicar a possibilidade de labels ausentes;
#' 'FALSE' para indicar a ausência de tolerância para labels não fornecidas.
#'
#' @param show 'TRUE' para imprimir a frequência com Label e Enunciado;
#' 'FALSE', caso contrário.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import tidyr
#'
#' @examples
#'
#' #Sem exemplo
#' @export
#'
#'
consistencia_label=function(log_consistenia_label=NULL,xdf,xdicionario,vars,var_mrg_nome=NA,mrg_citou=FALSE,pode_falta = FALSE, show = FALSE){

  if (base::is.null(log_consistenia_label)) {
    log_consistenia_label = base::data.frame(var = NA, resultado = NA,descricao = NA)
  }

  if (base::length(vars) > 1) {
    if (base::is.na(var_mrg_nome)) {
      nome_usar = base::paste0("MRG-", vars[1])
      message(base::paste0("Como nome=NULL, atribuimos o nome '",
                           nome, "'"))
    }else{
      nome_usar=var_mrg_nome
    }
  }else{
    nome_usar=vars
  }

  rodar=base::lapply(vars, base::list)
  rodar=stats::setNames(rodar, vars)
  if(base::length(vars)>1){
    rodar[[base::length(rodar)+1]]<-base::list(vars)
    names(rodar)[base::length(rodar)]=nome_usar
    rodar <- c(utils::tail(rodar, 1), utils::head(rodar, -1))
  }
  # if(mrg_citou==TRUE){
  #   rodar<-rodar[1]
  # }
  for(i in 1:base::length(rodar)){
    nome=base::names(rodar)[i]
    variaveis=rodar[[i]][[1]]
    if(mrg_citou==TRUE){
      fim=dplyr::left_join(
        xdf%>%dplyr::select(dplyr::all_of(variaveis))%>%
          tidyr::pivot_longer(everything())%>%
          dplyr::mutate(name=base::as.character(name),value=base::as.character(value))
        ,
        xdicionario%>%dplyr::filter(opcao_variavel%in%c(variaveis))%>%
          dplyr::mutate(opcao_variavel=base::as.character(opcao_variavel),opcao_cod=base::as.character(opcao_cod))
        ,
        by=c("name"="opcao_variavel","value"="opcao_cod")
      )%>%
        dplyr::group_by(name)%>%
        dplyr::count(value,opcao_label,pergunta_enunciado)%>%
        dplyr::mutate(mrg_nome=nome)%>%

        dplyr::left_join(
          xdicionario%>%dplyr::filter(opcao_variavel%in%c(names(rodar)[i]))%>%
            dplyr::mutate(opcao_variavel=base::as.character(opcao_variavel),opcao_cod=base::as.character(opcao_cod))%>%
            dplyr::select(opcao_variavel,pergunta_enunciado)%>%
            dplyr::rename(pergunta_enunciado_mrg=pergunta_enunciado)
          ,
          by=c('mrg_nome'='opcao_variavel')
        )%>%base::suppressWarnings()

      x=fim%>%
        dplyr::filter((base::is.na(opcao_label)|base::is.na(pergunta_enunciado)|base::is.na(pergunta_enunciado_mrg))&!base::is.na(value))%>%base::suppressWarnings()
    }
    if(mrg_citou==FALSE){
      procurar=nome

      fim=xdf%>%dplyr::select(dplyr::all_of(variaveis))%>%
        tidyr::pivot_longer(everything())%>%
        dplyr::count(value)%>%
        dplyr::mutate(name=procurar)%>%
        dplyr::relocate(name,.before = value)%>%
        dplyr::mutate(name=base::as.character(name),value=base::as.character(value))%>%{
          freq=tibble::tibble(.)
          dic_filtro=xdicionario%>%dplyr::filter(opcao_variavel%in%c(procurar))%>%
            dplyr::mutate(opcao_variavel=base::as.character(opcao_variavel),opcao_cod=base::as.character(opcao_cod))
          if(base::any(is.na(dic_filtro%>%dplyr::select(opcao_label)))&dic_filtro%>%dplyr::select(pergunta_enunciado,opcao_variavel)%>%dplyr::select(pergunta_enunciado)%>%unique()%>%nrow()==1){
            freq=freq%>%
              dplyr::left_join(
                dic_filtro%>%dplyr::select(opcao_cod,opcao_label,opcao_variavel),
                by=c("name"="opcao_variavel","value"="opcao_cod")
              )%>%
              dplyr::left_join(
                dic_filtro%>%dplyr::select(pergunta_enunciado,opcao_variavel)%>%unique(),
                by=c("name"="opcao_variavel")
              )%>%base::suppressWarnings()

          }else{
            freq=freq%>%
              dplyr::left_join(
                dic_filtro,
                by=c("name"="opcao_variavel","value"="opcao_cod")
              )%>%base::suppressWarnings()
          }

          freq
        }

      x=fim%>%
        dplyr::filter((base::is.na(opcao_label)|base::is.na(pergunta_enunciado))&!base::is.na(value))%>%base::suppressWarnings()
    }

    if(base::nrow(x)>0){
      if(pode_falta==FALSE){
        mensagem=base::cat(base::paste0("\033[1;31m[",nome,"] var x label ERRO (falta label)\033[0m\n"))
        resultado="error"
        descricao="-"
        cor="vermelho"
      }else{
        mensagem=base::cat(base::paste0("\033[1;33m[",nome,"] var x label OK (faltou mas pode faltar)\033[0m\n"))
        resultado="ok"
        descricao="Faltou label [pode faltar = TRUE]"
        cor="amarelo"
      }
    }else{
      mensagem=base::cat(base::paste0("\033[1;32m[",nome,"] var x label OK\033[0m\n"))
      resultado="ok"
      descricao="-"
      cor="verde"
    }

    log_consistenia_label <- base::rbind(log_consistenia_label, base::data.frame(var = nome, resultado = resultado,descricao = descricao))
    log_consistenia_label=log_consistenia_label%>%tidyr::drop_na()
    if(show==TRUE){
      if(resultado!="error"){
        base::cat("\033[3m labels\033[0m\n")
        IPpackage::print_cor(fim%>%unique(),cor)
      }
      if(resultado=="error"){
        base::cat("\033[3m erros\033[0m\n")
        IPpackage::print_cor(x%>%unique(),cor)
      }
    }
  }

  return(log_consistenia_label)
}
