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

consistencia_geral = function (
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
    aba_abertas = NULL,
    coluna_aberta_id = NULL,
    coluna_aberta_codificada = NULL,
    coluna_aberta_original = NULL,
    coluna_aberta_obs = NULL
)
{
  {# Start: base (entrou/não entrou) e se falta Label

    {# Start: Renomeando colunas do arquivo de label (padronizando)

      label = openxlsx::read.xlsx(caminho, sheet = aba_principal)

      if ( base::any(base::colnames(label) == coluna_nome_variavel) )
      {
<<<<<<< HEAD

        base::colnames(label)[base::colnames(label) == coluna_nome_variavel] <- "coluna_nome_variavel"

      }

      if ( base::any(base::colnames(label) == coluna_variaveis) )
=======
        base::colnames(label)[base::colnames(label) == coluna_nome_variavel] <- "coluna_nome_variavel"
      }

      if (base::any(base::colnames(label) == coluna_variaveis))
>>>>>>> Felipe-Teste
      {

        base::colnames(label)[base::colnames(label) == coluna_variaveis] <- "coluna_variaveis"

      }

<<<<<<< HEAD
      if ( base::any(base::colnames(label) == coluna_pode_faltar) )
=======
      if (base::any(base::colnames(label) == coluna_pode_faltar))
>>>>>>> Felipe-Teste
      {

        base::colnames(label)[base::colnames(label) == coluna_pode_faltar] <- "coluna_pode_faltar"

      }

<<<<<<< HEAD
      if ( base::any(base::colnames(label) == coluna_regra) )
=======
      if (base::any(base::colnames(label) == coluna_regra))
>>>>>>> Felipe-Teste
      {

        base::colnames(label)[base::colnames(label) == coluna_regra] <- "coluna_regra"

      }

<<<<<<< HEAD
      if ( base::any(base::colnames(label) == coluna_pode_faltar_label) )
=======
      if (base::any(base::colnames(label) == coluna_pode_faltar_label))
>>>>>>> Felipe-Teste
      {

        base::colnames(label)[base::colnames(label) == coluna_pode_faltar_label] <- "coluna_pode_faltar_label"

      }

<<<<<<< HEAD
      if ( base::any(base::colnames(label) == coluna_e_mrg_ou_nao) )
=======
      if (base::any(base::colnames(label) == coluna_e_mrg_ou_nao))
>>>>>>> Felipe-Teste
      {

        base::colnames(label)[base::colnames(label) == coluna_e_mrg_ou_nao] <- "coluna_e_mrg_ou_nao"

      }

      #Seleciono as colunas que quero e na ordem que quero
      label = label %>%
        dplyr::select(
          coluna_nome_variavel,
          coluna_variaveis,
          coluna_pode_faltar,
          coluna_regra,
          coluna_pode_faltar_label,
          coluna_e_mrg_ou_nao
        )

    }# End: Renomeando colunas do arquivo de label (padronizando)

    {# Start: Pegando todas as variáveis que irie rodar

      all_vars = label$coluna_nome_variavel %>% unique()
      all_vars = all_vars[!base::is.na(all_vars)]

    }# End: Pegando todas as variáveis que irie rodar

    {# Start: Criando data.frame com os resultado

      #Arquivo com a verificação das bases (entrou/não entrou)
      log_consistenia = base::data.frame(
        var = NA,
        entrou = NA,
        base = NA,
        resultado = NA,
        descricao = NA,
        regra = NA
      )

      #Arquivo com a verificação dos labels (se falta label)
      log_consistenia_label = base::data.frame(
        var = NA,
        resultado = NA,
        descricao = NA
      )

    }# End: Criando data.frame com os resultado

<<<<<<< HEAD
    for ( i in 1:base::length(all_vars) ) {# Start: rodando para cada uma das variáveis
=======
    for (i in 1:base::length(all_vars)) {# Start: rodando para cada uma das variáveis
>>>>>>> Felipe-Teste

      {# Start: Informações sobre o que estou rodando

        #nome da variávei
        nome_var = all_vars[i]

        {# Start: Variável/variáveis

          vars = base::unlist(
            base::strsplit(
              label %>%
                dplyr::filter( coluna_nome_variavel == nome_var ) %>%
                dplyr::select("coluna_variaveis") %>%
                dplyr::pull()
              ,","
            )
          )%>%
            stringr::str_trim()

          if ( any(vars %in% c("")) )
          {# Start: Removendo variável vazia

            vars = vars[-which(vars %in% c(""))]

          }# End: Removendo variável vazia

        }# End: Variável/variáveis

        {# Start: Regra da base

          regra = label %>%
            dplyr::filter(coluna_nome_variavel == nome_var) %>%
            dplyr::select("coluna_regra") %>%
            dplyr::pull()


          if ( !base::is.na(regra) )
          {# Start: Se não for NA

            if ( regra %>% stringr::str_detect("&amp;") )
            {# Start: Se Excel trouxe "&amp" ao invés de só "&"

              regra = regra %>%
                stringr::str_replace_all("&amp;","&")

            }# Start: Se Excel trouxe "&amp" ao invés de só "&"

          }# End: Se não for NA

        }# End: Regra da base

        #Pode faltar resposta (base)?
        falta_resp = label %>%
          dplyr::filter(coluna_nome_variavel == nome_var) %>%
          dplyr::select("coluna_pode_faltar") %>%
          dplyr::pull()

        {# Start: É um MRG?

          var_mrg_nome = label %>%
            dplyr::filter(coluna_nome_variavel == nome_var) %>%
            dplyr::select("coluna_e_mrg_ou_nao") %>%
            dplyr::pull() %>%
            stringr::str_detect("mrg")
          e_mrg=var_mrg_nome

          if ( (var_mrg_nome == FALSE) & (base::length(vars) > 1) )
          {# Start: Se a pessoa não informou que é MRG mas tem > de 1 var

            #Vira MRG (forçar)
            var_mrg_nome = TRUE

          }# End: Se a pessoa não informou que é MRG mas tem > de 1 var

          if (var_mrg_nome == TRUE)
          {# Start: Se for MRG, pego seu nome

            e_mrg=TRUE
            var_mrg_nome = nome_var

          } else {

            var_mrg_nome = NA

          }# End: Se for MRG, pego seu nome

          }# End: É um MRG?

        # É um MRG citou?
        mrg_citou = label %>%
          dplyr::filter( coluna_nome_variavel == nome_var ) %>%
          dplyr::select("coluna_e_mrg_ou_nao") %>%
          dplyr::pull() %>%
          stringr::str_detect("mrg_citou")
        e_mrg_citou=mrg_citou

        # Pode faltar label?
        pode_falta = label %>%
          dplyr::filter(coluna_nome_variavel == nome_var) %>%
          dplyr::select("coluna_pode_faltar_label") %>%
          dplyr::pull()

      }# End: Informações sobre o que estou rodando

      {# Start: consistência das variáveis

        if( base::any(base::is.na(vars))|
            base::is.na(regra)|
            base::is.na(falta_resp) )
        {# Start: Se não tenho condições de rodar

          x = base::data.frame(
            var = nome_var,
            entrou = NA,
            base = NA,
            resultado = NA,
            descricao = NA,
            regra = regra
          )

        }# End: Se não tenho condições de rodar
        else
        {# Start: Calculando a consistência da base

          x = IPpackage::consistencia(
            log_consistenia = NULL,
            x = banco_de_dados,
            vars = vars,
            nome = nome_var,
            regra = regra,
            pode_falta = falta_resp,
            show = FALSE
          ) %>%
            base::suppressWarnings()

          # Pego apenas a primeira linha
          x=x[1,]

          if( (e_mrg == TRUE | e_mrg_citou == TRUE) &
              x$resultado == "Erro" &
              x$entrou < x$base &
              falta_resp == TRUE
          )
          {# Start: Se for MRG, faltou e coluna_pode_faltar=TRUE
            x = x %>%
              dplyr::mutate(
                resultado = "OK"
                ,descricao = paste0(descricao, " [pode faltar = TRUE]")
              )
          }# End: Se for MRG, faltou e coluna_pode_faltar=TRUE

        }# End: Calculando a consistência da base

        if( all(base::is.na(log_consistenia)) )
        {# Start: armazenando o resultado no data.frame 'log_consistenia'

          log_consistenia = x

        }else{

          log_consistenia = base::rbind(log_consistenia,x)

        }# End: armazenando o resultado no data.frame 'log_consistenia'

        # Limpando
        base::rm(x)
        log_consistenia = log_consistenia %>%
          unique()


      }# End: consistência das variáveis

      {# Start: consistência dos labels
        if( base::any(base::is.na(vars))|
            base::is.na(regra)|
<<<<<<< HEAD
            base::is.na(falta_resp)
        )
=======
            base::is.na(falta_resp) )
>>>>>>> Felipe-Teste
        {# Start: Se não tenho condições de rodar

          y = base::data.frame(
            var = nome_var,
            resultado = NA,
            descricao = NA
          )

        }# End: Se não tenho condições de rodar
        else
        {# Start: Verificando Labels

          y <- IPpackage::consistencia_label(
            log_consistenia = NULL,
            xdf = banco_de_dados,
            xdicionario = DADOS_dicionario,
            vars = vars,
            var_mrg_nome = var_mrg_nome,
            mrg_citou = mrg_citou,
            pode_falta = pode_falta,
            show = FALSE) %>%
            base::data.frame() %>%
            base::suppressWarnings()

          # Pego apenas a primeira linha
          y<-y[1,]

        }# End: Verificando Labels

        if( all(base::is.na(log_consistenia_label)) )
        {# Start: armazenando o resultado no data.frame 'log_consistenia_label'

          log_consistenia_label = y

        }else{

          log_consistenia_label = base::rbind(log_consistenia_label,y)

        }# End: armazenando o resultado no data.frame 'log_consistenia_label'

        # Limpando

        base::rm(y)
        log_consistenia_label = log_consistenia_label %>%
          unique()


      }# End: consistência dos labels

    }# End: rodando para cada uma das variáveis

  }# End: base (entrou/não entrou) e se falta Label

  {# Start: Abertas
    if( base::is.null(aba_abertas)|
        base::is.null(coluna_aberta_id)|
        base::is.null(coluna_aberta_codificada)|
        base::is.null(coluna_aberta_original)|
        base::is.null(coluna_aberta_obs)
    )
    {# Start: Se não tenho condições de rodar

      log_consistencia_aberta = base::data.frame(
        var_codificada = "-",
        var_orig = "-",
        resultado = "-",
        descricao = "-"
      )

    }# End: Se não tenho condições de rodar
    else
    {# Start: Verificando as abertas

      questao_aberta = openxlsx::read.xlsx(caminho,sheet = aba_abertas)
      {# Start: Renomeando colunas do arquivo de abertas (padronizando)

        if( base::any(base::colnames(questao_aberta) == coluna_aberta_id) )
        {

          base::colnames(questao_aberta)[base::colnames(questao_aberta) == coluna_aberta_id] <- "coluna_aberta_id"

        }

<<<<<<< HEAD
        if( base::any(base::colnames(questao_aberta) == coluna_aberta_codificada) )
=======
        if(base::any(base::colnames(questao_aberta) == coluna_aberta_codificada))
>>>>>>> Felipe-Teste
        {

          base::colnames(questao_aberta)[base::colnames(questao_aberta) == coluna_aberta_codificada] <- "coluna_aberta_codificada"

        }

<<<<<<< HEAD
        if( base::any(base::colnames(questao_aberta) == coluna_aberta_original) )
=======
        if(base::any(base::colnames(questao_aberta) == coluna_aberta_original))
>>>>>>> Felipe-Teste
        {

          base::colnames(questao_aberta)[base::colnames(questao_aberta) == coluna_aberta_original] <- "coluna_aberta_original"

        }

<<<<<<< HEAD
        if( base::any(base::colnames(questao_aberta) == coluna_aberta_obs) )
=======
        if(base::any(base::colnames(questao_aberta) == coluna_aberta_obs))
>>>>>>> Felipe-Teste
        {

          base::colnames(questao_aberta)[base::colnames(questao_aberta) == coluna_aberta_obs] <- "coluna_aberta_obs"

        }


      }# End: Renomeando colunas do arquivo de abertas (padronizando)

      {# Start: Rodando as abertas

        #Data.frame pro resultado
        log_consistencia_aberta = base::data.frame(
          var_codificada = NA,
          var_orig = NA ,
          resultado = NA,
          descricao = NA
        )

        for( i in 1:base::length(questao_aberta$coluna_aberta_codificada) )
        {# Start: Rodando cada uma das variáveis

          log_consistencia_aberta <- IPpackage::consistencia_abertas(
            log_consistencia_aberta = log_consistencia_aberta,
            xdf = banco_de_dados,
            var_id = questao_aberta$coluna_aberta_id[i],
            var_codificada = questao_aberta$coluna_aberta_codificada[i],
            var_orig = questao_aberta$coluna_aberta_original[i],
            show = FALSE,
            obs = questao_aberta$coluna_aberta_obs[i]) %>%
            base::suppressWarnings()

        }# End: Rodando cada uma das variáveis

      }# End: Rodando as abertas

    }# End: Verificando as abertas

  }# End: Abertas

  {# Start: Juntando abertas com a consistencia

    y = log_consistencia_aberta

    base::colnames(y) = base::paste0(base::colnames(y), "_aberta")

    x = dplyr::left_join(
      log_consistenia %>%
        dplyr::mutate(var = base::as.character(var))
      ,y %>%
        dplyr::mutate(var_codificada_aberta = base::as.character(var_codificada_aberta))
      ,by = c("var" = "var_codificada_aberta")
    ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = base::colnames(y)[-base::which(base::colnames(y) == "var_codificada_aberta")]
          ,~base::ifelse(base::is.na(.), "-", .)
        )
      ) %>%
      unique() %>%
      base::suppressWarnings()

    log_consistenia = x
    base::rm(x,y)

  }# End: Juntando abertas com a consistencia

  {# Start: Juntando label com a consistencia

    y = log_consistenia_label

    base::colnames(y) = base::paste0(base::colnames(y),"_label")

    x = dplyr::left_join(
      log_consistenia %>%
        dplyr::mutate(var = base::as.character(var))
      ,y %>%
        dplyr::mutate(var_label = base::as.character(var_label))
      ,by = c("var" = "var_label")
    ) %>%
      dplyr::mutate(
        dplyr::across(
          .cols = base::colnames(y)[-base::which(base::colnames(y) == "var_label")]
          ,~base::ifelse(base::is.na(.), "-", .)
        )
      ) %>%
      unique() %>%
      base::suppressWarnings()

    log_consistenia = x
    base::rm(x,y)

  }# End: Juntando label com a consistencia

  #Retornando o Resultado
  return(log_consistenia)
}
