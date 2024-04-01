#' @title PosProce_meanofmean
#' @name PosProce_meanofmean
#'
#' @description A função tem a finalidade de recalcular a coluna 'media' e
#' 'media_peso' do processamento. A abordagem utilizada implica em calcular a
#' média das variáveis especificadas no parâmetro 'variaveis_recalcular'
#' (uma lista) e substituir esses valores nos dados contidos no arquivo
#' referenciado pelo parâmetro 'arquivo_medias'. Este arquivo deve ser originado
#' do processamento.
#'
#' @param arquivo_medias Aba 'Medias' ou 'Cruz_M' do processamento.
#' @param variaveis_recalcular Lista das variáveis a serem recalculadas,
#' acompanhadas de suas respectivas variáveis de composição (que serão incluídas
#'  no cálculo da média).
#' @param tipo Indicar a origem do 'arquivo_medias' (tipo = "Medias" ou tipo = "Cruz_M").
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @importFrom purrr imap
#'
#' @examples
#'
#'library(dplyr)
#'library(IPpackage)
#'
#'# Data.frame com resultado das médias em um processamento
#'Medias <- data.frame(
#'  splits = rep(1:2, each = 6)
#'  ,variavel = rep(c("v29imrg","v29i", "v30i", "v31imrg", "v31i", "v32i"),
#'   times = 2)
#'  ,media = c(0, 71.02880658, 71.79012346, 0, 54.49834163, 53.64318567, 0,
#'   70.20933977, 73.51046699, 0, 57.48792271, 52.91311014)
#'  ,media_peso = c(0, 71.02880658, 71.79012346, 0, 54.49834163, 53.64318567,
#'   0, 70.20933977, 73.51046699, 0, 57.48792271, 52.91311014)
#'  ,n_base = rep(c(100, 38), each = 6)
#'  ,n_base_peso = rep(c(100, 38), each = 6)
#'  ,desvp = c(NA, 23.70668439, 23.40351373, NA, 29.08172754, 28.08884382, NA,
#'   21.18357424, 19.23669407, NA, 27.0142608, 24.55376985)
#'  ,desvp_peso = c(NA, 23.70668439, 23.40351373, NA, 29.08172754, 28.08884382,
#'   NA, 21.18357424, 19.23669407, NA, 27.0142608, 24.55376985)
#'  ,erro = c(NA, 1.018289287, 1.005266992, NA, 1.249167159, 1.206519152, NA,
#'   1.803265592, 1.637536146, NA, 2.299606593, 2.090155694)
#'  ,erro_peso = c(NA, 1.018289287, 1.005266992, NA, 1.249167159, 1.206519152,
#'   NA, 1.803265592, 1.637536146, NA, 2.299606593, 2.090155694)
#'  ,min = rep(0, times = 12)
#'  ,Q1 = c(NA, 55.55555556, 55.55555556, NA, 33.33333333, 44.44444444, NA,
#'   55.55555556, 66.66666667, NA, 44.44444444, 44.44444444)
#'  ,mediana = c(NA, 77.77777778, 77.77777778, NA, 55.55555556, 53.64318567,
#'   NA, 77.77777778, 77.77777778, NA, 66.66666667, 53.64318567)
#'  ,Q3 = c(NA, 88.88888889, 88.88888889, NA, 77.77777778, 77.77777778, NA,
#'   77.77777778, 88.88888889, NA, 77.77777778, 66.66666667)
#'  ,max = c(100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100, 100)
#'  ,cv = c(NA, 33.37615474, 32.59990734, NA, 53.36259173, 52.36237087, NA,
#'   30.17201744, 26.16864626, NA, 46.99119315, 46.40394373)
#')
#'
#'# Calculando média das médias com a função
#'novo_medias = IPpackage::PosProce_meanofmean(
#'  arquivo_medias = Medias,
#'  variaveis_recalcular = list(
#'    "v29imrg" = c("v29i", "v30i")
#'    ,"v31imrg" = c("v31i", "v32i")
#'  ),
#'  tipo = "Medias"
#')
#'
#' @export
#'
#'

PosProce_meanofmean <- function(
    arquivo_medias,
    variaveis_recalcular,
    tipo = "Medias" #ou "Cruz_M"
){# Start: PosProce_meanofmean
  arquivo_medias = arquivo_medias %>%
    dplyr::group_by(splits) %>%
    dplyr::group_split() %>%
    purrr::imap(~{
      #split_i = 1;data_split_i = a[[1]]
      split_i = .y
      data_split_i = .x

      #Fazer para cada uma das variáveis
      if( base::length(variaveis_recalcular) == 0 )
      {# Start: Se nenhuma variável foi informada

        data_split_i_recalculado = data_split_i

      }# End: Se nenhuma variável foi informada
      else
      {# Start: Se alguma variável foi informada

        data_split_i_recalculado = data_split_i


        for( i in 1:base::length(variaveis_recalcular) )
        {# Start: Executando para cada variável

          variavel_atual = variaveis_recalcular[i]

          base::cat(
            sprintf("Rodando split %s: Vari\u00E1vel %s %s \n"
                    ,base::as.character(data_split_i_recalculado$splits %>% base::unique())
                    ,base::names(variavel_atual)
                    ,variavel_atual
            )
          )

          validacao1 = NA
          if( tipo == "Medias" )
          {# Start: verificando se tenho a variável no arquivo de Medias

            validacao1 = base::any( arquivo_medias$variavel %in% base::names(variavel_atual) )

          }# End: verificando se tenho a variável no arquivo de Medias
          if( tipo == "Cruz_M" )
          {# Start: verificando se tenho a variável no arquivo de Cruz_M

            validacao1 = base::any( arquivo_medias$varmedia %in% base::names(variavel_atual) )

          }# End: verificando se tenho a variável no arquivo de Cruz_M


          if( validacao1 == TRUE )
          {# Start: se tenho a variável no arquivo de processamento, executar

            if( tipo == "Medias" )
            {# Start: Calculando para o processamento 'Medias'

              if( base::any(data_split_i_recalculado$variavel %in% c(variavel_atual[[1]])) )
              {# Start: se tenho variável em 'Medias', calcular a média das médias

                #Calculando a média das médias
                recalculado = data_split_i_recalculado %>%
                  dplyr::filter(variavel %in% c(variavel_atual[[1]])) %>%
                  dplyr::summarise(
                    media = base::mean(media,na.rm = TRUE)
                    ,media_peso = base::mean(media_peso,na.rm = TRUE)
                  )

              }else{

                recalculado = base::data.frame(media = NA,media_peso = NA)

              }# End: se tenho variável em 'Medias', calcular a média das médias

              #Substituindo o valor antigo pelo novo
              data_split_i_recalculado = data_split_i_recalculado %>%
                dplyr::mutate(
                  dplyr::across(
                    .cols = -c("splits","variavel","n_base","n_base_peso"),
                    ~base::ifelse(
                      variavel == base::names(variavel_atual),NA,.
                    )
                  )
                ) %>%
                dplyr::mutate(
                  media = base::ifelse(
                    variavel == base::names(variavel_atual),recalculado$media,media
                  )
                  ,media_peso = base::ifelse(
                    variavel == base::names(variavel_atual),recalculado$media_peso,media_peso
                  )
                )

            }# End: Calculando para o processamento 'Medias'

            if( tipo == "Cruz_M" )
            {# Start: Calculando para o processamento 'Cruz_M'

              #Agrupar por 'varcol'
              data_split_i_recalculado = data_split_i_recalculado %>%
                dplyr::group_by(varcol) %>%
                dplyr::group_split() %>%
                purrr::imap(~{
                  #split_i_varcol_j = 1;data_split_i_varcol_j = a[[1]]
                  split_i_varcol_j = .y
                  data_split_i_varcol_j = .x

                  #Agrupar por 'codcol'
                  data_split_i_varcol_j = data_split_i_varcol_j %>%
                    dplyr::group_by(codcol) %>%
                    dplyr::group_split() %>%
                    purrr::imap(~{
                      #split_i_varcol_j_codcol_z = 1;data_split_i_varcol_j_codcol_z = a[[1]]
                      split_i_varcol_j_codcol_z = .y
                      data_split_i_varcol_j_codcol_z = .x

                      base::cat(
                        sprintf("varcol %s / codcol %s \n"
                                ,base::as.character(data_split_i_varcol_j_codcol_z$varcol %>% base::unique())
                                ,base::as.character(data_split_i_varcol_j_codcol_z$codcol %>% base::unique())
                        )
                      )

                      {# Start: Calculando
                        if( base::any(data_split_i_varcol_j_codcol_z$varmedia %in% c(variavel_atual[[1]])) )
                        {# Start: se tenho variável em 'Cruz_M', calcular a média das médias

                          recalculado = data_split_i_varcol_j_codcol_z %>%
                            dplyr::filter(varmedia %in% c(variavel_atual[[1]])) %>%
                            dplyr::summarise(
                              media = base::mean(media, na.rm = TRUE)
                              ,media_peso = base::mean(media_peso, na.rm = TRUE)
                            )

                        }else{

                          recalculado = base::data.frame(media = NA, media_peso = NA)

                        }# End: se tenho variável em 'Cruz_M', calcular a média das médias

                        #Substituindo o valor antigo pelo novo
                        data_split_i_varcol_j_codcol_z = data_split_i_varcol_j_codcol_z %>%
                          dplyr::mutate(
                            dplyr::across(
                              .cols = -c("splits","varmedia","varcol","codcol","n_base","n_base_peso","labcol","titulocol"),
                              ~base::ifelse(
                                varmedia == base::names(variavel_atual),NA,.
                              )
                            )
                          ) %>%
                          dplyr::mutate(
                            media = base::ifelse(
                              varmedia == base::names(variavel_atual),recalculado$media,media
                            )
                            ,media_peso = base::ifelse(
                              varmedia == base::names(variavel_atual),recalculado$media_peso,media_peso
                            )
                          )
                      }# End: Calculando

                      data_split_i_varcol_j_codcol_z

                    }) %>%
                    dplyr::bind_rows()

                  data_split_i_varcol_j

                }) %>%
                dplyr::bind_rows()

              data_split_i_recalculado

            }# End: Calculando para o processamento 'Cruz_M'

          }else{

            data_split_i_recalculado = data_split_i_recalculado

          }# End: se tenho a variável no arquivo de processamento, executar

        }# End: Executando para cada variável

      }# End: Se alguma variável foi informada

      data_split_i_recalculado

    }) %>%
    dplyr::bind_rows()

  #Retornar o arquivo com as médias alteradas
  base::return(arquivo_medias)

}# End: PosProce_meanofmean



