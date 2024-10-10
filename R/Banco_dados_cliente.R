#' @title Banco_dados_cliente
#' @name Banco_dados_cliente
#'
#' @description Esta função gera um arquivo Excel formatado a partir de um
#' conjunto de dados fornecido, com as variáveis organizadas e estilizadas
#' conforme as opções fornecidas. Permite a inclusão de filtros, congelamento
#' de títulos, e estilização do cabeçalho (com bordas e cores personalizadas).
#'
#' O arquivo pode ser salvo em um local especificado ou o objeto `Workbook` pode
#'  ser retornado.
#'
#' @param dados Um `data.frame` contendo os dados no formato que vem do servidor.
#'  Deve incluir todas as variáveis disponíveis para seleção
#' @param vars_quero Vetor de strings com os nomes das variáveis que serão
#' selecionadas e organizadas na planilha. A ordem das variáveis neste vetor
#' será mantida no arquivo final.
#' @param salvar Caminho para salvar o arquivo Excel gerado. Se `NULL`, a função
#' retorna o objeto `Workbook` sem salvar o arquivo. O padrão é `NULL`
#' @param esp Lógico. Indica se o nome da aba da planilha será em espanhol. Se
#' `TRUE`, a aba será chamada "Base de Datos"; caso contrário, será
#' "Banco de Dados". O padrão é `FALSE`
#' @param filtro_titulo Lógico. Determina se será adicionado um filtro na primeira
#' linha (título das colunas). O padrão é `TRUE`
#' @param freeze_titulo Lógico. Indica se a primeira linha da planilha (títulos
#' das colunas) será congelada, permitindo que permaneça visível ao rolar a
#' planilha para baixo. O padrão é `TRUE`
#' @param borda_e_cor_titulo Lógico. Define se será aplicada uma borda e cor de
#' fundo no título das colunas. O padrão é `TRUE`
#' @param cor_titulo_back String. Define a cor de fundo aplicada ao título das
#' colunas. O padrão é `"#808080"`
#' @param cor_titulo_fonte String. Define a cor da fonte aplicada ao título das
#' colunas. O padrão é `"white"`
#'
#' @return Retorna um objeto `Workbook` se `salvar` for `NULL`. Caso contrário,
#' salva o arquivo Excel no caminho especificado.
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import openxlsx
#' @import stringr
#'
#' @examples
#'
#' # Abrir o Workbook gerado em 'Dicionario_cliente'
#' openxlsx::openXL(
#'   file = IPpackage::Dicionario_cliente(
#'     dicionario = IPpackage::IPpackage_dicionario,
#'     vars_quero = c("id","v2", "v1", "v19_2", "v19mrg", "v10_2"),
#'     esp = TRUE,
#'     coluna_obs = FALSE,
#'     filtro_titulo = TRUE,
#'     freeze_titulo = TRUE,
#'     cor_titulo_back = "#808080",
#'     cor_titulo_fonte ="white"
#'   )
#' )
#'
#' # Abrir o Workbook gerado em 'Banco_dados_cliente'
#' openxlsx::openXL(
#'   file = IPpackage::Banco_dados_cliente(
#'     dados = IPpackage::IPpackage_exemplo,
#'     vars_quero = c("id","v2", "v1", "v19_2", "v10_2"),
#'     salvar = NULL,
#'     esp = TRUE,
#'     filtro_titulo = TRUE,
#'     freeze_titulo = TRUE,
#'     cor_titulo_back = "#808080",
#'     cor_titulo_fonte ="white"
#'   )
#' )
#'
#' @export



Banco_dados_cliente = function(
    dados, #no formato que vem do servidor
    vars_quero, # na ordem que ficará no dicionário e no banco
    salvar = NULL, # local que irá salvar
    esp = FALSE, #em espanhol?
    filtro_titulo = TRUE, #se quero ou não filtro na primeira linha
    freeze_titulo = TRUE, #congelar linha do título
    borda_e_cor_titulo = TRUE, #se é para colocar bordas e pintar o fundo do título
    cor_titulo_back = "#808080", #cor a ser aplicado no fundo do título
    cor_titulo_fonte ="white" #cor a ser aplicado na letra do título
)
{# Start: funcao_dicionario

  {# Start: ajustando os dados

    `%nin%` = base::Negate(`%in%`)

    #Dicionário
    x <- dados %>%
      dplyr::select(dplyr::all_of(vars_quero))

    #Trocando v por P
    vars = base::colnames(x)

    for( i in 1:length(vars) )
    {# Start: para cada variável


      vars[i] %>% stringr::str_sub(end=1) == "v"

      if( vars[i] %>% stringr::str_sub(end=1) == "v" )
      {# Start: se começa com "v", trocar

        vars[i] = IPpackage::substitui_prefixo_variavel(vars[i])

      }# End: se começa com "v", trocar

    }# End: para cada variável

    base::colnames(x) = vars %>%
      stringr::str_to_upper()

    base::rm(vars, i) %>%
      base::suppressWarnings()

  }# End: ajustando os dados

  {# Start: webshot (excel)

    wb <- openxlsx::createWorkbook()

    nome_sheet = base::ifelse(esp == FALSE, "Banco de Dados", "Base de Datos")

    sh = openxlsx::addWorksheet(wb = wb, sheetName = nome_sheet, gridLines = FALSE)

    # Adicionando o título
    openxlsx::writeData(
      wb = wb,
      sheet = nome_sheet,
      x = base::t(
        base::data.frame(
          base::colnames(x)
        )
      ),
      colNames = FALSE,
      startRow = 1
    )

    # Adicionando os dados
    openxlsx::writeData(
      wb = wb,
      sheet = nome_sheet,
      x = x,
      colNames = FALSE,
      startRow = 2
    )

    if( filtro_titulo == TRUE )
    {# Start: Adicionando filtro

      openxlsx::addFilter(
        wb = wb,
        sheet = nome_sheet,
        row = 1,
        cols = 1:base::ncol(x)
      )

    }# End: Adicionando filtro

    if( freeze_titulo == TRUE )
    {# Start: congelando linha superior

      openxlsx::freezePane(
        wb = wb,
        sheet = nome_sheet,
        firstRow = TRUE
      )

    }# End: congelando linha superior

    freezePane(
      wb,
      sheet,
      firstActiveRow = NULL,
      firstActiveCol = NULL,
      firstRow = FALSE,
      firstCol = FALSE
    )

    if( borda_e_cor_titulo == TRUE )
    {# Start: Adicionando borda e background no título

      for( j in 1:base::ncol(x) )
      {# Start: para cada coluna

        {# Start: formatando as colunas

          # Todas as linhas
          openxlsx::addStyle(
            wb = wb,
            sheet = nome_sheet,
            style = openxlsx::createStyle(
              #borda
              border = c("bottom", "right", "left", "top"),
              borderColour = "black",
              borderStyle = "dotted",
              #letras
              halign = "left",
              valign = "top"
            ),
            cols = j,
            rows = 1:(base::nrow(x)+1),
            gridExpand = T
          )

          # Primeira linhas (título)
          openxlsx::addStyle(
            wb = wb,
            sheet = nome_sheet,
            style = openxlsx::createStyle(
              #borda
              border = c("bottom", "right", "left", "top"),
              borderColour = "black",
              borderStyle = "dotted",
              #letras
              halign ="left",
              valign ="center",
              textDecoration = c("BOLD"),
              fgFill = cor_titulo_back,
              fontColour = cor_titulo_fonte
            ),
            cols = j,
            rows = 1,
            gridExpand = T
          )

        } # End: formatando as colunas

      }# End: para cada coluna

    }# End: Adicionando borda e background no título

    #openxlsx::openXL(wb)
    if( base::is.null(salvar) )
    {# Start: se dou um caminho para salvar, salvo. Se não, retorno a planilha formatada

      base::return(wb)

    } else {

      openxlsx::saveWorkbook(wb = wb, file = salvar, overwrite = TRUE)

    }# End: se dou um caminho para salvar, salvo. Se não, retorno a planilha formatada

    base::rm(x, i, j, ordem, vars, variavel, armazenas, y, wb, sh, v, antes, depois) %>%
      base::suppressWarnings()

  }# End: webshot (excel)

}# End: funcao_dicionario
