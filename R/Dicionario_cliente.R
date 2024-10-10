#' @title Dicionario_cliente
#' @name Dicionario_cliente
#'
#' @description Esta função gera um dicionário de dados personalizado a partir de
#' um dicionário (no formato do que entra na função de processamento).
#'
#' O dicionário resultante contém variáveis específicas (definidas em vars_quero)
#' organizadas e formatadas de acordo com as opções fornecidas, como a inclusão de
#' filtros, congelamento de títulos e estilização visual.
#'
#' Pode gerar uma planilha Excel formatada com as variáveis e descrições desejadas,
#' com opção de salvar o arquivo ou retornar o objeto Workbook.
#'
#' @param dicionario Um data.frame contendo o dicionário base. Deve conter as
#' colunas "opcao_variavel", "pergunta_enunciado", "opcao_cod", e "opcao_label",
#' além de uma coluna opcional de observações ("Obs") caso coluna_obs seja TRUE
#' @param vars_quero Um vetor de caracteres especificando as variáveis de
#' interesse na ordem que elas devem aparecer no dicionário
#' @param salvar Um caminho para salvar o arquivo Excel gerado. Se NULL, a
#' função retorna o objeto Workbook sem salvar o arquivo
#' @param esp Um valor lógico. Se TRUE, o dicionário será gerado em espanhol.
#' Se FALSE, o idioma padrão será o português
#' @param coluna_obs Um valor lógico. Indica se a coluna de observações ("Obs")
#' deve ser incluída no dicionário. Se TRUE, a coluna será incluída
#' @param filtro_titulo Um valor lógico. Se TRUE, adiciona um filtro na
#' primeira linha do dicionário (título das colunas)
#' @param freeze_titulo Um valor lógico. Se TRUE, a primeira linha do
#' dicionário (título das colunas) será congelada ao rolar a planilha
#' @param cor_titulo_back Cor de fundo para o título do dicionário (primeira
#' linha). O padrão é "#808080" (cinza)
#' @param cor_titulo_fonte Cor da fonte (texto) do título do dicionário
#' (primeira linha). O padrão é "white" (branco)
#'
#' @return Retorna um objeto Workbook se o parâmetro salvar for NULL.
#' Caso contrário, salva o arquivo Excel no local especificado.
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


Dicionario_cliente = function(
    dicionario, #no formato do dicionário que entra no processamento
    vars_quero, # na ordem que ficará no dicionário e no banco
    salvar = NULL, # local que irá salvar
    esp = FALSE, #em espanhol?
    coluna_obs = FALSE, #Se tenho coluna com Observação
    filtro_titulo = TRUE, #se quero ou não filtro na primeira linha
    freeze_titulo = TRUE, #congelar linha do título
    cor_titulo_back = "#808080", #cor a ser aplicado no fundo do título
    cor_titulo_fonte ="white" #cor a ser aplicado na letra do título
)
{# Start: funcao_dicionario

  {# Start: ajustando os dados

    `%nin%` = base::Negate(`%in%`)

    col_quero = c("opcao_variavel", "pergunta_enunciado", "opcao_cod", "opcao_label")

    if( coluna_obs == TRUE )
    {# Start: se tenho coluna com observação

      col_quero = c(col_quero, "Obs")

    }# End: se tenho coluna com observação

    #Dicionário
    x <- dicionario %>%
      dplyr::filter(opcao_variavel %in% vars_quero) %>%
      dplyr::select(dplyr::all_of(col_quero)) %>%
      dplyr::filter(!is.na(opcao_variavel) & opcao_variavel %nin% c("", " ")) %>%
      base::unique()

    x$ordem <- NA
    y = vars_quero

    for( i in 1:base::length(y) )
    {# Start: colocando as variáveis na ordem que quero

      if( base::any(y[i] %in% x$opcao_variavel) )
      {# Start: se tenho essa var, informar posição

        x$ordem[base::which(x$opcao_variavel == y[i])] <- i
        # base::print(y[i])

      }# End: se tenho essa var, informar posição

    }# End: colocando as variáveis na ordem que quero

    x <- x %>%
      dplyr::mutate(ordem_var = ordem) %>%
      dplyr::select(-ordem)

    # Ordenando os códigos de cada variável
    x = x %>%
      dplyr::group_by(opcao_variavel) %>%
      dplyr::group_split() %>%
      purrr::imap(
        ~{# Start: para cada variável

          .x %>%
            dplyr::mutate(opcao_cod = base::as.numeric(opcao_cod)) %>%
            dplyr::arrange(opcao_cod) %>%
            base::unique()

        }# End: para cada variável
      ) %>%
      dplyr::bind_rows() %>%
      base::unique() %>%
      dplyr::arrange(ordem_var) %>%
      dplyr::select(-ordem_var)

    #Trocando v por P
    vars = x$opcao_variavel

    for( i in 1:length(vars) )
    {# Start: para cada variável

      vars[i] %>% stringr::str_sub(end=1) == "v"

      if( vars[i] %>% stringr::str_sub(end=1) == "v" )
      {# Start: se começa com "v", trocar

        vars[i] = IPpackage::substitui_prefixo_variavel(vars[i])

      }# End: se começa com "v", trocar

    }# End: para cada variável

    x$opcao_variavel = vars %>%
      stringr::str_to_upper()

    base::rm(vars, i) %>%
      base::suppressWarnings()

  }# End: ajustando os dados

  {# Start: webshot (excel)

    wb <- openxlsx::createWorkbook()

    nome_sheet = base::ifelse(esp == FALSE, "Dicion\u00E1rio", "Diccionario")

    sh = openxlsx::addWorksheet(wb = wb, sheetName = nome_sheet, gridLines = FALSE)

    # Adicionando o título
    openxlsx::writeData(
      wb = wb,
      sheet = nome_sheet,
      x = base::data.frame(
        base::ifelse(esp == FALSE, "Vari\u00E1vel", "Variable"),
        base::ifelse(esp == FALSE, "Descri\u00E7\u00E3o", "Descripci\u00F3n"),
        base::ifelse(esp == FALSE, "C\u00F3digos", "C\u00F3digos"),
        base::ifelse(esp == FALSE, "Op\u00E7\u00F5es de Resposta", "Opciones de Respuesta"),
        base::ifelse(coluna_obs == TRUE, "Obs.:", NA)
      ),
      colNames = FALSE,
      startRow = 1
    )

    # Adicionando os dados
    openxlsx::writeData(
      wb = wb,
      sheet = nome_sheet,
      x = x %>%
        base::unique(),
      colNames = FALSE,
      startRow = 2
    )

    if( freeze_titulo == TRUE )
    {# Start: congelando linha superior

      openxlsx::freezePane(
        wb = wb,
        sheet = nome_sheet,
        firstRow = TRUE
      )

    }# End: congelando linha superior

    if( filtro_titulo == TRUE )
    {# Start: Adicionando filtro

      openxlsx::addFilter(
        wb = wb,
        sheet = nome_sheet,
        row = 1,
        cols = 1:base::ncol(x)
      )

    }# End: Adicionando filtro

    # Merge do enunciado de cada variável
    vars <- x$opcao_variavel %>%
      base::unique()

    for( i in 1:base::length(vars) )
    {# Start: para cada variável

      y = base::which(x$opcao_variavel==vars[i])

      if( base::length(y) > 1 )
      {# Start: se tenho mais de uma linha para essa varia´vel, fazer merge

        # Variável
        openxlsx::mergeCells(
          wb = wb,
          sheet = 1,
          cols = 1,
          rows = base::which(x$opcao_variavel == vars[i]) + 1
        )

        # Descrição
        openxlsx::mergeCells(
          wb = wb,
          sheet = 1,
          cols = 2,
          rows = base::which(x$opcao_variavel == vars[i]) + 1
        )

      }# End: se tenho mais de uma linha para essa varia´vel, fazer merge

    }# End: para cada variável

    # Formatando as células

    x <- base::rbind(
      base::rep("n linha igual sheet", base::ncol(x)),
      x
    )
    for( j in 1:base::ncol(x) )
    {# Start: para cada coluna

      if( j %nin% c(2, 4) )
      {# Start: formatando as colunas

        # Todas as linhas
        openxlsx::addStyle(
          wb = wb,
          sheet = 1,
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
          rows = 1:base::nrow(x),
          gridExpand = T
        )

        # Primeira linhas (título)
        openxlsx::addStyle(
          wb = wb,
          sheet = 1,
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

      } else {

        # Todas as linhas
        openxlsx::addStyle(
          wb = wb,
          sheet = 1,
          style = openxlsx::createStyle(
            #borda
            border = c("bottom", "right", "left", "top"),
            borderColour = "black",
            borderStyle="dotted",
            #letras
            halign="left",
            valign="top",
            #"quebrar texto automaticamente"
            wrapText = TRUE
          ),
          cols = j,
          rows = 1:base::nrow(x),
          gridExpand = T)

        # Primeira linhas (título)
        openxlsx::addStyle(
          wb = wb,
          sheet = 1,
          style = openxlsx::createStyle(
            #borda
            border = c("bottom", "right", "left", "top"),
            borderColour = "black",
            borderStyle="dotted",
            #letras
            halign = "left",
            valign="center",
            textDecoration = c("BOLD"),
            fgFill = "#808080",
            fontColour="white",
            #"quebrar texto automaticamente"
            wrapText = TRUE,
          ),
          cols = j,
          rows = 1,
          gridExpand = T)

      }# End: formatando as colunas

    }# End: para cada coluna

    #Tamanho das colunas
    openxlsx::setColWidths(wb = wb, sheet = 1, cols = c(2, 4), widths = c(55, 55))

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
