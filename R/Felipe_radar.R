#' @title Felipe_radar
#' @name Felipe_radar
#'
#' @description  Função para geração de gráficos do tipo 'radar'
#'
#' @param plot.data plot.data
#' @param base.size = 15
#' @param font.radar = "sans"
#' @param values.radar = values.radar
#' @param axis.labels = colnames(plot.data)[-1]
#' @param grid.min = 0
#' @param grid.um = 0.10
#' @param grid.dois = 0.20
#' @param grid.tres = 0.30
#' @param grid.quatro = 0.40
#' @param grid.cinco = 0.50
#' @param grid.seis = 0.60
#' @param grid.sete = 0.70
#' @param grid.oito = 0.80
#' @param grid.nove = 0.90
#' @param grid.dez = 1
#' @param centre.y = grid.min - ((1/9) *(grid.dez - grid.min))
#' @param plot.extent.x.sf = 1
#' @param plot.extent.y.sf = 1.2
#' @param x.centre.range = 0.02 * (grid.dez - centre.y)
#' @param label.centre.y = FALSE
#' @param grid.line.width = 0.5
#' @param gridline.min.linetype = "longdash"
#' @param gridline.um.linetype = "longdash"
#' @param gridline.dois.linetype = "longdash"
#' @param gridline.tres.linetype = "longdash"
#' @param gridline.quatro.linetype = "longdash"
#' @param gridline.cinco.linetype = "longdash"
#' @param gridline.seis.linetype = "longdash"
#' @param gridline.sete.linetype = "longdash"
#' @param gridline.oito.linetype = "longdash"
#' @param gridline.nove.linetype = "longdash"
#' @param gridline.dez.linetype = "longdash"
#' @param gridline.min.colour = "grey"
#' @param gridline.um.colour = "grey"
#' @param gridline.dois.colour = "grey"
#' @param gridline.tres.colour = "grey"
#' @param gridline.quatro.colour = "grey"
#' @param gridline.cinco.colour = "grey"
#' @param gridline.seis.colour = "grey"
#' @param gridline.sete.colour = "grey"
#' @param gridline.oito.colour = "grey"
#' @param gridline.nove.colour = "grey"
#' @param gridline.dez.colour = "grey"
#' @param grid.label.size = 4
#' @param gridline.label.offset = -0.1 * (grid.dez - centre.y)
#' @param label.gridline.min = TRUE
#' @param label.gridline.um = TRUE
#' @param label.gridline.dois = TRUE
#' @param label.gridline.tres = TRUE
#' @param label.gridline.quatro = TRUE
#' @param label.gridline.cinco = TRUE
#' @param label.gridline.seis = TRUE
#' @param label.gridline.sete = TRUE
#' @param label.gridline.oito = TRUE
#' @param label.gridline.nove = TRUE
#' @param label.gridline.dez = TRUE
#' @param axis.label.offset = 1.15
#' @param axis.label.size = 5
#' @param axis.label.color = NULL
#' @param axis.line.colour = "grey"
#' @param group.line.width = 1
#' @param group.point.size = 3
#' @param group.colours = NULL
#' @param background.circle.colour = "#D7D6D1"
#' @param background.circle.transparency = 0.2
#' @param plot.legend =ifelse(nrow(plot.data) >1,TRUE,FALSE)
#' @param legend.title = ""
#' @param plot.title = ""
#' @param plot.title.hjust =.5
#' @param plot.title.size =15
#' @param legend.text.size = 14
#' @param legend.position = "top"
#' @param fill = FALSE
#' @param fill.alpha = 0.5
#' @param graf.limits =c(2,2)
#'
#' @details Consute o livro para mais detalhes e exemplos.
#'
#' @import dplyr
#' @import ggplot2
#' @import scales
#' @import tibble
#'
#' @examples
#'library(dplyr)
#'library(scales)
#'library(tibble)
#'
#' mtcars_radar <- mtcars %>%
#'   tibble::as_tibble(rownames = "group") %>%
#'   dplyr::mutate_at(vars(-group), scales::rescale) %>%
#'   utils::tail(4) %>%
#'   dplyr::select(1:10)
#' Felipe_radar(mtcars_radar,group.colours=c("#FFB400","#FF5A5F", "#007A87","#8CE071"),
#' values.radar = c("0%","","20%","","40%","","60%","","80%","","100%"),grid.label.size=5,
#' background.circle.colour="white",axis.label.size = 5)
#'
#' ##Comparando com ggradar
#' #library(ggradar)
#' #ggradar(mtcars_radar)
#' ##Remodeno o fundo e salvando
#' #a=Felipe_radar(mtcars_radar,group.colours=c("#FFB400","#FF5A5F", "#007A87","#8CE071"),
#' #values.radar = c("0%","","20%","","40%","","60%","","80%","","100%"),grid.label.size=5,
#' #background.circle.colour="white",axis.label.size = 5,fill=T);a
#' #ggsave(a, file="Felipe_radar.png" , width=13, height=7, bg = "transparent")
#' #library(magick)
#' #magick::image_write(magick::image_transparent(magick::image_read("Felipe_radar.png"),
#' #white'),"Felipe_radar.png")
#' @export

Felipe_radar = function(
    plot.data,
    base.size = 15,
    font.radar = "sans",
    values.radar = c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%"),
    axis.labels = colnames(plot.data)[-1],
    axis.label.color = NULL,
    grid.min = 0,
    grid.um = 0.10,
    grid.dois = 0.20,
    grid.tres = 0.30,
    grid.quatro = 0.40,
    grid.cinco = 0.50,
    grid.seis = 0.60,
    grid.sete = 0.70,
    grid.oito = 0.80,
    grid.nove = 0.90,
    grid.dez = 1,
    centre.y = grid.min - ((1/9) *(grid.dez - grid.min)),
    plot.extent.x.sf = 1,
    plot.extent.y.sf = 1.2,
    x.centre.range = 0.02 * (grid.dez - centre.y),
    label.centre.y = FALSE,
    grid.line.width = 0.5,
    gridline.min.linetype = "longdash",
    gridline.um.linetype = "longdash",
    gridline.dois.linetype = "longdash",
    gridline.tres.linetype = "longdash",
    gridline.quatro.linetype = "longdash",
    gridline.cinco.linetype = "longdash",
    gridline.seis.linetype = "longdash",
    gridline.sete.linetype = "longdash",
    gridline.oito.linetype = "longdash",
    gridline.nove.linetype = "longdash",
    gridline.dez.linetype = "longdash",
    gridline.min.colour = "grey",
    gridline.um.colour = "grey",
    gridline.dois.colour = "grey",
    gridline.tres.colour = "grey",
    gridline.quatro.colour = "grey",
    gridline.cinco.colour = "grey",
    gridline.seis.colour = "grey",
    gridline.sete.colour = "grey",
    gridline.oito.colour = "grey",
    gridline.nove.colour = "grey",
    gridline.dez.colour = "grey",
    grid.label.size = 4,
    gridline.label.offset = -0.1 * (grid.dez - centre.y),
    label.gridline.min = TRUE,
    label.gridline.um = TRUE,
    label.gridline.dois = TRUE,
    label.gridline.tres = TRUE,
    label.gridline.quatro = TRUE,
    label.gridline.cinco = TRUE,
    label.gridline.seis = TRUE,
    label.gridline.sete = TRUE,
    label.gridline.oito = TRUE,
    label.gridline.nove = TRUE,
    label.gridline.dez = TRUE,
    axis.label.offset = 1.15,
    axis.label.size = 5,
    axis.line.colour = "grey",
    group.line.width = 1,
    group.point.size = 3,
    group.colours = NULL,
    background.circle.colour = "#D7D6D1",
    background.circle.transparency = 0.2,
    plot.legend =ifelse(nrow(plot.data) >1,TRUE,FALSE),
    legend.title = "",
    plot.title = "",
    plot.title.hjust=.5,
    plot.title.size=15,
    legend.text.size = 14,
    legend.position = "top",
    fill = FALSE,
    fill.alpha = 0.5,
    graf.limits=c(2,2)
)
{# Start: Felipe_radar

  {# Start: funções auxiliares

    CalculateGroupPath <- function (
    df
    )
    {# Start: CalculateGroupPath

      path <- df[, 1]
      angles <- base::seq(
        from = 0,
        to = 2 * base::pi,
        by = (2 * base::pi) / (base::ncol(df) -1)
      )
      graphData <- base::data.frame(seg = "", x = 0, y = 0)
      graphData <- graphData[-1, ]

      for ( i in base::levels(path) )
      {# Start: função auxiliar 'CalculateGroupPath'

        pathData <- base::subset(df, df[, 1] == i)

        for ( j in c(2:base::ncol(df)) )
        {# Start: para cada coluna

          graphData <- base::rbind(
            graphData,
            base::data.frame(
              group = i,
              x = pathData[, j] * base::sin(angles[j - 1]),
              y = pathData[,j] * base::cos(angles[j - 1])
            )
          )

        }# End: para cada coluna

        graphData <- base::rbind(
          graphData,
          base::data.frame(
            group = i,
            x = pathData[, 2] * base::sin(angles[1]),
            y = pathData[,2] * base::cos(angles[1])
          )
        )

      }# End: para cada path


      base::colnames(graphData)[1] <- base::colnames(df)[1]

      graphData$group <- base::factor(graphData$group, levels = base::levels(df[,1]))

      graphData

    }# End: função auxiliar 'CalculateGroupPath'

    CalculateAxisPath <- function (
    var.names,
    min,
    max
    )
    {# Start: função auxiliar 'CalculateAxisPath'

      n.vars <- base::length(var.names)
      angles <- base::seq(
        from = 0,
        to = 2 * base::pi,
        by = (2 * base::pi) / n.vars
      )
      min.x <- min * base::sin(angles)
      min.y <- min * base::cos(angles)
      max.x <- max * base::sin(angles)
      max.y <- max * base::cos(angles)
      axisData <- NULL

      for ( i in 1:n.vars )
      {# Start: para cada 'n.vars'

        a <- c(i, min.x[i], min.y[i])
        b <- c(i, max.x[i], max.y[i])
        axisData <- base::rbind(axisData, a, b)

      }# End: para cada 'n.vars'

      base::colnames(axisData) <- c("axis.no", "x", "y")
      base::rownames(axisData) <- base::seq(1:base::nrow(axisData))
      base::as.data.frame(axisData)

    }# End: função auxiliar 'CalculateAxisPath'

    funcCircleCoords <- function (
    center = c(0, 0),
    r = 1,
    npoints = 100
    )
    {# Start: função auxiliar 'funcCircleCoords'

      tt <- base::seq(
        from = 0,
        to = 2 * pi,
        length.out = npoints
      )

      xx <- center[1] + r * base::cos(tt)
      yy <- center[2] + r * base::sin(tt)

      base::return(
        base::data.frame(x = xx, y = yy)
      )

    }# End: função auxiliar 'funcCircleCoords'

  }# End: funções auxiliares

  #Arrumando o banco
  plot.data <- base::as.data.frame(plot.data)
  plot.data[, 1] <- base::factor(plot.data[, 1], levels = base::as.character(plot.data[, 1]))
  base::names(plot.data)[1] <- "group"
  var.names <- base::colnames(plot.data)[-1]
  plot.extent.x <- (grid.dez + base::abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.dez + base::abs(centre.y)) * plot.extent.y.sf

  {# Start: Verificando erros

    if( base::length(axis.labels) != base::ncol(plot.data) - 1 )
    {# Start: primeiro erro
      base::stop(
        "'axis.labels' contains the wrong number of axis labels", call. = FALSE
      )
    }# End: primeiro erro

    if( base::min(plot.data[, -1], na.rm = TRUE) < centre.y )
    {# Start: segundo erro

      base::stop(
        "plot.data' contains value(s) < centre.y", call. = FALSE
      )
    }# End: segundo erro

    if( base::max(plot.data[, -1], na.rm = TRUE) > grid.dez )
    {# Start: terceiro erro

      base::stop(
        "'plot.data' contains value(s) > grid.max", call. = FALSE
      )

    }# End: terceiro erro

  }# End: Verificando erros

  #Colocando a escala (novos valores)
  plot.data.offset <- plot.data
  plot.data.offset[, 2:base::ncol(plot.data)] <- plot.data[, 2:base::ncol(plot.data)]

  {# Start: Coordenadas

    group <- NULL
    group$path <- CalculateGroupPath(plot.data.offset)
    axis <- NULL
    axis$path <- CalculateAxisPath(var.names, grid.min, grid.dez)
    axis$label <- base::data.frame(text = axis.labels, x = NA, y = NA)
    n.vars <- base::length(var.names)
    angles <- base::seq(from = 0, to = 2 * base::pi, by = (2 * base::pi) / n.vars)
    axis$label$x <- base::sapply(
      1:n.vars,
      function(i, x) {
        ((grid.dez) * axis.label.offset) * base::sin(angles[i])
      }
    )
    axis$label$y <- base::sapply(
      1:n.vars,
      function(i, x) {
        ((grid.dez) * axis.label.offset) * base::cos(angles[i])
      }
    )

    gridline <- NULL

    gridline$min$path <- funcCircleCoords(c(0, 0), grid.min, npoints = 360)
    gridline$um$path <- funcCircleCoords(c(0, 0), grid.um, npoints = 360)
    gridline$dois$path <- funcCircleCoords(c(0, 0), grid.dois, npoints = 360)
    gridline$tres$path <- funcCircleCoords(c(0, 0), grid.tres, npoints = 360)
    gridline$quatro$path <- funcCircleCoords(c(0, 0), grid.quatro, npoints = 360)
    gridline$cinco$path <- funcCircleCoords(c(0, 0), grid.cinco, npoints = 300)
    gridline$seis$path <- funcCircleCoords(c(0, 0), grid.seis, npoints = 360)
    gridline$sete$path <- funcCircleCoords(c(0, 0), grid.sete, npoints = 360)
    gridline$oito$path <- funcCircleCoords(c(0, 0), grid.oito, npoints = 360)
    gridline$nove$path <- funcCircleCoords(c(0, 0), grid.nove, npoints = 360)
    gridline$dez$path <- funcCircleCoords(c(0, 0), grid.dez, npoints = 360)

    gridline$min$label <- base::data.frame(x = 0, y = grid.min, text = base::as.character(grid.min))
    gridline$um$label <- base::data.frame(x = 0, y = grid.um, text = base::as.character(grid.um))
    gridline$dois$label <- base::data.frame(x = 0, y = grid.dois, text = base::as.character(grid.dois))
    gridline$tres$label <- base::data.frame(x = 0, y = grid.tres, text = base::as.character(grid.tres))
    gridline$quatro$label <- base::data.frame(x = 0, y = grid.quatro, text = base::as.character(grid.quatro))
    gridline$cinco$label <- base::data.frame(x = 0, y = grid.cinco, text = base::as.character(grid.cinco))
    gridline$seis$label <- base::data.frame(x = 0, y = grid.seis, text = base::as.character(grid.seis))
    gridline$sete$label <- base::data.frame(x = 0, y = grid.sete, text = base::as.character(grid.sete))
    gridline$oito$label <- base::data.frame(x = 0, y = grid.oito, text = base::as.character(grid.oito))
    gridline$nove$label <- base::data.frame(x = 0, y = grid.nove, text = base::as.character(grid.nove))
    gridline$dez$label <- base::data.frame(x = 0, y = grid.dez, text = base::as.character(grid.dez))

  }# End: Coordenadas

  {# Start: Cores das legendas em volta do radar

    axis$label$color = "#000000"

    if( !base::is.null(axis.label.color) )
    {# Start: Cores das legendas em volta do radar

      for( i in 1:base::length(axis.label.color) )
      {# Start: olhando todos os 'text' informados na lsita com as cores

        if( base::names(axis.label.color)[i] %in% axis$label$text )
        {# Start: colocando a cor no lugar do preto

          axis$label$color[base::which(axis$label$text == base::names(axis.label.color)[i])] = axis.label.color[i]

        }# End: colocando a cor no lugar do preto

      }# End: olhando todos os 'text' informados na lsita com as cores

    }# End: se falei alguma cor, coloco. Se não, fica preto

  }# End: Cores das legendas em volta do radar

  theme_clear <- ggplot2::theme_bw(base_size = base.size) +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.text.x = ggplot2::element_blank(),
      axis.ticks = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      legend.key = ggplot2::element_rect(linetype = "blank")
    )

  if( plot.legend == FALSE )
  {# Start: se não quero a legenda

    legend.position = "none"

  }# End: se não quero a legenda

  #Grid inicial
  base <- ggplot2::ggplot(axis$label) +
    ggplot2::xlab(NULL) +
    ggplot2::ylab(NULL) +
    ggplot2::coord_equal() +
    ggplot2::scale_x_continuous( limits = c(-graf.limits[[1]] * plot.extent.x, graf.limits[[1]] * plot.extent.x) ) +
    ggplot2::scale_y_continuous( limits = c(-graf.limits[[2]] / 2 * plot.extent.y, graf.limits[[2]] / 2 * plot.extent.y) )

  for ( i in 1:base::nrow(axis$label) )
  {# Start: Colocando o label (em volta dos circulos)

    label_data <- axis$label[i, ]

    if ( base::abs(label_data$x) <= x.centre.range ) {

      hjust_value <- 0.5

    } else if ( label_data$x > x.centre.range ) {

      hjust_value <- 0

    } else {

      hjust_value <- 1

    }

    base <- base +
      ggplot2::geom_text(
        data = label_data,
        ggplot2::aes(x = x, y = y, label = text),
        color = label_data$color,
        size = axis.label.size,
        hjust = hjust_value,
        family = font.radar
      )

  }# End: Colocando o label (em volta dos circulos)

  #circulos pontilhados
  base <- base +
    #Menor(testando ponto ao inves de circulo)
    ggplot2::geom_point(
      ggplot2::aes(x = grid.min, y = grid.min),
      colour = gridline.min.colour
    ) +
    #Um a dez
    ggplot2::geom_path(
      data = gridline$um$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.um.linetype,
      colour = gridline.um.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$dois$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.dois.linetype,
      colour = gridline.dois.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$tres$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.tres.linetype,
      colour = gridline.tres.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$quatro$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.quatro.linetype,
      colour = gridline.quatro.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$cinco$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.cinco.linetype,
      colour = gridline.cinco.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$seis$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.seis.linetype,
      colour = gridline.seis.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$sete$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.sete.linetype,
      colour = gridline.sete.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$oito$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.oito.linetype,
      colour = gridline.oito.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$nove$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.nove.linetype,
      colour = gridline.nove.colour,
      size = grid.line.width
    ) +
    ggplot2::geom_path(
      data = gridline$dez$path,
      ggplot2::aes(x = x, y = y),
      lty = gridline.dez.linetype,
      colour = gridline.dez.colour,
      size = grid.line.width
    )

  #Ajustando o tema
  base <- base +
    theme_clear +
    ggplot2::geom_polygon(
      data = gridline$dez$path,
      ggplot2::aes(x, y),
      fill = background.circle.colour,
      alpha = background.circle.transparency
    ) +
    ggplot2::geom_path(
      data = axis$path,
      ggplot2::aes(x = x, y = y, group = axis.no),
      colour = axis.line.colour
    ) +
    #Colocando legenda, e linhas coloridas do radar
    ggplot2::geom_path(
      data = group$path,
      ggplot2::aes(x = x, y = y, group = group, colour = group),
      size = group.line.width
    ) +
    #Colocando os "pontos" no rodar (na linha colorida)
    ggplot2::geom_point(
      data = group$path,
      ggplot2::aes(x = x, y = y, group = group, colour = group),
      size = group.point.size
    )

  #Background dentro do radar colorido?
  if( fill == TRUE )
  {# Start: se quero Background dentro do radar colorido, colocar

    base <- base +
      ggplot2::geom_polygon(
        data = group$path,
        ggplot2::aes(x = x, y = y, group = group, fill = group),
        alpha = fill.alpha
      )

  }# End: se quero Background dentro do radar colorido, colocar


  if( plot.legend == TRUE )
  {# Start: Legenda

    base <- base +
      ggplot2::labs(
        colour = legend.title,
        size = legend.text.size
      )

  }# End: Legenda

  {# Start: Label das linhas pontilhados

    if( label.gridline.min == TRUE )
    {# Start: label da primeira linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[1]),
          data = gridline$min$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da primeira linha

    if( label.gridline.um == TRUE )
    {# Start: label da primeira linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[2]),
          data = gridline$um$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da primeira linha

    if( label.gridline.dois == TRUE )
    {# Start: label da segunda linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[3]),
          data = gridline$dois$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da segunda linha

    if( label.gridline.tres == TRUE )
    {# Start: label da terceira linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[4]),
          data = gridline$tres$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da terceira linha

    if( label.gridline.quatro == TRUE )
    {# Start: label da quarta linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[5]),
          data = gridline$quatro$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da quarta linha

    if( label.gridline.cinco == TRUE )
    {# Start: label da quinta linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[6]),
          data = gridline$cinco$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da quinta linha

    if( label.gridline.seis == TRUE )
    {# Start: label da sexta linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[7]),
          data = gridline$seis$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da sexta linha

    if( label.gridline.sete == TRUE )
    {# Start: label da sétima linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[8]),
          data = gridline$sete$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da sétima linha

    if( label.gridline.oito == TRUE )
    {# Start: label da oitava linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[9]),
          data = gridline$oito$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da oitava linha

    if( label.gridline.nove == TRUE )
    {# Start: label da nona linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[10]),
          data = gridline$nove$label,
          size = grid.label.size * 0.8,
          hjust = 1,
          family = font.radar
        )

    }# End: label da nona linha
    if( label.gridline.dez == TRUE )
    {# Start: label da décima linha

      base <- base +
        ggplot2::geom_text(
          ggplot2::aes(x = x, y = y, label = values.radar[11]),
          data = gridline$dez$label,
          size = grid.label.size * 0.8,
          hjust = 1, family = font.radar
        )

    }# End: label da décima linha

  }# End: Label das linhas pontilhados


  #label no meio do radar?
  if ( label.centre.y == TRUE )
  {# Start: colocando label no meio do radar

    centre.y.label <- base::data.frame(
      x = 0,
      y = 0,
      text = base::as.character(centre.y)
    )

    base <- base +
      ggplot2::geom_text(
        ggplot2::aes(x = x, y = y, label = text),
        data = centre.y.label,
        size = grid.label.size,
        hjust = 0.5,
        family = font.radar
      )

  }# End: colocando label no meio do radar


  if( !base::is.null(group.colours) )
  {# Start: Cores, fontes e local da legenda

    colour_values <- base::rep(group.colours, 100)

  } else {

    colour_values <- base::rep(
      c("#FF5A5F", "#FFB400", "#007A87",
        "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C",
        "#9CA299", "#565A5C", "#00A04B", "#E54C20")
      , 100.
    )

  }# End: Cores, fontes e local da legenda

  base <- base +
    #Legenda do tipo ponto com linhas
    ggplot2::theme(legend.key.width = unit(3, "line")) +
    #tamanho e fonte da legenda
    ggplot2::theme(text = ggplot2::element_text(size = legend.text.size, family = font.radar)) +
    #posição da legenda
    ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size), legend.position = legend.position) +
    #Cores das linhas com legenda
    ggplot2::theme(legend.key.height = unit(2, "line")) +
    ggplot2::scale_colour_manual(values = colour_values) +
    #Fonte geral
    ggplot2::theme(text = ggplot2::element_text(family = font.radar)) +
    #Legenda com fundo branco
    ggplot2::theme(legend.title = ggplot2::element_blank())
  if( base::isTRUE(fill) )
  {# Start: Background dentro do radar colorido?

    base <- base +
      ggplot2::scale_fill_manual(values = colour_values, guide = "none")

  }# End: Background dentro do radar colorido?
  if( legend.title != "" )
  {# Start: Título da legenda

    base <- base +
      ggplot2::theme(legend.title = ggplot2::element_text())

  }# End: Título da legenda

  if( plot.title != "" )
  {# Start: Título do radar

    base <- base +
      ggplot2::ggtitle(plot.title) +
      ggplot2::theme(plot.title = ggplot2::element_text(hjust = plot.title.hjust, size = plot.title.size))

  }# End: Título do radar

  base::return(base)

}# End: Felipe_radar
