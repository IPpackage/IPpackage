#' @title Felipe_radar
#' @name Felipe_radar
#'
#' @description  x
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
#'
#'
#'
#' @export
#'
#'

Felipe_radar<-function(
    plot.data
    ,base.size = 15
    ,font.radar = "sans"
    ,values.radar = c("0%","10%","20%","30%","40%","50%","60%","70%","80%","90%","100%")
    ,axis.labels = colnames(plot.data)[-1]
    ,grid.min = 0
    ,grid.um = 0.10
    ,grid.dois = 0.20
    ,grid.tres = 0.30
    ,grid.quatro = 0.40
    ,grid.cinco = 0.50
    ,grid.seis = 0.60
    ,grid.sete = 0.70
    ,grid.oito = 0.80
    ,grid.nove = 0.90
    ,grid.dez = 1
    ,centre.y = grid.min - ((1/9) *(grid.dez - grid.min))
    ,plot.extent.x.sf = 1
    ,plot.extent.y.sf = 1.2
    ,x.centre.range = 0.02 * (grid.dez - centre.y)
    ,label.centre.y = FALSE
    ,grid.line.width = 0.5
    ,gridline.min.linetype = "longdash"
    ,gridline.um.linetype = "longdash"
    ,gridline.dois.linetype = "longdash"
    ,gridline.tres.linetype = "longdash"
    ,gridline.quatro.linetype = "longdash"
    ,gridline.cinco.linetype = "longdash"
    ,gridline.seis.linetype = "longdash"
    ,gridline.sete.linetype = "longdash"
    ,gridline.oito.linetype = "longdash"
    ,gridline.nove.linetype = "longdash"
    ,gridline.dez.linetype = "longdash"
    ,gridline.min.colour = "grey"
      ,gridline.um.colour = "grey"
      ,gridline.dois.colour = "grey"
      ,gridline.tres.colour = "grey"
      ,gridline.quatro.colour = "grey"
      ,gridline.cinco.colour = "grey"
      ,gridline.seis.colour = "grey"
      ,gridline.sete.colour = "grey"
      ,gridline.oito.colour = "grey"
      ,gridline.nove.colour = "grey"
      ,gridline.dez.colour = "grey"
      ,grid.label.size = 4
    ,gridline.label.offset = -0.1 * (grid.dez - centre.y)
    ,label.gridline.min = TRUE
    ,label.gridline.um = TRUE
    ,label.gridline.dois = TRUE
    ,label.gridline.tres = TRUE
    ,label.gridline.quatro = TRUE
    ,label.gridline.cinco = TRUE
    ,label.gridline.seis = TRUE
    ,label.gridline.sete = TRUE
    ,label.gridline.oito = TRUE
    ,label.gridline.nove = TRUE
    ,label.gridline.dez = TRUE
    ,axis.label.offset = 1.15
    ,axis.label.size = 5
    ,axis.line.colour = "grey"
      ,group.line.width = 1
    ,group.point.size = 3
    ,group.colours = NULL
    ,background.circle.colour = "#D7D6D1"
      ,background.circle.transparency = 0.2
    ,plot.legend =ifelse(nrow(plot.data) >1,TRUE,FALSE)
    ,legend.title = ""
    ,plot.title = ""
    ,plot.title.hjust=.5
    ,plot.title.size=15
    ,legend.text.size = 14
    ,legend.position = "top"
    ,fill = FALSE
    ,fill.alpha = 0.5
    ,graf.limits=c(2,2)
){
  #Pacotes
  {
    carregando_pacotes<-function(){
      packages<-c("dplyr","ggplot2")

      package.check <- lapply(
        packages,
        FUN = function(x) {
          if (!require(x, character.only = TRUE)) {
            install.packages(x, dependencies = TRUE)
            library(x, character.only = TRUE)
          }
        }
      );rm(packages,package.check)
    }
    carregando_pacotes();rm(carregando_pacotes)
    #Funções
    CalculateGroupPath<-function (df){
      path <- df[, 1]
      angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/(ncol(df) -1))
      graphData <- data.frame(seg = "", x = 0, y = 0)
      graphData <- graphData[-1, ]
      for (i in levels(path)) {
        pathData <- subset(df, df[, 1] == i)
        for (j in c(2:ncol(df))) {graphData <- rbind(graphData, data.frame(group = i,x = pathData[, j] * sin(angles[j - 1]), y = pathData[,j] * cos(angles[j - 1])))}
        graphData <- rbind(graphData, data.frame(group = i,x = pathData[, 2] * sin(angles[1]), y = pathData[,2] * cos(angles[1])))
      }
      colnames(graphData)[1] <- colnames(df)[1]
      graphData$group <- factor(graphData$group, levels = levels(df[,1]))
      graphData
    }
    CalculateAxisPath<-function (var.names, min, max){
      n.vars <- length(var.names)
      angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/n.vars)
      min.x <- min * sin(angles)
      min.y <- min * cos(angles)
      max.x <- max * sin(angles)
      max.y <- max * cos(angles)
      axisData <- NULL
      for (i in 1:n.vars) {
        a <- c(i, min.x[i], min.y[i])
        b <- c(i, max.x[i], max.y[i])
        axisData <- rbind(axisData, a, b)
      }
      colnames(axisData) <- c("axis.no", "x", "y")
      rownames(axisData) <- seq(1:nrow(axisData))
      as.data.frame(axisData)
    }
    funcCircleCoords<-function (center = c(0, 0), r = 1, npoints = 100){
      tt <- seq(0, 2 * pi, length.out = npoints)
      xx <- center[1] + r * cos(tt)
      yy <- center[2] + r * sin(tt)
      return(data.frame(x = xx, y = yy))
    }
  }
  #Arrumando o banco
  plot.data <- as.data.frame(plot.data)
  plot.data[,1]<-factor(plot.data[,1],levels = as.character(plot.data[,1]))
  #if (!is.factor(plot.data[, 1])) {plot.data[, 1] <- as.factor(as.character(plot.data[,1]))}
  names(plot.data)[1] <- "group"
  var.names <- colnames(plot.data)[-1]
  plot.extent.x <- (grid.dez + abs(centre.y)) * plot.extent.x.sf
  plot.extent.y <- (grid.dez + abs(centre.y)) * plot.extent.y.sf
  #Verificando erros
  if (length(axis.labels) != ncol(plot.data) - 1) {stop("'axis.labels' contains the wrong number of axis labels",call. = FALSE)}
  if (min(plot.data[, -1],na.rm =TRUE) < centre.y) {stop("plot.data' contains value(s) < centre.y", call. = FALSE)}
  if (max(plot.data[, -1],na.rm =TRUE) > grid.dez) {stop("'plot.data' contains value(s) > grid.max", call. = FALSE)}
  #Colocando a escala (novos valores)
  plot.data.offset <- plot.data
  #plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)] + abs(centre.y)
  plot.data.offset[, 2:ncol(plot.data)] <- plot.data[, 2:ncol(plot.data)]
  #Coordenadas
  {
    group <- NULL
    group$path <- CalculateGroupPath(plot.data.offset)
    axis <- NULL
    axis$path <- CalculateAxisPath(var.names, grid.min ,grid.dez)

    axis$label <- data.frame(text = axis.labels, x = NA, y = NA)
    n.vars <- length(var.names)
    angles <- seq(from = 0, to = 2 * pi, by = (2 * pi)/n.vars)
    axis$label$x <- sapply(1:n.vars, function(i, x) {((grid.dez) * axis.label.offset) * sin(angles[i])})
    axis$label$y <- sapply(1:n.vars, function(i, x) {((grid.dez) * axis.label.offset) * cos(angles[i])})

    gridline <- NULL
    gridline$min$path    <- funcCircleCoords(c(0, 0), grid.min    , npoints = 360)
    gridline$um$path     <- funcCircleCoords(c(0, 0), grid.um     , npoints = 360)
    gridline$dois$path   <- funcCircleCoords(c(0, 0), grid.dois   , npoints = 360)
    gridline$tres$path   <- funcCircleCoords(c(0, 0), grid.tres   , npoints = 360)
    gridline$quatro$path <- funcCircleCoords(c(0, 0), grid.quatro , npoints = 360)
    gridline$cinco$path  <- funcCircleCoords(c(0, 0), grid.cinco  , npoints = 300)
    gridline$seis$path   <- funcCircleCoords(c(0, 0), grid.seis   , npoints = 360)
    gridline$sete$path   <- funcCircleCoords(c(0, 0), grid.sete   , npoints = 360)
    gridline$oito$path   <- funcCircleCoords(c(0, 0), grid.oito   , npoints = 360)
    gridline$nove$path   <- funcCircleCoords(c(0, 0), grid.nove   , npoints = 360)
    gridline$dez$path    <- funcCircleCoords(c(0, 0), grid.dez    , npoints = 360)

    gridline$min$label    <- data.frame(x = 0, y = grid.min    , text = as.character(grid.min   ))
    gridline$um$label     <- data.frame(x = 0, y = grid.um     , text = as.character(grid.um    ))
    gridline$dois$label   <- data.frame(x = 0, y = grid.dois   , text = as.character(grid.dois  ))
    gridline$tres$label   <- data.frame(x = 0, y = grid.tres   , text = as.character(grid.tres  ))
    gridline$quatro$label <- data.frame(x = 0, y = grid.quatro , text = as.character(grid.quatro))
    gridline$cinco$label  <- data.frame(x = 0, y = grid.cinco  , text = as.character(grid.cinco ))
    gridline$seis$label   <- data.frame(x = 0, y = grid.seis   , text = as.character(grid.seis  ))
    gridline$sete$label   <- data.frame(x = 0, y = grid.sete   , text = as.character(grid.sete  ))
    gridline$oito$label   <- data.frame(x = 0, y = grid.oito   , text = as.character(grid.oito  ))
    gridline$nove$label   <- data.frame(x = 0, y = grid.nove   , text = as.character(grid.nove  ))
    gridline$dez$label    <- data.frame(x = 0, y = grid.dez    , text = as.character(grid.dez   ))

  }

  theme_clear <- theme_bw(base_size = base.size) + theme(axis.text.y = element_blank(),axis.text.x = element_blank(), axis.ticks = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.border = element_blank(), legend.key = element_rect(linetype = "blank"))
  if(plot.legend == FALSE){legend.position = "none"}
  #Grid inicial
  base <-ggplot(axis$label) +
    xlab(NULL) +
    ylab(NULL) +
    coord_equal()  +
    geom_text(data = subset(axis$label, axis$label$x < (-x.centre.range)),aes(x = x, y = y, label = text), size = axis.label.size,hjust = 1, family = font.radar) +
    scale_x_continuous(limits = c(-graf.limits[[1]] * plot.extent.x, graf.limits[[1]] * plot.extent.x)) + scale_y_continuous(limits = c(-graf.limits[[2]]/2 * plot.extent.y, graf.limits[[2]]/2 * plot.extent.y))
  #circulos pontilhados
  base<-base+
    #Menor(testando ponto ao inves de circulo)
    geom_point(aes(x=grid.min,y=grid.min),colour=gridline.min.colour)+
    #Um a dez
    geom_path(data = gridline$um$path    , aes(x = x,y = y), lty = gridline.um.linetype    , colour = gridline.um.colour    ,size = grid.line.width)+
    geom_path(data = gridline$dois$path  , aes(x = x,y = y), lty = gridline.dois.linetype  , colour = gridline.dois.colour  ,size = grid.line.width)+
    geom_path(data = gridline$tres$path  , aes(x = x,y = y), lty = gridline.tres.linetype  , colour = gridline.tres.colour  ,size = grid.line.width)+
    geom_path(data = gridline$quatro$path, aes(x = x,y = y), lty = gridline.quatro.linetype, colour = gridline.quatro.colour,size = grid.line.width)+
    geom_path(data = gridline$cinco$path , aes(x = x,y = y), lty = gridline.cinco.linetype , colour = gridline.cinco.colour ,size = grid.line.width)+
    geom_path(data = gridline$seis$path  , aes(x = x,y = y), lty = gridline.seis.linetype  , colour = gridline.seis.colour  ,size = grid.line.width)+
    geom_path(data = gridline$sete$path  , aes(x = x,y = y), lty = gridline.sete.linetype  , colour = gridline.sete.colour  ,size = grid.line.width)+
    geom_path(data = gridline$oito$path  , aes(x = x,y = y), lty = gridline.oito.linetype  , colour = gridline.oito.colour  ,size = grid.line.width)+
    geom_path(data = gridline$nove$path  , aes(x = x,y = y), lty = gridline.nove.linetype  , colour = gridline.nove.colour  ,size = grid.line.width)+
    geom_path(data = gridline$dez$path   , aes(x = x,y = y), lty = gridline.dez.linetype   , colour = gridline.dez.colour   ,size = grid.line.width)
  #Labels envolta do circulo
  base<-base+
    geom_text(data = subset(axis$label, abs(axis$label$x)<=x.centre.range), aes(x = x, y = y, label = text), size = axis.label.size,hjust = 0.5, family = font.radar)+
    geom_text(data = subset(axis$label, axis$label$x >x.centre.range), aes(x = x, y = y, label = text), size = axis.label.size,hjust = 0, family = font.radar)
  #Ajustando o tema
  base <-base+
    theme_clear+
    geom_polygon(data = gridline$dez$path, aes(x,y), fill = background.circle.colour, alpha = background.circle.transparency)+
    geom_path(data = axis$path, aes(x = x, y = y,group = axis.no), colour = axis.line.colour)+
    #Colocando legenda, e linhas coloridas do radar
    geom_path(data = group$path, aes(x = x, y = y,group = group, colour = group), size = group.line.width)+
    #Colocando os "pontos" no rodar (na linha colorida)
    geom_point(data = group$path, aes(x = x,y = y, group = group, colour = group), size = group.point.size)

  #Background dentro do radar colorido?
  if(fill == TRUE){base <- base + geom_polygon(data = group$path, aes(x = x,y = y, group = group, fill = group), alpha = fill.alpha)}

  #Legenda
  if(plot.legend == TRUE){base <- base + labs(colour = legend.title, size = legend.text.size)}
  #Label das linhas pontilhados
  if(label.gridline.min   ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[1]),data = gridline$min$label    , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.um    ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[2]),data = gridline$um$label     , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.dois  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[3]),data = gridline$dois$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.tres  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[4]),data = gridline$tres$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.quatro==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[5]),data = gridline$quatro$label , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.cinco ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[6]),data = gridline$cinco$label  , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.seis  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[7]),data = gridline$seis$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.sete  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[8]),data = gridline$sete$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.oito  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[9]),data = gridline$oito$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.nove  ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[10]),data = gridline$nove$label  , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}
  if(label.gridline.dez   ==TRUE){base<-base  + geom_text(aes(x = x, y = y, label = values.radar[11]),data = gridline$dez$label   , size = grid.label.size * 0.8, hjust = 1, family = font.radar)}

  #label no meio do radar?
  if (label.centre.y == TRUE) {
    centre.y.label <- data.frame(x = 0, y = 0, text = as.character(centre.y))
    base <- base + geom_text(aes(x = x, y = y, label = text),data = centre.y.label, size = grid.label.size, hjust = 0.5, family = font.radar)
  }
  #Cores, fontes e local da legenda
  if(!is.null(group.colours)){colour_values <- rep(group.colours, 100)}else{
    colour_values <- rep(c("#FF5A5F", "#FFB400", "#007A87",
                                    "#8CE071", "#7B0051", "#00D1C1", "#FFAA91", "#B4A76C",
                                    "#9CA299", "#565A5C", "#00A04B", "#E54C20"), 100)
  }
  base <-base +
    #Legenda do tipo ponto com linhas
    theme(legend.key.width = unit(3, "line")) +
    #tamanho e fonte da legenda
    theme(text = element_text(size = legend.text.size, family = font.radar)) +
    #posição da legenda
    theme(legend.text = element_text(size = legend.text.size),legend.position = legend.position)+
    #Cores das linhas com legenda
    theme(legend.key.height = unit(2,"line")) + scale_colour_manual(values = colour_values)+
    #Fonte geral
    theme(text = element_text(family = font.radar))+
    #Legenda com fundo branco
    theme(legend.title = element_blank())
  #Background dentro do radar colorido?
  if(isTRUE(fill)){base <- base + scale_fill_manual(values = colour_values,guide = "none")}
  #Título da legenda
  if(legend.title != ""){base <- base + theme(legend.title = element_text())}
  #Título do radar
  if(plot.title != ""){base <- base + ggtitle(plot.title)+
    theme(plot.title = element_text(hjust = plot.title.hjust,size=plot.title.size))}
  #Fim
  return(base)
}
