library(shiny)
library(shinyjs)
library(hgchmagic)
library(DT)
#library(formattable)
library(tidyverse)
library(zip)

hcoptslang <- getOption("highcharter.lang")
hcoptslang$contextButtonTitle <- 'Descargar Imagen'
hcoptslang$printChart <- "Imprimir Gráfico"
hcoptslang$downloadJPEG <- "Descarga en JPEG"
hcoptslang$downloadPNG <- "Descarga en PNG"
hcoptslang$downloadPDF <- "Descarga en PDF"
hcoptslang$downloadSVG <- "Descarga en SVG"
hcoptslang$thousandsSep <- "."
hcoptslang$decimalPoint <- ","
# hcoptslang$downloadCSV <- "Descarga en CSV"
# hcoptslang$downloadXLS <- "Descarga en XLS"



options(highcharter.lang = hcoptslang)

data <- read_csv('data/clean/movilidad_latam_data.csv')

dic_ob <- read_csv('data/clean/movilidad_latam_dic_.csv')
dic_ob$ctypes[dic_ob$id == 'year_commenced'] <- 'Cat'

dic_sel <- dic_ob %>% filter(ctypes == 'Num')
dataSel <- data[,c('pais', 'ciudad', dic_sel$id)]

mapLam <- jsonlite::fromJSON("data/latin-america.json", simplifyVector = FALSE)
codigos <- read_csv('data/clean/codigos.csv')

dataComp <- read_csv('data/clean/comparaciones.csv')

acd <- dic_sel  %>% drop_na(grupo) %>%
  dplyr::group_by(grupo) %>% nest()

lista_dim <- purrr::map(seq_along(acd$data), function(i) setNames(acd$data[[i]]$id, acd$data[[i]]$label))
names(lista_dim) <- acd$grupo


# Thema -------------------------------------------------------------------

caf_theme <- hc_theme(
  colors = c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2'),
  chart = list(
    backgroundColor = "transparent"
  ),
  title = list(
    style = list(
      color = '#333333',
      fontFamily = "Open Sans",
      textDecoration= 'none'
    )
  ),
  bar = list(
    colorByPoint = T
  ),
  column = list(
    colorByPoint = T
  ),
  
  legend = list(
    itemStyle = list(
      fontFamily = '',
      color = 'black'
    ),
    itemHoverStyle = list(
      color = 'gray'
    )
  )
)

options(scipen=999)

count_pl <- function(x) {
  if(is.na(x)){return(0)}
  
  if ((x %% 1) != 0) {
    nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
  } else {
    return(0)
  }
}


vizScatter <- function(data, title = NULL, subtitle = NULL, caption = NULL, horLabel = NULL, verLabel = NULL, theme = NULL, export = TRUE,...){
  
  #data <- sampleData('Cat-Cat-Num-Num')
  if(class(data)[1] == "Fringe"){
    ni <- getClabels(data)
  }else{
    ni <- names(data)
  }
  
  x <- ni[3]
  y <- ni[4]
  
  f <- fringe(data)
  nms <- getClabels(f)
  
  horLabel <-horLabel %||% getClabels(f)[3]
  verLabel <- verLabel %||% getClabels(f)[4]
  title <-  title %||% ""
  
  df <- f$d %>% drop_na()  %>% dplyr::group_by(a,b) #%>%
  #dplyr::summarise(c = c, d = d)
  df$a <- as.character(df$a)
  df$c <- as.numeric(df$c)
  df$d <- as.numeric(df$d)
  df$text1 <- map_chr(df$c, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=","))
  df$text2 <- map_chr(df$d, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ","))
  df$z <- 1
  hc <- hchart(df, type = "bubble", hcaes(x = c, y = d, z = z,group = a),  maxSize = 15, marker= list(
    fillOpacity=1)) %>%
    hc_xAxis(title = list(text=horLabel), gridLineWidth= 1) %>%
    hc_yAxis(title = list(text=verLabel)) %>%
    hc_tooltip(
      headerFormat= '',
      pointFormat = paste0("<br><strong>{point.b} - {point.a}</strong><br>",
                           horLabel, ": {point.text1} <br>",
                           verLabel, ": {point.text2}"))
  hc <- hc %>% hc_add_theme(custom_theme(custom=theme))
  if(export) hc <- hc %>% hc_exporting(enabled = TRUE, buttons= list(
    contextButton= list(
      symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
      height= 30,
      width= 33,
      symbolSize= 24,
      symbolX= 30,
      symbolY= 30,
      menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
    )
  ))
  hc
}




# vizMapa <- function(dfSerie, dfBub, export = TRUE,...){
#   
#   
#   hc <-  highchart(type = "map") %>%
#     hc_chart(backgroundColor = "transparent",
#              style = list(
#                fontFamily= 'Open Sans'
#              )) %>%
#     hc_add_series_map(map = mapLam, showInLegend = FALSE, nullColor = "#f7f7f7", borderWidth = 1,
#                       df = dfSerie,  value = "z", joinBy = "name", allowPointSelect = TRUE,
#                       tooltip= list(
#                         headerFormat= '',
#                         pointFormat='<b>{point.name}</b>'
#                       )) %>%
#     hc_colorAxis(maxColor = "#f7f7f7", minColor = "#f7f7f7") %>% 
#     hc_legend(enabled = FALSE) %>% 
#     hc_add_series(data = dfBub, type = "mapbubble",
#                   dataLabels= list(
#                     enabled= TRUE,
#                     color= '#000',
#                     format = '{point.name}',
#                     style= list(
#                       fontWeight = 'bold',
#                       textShadow = FALSE,
#                       fontFamily = 'Open Sans',
#                       textOutline = FALSE
#                     )),
#                   allowPointSelect = TRUE,
#                   cursor = 'pointer', minSize = '3%',
#                   maxSize = 30,
#                   color = "#005186",
#                   tooltip= list(
#                     headerFormat= '',
#                     pointFormat='<b>{point.name}</b><br>
#                     <b>{point.label}: </b>{point.w}</>'
#                   )) %>% 
#     hc_mapNavigation(enabled = TRUE,
#                      buttonOptions = list(align = 'left',
#                                           verticalAlign = 'top')
#     ) 
#   if(export) hc <-
#     hc %>% hc_exporting(enabled = TRUE, buttons= list(
#       contextButton= list(
#         symbol= 'url(https://cdn1.iconfinder.com/data/icons/feather-2/24/download-32.png)',
#         height= 30,
#         width= 33,
#         symbolSize= 24,
#         symbolX= 30,
#         symbolY= 30,
#         menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
#       )
#     ))
#   
#   hc
# }



# thema barras por país

paisesColor <- data.frame(pais = unique(data$pais), color = c('#0b356D', '#3F8909', '#ACA9A9','#CD7031','#1670D2', '#c9d4e2', '#549293', '#915392', '#3f1f25', '#870E25', '#68A399', '#2a5F06', '#3F1F3F', '#DD7589', '#526643'))


