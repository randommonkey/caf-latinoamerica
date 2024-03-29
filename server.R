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
  df$c <- round(as.numeric(df$c), 2)
  df$d <- round(as.numeric(df$d),2)
  df$text1 <- map_chr(df$c, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark="."))
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

vizBar <- function(data,
                   title = NULL,
                   subtitle = NULL,
                   caption = NULL,
                   horLabel = NULL,
                   verLabel = NULL,
                   horLine = NULL,
                   horLineLabel = " ",
                   verLine = NULL,
                   verLineLabel = " ",
                   labelWrap = 12,
                   colors = NULL,
                   agg = "sum",
                   orientation = "ver",
                   marks = c(".", ","),
                   nDigits = NULL,
                   dropNa = FALSE,
                   percentage = FALSE,
                   format = c('', ''),
                   order = NULL,
                   showText = TRUE,
                   legendPosition = c("right", "bottom"),
                   tooltip = list(headerFormat = NULL, pointFormat = NULL),
                   export = FALSE,
                   theme = NULL,
                   lang = 'es',
                   ...) {
  
  
  f <- fringe(data)
  nms <- getClabels(f)
  d <- f$d
  
  title <-  title %||% ""
  subtitle <- subtitle %||% ""
  caption <- caption %||% ""
  
  
  labelsXY <- orientationXY(orientation,
                            x = nms[1],
                            y = ifelse(nrow(d) == dplyr::n_distinct(d$a), nms[2], paste(agg, nms[2])),
                            hor = horLabel,
                            ver = verLabel)
  if (dropNa)
    d <- d %>%
    tidyr::drop_na()
  
  d <- d  %>%
    tidyr::replace_na(list(a = ifelse(is.character(d$a), "NA", NA),
                           b = NA)) %>%
    dplyr::group_by(a) %>%
    dplyr::summarise(b = agg(agg, b))
  d$a <- as.character(d$a)
  d$a[is.na(d$a)] <- 'NA'
  
  if (is.null(nDigits)) {
    nDig <- 0
  } else {
    nDig <- nDigits
  }
  
  if (percentage) {
    d$b <- (d[['b']] * 100) / sum(d[['b']], na.rm = TRUE)
  }
  
  d$b <- round(d$b, nDig)
  d <- d %>% arrange(-b) 
  
  
  d <- d %>% plyr::rename(c('b' = 'y'))
  d$color <- colors
  print(d$colors)
  
  data <- list()
  bla <- map(1:nrow(d), function(z){
    data$data[[z]] <<- list("name" = d$a[z],
                            "y" = d$y[z],
                            "color" = as.character(d$color[z]))
  })
  
  formatLabAxis <- paste0('{value:', marks[1], marks[2], 'f}')
  if (!is.null(nDigits)) {
    formatLabAxis <- paste0('{value:', marks[1], marks[2], nDigits, 'f}')
  }
  
  
  if (is.null(format)) {
    format[1] = ""
    format[2] = ""
  }
  
  aggFormAxis <- 'function() {return this.value+"";}'
  
  
  if (percentage) {
    aggFormAxis <- 'function() {return this.value+"%";}'
    format[2] <- "%"
  }
  
  
  aggFormAxis <- paste0("function() { return '", format[1] , "' + Highcharts.numberFormat(this.value, ", nDig, ", '", marks[2], "', '", marks[1], "') + '", format[2], "'}"
  )
  
  
  if (is.null(tooltip$pointFormat)) {
    tooltip$pointFormat <- paste0('<b>{point.name}</b><br/>', paste0(agg, ' ' ,nms[2], ': '), format[1],'{point.y}', format[2])
  }
  if (is.null(tooltip$headerFormat)) {
    tooltip$headerFormat <- ""
  }
  
  hc <- highchart() %>%
    hc_chart(type = ifelse(orientation == "hor", "bar", "column")) %>%
    hc_title(text = title) %>%
    hc_subtitle(text = subtitle) %>%
    hc_plotOptions(
      bar = list(
        colorByPoint = T
      ),
      column = list(
        colorByPoint = T
      )
    ) %>% 
    hc_tooltip(useHTML=TRUE, pointFormat = tooltip$pointFormat, headerFormat = tooltip$headerFormat) %>%
    hc_xAxis(
      title =  list(text = labelsXY[1]),
      type= 'category'
    ) %>%
    hc_yAxis(
      title = list (
        text = labelsXY[2]),
      labels = list (
        format = formatLabAxis,
        formatter = JS(aggFormAxis)
      )
    ) %>%
    hc_series(
      data
    ) %>%
    hc_credits(enabled = TRUE, text = caption) %>%
    hc_legend(enabled = FALSE,
              align= legendPosition[1],
              verticalAlign= legendPosition[2])
  if (export){
    hc <- hc %>%
      hc_exporting(enabled = TRUE, buttons= list(
        contextButton= list(
          menuItems = list('printChart', 'downloadJPEG', 'downloadPNG', 'downloadSVG', 'downloadPDF')
        )
      ))}
  hc  %>% hc_add_theme(custom_theme(custom = hc_theme(
    #colors = c(),
    chart = list(
      backgroundColor = "#FFFFFF",
      style = list(
        color = '#333333',
        fontFamily = "Open Sans",
        textDecoration= 'none'
      )
    )
  )
  ))
}

shinyServer(
  function(input, output, session) {
    
    datV <- reactiveValues(tabElg = 'comparacion')
    
    observe({
      idClick <- input$last_click
      if (is.null(idClick)) idClick <- 'comparacion'
      datV$tabElg <- idClick
      print('click')
      print(datV$tabElg)
    })
    
    
    # Comparación -------------------------------------------------------------
    
    paises <- reactive({
      var1 <- input$selVarUno
      if (is.null(var1)) return()
      var2 <- input$selVarDos
      if (is.null(var2)) return()
      data <- dataSel %>% select_("pais", "ciudad", var1, var2)
      data %>% drop_na()
    })
    
    output$compPais <- renderUI({
      selectizeInput('PaisComp', HTML('<div class="titX">País</div>'), c(`Todos los paises`= '', unique(paises()$pais)), multiple = TRUE, options = list(plugins= list('remove_button')))
    })
   
    
    ciudades <- reactive({
      paisSelc <- input$PaisComp
      if (is.null(paisSelc)){
        d <- paises()
      } else {
      d <- paises() %>% filter(pais %in% paisSelc)
      }
      
      d
    })
 
    
    output$ciudComp <- renderUI({
      selectizeInput('compCiud', '',  c(`Todas las ciudades` = '', ciudades()$ciudad), multiple = TRUE,  options = list(plugins= list('remove_button')))
    })
    
    
    output$tipoVar <- renderUI({
      selectizeInput('typeVar', ', comparación de datos', c('cuantitativos', 'cualitativos'))
    })
    
    output$selecUno <- renderUI({
      varUno <- as.list(setNames(unique(dataComp$variableUno), unique(dataComp$sigUno)))
      selectizeInput('selVarUno', 'de ', varUno)
    })
    
    output$selecDos <- renderUI({
       varUnoElg <- input$selVarUno
       if (is.null(varUnoElg)) return()
       dataVarDs <- dataComp %>% filter(variableUno == varUnoElg)
       print(dataVarDs)
      varDos <- as.list(setNames(unique(dataVarDs$VariableDos), unique(dataVarDs$sigDos)))
      selectizeInput('selVarDos', 'Vs', varDos)
    })
    
    
    baseComp <- reactive({
      
      tipData  <- input$typeVar
      if (is.null(tipData)) return()
      
      if (tipData == 'cuantitativos') tipData <- 'Num'
      if (tipData == 'cualitativos') tipData <- 'Cat'
      
      d <- dic_ob %>% filter(ctypes == tipData) %>% drop_na(label)
      
      if (tipData == 'Cat') {
        datSelC <- data[,d$id] }
      if (tipData == 'Num') {
        datSelC <- data[, c('pais', 'ciudad', d$id)]}
      
      idP <- input$PaisComp
      if (is.null(idP)) idP <- 'Todas'
      
      if (idP == 'Todas') {
        paises <- unique(datSelC$pais)
      } else {
        paises <- idP
      } 
      
      idC <- input$compCiud
      if (is.null(idC)) idC <- 'Todas'
      
      if (idC == 'Todas') {
        ciudades <- unique(datSelC$ciudad)
      } else {
        ciudades <- idC
      } 
      
      df <- datSelC %>% filter(pais %in% paises)
      df <- df %>% filter(ciudad %in% ciudades)
      df <- Filter(function(x) !all(is.na(x)), df)
      df
    })
    
   
    
    
    varCualitativas <- reactive({
      tc <- input$typeVar
      if (is.null(tc)) return()
      
      if (tc == 'cualitativos') {
        varInf <- data.frame(id = names(baseComp()))
        a <- varInf %>% inner_join(dic_ob)
      }
      a
    })
    
    output$varCual <- renderUI({
      
      tc <- input$typeVar
      if (is.null(tc)) return()
      
      vel <- setNames(varCualitativas()$id, varCualitativas()$label)
      acd <- varCualitativas()  %>% drop_na(grupo) %>%
        dplyr::group_by(grupo) %>% nest()
      lista_cat <- purrr::map(seq_along(acd$data), function(i) setNames(acd$data[[i]]$id, acd$data[[i]]$label))
      names(lista_cat) <- acd$grupo
      selectizeInput('idCat', ' de', lista_cat, multiple = TRUE,
                     selected = sample(vel, 4),
                     options = list(plugins= list('remove_button')))
    })
    
    
    varSelc <- reactive({
      print('error1')
      varInf <- names(baseComp())
      varCom <- dataComp %>% distinct(variableUno, .keep_all = TRUE)
      varInfv1 <- data.frame(variableUno = intersect(varCom$variableUno, varInf))
      
      varCom2 <- dataComp %>% distinct(VariableDos, .keep_all = TRUE)
      varInfv2 <- data.frame(VariableDos = intersect(varCom2$VariableDos, varInf))
      
      dataF <- varInfv2 %>% inner_join(dataComp)
      
      varInfv1 %>% inner_join(dataF)
    })
    
    
    output$varComp <- renderUI({
      
      if (input$typeVar == 'cuantitativos'){
        h <- div(class = 'selectorCuanti',
                 uiOutput('selecUno'),
                 uiOutput('selecDos'))}
      if (input$typeVar == 'cualitativos') {
        h <- uiOutput('varCual')
      }
      h
    })
    
    baseComGraf <- reactive({
      df <- baseComp()
      
      if (input$typeVar == 'cuantitativos') {
        d <- df[,c('pais', 'ciudad' ,input$selVarUno, input$selVarDos)]}
      if (input$typeVar == 'cualitativos') {
        d <- df[,c('pais', 'ciudad', input$idCat)]
      }
      
      varInf <- data.frame(id = names(d))
      dicInf <- varInf %>% inner_join(dic_ob)
      names(d) <- dicInf$label
      d
    })          
    
    output$lala <- renderPrint({
      baseComGraf()
    })
    
    output$vizComNum <- renderHighchart({
      if (is.null(input$selVarUno)) return()
      if (is.null(input$selVarDos)) return()
      vizScatter(baseComGraf(), theme = caf_theme)
    })
    
    output$tabCat <- renderDataTable({#renderFormattable({
      
      df <- baseComGraf()
      df <- df %>% select(País = pais, Ciudad = ciudad, everything())
      # df <- df %>% gather(Variables, valor, -ciudad)
      # df <- df %>% spread(ciudad, valor) 
      # Npais <- grep('pais', df$Variables)
      # df <- df[-Npais,]
      # 
      # df$Variables <- paste0('<span style="font-weight: 700;">', df$Variables, '</span>')
      # df <- df %>% plyr::rename(c('Variables' = 'Variables Seleccionadas'))
      
      DT::datatable(df, escape = FALSE, 
                    class = 'cell-border stripe',
                    rownames = FALSE,
                    extensions = c('FixedColumns'),
                    options = list(
                      initComplete = JS(
                        "function(settings, json) {",
                        "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#000'});",
                        "}"),
                      dom = 'tip',
                      searching = FALSE,
                      pageLength = 5,
                      scrollX = 270,
                      scrollY = 270,
                      paging=FALSE,
                      fixedColumns = list(leftColumns = 1, rightColumns = 0),
                      language = list(url = "//cdn.datatables.net/plug-ins/f2c75b7247b/i18n/Spanish.json")
                    )
      )
      
    })
    
    output$vizComparada <- renderUI({
      if (input$typeVar == 'cuantitativos') {
        h <- highchartOutput('vizComNum', width = 800, height = 500) }
      if (input$typeVar == 'cualitativos') {
        h <-  div(dataTableOutput('tabCat'))#formattableOutput('tabCat', width = "800px")
      }
      h
    })
    
    
    output$textCOMP <- renderUI({
      
      idT <- input$typeVar
      
      if (idT == 'cuantitativos') {
        if (is.null(input$selVarUno)) return()
        if (is.null(input$selVarDos)) return()
        des <- HTML(paste0(
          '<p><b>(Eje X) ', dic_ob$label[dic_ob$id == input$selVarUno], ': </b>',
          dic_ob$Descripción[dic_ob$id == input$selVarUno],'.</p><p><b>(Eje Y) ',
          dic_ob$label[dic_ob$id == input$selVarDos], ': </b>',
          dic_ob$Descripción[dic_ob$id == input$selVarDos],'.</p>'
        ))}
      if (idT == 'cualitativos') {
        selectedVarNu <- input$idCat 
        if(is.null(selectedVarNu)) return()
        a <- purrr::map(selectedVarNu, function(z){
          paste0('<p>','<span style="font-weight: bold;">', dic_ob$label[dic_ob$id == z],':','</span>',' ',
                 dic_ob$Descripción[dic_ob$id == z], '</p>')
        }) %>% unlist()  
        des <- HTML(a)
      }
      des
      
    })    
    
    
    # Ranking -----------------------------------------------------------------
    
    output$varRanking <- renderUI({
      selectizeInput('varR', ' de', choices = lista_dim)
    })   
    
    output$textRank <- renderUI({
      if(is.null(input$varR)) return()
      
      HTML(paste0(
        '<p><b>', dic_ob$label[dic_ob$id == input$varR], ': </b>',
        dic_ob$Descripción[dic_ob$id == input$varR] ,'</p><p><b>'
      ))
    })
    
    datRank <- reactive({
      elegV <- input$varR
      
      if (is.null(elegV)) return()
      
      df <- data %>% select_('pais','ciudad', elegV)
      df <- df %>% drop_na()
      df
    })
    
    output$topElg <- renderUI({
      
      nF <- nrow(datRank())
      
      if (is.null(nF)) nF <- 1
      
      if(nF > 5) {
        a <- rep(round(nF/5), 3)
        d <-  c(cumsum(a), nF) }
      if(nF <= 5) {
        d <- nF
      }
      
      topCh <- paste0('Top ', d)
      
      selectInput("topRank", "", choices = topCh)
    })
    
    output$GrafBar <- renderUI({
      
      map(c('horizontal', 'vertical', 'treemap'), function(z){
        tags$button(id = z, class = 'barType', type = "button",
                    tags$img(src = paste0('bar/',z, '.png'))
        )
      })
      
    })
    
    output$vizRank <- renderHighchart({
      
      df <- datRank()
      
      if (is.null(df)) return()
      
      
      varInf <- data.frame(id = names(df))
      dic <- varInf %>% inner_join(dic_ob)
      names(df) <- dic$label
      nrowS <- as.numeric(trimws(gsub('[A-z]', '',input$topRank)))
      df <- df[1:nrowS, ]
      
      color <- left_join(df, paisesColor)
      color <- color %>% select(-pais)
      names(color) <- c('ciudad', 'categoria', 'color')
      
      color <- color %>% arrange(-(categoria)) %>% slice(1:nrowS)
      
      df <- df %>% select(-pais)
      
      idOrie <- input$last_btn
      if (is.null(idOrie)) idOrie <- 'horizontal'
      if (idOrie == 'horizontal') {
        or <- 'hor'
      }
      if (idOrie == 'vertical') {
        or <- 'ver'
      }
      
      
      if (dim(df)[2] <= 2 & idOrie != 'treemap') {
        v <- vizBar(df, sort = 'desc', orientation = or,  verLabel = '', horLabel = '', colors = color$color, tooltip = list(headerFormat = ' ',pointFormat = paste0('<b>Ciudad:</b> {point.name} <br/> <b>', names(df)[2] ,':</b> {point.y:,.1f} ')))}
      if(dim(df)[2] <= 2 & idOrie == 'treemap') {
        v <- hgch_treemap_CatNum(df, maxColor = '#509f27', minColor = '#005186') }
      # if (dim(df)[2] > 2) {
      #   v <- hgch_bar_grouped_CatNumP(df, sort = 'desc', orientation = or, theme = caf_theme, horLabel = '', verLabel = '' )
      # }
      
      v %>%  hc_exporting(enabled = TRUE, buttons= list(
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
    })
    
    
    # Mapa --------------------------------------------------------------------
    
    output$PaisSel <- renderUI({
      selectizeInput('idPais', HTML('<div class="titX">en</div>'), c(`América latina`= '', unique(dataSel$pais)), multiple = TRUE,  options = list(plugins= list('remove_button')))
    })
    
    dataPais <- reactive({
      
      idcS <- input$idPais
      if (is.null(idcS)) {
        df <- dataSel }
      if (!is.null(idcS)) {
        df <- dataSel %>% filter(pais %in% idcS)
        df <- Filter(function(x) !all(is.na(x)), df)
      }
      df
    })
    
    datCiu <- reactive({
      bd_ciud <- dataPais() %>%
        distinct(ciudad, .keep_all = TRUE) %>% select(pais, ciudad)%>% drop_na()
      bd_ciud$ciudad
    })
    
    
    output$ciudadSel <- renderUI({
      selectizeInput('idCiud', '',  c(`Todas las ciudades` = '',datCiu()), multiple = TRUE,  options = list(plugins= list('remove_button')))
    })
    
    
    baseMap <- reactive({
      
      idP <- input$idPais
      idC <- input$idCiud
      
      if (is.null(idP) & is.null(idC)) {
        df <- dataSel }
      if (!is.null(idP) & is.null(idC)) {
        df <- dataSel %>% filter(pais %in% idP) }
      if (!is.null(idP) & !is.null(idC)){
        df <- dataSel %>% filter(pais %in% idP, ciudad %in% idC)}
      if (is.null(idP) & !is.null(idC)) {
        df <- dataSel %>% filter(ciudad %in% idC)
      }
      df <- Filter(function(x) !all(is.na(x)), df)
      df
      
    })
    
    baseDim <- reactive({
      varInf <- data.frame( id = names(baseMap()))
      dicS <- varInf %>% left_join(dic_sel) %>% drop_na()
      acd <- dicS  %>% drop_na(grupo) %>%
        dplyr::group_by(grupo) %>% nest()
      lista_dim <- purrr::map(seq_along(acd$data), function(i) setNames(acd$data[[i]]$id, acd$data[[i]]$label))
      names(lista_dim) <- acd$grupo
      lista_dim
    })
    
    output$menuInd <- renderUI({
      selectizeInput('indSel', HTML('<div class="titX">Distribución geográfica</div>'), baseDim())
    })
    
    # 
    # 
    output$descripcion <- renderUI({
      if(is.null(input$indSel)) return()
      
      HTML(paste0(
        '<p><b>', dic_ob$label[dic_ob$id == input$indSel], ': </b>',
        dic_ob$Descripción[dic_ob$id == input$indSel] ,'</p><p><b>'
      ))
    })
    
    
    dataBubble <- reactive({
      varS <- input$indSel
      if (is.null(varS)) return()
      datM <- baseMap() %>% left_join(codigos)
      df1 <- datM %>% select(name = ciudad, lat, lon, z = varS) %>% drop_na(z)
      df1$label <- dic_ob$label[dic_ob$id == varS]
      df1$w <- map_chr(df1$z, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ","))
      df1
    })
    
    bla <- reactive({
      lstB <- input$last_click
      if (is.null(lstB)) return()
      if (lstB == 'mapa') {
        varS <- input$indSel
        df0 <- baseMap() %>% left_join(codigos) %>% select(name = country_name) %>% distinct()
        df0$z <- 1
        
        h <- highchart(type = "map") %>%
          hc_chart(backgroundColor = "transparent", 
                   style = list(
                     fontFamily= 'Open Sans'
                   )) %>%
          # hc_plotOptions(
          #     series = list(
          #       dataLabels = list(
          #         allowOverlap = TRUE
          #       )
          #     )
          # ) %>%
          highcharter::hc_add_series_map(map = mapLam, showInLegend = FALSE, nullColor = "#f7f7f7", borderWidth = 1,
                                         df = df0,  value = "z", joinBy = "name",
                                         tooltip= list(
                                           headerFormat= '',
                                           pointFormat='<b>{point.name}</b>'
                                         )) %>%
          hc_colorAxis(maxColor = "#f7f7f7", minColor = "#f7f7f7") %>%
          hc_legend(enabled = FALSE) %>%
          hc_add_series(data = dataBubble(), type = "mapbubble",
                        dataLabels= list(
                          enabled= TRUE,
                          color= '#000',
                          format = '{point.name}',
                          style= list(
                            fontWeight = 'bold',
                            textShadow = FALSE,
                            fontFamily = 'Open Sans',
                            textOutline = FALSE
                          )),
                        allowPointSelect = TRUE,
                        allowOverlap = TRUE,
                        cursor = 'pointer', minSize = '3%',
                        maxSize = 30,
                        color = "#005186",
                        tooltip= list(
                          headerFormat= '',
                          pointFormat='<b>{point.name}</b><br>
                          <b>{point.label}: </b>{point.w}</>'
                        )) %>%
          hc_mapNavigation(enabled = TRUE,
                           buttonOptions = list(align = 'left',
                                                verticalAlign = 'top')
          ) %>%
          hc_exporting(enabled = TRUE, buttons= list(
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
      }
      
      h
    })
    
    output$siglasMapa <- renderUI({
      if (mean(dataBubble()$z) == 1) return()
      
      varS <- trimws(input$indSel)
      if(is.null(varS)) return()
      
      a <- HTML(dic_ob$Unidad[dic_ob$id == varS])
      if (is.na(a)) a <- ''
      
      div(class = 'ContSigla',
          div(class = 'circulo', ''),
          HTML(paste0('<span>', unique(dataBubble() %>%  filter(z == max(dataBubble()$z)) %>%  .$w), ' <span>', a, '</span></span>')),
          div(class = 'circuloPequ', ''),
          HTML(paste0('<span>', unique(dataBubble() %>%  filter(z == min(dataBubble()$z)) %>%  .$w), ' <span>', a, '</span></span>'))
      )
      # 
      # print(dataBubble())
    })
    
    output$MapaGraf <- renderHighchart({
      
      
      h <- bla() #%>% 
      # hc_add_series(data = df, type = "mapbubble",
      #               minSize = 0, maxSize = 30)
      h
    })
    
    output$MapaViz <- renderUI({
      highchartOutput('MapaGraf', width = 850, height = 590)
    })
    
    observeEvent(input$ClearComp, {
      shinyjs::reset("compPais")
      shinyjs::reset("ciudComp")
      shinyjs::reset("tipoVar")
      shinyjs::reset("varComp")
    })
    
    
    observeEvent(input$ClearMap, {
      shinyjs::reset("menuInd")
      shinyjs::reset("PaisSel")
      shinyjs::reset("ciudadSel")
    })
    
    
    observeEvent(input$ClearRank, {
      shinyjs::reset("topElg")
      shinyjs::reset("varRanking")
      shinyjs::reset("GrafBa")
    })
    
    
    output$resultado <- renderUI({
      
      lstB <- input$last_click
      
      if (is.null(lstB)) lstB <- 'comparacion'
      
      if ( lstB == 'comparacion') {
        r <-   list(
          HTML('<h4 style="margin-left:3%;font-weight: 600;">En esta sección encontrará información de transporte de 56 ciudades de Latinoamérica, permitiendole
               hacer comparaciones de variables cuantitativas y cualitativas de las ciudades.</h5>'),
          div(class = 'titulo',
              uiOutput('compPais'),
              uiOutput('ciudComp'),
              uiOutput('tipoVar'),
              uiOutput('varComp'),
              div( class = 'cleanBut',
                   actionButton('ClearComp', icon("erase", lib = "glyphicon")))),
          div(class = 'temCont',
              div(class = 'contViz',
                  uiOutput('vizComparada')),
              div(class = 'ficha', id = 'styleScroll',
                  uiOutput('textCOMP')))
          )}
      if (lstB == 'mapa') {
        r <- list(
          div(class = 'titulo',
              uiOutput('menuInd'),
              uiOutput('PaisSel'),
              uiOutput('ciudadSel'),
              div( class = 'cleanBut',
                   actionButton('ClearMap', icon("erase", lib = "glyphicon"), class = 'cleanBut'))),
          div(class = 'temCont',
              #div(class = 'contViz',
              div(class = 'mapaStyle',
                  uiOutput('MapaViz'),
                  uiOutput('siglasMapa')
              ),#, width = 800, height = 530)),
              div(class = 'ficha', id = 'styleScroll',
                  uiOutput('descripcion'))
          )
        )}
      if (lstB == 'ranking') {
        r <- list(
          div(class = 'titulo',
              uiOutput('topElg'),
              uiOutput('varRanking'),
              uiOutput('GrafBar'),
              div( class = 'cleanBut',
                   actionButton('ClearRank', icon("erase", lib = "glyphicon"), class = 'cleanBut'))),
          div(class = 'temCont',
              div(class = 'contViz',
                  highchartOutput('vizRank', width = 800, height = 500)),
              div(class = 'ficha', id = 'styleScroll',
                  uiOutput('textRank')))
        )
      }
      
      list(r,
           div(class = 'styBut',
               downloadButton('idFilterCSV', 'Descarga datos de filtros'),
               downloadButton('idDownCSV', 'Descarga todos los datos'))
      )
    })  
    
    
    
    
    output$idDownCSV <- downloadHandler(
      "all_data.zip",
      content = function(file) {
        dir.create(tmp <- tempfile())
        write_csv(data, file.path(tmp, "data_all.csv"))
        write_csv(data, file.path(tmp, "dic_all.csv"))
        rio::export(data, file.path(tmp, "dic_all.xlsx"))
        zip(file, tmp)
      })  
    
    output$idFilterCSV <- downloadHandler(
      "filter_data.zip",
      content = function(file) {
        
        lstB <- input$last_click
        
        if (is.null(lstB)) lstB <- 'comparacion'
        
        if ( lstB == 'comparacion') {
          
          df <- baseComp()
          
          if (input$typeVar == 'cuantitativos') {
            d <- df[,c('pais', 'ciudad' ,input$selVarUno, input$selVarDos)]}
          if (input$typeVar == 'cualitativos') {
            d <- df[,c('pais', 'ciudad', input$idCat)]
          }
          
          varInf <- data.frame(id = names(d))
          dic <- varInf %>% inner_join(dic_ob)
          dir.create(tmp <- tempfile())
          write_csv(d, file.path(tmp, "data_filter.csv"), na = '')
          write_csv(dic, file.path(tmp, "dic_filter.csv"), na = '')}
        if ( lstB == 'mapa') {
          dir.create(tmp <- tempfile())
          df <- baseMap()
          if (is.null(df)) return()
          varInf <- data.frame(id = names(df))
          dic <- varInf %>% inner_join(dic_ob)
          write_csv(df, file.path(tmp, "data_filter.csv"), na = '')
          write_csv(dic, file.path(tmp, "dic_filter.csv"), na = '') }
        if ( lstB == 'ranking' ) {
          dir.create(tmp <- tempfile())
          df <- datRank()
          if (is.null(df)) return()
          varInf <- data.frame(id = names(df))
          dic <- varInf %>% inner_join(dic_ob)
          write_csv(df, file.path(tmp, "data_filter.csv"), na = '')
          write_csv(dic, file.path(tmp, "dic_filter.csv"), na = '')
        }
        zip(file, tmp)
      })
    
  })
