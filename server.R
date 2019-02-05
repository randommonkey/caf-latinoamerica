
shinyServer(
  function(input, output, session) {
    
    datV <- reactiveValues(tabElg = 'comparacion')
    
    observe({
      idClick <- input$last_click
      if (is.null(idClick)) idClick <- 'comparacion'
      datV$tabElg <- idClick
      print(datV$tabElg)
    })
    
    # Comparación -------------------------------------------------------------
    
    
    output$compPais <- renderUI({
      selectizeInput('PaisComp', HTML('<div class="titX">País</div>'), c(`Todos los paises`= '', unique(data$pais)), multiple = TRUE, options = list(plugins= list('remove_button')))
    })
    
    dataCompPais <- reactive({
      idcS <- input$PaisComp
      
      varComp <- unique(c(unique(dataComp$variableUno), unique(dataComp$VariableDos)))
      if (is.null(idcS)) {
        df <- data 
        df <- df[,c('pais', 'ciudad', varComp)]}
      if (!is.null(idcS)) {
        df <- data[,c('pais', 'ciudad', varComp)]
        df <- df %>% filter(pais %in% idcS)
        df <- Filter(function(x) !all(is.na(x)), df)
      }
      df
    })
    
    
    datCompCiudad <- reactive({
      data <- dataCompPais()
      if (is.null(data)) return()
      bd_ciud <- data %>%
        distinct(ciudad, .keep_all = TRUE) %>% select(pais, ciudad) %>% drop_na()
      bd_ciud$ciudad
    })
    
    output$ciudComp <- renderUI({
      selectizeInput('compCiud', '',  c(`Todas las ciudades` = '', datCompCiudad()), multiple = TRUE,  options = list(plugins= list('remove_button')))
    })
    
    output$tipoVar <- renderUI({
      selectizeInput('typeVar', ', comparación de datos', c('cuantitativos', 'cualitativos'))
    })
    
    
    baseComp <- reactive({
      
      idP <- input$PaisComp
      idC <- input$compCiud
      
      tipData  <- input$typeVar
      if (tipData == 'cuantitativos') tipData <- 'Num'
      if (tipData == 'cualitativos') tipData <- 'Cat'
      
      d <- dic_ob %>% filter(ctypes == tipData) %>% drop_na(label)
      d$id
      if (tipData == 'Cat') {
        datSelC <- data[,d$id] }
      if (tipData == 'Num') {
        datSelC <- data[, c('pais', 'ciudad', d$id)]}
      datSelC
      if (is.null(idP) & is.null(idC)) {
        df <- datSelC }
      if (!is.null(idP) & is.null(idC)) {
        df <- datSelC %>% filter(pais %in% idP) }
      if (!is.null(idP) & !is.null(idC)){
        df <- datSelC %>% filter(pais %in% idP, ciudad %in% idC)}
      if (is.null(idP) & !is.null(idC)) {
        df <- datSelC %>% filter(ciudad %in% idC)
      }
      df <- Filter(function(x) !all(is.na(x)), df)
      df
    })
    
    varCualitativas <- reactive({
      
      if (input$typeVar == 'cualitativos') {
        
        varInf <- data.frame(id = names(baseComp()))
        a <- varInf %>% inner_join(dic_ob)
      } else {
        return()
      }
      a
    })
    
    output$varCual <- renderUI({
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
      varInf <- names(baseComp())
      varCom <- dataComp %>% distinct(variableUno, .keep_all = TRUE)
      varInfv1 <- data.frame(variableUno = intersect(varCom$variableUno, varInf))
      
      varCom2 <- dataComp %>% distinct(VariableDos, .keep_all = TRUE)
      varInfv2 <- data.frame(VariableDos = intersect(varCom2$VariableDos, varInf))
      
      dataF <- varInfv2 %>% inner_join(dataComp)
      
      varInfv1 %>% inner_join(dataF)
    })
    
    output$selecUno <- renderUI({
      if (is.null(varSelc)) return()
      varsu <- varSelc() %>% distinct(variableUno, .keep_all = TRUE)
      selVar1 <- as.list(setNames(varsu$variableUno, varsu$sigUno))
      selectizeInput('selVarUno', 'de ', selVar1)
    })
    
    output$selecDos <- renderUI({
      varSu <- varSelc() %>% filter(variableUno == input$selVarUno)
      varsu <- varSu %>% distinct(VariableDos, .keep_all = TRUE)
      selVar1 <- as.list(setNames(varsu$VariableDos, varsu$sigDos))
      selectizeInput('selVarDos', 'Vs', selVar1)
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
      
      df <- data %>% select_('ciudad', elegV)
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
      
      
      idOrie <- input$last_btn
      if (is.null(idOrie)) idOrie <- 'horizontal'
      if (idOrie == 'horizontal') {
        or <- 'hor'
      }
      if (idOrie == 'vertical') {
        or <- 'ver'
      }
      
      
      if (dim(df)[2] <= 2 & idOrie != 'treemap') {
        #df[,2] <- round(df[,2], 1)
        v <- hgch_bar_CatNum(df, sort = 'desc', orientation = or,  verLabel = '', colors = rep('#0b356D', nrow(df)), tooltip = list(headerFormat = ' ',pointFormat = paste0('<b>Ciudad:</b> {point.a} <br/> <b>', names(df)[2] ,':</b> {point.b:,.1f} ')))  }
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
      datM <- baseMap() %>% left_join(codigos)
      df1 <- datM %>% select(name = ciudad, lat, lon, z = varS) %>% drop_na(z)
      df1$label <- dic_ob$label[dic_ob$id == varS]
      df1$w <- map_chr(df1$z, function(x) format(round(x,2), nsmall=(ifelse(count_pl(x)>2, 2, 0)), big.mark=".", decimal.mark = ","))
      df1
    })
    
    bla <- reactive({
      lstB <- input$last_click
      
      if (lstB == 'mapa') {
        varS <- input$indSel
      df0 <- baseMap() %>% left_join(codigos) %>% select(name = country_name) %>% distinct()
      df0$z <- 1
      
      h <- highchart() %>%
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
    
    
    
    output$MapaGraf <- renderHighchart({

      
      h <- bla() #%>% 
        # hc_add_series(data = df, type = "mapbubble",
        #               minSize = 0, maxSize = 30)
      h
    })
    
    output$MapaViz <- renderUI({
      highchartOutput('MapaGraf', width = 750, height = 570)
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
              uiOutput('MapaViz'),#),#, width = 800, height = 530)),
              div(class = 'ficha', id = 'styleScroll',
                  uiOutput('descripcion')))
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
