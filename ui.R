shinyUI(
fluidPage(
  useShinyjs(),
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$img(src = 'Cargando.gif', class="loadmessage")),
  tags$head(
    tags$link(rel="stylesheet", type="text/css", href="style.css"),
    includeScript("js/iframeSizer.contentWindow.min.js"),
    includeScript("js/caf.js")
  ),
 # div(id = 'barraNav', includeHTML('lateralMenu.R')),
 HTML( paste0('
<div class="caf-container">
              <nav class="caf-nav">
              <a href="#" id="mapa" class="caf-nav__item tab-manager">Mapa</a>
              <a href="#" id="ranking" class="caf-nav__item tab-manager">Ranking</a>
              <a href="#" id="comparacion" class="caf-nav__item caf-nav__item--active tab-manager">Comparación</a>
              </nav>
              <div class="caf-content">',
        uiOutput('resultado'),
    '</div>
 </div>
        '
   )
  )
 )
)