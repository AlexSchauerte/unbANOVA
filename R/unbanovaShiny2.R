#' Start the shiny App
#'
#' This function starts the shiny gui.
#' @keywords anova
#' @export
#' @import data.table
#' 
unbanovaShiny2 <- function(){
library(data.table)
  
#### modules ####
widgets_UI <- function(id) {
   ns <- shiny::NS(id)

   shiny::fluidRow(
     shiny::column(4, shinyWidgets::switchInput(ns("showMeans_graph"), value = TRUE, inline = TRUE, size = "small", offStatus = "dark"), "show marginal means"),
     shiny::column(4, shinyWidgets::switchInput(ns("showEffects_graph"), value = FALSE, inline = TRUE, size = "small", offStatus = "dark"), "show effects"),
     shiny::column(4, shinyWidgets::switchInput(ns("showWeights_graph"), value = TRUE, inline = TRUE, size = "small", offStatus = "dark"), "show frequencies")
   )
}

widgets <- function(input, output, session) {
  temp <- shiny::reactiveValues()

  shiny::observe({
    temp$showMeans_graph <- if(is.null(input$showMeans_graph)){TRUE}else{input$showMeans_graph}
    temp$showEffects_graph <- if(is.null(input$showEffects_graph)){FALSE}else{input$showEffects_graph}
    temp$showWeights_graph <- if(is.null(input$showWeights_graph)){TRUE}else{input$showWeights_graph}
  })

  return(temp)
}


shiny::runApp(
shiny::shinyApp(
#### ui ####
ui = shiny::fluidPage(
  shiny::tags$head( 
    shiny::tags$style(shiny::HTML("
      .dropdown:hover>.dropdown-menu {
        display: block;
      }

      .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary {background-color: #e95420}

      #SumOfSquares>table , #MarginalMeans>table , #Effects>table{
        table-layout: fixed;
        border-bottom: 2px solid #cccccc;
        border-top: 2px solid #cccccc;
        float: right;
      }

      .th {
        width:20%;
      }

      #means_table>table>tbody td:first-child {
        font-weight: bold;
      }

      #freqs_table>table>tbody td:first-child {
        font-weight: bold;
      }

      #freqs_table>table>tbody tr:last-child td {
        border-top: 3px solid #cccccc;
      }

      #freqs_table>table td:last-child , #freqs_table>table th:last-child {
        border-left: 3px solid #cccccc;
      }

      #if_means_table>table>tbody td:first-child {
        font-weight: bold;
      }
      
      
    ")),
    shiny::tags$script(src = "https://kit.fontawesome.com/9b266d4589.js", crossorigin = "anonymous")
  ),
  theme = shinythemes::shinytheme("united"),
  shinyjs::useShinyjs(),
  #bsplus::use_bs_tooltip(),
  #shinythemes::themeSelector(),
  shiny::titlePanel("unbANOVA"),
  shiny::sidebarLayout(
    #### . sidebar ####
    shiny::sidebarPanel(
      shiny::tabsetPanel(id = "inputs",
        shiny::tabPanel("Theoretical Analysis",
          shiny::tabsetPanel(id = "theoretical_analysis",
            shiny::tabPanel("load in sets [optional]",
              shiny::tabsetPanel(
                shiny::tabPanel("examples",
                    shiny::br(),
                    shiny::selectInput("exampledata", label = NULL, choices = list("example data" = "",
                                                                                   "2 x 2" = "two_x_two",
                                                                                   "3 x 2 x 3" = "three_x_two_x_three",
                                                                                   "Maxwell, Delaney & Kelley (2017; p.361)" = "MaxDelKel2017_p361",
                                                                                   "Maxwell, Delaney & Kelley (2017; p.376)" = "MaxDelKel2017_p376")
                    ),
                    shiny::uiOutput("source")
                ),
                shiny::tabPanel("upload set",
                    shiny::br(),
                    shiny::fileInput("importset", NULL, accept = ".rds", width = "100%"),
                )
              )                      
            ),
            shiny::tabPanel("Levels",
              shiny::br(),
              shiny::numericInput("fact_1", paste0("number of levels of treatment X"), 2, min = 2, max = 5, step = 1),
              shiny::hr(),
              shiny::numericInput("fact_2", shiny::HTML(paste0("number of levels of covariate K", shiny::tags$sub(1))), 2, min = 2, max = 5, step = 1),
              lapply(3:6, function(i){
                shinyjs::hidden(shiny::numericInput(paste0("fact_",i), shiny::HTML(paste0("number of levels of covariate K", shiny::tags$sub(i - 1))), 2, min = 2, max = 5, step = 1))
              }),
              shiny::div(style = "float:right;",
                tippy::tippy(shinyjs::hidden(shiny::actionLink("hide", NULL, icon = shiny::icon("minus-square"))), content = "remove covariate", placement = "left", animation = "shift-away", arrow = TRUE),
                shiny::HTML("&nbsp;&nbsp;&nbsp;"),
                tippy::tippy(shiny::actionLink("add", NULL, icon = shiny::icon("plus-square")), content = "add covariate", placement = "right", animation = "shift-away", arrow = TRUE)
              )
            ),
            shiny::tabPanel("Means & Frequencies",
              shiny::br(),
              shiny::strong("means:"),
              rhandsontable::rHandsontableOutput("matrixmeans"),
              shiny::hr(),
              shiny::strong("freqs:"),
              rhandsontable::rHandsontableOutput("matrixfreqs"),
              shiny::hr(),
              shiny::downloadButton("downloadable_data"),
              shiny::hr(),
              shiny::actionButton("analyse_theoretical", "Analyse", icon = icon("calculator"))
        ))),
        shiny::tabPanel("Empirical Analysis",
          shiny::br(),
          shiny::fileInput("importempiricaldata", NULL, accept = ".csv", width = "100%"),
          shiny::uiOutput("empiricalimportUI")                   
        ),
        shiny::tabPanel("", icon = shiny::icon("ellipsis-h"),
          shiny::br(),
          shiny::div(shinyWidgets::switchInput("showMeans", value = TRUE, inline = TRUE, size = "mini", offStatus = "dark"), " show marginal means"),
          shiny::div(shinyWidgets::switchInput("showEffects", inline = TRUE, size = "mini", offStatus = "dark"), " show effects"),
          shiny::uiOutput("referenceGroup"),
          shiny::div(shinyWidgets::switchInput("showSS", value = TRUE, inline = TRUE, size = "mini"), " show sum of squares"),
          shiny::hr(),
          shiny::div(shinyWidgets::switchInput("includeATE", value = FALSE, inline = TRUE, size = "mini", offStatus = "dark"), " include ATE")
        )
      )
    ),
    #### . mainPanel ####
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Overview",
          shiny::h3("summary"),
          shiny::fluidRow(shiny::column(6, shiny::textOutput("design")), shiny::column(6, shiny::uiOutput("overviewIsBalanced"))),
          shiny::fluidRow(shiny::column(6, shiny::div("number of cells: ", shiny::textOutput("countcells", inline = TRUE))), shiny::column(6, shiny::uiOutput("overviewIsProportional"))),
          shiny::fluidRow(shiny::column(6, shiny::div("N = ", shiny::textOutput("sumfreq", inline = TRUE))), shiny::column(6, shiny::uiOutput("overviewIsInteractionfree"))),
          shiny::h3("means"),
          shiny::tableOutput("means_table"),
          shiny::h3("frequencies"),
          shiny::tableOutput("freqs_table"),
          shiny::h3("X-K-interactionfree means (estimated)"),
          shiny::tableOutput("if_means_table")
        ),
        #shiny::tabPanel("PrintResult",
        #  shiny::verbatimTextOutput("printresult")
        #),
        shiny::tabPanel("Results",
          shiny::uiOutput("marginalMeansDisplay"),
          shiny::uiOutput("effectsDisplay"),
          shiny::uiOutput("SSDisplay"),
          shiny::uiOutput("Attributes")
        ),
        shiny::tabPanel("Empirical Results",
          shiny::uiOutput("empiricalresults")
        ),
        #shinyWidgets::dropMenu
        shiny::navbarMenu("Graphs", icon = shiny::icon("chart-bar", "fa"),
          shiny::tabPanel("ANOVA I", shiny::h2("ANOVA I"), shinycssloaders::withSpinner(shiny::plotOutput("graph_anova1")), widgets_UI("graphoptions_1"), shiny::downloadButton("dl_anova1_graph", label = "Download", style = "float:right;")),
          shiny::tabPanel("ANOVA II", shiny::h2("ANOVA II"), shinycssloaders::withSpinner(shiny::plotOutput("graph_anova2")), widgets_UI("graphoptions_2"), shiny::downloadButton("dl_anova2_graph", label = "Download", style = "float:right;")),
          shiny::tabPanel("ANOVA III", shiny::h2("ANOVA III"), shinycssloaders::withSpinner(shiny::plotOutput("graph_anova3")), widgets_UI("graphoptions_3"), shiny::downloadButton("dl_anova3_graph", label = "Download", style = "float:right;")),
          shiny::tabPanel("ATE", shiny::h2("ATE"), shinycssloaders::withSpinner(shiny::plotOutput("graph_ate")), widgets_UI("graphoptions_4"), shiny::downloadButton("dl_ate_graph", label = "Download", style = "float:right;"))
        )
      )
    )
)),
#### server ####
server = function(input, output, session){
  session$onSessionEnded(stopApp)

  actions <- reactiveValues(freshstart = TRUE,
                            freshupload = FALSE,
                            freshchangeoflevels = FALSE)
  
  mode <- reactiveVal()
  
  #### . input stuff ####
  nrofcov <- shiny::reactiveVal(value = 1)

  shiny::observeEvent({
    input$add
  },{
    if(nrofcov() == 5) return(NULL)
    shinyjs::show(id = paste0("fact_", nrofcov() + 2))
    nrofcov(nrofcov() + 1)
  })

  shiny::observeEvent({
    input$hide
  },{
    if(nrofcov() == 1) return(NULL)
    nrofcov(nrofcov() - 1)
    shinyjs::hide(id = paste0("fact_", nrofcov() + 2))
  })

  shiny::observe({
    shiny::req(nrofcov())
    if(nrofcov() < 5){shinyjs::show("add")} else {shinyjs::hide("add")}
    if(nrofcov() > 1){shinyjs::show("hide")} else {shinyjs::hide("hide")}
  })

  Faktorstufen <- shiny::eventReactive({
      nrofcov()
      input$fact_1
      input$fact_2
      input$fact_3
      input$fact_4
      input$fact_5
      input$fact_6
    },{
      sapply(1:(nrofcov() + 1), function(i){
        input[[paste0("fact_",i)]]
      })
  })

  dimnames <- shiny::reactive({
    if(!is.null(Faktorstufen())){
       list(paste0("X = ", 1:Faktorstufen()[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen()[-1]) * 2- 1)]))
    } else {
       list(NULL, NULL)
    }
  })

  stored <- shiny::reactiveValues(means = 50, freqs = 10)

  output$matrixmeans <- rhandsontable::renderRHandsontable({
    shiny::req(Faktorstufen(), stored$means)
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(structure(matrix(stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), 1:prod(Faktorstufen()[-1]), format = "0.000"), contextMenu = FALSE, stretchH = "all")
  })

  output$matrixfreqs <- rhandsontable::renderRHandsontable({
    shiny::req(Faktorstufen(), stored$freqs)
    #the validator makes sure that input is non-zero, positive integer < 10000000 // type and format reduce to integer and non comma number
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(structure(matrix(stored$freqs, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), 1:prod(Faktorstufen()[-1]), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,6}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
  })

  output$referenceGroup <- shiny::renderUI({
    shiny::req(showEffects())
    shiny::selectInput("referenceGroup", "Reference Group", choices = c("None" = 0, 1:Faktorstufen()[1]), selected = if(!(is.null(input$referenceGroup)) && (input$referenceGroup %in% 0:Faktorstufen()[1])){input$referenceGroup} else {NULL})
  })

  referenceGroup <- shiny::reactive({
    shiny::req(input$referenceGroup)
    if(input$referenceGroup > Faktorstufen()[1]) return(NULL)
    as.numeric(input$referenceGroup)
  })

  #### . data stuff ####
  data <- shiny::reactiveValues()
  
  shiny::observeEvent({
    input$matrixmeans
    input$matrixfreqs
  },{
    stored$means <- rhandsontable::hot_to_r(input$matrixmeans)
    stored$freqs <- rhandsontable::hot_to_r(input$matrixfreqs)
  })

  shiny::observeEvent({
    input$analyse_theoretical
  },{
    shiny::req(Faktorstufen(), stored$means, stored$freqs)

    if(actions$freshupload){ # if data is freshly uploaded, wait for Faktorstufen to update accordingly, then you can go on
      shiny::req(Faktorstufen()[1] == nrow(stored$means), prod(Faktorstufen()[-1]) == ncol(stored$means))
      actions$freshupload <- FALSE
    }

    data$work <- list(
      means = matrix(stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
      freq = matrix(stored$freqs, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
      levels = Faktorstufen(),
      dimnames = dimnames()
    )
    
    mode("theoretical")
  })

  output$downloadable_data <- shiny::downloadHandler(
    filename = paste0("unbANOVA_data_", gsub(":", "-", sub(" ", "_", Sys.time())),".Rds"),
    content = function(file) {
       saveRDS(list(
           means = data$work$means,
           freq = data$work$freq,
           k = data$work$levels[-1]
       ), file = file)
    }
  )
  
  #### . overview ####
  output$countcells <- shiny::renderText({
    shiny::req(data$work)
    prod(data$work$levels)
  })
  
  output$sumfreq <- shiny::renderText({
    shiny::req(data$work)
    sum(data$work$freq)
  })
  
  output$design <- shiny::renderText({
    shiny::req(data$work)
    paste0(paste0(data$work$levels, collapse = " x "), " - Design")
  })
    
  output$means_table <- shiny::renderTable({
    shiny::req(data$work)
    structure(data$work$means, dimnames = data$work$dimnames)
  }, rownames = TRUE, bordered = TRUE, digits = 3, sanitize.text.function = function(x) x)
  
  output$freqs_table <- shiny::renderTable({
    shiny::req(data$work)
    structure(cbind(rbind(data$work$freq, colSums(data$work$freq)), rowSums(rbind(data$work$freq, colSums(data$work$freq)))), dimnames = lapply(data$work$dimnames, function(x){rlist::list.append(x, "&#931;")}))
  }, rownames = TRUE, bordered = TRUE, digits = 0, sanitize.text.function = function(x) x)
  
  output$if_means_table <- shiny::renderTable({
    shiny::req(data$work, data$result$attr)
    structure(data$result$attr$interactionfreeMeans, dimnames = data$work$dimnames)
  }, rownames = TRUE, bordered = TRUE, digits = 3, sanitize.text.function = function(x) x)
  
  output$overviewIsBalanced <- shiny::renderUI({
    shiny::req(data$work, data$result$attr)
    shiny::HTML(if(data$result$attr$isBalanced){"Data is <font color=\"#009000\"><b>balanced</b> <i class='fas fa-balance-scale'></i></font>"}else{"Data is <font color=\"#A00000\"><b>not balanced</b> <i class='fas fa-balance-scale-left'></i></font>"})
  })
  
  output$overviewIsProportional <- shiny::renderUI({
    shiny::req(data$work, data$result$attr)
    shiny::HTML(if(data$result$attr$isProportional){"Data is <font color=\"#009000\"><b>proportional</b> <i class='fas fa-percentage'></i></font>"}else{"Data is <font color=\"#A00000\"><b>not proportional</b> <i class='fas fa-percentage'></i></font>"})
  })
  
  output$overviewIsInteractionfree <- shiny::renderUI({
    shiny::req(data$work, data$result$attr)
    shiny::HTML(if(data$result$attr$isInteractionfree){"Data has <font color=\"#009000\"><b>no x-K-interaction</b> <i class='fas fa-star-of-life'></i></font>"}else{"Data has <font color=\"#A00000\"><b>x-K-interaction</b> <i class='fas fa-star-of-life'></i></font>"})
  })
  
  #### . results ####
  includeATE <- shiny::reactive({if(is.null(input$includeATE)){FALSE}else{input$includeATE}})
  
  shiny::observeEvent({
    includeATE()
    data$work
  },{
    data$result <- unbANOVA::unbalancedANOVA(means = data$work$means, freq = data$work$freq, k.levels = data$work$levels[-1], type = c("I", "II", "III", if(includeATE()) "ATE"))

    output$Attributes <- shiny::renderUI(shiny::fluidRow(
                          shiny::column(4, shiny::HTML(if(data$result$attr$isBalanced){"Data is <font color=\"#009000\"><b>balanced</b> <i class='fas fa-balance-scale'></i></font>"}else{"Data is <font color=\"#A00000\"><b>not balanced</b> <i class='fas fa-balance-scale-left'></i></font>"})),
                          shiny::column(4, shiny::HTML(if(data$result$attr$isProportional){"Data is <font color=\"#009000\"><b>proportional</b> <i class='fas fa-percentage'></i></font>"}else{"Data is <font color=\"#A00000\"><b>not proportional</b> <i class='fas fa-percentage'></i></font>"})),
                          shiny::column(4, shiny::HTML(if(data$result$attr$isInteractionfree){"Data has <font color=\"#009000\"><b>no x-K-interaction</b> <i class='fas fa-star-of-life'></i></font>"}else{"Data has <font color=\"#A00000\"><b>x-K-interaction</b> <i class='fas fa-star-of-life'></i></font>"}))
                         ))
    output$MarginalMeans <- shiny::renderTable(summary(data$result)$`Marginal Means`, rownames = TRUE, width = "100%", bordered = FALSE, striped = TRUE, align = "r", digits = 3)
    output$Effects <- shiny::renderTable(effects(data$result, reference.group = referenceGroup()), rownames = TRUE, width = "100%", bordered = FALSE, striped = TRUE, align = "r", digits = 3)
    output$SumOfSquares <- shiny::renderTable(summary(data$result)$`Sum of Squares`, width = if(includeATE()) "80%" else "75%", bordered = FALSE, align = "r", digits = 3)
  })

  output$printresult <- shiny::renderPrint({ # used for result checking
    print(data$result)
    print(data$result$call)
    data$result$attr
  })

  #### . display results ####
  shiny::observeEvent({
    data$work
  },{
    shiny::req(data$result)
      
    showMeans <- shiny::reactive({if(is.null(input$showMeans)){TRUE}else{input$showMeans}})
    showEffects <- shiny::reactive({if(is.null(input$showEffects)){FALSE}else{input$showEffects}})
    showSS <- shiny::reactive({if(is.null(input$showSS)){TRUE}else{input$showSS}})
      
    output$marginalMeansDisplay <- shiny::renderUI({
      if(showMeans()){
        shiny::div(
          shiny::h3("Marginal Means"),
          shiny::tableOutput("MarginalMeans")
        )
      }
    })
      
    output$effectsDisplay <- shiny::renderUI({
      if(showEffects()){
        shiny::div(
          shiny::h3("Effects"),
          shiny::tableOutput("Effects")
        )
      }
    })
      
    output$SSDisplay <- shiny::renderUI({
      if(showSS()){
        shiny::div(
          shiny::h3("Sum of Squares"),
          shiny::tableOutput("SumOfSquares")
        )
      }
    })
  })

  #### . import ####
  shiny::observeEvent({
    input$exampledata
  },{
    shiny::req(input$exampledata)

    data$d <- get(input$exampledata)
  })

  output$source <- shiny::renderUI({
    shiny::req(input$exampledata)

    switch(input$exampledata,
      "MaxDelKel2017_p361" = shiny::div(id = "cite", shiny::strong("example data taken from:"), br(), "page 361 of Maxwell, S. E., Delaney, H. D., & Kelley, K. (2017).", shiny::tags$i("Designing experiments and analyzing data: A model comparison perspective"), "(3rd ed.). New York: Routledge."),
      "MaxDelKel2017_p376" = shiny::div(id = "cite", shiny::strong("example data taken from:"), br(), "page 376 of Maxwell, S. E., Delaney, H. D., & Kelley, K. (2017).", shiny::tags$i("Designing experiments and analyzing data: A model comparison perspective"), "(3rd ed.). New York: Routledge."),
      shiny::div()
    )
  })

  shiny::observeEvent({
    input$importset
  },{
    data$d <- readRDS(input$importset$datapath)
  })
  
  shiny::observeEvent({
    data$d
  },{
    shiny::req(data$d, mode() == "theoretical")

    shiny::updateNumericInput(session, inputId = "fact_1", value = nrow(data$d$means))

    lapply(1:length(data$d$k), function(i){
      shiny::updateNumericInput(session, inputId = paste0("fact_", i+1), value = data$d$k[i])
    })

    sapply((length(data$d$k) - nrofcov()):0, function(i){
      if(i == 0) return(NULL)
      if(i > 0) shinyjs::click(id = "add")
      if(i < 0) shinyjs::click(id = "hide")
    })

    actions$freshupload <- TRUE
    stored$means <- data$d$means
    stored$freqs <- data$d$freq
  })
  
  #### . empirical data UI ####
  # data$c stores the original loaded-in data.frame
  # data$b stores the subset data.frame rearranged by the emp_col inputs
  # data$a stores the package-interna-like means & freqs matrices calculated from the imported data
  
  shiny::observeEvent({
    input$importempiricaldata
  },{
    data$c <- read.csv(input$importempiricaldata$datapath)
  })
  
  nrofcov_emp <- shiny::reactiveVal(value = 1)
  
  shiny::observeEvent({
    input$add_emp
  },{
    if(nrofcov_emp() == 5) return(NULL)
    shinyjs::show(id = paste0("emp_col_", nrofcov_emp() + 2))
    nrofcov_emp(nrofcov_emp() + 1)
  })

  shiny::observeEvent({
    input$hide_emp
  },{
    if(nrofcov_emp() == 1) return(NULL)
    nrofcov_emp(nrofcov_emp() - 1)
    shinyjs::hide(id = paste0("emp_col_", nrofcov_emp() + 2))
  })

  shiny::observe({
    shiny::req(nrofcov_emp())
    if(nrofcov_emp() < 5){shinyjs::show("add_emp")} else {shinyjs::hide("add_emp")}
    if(nrofcov_emp() > 1){shinyjs::show("hide_emp")} else {shinyjs::hide("hide_emp")}
  })
  
  emp_selected_columns <- shiny::eventReactive({
    nrofcov_emp()
    input$emp_col_1
    input$emp_col_2
    input$emp_col_3
    input$emp_col_4
    input$emp_col_5
    input$emp_col_6
  },{
    sapply(1:(nrofcov_emp() + 1), function(i){
      input[[paste0("emp_col_",i)]]
    })
  })
  
  output$empiricalimportUI <- shiny::renderUI({
    shiny::req(data$c)
    
    shiny::div(
      shiny::selectInput("emp_col_y", paste0("column of Y variable"), choices = colnames(data$c)),
      shiny::selectInput("emp_col_1", paste0("column of treatment X"), choices = colnames(data$c)),
      shiny::hr(),
      shiny::selectInput("emp_col_2", shiny::HTML(paste0("column of covariate K", shiny::tags$sub(1))), choices = colnames(data$c)),
      lapply(3:6, function(i){
        shinyjs::hidden(shiny::selectInput(paste0("emp_col_",i), shiny::HTML(paste0("column of covariate K", shiny::tags$sub(i - 1))), choices = colnames(data$c)))
      }),
      shiny::div(style = "float:right;",
        tippy::tippy(shinyjs::hidden(shiny::actionLink("hide_emp", NULL, icon = shiny::icon("minus-square"))), content = "remove covariate", placement = "left", animation = "shift-away", arrow = TRUE),
        shiny::HTML("&nbsp;&nbsp;&nbsp;"),
        tippy::tippy(shiny::actionLink("add_emp", NULL, icon = shiny::icon("plus-square")), content = "add covariate", placement = "right", animation = "shift-away", arrow = TRUE)
      ),
      
      shiny::div(style = "display:inline-block;",
        tippy::tippy(shiny::actionButton("checkempiricaldata", label = "", icon = icon("question-circle")), content = "show datatable as read", placement = "right", animation = "shift-away", arrow = TRUE),
        tippy::tippy(shiny::actionButton("checkreaddata", label = "", icon = icon("question-circle")), content = "show calculated means and freqs", placement = "right", animation = "shift-away", arrow = TRUE),
        shiny::div(style = "float:right;", tippy::tippy(shiny::actionButton("analyse_empirical", label = "Analyse", icon = icon("calculator")), content = "import data", placement = "right", animation = "shift-away", arrow = TRUE))
      )
    )
  })
  
  shiny::observeEvent({
    input$checkempiricaldata
  },{
    shiny::req(input$checkempiricaldata)
    
    shiny::showModal(shiny::modalDialog(size = "l", easyClose = TRUE, 
      DT::renderDataTable(DT::datatable(read.csv(input$importempiricaldata$datapath), selection = "none", options = list(dom = 'ltpi')))
    ))
  })
  
  data$b <- shiny::eventReactive({
    input$emp_col_y
    emp_selected_columns()
  },{
    setNames(cbind(data$c[input$emp_col_y],
                   data$c[emp_selected_columns()[1]],
                   lapply(emp_selected_columns()[-1], function(x){
                     data$c[x]
                   })),
      c("Y", "X", paste0("K", (2:length(emp_selected_columns()))-1)))
  })
  
  data$a <- shiny::eventReactive({
    data$b()
  },{
    temp <- data.table(data$b())[,
                         .(freqs = .N, means = mean(Y)),
                         by = c("X", paste0("K", (2:length(emp_selected_columns()))-1))]
    
    Faktorstufen_emp <- c(length(unique(temp$X)), sapply(grep("^K[[:digit:]]$", colnames(temp), value = TRUE), function(x) length(unique(temp[[x]]))))
    
    dimnames_emp <- list(paste0("X = ", 1:Faktorstufen_emp[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen_emp[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:Faktorstufen_emp[-1][i], each = prod(Faktorstufen_emp[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen_emp[-1]) * 2- 1)]))
    
    list(means = matrix(temp$means, nrow = Faktorstufen_emp[1], byrow = TRUE, dimnames = dimnames_emp),
         freq = matrix(temp$freqs, nrow = Faktorstufen_emp[1], byrow = TRUE, dimnames = dimnames_emp),
         k = unname(Faktorstufen_emp[-1]))
  })
  
  shiny::observeEvent({
    input$checkreaddata
  },{
    shiny::req(input$checkreaddata, data$a())
    
    shiny::showModal(shiny::modalDialog(size = "l", easyClose = TRUE, 
       shiny::renderTable(data$a()$means),
       shiny::renderTable(data$a()$freq),
       shiny::renderTable(data$a()$k),
       shiny::renderPrint(data$a()),
       shiny::renderPrint(data$b()),
       shiny::renderPrint(data$c),
    ))
  })

  shiny::observeEvent({
    input$analyse_empirical
  },{
    shiny::req(input$analyse_empirical)
    
    data$d <- data$a()
    
    temp <- data$b()
    temp$X <- factor(temp$X)
    lapply(grep("^K[[:digit:]]$", colnames(temp), value = TRUE), function(x){
      temp[[x]] <<- factor(temp[[x]])
    })
    stored$empirical <- temp
    
    mode("empirical")
  })
  
  output$empiricalresults <- shiny::renderUI({
    if(is.null(stored$empirical) || mode() == "theoretical") {
      "Please use the 'empirical analysis' tab to see these results. You either have not clicked 'Analyse' on the 'empirical analysis' tab or you are looking at theoretical results, in which case this tab is meaningless to you."
    } else {
      shiny::div(
        shiny::h3("ANOVA I (stats package)"),
        shiny::renderPrint(stats::anova(lm(Y ~ X*K1, data = stored$empirical))),
        shiny::h3("ANOVA II (car package: default / type = 2)"),
        shiny::renderPrint(car::Anova(lm(Y ~ X*K1, data = stored$empirical))),
        shiny::h3("ANOVA III (car package: type = 3)"),
        shiny::renderPrint(car::Anova(lm(Y ~ X*K1, data = stored$empirical, contrasts = list(X = "contr.helmert", K1 = "contr.helmert")), type = 3))
      )
    }
  })
  
  shiny::observeEvent({
    input$analyse_empirical
  },{
    shiny::req(data$a())
    
    fs_temp <- c(length(unique(data$b()$X)), data$a()$k)
    
    data$work <- list(
      means = data$a()$means,
      freq = data$a()$freq,
      levels = fs_temp,
      dimnames = list(paste0("X = ", 1:fs_temp[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(fs_temp[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:fs_temp[-1][i], each = prod(fs_temp[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(fs_temp[-1]) * 2- 1)]))
    )
  })
  
  #### . graph stuff ####
  graphoptions1 <- shiny::callModule(widgets, "graphoptions_1")
  graphoptions2 <- shiny::callModule(widgets, "graphoptions_2")
  graphoptions3 <- shiny::callModule(widgets, "graphoptions_3")
  graphoptions4 <- shiny::callModule(widgets, "graphoptions_4")

  output$graph_anova1 <- shiny::renderPlot({
    shiny::req(data$result, !actions$freshupload, !actions$needscalculation)

    unbANOVA::plotunbANOVA(results = data$result, type = "I", showWeights = graphoptions1$showWeights_graph, showMM = graphoptions1$showMeans_graph, showEffects = graphoptions1$showEffects_graph)
  })

  output$dl_anova1_graph <- shiny::downloadHandler(
    filename = "anova1.png",
    content = function(file) {
      ggplot2::ggsave(file, unbANOVA::plotunbANOVA(results = data$result, type = "I", showWeights = graphoptions1$showWeights_graph, showMM = graphoptions1$showMeans_graph, showEffects = graphoptions1$showEffects_graph))
    }
  )

  output$graph_anova2 <- shiny::renderPlot({
    shiny::req(data$result, !actions$freshupload, !actions$needscalculation)

    unbANOVA::plotunbANOVA(results = data$result, type = "II", showWeights = graphoptions2$showWeights_graph, showMM = graphoptions2$showMeans_graph, showEffects = graphoptions2$showEffects_graph)
  })

  output$dl_anova2_graph <- shiny::downloadHandler(
    filename = "anova2.png",
    content = function(file) {
      ggplot2::ggsave(file, unbANOVA::plotunbANOVA(results = data$result, type = "II", showWeights = graphoptions2$showWeights_graph, showMM = graphoptions2$showMeans_graph, showEffects = graphoptions2$showEffects_graph))
    }
  )

  output$graph_anova3 <- shiny::renderPlot({
    shiny::req(data$result, !actions$freshupload, !actions$needscalculation)

    unbANOVA::plotunbANOVA(results = data$result, type = "III", showWeights = graphoptions3$showWeights_graph, showMM = graphoptions4$showMeans_graph, showEffects = graphoptions3$showEffects_graph)
  })

  output$dl_anova3_graph <- shiny::downloadHandler(
    filename = "anova3.png",
    content = function(file) {
      ggplot2::ggsave(file, unbANOVA::plotunbANOVA(results = data$result, type = "III", showWeights = graphoptions3$showWeights_graph, showMM = graphoptions4$showMeans_graph, showEffects = graphoptions3$showEffects_graph))
    }
  )

  output$graph_ate <- shiny::renderPlot({
    shiny::req(data$result, !actions$freshupload, !actions$needscalculation)

    unbANOVA::plotunbANOVA(results = data$result, type = "ATE", showWeights = graphoptions3$showWeights_graph, showMM = graphoptions4$showMeans_graph, showEffects = graphoptions4$showEffects_graph)
  })

  output$dl_ate_graph <- shiny::downloadHandler(
    filename = "ate.png",
    content = function(file) {
      ggplot2::ggsave(file, unbANOVA::plotunbANOVA(results = data$result, type = "ATE", showWeights = graphoptions3$showWeights_graph, showMM = graphoptions4$showMeans_graph, showEffects = graphoptions4$showEffects_graph), dpi = 600)
    }
  )
}

#shiny::shinyApp(ui = ui, server = server)
))
}
