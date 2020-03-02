#' Start the light-shiny App
#'
#' This function starts the (light) shiny gui.
#' @keywords anova
#' @export
unbanovaLightShiny <- function(){
shiny::runApp(
shiny::shinyApp(
#### ui ####
ui = shiny::fluidPage(
  shiny::tags$head( #makes dropdown menu open on hover
    shiny::tags$style(shiny::HTML("
      .dropdown:hover>.dropdown-menu {
        display: block;
      }
      
      .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary {background-color: #e95420}
      
      table {
        table-layout: fixed;
      }
      
      .th {
        width:20%;
      }
      
      #SumOfSquares>table {
        float: right;
      }
    "))
  ),
  theme = shinythemes::shinytheme("united"),
  shinyjs::useShinyjs(),
  bsplus::use_bs_tooltip(),
  #shinythemes::themeSelector(),
  shiny::titlePanel("unbANOVA"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tabsetPanel(id = "inputs",
        shiny::tabPanel("Levels",
          shiny::uiOutput("theoreticalTab")
        ),
        shiny::tabPanel("data",
            shiny::uiOutput("practicalTab")
        ),
        shiny::tabPanel("Means & Frequencies",
          shiny::br(),
          shiny::strong("means:"),
          rhandsontable::rHandsontableOutput("matrixmeans"),
          shiny::hr(),
          shiny::strong("freqs:"),
          rhandsontable::rHandsontableOutput("matrixfreqs")       
        ),
        shiny::tabPanel("", icon = shiny::icon("ellipsis-h"),
          shiny::br(),
          shiny::div(shinyWidgets::switchInput("showMeans", value = T, inline = T, size = "mini", offStatus = "dark"), "show marginal means"),
          shiny::div(shinyWidgets::switchInput("showEffects", inline = T, size = "mini", offStatus = "dark"), "show effects"),
          shiny::div(shinyWidgets::switchInput("showSS", value = T, inline = T, size = "mini"), "show sum of squares")
        )
      )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Overview",
          shiny::br(),
          shiny::textOutput("design"),
          shiny::div("number of cells: ", shiny::textOutput("countcells", inline = TRUE)),
          shiny::div("N = ", shiny::textOutput("sumfreq", inline = TRUE)), shiny::br(),
          shiny::uiOutput("overviewIsBalanced"),
          shiny::uiOutput("overviewIsProportional"),
          shiny::uiOutput("overviewIsInteractionfree"),
          shiny::uiOutput("means"),
          shiny::uiOutput("freqs"),
          shiny::uiOutput("if_freqs")
          
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
        shiny::navbarMenu("Graphs", icon = shiny::icon("chart-bar", "fa"),
          shiny::tabPanel("ANOVA I", shinycssloaders::withSpinner(shiny::plotOutput("graph_anova1"))),
          shiny::tabPanel("ANOVA II", shinycssloaders::withSpinner(shiny::plotOutput("graph_anova2"))),
          shiny::tabPanel("ANOVA III", shinycssloaders::withSpinner(shiny::plotOutput("graph_anova3"))),
          shiny::tabPanel("ATE", shinycssloaders::withSpinner(shiny::plotOutput("graph_ate")))
        )
      )
    )
)),
#### server ####
server = function(input, output, session){
  session$onSessionEnded(stopApp)
  
  actions <- reactiveValues(freshstart = TRUE,
                            freshupload = FALSE,
                            freshchangeoflevels = FALSE,
                            freshchangemeansorfreqs = FALSE)
  
  #### . input stuff ####
  mode <- shiny::reactiveVal({value = 0})
  # mode = 0 --> theoretical
  # mode = 1 --> practical
  
  shiny::observeEvent({
    input$mode
  },{
    mode((mode() + 1) %% 2)
  })
  
  output$modeUI <- shiny::renderUI({
    shiny::div(
      shiny::br(),
      strong("App is currently in a different mode. If you want to switch click below, but your current inputs will be lost."),
      shiny::br(),
      shiny::br(),
      shiny::div(align="center", shiny::actionButton("mode", "Switch mode", icon = shiny::icon("sync-alt")))
    )
  })
  
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
  
  output$theoreticalTab <- shiny::renderUI({
    if(!(mode())){
      shiny::div(
        shiny::br(),
        shiny::numericInput("fact_1", paste0("number of levels of treatment X"), 2, min = 2, max = 5, step = 1),
        shiny::hr(),
        shiny::div(
          tippy::with_tippy(shiny::actionLink(style = "float:right;", "add", NULL, icon = shiny::icon("plus-square")), tooltip = "add covariate", placement = "left", animation = "shift-away", arrow = TRUE),
          shiny::HTML("&nbsp;&nbsp;&nbsp;"),
          tippy::with_tippy(shinyjs::hidden(shiny::actionLink(style = "float:right;", "hide", NULL, icon = shiny::icon("minus-square"))), tooltip = "remove covariate", placement = "right", animation = "shift-away", arrow = TRUE)
        ),
        shiny::br(),
        shiny::numericInput("fact_2", shiny::HTML(paste0("number of levels of covariate K", shiny::tags$sub(1))), 2, min = 2, max = 5, step = 1),
        lapply(3:6, function(i){
          shinyjs::hidden(shiny::numericInput(paste0("fact_",i), shiny::HTML(paste0("number of levels of covariate K", shiny::tags$sub(i - 1))), 2, min = 2, max = 5, step = 1))
        }),
        shiny::hr(),
        shiny::tabsetPanel(
          shiny::tabPanel("example data",
            shiny::br(),
            shiny::selectInput("exampledata", label = NULL, choices = list("example data" = "",
                                                                           "2 x 2" = "two_x_two",
                                                                           "3 x 2 x 3" = "three_x_two_x_three",
                                                                           "Maxwell, Delaney & Kelley (2017; p.361)" = "MaxDelKel2017_p361",
                                                                           "Maxwell, Delaney & Kelley (2017; p.376)" = "MaxDelKel2017_p376")
            ),
            shiny::uiOutput("source")
          ),
          shiny::tabPanel("upload data",
            shiny::br(),
            shiny::fileInput("importset", NULL, width = "100%"),
          )
        )
      )
    } else {
      shiny::uiOutput("modeUI")
    }
  })
  
  output$practicalTab <- shiny::renderUI({
    if(mode()){
      shiny::div(
        "test"
      )
    } else {
      shiny::uiOutput("modeUI")
    }
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
  
  shiny::observeEvent({
    Faktorstufen()
  },{
    shiny::req(!actions$freshupload)
    actions$freshchangeoflevels <- TRUE
    actions$needscalculation <- TRUE
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
    rhandsontable::hot_table(rhandsontable::rhandsontable(structure(matrix(stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), contextMenu = FALSE, stretchH = "all")
  })
  
  output$matrixfreqs <- rhandsontable::renderRHandsontable({
    shiny::req(Faktorstufen(), stored$freqs)
    #the validator makes sure that input is non-zero, positive integer < 1000 // type and format reduce to integer and non comma number
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(structure(matrix(stored$freqs, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), 1:prod(Faktorstufen()[-1]), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,2}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
  })
  
  #### . overview ####
  output$countcells <- shiny::renderText(prod(Faktorstufen()))
  output$sumfreq <- shiny::renderText(sum(data$work$freq))
  output$design <- shiny::renderText(paste0(paste0(Faktorstufen(), collapse = " x "), " - Design"))
  
  output$means_table <- shiny::renderTable({
    shiny::req(!actions$freshupload,
               nrow(data$work$freq) == length(dimnames()[[1]]), ncol(data$work$freq) == length(dimnames()[[2]]))
    structure(data$work$means, dimnames = dimnames())
  }, rownames = TRUE, bordered = TRUE, sanitize.text.function = function(x) x)
  
  output$freqs_table <- shiny::renderTable({
    shiny::req(!actions$freshupload,
               nrow(data$work$freq) == length(dimnames()[[1]]), ncol(data$work$freq) == length(dimnames()[[2]]))
    structure(data$work$freq, dimnames = dimnames())
  }, rownames = TRUE, bordered = TRUE, digits = 0, sanitize.text.function = function(x) x)
  
  output$if_freqs_table <- shiny::renderTable({
    shiny::req(!actions$freshupload, !actions$needscalculation)
    structure(data$result$attr$interactionfreeMeans, dimnames = dimnames())
  }, rownames = TRUE, bordered = TRUE, sanitize.text.function = function(x) x)
  
  output$overviewIsBalanced <- shiny::renderUI({
                                shiny::req(data$result)
                                shiny::HTML(if(data$result$attr$isBalanced){paste0("Data is <font color=\"#009000\"><b>balanced</b></font>", fontawesome::fa("balance-scale", fill = "#009000"))}else{paste0("Data is <font color=\"#A00000\"><b>not balanced</b></font> ", fontawesome::fa("balance-scale-left", fill = "#A00000"))})
                               })
  output$overviewIsProportional <- shiny::renderUI({
                                    shiny::req(data$result)
                                    shiny::HTML(if(data$result$attr$isProportional){paste0("Data is <font color=\"#009000\"><b>proportional</b></font> ", fontawesome::fa("percentage", fill = "#009000"))}else{paste0("Data is <font color=\"#A00000\"><b>not proportional</b></font> ", fontawesome::fa("percentage", fill = "#A00000"))})
                                   })
  output$overviewIsInteractionfree <- shiny::renderUI({
                                       shiny::req(data$result)
                                       shiny::HTML(if(data$result$attr$isInteractionfree){paste0("Data has <font color=\"#009000\"><b>no x-K-interaction</b></font> ", fontawesome::fa("star-of-life", fill = "#009000"))}else{paste0("Data has <font color=\"#A00000\"><b>x-K-interaction</b></font> ", fontawesome::fa("star-of-life", fill = "#A00000"))})
                                      })
  
  output$means <- shiny::renderUI({
    shiny::req(data$work$means)
    shiny::div(
      shiny::h3("means"),
      shiny::tableOutput("means_table")
    )
  })
  
  output$freqs <- shiny::renderUI({
    shiny::req(data$work$freq)
    shiny::div(
      shiny::h3("frequencies"),
      shiny::tableOutput("freqs_table")
    )
  })
  
  output$if_freqs <- shiny::renderUI({
    shiny::req(data$result$attr$interactionfreeMeans)
    shiny::div(
      shiny::h3("x-K-interactionfree means (calculated)"),
      shiny::tableOutput("if_freqs_table")
    )
  })

  #### . data stuff ####
  data <- shiny::reactiveValues()
  
  shiny::observeEvent({
    input$matrixmeans
    input$matrixfreqs
  },{
    actions$freshchangemeansorfreqs <- TRUE
    stored$means <- rhandsontable::hot_to_r(input$matrixmeans)
    stored$freqs <- rhandsontable::hot_to_r(input$matrixfreqs)
  })
  
  shiny::observeEvent({
    actions$freshchangeoflevels
    actions$freshchangemeansorfreqs
  },{
    shiny::req(actions$freshchangeoflevels |  actions$freshchangemeansorfreqs)
    
    #clear exampledata and fileupload
    
    actions$freshchangemeansorfreqs <- FALSE
  })
  
  shiny::observeEvent({
    actions$freshchangeoflevels
  },{
    shiny::req(actions$freshchangeoflevels)
    
    stored$means <- 50
    stored$freqs <- 10
    
    actions$freshchangeoflevels <- FALSE
  })
  
  shiny::observeEvent({
    Faktorstufen()
    stored$means
    stored$freqs
  },{
    shiny::req(Faktorstufen(), stored$means, stored$freqs)
    
    if(actions$freshupload){ # if data is freshly uploaded, wait for Faktorstufen to update accordingly, then you can go on
      shiny::req(Faktorstufen()[1] == nrow(stored$means), prod(Faktorstufen()[-1]) == ncol(stored$means))
      actions$freshupload <- FALSE
    }
    
    data$work <- list(
      means = matrix(stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
      freq = matrix(stored$freqs, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames())
    )
    
    actions$needscalculation <- TRUE
  })
  
  #### . results ####
  shiny::observeEvent({
    data$work  
  },{
    data$result <- unbANOVA::unbalancedANOVA(means = data$work$means, freq = data$work$freq, k.levels = Faktorstufen()[-1])
    
    data$graph <- cbind(
                    setNames(rev(expand.grid(sapply(rev(c(Faktorstufen()[1], Faktorstufen()[-1])), seq, simplify = F))), c("x", paste0("K", 1:length(Faktorstufen()[-1])))),
                    list(freq = as.numeric(t(data$work$freq))), means = as.numeric(t(data$work$means)), ifmeans = as.numeric(t(data$result$attr$interactionfreeMeans)), anova1 = rep(data$result$anova1$marginalMeans, each = prod(Faktorstufen()[-1])),
                         anova2 = rep(data$result$anova2$marginalMeans, each = prod(Faktorstufen()[-1])), anova3 = rep(data$result$anova3$marginalMeans, each = prod(Faktorstufen()[-1])), ate = rep(data$result$ATE$marginalMeans, each = prod(Faktorstufen()[-1]))
                  )
    
    data$names <- do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K",i," = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c(", "))))[1:(length(Faktorstufen()[-1]) * 2- 1)])
    
    output$Attributes <- shiny::renderUI(shiny::fluidRow(
                          shiny::column(4, shiny::HTML(if(data$result$attr$isBalanced){paste0("Data is <font color=\"#009000\"><b>balanced</b></font> ", fontawesome::fa("balance-scale", fill = "#009000"))}else{paste0("Data is <font color=\"#A00000\"><b>not balanced</b></font> ", fontawesome::fa("balance-scale-left", fill = "#A00000"))})),
                          shiny::column(4, shiny::HTML(if(data$result$attr$isProportional){paste0("Data is <font color=\"#009000\"><b>proportional</b></font> ", fontawesome::fa("percentage", fill = "#009000"))}else{paste0("Data is <font color=\"#A00000\"><b>not proportional</b></font> ", fontawesome::fa("percentage", fill = "#A00000"))})),
                          shiny::column(4, shiny::HTML(if(data$result$attr$isInteractionfree){paste0("Data has <font color=\"#009000\"><b>no x-K-interaction</b></font> ", fontawesome::fa("star-of-life", fill = "#009000"))}else{paste0("Data has <font color=\"#A00000\"><b>x-K-interaction</b></font> ", fontawesome::fa("star-of-life", fill = "#A00000"))}))
                         ))
    output$MarginalMeans <- shiny::renderTable(summary(data$result)$`Marginal Means`, rownames = TRUE, width = "100%", bordered = TRUE)
    output$Effects <- shiny::renderTable(effects(data$result), rownames = TRUE, width = "100%", bordered = TRUE)
    output$SumOfSquares <- shiny::renderTable(summary(data$result)$`Sum of Squares`, width = "80%", bordered = TRUE)
    
    actions$freshstart <- FALSE
    actions$needscalculation <- FALSE
  })
  
  output$printresult <- shiny::renderPrint({ # used for result checking
    print(data$result)
    print(data$result$call)
    data$result$attr
  })
  
  #### . display results ####
  showMeans <- shiny::reactive({if(is.null(input$showMeans)){TRUE}else{input$showMeans}})
  showEffects <- shiny::reactive({if(is.null(input$showEffects)){FALSE}else{input$showEffects}})
  showSS <- shiny::reactive({if(is.null(input$showSS)){TRUE}else{input$showSS}})
  
  output$marginalMeansDisplay <- shiny::renderUI({
    shiny::req(data$result)
    if(showMeans()){
     shiny::div(
       shiny::h3("Marginal Means"),
       shiny::tableOutput("MarginalMeans")
     )
    }
  })
  
  output$effectsDisplay <- shiny::renderUI({
    shiny::req(data$result)
    if(showEffects()){
     shiny::div(
       shiny::h3("Effects"),
       shiny::tableOutput("Effects")
     )
    }
  })
  
  output$SSDisplay <- shiny::renderUI({
    shiny::req(data$result)
    if(showSS()){
     shiny::div(
       shiny::h3("Sum of Squares"),
       shiny::tableOutput("SumOfSquares")
     )
    }
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
    shiny::req(data$d)
    
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
  
  
  
  #### . graph stuff ####
  output$graph_anova1 <- shiny::renderPlot({
    shiny::req(data$graph, !actions$freshupload, !actions$needscalculation)
    
    ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
      ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1]))) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=anova1, color = "Anova I", shape = 15), size = 4) +
      ggplot2::scale_shape_identity() +
      ggplot2::geom_text(mapping = ggplot2::aes(y=anova1, color = "Anova I"), label = rep(round(data$result$anova1$marginalMeans, 2), each = prod(Faktorstufen()[-1])), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
      ggplot2::scale_x_discrete(name = "", limits = 1:Faktorstufen()[1], labels = paste0("x = ", 1:Faktorstufen()[1])) +
      ggplot2::labs(color = ggplot2::element_blank())
  })
  
  output$graph_anova2 <- shiny::renderPlot({
    shiny::req(data$graph, !actions$freshupload, !actions$needscalculation)
    
    ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
      ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), alpha = .5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), alpha = .5, size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=anova2, color = "Anova II", shape = 15), size = 4) +
      ggplot2::geom_line(mapping = ggplot2::aes(y=ifmeans, color = rep(data$names, Faktorstufen()[1]))) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=ifmeans, color = rep(data$names, Faktorstufen()[1])), size = 2) +
      ggplot2::scale_shape_identity() +
      ggplot2::geom_text(mapping = ggplot2::aes(y=anova2, color = "Anova II"), label = rep(round(data$result$anova2$marginalMeans, 2), each = prod(Faktorstufen()[-1])), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
      ggplot2::scale_x_discrete(name = "", limits = 1:Faktorstufen()[1], labels = paste0("x = ", 1:Faktorstufen()[1])) +
      ggplot2::labs(color = ggplot2::element_blank())
  })
  
  output$graph_anova3 <- shiny::renderPlot({
    shiny::req(data$graph, !actions$freshupload, !actions$needscalculation)
    
    ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
      ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1]))) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=anova3, color = "Anova III", shape = 15), size = 4) +
      ggplot2::scale_shape_identity() +
      ggplot2::geom_text(mapping = ggplot2::aes(y=anova3, color = "Anova III"), label = rep(round(data$result$anova3$marginalMeans, 2), each = prod(Faktorstufen()[-1])), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
      ggplot2::scale_x_discrete(name = "", limits = 1:Faktorstufen()[1], labels = paste0("x = ", 1:Faktorstufen()[1])) +
      ggplot2::labs(color = ggplot2::element_blank())
  })
  
  output$graph_ate <- shiny::renderPlot({
    shiny::req(data$graph, !actions$freshupload, !actions$needscalculation)
    
    ggplot2::ggplot(data = data$graph, mapping = ggplot2::aes(x=x)) +
      ggplot2::geom_line(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1]))) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), alpha = 0.5, size = 30*(rep(colSums(data$work$freq), times = Faktorstufen()[1])/sum(data$graph$freq)) + 5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=means, color = rep(data$names, Faktorstufen()[1])), size = 30*(data$graph$freq/sum(data$graph$freq)) + 5) +
      ggplot2::geom_point(mapping = ggplot2::aes(y=ate, color = "Anova III", shape = 15), size = 4) +
      ggplot2::scale_shape_identity() +
      ggplot2::geom_text(mapping = ggplot2::aes(y=ate, color = "Anova III"), label = rep(round(data$result$ATE$marginalMeans, 2), each = prod(Faktorstufen()[-1])), nudge_y = -(max(data$graph$means) - min(data$graph$means))/30, size = 4) +
      ggplot2::scale_x_discrete(name = "", limits = 1:Faktorstufen()[1], labels = paste0("x = ", 1:Faktorstufen()[1])) +
      ggplot2::labs(color = ggplot2::element_blank())
  })
}

#shiny::shinyApp(ui = ui, server = server)
))
}