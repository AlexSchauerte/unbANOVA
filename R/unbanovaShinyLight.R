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
  tags$head( #makes dropdown menu open on hover
    tags$style(HTML("
      .dropdown:hover>.dropdown-menu {
        display: block;
      }
    "))
  ),
  theme = shinythemes::shinytheme("united"),
  #shinythemes::themeSelector(),
  shiny::titlePanel("unbANOVA"),
  shiny::sidebarLayout(
    shiny::sidebarPanel(
      shiny::tabsetPanel(id = "inputs",
        shiny::tabPanel("Levels",
          shiny::div(style = "width:100%; text-align:center; display:inline-block", shiny::uiOutput("levelstreat", inline = T), shiny::uiOutput("nrofcov", inline = T)),
          shiny::uiOutput("factorinputs"),
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
        ),
        shiny::tabPanel("Means & Frequencies",
          shiny::strong("means:"),
          rhandsontable::rHandsontableOutput("matrixmeans"),
          shiny::hr(),
          shiny::strong("freqs:"),
          rhandsontable::rHandsontableOutput("matrixfreqs")       
        )
      )
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel("Results",
          shiny::h3("Marginal Means"),
          shiny::tableOutput("MarginalMeans"),
          shiny::hr(),
          shiny::h3("Effects"),
          shiny::tableOutput("Effects"),
          shiny::hr(),
          shiny::h3("Sum of Squares"),
          shiny::tableOutput("SumOfSquares"),
          shiny::hr(),
          shiny::uiOutput("Attributes")
        ),
        shiny::navbarMenu("Graphs", icon = shiny::icon("chart-bar", "fa"),
          shiny::tabPanel("ANOVA I", shiny::plotOutput("graph_anova1")),
          shiny::tabPanel("ANOVA II", shiny::plotOutput("graph_anova2")),
          shiny::tabPanel("ANOVA III", shiny::plotOutput("graph_anova3")),
          shiny::tabPanel("ATE", shiny::plotOutput("graph_ate"))
        )
      )
    )
)),
#### server ####
server = function(input, output, session){
  session$onSessionEnded(stopApp)
  
  #### . input stuff ####
  factors <- shiny::reactiveValues()
  
  shiny::observe({
    factors[[letters[1]]] <- if(!(is.null(input$factors))){input$factors}else{1}
    factors[[letters[2]]] <- if(!(is.null(input$fact_1))){input$fact_1}else{2}
    lapply(1:(shiny::isolate(factors$a)) + 2, function(i){
      factors[[letters[i]]] <<- if(!(is.null(input[[paste0("fact_",i - 1)]]))){input[[paste0("fact_",i - 1)]]}else{2}
    })
  })
  
  output$nrofcov <- shiny::renderUI({
    shiny::div(style = "width:49%; display:inline-block", shiny::numericInput("factors", "number of categorial covariates", if(!(is.null(factors$a))){factors$a} else {1}, min = 1, max = 5, step = 1))
  })
  
  output$factorinputs <- shiny::renderUI({
    if(factors$a + 1 > 1){
      lapply(2:(factors$a + 1), function(i){
        shiny::numericInput(paste0("fact_",i), shiny::HTML(paste0("number of levels of covariate K", shiny::tags$sub(i - 1))), if(!(is.null(factors[[letters[i + 1]]]))){factors[[letters[i + 1]]]} else {2}, min = 2, max = 5, step = 1)
      })
    }
  })
  
  output$levelstreat <- shiny::renderUI({
    shiny::div(style = "width:49%; display:inline-block", shiny::numericInput("fact_1", paste0("number of treatment levels"), if(!(is.null(factors$b))){factors$b} else {2}, min = 2, max = 5, step = 1))
  })
  
  Faktorstufen <- shiny::reactive({
    shiny::req(factors$a)
    
    sapply(2:(factors$a + 2), function(i){
      factors[[letters[i]]]
    })
  })
  
  dimnames <- shiny::reactive({
    if(!is.null(Faktorstufen())){
       list(paste0("x = ", 1:Faktorstufen()[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen()[-1]) * 2- 1)]))
    } else {
       list(NULL, NULL)
    }
  })
  
  stored <- reactiveValues(means = 50, freqs = 10)
  
  output$matrixmeans <- rhandsontable::renderRHandsontable({
    shiny::req(Faktorstufen(), stored)
    rhandsontable::hot_table(rhandsontable::rhandsontable(structure(matrix(stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), contextMenu = FALSE, stretchH = "all")
  })
  
  output$matrixfreqs <- rhandsontable::renderRHandsontable({
    shiny::req(Faktorstufen(), stored)
    #the validator makes sure that input is non-zero, positive integer < 1000 // type and format reduce to integer and non comma number
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(structure(matrix(stored$freqs, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames())), 1:prod(Faktorstufen()[-1]), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,2}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
  })
  
  #### . data stuff ####
  data <- shiny::reactiveValues()
  
  observeEvent({
    input$matrixmeans
    input$matrixfreqs
  },{
    shiny::req(Faktorstufen(), input$matrixmeans, input$matrixfreqs)
    
    data$work <- list(
      means = matrix(rhandsontable::hot_to_r(input$matrixmeans), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
      freq = matrix(rhandsontable::hot_to_r(input$matrixfreqs), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames())
    )
  })
  
  #### . results ####
  observeEvent({
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
    
    output$SumOfSquares <- shiny::renderTable(summary(data$result)$`Sum of Squares`, width = "100%", bordered = TRUE)
  })
  
  #### . import ####
  shiny::observeEvent({
    input$exampledata
  },{
    if(input$exampledata != ""){
      data$d <- get(input$exampledata)
      
      output$source <- shiny::renderUI({switch(input$exampledata,
      "MaxDelKel2017_p361" = shiny::div(id = "cite", shiny::strong("example data taken from:"), br(), "page 361 of Maxwell, S. E., Delaney, H. D., & Kelley, K. (2017).", shiny::tags$i("Designing experiments and analyzing data: A model comparison perspective"), "(3rd ed.). New York: Routledge."), 
      "MaxDelKel2017_p376" = shiny::div(id = "cite", shiny::strong("example data taken from:"), br(), "page 376 of Maxwell, S. E., Delaney, H. D., & Kelley, K. (2017).", shiny::tags$i("Designing experiments and analyzing data: A model comparison perspective"), "(3rd ed.). New York: Routledge."), 
      shiny::div()
      )})
    }
  })
  
  shiny::observeEvent({
    input$importset
  },{
    data$d <- readRDS(input$importset$datapath)
  })
  
  shiny::observeEvent({
    data$d
  },{
    req(data$d)
    
    shiny::updateNumericInput(session, inputId = "factors", value = length(data$d$k))
    shiny::updateNumericInput(session, inputId = "fact_1", value = nrow(data$d$means))
    
    lapply(1:length(data$d$k), function(i){
      shiny::updateNumericInput(session, inputId = paste0("fact_", i+1), value = data$d$k[i])
    })
    
    stored$means <- data$d$means
    stored$freqs <- data$d$freq
  })
  
  
  
  #### . graph stuff ####
  output$graph_anova1 <- shiny::renderPlot({
    req(data$graph)
    
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
    req(data$graph)
    
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
    req(data$graph)
    
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
    req(data$graph)
    
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