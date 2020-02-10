#' Start the light-shiny App
#'
#' This function starts the (light) shiny gui.
#' @keywords anova
#' @export
unbanovaLightShiny <- function(){
shiny::runApp(shiny::shinyApp(
#### ui ####
ui = shiny::fluidPage(shiny::titlePanel("unbANOVA"), shiny::sidebarLayout(
  shiny::sidebarPanel(
    shiny::tabsetPanel(
      shiny::tabPanel("Levels",
        shiny::div(style = "width:100%; text-align:center; display:inline-block", shiny::uiOutput("levelstreat", inline = T), shiny::uiOutput("nrofcov", inline = T)),
        shiny::uiOutput("factorinputs")
      ),
      shiny::tabPanel("Means & Frequencies",
        shiny::strong("means:"),
        shiny::uiOutput("matrixmeans"),
        shiny::strong("freqs:"),
        shiny::uiOutput("matrixfreqs")       
      )
    )
  ),
  shiny::mainPanel(
    shiny::tabsetPanel(
      shiny::tabPanel("Main Results", shiny::verbatimTextOutput("results")),
      shiny::tabPanel("Effects", shiny::verbatimTextOutput("effects")),
      shiny::tabPanel("Graph - ANOVA I", shiny::plotOutput("graph_anova1")),
      shiny::tabPanel("Graph - ANOVA II", shiny::plotOutput("graph_anova2")),
      shiny::tabPanel("Graph - ANOVA III", shiny::plotOutput("graph_anova3")),
      shiny::tabPanel("Graph - ATE", shiny::plotOutput("graph_ate"))
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
       list(paste0("x = ", 1:Faktorstufen()[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K",i," = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen()[-1]) * 2- 1)]))
    } else {
       list(NULL, NULL)
    }
  })
  
  output$matrixmeans <- shiny::renderUI({
    shinyMatrix::matrixInput("means", value = structure(matrix(50, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames()),
                inputClass = "", rows = list(names = TRUE), cols = list(names = TRUE), class = "numeric")
  })
  
  output$matrixfreqs <- shiny::renderUI({
    shinyMatrix::matrixInput("freqs", value = structure(matrix(10, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1])), dimnames = dimnames()),
                inputClass = "", rows = list(names = TRUE), cols = list(names = TRUE), class = "numeric")
  })
  
  #### . data stuff ####
  data <- shiny::reactiveValues()
  observe({
    shiny::req(Faktorstufen(), input$means, input$freqs, nrow(input$means) == Faktorstufen()[1], ncol(input$means) == prod(Faktorstufen()[-1]))
    
    data$work <- list(means = input$means, freq = input$freqs)
    
    data$result <- unbANOVA::unbalancedANOVA(means = data$work$means, freq = data$work$freq, k.levels = Faktorstufen()[-1])
    
    data$graph <- cbind(
                    setNames(rev(expand.grid(sapply(rev(c(Faktorstufen()[1], Faktorstufen()[-1])), seq, simplify = F))), c("x", paste0("K", 1:length(Faktorstufen()[-1])))),
                    list(freq = as.numeric(t(data$work$freq))), means = as.numeric(t(data$work$means)), ifmeans = as.numeric(t(data$result$attr$interactionfreeMeans)), anova1 = rep(data$result$anova1$marginalMeans, each = prod(Faktorstufen()[-1])),
                         anova2 = rep(data$result$anova2$marginalMeans, each = prod(Faktorstufen()[-1])), anova3 = rep(data$result$anova3$marginalMeans, each = prod(Faktorstufen()[-1])), ate = rep(data$result$ATE$marginalMeans, each = prod(Faktorstufen()[-1]))
                  )
    
    data$names <- do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K",i," = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c(", "))))[1:(length(Faktorstufen()[-1]) * 2- 1)])
  })
  
  output$results <- shiny::renderPrint(summary(data$result))
  output$effects <- shiny::renderPrint(effects(data$result))
  
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
}))}