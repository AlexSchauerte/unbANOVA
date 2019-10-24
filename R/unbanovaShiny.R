#' Start the shiny App
#'
#' This function starts the shiny gui.
#' @keywords anova
#' @export
unbanovaShiny <- function(){
  shiny::runApp(shiny::shinyApp(server = function(input, output, session){
  session$onSessionEnded(stopApp) #Beende App, wenn Fenster geschlossen
  
  actions <- shiny::reactiveValues()
  
  #### factors ####
  factors <- shiny::reactiveValues()
  
  shiny::observe({
    factors[[letters[1]]] <- if(!(is.null(input$factors))){input$factors}else{1}
    factors[[letters[2]]] <- if(!(is.null(input$fact_1))){input$fact_1}else{2}
    lapply(1:(shiny::isolate(factors$a)) + 2, function(i){
      factors[[letters[i]]] <<- if(!(is.null(input[[paste0("fact_",i - 1)]]))){input[[paste0("fact_",i - 1)]]}else{2}
    })
  })
  
  #### conditional effects ####
  #NA if condeff not selected; level if selected
  condeff <- shiny::reactive({
    shiny::req(factors$a)
    
    sapply(1:factors$a, function(i){
      if(is.null(input[[paste0("condeff_", i)]])) {NA} else {if(input[[paste0("condeff_", i)]] > 0) input[[paste0("condeff_", i)]] else NA}
    })
  })
  
  output$condui <- shiny::renderUI({
    lapply(1:factors$a, function(i) {shiny::div(style = "width:100%;", shiny::strong(paste0("K",i)), shiny::div(style = "width:75%; vertical-align:top; display:inline-block", shiny::selectInput(paste0("condeff_",i), label = NULL, choices = c('-' = 0, 1:Faktorstufen()[i + 1]), selected = condeff()[i])), shiny::div())})
  })
  
  #### gespeicherte Daten ####
  Faktorstufen <- shiny::debounce(shiny::reactive({
    shiny::req(factors$a)
    #cat("I am @ Faktorstufen.\n")
    
    sapply(2:(factors$a + 2), function(i){
      factors[[letters[i]]]
    })
  }), millis = 500)
  
  countcells <- shiny::reactive({
    prod(Faktorstufen())
  })
  
  data <- shiny::reactiveValues()
  
  dimnames <- shiny::reactive({
    if(!is.null(Faktorstufen())){
       list(paste0("x = ", 1:Faktorstufen()[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen()[-1]) * 2- 1)]))
    } else {
       list(NULL, NULL)
    }
  })
  
  shiny::observeEvent({
    Faktorstufen()
  },{
    #cat("I am @ data$should.\n")
    
    if(is.null(data$stored)){
      data$should <- list(
                       means = matrix(rep(50, countcells()), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
                       freq  = matrix(rep(10, countcells()), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames())
                     )
    } else {
      data$should <- list(
                       means = matrix(data$stored$means, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
                       freq  = matrix(data$stored$freq, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames())
                     )
      data$stored <- NULL
    }
  })
  
  output$meantable <- rhandsontable::renderRHandsontable({
    shiny::req(data$should$means)
    rhandsontable::hot_table(rhandsontable::rhandsontable(data$should$means), contextMenu = FALSE, stretchH = "all")
  })
  
  output$freqtable <- rhandsontable::renderRHandsontable({
    shiny::req(data$should$freq)
    #the validator makes sure that input is non-zero, positive integer < 1000 // type and format reduce to integer and non comma number
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(data$should$freq), 1:ncol(data$should$freq), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,2}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
  })
  
  shiny::observeEvent({
    input$meantable
    input$freqtable
    condeff()
  },{
    #cat("I am @ data$work.\n")
    shiny::req(input$meantable)
    shiny::req(input$freqtable)
    
    
    #cat(paste0("k.levels = ", paste0(Faktorstufen()[-1], collapse = ", "), "\n"))
    #cat(paste0("fixed = ", paste0(condeff(), collapse = ", "), "\n\n"))
    
    data$work <- list(
                  means = matrix(rhandsontable::hot_to_r(input$meantable), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
                  freq = matrix(rhandsontable::hot_to_r(input$freqtable), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames())
                 )
    
    data$result <- unbANOVA::unbalancedANOVA(data$work$means, data$work$freq, k.levels = Faktorstufen()[-1], fixed = condeff())
  })
  
  output$meantable_readOnly <- rhandsontable::renderRHandsontable({
    shiny::req(data$work$means)
    rhandsontable::hot_table(rhandsontable::rhandsontable(data$work$means, readOnly = TRUE), contextMenu = FALSE, stretchH = "all")
  })
  
  output$freqtable_readOnly <- rhandsontable::renderRHandsontable({
    shiny::req(data$work$freq)
    rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(data$work$freq, readOnly = TRUE), 1:ncol(data$should$freq), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,2}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
  })
  
  #### Hilfswerte / -funktionen ####
  covnames <- shiny::reactive({sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K", i)})})
  
  output$sumfreq <- shiny::renderText(paste0("N = ", sum(data$work$freq)))
  
  output$countcells <- shiny::renderText(paste0("number of cells: ", countcells()))
  
  output$design <- shiny::renderText(paste0(paste0(Faktorstufen(), collapse = " x "), " - Design"))
  
  #### Faktorstufen (Eingabe) ####
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
  
  showMeans <- shiny::reactive({if(is.null(input$showMeans)){TRUE}else{input$showMeans}})
  showEffects <- shiny::reactive({if(is.null(input$showEffects)){FALSE}else{input$showEffects}})
  showSS <- shiny::reactive({if(is.null(input$showSS)){FALSE}else{input$showSS}})
  
  #### Typ I ####
  output$anova1 <- shiny::renderUI({
    shiny::req(data$result)
    if("Anova - Type I" %in% input$showTypes){
      list(
        shiny::fluidRow(shiny::div(style = "text-align:center;", shiny::h4(style = "display:inline-block;", "Type I"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("inf_anova1", label = NULL, icon = shiny::icon("question-circle", "fa"), style="color: #333333;"), id_modal = "inf_anova1_mod"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("graph_anova1", label = NULL, icon = shiny::icon("chart-bar", "fa"), style="color: #333333;"), id_modal = "inf_anova1_graph")),
          if(showMeans()) lapply(1:Faktorstufen()[1], function(x){shinydashboard::valueBox(round(data$result$anova1$marginalMeans[x], 5), names(data$result$anova1$marginalMeans[x]), width = 3, color = "purple")}),
          if(showEffects() & !showMeans()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova1$effects[x], 5), names(data$result$anova1$effects[x]), width = 3, color = "purple")}),
          if(showSS()) shinydashboard::valueBox(round(data$result$anova1$SS, 5), paste0("sumofsquares"), width = 3, color = "purple")
        ),
        shiny::fluidRow(
          if(showMeans() & showEffects()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova1$effects[x], 5), names(data$result$anova1$effects[x]), width = 3, color = "purple")})
        )
      )
    }
  })
  
  #### Typ II ####
  output$anova2 <- shiny::renderUI({
    shiny::req(data$result)
    if("Anova - Type II" %in% input$showTypes){
      list(
        shiny::fluidRow(shiny::div(style = "text-align:center;", shiny::h4(style = "display:inline-block;", "Type II"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("inf_anova2", label = NULL, icon = shiny::icon("question-circle", "fa"), style="color: #333333;"), id_modal = "inf_anova2_mod"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("graph_anova2", label = NULL, icon = shiny::icon("chart-bar", "fa"), style="color: #333333;"), id_modal = "inf_anova2_graph")),
          if(showMeans()) lapply(1:Faktorstufen()[1], function(x){shinydashboard::valueBox(round(data$result$anova2$marginalMeans[x], 5), names(data$result$anova2$marginalMeans[x]), width = 3, color = "purple")}),
          if(showEffects() & !showMeans()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova2$effects[x], 5), names(data$result$anova2$effects[x]), width = 3, color = "purple")}),
          if(showSS()) shinydashboard::valueBox(round(data$result$anova2$SS, 5), paste0("sumofsquares"), width = 3, color = "purple")
        ),
        shiny::fluidRow(
          if(showMeans() & showEffects()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova2$effects[x], 5), names(data$result$anova2$effects[x]), width = 3, color = "purple")})
        )
      )
    }
  })
  
  #### Typ III ####
  output$anova3 <- shiny::renderUI({
    shiny::req(data$result)
    if("Anova - Type III" %in% input$showTypes){
      list(
        shiny::fluidRow(shiny::div(style = "text-align:center;", shiny::h4(style = "display:inline-block;", "Type III"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("inf_anova3", label = NULL, icon = shiny::icon("question-circle", "fa"), style="color: #333333;"), id_modal = "inf_anova3_mod"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("graph_anova3", label = NULL, icon = shiny::icon("chart-bar", "fa"), style="color: #333333;"), id_modal = "inf_anova3_graph")),
          if(showMeans()) lapply(1:Faktorstufen()[1], function(x){shinydashboard::valueBox(round(data$result$anova3$marginalMeans[x], 5), names(data$result$anova3$marginalMeans[x]), width = 3, color = "purple")}),
          if(showEffects() & !showMeans()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova3$effects[x], 5), names(data$result$anova3$effects[x]), width = 3, color = "purple")}),
          if(showSS()) shinydashboard::valueBox(round(data$result$anova3$SS, 5), paste0("sumofsquares"), width = 3, color = "purple")
        ),
        shiny::fluidRow(
          if(showMeans() & showEffects()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$anova3$effects[x], 5), names(data$result$anova3$effects[x]), width = 3, color = "purple")})
        )
      )
    }
  })
  
  #### ATE ####
  output$ATE <- shiny::renderUI({
    shiny::req(data$result)
    if("ATE" %in% input$showTypes){
      list(
        shiny::fluidRow(shiny::div(style = "text-align:center;", shiny::h4(style = "display:inline-block;", "average treatment effect"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("inf_ATE", label = NULL, icon = shiny::icon("question-circle", "fa"), style="color: #333333;"), id_modal = "inf_ate_mod"),
                                   HTML("&ensp;"), bsplus::bs_attach_modal(shiny::actionLink("graph_ATE", label = NULL, icon = shiny::icon("chart-bar", "fa"), style="color: #333333;"), id_modal = "inf_ate_graph")),
          if(showMeans()) lapply(1:Faktorstufen()[1], function(x){shinydashboard::valueBox(round(data$result$ATE$marginalMeans[x], 5), names(data$result$ATE$marginalMeans[x]), width = 3, color = "purple")}),
          if(showEffects() & !showMeans()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$ATE$effects[x], 5), names(data$result$ATE$effects[x]), width = 3, color = "purple")}),
          if(showSS()) shinydashboard::valueBox(round(data$result$ATE$SS, 5), paste0("sumofsquares"), width = 3, color = "purple")
        ),
        shiny::fluidRow(
          if(showMeans() & showEffects()) lapply(1:sum(1:Faktorstufen()[1] - 1), function(x){shinydashboard::valueBox(round(data$result$ATE$effects[x], 5), names(data$result$ATE$effects[x]), width = 3, color = "purple")})
        )
      )
    }  
  })
  
  output$interactionfreedata <- rhandsontable::renderRHandsontable({
    shiny::req(data$result$attr$interactionfreeMeans)
    rhandsontable::hot_table(rhandsontable::rhandsontable(matrix(data$result$attr$interactionfreeMeans, nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = list(paste0("x = ", 1:Faktorstufen()[1]), do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K<sub>",i,"</sub> = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c("<br>"))))[1:(length(Faktorstufen()[-1]) * 2- 1)]))), readOnly = TRUE), contextMenu = FALSE, stretchH = "all")
  })
  
 #### Anzeigen (balanciert, Proportional, ...) ####
  output$headerbalance <- shiny::renderUI({
    if(is.null(data$result$attr$isBalanced)) return(NULL)
    
    if(data$result$attr$isBalanced){ 
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("balance-scale", fill = "#009000"), style = "display:inline-block;"), title = "data is balanced", placement = "bottom")
    } else {
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("balance-scale-left", fill = "#A00000"), style = "display:inline-block;"), title = "data is not balanced", placement = "bottom")
    }
  })
  
  output$headerproportional <- shiny::renderUI({
    if(is.null(data$result$attr$isProportional)) return(NULL)
    
    if(is.na(data$result$attr$isProportional)){ 
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("percentage", fill = "#808080"), style = "display:inline-block;"), title = "not applicable because of conditional parameters", placement = "bottom")
    } else if(data$result$attr$isProportional){ 
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("percentage", fill = "#009000"), style = "display:inline-block;"), title = "data is proportional", placement = "bottom")
    } else {
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("percentage", fill = "#A00000"), style = "display:inline-block;"), title = "data is not proportional", placement = "bottom")
    }
  })
  
  output$headerinteractionfree <- shiny::renderUI({
    if(is.null(data$result$attr$isInteractionfree)) return(NULL)
    
    if(data$result$attr$isInteractionfree){ 
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("star-of-life", fill = "#009000"), style = "display:inline-block;"), title = "data is free of x-K-interaction", placement = "bottom")
    } else {
      bsplus::bs_embed_tooltip(shiny::div(fontawesome::fa("star-of-life", fill = "#A00000"), style = "display:inline-block;"), title = "data has x-K-interaction", placement = "bottom")
    }
  })
  
 #### Graphiken ####
  shiny::observeEvent({
    data$result
  },{
    #data$graph <- NULL
    data$graph <- cbind(
                    setNames(rev(expand.grid(sapply(rev(c(Faktorstufen()[1], Faktorstufen()[-1])), seq, simplify = F))), c("x", paste0("K", 1:length(Faktorstufen()[-1])))),
                    list(freq = as.numeric(t(data$work$freq))), means = as.numeric(t(data$work$means)), ifmeans = as.numeric(t(data$result$attr$interactionfreeMeans)), anova1 = rep(data$result$anova1$marginalMeans, each = prod(Faktorstufen()[-1])),
                         anova2 = rep(data$result$anova2$marginalMeans, each = prod(Faktorstufen()[-1])), anova3 = rep(data$result$anova3$marginalMeans, each = prod(Faktorstufen()[-1])), ate = rep(data$result$ATE$marginalMeans, each = prod(Faktorstufen()[-1]))
                  )
    
    data$names <- do.call(paste0, rlist::list.flatten(rlist::list.expand(sapply(1:length(Faktorstufen()[-1]), function(i){paste0("K",i," = ", rep(1:Faktorstufen()[-1][i], each = prod(Faktorstufen()[-1][-c(1:i)])))}, simplify = FALSE), list(c(", "))))[1:(length(Faktorstufen()[-1]) * 2- 1)])
  })
  
  output$graph_anova1 <- shiny::renderPlot({
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
  
 #### Daten importieren ####
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
    output$importmeans <- rhandsontable::renderRHandsontable({
      rhandsontable::hot_table(rhandsontable::rhandsontable(data$d$means), contextMenu = FALSE, stretchH = "all")
    })
    
    output$importfreq <- rhandsontable::renderRHandsontable({
      rhandsontable::hot_table(rhandsontable::hot_col(rhandsontable::rhandsontable(data$d$freq), 1:ncol(data$d$freq), validator = "function (value, callback) {if (value === null || value === void 0) {value = '';} if (this.allowEmpty && value === '') {return callback(true);} else if (value === '') {return callback(false);} let isNumber = /^[1-9]\\d{0,2}$/.test(value); if (!isNumber) {return callback(false);} if (isNaN(parseFloat(value))) {return callback(false);} return callback(true);}", allowInvalid = FALSE, type = "numeric", format = "0,0"), contextMenu = FALSE, stretchH = "all")
    })
  })
  
  shiny::observeEvent({
    data$d
  },{
    output$importedtables <- shiny::renderUI({
      shiny::fluidRow(
          shiny::column(6,shiny::strong("imported frequency table"), shiny::br(), rhandsontable::rHandsontableOutput("importfreq")),
          shiny::column(6,shiny::strong("imported means table"), shiny::br(), rhandsontable::rHandsontableOutput("importmeans"))
      )
    })
  })
  
  output$importfooter <- shiny::renderUI({
    if(is.null(data$d)){
      shiny::div(style="display:inline-block; float:right", bsplus::bs_modal_closebutton(label = shiny::div(fontawesome::fa("times", fill = "#FFFFFF"), "Dismiss")))
    } else {
      shiny::div(shiny::actionLink("save", style = "width:100%", bsplus::bs_modal_closebutton(label = shiny::div(style = "color:#FFFFFF;", fontawesome::fa("save", fill = "#FFFFFF"), "Import"))),
          shiny::div(style = "display:inline-block; width:10px"), shiny::div(style="display:inline-block; float:right", bsplus::bs_modal_closebutton(label = shiny::div(fontawesome::fa("times", fill = "#FFFFFF"), "Dismiss"))))
    }
  })
  
  shiny::observeEvent({
    input$save
  },{
    factors[[letters[1]]] <- length(data$d$k) - 1 #necessary since it forces "factors$a" to update even if length(data$d$k) [and nrow(data$d$means)] equal current state of App
    factors[[letters[1]]] <- length(data$d$k) 
    factors[[letters[2]]] <- nrow(data$d$means)
    lapply(1:factors$a, function(i){
      factors[[letters[i + 2]]] <<- data$d$k[i]
    })
    
    data$stored$means <- rhandsontable::hot_to_r(input$importmeans)
    data$stored$freq <- rhandsontable::hot_to_r(input$importfreq)
  })
  
  #### Downloads ####
  output$downloadable_data <- shiny::downloadHandler(filename = paste0("unbANOVA_data_", gsub(":", "-", sub(" ", "_", Sys.time())),".Rds"),
    content = function(file) {
      saveRDS(list(
        means = matrix(rhandsontable::hot_to_r(input$meantable_readOnly), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
        freq = matrix(rhandsontable::hot_to_r(input$freqtable_readOnly), nrow = Faktorstufen()[1], ncol = prod(Faktorstufen()[-1]), dimnames = dimnames()),
        k = Faktorstufen()[-1]
      ), file = file)
    }
  )
  
  output$more_general <- shiny::downloadHandler(
      filename = "Introduction.pdf",
      content = function(file){
        tempReport <- file.path(tempdir(), "Introduction.pdf")
        file.copy("reports/Introduction.pdf", tempReport, overwrite = TRUE)
        file.rename(tempReport, file)
      },
      contentType = 'application/pdf'
  )
  
},
#### ui ####
  ui = shinydashboard::dashboardPage(skin = "purple", title = "unbalanced ANOVA",
    #### Header ####
    shinydashboard::dashboardHeader(
      title = "unbalancedANOVA",
      titleWidth = "20%"
    ),
    #### Sidebar ####
    shinydashboard::dashboardSidebar(
      width = "20%",
      shinydashboard::sidebarMenu(id = "sidebarmenu",
        shinydashboard::menuItem("treatment & covariates", tabName = "a"),
        shiny::conditionalPanel("input.sidebarmenu === 'a'",
          shiny::div(style = "width:100%; text-align:center; display:inline-block", shiny::uiOutput("levelstreat", inline = T), shiny::uiOutput("nrofcov", inline = T)),
          shiny::uiOutput("factorinputs")
        ),
        shinydashboard::menuItem("conditional", tabName = "b"),
        shiny::conditionalPanel("input.sidebarmenu === 'b'",
          shiny::uiOutput("condui")
        ),
        shinydashboard::menuItem("options", tabName = "c"),
        shiny::conditionalPanel("input.sidebarmenu === 'c'",
          shinyWidgets::checkboxGroupButtons(
            inputId = "showTypes", label = "Show results of following types:", size = "sm", direction = "vertical",
            choices = c("Anova - Type I", "Anova - Type II", "Anova - Type III", "ATE"), selected = c("Anova - Type I", "Anova - Type II", "Anova - Type III", "ATE"),
            justified = TRUE, checkIcon = list(yes = shiny::icon("ok", lib = "glyphicon"), no = shiny::icon("remove", lib = "glyphicon"))
          ),
          shiny::div(shinyWidgets::switchInput("showMeans", value = T, inline = T, size = "mini", offStatus = "dark"), "show marginal means"),
          shiny::div(shinyWidgets::switchInput("showEffects", inline = T, size = "mini", offStatus = "dark"), "show effects"),
          shiny::div(shinyWidgets::switchInput("showSS", inline = T, size = "mini", offStatus = "dark"), "show sum of squares")
        ),
        shiny::hr(),  
        shiny::div(style = "width:100%;",
            shiny::div(style = "width:33%; text-align:center; display:inline-block", bsplus::bs_attach_modal(shiny::actionLink("import", label = NULL, icon = shiny::icon("upload", "fa-lg")), id_modal = "importModal")),
            shiny::div(style = "width:33%; text-align:center; display:inline-block", shiny::downloadLink("more_general", label = shiny::icon("question-circle", "fa-lg"))),
            shiny::div(style = "width:33%; text-align:center; display:inline-block", bsplus::bs_attach_modal(shiny::actionLink("download", label = NULL, icon = shiny::icon("download", "fa-lg")), id_modal = "downModal"))
        ),
        shiny::div(style = "text-align:center", shiny::textOutput("sumfreq")),
        shiny::div(style = "text-align:center", shiny::textOutput("countcells")),
        shiny::div(style = "text-align:center", shiny::textOutput("design"))
      )
    ),
    #### Body ####
    shinydashboard::dashboardBody(
      ## change color of checkboxGroupButtons and switchInput (shinyWidget) to purple variants
      shiny::tags$style(shiny::HTML("
        .bootstrap-switch .bootstrap-switch-handle-on.bootstrap-switch-primary {background-color: #605ca8}
        .btn-default {color: #fff; background-color: #222d32; border-color: #fff;}             
        .btn-default:focus {color: #fff; background-color: #151b1e; border-color: #fff;}
        .btn-default:hover {color: #fff; background-color: #151b1e; border-color: #fff;}
        .btn-default.active {color: #fff; background-color: #605ca8; border-color: #fff;}
        .btn-default.active:focus {color: #fff; background-color: #413e74; border-color: #fff;}
        .btn-default.active:hover {color: #fff; background-color: #413e74; border-color: #fff;}
        .box-title {width:100%;}
        .small-box {margin-bottom: 0px; line-height: 0.9}
      ")),
      ##
      shiny::uiOutput("modals"),
      bsplus::bs_modal("importModal",
          title = "import data",
          size = "large",
          footer = shiny::uiOutput("importfooter"),
          body = shiny::div(
            shiny::fluidRow(
              shiny::column(4, shiny::fileInput("importset", NULL, width = "100%")),
              shiny::column(4),
              shiny::column(4, shiny::selectInput("exampledata", label = NULL, choices = list("example data" = "",
                                                                                              "2 x 2" = "two_x_two",
                                                                                              "3 x 2 x 3" = "three_x_two_x_three",
                                                                                              "Maxwell, Delaney & Kelley (2017; p.361)" = "MaxDelKel2017_p361",
                                                                                              "Maxwell, Delaney & Kelley (2017; p.376)" = "MaxDelKel2017_p376")))
            ),
            shiny::br(),
            shiny::uiOutput("importedtables"),
            shiny::br(),
            shiny::uiOutput("source")
          )
      ),
      bsplus::bs_modal("downModal",
          title = "download data",
          size = "large",
          footer = shiny::div(shiny::downloadButton("downloadable_data"),
            shiny::div(style = "display:inline-block; width:10px"), shiny::div(style="display:inline-block; float:right", bsplus::bs_modal_closebutton(label = shiny::div(fontawesome::fa("times", fill = "#FFFFFF"), "Dismiss")))),
          body = shiny::div(
            shiny::fluidRow(
              shiny::column(6,shiny::strong("frequency table"), rhandsontable::rHandsontableOutput("freqtable_readOnly")),
              shiny::column(6,shiny::strong("mean table"), rhandsontable::rHandsontableOutput("meantable_readOnly"))
            ),
            shiny::br()#,
            #shiny::textOutput("design")
          )
      ),
      bsplus::bs_modal("inf_anova1_mod",
          title = "ANOVA - Type I",
          size = "large",
          footer = NULL,
          body = shiny::div(
            shiny::withMathJax(shiny::HTML("\\[\\widehat{E\\left(Y|X=i\\right)} = \\sum\\limits^{k}_{j = 1}\\dfrac{n_{ij}}{n_{i\\bullet}}\\ \\hat{\\mu}_{ij}\\]")),
            "Type I marginal means are weighted marginal means. The means of the groups (divided by the categorial covariates) in each level of X are weighted by their corresponding cell frequency in relation to the marginal frequency."
          )
      ),
      bsplus::bs_modal("inf_anova2_mod",
          title = "ANOVA - Type II",
          size = "large",
          footer = NULL,
          body = shiny::div(
            shiny::withMathJax(shiny::HTML("\\[\\widehat{\\overline{E\\left(Y^{\\text{if}}|X = i, K\\right)}} = \\sum\\limits^{k}_{j = 1}\\dfrac{1}{k}\\ \\hat{\\mu}^{\\text{if}}_{ij}\\]")),
            "Type II marginal means are unweighted marginal means. Based on a dataset, which is free from interaction ('if' stands for interaction-free), the means of the groups (divided by the categorial covariates) in each level of X are each weighted by one over the number of groups.",
            shiny::br(),
            shiny::br(),
           shiny::strong("interaction free - means"),
            rhandsontable::rHandsontableOutput("interactionfreedata")
          )
      ),
      bsplus::bs_modal("inf_anova3_mod",
          title = "ANOVA - Type III",
          size = "large",
          footer = NULL,
          body = shiny::div(
            shiny::withMathJax(shiny::HTML("\\[\\widehat{\\overline{E\\left(Y|X = i,K\\right)}} = \\sum\\limits^{k}_{j = 1}\\dfrac{1}{k}\\ \\hat{\\mu}_{ij}\\]")),
            "Type III marginal means are unweighted marginal means. The means of the groups (divided by the categorial covariates) in each level of X are each weighted by one over the number of groups."
          )
      ),
      bsplus::bs_modal("inf_ate_mod",
          title = "Average Treatment Effect",
          size = "large",
          footer = NULL,
          body = shiny::div(
            shiny::withMathJax(shiny::HTML("\\[\\widehat{ATE\\left(Y|X = i\\right)} = \\widehat{E\\left[E\\left(Y|X = i, K\\right)\\right]} = \\sum\\limits^{k}_{j = 1}\\dfrac{n_{\\bullet j}}{n_{\\bullet\\bullet}}\\ \\mu_{ij}\\]")),
            "ATEs are weighted marginal means and are specifically causally unbiased estimates. The means of the groups (divided by the categorial covariates) in each level of X are weighted by the marginal frequency of that particular covariate group with respect to the total frequency of all groups."
          )
      ),
      bsplus::bs_modal("inf_anova1_graph",
          title = "ANOVA - Type I",
          size = "large",
          footer = NULL,
          body = shiny::plotOutput("graph_anova1")
      ),
      bsplus::bs_modal("inf_anova2_graph",
          title = "ANOVA - Type II",
          size = "large",
          footer = NULL,
          body = shiny::plotOutput("graph_anova2")
      ),
      bsplus::bs_modal("inf_anova3_graph",
          title = "ANOVA - Type III",
          size = "large",
          footer = NULL,
          body = shiny::plotOutput("graph_anova3")
      ),
      bsplus::bs_modal("inf_ate_graph",
          title = "Average Treatment Effect",
          size = "large",
          footer = NULL,
          body = shiny::plotOutput("graph_ate")
      ),
      shiny::fluidRow(
        shinydashboard::box(title = shiny::div("frequencies", shiny::div(style = "float:right;", shiny::uiOutput("headerproportional", inline = TRUE), shiny::uiOutput("headerbalance", inline = TRUE))), 
          rhandsontable::rHandsontableOutput("freqtable")
        ),
        shinydashboard::box(title = shiny::div("means", shiny::div(style = "float:right;", shiny::uiOutput("headerinteractionfree", inline = TRUE))),
          rhandsontable::rHandsontableOutput("meantable")
        )
      ),
      shiny::fluidRow(
        shiny::uiOutput("warnings")
      ),
      shiny::uiOutput("anova1"),
      shiny::uiOutput("anova2"),
      shiny::uiOutput("anova3"),
      shiny::uiOutput("ATE")
    )
  )))
}
