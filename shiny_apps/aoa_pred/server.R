library(dplyr)
library(ggplot2)
library(shiny)
# library(MASS)
library(magrittr)
library(tidyr)
library(directlabels)
library(RMySQL)
library(readr)
library(boot) # for inv.logit
library(stringr)
source("../data_loading.R")
source("../palette.R")
Sys.setlocale(locale="en_US.UTF-8")
options(error = NULL)

## DEBUGGING
input <- list(language = "English", form = "WG WS", measure = "produces",
              pred_vars = c("input.freq"), 
             cats = "All", 
             robust = FALSE, wordform = NULL, complexity = NULL)

list.items.by.definition <- function(item.data) {
  items <- item.data$item.id
  names(items) <- item.data$definition
  return(items)
}

list.items.by.id <- function(item.data) {
  items <- item.data$definition
  names(items) <- item.data$item.id
  return(items)
}

data.fun <- function(admins, fun.instrument, fun.measure) {

  instrument.word.data <- function(inst_id) {
    inst <- filter(fun.instrument, instrument_id == inst_id)
    word_ids <- inst$words.by.definition[[1]]
    get.instrument.data(inst$table[[1]], word_ids[!is.na(word_ids)]) %>%
      mutate(instrument_id = inst$instrument_id)
  }

  word.data <- bind_rows(sapply(fun.instrument$instrument_id, instrument.word.data, simplify = FALSE)) %>% 
      mutate(produces = value == 'produces',
             understands = value == 'understands' | value == 'produces') %>%
      select(-value) %>%
      gather(measure, value, produces, understands) %>%
      filter(measure == fun.measure) %>%
      left_join(admins) %>%
      filter(!is.na(age)) %>%
      group_by(instrument_id, item.id, age) %>%
      summarise(mean = mean(value, na.rm = TRUE)) %>%
      group_by(instrument_id) %>%
      mutate(item.id = paste("item_", item.id, sep = "")) %>%
      rowwise() %>%
      mutate(item = filter(fun.instrument,
                           fun.instrument$instrument_id == instrument_id)$words.by.id[[1]][item.id],
             type = "word") %>%
      left_join(select(fun.instrument, instrument_id, form)) %>%
      select(-instrument_id, -item.id)
  
  word.data
}


##### SERVER STARTS HERE
shinyServer(function(input, output, session) {
  
  output$loaded <- reactive({0})
  outputOptions(output, 'loaded', suspendWhenHidden=FALSE)
  
  wordbank <- connect.to.wordbank("dev")
  
  common.tables <- get.common.tables(wordbank)
  
  admins <- get.administration.data(common.tables)
  
  items <- get.item.data(common.tables) %>%
    mutate(definition = iconv(definition, from = "utf8", to = "utf8"))
  
  start.instrument.tables <- init.instrument.tables(wordbank, common.tables)
  
  instrument.tables <- start.instrument.tables %>%
    group_by(instrument_id) %>%
    do(words.by.definition = list.items.by.definition(filter(items,
                                                             instrument_id==.$instrument_id,
                                                             type=='word')),
       words.by.id = list.items.by.id(filter(items,
                                             instrument_id==.$instrument_id,
                                             type=='word')),
       wordform.by.definition = list.items.by.definition(filter(items,
                                                                instrument_id==.$instrument_id,
                                                                type=='word_form')),
       wordform.by.id = list.items.by.id(filter(items,
                                                instrument_id==.$instrument_id,
                                                type=='word_form')),
       complexity.by.definition = list.items.by.definition(filter(items,
                                                                  instrument_id==.$instrument_id,
                                                                  type=='complexity')),
       complexity.by.id = list.items.by.id(filter(items,
                                                  instrument_id==.$instrument_id,
                                                  type=='complexity'))) %>%
    left_join(start.instrument.tables)
  
  languages <- sort(unique(instrument.tables$language))
  
  start.language <- function() {"English"}
  start.form <- function() {"WS"}
  start.measure <- function() {"produces"}
  start.words <- function(words) {words[[1]]}
  start.wordform <- function(wordform) {}
  start.complexity <- function(complexity) {}
  
  input.language <- reactive({
    ifelse(is.null(input$language), start.language(), input$language)
  })
  
  input.form <- reactive({
    ifelse(is.null(input$form), start.form(), input$form)  
  })

  input.forms <- reactive({
    strsplit(input.form(), ' ')[[1]]
  })
  
  input.measure <- reactive({
    ifelse(is.null(input$measure), start.measure(), input$measure)
  })
  
  instrument <- reactive({
    inst <- filter(instrument.tables, language == input.language(), form %in% input.forms())$instrument_id
    for (inst_id in inst) {
      if (length(instrument.tables[instrument.tables$instrument_id == inst_id, ]$table[[1]]) == 0) {
        instrument.tables <<- add.instrument.table(wordbank, instrument.tables, inst_id)
      }
    }
    filter(instrument.tables, language == input.language(), form %in% input.forms())
  })
  
  ylabel <- reactive({
    if (input.measure() == "understands") {"Proportion of Children Understanding"}
    else if (input.measure() == "produces") {"Proportion of Children Producing"}
  })
  
  age.min <- reactive({min(instrument()$age_min)})
  age.max <- reactive({max(instrument()$age_max)})
  
  
  ############## PREPROCESS DATA #############
  
  
  data <- reactive({
    data.fun(admins, instrument(), input.measure())
  })
  
  xs <- 8:36
  
 
  
  # fit model to cat of interest
  model <- function () {
    if (input$robust) {
      rlm(as.formula(paste("aoa ~ ",str_c(input$pred_vars,collapse=" + ")," + 1", sep="")),
          data=d.subset(), na.action = na.exclude)
    } else {
      lm(as.formula(paste("aoa ~ ",str_c(input$pred_vars,collapse=" + ")," + 1", sep="")),
         data=d.subset(), na.action = na.exclude)      
    }
  }
  

  ############## PLOT #############
  
  plot <- function() {
    d.subset <- function() {
      if (input$cats == "All") {
        d 
      } else {      
        d %>% filter(small.cat %in% input$cats) 
      }
    }
    
    aoas <- data() %>% 
      rename(word = item) %>%
      group_by(word) %>%
      do(data.frame(aoa = xs[inv.logit(predict(lm(mean ~ age, data=.), 
                                               data.frame(age = xs))) > .5][1]))
    
    childes.freqs <- read_csv("childes.freqs.csv") %>%
      gather(word,input.freq) %>%
      group_by(word) %>%
      summarise(input.freq = log(sum(input.freq)))
    
    
    d <- aoas %>% 
      left_join(childes.freqs) 
    
    d.plot <- d
    d.plot$aoa.pred <- predict(model()) 
    d.plot %<>% filter(!is.na(aoa.pred))
    
    rmodel <- reactive({if (input$robust) { "rlm" } else { "lm" }})
    
    qplot(aoa.pred, aoa, 
          xlab = "Predicted AoA", 
          ylab = "Actual AoA", 
          data=d.plot) + 
      geom_smooth(method = rmodel())
  }
  
  ## Find word in multiple forms
  
  observe({
    if (length(input.forms()) == 1) {
      words <- names(filter(instrument.tables,
                            language == input.language(),
                            form == input.forms())$words.by.definition[[1]])
    } else {
      words1 <- names(filter(instrument.tables,
                             language == input.language(),
                             form == input.forms()[1])$words.by.definition[[1]])
      words2 <- names(filter(instrument.tables,
                             language == input.language(),
                             form == input.forms()[2])$words.by.definition[[1]])
      words <- intersect(words1, words2)
    }
    updateSelectInput(session, 'words', choices = words, selected = "")
  })

  ############## REACTIVE FORM HANDLERS ##############
  
  forms <- reactive({
    form_opts <- Filter(function(form) {form %in% unique(filter(instrument.tables,
                                                   language == input.language())$form)},
           list("Words & Sentences" = "WS", "Words & Gestures" = "WG"))
    
    if (all(c("WS", "WG") %in% form_opts)) {
      form_opts$"Both" <- "WG WS"
    }
    
    form_opts
  })
  
  measures <- reactive({
    if ("WG" %in% input.forms()) {
      list("Produces" = "produces", "Understands" = "understands")
    } else {
      list("Produces" = "produces")
    } 
  })
  
  
  ############## OUTPUT HANDLERS ############
  
  output$plot <- renderPlot({
    plot()    
  }, height = function() {
    session$clientData$output_plot_width * 0.7
  })  
  
  output$language_selector <- renderUI({    
    selectInput("language", label = h4("Language"), 
                choices = languages, selected = input.language())
  })
  
  output$form_selector <- renderUI({    
    selectInput("form", label = h4("Form"),
                choices = forms(), selected = input.form())
  })
  
  output$measure_selector <- renderUI({
    selectInput("measure", label = h4("Measure"), 
                choices = measures(), selected = input.measure())
  })
  
  table.data <- reactive({
    d <- data()
    if (nrow(d) == 0) {
      expand.grid(age = age.min():age.max(), form = input.forms()) %>%
        select(form, age)
    } else {
      d %>%
        select(form, age, item, mean) %>%
        spread(item, mean)
    }
  })
  
  output$table <- renderTable({
    table.data()
  }, include.rownames = FALSE, digits = 2)

  output$downloadTable <- downloadHandler(
    filename = function() { 'item_trajectory_table.csv' },
    content = function(file) {
      td <- table.data()
      extra.cols <- data.frame(language = rep(input.language(), nrow(td)),
                               measure = rep(input.measure(), nrow(td)))
      write.csv(bind_cols(extra.cols, td), file, row.names = FALSE)
      })
  
  output$downloadPlot <- downloadHandler(
    filename = function() { 'item_trajectory.pdf' },
    content = function(file) {
      cairo_pdf(file, width=10, height=7, family=font)
      print(plot())
      dev.off()
    })
  
  output$loaded <- reactive({1})
  
})
