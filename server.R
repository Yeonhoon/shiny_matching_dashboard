shiny::shinyServer(function(input, output, session){
  
  ns <- session$ns
  rv <- reactiveValues(data=NULL)
  matchingResult <- reactiveValues(data=NULL)
  matchingData <- reactiveValues(data=NULL)
  options(shiny.maxRequestSize = 30*1024^2)
  

#**custom functions --------------------------------------------------------

  
  myEval <- function(text){
    eval(parse(text=text))
  }
  
  # check class of each variables
  classTable <- function(x){
    varname <- c()
    classname <- c()
    for (i in colnames(x)){
      varname <- c(varname,i)
      classname <- c(classname, class(x[[i]]))
    }
    tab <- as.data.frame(cbind(varname,classname))
    colnames(tab) <- c('Variable','Class')
    return(tab)
  }
  
  # result for regression
  
  listReg <- function(data, regType, formula, strata){
    L <- list()
    rst <- by(data, strata, function(sub_df) {
      if(regType=='logistic') {
        fit <- glm(formula, data=sub_df, family=binomial())
      } else {
        fit <- coxph(formula, data=sub_df)
      }
    })
    for (i in (names(rst))){
      if(regType=='logistic'){
        L[[i]] <- tbl_regression(rst[[i]], exponentiate = T) %>% 
          modify_cols_merge(pattern = "{estimate} [{ci}]",
                            row = !is.na(estimate)) %>% 
          modify_header(label="**Variable**", estimate="**OR (95% CI)**") 
      }
      else {
        L[[i]] <- tbl_regression(rst[[i]], exponentiate = T) %>% 
          modify_cols_merge(pattern = "{estimate} [{ci}]",
                            row = !is.na(estimate)) %>% 
          modify_header(label="**Variable**", estimate="**HR (95% CI)**") 
      }
    }
    return(L)
  }
  
  uniReg <- function(data,regType, formula){
    if(regType == 'logistic'){
      fit <- glm(formula, data=data, family='binomial')
      rst <- tbl_regression(fit,exponentiate=T) %>% 
        modify_cols_merge(pattern = "{estimate} [{ci}]",
                          row = !is.na(estimate)) %>% 
        modify_header(label="**Variable**", estimate="**OR (95% CI)**") 
    } else {
      fit <- coxph(formula, data=data)
      rst <- tbl_regression(fit,exponentiate=T) %>% 
        modify_cols_merge(pattern = "{estimate} [{ci}]",
                          row = !is.na(estimate)) %>% 
        modify_header(label="**Variable**", estimate="**HR (95% CI)**") 
    }
    return(rst)
  }
  
  
  # Result table
  GTTab <- function(x, strata=NA){
    if (!is.na(strata)){
      x %>% 
        as_gt() %>% 
        tab_header(paste('Stratified by', input$strata)) %>% 
        tab_options(table.font.names = 'Times New Roman') 
    } else {
      x %>% 
        
        as_gt() %>% 
        # tab_header(paste('Stratified by', input$strata)) %>% 
        tab_options(table.font.names = 'Times New Roman')
    }
  }
  
  
  # 1. import Data ----------------------------------------------------
  observeEvent(input$file, {
    rv$data <- fread(input$file$datapath, encoding='UTF-8', 
                     header=T,stringsAsFactors = F, sep = ",") %>% as.data.table()

    output$datatable <- renderDT({
      datatable(head(rv$data,10), 
                extensions = c("Scroller", "ColReorder", "KeyTable"),
                options = list(lengthChange = F,
                               scrollY = T,
                               scrollX = T
                ))
    })
    shinyWidgets::show_alert(
      type='success',
      title='Success !!',
      text = "Data has been imported.")
    
    # refresh results
    shinyWidgets::updatePickerInput(session, 'trt', label='Target Variable',
                                    choices = names(rv$data),
                                    options = list(size=5,
                                                   title="Select a target variable"))
    shinyWidgets::updatePickerInput(session, 'match', label='Matching Variables',
                                    choices = names(rv$data),
                                    options = list(size=5,
                                                   `actions-box`=T,
                                                   title="Select matching variables"))
    # updateSelectInput(session, 'trt', label='Target Variable', 
    #                   choices = c(`Select a variable`="",names(rv$data)), selected = NA)
    # updateSelectInput(session, 'match', label='Matching Variables',choices = names(rv$data), selected=NA)
    
    # structure of data
    output$strData <- renderDT({
      datatable(classTable(rv$data),
                extensions = c("Scroller","ColReorder","KeyTable"),
                options = list(lengthChange = F,
                               scrollY = T, scrollX=T))
    })
  })
  
  # matching formula
  result <- eventReactive(input$startMatching, {
    req(input$trt, input$match)
    temp <- ""
    if(length(input$match)>0) {
        temp <- paste0(input$trt,'~', paste0(input$match, collapse = "+"))
    }
    for(i in input$match){
      if(is.character(rv$data[[i]])){
        rv$data[[i]] <- as.factor(rv$data[[i]])
      }
    }
    if(is.character(rv$data[[input$trt]])) rv$data[[input$trt]] <- as.factor(rv$data[[input$trt]])
    set.seed(7795)
    if(input$caliperValue>0 & input$ratioValue>0){
      m.out <- MatchIt::matchit(formula = myEval(temp),data=rv$data,
                                caliper = input$caliperValue,
                                ratio = as.numeric(input$ratioValue))
    } else {
      m.out <- matchit(formula = myEval(temp),
                       data=rv$data)
    }
  })
  
  
  # Exclude Treatment variable on matching vars
  observeEvent(input$trt,{
    vars <- setdiff(names(rv$data),input$trt)
    shinyWidgets::updatePickerInput(session,'match',
                      label = "Matching Variables",
                      selected=NA,
                      choices=vars)
  })
  
  observeEvent(input$outcome, {
    vars <- setdiff(names(matchingData$data),input$outcome)
    shinyWidgets::updatePickerInput(session,'covariate',
                      label = 'Covariate variables',
                      selected=NA,
                      choices = vars)
  })
  
  # matching info
  observeEvent(input$startMatching, {
    output$matchingInfo <- renderPrint({
      print(result())
    })
    
    output$renderMatchingInfo <- renderUI({
      verbatimTextOutput('matchingInfo')
    })
  })
  
  # loveplot --------------------------------
  observeEvent(input$startMatching,{
      output$lovePlot <- renderPlot({
        tryCatch({
          shinyWidgets::show_alert(
            type='success',
            title='Success !!',
            text = "Matching succeeded")
          cobalt::love.plot(result(),
                            stars='raw',
                            drop.distance = T,
                            drop.missing = T,
                            abs = T,
                            var.order = 'unadjusted',
                            thresholds = 0.2,
                            themes = ggthemes::theme_stata() + 
                              theme(axis.text.y = element_text(angle=0,size=13,family = 'serif'),
                                    axis.text.x= element_text(size=13, family='serif'),
                                    legend.text = element_text(size=13))
          )
          
        },
        warning = function(w){
          shinyalert::shinyalert("Oops!",'Something Wrong !!',
                                 type = 'error')
          return ()
        },
        error = function(e){
          shinyalert::shinyalert("Oops!",'Something Wrong !!',
                                 type = 'error')
        }
        
        )
      })
      output$renderLovePlot <- renderUI({
        plotOutput('lovePlot')
      })
  })
  
# bal.plot ----------------------------------------------------------------

  observeEvent(input$startMatching,{
      updateBoxSidebar("sidebar",session=session)
      updateSelectInput(session=session, 
                      inputId = 'matchedVars', 
                      label='Matched variables',
                      choices = input$match)
      
      output$mResult <- renderPlot({
        cobalt::bal.plot(result(),
                         var.name=input$matchedVars,
                         which = 'both',
                         themes = ggthemes::theme_stata() + 
                           theme(axis.text.y = element_text(angle=0,size=13,family = 'serif'),
                                 axis.text.x= element_text(size=13, family='serif'),
                                 legend.text = element_text(size=13)) )
      })
      
      output$renderMatchingVar <- renderUI({
        plotOutput('mResult')
      })
  })  
      
  observeEvent(input$update,{
      # output$mResult <- renderPlot({
      #   cobalt::bal.plot(result(),
      #                    var.name=input$matchedVars,
      #                    which = 'both',
      #                    themes = ggthemes::theme_stata() + 
      #                      theme(axis.text.y = element_text(angle=0,size=13,family = 'serif'),
      #                            axis.text.x= element_text(size=13, family='serif'),
      #                            legend.text = element_text(size=13)) )
      # })
      
      output$renderMatchingVar <- renderUI({
        plotOutput('mResult')
      })
  })


# result -------------------------------------------------------------------------

  #매칭 데이터 사용 시 selector update
  observeEvent(input$startMatching,{
    matchingData$data <- match.data(object = result(),
                                    distance = 'prop.score', 
                                    weights = 'wt',
                                    subclass = 'sc')
    updateSelectInput(session, 'outcome', label='Outcome Variable',choices = names(matchingData$data), selected=NA)
    updateSelectInput(session, 'covariate', label='Covariates',choices = names(matchingData$data))
    updateSelectInput(session, 'duration', label='Duration Variable',choices = names(matchingData$data))
    updateSelectInput(session, 'strata', label='Stratification Variable',
                      choices = names(matchingData$data[,!sapply(matchingData$data,is.numeric),with=F]),
                      selected=NA)
  })
  
# Create formula ------------------------------------------------
  
  matchFit <- eventReactive(input$startReg,{
    req(input$outcome, input$covariate)
    temp <- ""
    for(i in input$covariate){
      if(is.character(matchingData$data[[i]])){
        matchingData$data[[i]] <- as.factor(matchingData$data[[i]])
      }
    }
    if(is.character(matchingData$data[[input$outcome]])) {
      matchingData$data[[input$outcome]] <- as.factor(matchingData$data[[input$outcome]]) 
    }
    
    if(length(input$covariate)>0) {
      if(input$regType=='Logistic'){
        temp <- paste0(input$outcome,'~', paste0(input$covariate, collapse = "+"))
        if(input$doStrata){
          rst <- listReg(data = matchingData$data,
                         regType = 'logistic',
                         formula = myEval(temp),
                         strata = matchingData$data[[input$strata]])
          x <- tbl_merge(rst,
                    tab_spanner = names(rst))
          GTTab(x, strata = input$strata)
        } else {
          rst <- uniReg(matchingData$data,regType = 'logistic', formula = myEval(temp))
          GTTab(rst)
        }
      } else {
        temp <- paste0("Surv(",input$duration,",", input$outcome,")",'~', paste0(input$covariate, collapse = "+"))
        if (input$doStrata){
          rst <- listReg(data = matchingData$data,regType = 'cox',
                         formula = myEval(temp),
                         strata = matchingData$data[[input$strata]])
          x <- tbl_merge(rst,
                    tab_spanner = names(rst))
          GTTab(x, strata = input$strata)
        } else {
          rst <- uniReg(matchingData$data,regType = 'cox', formula = myEval(temp))
          GTTab(rst)
        }
      }
    }
  })
  
  unmatchedFit <- eventReactive(input$startReg,{
    req(input$outcome, input$covariate)
    temp <- ""
    for(i in input$covariate){
      if(is.character(rv$data[[i]])){
        rv$data[[i]] <- as.factor(rv$data[[i]])
      }
    }
    if(is.character(rv$data[[input$outcome]])) {
      rv$data[[input$outcome]] <- as.factor(rv$data[[input$outcome]]) 
    }
    if(length(input$covariate)>0) {
      # logistic
      if(input$regType=='Logistic'){
        temp <- paste0(input$outcome,'~', paste0(input$covariate, collapse = "+"))
        if(input$doStrata){
          rst <- listReg(data = rv$data,
                         regType = 'logistic',
                         formula = myEval(temp),
                         strata = rv$data[[input$strata]])
          x <- tbl_merge(rst,
                         tab_spanner = names(rst))
          GTTab(x, strata = input$strata)
        } else {
          rst <- uniReg(rv$data,regType = 'logistic', formula = myEval(temp))
          GTTab(rst)
        }
        # cox
      } else {
        temp <- paste0("Surv(",input$duration,",", input$outcome,")",'~', paste0(input$covariate, collapse = "+"))
        if (input$doStrata){
          rst <- listReg(data = rv$data,regType = 'cox',
                         formula = myEval(temp),
                         strata = rv$data[[input$strata]])
          x <- tbl_merge(rst,
                         tab_spanner = names(rst))
          GTTab(x, strata = input$strata)
        } else {
          rst <- uniReg(rv$data,regType = 'cox', formula = myEval(temp))
          GTTab(rst)
        }
      }
    }
  })
  
  strataVar <- eventReactive(input$startReg,{
    req(input$strata)
    input$strata
  })
  
  regTypeVar <- eventReactive(input$startReg,{
    req(input$regType)
    input$regType
  })
  
  
  # regression result: OR,HR -----------------------------------------
  
  observeEvent(input$startReg,{
    output$matchingResult <- render_gt({
      matchFit()
    })
    
    output$unmatchingResult <- render_gt({
      unmatchedFit()
    })
    
    # output$matchingKM <- renderPlotly({
    #   fit = survfit(Surv(input$duration, input$outcome) ~ input$covariate, data=matchingData$data)
    #   survminer::ggsurvplot(fit)
    # })
    
  })
  
  output$renderMatchingResult <- renderUI({
    withProgress('Creating Tables', min = 0, max=100, value = 0)
    gt_output('matchingResult')
  })
  
  output$renderUnmatcheResult <- renderUI({
    withProgress('Creating Tables', min = 0, max=100, value = 0)
    gt_output('unmatchingResult')
  })
  
  # output$renderMatchingKMPlot <- renderUI({
  #   plotlyOutput('matchingKM')
  # })
  

  
# Reset -------------------------------------------------------------------
  observeEvent(input$reset,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmReset",
      session = session,
      title="Warning",
      type = 'warning',
      text="Reset all variables?",
      btn_labels = c("Cancel", "Reset"),
      btn_colors = c("#DDDDDD", "#FE2E2E"),
      html=T,
      closeOnClickOutside=T,
      showCloseButton=T
    )
  })
  
  observeEvent(input$confirmReset,{
    if(input$confirmReset){
      updateSelectInput(session, 'trt', label='Target Variable', 
                        choices = c(`Select a variable`="",names(rv$data)), selected = NA)
      updateSelectInput(session, 'match', label='Matching Variable',choices = names(rv$data), selected=NA)
      updateSelectInput(session=session, 
                        inputId = 'matchedVars', 
                        label='Matched variables')
      updateBoxSidebar("sidebar",session=session)
      output$renderLovePlot <- renderUI(NULL)
      output$renderMatchingInfo <- renderUI(NULL)
      output$renderMatchingVar <- renderUI(NULL)
      updateSelectInput(inputId='ratioValue', label='Ratio', choices = c(1, 2, 3,4,5), selected = 1)
      updateSliderInput(inputId = 'caliperValue', label = 'Caliper', value = 0, min=0, max=0.5, step = 0.1)
      updateSelectInput(session, 'outcome', label='Outcome Variable',choices = names(matchingData$data), selected=NA)
      updateSelectInput(session, 'covariate', label='Covariates',choices = names(matchingData$data))
      updateSelectInput(session, 'duration', label='Duration Variable',choices = names(matchingData$data))
      updateSelectInput(session, 'strata', label='Stratification Variable',
                        choices = names(matchingData$data[,!sapply(matchingData$data,is.numeric),with=F]),
                        selected=NA)
      output$renderUnmatcheResult <- renderUI(NULL)
      output$renderMatchingResult <- renderUI(NULL)
    }
  })
  
  observeEvent(input$reset2,{
    shinyWidgets::ask_confirmation(
      inputId = "confirmReset2",
      session = session,
      title="Warning",
      type = 'warning',
      text="Reset all variables?",
      btn_labels = c("Cancel", "Reset"),
      btn_colors = c("#DDDDDD", "#FE2E2E"),
      html=T,
      closeOnClickOutside=T,
      showCloseButton=T
    )
  })
  
  observeEvent(input$confirmReset2,{
    if(input$confirmReset2){
      updateSelectInput(session, 'outcome', label='Outcome Variable',choices = names(matchingData$data), selected=NA)
      updateSelectInput(session, 'covariate', label='Covariates',choices = names(matchingData$data))
      updateSelectInput(session, 'duration', label='Duration Variable',choices = names(matchingData$data))
      updateSelectInput(session, 'strata', label='Stratification Variable',
                        choices = names(matchingData$data[,!sapply(matchingData$data,is.numeric),with=F]),
                        selected=NA)
      output$renderUnmatcheResult <- renderUI(NULL)
      output$renderMatchingResult <- renderUI(NULL)
    }
  })
  
}

)



