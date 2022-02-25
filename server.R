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
    shinyalert::shinyalert("Good!", "Data has been imported.",
                           type = 'success')
    
    # refresh results
    
    updateSelectInput(session, 'trt', label='Target Variable', 
                      choices = c(Vars="",names(rv$data)), selected = NA)
    updateSelectInput(session, 'match', label='Matching Variable',choices = names(rv$data), selected=NA)
    
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
  
  # loveplot --------------------------------
  observeEvent(input$startMatching,{
    shinyjs::toggle(id='hidden2', condition = F)
    if(nchar(input$startMatching)>0){
      show('hidden2')
      toggle(id='hidden2',condition=T)
      
      output$lovePlot <- renderPlot({
        tryCatch({
          shinyalert::shinyalert("Good!",'Macting was successful !!',
                                 type = 'success')
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
    }
  })
  
  

# bal.plot ----------------------------------------------------------------

  observeEvent(input$startMatching,{
    if(nchar(input$startMatching)>0){
      shinyjs::toggle(id='hidden1', condition = F)
      show('hidden1')
      updateBoxSidebar("sidebar",session=session)
      updateSelectInput(session=session, 
                      inputId = 'matchedVars', 
                      label='Matched variables',
                      choices = input$match)
        }
      })  
      
  observeEvent(input$update,{
    if(input$update){
      show('hidden1')
      toggle(id='hidden1',condition=T)
      output$mResult <- renderPlot({
        cobalt::bal.plot(result(),
                         var.name=input$matchedVars,
                         which = 'both',
                         themes = ggthemes::theme_stata() + 
                           theme(axis.text.y = element_text(angle=0,size=13,family = 'serif'),
                                 axis.text.x= element_text(size=13, family='serif'),
                                 legend.text = element_text(size=13)) )
      })
    }
  })


# result -------------------------------------------------------------------------

  #매칭 데이터 사용
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
    for(i in matchingData[[input$covariate]]){
      if(is.character(matchingData$data[[i]])){
        matchingData$data[[i]] <- as.factor(matchingData$data[[i]])
      }
    }
    if(is.character(matchingData$data[[input$outcome]])) {
      matchingData$data[[input$outcome]] <- as.factor(matchingData$data[[input$outcome]]) 
    }
    
    if(length(input$covariate)>0) {
      if(input$regType=='logistic'){
        temp <- paste0(input$outcome,'~', stringr::str_c(input$covariate, collapse = "+"))
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
        temp <- paste0("Surv(",input$duration,",", input$outcome,")",'~', stringr::str_c(input$covariate, collapse = "+"))
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
  })
  
  output$renderResult <- renderUI({
    gt_output('matchingResult')
  })
}
)
