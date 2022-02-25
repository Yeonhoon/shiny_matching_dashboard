shiny::shinyUI(dashboardPage(
  dashboardHeader(
    title="MIMIC-IV matching"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Import Data', tabName='dataImport', icon = icon('file-csv',lib='font-awesome')),
      menuItem('Matching', tabName='match',icon= icon('desktop')),
      menuItem('Modeling', tabName='modeling', icon=icon('table',lib = 'font-awesome'))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(                                           
      tags$link(rel='stylesheet', type='text/css', href='boxShadow.css'),
    ),
    tabItems(
      # import data --------------------------------------
      tabItem(tabName='dataImport',
              h2('Import Data'),
              fluidRow(
                box(
                  solidHeader = T,
                  collapsible = T,
                  status = 'navy',
                  width=6, height=200,
                  title=tags$strong("CSV"),
                  fileInput("file", "Import a CSV file",
                            buttonLabel = "Import",
                            accept = c('.csv')
                  )
                ),
              ),
              fluidRow(
                tabsetPanel(
                  tabPanel(
                      width=6, height=600,
                      title = strong('Data Preview'),
                      collapsed = F, collapsible = T,
                      tags$br(),
                      DTOutput('datatable')
                  ),
                  tabPanel(
                      width = 6, height = 600,
                      collapsible = T,
                      title = strong('Structure of Data'),
                      DTOutput("strData")
                  )
                )
              ),
      ), 
      
# 2. Select variables -----------------------------
      tabItem(tabName='match',
              h2('Matching data'),
              fluidRow(
                box(
                  status = 'primary',
                  title= strong('Select variables'),
                  selectInput('trt',"Treatment variable",
                              choices=colnames(file)),
                  selectInput('match','Matching variables', 
                              choices = colnames(file),
                              multiple = T,
                              selectize = T)
                
                ),

# 2. setting parameter -------------------------------------------------------
                box(
                  title = strong('Parameters'),
                  status = 'primary',
                  tabsetPanel(
                    type = 'pills',
                    tabPanel(
                      br(),
                      title=strong('Caliper'),
                      sliderInput(inputId = 'caliperValue',
                                  label = 'Caliper',
                                  value = 0,
                                  min=0, max=0.5,
                                  step = 0.1)
                    ),
                    tabPanel(
                      br(),
                      title = strong('Ratio'),
                      selectInput(inputId='ratioValue',
                                  label='Ratio',
                                  choices = c(1, 2, 3,4,5),
                                  selected = 1)
                    )
                  )
                ),
              # icon: https://getbootstrap.com/docs/3.3/components/
              fluidRow(
                  column(12, align='center',
                    actionButton(inputId='startMatching',
                                 label='Matching Start',
                                 icon = icon('play',lib = 'glyphicon'),
                                 style="color: white;
                                        background-color: #33ACFF;"
                                 ),
                    actionButton(inputId='reset',
                                 label="Reset",
                                 icon = icon('repeat', lib = 'glyphicon',
                                             verify_fa = FALSE))
                )
              ),
              br(),
# 2. matching result --------------------------------------
              fluidRow(
                box(
                  status="danger",
                  width=6,
                  height=400,
                  collapsible = T,
                  title=strong("Standard Mean Differences"),
                  hidden(div(
                    id='hidden2',
                    plotOutput('lovePlot') %>% withSpinner(type=8, size=.3,hide.ui = F)
                    )
                  )
                ),
                box(
                  status='danger',
                  sidebar = boxSidebar(
                    width = 30,
                    background = "#0B3F6F",
                    id='sidebar',
                    # startOpen = T,
                    selectInput("matchedVars","Matched variables",
                                choices = c(1,2)),
                    actionButton('update','Update')
                    # selectInput("type",'Plot type',
                    #             choices = c('histogram',''))
                  ),
                  width=6,
                  collapsible = T,
                  title=strong('Macthing variable'),
                  hidden(div(
                    id='hidden1',
                    plotOutput(outputId = "mResult") %>% 
                      withSpinner(type=8, size=.3, hide.ui = F)
                  ))
                )
              )
            )
          ),

# 3. matched result ---------------------------------------------------------
      tabItem(
        tabName = "modeling",
        h2('Modeling with regressions'),
        fluidRow(
          box(
            title = "Selection",
            status='success',
            solidHeader = T,
            selectInput('outcome',"Outcome variable",
                        choices=colnames(file)),
            selectInput('covariate','Covariate variables',
                           choices = names(file),
                           multiple = T),
            radioButtons("regType",'Choose Regression Type',
                               choiceValues = list('logistic','cox'),
                               choiceNames = list('Logistic','Cox'),
                               inline=T
            ),
            conditionalPanel(
              condition = "input.regType=='cox'",
              selectInput('duration','Duration variable',
                             selected=NULL,
                            
                             choices = names(file)),
            ),
            checkboxInput('doStrata',
                          label = "Stratification?",
                          value = F),
            conditionalPanel(
              condition='input.doStrata',
              selectInput('strata','Stratification variable',
                          choices=names(file),
                          selected = NA),
            ),
            actionButton('startReg','Run model',
                         icon=icon('play',lib='glyphicon'))
          ),
        ),
        fluidRow(
          box(
            # id='result',
            title='Before Matching',
            status = 'warning',
            width=6,
            conditionalPanel(
              condition = "input.startReg",
              # uiOutput('renderResult')
            ),
            solidHeader = T,
            collapsible = T
          ),
          box(
            # id='result',
            title='After Matching',
            status = 'danger',
            width=12,
            conditionalPanel(
              condition = "input.startReg",
              withSpinner(uiOutput('renderResult'),
                          type=8, size=.3,hide.ui = F)
            ),
            solidHeader = T,
            collapsible = T
          )
        )
      )
    )
  ),
)
)