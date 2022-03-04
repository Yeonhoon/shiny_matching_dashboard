shiny::shinyUI(dashboardPage(
  dashboardHeader(
    title="MIMIC-IV matching"
    ),
  dashboardSidebar(
    sidebarMenu(
      menuItem('Import Data', tabName='dataImport', icon = icon('file-csv',lib='font-awesome',verify_fa = FALSE)),
      menuItem('Matching', tabName='match',icon= icon('desktop',verify_fa = FALSE)),
      menuItem('Modeling', tabName='modeling', icon=icon('table',lib = 'font-awesome', verify_fa = FALSE))
    )
  ),
  dashboardBody(
    useShinyjs(),
    tags$head(                                           
      tags$link(rel='stylesheet', type='text/css', href='boxShadow.css'),
    ),
    tabItems(
# 1. import data --------------------------------------
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
                  shinyWidgets::pickerInput('trt',"Treatment variable",
                              choices=colnames(file)),
                  shinyWidgets::pickerInput('match','Matching variables', 
                              choices = colnames(file),
                              options= list(`actions-box`=T),
                              multiple = T)
                ),

# 2. setting parameter -------------------------------------------------------
                box(
                  title = strong('Parameters'),
                  status = 'primary',
                  tabsetPanel(
                    type='tabs',
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
                                  choices = c(1, 2, 3, 4, 5),
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
                                 style="color: white; background-color: #33ACFF;"
                                 ),
                    actionButton(inputId='reset',
                                 label="Reset",
                                 style="color: white; background-color: #FF6B6B;",
                                 icon = icon('repeat', lib = 'glyphicon',
                                             verify_fa = FALSE))
                )
              ),
              br(),
# 2. matching Info --------------------------------------------------------
              fluidRow(
                column(12, align='center',
                  box(
                    width=12,
                    title = strong('Matching Information'),
                    status = 'info',
                    collapsible = T,
                    uiOutput('renderMatchingInfo')
                  )
                )
              ),
              br(),

# 2. matching result --------------------------------------
              fluidRow(
                column(12,
                  box(
                    title=strong("Standard Mean Differences"),
                    status="danger",
                    height=400,
                    collapsible = T,
                    collapsed = F,
                    uiOutput('renderLovePlot')
                  ),
                  box(
                    title=strong('Macthing variable'),
                    status='danger',
                    sidebar = boxSidebar(
                      width = 30,
                      background = "#0B3F6F",
                      id='sidebar',
                      selectInput("matchedVars","Matched variables",
                                  choices = c(1,2)),
                      actionButton('update','Update')
                    ),
                    collapsible = T,
                    uiOutput(outputId = "renderMatchingVar")
                  )
                ),
              )
              )
      ), 

# 3. Modeling  ---------------------------------------------------------
      tabItem(
        tabName = "modeling",
        h2('Modeling with regressions'),
        fluidRow(
          box(
            title = "Selection",
            status='primary',
            solidHeader = T,
            selectInput('outcome',"Outcome variable",
                        choices=colnames(file)),
            selectInput('covariate','Covariate variables',
                           choices = names(file),
                           multiple = T),
            shinyWidgets::radioGroupButtons(
              inputId = 'regType',
              label = 'Choose Regression Type',
              justified = T,
              choices = c('Logistic','Cox'),
              status="primary",
            ),
            conditionalPanel(
              condition = "input.regType=='Cox'",
              selectInput('duration','Duration variable',
                             selected=NULL,
                            
                             choices = names(file)),
            ),
            shinyWidgets::materialSwitch('doStrata', value=F,
                                         status='primary',
                                      label = strong('Stratification')),
            conditionalPanel(
              condition='input.doStrata',
              selectInput('strata','Stratification variable',
                          choices=names(file),
                          selected = NA),
            ),
            actionButton('startReg','Run model',
                         icon=icon('play',lib='glyphicon')),
            actionButton('reset2','Reset',
                         icon=icon('repeat', lib='glyphicon'),
                         style="color: white; background-color: #FF6B6B;")
          ),
        ),
        fluidRow(
            box(
              # id='result',
              width=6,
              title='Before Matching',
              status = 'warning',
              conditionalPanel(
                condition = "input.startReg",
                withSpinner(uiOutput('renderUnmatcheResult'),
                            type=8, size=.3,hide.ui = F)
              ),
              solidHeader = T,
              collapsible = T
            ),
            # box(
            #   width = 6,
            #   title = 'Kaplan-Meier',
            #   status = 'warning',
            #   solidHeader = T,
            #   collapsible = T
            # )
          ),
        fluidRow(
          box(
            # id='result',
            title='After Matching',
            status = 'danger',
            width=6,
            conditionalPanel(
              condition = "input.startReg",
              withSpinner(uiOutput('renderMatchingResult'),
                          type=8, size=.3,hide.ui = F)
            ),
            solidHeader = T,
            collapsible = T
          ),
          # box(
          #   width = 6,
          #   title = 'Kaplan-Meier',
          #   status = 'danger',
          #   solidHeader = T,
          #   collapsible = T,
          #   conditionalPanel(
          #     condition = "input.startReg",
          #     uiOutput('renderMatchingKMPlot')
          #   )
          # )
        )
      )
    )
  ),
)
)