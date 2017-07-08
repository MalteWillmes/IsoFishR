install_load <- function (package1,...)  {   
  
  # convert arguments to vector
  packages <- c(package1,...)
  
  # start loop to determine if each package is installed
  for(package in packages){
    
    # if package is installed locally, load
    if(package %in% rownames(installed.packages())){
      do.call('library', list(package))}
    
    # if package is not installed locally, download, then load
    else {
      install.packages(package)
      do.call("library", list(package))
    }
  } 
}

install_load('shiny', 'shinydashboard', 'tidyverse', 'DT', 'zoo', 'ggplot2', 'changepoint','colourpicker')

ui <- dashboardPage(skin="black",
                    dashboardHeader(title = "IsoFishR",
                                    tags$li(a(target="_blank", href = 'http://www.hobbslab.com',
                                              img(src = 'logo.png',
                                                  title = "Hobbslab", height = "30px"),
                                              style = "padding-top:10px; padding-bottom:10px;"),
                                            class = "dropdown")),
                                                 
  dashboardSidebar(
    sidebarMenu(
      p(""),
      menuItem("About", tabName="About", icon=icon("cog")),
      menuItem("Projects", tabName="Projects", icon=icon("folder")),
      menuItem("Analysis", tabName="Analysis",icon=icon("area-chart")),
      menuItem("Data Explorer", tabName="Dataexplorer",icon=icon("bar-chart")),
      menuItem("Data Table", tabName="Data", icon=icon("table")))),

  
  dashboardBody(
    tabItems(
      tabItem(tabName = "About",
               fluidRow(
                box(width = 10, status = "primary",
                  h2("Welcome to IsoFishR"),
                  hr(),
                  h4("This application is designed to process raw run files 
                     from laser ablation (LA) strontium isotope (Sr) analysis of carbonate and bioapatite samples. It includes many features
                     to make data management and analysis easy."),
                  h3("Projects"),
                  h4("Full project management including creating a new project based on a group of run files, 
                    loading an existing project to continue processing samples, updating project settings after a project has been created, and
                    commenting."),
                  tags$ul(
                    tags$li(h4("New project creation including subfolders to organize data and plots:")),
                    img(src='newproject.png'),
                    br(),
                    br(),
                    tags$li(h4("Select a project from the list of existing projects and load default settings. Analyze as many
                            samples as you have time for and pick up later where you left off:")),
                    img(src='selectproject.png'),
                    br(),
                    br(),
                    tags$li(h4("Update a project's settings to the current settings once the best values have been determined:")),
                    img(src='updateproject.png'),
                    br(),
                    br(),
                    tags$li(h4("Runfile settings: Displays the values of the settings needed to read the run file into the app. Values can be changed here.")),
                    img(src='runfilesettings.png'),
                    br(),
                    br(),
                    tags$li(h4("Input values: Displays the values of the project parameters such as run speed and detection limits. These values
                               can be changed on the analysis tab.")),
                    img(src='inputvalues.png'),
                    br(),
                    br(),
                    tags$li(h4("Project comments: Keep track of progress and project details with the ability to save and view project-wide comments.")),
                    img(src='projectcomment.png'),
                    br(),
                    br()),
                  h3("Analysis"),
                  h4("Upload, analyze, and save processed data with the data analysis tab. Manually or automatically set profile regions, comment on
                    the profile, trim data, remove outliers, flag for review, and more!"),
                  img(src='analysis.png',height="900",width="1300"),
                  h3("Data"),
                  h3("Example Data")
                  ),
                box(
                  title = "Version history", width = 2, status="warning",
                  "0.1 - inital release"
                ),
              box(
                title = "Authors", width = 2, status="danger",
                p("Katherine Ransom (lockhart.katherine@gmail.com)
                Malte Willmes (mwillmes@ucdavis.edu)")
              ))
    
               ),

      tabItem(tabName = "Projects",
              fluidRow(
                box(
                  title = "Create a new project and save current settings", width = 4, status = "primary",
                  textInput(inputId="new.project",label="New Project Name",value="Project"),
                  actionButton("save","Save")
                ),
                box(
                  title = "Project selection", width = 4, status = "primary",
                  selectInput(inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected="Default")
                ),
                box(
                  title = "Update project settings", width = 4, status = "primary",
                  actionButton("updatesettings","Update")
                )
                ),
                fluidRow(
                  box(
                  title = "Runfile settings", width = 4, status="warning",
                  #fileInput("settings_file",label=NULL),
                  numericInput("vskip","Header length",value=46),
                  numericInput("raw88","Raw 88 column number",value=1),
                  numericInput("raw87","Raw 87 column number",value=2),
                  numericInput("raw86","Raw 86 column number",value=3),
                  numericInput("raw85","Raw 85 column number",value=4),
                  numericInput("raw84","Raw 84 column number",value=5),
                  numericInput("raw83","Raw 83 column number",value=6),
                  numericInput("cyclesec","Cycle seconds column number",value=8),
                  radioButtons("sep","Separator type",choices = list(Comma=",",Tab="\t",Decimal=".")),
                  checkboxInput("header","Data has column headings"),
                  numericInput("integration","Integration time",value=5)
                  
               ),
                box(
                  title="Input values",status="warning",width=4,DT::dataTableOutput("inputVals")),
                box(title="Project comments",status="warning",width=4,
                    textAreaInput(inputId="project.comment",label=NULL,value="Add comment here"),
                    actionButton("comment","Save Comment"),DT::dataTableOutput("comments")))),

      tabItem(tabName = "Analysis",
                      fluidRow(
                        column(9,fluidRow(
                        box(
                          width = 12, status = "primary",
                          plotOutput("plot1",brush = brushOpts(id ="plot_brush",direction ="x"))),
                        
                        box(width=4,height=270,
                            fileInput("file","Analyze runfile"),
                            
                            actionButton("appendnew","Save Data"),
                            verbatimTextOutput("overwritewarn"),
                            verbatimTextOutput("warnmsg")),
                        
                        box(width=8,height=100,
                            div(style="float:left; width:70px",numericInput("trimleft","Left trim",value=-1)),
                            div(style="float:left; width:70px",numericInput("trimright","Right trim",value=-1)),
                            div(style="float:left; width:70px;",checkboxInput("points", "Points")),
                            div(style="float:left; width:70px;",checkboxInput("ocean", "Ocean")),
                            div(style="height:40px; float:right",downloadButton('downloadplot1','')),
                            div(style="float:left; width:100px",colourInput("linecol", "Line", "black", allowTransparent = TRUE,transparentText = "None",showColour = "background")),
                            div(style="float:left; width:100px",colourInput("shadecol", "Shading", "grey", allowTransparent = TRUE, transparentText = "None", showColour = "background")),
                            verbatimTextOutput("brush_info")
                            
                        ),
                        box(width=8,height=150,
                            div(style="float:left; width:400px",textInput("profile.comment","Comment on profile","")),
                            div(style="float:left; width:100px",textInput("SampleID","Sample ID","")),
                            div(style="float:left; width:100px",textInput("Userinitials","User")),
                            div(style="float:left; width:200px",checkboxInput("flag","Flag for review")))),
                        
                      fluidRow(                          
                          box(
                          width=4,collapsible=TRUE, collapsed=TRUE, title="Change parameters",
                          div(style="float:left; width:90px", numericInput("Raw88LowerThresh", "Min 88Sr", value=0.5, step = .1)),
                          div(style="float:left; width:90px",numericInput("Raw88UpperThresh", "Max 88Sr", value=9.8, step = .1)),
                          div(style="float:left; width:90px",numericInput("Speed", "Run speed", value=10, step = 1)),
                          div(style="float:left; width:125px;",radioButtons("smoother","Smoothing type",choices = list("MA"="MA","Loess"="loess"),inline=TRUE)),
                          div(style="float:left; width:90px",numericInput("average_num","MA window",value=10,step=1,min=1,max=NA)),
                          div(style="float:left; width:90px",numericInput("span","Loess span",value=0.05,step=0.01,min=0.01,max=1)),
                          div(style="float:left; width:120px",radioButtons("CI","CI for outliers",choices = list("3"=0.9985,"2"=0.975,"1"=0.84),inline=TRUE)),
                          div(style="float:left; width:90px",numericInput("outlier_num","Outlier num",value=50,step=1,min=30)),
                          div(style="float:left; width:90px",numericInput("fluency","Fluency",value=1.85)),
                          div(style="float:left; width:90px",numericInput("spotsize","Spot size",value=40)),
                          div(style="float:left; width:90px",numericInput("energy","Laser energy",value=55)),
                          div(style="float:left; width:90px",textInput("sampletype",label="Sample type",value="IsoFishR"))),
                          
                          tabBox(title="Filters",height=240,
                            width=8, selected="About",
                            tabPanel("About", "Filters can be used to split the data into different regions"),
                            tabPanel("Manual",
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_one_label", label=NULL, value = "Core")),
                                div(style="height:35px",numericInput("range_one_min","",value=-1)),
                                div(style="height:55px",numericInput("range_one_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion1", "Read",style="background-color:#6b8acd"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_two_label", label=NULL, value = "Natal")),
                                div(style="height:35px",numericInput("range_two_min","",value=-1)),
                                div(style="height:55px", numericInput("range_two_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion2", "Read",style="background-color:#4cb28d"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_three_label",label=NULL, value = "Adult")),
                                div(style="height:35px",numericInput("range_three_min","",value=-1)),
                                div(style="height:55px",numericInput("range_three_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion3", "Read",style="background-color:#72853c"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_four_label",label=NULL, value = "Range 4")),
                                div(style="height:35px",numericInput("range_four_min","",value=-1)),
                                div(style="height:55px", numericInput("range_four_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion4", "Read",style="background-color:#c78a40"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_five_label", label=NULL, value = "Range 5")),
                                div(style="height:35px",numericInput("range_five_min","",value=-1)),
                                div(style="height:55px",numericInput("range_five_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion5", "Read",style="background-color:#c75d9c"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_six_label", label=NULL, value = "Range 6")),
                                div(style="height:35px",numericInput("range_six_min","",value=-1)),
                                div(style="height:55px",numericInput("range_six_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion6", "Read",style="background-color:#cb554f"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_seven_label", label=NULL, value = "Range 7")),
                                div(style="height:35px",numericInput("range_seven_min","",value=-1)),
                                div(style="height:55px",numericInput("range_seven_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion7", "Read",style="background-color:#9061c9"))),
                            
                            div(style="float:left;width: 80px",
                                div(style="height:20px",textInput("range_eight_label", label=NULL, value = "Range 8")),
                                div(style="height:35px",numericInput("range_eight_min","",value=-1)),
                                div(style="height:55px",numericInput("range_eight_max","",value=-1)),
                                div(style= "height:35px",actionButton("readregion8", "Read",style="background-color:#83b844"))),
                            div(style="float:right; width:125px",actionButton("reset_ranges", "Reset ranges"))),

                          
                            tabPanel("Changepoints", 
                                     div(style="float:left;width:120px;",checkboxInput("changepoints", "Enable Changepoints")),
                                     div(style="float:left;width:100px",numericInput("manual_pen",label="Penalty value",value=0.00005,step=0.00001,min=0,max=0.01)),
                                     div(style="float:left;width:100px",selectInput("change_method",label="Method",choices= c("PELT", "AMOC", "SegNeigh", "BinSeg"), selected="PELT"))
                                     ))
                          
                        
                          )
                        ),
                        
                        column(3,fluidRow(
                        box(
                          width =12, status = "warning",style='padding:2px;',
                          plotOutput("plot2",height="195px")),
                        box(width=12, status="warning",style='padding:2px;',
                          plotOutput("plot3",height="195px")),
                        box(width=12, status="warning",style='padding:2px;',
                          plotOutput("plot4",height="195px")),
                        box(width=12, status="warning",style='padding:2px;',
                          plotOutput("outlier_plot",height="195px"))))
                      )),

      tabItem(tabName = "Dataexplorer",
              fluidRow(
                box(
                  title = "Aggregate Data", width = 4, status = "primary",
                  selectInput(inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected="Default"),
                  actionButton("aggregatedata","Aggregate Data")
                ),
                box(
                  title = "Batch processing", width = 4, status = "primary",
                  selectInput(inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected="Default")
                ),
                box(
                  title = "Plotting data", width = 4, status = "primary"
                
                  ))),

      tabItem(tabName = "Data",
              fluidRow(tabBox(
              width = 12,
               # id = "datatabset",
                tabPanel("Input data",uiOutput("tb")), 
                tabPanel("Processed data",uiOutput("processed")),
                tabPanel("Background data",uiOutput("background")))))
      
              
    
)))

server <- shinyServer(function(input, output, session) {
  
  # sets the reac values when reac$redraw=TRUE 
  reac <- reactiveValues(redraw = TRUE, range_one_min = isolate(input$range_one_min), range_one_max  = isolate(input$range_one_max),
                         range_two_min = isolate(input$range_two_min), range_two_max  = isolate(input$range_two_max),
                         range_three_min = isolate(input$range_three_min), range_three_max  = isolate(input$range_three_max),
                         range_four_min = isolate(input$range_four_min), range_four_max  = isolate(input$range_four_max),
                         range_five_min = isolate(input$range_five_min), range_five_max  = isolate(input$range_five_max),
                         range_six_min = isolate(input$range_six_min), range_six_max  = isolate(input$range_six_max),
                         range_seven_min = isolate(input$range_seven_min), range_seven_max  = isolate(input$range_seven_max),
                         range_eight_min = isolate(input$range_eight_min), range_eight_max  = isolate(input$range_eight_max))
  
  # If any inputs are changed, set the redraw parameter to FALSE, this sets the redraw parameter to FALSE upon new file
  # upload as well because the range inputs get reset
  # testing on if input$range_one_min==-1 gives and error in if (argument is of length zero) because the input ranges
  # are initially null
  # reac$redraw is true until a range input is changed, then it becomes false
  # this also causes reac$redraw to become false when range inputs are reset (delay in range removal)
  
  observe({
    input$range_one_min
    input$range_one_max
    input$range_two_min
    input$range_two_max
    input$range_three_min # removal of this code chunk will prevent timer from functioning
    input$range_three_max
    input$range_four_min
    input$range_four_max
    input$range_five_min
    input$range_five_max
    input$range_six_min
    input$range_six_max
    input$range_seven_min
    input$range_seven_max
    input$range_eight_min
    input$range_eight_max
    if(input$range_one_min==-1){reac$redraw <- TRUE} # close but still see two flashes for new file upload due to reactive values set after initial redraw
    else{reac$redraw <- FALSE} 
  }) 
  
  # This event will fire for a change to the ranges, but will also fire for
  # a timer and with the 'redraw now' button.
  # The net effect is that when an input is changed, a 2 second timer
  # is started. This will be reset any time that a further input is
  # changed. If it is allowed to lapse (or if the button is pressed)
  # then the inputs are copied into the reactiveValues which in turn
  # trigger the plot to be redrawn.
  observe({
    invalidateLater(2000, session)
    input$range_one_min
    input$range_one_max
    input$range_two_min
    input$range_two_max
    input$range_three_min
    input$range_three_max
    input$range_four_min
    input$range_four_max
    input$range_five_min
    input$range_five_max
    input$range_six_min
    input$range_six_max
    input$range_seven_min
    input$range_seven_max
    input$range_eight_min
    input$range_eight_max
    input$redraw # required for redraw now "submit ranges" button to work by triggering the code below
    if (isolate(reac$redraw)) { # if redraw now button "submit ranges" is pressed then set the reactive values immediately (if this value is TRUE)
      reac$range_one_min <- input$range_one_min
      reac$range_one_max <- input$range_one_max
      reac$range_two_min <- input$range_two_min
      reac$range_two_max <- input$range_two_max
      reac$range_three_min <- input$range_three_min
      reac$range_three_max <- input$range_three_max
      reac$range_four_min <- input$range_four_min
      reac$range_four_max <- input$range_four_max
      reac$range_five_min <- input$range_five_min
      reac$range_five_max <- input$range_five_max
      reac$range_six_min <- input$range_six_min
      reac$range_six_max <- input$range_six_max
      reac$range_seven_min <- input$range_seven_min
      reac$range_seven_max <- input$range_seven_max
      reac$range_eight_min <- input$range_eight_min
      reac$range_eight_max <- input$range_eight_max
      }    
    else {
      isolate(reac$redraw <- TRUE)
    }
  })
  
  # step to read in the raw data and rename the columns 
  raw_data <- reactive({
    infile <- input$file
    if(is.null(infile)){return()} 
    raw_data <- read.table(infile$datapath, skip=input$vskip, sep=input$sep,header=ifelse(input$header==TRUE,TRUE,FALSE)) 
    return(raw_data)})
  
  # this reactive output contains the data file name, path, and size
  output$filedf <-  DT::renderDataTable({
    if(is.null(raw_data())){return()}
    input$file
  })
  
  # this reactive output contains the summary of the dataset and display the summary as printed in R
  output$sum <- renderPrint({
    if(is.null(raw_data())){return()}
    summary(raw_data())  
  })
  
  # This reactive output contains the raw dataset and displays the dataset in table format
  output$table <-  DT::renderDataTable({
    if(is.null(raw_data())){return()}
    raw_data()
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$tb <- renderUI({
    if(is.null(raw_data())){return ()}
    else
      tabsetPanel(tabPanel("About file", DT::dataTableOutput("filedf")),tabPanel("Data", DT::dataTableOutput("table")),tabPanel("Summary", verbatimTextOutput("sum")))
  })
  
  processed <- reactive({
    #validate(need(is.null(reac$range_one_min) != TRUE,"Please click the regions tab"))
    
    raw <- raw_data()
    if(is.null(raw_data())){return()}
    
    # label and order the columns
    names(raw)[input$raw88] <- c("Raw88")
    names(raw)[input$raw87] <- c("Raw87")
    names(raw)[input$raw86] <- c("Raw86")
    names(raw)[input$raw85] <- c("Raw85")
    names(raw)[input$raw84] <- c("Raw84")
    names(raw)[input$raw83] <- c("Raw83")
    names(raw)[input$cyclesec] <- c("CycleSecs")
    raw <- raw[,c("Raw88", "Raw87","Raw86", "Raw85",  "Raw84", "Raw83", "CycleSecs")]
    
    # subset the background values as the first 30 seconds
    # and calculate average for each raw voltage
    background <- subset(raw,raw$CycleSecs<=30)
    
    # remove rows in background if Raw88 is above detection threshold
    Raw88LowerThresh <- input$Raw88LowerThresh # user defined
    background <- background[!(background$Raw88 > Raw88LowerThresh),]
    
    backgrounds <- apply(background[,c(1:6)],2, FUN=mean)
    
    # remove the background row data from the raw data and name as data
    nrow <- nrow(background)
    data <- raw[-1:-nrow,]
    
    # calculate the distance, start distance =0 at the first occurance within data
    # of Raw88 above threshold
    bkg_secs <- data[,7][data[,1] > Raw88LowerThresh][1]
    speed <- input$Speed # in micron per second, user defined
    data = within(data,{
      distance = ifelse(Raw88 > Raw88LowerThresh, CycleSecs - bkg_secs, NA) * speed
    })
    
    # remove NA to clean up data and remove the rows before distance starts
    data <- na.omit(data)
    
    # remove all columns in data if Raw88 below or above thresholds
    Raw88UpperThresh <- input$Raw88UpperThresh # user defined
    data <- data[!(data$Raw88 < Raw88LowerThresh | data$Raw88 > Raw88UpperThresh),]
    
    # subtract out background and save to new columns as net voltages
    n <- seq(1,6,by=1)
    for (i in n)
    {
      p <- data[,i]-backgrounds[i]
      data <- cbind(data,p)
    }
    names(data)[9:14] <- c("Sr88", "Net87", "Sr86",  "Rb85",	"Net84",	"Kr83")
    
    # Step 5: determine the mass bias
    data$Sr8688 <- data$Sr86/data$Sr88
    data$Sr8488 <- data$Net84/data$Sr88
    data$Sr8486 <- data$Net84/data$Sr86
    data$Rb85Sr88 <- data$Rb85/data$Sr88
    # .1194 is ratio of relative abundance of Sr 86 and Sr 88 
    data$MbFactor <- log10(0.1194/data$Sr8688)/log10(85.9092607/87.9056123) # produces Nans for some values at beginning and end
    
    # Step 6: Rubidium correction
    data$Rb87 <- (data$Rb85/2.59712)*(84.9117897/86.9091805)^data$MbFactor
    data$Sr87 <- data$Net87-data$Rb87
    data$Sr87Sr88 <- data$Sr87/data$Sr88
    data$Net87Sr86 <- data$Net87/data$Sr86
    
    # Step 7: Apply mass bias to 87Sr/86Sr
    data$Sr87Sr86 <- (data$Sr87/data$Sr86)*(86.9088775/85.9092607)^data$MbFactor
    
    # trim the data based on the trim amount and reset distance at zero
    #data$distance = ifelse(data$distance >= input$trimright | data$distance <= input$trimleft,NA,data$distance)
    trim_left_dist <- min(data[,8]) + input$trimleft # find the original trim min distance
    trim_right_dist <- max(data[,8]) - input$trimright # find the original max trim distance
    data = within(data,{distance=ifelse(distance <= trim_left_dist,NA,distance)})
    data = within(data,{distance=ifelse(distance >=trim_right_dist,NA,distance)})
    data$distance = data$distance - min(data$distance,na.rm=TRUE)
    data = na.omit(data) 
    
    # Step 7b: outlier rejection with a rolling method and assume a normal distribution
    means <- rollapply(data$Sr87Sr86,input$outlier_num, mean,fill="extend")
    sds <- rollapply(data$Sr87Sr86,input$outlier_num, sd,fill="extend")
    z_roll <- (data$Sr87Sr86-means)/sds
    CI <- as.numeric(input$CI) # user input confidence interval
    vals<-qnorm(CI,lower.tail=FALSE)*sds+means 
    val2s<-qnorm(CI)*sds+means
    
    # outlier plot
    Outlier <- as.factor(ifelse(data$Sr87Sr86<vals | data$Sr87Sr86>val2s,"yes","no"))
    fmt_dcimals <- function(decimals=0){ # function to format the y axis of the outlier plot to 5 decimals
      function(x) format(x,nsmall = decimals,scientific = FALSE)
    }
    p <- ggplot(data,aes(distance, Sr87Sr86)) +
      theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
      theme(legend.position="none") +
      labs(y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), 
           x=expression(paste("Distance (",mu,"m)"))) +
      scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +
      scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))+
      ggtitle("Outlier detection")+
      annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#6b8acd") +
      annotate("rect", xmin = reac$range_two_min, xmax = reac$range_two_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#4cb28d") +
      annotate("rect", xmin = reac$range_three_min, xmax = reac$range_three_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#72853c") +
      annotate("rect", xmin = reac$range_four_min, xmax = reac$range_four_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c78a40") +
      annotate("rect", xmin = reac$range_five_min, xmax = reac$range_five_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c75d9c") +
      annotate("rect", xmin = reac$range_six_min, xmax = reac$range_six_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#cb554f") +
      annotate("rect", xmin = reac$range_seven_min, xmax = reac$range_seven_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#9061c9") +
      annotate("rect", xmin = reac$range_eight_min, xmax = reac$range_eight_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#83b844") +
      geom_point(aes(color=Outlier),size=1.5) +
      scale_color_manual(values = c("black", "red"))
    
    # remove the outlier rows
    data <- data[!(data$Sr87Sr86 < vals | data$Sr87Sr86 > val2s),] # removes all rows with an outlier value
    
    # Step 7c: Apply a moving average to the remaining Sr87Sr86 data and fill in NAs created at the beginning and end
    # by the rollapply function with the nearby values
    data$Sr87Sr86 <- rollapply(data$Sr87Sr86,input$integration, mean, fill="extend", partial=TRUE)
    
    # calculate total Sr
    data$totalSr <- rowSums(data[,c(9,11,13,21)],na.rm=TRUE)
    

    
    
    data = within(data,{
      region = 
        ifelse(distance >= reac$range_one_min & distance <= reac$range_one_max, input$range_one_label,
               ifelse(distance >= reac$range_two_min & distance <= reac$range_two_max, input$range_two_label, 
                      ifelse(distance >= reac$range_three_min & distance <= reac$range_three_max, input$range_three_label,
                             ifelse(distance >= reac$range_four_min & distance <= reac$range_four_max, input$range_four_label,
                                    ifelse(distance >= reac$range_five_min & distance <= reac$range_five_max, input$range_five_label,
                                           ifelse(distance >= reac$range_six_min & distance <= reac$range_six_max, input$range_six_label,
                                                  ifelse(distance >= reac$range_seven_min & distance <= reac$range_seven_max, input$range_seven_label,
                                                         ifelse(distance >= reac$range_eight_min & distance <= reac$range_eight_max, input$range_eight_label,
                                           "none"))))))))
    })
    
    data$region <- as.factor(data$region)
    
    if(input$smoother=="MA"){
      # calculate the moving average if selected
      data$MA <- rollapply(data$Sr87Sr86,width=input$average_num, FUN= mean, partial=TRUE)
      sds <- rollapply(data$Sr87Sr86,input$average_num,FUN=sd,fill="extend") # standard deviation of the sample mean
      ses <- sds/sqrt(input$average_num) # standard error the of sample mean: http://www.biostathandbook.com/standarderror.html
      data$MAp2SE <- data$MA + 2*ses # for smoothed plotting on plot 1
      data$MAm2SE <- data$MA - 2*ses} else{
        
      # calculate loess fit and errors if selected
      loess.fit <- loess(Sr87Sr86~distance,data,span=input$span)
      preds <- predict(loess.fit,data,se=TRUE)
      data$Loess <- preds$fit # column 27
      data$Loessp2SE <- preds$fit + 2*preds$se # for smoothed plotting on plot 1, column 28
      data$Loessm2SE <- preds$fit - 2*preds$se} # column 29
    
    data[,9] = round(data[,9],digits=1) # round Sr88 to 1 decimal place
    data[,c(10:25,27:29)] = round(data[,c(10:25,27:29)],digits=5) # round other calculated values and factors to 5 decimal places
    data[,c(8)] = round(data[,c(8)],digits=0) # rounds distance to whole numbers

    # code to summarize data by region label, attach the sample ID, and save as a separate object
    means <- aggregate(.~region,data=data,mean)
    means <- means[,c(-28,-29)] # remove the p2SE and m2SE columns
    colnames(means)[2:27] <- paste("Mean", colnames(means)[2:27], sep = "_")
    
    if(input$smoother=="MA"){
      means$Smooth_Min <- aggregate(MA~region,data=data,min)[,2]
      means$Smooth_Max <- aggregate(MA~region,data=data,max)[,2]
      smoothed_sds <- aggregate(MA~region,data=data,sd)
      smoothed_cts <- aggregate(MA~region,data=data,FUN = function(x){NROW(x)})
      smoothed_ses <- smoothed_sds[,2]/sqrt(smoothed_cts[,2])
      means$Smooth_2SD <- smoothed_sds[,2]*2
      means$Smooth_2SE <- smoothed_ses*2
      means$Smooth_Variance <- aggregate(MA~region,data=data,var)[,2]} else{
      means$Smooth_Min <- aggregate(Loess~region,data=data,min)[,2]
      means$Smooth_Max <- aggregate(Loess~region,data=data,max)[,2]
      smoothed_sds <- aggregate(Loess~region,data=data,sd)
      smoothed_cts <- aggregate(Loess~region,data=data,FUN = function(x){NROW(x)})
      smoothed_ses <- smoothed_sds[,2]/sqrt(smoothed_cts[,2])
      means$Smooth_2SD <- smoothed_sds[,2]*2
      means$Smooth_2SE <- smoothed_ses*2
      means$Smooth_Variance <- aggregate(Loess~region,data=data,var)[,2]}
    
    means$RunFile_ID <- unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1] # column 33
    means$ShinyDate <- Sys.Date() # column 34
    # reorder the columns so Sample ID and Shiny Date are column 1 and 2
    means <- means[,c(33,34,1:32)]
    
    # calculate the standard errors based on region data and not the smoothed data
    region_sds <- aggregate(Sr87Sr86~region,data=data,sd)
    region_cts <- aggregate(Sr87Sr86~region,data=data,FUN = function(x){NROW(x)})
    region_ses <- region_sds[,2]/sqrt(region_cts[,2])
    means$Region_2SD <- region_sds[,2]*2 # column 35
    means$Region_2SE <- region_ses*2 # column 36
    means$Region_Min <- aggregate(Sr87Sr86~region,data=data,min)[,2] # column 37
    means$Region_Max <- aggregate(Sr87Sr86~region,data=data,max)[,2] # column 38
    means$Region_Variance <- aggregate(Sr87Sr86~region,data=data,var)[,2] # column 39
    
    
    # assign user input range values to columns based on the range label
    means = within(means,{
      MinRange = 
        ifelse(reac$range_one_min & reac$range_one_max > -1 & region==input$range_one_label, reac$range_one_min,
               ifelse(reac$range_two_min & reac$range_two_max > -1 & region==input$range_two_label, reac$range_two_min,
                      ifelse(reac$range_three_min & reac$range_three_max > -1 & region==input$range_three_label, reac$range_three_min,
                             ifelse(reac$range_four_min & reac$range_four_max > -1 & region==input$range_four_label, reac$range_four_min,
                                    ifelse(reac$range_five_min & reac$range_five_max > -1 & region==input$range_five_label, reac$range_five_min,
                                           ifelse(reac$range_six_min & reac$range_six_max > -1 & region==input$range_six_label, reac$range_six_min,
                                                  ifelse(reac$range_seven_min & reac$range_seven_max > -1 & region==input$range_seven_label, reac$range_seven_min,
                                                         ifelse(reac$range_eight_min & reac$range_eight_max > -1 & region==input$range_eight_label, reac$range_eight_min,
                                           "NA"))))))))
    })
    
    means = within(means,{
      MaxRange = 
        ifelse(reac$range_one_min & reac$range_one_max > -1 & region==input$range_one_label, reac$range_one_max,
               ifelse(reac$range_two_min & reac$range_two_max > -1 & region==input$range_two_label, reac$range_two_max,
                      ifelse(reac$range_three_min & reac$range_three_max > -1 & region==input$range_three_label, reac$range_three_max,
                             ifelse(reac$range_four_min & reac$range_four_max > -1 & region==input$range_four_label, reac$range_four_max,
                                    ifelse(reac$range_five_min & reac$range_five_max > -1 & region==input$range_five_label, reac$range_five_max,
                                           ifelse(reac$range_six_min & reac$range_six_max > -1 & region==input$range_six_label, reac$range_six_max,
                                                  ifelse(reac$range_seven_min & reac$range_seven_max > -1 & region==input$range_seven_label, reac$range_seven_max,
                                                         ifelse(reac$range_eight_min & reac$range_eight_max > -1 & region==input$range_eight_label, reac$range_eight_max,
                                           "NA"))))))))
    })
    
    # assign other user inputs to columns (this could also be done below when the data is saved as with the comment and trim fields)
    means$RunSpeed <- input$Speed
    means$Raw88LowerThresh <- input$Raw88LowerThresh
    means$Raw88UpperThresh <- input$Raw88UpperThresh
    if(input$smoother=="MA"){
      means$Smoother<- "Moving average"} else{
        means$Smoother <- "Loess"}
    if(input$smoother=="MA"){
      means$Smooth_parameter <- input$average_num
    }
    if(input$smoother=="loess"){
      means$Smooth_parameter <- input$span
    }
    
    # round appropriately
    means[,12] = round(means[,12],digits=1) # round Sr88 to 1 decimal place
    means[,c(13:33,35:38)] = round(means[,c(13:33,35:38)],digits=5) # round other calculated values, MB factors, and smoothed values to 5 decimal places
    means[,c(34)] = round(means[,c(34)],digits=10) # round region variance to 7 decimal places
    means[,c(39)] = round(means[,c(39)],digits=10) # round region variance to 7 decimal places
    means[,c(11)] = round(means[,c(11)],digits=0) # round distance to whole numbers
    means[,c(10)] = round(means[,c(10)],digits=1) # round cycle secs to one decimal

    # save data to a list for use later
    processed <- list(processed_data=data,background=background,region_means=means,outlier_plot=p)
    
    return(processed)
  })
  
  output$outlier_plot <- renderPlot({
    if(is.null(processed())){return()}
    processed <- processed()
    outlier_plot <- processed$outlier_plot
    plot(outlier_plot)
  })
  
  

  
  
  # action to append new sample processed region means to csv file
  warnmsg <- eventReactive(input$appendnew, {
    processed <- processed()
    region_means <- processed$region_means
    region_means$Sample_ID <- input$SampleID
    region_means$User_initials <- input$Userinitials
    region_means$Sample_Type <- input$sampletype
    region_means$Comment <- input$profile.comment
    region_means$TrimLeft <- input$trimleft
    region_means$TrimRight <- input$trimright
    region_means$Fluency <- input$fluency
    region_means$SpotSize <- input$spotsize
    region_means$LaserEnergy <- input$energy
    region_means$IntegrationTime <- input$integration
    region_means$OutlierNum <- input$outlier_num
    region_means$CIoutliers <- input$CI

    fullprofile <-processed$processed_data
    
    region_means$Length <- max(fullprofile$distance)
    colnames(region_means)[59] <- c("Length (um)")
    region_means$Flag <- ifelse(input$flag==TRUE,"Yes","No")
    region_means <- region_means[c("RunFile_ID",	"Sample_ID",	"User_initials",	"Sample_Type",	"ShinyDate",	"Comment","Flag",	
                                  "region",	"Mean_Raw88",	"Mean_Raw87",	"Mean_Raw86",	"Mean_Raw85",	"Mean_Raw84",	
                                  "Mean_Raw83",	"Mean_CycleSecs",	"Mean_distance",	"Mean_Sr88",	"Mean_Net87",	
                                  "Mean_Sr86",	"Mean_Rb85",	"Mean_Net84",	"Mean_Kr83",	"Mean_Sr8688",	"Mean_Sr8488",	
                                  "Mean_Sr8486",	"Mean_Rb85Sr88",	"Mean_MbFactor",	"Mean_Rb87",	"Mean_Sr87",	"Mean_Sr87Sr88",	
                                  "Mean_Net87Sr86",	"Mean_Sr87Sr86",	"Mean_totalSr",	"Mean_MA",	"Smooth_Min",	"Smooth_Max",	
                                  "Smooth_2SD",	"Smooth_2SE",	"Smooth_Variance",	"Region_2SD",	"Region_2SE",	"Region_Min",	
                                  "Region_Max",	"Region_Variance",	"MinRange",	"MaxRange",	"RunSpeed",	"Raw88LowerThresh",	
                                  "Raw88UpperThresh",	"Smoother",	"Smooth_parameter",	"TrimLeft",	"TrimRight",	"Fluency",	
                                  "SpotSize",	"LaserEnergy",	"IntegrationTime",	"OutlierNum",	"CIoutliers",	"Length (um)")]
    
  
    fullprofile$RunSpeed <- input$Speed
    fullprofile$Raw88LowerThresh <- input$Raw88LowerThresh
    fullprofile$Raw88UpperThresh <- input$Raw88UpperThresh
    if(input$smoother=="MA"){
      fullprofile$Smoother<- "Moving average"} else{
        fullprofile$Smoother <- "Loess"}
    if(input$smoother=="MA"){
      fullprofile$Smooth_parameter <- input$average_num
    }
    if(input$smoother=="loess"){
      fullprofile$Smooth_parameter <- input$span
    }
    fullprofile$RunFile_ID <-  unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1]
    fullprofile$TrimLeft <- input$trimleft
    fullprofile$TrimRight <- input$trimright
    fullprofile$Fluency <- input$fluency
    fullprofile$SpotSize <- input$spotsize
    fullprofile$LaserEnergy <- input$energy
    fullprofile$IntegrationTime <- input$integration
    fullprofile$OutlierNum <- input$outlier_num
    fullprofile$CIoutliers <- input$CI
    fullprofile$Comment <- input$profile.comment
    fullprofile$Sample_ID <- input$SampleID
    fullprofile$User_initials <- input$Userinitials
    fullprofile$Sample_Type <- input$sampletype
    fullprofile$Length <- max(fullprofile$distance)
    colnames(fullprofile)[48] <- c("Length (um)")
    fullprofile$ShinyDate <- Sys.Date()
    fullprofile$Flag <- ifelse(input$flag==TRUE,"Yes","No")
    fullprofile <- fullprofile[c("RunFile_ID","Sample_ID",	"User_initials",	"Sample_Type",	"ShinyDate",	"Comment","Flag",	
                                 "region",	"Raw88",	"Raw87",	"Raw86",	"Raw85",	"Raw84",	"Raw83",	"CycleSecs",	"distance",	
                                 "Sr88",	"Net87",	"Sr86",	"Rb85",	"Net84",	"Kr83",	"Sr8688",	"Sr8488",	"Sr8486",	"Rb85Sr88",	
                                 "MbFactor",	"Rb87",	"Sr87",	"Sr87Sr88",	"Net87Sr86",	"Sr87Sr86",	"totalSr",	"MA",	"MAp2SE",	
                                 "MAm2SE",	"RunSpeed",	"Raw88LowerThresh",	"Raw88UpperThresh",	"Smoother",	
                                 "Smooth_parameter",	"TrimLeft",	"TrimRight",	"Fluency",	"SpotSize",	"LaserEnergy",	
                                 "IntegrationTime",	"OutlierNum",	"CIoutliers",	"Length (um)")]
    
    
    outlier_plot <- processed$outlier_plot
    
    # append region means to a new or existing file
    t <- try(
     if(file.exists(file.path("Projects",input$project.name,paste0(input$project.name,"_summary.csv")))==FALSE){
       write.table(region_means,file=file.path("Projects",input$project.name,paste0(input$project.name,"_summary.csv")),col.names = TRUE,row.names=FALSE,sep=",")
       write.table(fullprofile, file =file.path("Projects",input$project.name,"Data",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],".csv")), col.names = TRUE, row.names = FALSE, sep = ",")
       ggsave(filename=file.path("Projects",input$project.name,"Plots/87Sr86Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_87Sr86Sr.pdf")), plot = plot1(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/88Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_88Sr_volts.pdf")), plot = plot2(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/85Rb88Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_85Rb88Sr.pdf")), plot = plot3(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/83Kr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_83Kr_volts.pdf")), plot = plot4(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/Outlier",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_outliers.pdf")), plot = outlier_plot, device = "pdf",width=8,height=4,scale=1.5)
       
       }else{
       write.table(region_means,file=file.path("Projects",input$project.name,paste0(input$project.name,"_summary.csv")),append=TRUE,col.names = FALSE,row.names=FALSE,sep=",")
       write.table(fullprofile, file =file.path("Projects",input$project.name,"Data",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],".csv")), col.names = TRUE, row.names = FALSE, sep = ",")
       ggsave(filename=file.path("Projects",input$project.name,"Plots/87Sr86Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_87Sr86Sr.pdf")), plot = plot1(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/88Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_88Sr_volts.pdf")), plot = plot2(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/85Rb88Sr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_85Rb88Sr.pdf")), plot = plot3(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/83Kr",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_83Kr_volts.pdf")), plot = plot4(), device = "pdf",width=8,height=4,scale=1.5)
       ggsave(filename=file.path("Projects",input$project.name,"Plots/Outlier",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_outliers.pdf")), plot = outlier_plot, device = "pdf",width=8,height=4,scale=1.5) 
        },
     silent=TRUE)     
      # if there is no error print a success message otherwise print an error message
      if ("try-error" %in% class(t)){
        print("Error: the file is open, please close the file")
      } else {
        print("Data saved")
      }
    #})    
  })

output$warnmsg <- renderPrint({warnmsg()})

# reset the warning message upon new file upload
observe({
        input$file
        warnmsg <- NA
        output$warnmsg <- renderPrint({warnmsg()})
        })
  
  overwritewarn <- reactive({
    if(is.null(processed())){print("No data has been loaded")
      } else { 
    if(file.exists(file.path("Projects",input$project.name,"Data",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],".csv")))==TRUE){
     # print("WARNING: Profile exists. Save will overwrite.")
      cat("WARNING: Profile",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],".csv"),"already exists","\n")
    } else {
      #print("Profile has not been previously saved")
      cat("Profile",paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],".csv"),"has not yet been saved","\n")
    }
      }
  })
  
output$overwritewarn <- renderPrint({overwritewarn()})
  
  # reset the overwrite warning message upon new file upload
  observe({
    input$appendnew
    overwritewarn <- NA
    output$overwritewarn <- renderPrint({overwritewarn()})
  })

  # action to read brushed values into region1
  observeEvent(input$readregion1, {
    updateNumericInput(session,  "range_one_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_one_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion2, {
    updateNumericInput(session,  "range_two_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_two_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion3, {
    updateNumericInput(session,  "range_three_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_three_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion4, {
    updateNumericInput(session,  "range_four_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_four_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion5, {
    updateNumericInput(session,  "range_five_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_five_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion6, {
    updateNumericInput(session,  "range_six_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_six_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion7, {
    updateNumericInput(session,  "range_seven_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_seven_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  observeEvent(input$readregion8, {
    updateNumericInput(session,  "range_eight_min", label = NULL, value = input$plot_brush$xmin,
                       min = NULL, max = NULL, step = NULL)
    updateNumericInput(session,  "range_eight_max", label = NULL, value = input$plot_brush$xmax,
                       min = NULL, max = NULL, step = NULL)
  })
  
  plot1 <- function(){
    if(is.null(processed())){return()}
    processed <- processed()
    processed_data <- processed$processed_data
    dat <- processed_data
    # create a group variable for non-continuous data analysis (3- sections example)
    diff <- diff(dat$distance,lag=1,differences=1)
    gaps <- which(diff>15) # will break at any gap greater than 15um
    breaks_dist <- dat$distance[gaps]
    breaks <- c(-1,breaks_dist,max(dat$distance))
    dat$group <- cut(dat$distance,breaks=breaks)
    
    fmt_dcimals <- function(decimals=0){
      function(x) format(x,nsmall = decimals,scientific = FALSE)
    }
    p <- ggplot(dat) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))+ labs(title=paste0("Runfile:"," ",unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1]," ","Sample:"," ",input$SampleID),
                                         y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), x=expression(paste("Distance (",mu,"m)"))) +
      
      annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#6b8acd") +
      annotate("rect", xmin = reac$range_two_min, xmax = reac$range_two_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#4cb28d") +
      annotate("rect", xmin = reac$range_three_min, xmax = reac$range_three_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#72853c") +
      annotate("rect", xmin = reac$range_four_min, xmax = reac$range_four_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c78a40") +
      annotate("rect", xmin = reac$range_five_min, xmax = reac$range_five_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c75d9c") +
      annotate("rect", xmin = reac$range_six_min, xmax = reac$range_six_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#cb554f") +
      annotate("rect", xmin = reac$range_seven_min, xmax = reac$range_seven_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#9061c9") +
      annotate("rect", xmin = reac$range_eight_min, xmax = reac$range_eight_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#83b844") +
      scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) + 
      scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10)) 
    
    if(input$smoother=="MA"){
      p <- p +
        geom_ribbon(aes(x=distance,ymin=MAm2SE,ymax=MAp2SE,group=group),fill=input$shadecol) +
        geom_line(aes(distance,MA,group=group), size=0.5, colour = input$linecol) +
        geom_line(aes(distance,MAp2SE,group=group),linetype=2, size=0.5) +
        geom_line(aes(distance,MAm2SE,group=group),linetype=2, size =0.5)} 
    else{
          p <- p +
            geom_ribbon(aes(x=distance,ymin=Loessm2SE,ymax=Loessp2SE,group=group),fill=input$shadecol) +
            geom_line(aes(distance,Loess,group=group), size=0.5, colour = input$linecol) +
            geom_line(aes(distance,Loessp2SE,group=group),linetype=2, size=0.5) +
            geom_line(aes(distance,Loessm2SE,group=group),linetype=2, size =0.5)}
    
    if(input$points){
      p <- p + geom_point(aes(distance, Sr87Sr86),size=1.5)}
    
    if(input$ocean){
      p <- p + geom_hline(yintercept=0.70918, color="blue")}
    
    if(input$changepoints){
      change <- cpt.mean(dat$Sr87Sr86, penalty = "Manual",  pen.value = input$manual_pen, method = input$change_method, test.stat = "Normal", class = TRUE, param.estimates = TRUE)
      changepts <-change@cpts
      cptsmean<-change@param.est$mean
      p <- p + geom_vline(xintercept=dat$distance[changepts], color="red", linetype="dotted", size=1)
    }
    
    return(p)}
  

  
  output$plot1 <- renderPlot({plot1()},height=400)
  
  plot2 <-  function(){
    if(is.null(processed())){return()}
    processed <- processed()
    processed_data <- processed$processed_data
    dat <- processed_data[,c("distance","Raw88")]
    
    fmt_dcimals <- function(decimals=0){
      function(x) format(x,nsmall = decimals,scientific = FALSE)
    }
    
    p <- ggplot(dat) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ labs(
      y= expression(paste(""^"88"*"Sr (V)")), x=expression(paste("Distance (",mu,"m)"))) +
      annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#6b8acd") +
      annotate("rect", xmin = reac$range_two_min, xmax = reac$range_two_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#4cb28d") +
      annotate("rect", xmin = reac$range_three_min, xmax = reac$range_three_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#72853c") +
      annotate("rect", xmin = reac$range_four_min, xmax = reac$range_four_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c78a40") +
      annotate("rect", xmin = reac$range_five_min, xmax = reac$range_five_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c75d9c") +
      annotate("rect", xmin = reac$range_six_min, xmax = reac$range_six_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#cb554f") +
      annotate("rect", xmin = reac$range_seven_min, xmax = reac$range_seven_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#9061c9") +
      annotate("rect", xmin = reac$range_eight_min, xmax = reac$range_eight_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#83b844") +
      geom_point(aes(distance,Raw88),size=1, color="red") +
      scale_y_continuous(labels = fmt_dcimals(2), breaks = scales::pretty_breaks(n = 10)) + 
      scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))+
      ggtitle(expression(paste(""^"88"*"Sr (V)")))
    return(p)}
  
  output$plot2 <- renderPlot({plot2()})
  
  plot3 <- function(){
    if(is.null(processed())){return()}
    processed <- processed()
    processed_data <- processed$processed_data
    dat <- processed_data[,c("distance","Rb85Sr88")]
    
    fmt_dcimals <- function(decimals=0){
      function(x) format(x,nsmall = decimals,scientific = FALSE)
    }
    
    p <- ggplot(dat) + theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ labs(
      y= expression(paste(""^"85"*"Rb/"^"88"*"Sr")), x=expression(paste("Distance (",mu,"m)"))) +
      annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#6b8acd") +
      annotate("rect", xmin = reac$range_two_min, xmax = reac$range_two_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#4cb28d") +
      annotate("rect", xmin = reac$range_three_min, xmax = reac$range_three_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#72853c") +
      annotate("rect", xmin = reac$range_four_min, xmax = reac$range_four_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c78a40") +
      annotate("rect", xmin = reac$range_five_min, xmax = reac$range_five_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c75d9c") +
      annotate("rect", xmin = reac$range_six_min, xmax = reac$range_six_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#cb554f") +
      annotate("rect", xmin = reac$range_seven_min, xmax = reac$range_seven_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#9061c9") +
      annotate("rect", xmin = reac$range_eight_min, xmax = reac$range_eight_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#83b844") +
      geom_point(aes(distance,Rb85Sr88),size=1, color="orange") +
      scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) + 
      scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))+
      ggtitle(expression(paste(""^"85"*"Rb/"^"88"*"Sr ratio")))
    return(p)}
  
  output$plot3 <- renderPlot({plot3()})
  
  plot4 <- function(){
    if(is.null(processed())){return()}
    processed <- processed()
    processed_data <- processed$processed_data
    dat <- processed_data[,c("distance","Kr83")]
    
    fmt_dcimals <- function(decimals=0){
      function(x) format(x,nsmall = decimals,scientific = FALSE)
    }
    
    p <- ggplot(dat) + theme_bw() +theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) + labs(
      y= expression(paste(""^"83"*"Kr (V)")), x=expression(paste("Distance (",mu,"m)"))) +
      annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#6b8acd") +
      annotate("rect", xmin = reac$range_two_min, xmax = reac$range_two_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#4cb28d") +
      annotate("rect", xmin = reac$range_three_min, xmax = reac$range_three_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#72853c") +
      annotate("rect", xmin = reac$range_four_min, xmax = reac$range_four_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c78a40") +
      annotate("rect", xmin = reac$range_five_min, xmax = reac$range_five_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#c75d9c") +
      annotate("rect", xmin = reac$range_six_min, xmax = reac$range_six_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#cb554f") +
      annotate("rect", xmin = reac$range_seven_min, xmax = reac$range_seven_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#9061c9") +
      annotate("rect", xmin = reac$range_eight_min, xmax = reac$range_eight_max, ymin = -Inf, ymax = +Inf,
               alpha = .8, fill="#83b844") +
      geom_point(aes(distance,Kr83),size=1, color="lightgreen") +
      scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) + 
      scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))+
      ggtitle(expression(paste("Background "^"83"*"Kr (V)")))
    return(p)}
  
  output$plot4 <- renderPlot({plot4()})
  
  # Create the brush output for plot 1
  output$brush_info <- renderPrint({
    str(input$plot_brush$xmin)
    str(input$plot_brush$xmax)
  })
  
  
  ### prepare objects for display of processed data tables
  # this reactive output contains the summary of the processed dataset and display the summary in table format
  output$sum_processed <- renderPrint({
    processed <- processed()
    processed_data <- processed$processed_data
    if(is.null(processed_data)){return()}
    summary(processed_data)  
  })
  
  # This reactive output contains the processed dataset and display the dataset in table format
  output$processed_table <-  DT::renderDataTable({
    processed <- processed()
    processed_data <- processed$processed_data
    if(is.null(processed_data)){return()}
    DT::datatable(processed_data, options = list(pageLength = 20))
  })
  
  # This reactive output contains the region means and display the dataset in table format
  output$region_means <-  DT::renderDataTable({
    processed <- processed()
    region_means <- processed$region_means
    if(is.null(region_means)){return()}
    DT::datatable(region_means)
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$processed <- renderUI({
    processed <- processed()
    processed_data <- processed$processed_data
    if(is.null(processed_data)){return()}
    else
      tabsetPanel(tabPanel("Processed data", div(style = 'overflow-x: scroll',DT::dataTableOutput("processed_table"))),
                  tabPanel("Processed stats", verbatimTextOutput("sum_processed")),
                  tabPanel("Summary data",div(style = 'overflow-x: scroll',DT::dataTableOutput("region_means"))))
  }) 
  
  
  ### prepare objects for display of background data tables
  # this reactive output contains the summary of the background data and display the summary in table format
  output$sum_background <- renderPrint({
    processed <- processed()
    background <- processed$background
    if(is.null(background)){return()}
    summary(background)  
  })
  
  # This reactive output contains the dataset and display the background in table format
  output$background_table <-  DT::renderDataTable({
    processed <- processed()
    background <- processed$background
    if(is.null(background)){return()}
    DT::datatable(background, options = list(pageLength = 20))
  })
  
  # the following renderUI is used to dynamically generate the tabsets when the file is loaded. Until the file is loaded, app will not show the tabset.
  output$background <- renderUI({
    processed <- processed()
    background <- processed$background
    if(is.null(background)){return()}
    else
      tabsetPanel(tabPanel("Background Data", DT::dataTableOutput("background_table"),
                           helpText("First 30 sec extracted as background values")),
                  tabPanel("Summary", verbatimTextOutput("sum_background"),
                           helpText("Mean raw background voltage for each subtracted from 
                                    respective remaining data")))
  }) 
  
   #create the download for plot 1
   output$downloadplot1 <- downloadHandler(
     filename = function () {paste0(unlist(strsplit(as.character(input$file), split='.', fixed=TRUE))[1],"_87Sr86Sr.pdf")},
     content = function(file) {
       ggsave(file, plot = plot1(), device = "pdf",width=8,height=4,scale=1.5)
     }
   )
  
  # values to reset upon new file upload   
  observe({
    input$file
    updateNumericInput(session,"trimleft",value=-1)
    updateNumericInput(session,"trimright",value=-1)
    updateTextInput(session,"profile.comment",value="")
    updateTextInput(session,"SampleID",value="")
  }) 
  
  # ranges to reset upon new file upload or ranges reset button
  observe({
    input$file
    input$reset_ranges
    updateTextInput(session,"range_one_label", label=NULL, value = "Core")
    updateNumericInput(session,"range_one_min","",value=-1)
    updateNumericInput(session,"range_one_max","",value=-1)
    updateTextInput(session,"range_two_label", label=NULL, value = "Natal")
    updateNumericInput(session,"range_two_min","",value=-1)
    updateNumericInput(session,"range_two_max","",value=-1)
    updateTextInput(session,"range_three_label",label=NULL, value = "Adult")
    updateNumericInput(session,"range_three_min","",value=-1)
    updateNumericInput(session,"range_three_max","",value=-1)
    updateTextInput(session,"range_four_label",label=NULL, value = "Range 4")
    updateNumericInput(session,"range_four_min","",value=-1)
    updateNumericInput(session,"range_four_max","",value=-1)
    updateTextInput(session,"range_five_label", label=NULL, value = "Range 5")
    updateNumericInput(session,"range_five_min","",value=-1)
    updateNumericInput(session,"range_five_max","",value=-1)
    updateTextInput(session,"range_six_label", label=NULL, value = "Range 6")
    updateNumericInput(session,"range_six_min","",value=-1)
    updateNumericInput(session,"range_six_max","",value=-1)
    updateTextInput(session,"range_seven_label", label=NULL, value = "Range 7")
    updateNumericInput(session,"range_seven_min","",value=-1)
    updateNumericInput(session,"range_seven_max","",value=-1)
    updateTextInput(session,"range_eight_label", label=NULL, value = "Range 8")
    updateNumericInput(session,"range_eight_min","",value=-1)
    updateNumericInput(session,"range_eight_max","",value=-1)
  })
  
  output$inputVals <- DT::renderDataTable({
    inputs <- rbind(input$Raw88LowerThresh,input$Raw88UpperThresh,
                    input$average_num,input$span,input$outlier_num,
                    input$CI,input$Speed)
    rownames(inputs) <- c("Raw88 Lower Limit","Raw88 Upper Limit", 
                          "Moving Average Num", "Loess Span","Outlier Num",
                          "CI for Outliers","Run speed")
    colnames(inputs) <- c("Value")
    DT::datatable(inputs,options=list(dom="t"))
  })
  
  # code to read a settings file from a selected project create a settings object (function)
  # the object will update if a project setting is updaed with the Update button (input$updatesettings)
  readSettings <- reactive({
      input$updatesettings
      settingsdf <- read.csv(file.path("Projects",input$project.name,paste0(input$project.name,"_settings.csv")),header=TRUE)
  })
  # take the settings object and set the settings to the defaults in the csv with each new file upload
   observe({
       input$file # updates the settings to the default project settings with a new file upload as well
       settingsdf <- readSettings()
       updateNumericInput(session, "raw88", value = settingsdf$raw88)
       updateNumericInput(session, "raw87", value = settingsdf$raw87)
       updateNumericInput(session, "raw86", value=settingsdf$raw86)
       updateNumericInput(session, "raw85", value=settingsdf$raw85)
       updateNumericInput(session, "raw84", value=settingsdf$raw84)
       updateNumericInput(session, "raw83", value=settingsdf$raw83)
       updateNumericInput(session, "cyclesec", value=settingsdf$cyclesec)
       updateNumericInput(session, "vskip", value=settingsdf$vskip)
       updateCheckboxInput(session,"header",value=settingsdf$header)
       ifelse(is.na(settingsdf$sep),updateRadioButtons(session,"sep",selected="\t"),updateRadioButtons(session,"sep",selected=settingsdf$sep))
       updateRadioButtons(session,"smoother",selected=settingsdf$smoother)
       updateNumericInput(session,"Raw88LowerThresh",value=settingsdf$Raw88LowerThresh)
       updateNumericInput(session,"Raw88UpperThresh",value=settingsdf$Raw88UpperThresh)
       updateNumericInput(session,"average_num",value=settingsdf$average_num)
       updateNumericInput(session,"span",value=settingsdf$span)
       updateNumericInput(session,"outlier_num",value=settingsdf$outlier_num)
       updateRadioButtons(session,"CI",selected=settingsdf$CI)
       updateNumericInput(session,"Speed",value=settingsdf$RunSpeed)
       updateTextInput(session,"range_one_label",value=settingsdf$RangeOneLabel)
       updateTextInput(session,"range_two_label",value=settingsdf$RangeTwoLabel)
       updateTextInput(session,"range_three_label",value=settingsdf$RangeThreeLabel)
       updateTextInput(session,"range_four_label",value=settingsdf$RangeFourLabel)
       updateTextInput(session,"range_five_label",value=settingsdf$RangeFiveLabel)
       updateTextInput(session,"range_six_label",value=settingsdf$RangeSixLabel)
       updateTextInput(session,"range_seven_label",value=settingsdf$RangeSevenLabel)
       updateTextInput(session,"range_eight_label",value=settingsdf$RangeEightLabel)
       updateNumericInput(session,"fluency",value=settingsdf$Fluency)
       updateNumericInput(session,"spotsize",value=settingsdf$SpotSize)
       updateNumericInput(session,"energy",value=settingsdf$Energy)
       updateNumericInput(session,"integration",value=settingsdf$Integration)
       updateTextInput(session,"sampletype",value=settingsdf$SampleType)
   })
  
 
  # create a new project and create the files and subdirectories 
  observeEvent(input$save,{
    settings <- cbind(input$raw88,input$raw87,input$raw86,input$raw85,input$raw84,
                                             input$raw83,input$cyclesec,input$vskip,input$header,input$sep,
                                             input$smoother,input$Raw88LowerThresh,input$Raw88UpperThresh,
                                             input$average_num,input$span,input$outlier_num,
                                             input$CI,input$Speed,input$fluency,input$spotsize, input$energy, input$integration, 
                                             input$sampletype,input$range_one_label,input$range_two_label,
                                             input$range_three_label,input$range_four_label,input$range_five_label,
                                             input$range_six_label,input$range_seven_label,input$range_eight_label)
    colnames(settings) <- c("raw88","raw87","raw86","raw85","raw84","raw83",
                          "cyclesec","vskip","header","sep","smoother",
                          "Raw88LowerThresh", "Raw88UpperThresh", "average_num","span",
                          "outlier_num","CI","RunSpeed","Fluency","SpotSize","Energy","Integration", "SampleType",
                          "RangeOneLabel","RangeTwoLabel","RangeThreelabel","RangeFourLabel",
                          "RangeFiveLabel","RangeSixLabel","RangeSevenLabel","RangeEightLabel")
    if(dir.exists(paste0("Projects/",input$new.project))==FALSE){
    dir.create(paste0("Projects/",input$new.project))
    dir.create(paste0("Projects/",input$new.project,"/Runfiles"))
    dir.create(paste0("Projects/",input$new.project,"/Plots"))
    dir.create(paste0("Projects/",input$new.project,"/Plots/87Sr86Sr"))
    dir.create(paste0("Projects/",input$new.project,"/Plots/88Sr"))
    dir.create(paste0("Projects/",input$new.project,"/Plots/83Kr"))
    dir.create(paste0("Projects/",input$new.project,"/Plots/85Rb88Sr"))
    dir.create(paste0("Projects/",input$new.project,"/Plots/Outlier"))
    dir.create(paste0("Projects/",input$new.project,"/Data"))
    write.table(settings,file.path("Projects",input$new.project,paste0(input$new.project,"_settings.csv")),row.names=FALSE,col.names=TRUE,sep=",")}
    })
  
  # update the selected project to the newly created project upon creation (this is turn will read in the new project settings and update them)  
  observeEvent(input$save,{
    updateSelectInput(session,inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected=input$new.project)
  })
  
  # button to allow user to update the project settings. Will overwrite the current settings csv file in a project folder.
  observeEvent(input$updatesettings,{
    settings <- cbind(input$raw88,input$raw87,input$raw86,input$raw85,input$raw84,
                      input$raw83,input$cyclesec,input$vskip,input$header,input$sep,
                      input$smoother,input$Raw88LowerThresh,input$Raw88UpperThresh,
                      input$average_num,input$span,input$outlier_num,
                      input$CI,input$Speed,input$fluency,input$spotsize, input$energy, input$integration, 
                      input$sampletype,input$range_one_label,input$range_two_label,
                      input$range_three_label,input$range_four_label,input$range_five_label,
                      input$range_six_label,input$range_seven_label,input$range_eight_label)
    colnames(settings) <- c("raw88","raw87","raw86","raw85","raw84","raw83",
                            "cyclesec","vskip","header","sep","smoother",
                            "Raw88LowerThresh", "Raw88UpperThresh", "average_num","span",
                            "outlier_num","CI","RunSpeed","Fluency","SpotSize","Energy","Integration", "SampleType",
                            "RangeOneLabel","RangeTwoLabel","RangeThreelabel","RangeFourLabel",
                            "RangeFiveLabel","RangeSixLabel","RangeSevenLabel","RangeEightLabel")
    write.table(settings,file.path("Projects",input$project.name,paste0(input$project.name,"_settings.csv")),row.names=FALSE,col.names=TRUE,sep=",")
  })
  
  # project wide comments input and display
  observeEvent(input$comment,{
    date <- Sys.Date()
    comment <- input$project.comment
    comments <- cbind.data.frame(date,comment)
    if(file.exists(file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")))==FALSE){
      write.table(comments,file=file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),col.names = c("Date","Comment"),row.names=FALSE,sep=",")
    }else{
      write.table(comments,file=file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),append=TRUE,col.names = FALSE,row.names=FALSE,sep=",") 
    }
  })
  

   output$comments <-  DT::renderDataTable({
     input$comment
     t <- try(comments <- read.csv(file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),header=TRUE),silent=TRUE)
     if ("try-error" %in% class(t)){
       return()
     } else {
       DT::datatable(comments, options = list(pageLength = 20,searching=FALSE))
     }
   })

   
   # Aggregate data for a project
   observeEvent(input$aggregatedata, {
     agdata_path <-paste0("Projects/",input$project.name,"/Data")
     agfiles <-dir(agdata_path, pattern = "*.csv")
     data.aggregate <- agfiles %>%
       map(~ read_csv(file.path(agdata_path, .))) %>% 
       reduce(rbind)
     write.table(data.aggregate,file=file.path("Projects",input$project.name,paste0(input$project.name,"_data_aggregate.csv")), row.names=FALSE,sep=",")
   })

  # Stop shiny app when closing the browser
  session$onSessionEnded(stopApp)
  })

shinyApp(ui=ui,server=server)
