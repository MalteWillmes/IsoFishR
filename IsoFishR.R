#Clear the environment
rm(list=ls())

####Installing all required packages####
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
install_load('shiny', 'mgcv', 'shinydashboard', 'tidyverse','broom', 'DT', 'zoo', 'changepoint','colourpicker', 'shinyWidgets')


library(shiny)

#Turn off warnings
options(warn=-1)
#Uncomment to turn warnings back on
#options(warn=0)

####Creating the dashboard and ui outline####
   ui <- dashboardPage(skin="black", title="IsoFishR",
                       dashboardHeader(title = img(src = 'logo.png',
                                                   title = "IsoFishR", height = "48px")),
                       
                       dashboardSidebar(
                         sidebarMenu(
                           p(""),
                           menuItem("About", tabName="About", icon=icon("info")),
                           menuItem("Projects", tabName="Projects", icon=icon("folder")),
                           menuItem("Data Reduction ", tabName="Data_reduction",icon=icon("cogs")),
                           menuItem("Data Analysis", tabName="Data_analysis",icon=icon("area-chart")),
                           menuItem("Data Reporting", tabName="Data_reporting", icon=icon("table")))),
####About####                      
                       
                       dashboardBody(
                         tabItems(
                           tabItem(tabName = "About",
                                   fluidRow(
                                     box(width = 9, status = "primary",
                                         h2("Welcome to IsoFishR"),
                                         hr(),
                                         h5("IsoFishR aims to provide a fast, reproducible, and transparent data reduction application for laser-ablation strontium isotope analysis"),
                                         h5(a(target="_blank", href = 'https://github.com/MalteWillmes/IsoFishR/blob/master/README.md',"Tutorial")),
                                         h5(a(target="_blank", href = 'https://github.com/MalteWillmes/IsoFishR',"Code on GitHub")),
                                         h5(a(target="_blank", href = 'https://github.com/MalteWillmes/IsoFishR/blob/master/LICENSE',"MIT license")),
                                         tags$br(),
                                         h5("Funding sources"),
                                         h5("This program has been developed in the Biogeochemistry and Fish Ecology Lab at UC Davis, California."),
                                         tags$br(),
                                         h5("Acknowledgements"),
                                         h5("We would like to thank Julie Griffin, Mackenzie Gilliam, James Chhor, and Brian Healey for testing the application and providing helpful comments.")
                                         ),
                                     box(
                                       title = "Version history", width = 3, status="warning",
                                       "0.1 - initial release (12/2016)",
                                       br(),
                                       "0.5 - moving forward (4/2017)",
                                       br(),
                                       "0.6 - updated projects management (8/2017)",
                                       br(),
                                       "0.9 - GitHub commit (11/2017)",
                                       br(),
                                       "1.0 - GitHub release (01/2018)"
                                     ),
                                     box(
                                       title = "Contact the Authors", width = 3, status="danger",
                                       h5( "Malte Willmes (mwillmes@ucdavis.edu)"),
                                       h5("Katherine Ransom (lockhart.katherine@gmail.com)"),
                                       h5( "James Hobbs (jahobbs@ucdavis.edu)")
                                     ))
                                   
                                   ),
####Projects####                         
                           tabItem(tabName = "Projects",
                                   fluidRow(
                                     column(width = 4,
                                            box(
                                              title = "Project management", width = NULL, status = "primary",
                                              selectInput(inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected="Default"),
                                              textInput(inputId="new.project",label="Create a new project ",value="Project"),
                                              actionButton("save","Save new project")
                                            ),
                                            box(
                                              title = "Update project settings", width = NULL, solidHeader = TRUE, status = "warning",
                                              actionButton("updatesettings","Update")
                                            ),
                                            box(
                                              title = "Project comments", width = NULL, status = "primary",
                                                textAreaInput(inputId="project.comment",label=NULL,value="Add comment here"),
                                                actionButton("comment","Save Comment"),DT::dataTableOutput("comments")
                                              )
                                              
                                            ),
                                     
                                     column(width = 4,
                                            box(
                                              title = "Default analysis parameters", width = NULL, status = "primary",
                                              textInput("username","User name"),
                                              textInput("sampletype",label="Sample type",value="Otolith"),
                                              radioButtons("analysistype","Analysis type", choices=list ("Line"="Line", "Spot"="Spot")),
                                              textInput("sampledirection",label="Analysis direction",value=""),
                                              numericInput("spotsize","Spot size",value=40),
                                              numericInput("speed", "Run speed", value=10, step = 1),
                                              radioButtons("smoother","Smoothing type for analysis",choices = list("MA"="MA","spline"="Spline", "Data Points (no smoothing)"="Data_points"),inline=TRUE),
                                              numericInput("average_num","Moving average window",value=10,step=1,min=1,max=NA),
                                              numericInput("spline_k","Spline k value",value=10,step=1,min=0.01,max=1000),
                                              radioButtons("CI","Outlier detection k*IQR ",choices = list(3,2,1.5,1),inline=TRUE),
                                              numericInput("outlier_num","Outlier moving window",value=20,step=1,min=30),
                                              numericInput("fluency","Fluency",value=1.85),
                                              numericInput("energy","Laser energy",value=55),
                                              textInput("defrange1","Range 1 label", value="Range 1"),
                                              textInput("defrange2","Range 2 label", value="Range 2"),
                                              textInput("defrange3","Range 3 label", value="Range 3"),
                                              textInput("defrange4","Range 4 label", value="Range 4"),
                                              textInput("defrange5","Range 5 label", value="Range 5"),
                                              textInput("defrange6","Range 6 label", value="Range 6"),
                                              textInput("defrange7","Range 7 label", value="Range 7"),
                                              textInput("defrange8","Range 8 label", value="Range 8")
                                            )
                                     ),
                                     
                                     column(width = 4,
                                            box(
                                              title = "Data reduction settings", width = NULL, status = "danger",
                                              numericInput("integration","Integration time/s",value=5),
                                              numericInput("blanktime","Blank time (s)",value=30),
                                              numericInput("raw88lowerthresh", "Min 88Sr", value=0.5, step = .1),
                                              numericInput("raw88upperthresh", "Max 88Sr", value=9.8, step = .1),
                                              numericInput("Sr8688ratio", "86Sr/88Sr ratio", value=0.1194, step = .001),
                                              numericInput("Rb8587ratio", "85Rb/87Rb", value=2.59712, step = .001)
                                            ),
                                            
                                            box(
                                              title = "Runfile import settings", width = NULL, status = "danger",
                                              numericInput("vskip","Header length",value=46),
                                              numericInput("raw88","Raw 88 column number",value=1),
                                              numericInput("raw87","Raw 87 column number",value=2),
                                              numericInput("raw86","Raw 86 column number",value=3),
                                              numericInput("raw85","Raw 85 column number",value=4),
                                              numericInput("raw84","Raw 84 column number",value=5),
                                              numericInput("raw83","Raw 83 column number",value=6),
                                              numericInput("cyclesec","Cycle seconds column number",value=8),
                                              radioButtons("sep","Separator type",choices = list(Comma=",",Tab="\t",Decimal=".")),
                                              checkboxInput("header","Data has column headings")
                                            )
                                           
                                     )
                                   )
                           ),
                                     
####Data reduction####                                    
tabItem(tabName = "Data_reduction",
fluidRow(
  box(
    title = "Data reduction", width = 3, status = "primary", height=200, style='padding:2px;',
    htmlOutput("ProcessHTML"),
    fileInput("file","Select one or multiple runfiles", multiple = TRUE)

  ),

  box(
    width = 3, status = "danger",style='padding:2px;',
    plotOutput("TotalSr_plot", height=200)),
  box(
    width = 3, status = "success",style='padding:2px;',
    plotOutput("Kr_plot", height=200)),
  box(
    width = 3, status = "warning",style='padding:2px;',
    plotOutput("Rb_plot", height=200))
  
  
 ),

fluidRow(
    column(width = 3,
  box(
    width = NULL,style='padding:2px;',
    uiOutput("Sample_selector"),
    actionButton("save_data","Save reduced data"),
    checkboxInput("appending_processed", "Append data?",value=TRUE),
    uiOutput('narem'),
    uiOutput('rollfill'))
    ),
    column(width = 9,
  box(
    width = NULL, style='padding:2px;', height=400,
    div(style="width:40px",dropdownButton(
      checkboxInput("main_points",label="Points", value=TRUE),
      checkboxInput("main_outlier_points",label="Outlier", value=TRUE),
      checkboxInput("main_ma",label="Moving average", value=TRUE),
      checkboxInput("main_ci",label="95% CI", value=TRUE),
      checkboxInput("main_sd",label="SD", value=FALSE),
      checkboxInput("main_spline",label="Spline fit", value=FALSE),
      checkboxInput("main_ocean",label="Mean ocean", value=FALSE),
      checkboxInput("reduced_custom",label="Custom line", value=FALSE),
      numericInput("reduced_custom_input",label="Custom line 87Sr/86Sr",value=0.705),
      colourInput("linecol", "Line", "black", allowTransparent = TRUE,showColour = "background"),
      colourInput("shadecol", "Shading", "gray", allowTransparent = TRUE, showColour = "background"),
      downloadButton('download_main_plot',''),
      circle = TRUE, status = "warning", size = "sm",
      icon = icon("gear"), label = NULL, tooltip = "Graph Settings", right = FALSE,
      up = FALSE)),
    plotOutput("mainplot", height=360)
    )))),

  
  
 tabItem(tabName = "Data_analysis",
         fluidRow(
           column(width = 3,
                  box(
                    width = NULL, title="Step 1: Select data to be analyzed",  status = "primary", style='padding:2px;',
                    htmlOutput("AnalysisHTML"),
                    fileInput("processed_file","Select .csv file",
                              accept=c("txt/csv", "text/comma-separated-values,text/plain", ".csv"))
                    ),
                  box(
                    width = NULL, title="Step 2: Select sample to edit",  status = "primary", style='padding:2px;',height=305,
                    uiOutput("Analyzed_sample_selector"),
                    uiOutput("Sample_ID"),
                    uiOutput("Sample_comment"),
                    uiOutput("flag_review")
                  ),
                  box(
                    title = "Step 3: Trim data", width = NULL, solidHeader = TRUE, status = "primary", style='padding:2px;',height=280,
                    uiOutput("trim_left"),
                    uiOutput("trim_right"),
                    uiOutput("recalculate_distance"),
                    uiOutput("trim_status_of_profile")
                  ),
                  box(
                    title = "Step 4: Reverse the profile?", width = NULL, solidHeader = TRUE, status = "primary", style='padding:2px;',height=150,
                    uiOutput("profile_direction_selector"),
                    uiOutput("status_of_profile")
                  )
                  
                  
           ),
           
           column(width = 9,
                  box(
                    status = "warning", width = NULL, height=470,style='padding:2px;',
                    div(style="width:40px",dropdownButton(
                                   checkboxInput("analyze_points",label="Points", value=TRUE),
                                   checkboxInput("analyze_outlier_points",label="Outlier", value=TRUE),
                                   checkboxInput("analyze_ma",label="Moving average", value=TRUE),
                                   checkboxInput("analyze_ci",label="95% CI", value=TRUE),
                                   checkboxInput("analyze_sd",label="SD", value=FALSE),
                                   checkboxInput("analyze_spline",label="Spline fit", value=FALSE),
                                   checkboxInput("analyze_ocean",label="Mean ocean", value=FALSE),
                                   checkboxInput("analyze_custom",label="Custom line", value=FALSE),
                                   numericInput("analyze_custom_input",label="Custom line 87Sr/86Sr",value=0.705),
                                   colourInput("analyze_linecol", "Line", "black", allowTransparent = TRUE,showColour = "background"),
                                   colourInput("analyze_shadecol", "Shading", "gray", allowTransparent = TRUE, showColour = "background"), 
                                   downloadButton('download_analysis_plot',''),
                                   circle = TRUE, status = "warning", size = "sm",
                                   icon = icon("gear"), label = NULL, tooltip = "Graph Settings", right = FALSE,
                                   up = FALSE)),
                    plotOutput("analyzed_plot", height=420, brush = brushOpts(id ="analyzed_plot_brush",direction ="x"))
                    
                  ),
            
                  tabBox(title="Step 5: Filters",height=280, width = 9, selected="About",
                         tabPanel("About", "Filters can be used to split the data into different regions"),
                         tabPanel("Manual Filter",
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_one_label", label=NULL)),
                        div(style="height:35px",numericInput("range_one_min","",value=-1)),
                        div(style="height:55px",numericInput("range_one_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion1", "Read",style="background-color:#6b8acd"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_two_label", label=NULL)),
                        div(style="height:35px",numericInput("range_two_min","",value=-1)),
                        div(style="height:55px", numericInput("range_two_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion2", "Read",style="background-color:#4cb28d"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_three_label",label=NULL)),
                        div(style="height:35px",numericInput("range_three_min","",value=-1)),
                        div(style="height:55px",numericInput("range_three_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion3", "Read",style="background-color:#72853c"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_four_label",label=NULL)),
                        div(style="height:35px",numericInput("range_four_min","",value=-1)),
                        div(style="height:55px", numericInput("range_four_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion4", "Read",style="background-color:#c78a40"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_five_label", label=NULL)),
                        div(style="height:35px",numericInput("range_five_min","",value=-1)),
                        div(style="height:55px",numericInput("range_five_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion5", "Read",style="background-color:#c75d9c"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_six_label", label=NULL)),
                        div(style="height:35px",numericInput("range_six_min","",value=-1)),
                        div(style="height:55px",numericInput("range_six_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion6", "Read",style="background-color:#cb554f"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_seven_label", label=NULL)),
                        div(style="height:35px",numericInput("range_seven_min","",value=-1)),
                        div(style="height:55px",numericInput("range_seven_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion7", "Read",style="background-color:#9061c9"))),
                    
                    div(style="float:left;width: 80px",
                        div(style="height:20px",textInput("range_eight_label", label=NULL)),
                        div(style="height:35px",numericInput("range_eight_min","",value=-1)),
                        div(style="height:55px",numericInput("range_eight_max","",value=-1)),
                        div(style= "height:35px",actionButton("readregion8", "Read",style="background-color:#83b844"))),
                    div(style="float:right; width:100px",actionButton("reset_ranges", "Reset ranges")),
                    div(style="float:right; width:100px",verbatimTextOutput("brush_info"))
                    ),
                    tabPanel("Changepoints", 
                             div(style="float:left;width:120px;",uiOutput('change_points_check')),
                             div(style="float:left;width:100px", uiOutput('change_method')),
                             div(style="float:left;width:100px", uiOutput('change_penalty')),
                             div(style="float:left;width:100px",uiOutput('changepoint_plotting_check')),
                             div(style="float:right;width:120px;", colourInput("changepoint_linecol", "Line", "red", allowTransparent = TRUE,showColour = "background"))
                            )
                  ),
                  box(
                    title = "Step 6: Saving data", width = 3, background = "orange", style='padding:2px;',
                    actionButton("save_file_edits","Save edits to profile")

                  ),
                  box(
                    title = "Step 7: Export data", width = 3, background = "black", style='padding:2px;',
                    actionButton("save_edits","Export analyzed data"),
                    checkboxInput("appending_analyzed", "Append data?",value=FALSE)
                  )
                  
           )
           )),
           
                           
tabItem(tabName = "Data_reporting",
        fluidRow(
          tabBox(width = 12,
            # The id lets us use input$tabset1 on the server to find the current tab
            id = "tabset_datatables",
            
            tabPanel("Analyzed data summary", 
                     div(style = 'overflow-x: scroll',DT::dataTableOutput('analyzed_data_table_summary'))
                     ),
            tabPanel("Analyzed data all ", 
                     div(style = 'overflow-x: scroll',DT::dataTableOutput('analyzed_data_table_all')
                         )),
            tabPanel("Reduced data all", 
                     div(style = 'overflow-x: scroll',DT::dataTableOutput('reduced_data_table_all'))
            ))))
)))

                           
####Server side####
server <- shinyServer(function(input, output, session) {
  #Set maximum filesize
  options(shiny.maxRequestSize=3000*1024^2)
  output$ProcessHTML <- renderText({
    paste("Selected project: ", input$project.name)
  })
  output$AnalysisHTML <- renderText({
    paste("Selected project: ", input$project.name)
  })
 
####Read runfiles####
  raw_data_all <- reactive({
    infile <- input$file
    raw_data_all<-""
    if(is.null(infile)){return()} 
    for(i in 1:length(input$file[,1])){
      raw_data <- read.table(c(input$file[[i, 'datapath']]), skip=input$vskip, sep=input$sep,header=ifelse(input$header==TRUE,TRUE,FALSE))
      raw_data <- cbind(raw_data, input$file[i,],row.names = NULL)
      raw_data_all <- rbind(raw_data_all, raw_data)
    }
     ##Drop extra columns and rename and order the raw data
     raw_data_all <- raw_data_all %>% filter(name!="")%>%select (-size, -type,-datapath)
     names(raw_data_all)[input$raw88] <- c("Raw88")
     names(raw_data_all)[input$raw87] <- c("Raw87")
     names(raw_data_all)[input$raw86] <- c("Raw86")
     names(raw_data_all)[input$raw85] <- c("Raw85")
     names(raw_data_all)[input$raw84] <- c("Raw84")
     names(raw_data_all)[input$raw83] <- c("Raw83")
     names(raw_data_all)[input$cyclesec] <- c("CycleSecs")
     raw_data_all <- raw_data_all[,c("Raw88", "Raw87","Raw86", "Raw85",  "Raw84", "Raw83", "CycleSecs","name")]

    return(raw_data_all)})
  
  
  
####Process data####
  processed_data <- reactive({
    
    raw <- raw_data_all()
    if(is.null(raw_data_all())){return()}
    
    #Change to numeric
    raw <- raw %>% group_by(name) %>% mutate_all(funs(as.numeric)) 
    
    #Calculate average based on integration time
    raw <- raw %>% group_by(name) %>% replace(1:6, rollapply(raw[1:6],width=input$integration, by=input$integration, FUN=mean, partial=TRUE,fill=NA)) %>%
                drop_na()
               
    
    #Split data to background and sample based on background time
    processed <- raw   %>% group_by(name) %>%
                           filter(CycleSecs>input$blanktime)
    background <- raw  %>% group_by(name) %>%
                           filter(CycleSecs<=input$blanktime)
    
    #Clean background data: Remove any values above the detection threshold and summarize
    background <- background %>% group_by(name) %>%
                                 filter(Raw88<input$raw88lowerthresh)%>%
                                 select (-CycleSecs)%>%
                                 rename(Blk88=Raw88, Blk87=Raw87, Blk86=Raw86, Blk85=Raw85, Blk84=Raw84, Blk83=Raw83)%>%
                                 summarise_all(funs(mean))
                                 
                                 
    
    #Clean processed data: Remove any values below or above the detection threshold
    processed <- processed %>% group_by(name) %>%
                               filter(Raw88>=input$raw88lowerthresh)%>%
                               filter(Raw88<=input$raw88upperthresh)
    
    #Perform a background subtraction on the processed data
     processed <- processed  %>% group_by(name) %>% 
                 left_join(background, processed, by = "name")%>%
                 #mutate_each(funs(replace(., .<0, 0)))
                 mutate(Raw88=Raw88-Blk88)%>%
                 mutate(Raw87=Raw87-Blk87)%>%
                 mutate(Raw86=Raw86-Blk86)%>%
                 mutate(Raw85=Raw85-Blk85)%>%
                 mutate(Raw84=Raw84-Blk84)%>%
                 mutate(Raw83=Raw83-Blk83)
       
    #Calculate distance based on user input of laser scan speed
      processed <- processed  %>% group_by(name) %>% 
                    mutate(Distance=(CycleSecs-min(CycleSecs))*input$speed)
      
    
    #Calculate raw ratios and Mass Bias
    processed <- processed  %>% group_by(name) %>% 
                 mutate(Sr8688=Raw86/Raw88)%>%
                 mutate(Sr8488=Raw84/Raw88)%>%
                 mutate(Sr8486=Raw84/Raw86)%>%
                 mutate(Sr8786_raw=Raw87/Raw86)%>%
                 mutate(Rb85Sr88=Raw85/Raw88)%>%
                 mutate(MbFactor=log10(input$Sr8688ratio/Sr8688)/log10(85.9092607/87.9056123))
    
    #Rubidium correction and apply Mass Bias
      processed <- processed  %>% group_by(name) %>%
      mutate(Rb87=(Raw85/input$Rb8587ratio)*(84.9117897/86.9091805)^MbFactor)%>%
      mutate(Sr87=Raw87-Rb87)%>%
      mutate(Sr87Sr86_rbcor=Sr87/Raw86)%>%
      mutate(Sr87Sr86_rbmbcor=(Sr87/Raw86)*(86.9088775/85.9092607)^MbFactor)
    
    #Calculate total Sr voltage and rename 87Sr/86Sr column
      processed <- processed  %>% group_by(name) %>%
                   mutate(Sr87Sr86=Sr87Sr86_rbmbcor)%>%
                   mutate(totalSr=Raw88+Raw86+Raw84+Sr87)
      
    #Outlier rejection
      CI <- as.numeric(input$CI) # user input confidence interval
      processed <- processed %>% group_by(name) %>% 
        mutate(median=rollapply(Sr87Sr86,input$outlier_num, mean,fill="extend", partial=TRUE))%>%
        mutate(upper75prob = rollapply(Sr87Sr86, width = input$outlier_num, FUN = quantile, partial = TRUE, probs = 0.75, na.rm=TRUE),
               lower25prob = rollapply(Sr87Sr86, width = input$outlier_num, FUN = quantile, partial = TRUE, probs = 0.25, na.rm=TRUE),
               IQR_range = upper75prob - lower25prob,
               up=upper75prob+(CI*IQR_range),
               low=lower25prob-(CI*IQR_range),
               Sr87Sr86_outlier=ifelse(Sr87Sr86>up|Sr87Sr86<low, Sr87Sr86, NA),
               Sr87Sr86 = ifelse(Sr87Sr86>up|Sr87Sr86<low, NA, Sr87Sr86))
              
        
      
      
    #Apply smoothing function: Moving average 
      processed <- processed  %>% group_by(name) %>%
                                mutate(Sr87Sr86_MA=rollapply(Sr87Sr86, input$average_num, FUN= mean, partial=TRUE,fill=input$rollfill, na.rm=input$narem))%>%
                                mutate(Sr87Sr86_MA_sds=rollapply(Sr87Sr86,input$average_num,FUN=sd, partial=TRUE, fill=input$rollfill, na.rm=input$narem))%>%
                                mutate(Sr87Sr86_MA_ses= (Sr87Sr86_MA_sds/sqrt(input$average_num)))
      
    #Apply Spline fit
      processed_spline <- processed  %>% group_by(name)%>% do(Sr87Sr86_spline=gam(Sr87Sr86 ~ s(Distance, k=input$spline_k), data=.))  
      processed_spline_aug <- augment(processed_spline, Sr87Sr86_spline)
      

    #Join data together based on name and Distance
      processed <- left_join(processed, processed_spline_aug, by = c("name","Distance"))
      #Estimate NA values
      processed <- processed %>% mutate (.fitted=na.approx(.fitted, na.rm = FALSE))%>% mutate (.se.fit=na.approx(.se.fit, na.rm = FALSE))
      
      
    #Add extra columns to hold default values (remember to add these also into the data_processsed clean dataframe in the next step "Create data subsets")
      processed <- processed  %>%  mutate(Sample_ID=name)%>%
                                   mutate(Date=strftime(Sys.time(), format="%Y-%m-%d_%H:%M:%S"))%>%
                                   mutate(Run_ID=name)%>%
                                   mutate(Sr87Sr86=Sr87Sr86.x) %>%
                                   mutate(Sr87Sr86_spline=.fitted) %>%
                                   mutate(Sr87Sr86_spline_ses=.se.fit)%>%
                                   mutate(profile_direction="Profile is normal")%>%
                                   mutate(trim_left=0)%>%
                                   mutate(trim_right=0)%>%
                                   mutate(recalc_distance=TRUE)%>%
                                   mutate(comment="")%>%
                                   mutate(flag_review=FALSE) %>%
                                   select(-Sr87Sr86.x, -Sr87Sr86.y, -.rownames, -.hat, -.cooksd, -.resid, -.sigma, -.fitted, -.se.fit) %>%
                                   ungroup()%>%
                                   mutate(name=paste0(Run_ID,"_",strftime(Sys.time(), format="%Y%m%d%H%M%S")))%>%
                                   group_by(name)%>%
                                   mutate (changepoints=FALSE)%>%
                                   mutate (manual_pen=0.00005)%>%
                                   mutate (change_method="")%>%
                                   mutate (changepoint_number= 0) %>%
                                   mutate (changepoint_mean=0)%>%
                                   mutate (changepoint_plotting=FALSE)%>%
                                   mutate (region_number=0)%>%
                                   mutate (region_name="")%>%
                                   mutate (region_mean=0)%>%
                                   mutate (region_sd=0)%>%
                                   mutate (region_mindistance=0)%>%
                                   mutate (region_maxdistance=0)
    return(processed)})

####Create data subsets####
#background data#
  background_data <- reactive({
    
    raw <- raw_data_all()
    if(is.null(raw_data_all())){return()}

    background <- raw  %>% group_by(name) %>%
      filter(CycleSecs<=input$blanktime)
    
    #Background data subset: Remove any values above the detection threshold and summarize
    background <- background %>% group_by(name) %>%
      filter(Raw88<input$raw88lowerthresh)%>%
      rename(Blk88=Raw88, Blk87=Raw87, Blk86=Raw86, Blk85=Raw85, Blk84=Raw84, Blk83=Raw83)
    return(background)})
   
  
#processed data subset#
  processed_data_cleaned <- reactive({
    
    processed_data_cleaned <- processed_data()
    if(is.null(processed_data())){return()}
    #Add the columns to keep for the next step in data processed (remove blanks, raw ratios, and ratio calculations)
    processed_data_cleaned <- processed_data_cleaned  %>% group_by(name) %>% select (name, Run_ID, Sample_ID, Date, CycleSecs, Distance, totalSr, Raw83, Rb85Sr88, Sr87Sr86, Sr87Sr86_outlier, Sr87Sr86_MA, Sr87Sr86_MA_sds, Sr87Sr86_MA_ses,Sr87Sr86_spline, 
                                                                                     Sr87Sr86_spline_ses, profile_direction, trim_right, trim_left, recalc_distance, comment, flag_review, changepoints, manual_pen, change_method, changepoint_number,changepoint_mean, changepoint_plotting,
                                                                                     region_number, region_name, region_mean, region_sd, region_mindistance, region_maxdistance) %>%
      mutate(Distance=round(Distance, digits=0))%>%
      mutate(totalSr=round(totalSr, digits=4))%>%
      mutate(Raw83=round(Raw83, digits=8))%>%
      mutate(Rb85Sr88=round(Rb85Sr88, digits=8))%>%
      mutate(Sr87Sr86=round(Sr87Sr86, digits=8))%>%
      mutate(Sr87Sr86_outlier=round(Sr87Sr86_outlier, digits=8))%>%
      mutate(Sr87Sr86_MA=round(Sr87Sr86_MA, digits=8))%>%
      mutate(Sr87Sr86_MA_sds=round(Sr87Sr86_MA_sds, digits=8))%>%
      mutate(Sr87Sr86_MA_ses=round(Sr87Sr86_MA_ses, digits=8)) %>%
      mutate(Sr87Sr86_spline=round(Sr87Sr86_spline, digits=8)) %>%
      mutate(Sr87Sr86_spline_ses=round(Sr87Sr86_spline_ses, digits=8))%>%
     group_by(name)%>%
      mutate (row_num=row_number())%>%
      ungroup()
    return(processed_data_cleaned )})
  
####Saving data####
  observeEvent(input$save_data, {
    t <- try(
    if(input$appending_processed){
      write.table(raw_data_all(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_01_raw_data.csv")),col.names=!file.exists(file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_01_raw_data.csv"))),row.names=FALSE,sep="," ,append = TRUE)
      write.table(background_data(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_02_background_data.csv")),col.names=!file.exists(file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_02_background_data.csv"))),row.names=FALSE,sep=",",append = TRUE)
      write.table(processed_data_cleaned(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_03_reduced_data.csv")),col.names=!file.exists(file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_03_reduced_data.csv"))),row.names=FALSE,sep=",",append = TRUE)
    }
    else{
    write.table(raw_data_all(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_01_raw_data.csv")),row.names=FALSE,col.names= TRUE, sep=",")
    write.table(background_data(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_02_background_data.csv")),col.names= TRUE, row.names=FALSE,sep=",")
    write.table(processed_data_cleaned(),file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_03_reduced_data.csv")),col.names= TRUE, row.names=FALSE,sep=",")
    },
    silent=TRUE) 
    # if there is no error print a success message otherwise print an error message
    if ("try-error" %in% class(t)){
      save_data_failed_notification <- showNotification(paste("File access denied: Saving failed. Is the file open?"), duration = 8, type="error")
    } else {
      save_data_notification <- showNotification(paste("Reduced data saved"), duration = 8, type="message")
    }
  })

  observeEvent(input$save_edits, {
    t <- try(
    if(input$appending_analyzed){
      write.table(analyzer_overwatch$analyzed,file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_04_analyzed_data.csv")),col.names=!file.exists(file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_04_analyzed_data.csv"))), row.names=FALSE,sep=",", append = TRUE)  
    }
    else{
      write.table(analyzer_overwatch$analyzed,file=file.path("Projects",input$project.name,"/Data",paste0(input$project.name,"_04_analyzed_data.csv")),row.names=FALSE, col.names= TRUE, sep=",") 
    },
    silent=TRUE) 
    # if there is no error print a success message otherwise print an error message
    if ("try-error" %in% class(t)){
      save_edits_failed_notification <- showNotification(paste("File access denied: Saving failed. Is the file open?"), duration = 8, type="error")
    } else {
      save_edits_notification <- showNotification(paste("Analyzed data saved"), duration = 8, type="message")
    }
  })
  

####Plot code####
  fmt_dcimals <- function(decimals=0){
    function(x) format(x,nsmall = decimals,scientific = FALSE)
  }
   
#mainplot
  mainplot <- function(){
    req(input$Sample_selector)
    if(is.null(processed_data_cleaned())){return()}
    processed_plot <-processed_data_cleaned()
    processed_plot <-processed_plot %>% filter(name==input$Sample_selector)
    
    p <- ggplot(processed_plot)
    p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") 
    p <- p + labs(y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), x=expression(paste("Distance (",mu,"m)")), title=paste0("Runfile:"," ",unlist(strsplit(as.character(input$Sample_selector), split='.', fixed=TRUE))[1]))
    p <- p + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
    p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))

    if(input$main_sd){
    p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-Sr87Sr86_MA_sds, ymax=Sr87Sr86_MA+Sr87Sr86_MA_sds), fill=input$shadecol)
    }
    if(input$main_ci){
    p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-(1.96*Sr87Sr86_MA_ses), ymax=Sr87Sr86_MA+(1.96*Sr87Sr86_MA_ses)), fill=input$shadecol)
    }
    if(input$main_ma){
      p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_MA), color=input$linecol)}
    if(input$main_spline){
      p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_spline-Sr87Sr86_spline_ses, ymax=Sr87Sr86_spline+Sr87Sr86_spline_ses), fill=input$shadecol)
      p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_spline), color=input$linecol)}
    
    if(input$main_points){
      p <- p + geom_point(aes(x=Distance,y=Sr87Sr86), shape=1)}
    if(input$main_outlier_points){
      p <- p + geom_point(aes(x=Distance,y=Sr87Sr86_outlier), color="red", shape=1)}
    
    if(input$main_ocean){
      p <- p + geom_line(aes(x=Distance,y=0.70918), color="blue")}
    
    if(input$reduced_custom){
      p <- p + geom_line(aes(x=Distance,y=input$reduced_custom_input), color="orange")}
    
    return(p)}
  
  output$mainplot <- renderPlot({mainplot()})
  
#Total Sr plot
  TotalSr_plot <- function(){
    req(input$Sample_selector)
    if(is.null(processed_data_cleaned())){return()}
    processed_plot <-processed_data_cleaned()
    processed_plot <-processed_plot %>% filter(name==input$Sample_selector)
    
    p <- ggplot(processed_plot)
    p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=("Sr (V)"), x=expression(paste("Distance (",mu,"m)")))
    p <- p + scale_y_continuous(labels = fmt_dcimals(2), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
    p <- p + geom_point(aes(x=Distance,y=totalSr), size=1, color="red", shape=1, na.rm=TRUE)
    
    
    return(p)}
  
  output$TotalSr_plot <- renderPlot({TotalSr_plot()})
  
  #Kr plot
  Kr_plot <- function(){
    req(input$Sample_selector)
    if(is.null(processed_data_cleaned())){return()}
    processed_plot <-processed_data_cleaned()
    processed_plot <-processed_plot %>% filter(name==input$Sample_selector)
    
    p <- ggplot(processed_plot)
    p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=expression(paste(""^"83"*"Kr (V)")), x=expression(paste("Distance (",mu,"m)")))
    p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
    p <- p + geom_point(aes(x=Distance,y=Raw83), size=1, color="lightgreen", shape=1 , na.rm=TRUE)
    
    
    return(p)}
  
  output$Kr_plot <- renderPlot({Kr_plot()})
  
  #Rb plot
  Rb_plot <- function(){
    req(input$Sample_selector)
    if(is.null(processed_data_cleaned())){return()}
    processed_plot <-processed_data_cleaned()
    processed_plot <-processed_plot %>% filter(name==input$Sample_selector)
    
    p <- ggplot(processed_plot)
    p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=expression(paste(""^"85"*"Rb/"^"88"*"Sr")), x=expression(paste("Distance (",mu,"m)")))
    p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
    p <- p + geom_point(aes(x=Distance,y=Rb85Sr88),size=1, color="orange", shape=1, na.rm=TRUE)
    
    
    return(p)}
  
  output$Rb_plot <- renderPlot({Rb_plot()})
  
###Action to save all plots from processing tab #####
  observeEvent(input$save_data, {
    
  names <-  as.character(unique(processed_data_cleaned()$name))
  t <- try(
  for (i in names){
    #mainplot
    mainplot <- function(){
      if(is.null(processed_data_cleaned())){return()}
      processed_plot <-processed_data_cleaned()
      processed_plot <-processed_plot %>% filter(name==i)
      
      p <- ggplot(processed_plot)
      p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") 
      p <- p + labs(y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), x=expression(paste("Distance (",mu,"m)")), title=processed_plot$name)
      p <- p + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
      p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))
      
      
      if(input$main_sd){
        p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-Sr87Sr86_MA_sds, ymax=Sr87Sr86_MA+Sr87Sr86_MA_sds), fill=input$shadecol)
      }
      if(input$main_ci){
        p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-(1.96*Sr87Sr86_MA_ses), ymax=Sr87Sr86_MA+(1.96*Sr87Sr86_MA_ses)), fill=input$shadecol)
      }
      if(input$main_ma){
        p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_MA), color=input$linecol)}
      if(input$main_spline){
        p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_spline-Sr87Sr86_spline_ses, ymax=Sr87Sr86_spline+Sr87Sr86_spline_ses), fill=input$shadecol, na.rm=TRUE)
        p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_spline), na.rm=TRUE, color=input$linecol)}
      
      if(input$main_points){
        p <- p + geom_point(aes(x=Distance,y=Sr87Sr86), shape=1, na.rm=TRUE)}
      if(input$main_outlier_points){
        p <- p + geom_point(aes(x=Distance,y=Sr87Sr86_outlier), color="red", shape=1, na.rm=TRUE)}
      
      if(input$main_ocean){
        p <- p + geom_line(aes(x=Distance,y=0.70918), color="blue")}
      if(input$reduced_custom){
        p <- p + geom_line(aes(x=Distance,y=input$reduced_custom_input), color="orange")}
      
      return(p)}
    
    #Total Sr plot
    TotalSr_plot <- function(){
      if(is.null(processed_data_cleaned())){return()}
      processed_plot <-processed_data_cleaned()
      processed_plot <-processed_plot %>% filter(name==i)
      p <- ggplot(processed_plot)
      p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=("Sr (V)"), x=expression(paste("Distance (",mu,"m)")))
      p <- p + scale_y_continuous(labels = fmt_dcimals(2), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
      p <- p + geom_point(aes(x=Distance,y=totalSr), size=1, color="red", shape=1, na.rm=TRUE)
      return(p)}
    
    #Kr plot
    Kr_plot <- function(){
      if(is.null(processed_data_cleaned())){return()}
      processed_plot <-processed_data_cleaned()
      processed_plot <-processed_plot %>% filter(name==i)
      p <- ggplot(processed_plot)
      p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=expression(paste(""^"83"*"Kr (V)")), x=expression(paste("Distance (",mu,"m)")))
      p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
      p <- p + geom_point(aes(x=Distance,y=Raw83), size=1, color="lightgreen", shape=1 , na.rm=TRUE)
      return(p)}
    
    #Rb plot
    Rb_plot <- function(){
      if(is.null(processed_data_cleaned())){return()}
      processed_plot <-processed_data_cleaned()
      processed_plot <-processed_plot %>% filter(name==i)
      p <- ggplot(processed_plot)
      p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") + labs(y=expression(paste(""^"85"*"Rb/"^"88"*"Sr")), x=expression(paste("Distance (",mu,"m)")))
      p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(.01,0.01), breaks = scales::pretty_breaks(n = 10))
      p <- p + geom_point(aes(x=Distance,y=Rb85Sr88),size=1, color="orange", shape=1, na.rm=TRUE)
      return(p)}
    
  ggsave(filename=file.path("Projects",input$project.name,"Plots/87Sr86Sr_reduced",paste0(i,"_87Sr86Sr_reduced.pdf")), plot = mainplot(), device = "pdf",width=8,height=4,scale=1.5)
  ggsave(filename=file.path("Projects",input$project.name,"Plots/totalSr",paste0(i,"_totalSr_volts.pdf")), plot = TotalSr_plot(), device = "pdf",width=8,height=4,scale=1.5)
  ggsave(filename=file.path("Projects",input$project.name,"Plots/85Rb88Sr",paste0(i,"_85Rb88Sr.pdf")), plot = Rb_plot(), device = "pdf",width=8,height=4,scale=1.5)
  ggsave(filename=file.path("Projects",input$project.name,"Plots/83Kr",paste0(i,"_83Kr_volts.pdf")), plot = Kr_plot(), device = "pdf",width=8,height=4,scale=1.5)
  },silent=TRUE)
  # if there is no error print a success message otherwise print an error message
  if ("try-error" %in% class(t)){
    save_process_plots_failed_notification <- showNotification(paste("Plot file access denied: Saving failed. Is the file open?"), duration = 12, type="error")
  } else {
    save_process_plots_notification <- showNotification(paste("Reduced data plots saved"), duration = 8, type="message")
  }
  })
  
####Analyzed plot ####
  
  analyzed_plot <- function(){
    #Check if available
    req(input$Analyzed_sample_selector)
    analyzed_plot <- analyzer_overwatch$analyzed 
    analyzed_plot <- analyzed_plot %>% filter(name==input$Analyzed_sample_selector)
    
    p <- ggplot(analyzed_plot)
    p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") 
    p <- p + labs(y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), x=expression(paste("Distance (",mu,"m)")), title=paste0("Runfile:"," ",unlist(strsplit(as.character(input$Analyzed_sample_selector), split='.', fixed=TRUE))[1]," ","Sample ID:"," ",input$Sample_ID))
    p <- p + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
    p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))
   
    if(input$analyze_sd){
      p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-Sr87Sr86_MA_sds, ymax=Sr87Sr86_MA+Sr87Sr86_MA_sds), fill=input$analyze_shadecol, na.rm=TRUE)
    }
    if(input$analyze_ci){
      p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-(1.96*Sr87Sr86_MA_ses), ymax=Sr87Sr86_MA+(1.96*Sr87Sr86_MA_ses)), fill=input$analyze_shadecol, na.rm=TRUE)
    }
    if(input$analyze_ma){
      p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_MA), na.rm=TRUE, color=input$analyze_linecol)}
    
    if(input$analyze_spline){
      p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_spline-Sr87Sr86_spline_ses, ymax=Sr87Sr86_spline+Sr87Sr86_spline_ses), fill=input$shadecol, na.rm=TRUE)
      p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_spline), na.rm=TRUE, color=input$linecol)}
    
    if(input$analyze_points){
      p <- p + geom_point(aes(x=Distance,y=Sr87Sr86), shape=1, na.rm=TRUE)}
    if(input$analyze_outlier_points){
      p <- p + geom_point(aes(x=Distance,y=Sr87Sr86_outlier), color="red", shape=1, na.rm=TRUE)}
    
    if(input$analyze_ocean){
      p <- p + geom_line(aes(x=Distance,y=0.70918), color="blue")}
    
    if(input$analyze_custom){
      p <- p + geom_line(aes(x=Distance,y=input$analyze_custom_input), color="orange")}
  
      #Manual regions
    p <- p + annotate("rect", xmin = reac$range_one_min, xmax = reac$range_one_max, ymin = -Inf, ymax = +Inf,
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
               alpha = .8, fill="#83b844")

    #Changepoints
    if(analyzed_plot[1,"changepoint_plotting"]==TRUE){
      p <- p + geom_line(aes(x=Distance,y=changepoint_mean), color=input$changepoint_linecol)}
    
    return(p)}
  
  output$analyzed_plot <- renderPlot({analyzed_plot()})
 
   # Create the brush output for analyzed_plot
  output$brush_info <- renderPrint({
    str(input$analyzed_plot_brush$xmin)
    str(input$analyzed_plot_brush$xmax)
  })
  

####Action to save all plots from analysis tab ####
  
  observeEvent(input$save_edits, {
    
    names <-  as.character(unique(analyzed_data()$name))
    t <- try(
    for (i in names){
      analyzed_plot <- function(){
        if(is.null(analyzed_data())){return()}
        analyzed_plot <- analyzer_overwatch$analyzed
        analyzed_plot <- analyzed_plot %>% filter(name==i)
        manual_region_plotter <- analyzed_plot %>% group_by(region_number) %>% select(region_number, Distance)%>%
        mutate (min_distance =min(Distance))%>%
        mutate (max_distance=max(Distance))%>% select(-Distance)%>%distinct()
        region_colors <- c("NA", "#6b8acd", "#4cb28d", "#72853c", "#c78a40", "#c75d9c", "#cb554f", "#9061c9", "#83b844")
        region_alpha <- c(0,0.8,0.8,0.8,0.8,0.8,0.8,0.8,0.8)
        region_colors <-data.frame(region_colors, region_alpha) %>%
        mutate(region_number=row_number()-1)
        #Join data with colors
        manual_region_plotter<-left_join(manual_region_plotter, region_colors, by="region_number")

        p <- ggplot(analyzed_plot)
        p <- p + theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+ theme(legend.position="none") 
        p <- p + labs(y=expression(paste(""^"87"*"Sr/"^"86"*"Sr")), x=expression(paste("Distance (",mu,"m)")), title= paste0(analyzed_plot$name, " ","Sample ID:"," ",analyzed_plot$Sample_ID))
        p <- p + theme(axis.text.x  = element_text(size=12), axis.text.y  = element_text(size=12), axis.title.x = element_text(size=15), axis.title.y = element_text(size=15))
        p <- p + scale_y_continuous(labels = fmt_dcimals(5), breaks = scales::pretty_breaks(n = 10)) +  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(n = 10))
        
        
        if(input$analyze_sd){
          p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-Sr87Sr86_MA_sds, ymax=Sr87Sr86_MA+Sr87Sr86_MA_sds), fill=input$analyze_shadecol, na.rm=TRUE)
        }
        if(input$analyze_ci){
          p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_MA-(1.96*Sr87Sr86_MA_ses), ymax=Sr87Sr86_MA+(1.96*Sr87Sr86_MA_ses)), fill=input$analyze_shadecol, na.rm=TRUE)
        }
        if(input$analyze_ma){
          p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_MA), na.rm=TRUE, color=input$analyze_linecol)}
        if(input$analyze_spline){
          p <- p +geom_ribbon(aes(x=Distance,ymin=Sr87Sr86_spline-Sr87Sr86_spline_ses, ymax=Sr87Sr86_spline+Sr87Sr86_spline_ses), fill=input$shadecol, na.rm=TRUE)
          p <- p +geom_line(aes(x=Distance,y=Sr87Sr86_spline), na.rm=TRUE, color=input$linecol)}
        
        if(input$analyze_points){
          p <- p + geom_point(aes(x=Distance,y=Sr87Sr86), shape=1, na.rm=TRUE)}
        if(input$analyze_outlier_points){
          p <- p + geom_point(aes(x=Distance,y=Sr87Sr86_outlier), color="red", shape=1, na.rm=TRUE)}
        
        if(input$analyze_ocean){
          p <- p + geom_line(aes(x=Distance,y=0.70918), color="blue")}
        
        if(input$analyze_custom){
          p <- p + geom_line(aes(x=Distance,y=input$analyze_custom_input), color="orange")}
        
        #Manual regions
        p <- p + annotate("rect", xmin =manual_region_plotter$min_distance , xmax = manual_region_plotter$max_distance, ymin = -Inf, ymax = +Inf, group=manual_region_plotter$region_number,
                          alpha =manual_region_plotter$region_alpha, fill=manual_region_plotter$region_colors)
         
        #Changepoints
        if(analyzed_plot[1,"changepoint_plotting"]==TRUE){
          p <- p + geom_line(aes(x=Distance,y=changepoint_mean), color=input$changepoint_linecol)}
        
        return(p)} 
  
      ggsave(filename=file.path("Projects",input$project.name,"Plots/87Sr86Sr_analyzed",paste0(i,"_87Sr86Sr_analyzed.pdf")), plot = analyzed_plot(), device = "pdf",width=8,height=4,scale=1.5)
    }, silent=TRUE) 
    # if there is no error print a success message otherwise print an error message
    if ("try-error" %in% class(t)){
      save_analysis_plot_failed_notification <- showNotification(paste("Plot file access denied: Saving failed. Is the file open?"), duration = 12, type="error")
    } else {
      save_analysis_plot_notification <- showNotification(paste("Analyzed plots saved"), duration = 8, type="message")
    }
  })
  
####Individual plot downloads####
  
  #create the download for the single analysis plot
  output$download_analysis_plot <- downloadHandler(
    filename = function () {paste0(unlist(strsplit(as.character(input$Sample_ID), split='.', fixed=TRUE))[1],"_87Sr86Sr.pdf")},
    content = function(file) {
      ggsave(file, plot = analyzed_plot(), device = "pdf",width=8,height=4,scale=1.5)
    }
  )
  #create the download for the single process plot
  output$download_main_plot <- downloadHandler(
    filename = function () {paste0(unlist(strsplit(as.character(input$Sample_selector), split='.', fixed=TRUE))[1],"_87Sr86Sr.pdf")},
    content = function(file) {
      ggsave(file, plot = mainplot(), device = "pdf",width=8,height=4,scale=1.5)
    }
  )
####ui code####
  #Creating the processed sample selector
  output$Sample_selector <- renderUI ({
    sample_names <- processed_data_cleaned()$name
    selectInput("Sample_selector","Select sample to plot",choices=sample_names)
    
  })
  #Creating the analyzed sample selector
  output$Analyzed_sample_selector <- renderUI ({
    analyzed_sample_names <- analyzed_data()$name
    selectInput("Analyzed_sample_selector",label="Select sample",choices=analyzed_sample_names)
    
  })
  
  #Point or line data 
  output$narem <- renderUI ({
    analysistype_selected <- input$analysistype
    selection_narem <- ifelse(analysistype_selected =="Line",TRUE,FALSE)
    selectInput("narem", label="Remove NAs?", choices=c("TRUE","FALSE"), selected = selection_narem)

  })
  output$rollfill <- renderUI ({
    analysistype_selected <- input$analysistype
    selection_rollfill <- ifelse(analysistype_selected =="Line","extend",FALSE)
    selectInput("rollfill", label="Fill NAs?", choices=c("extend","FALSE"), selected = selection_rollfill)
  })


  #Checking the Sample_ID
  output$Sample_ID <- renderUI ({
    #Check if available
    req(input$Analyzed_sample_selector)

    sample_id <- analyzer_overwatch$analyzed
    sample_id <- sample_id %>% group_by(name) %>%
      filter(name==input$Analyzed_sample_selector)%>%
      select(name, Sample_ID)%>%
      summarise(Sample_ID=first(Sample_ID))
    textInput("Sample_ID", label="Sample ID", value=sample_id$Sample_ID)
  })
  
  #Checking the Comment
  output$Sample_comment <- renderUI ({
    #Check if available
    req(input$Analyzed_sample_selector)
    
   sample_comment <- analyzer_overwatch$analyzed
   sample_comment <- sample_comment %>% group_by(name) %>%
      filter(name==input$Analyzed_sample_selector)%>%
      select(name, comment)%>%
     summarise(comment=first(comment))
   textInput("Sample_comment", label="Comment", value=sample_comment$comment)
  })
  #Reading the trim values from the data
  output$trim_left <- renderUI ({
    if(is.null(analyzer_overwatch$analyzed)){return()}
    numericInput("trim_left","Left trim",value=0)
  })
 output$trim_right <- renderUI ({
   if(is.null(analyzer_overwatch$analyzed)){return()}
    numericInput("trim_right","Right trim",value=0)
  })
 
 #Flag for review?
 output$flag_review<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   
   flag <- analyzer_overwatch$analyzed
   flag <- flag %>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, flag_review)%>%
     summarise(flag_review=first(flag_review))
   checkboxInput("flag_review", "Flag for review?",value=flag$flag_review)
 })
 
 
 #Status of profile for reverse
   #Check if available
   output$status_of_profile <- renderText({
     req(input$Analyzed_sample_selector)
     paste(status_profile ())
   })

 
 output$profile_direction_selector<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   selectInput("profile_direction_selector", "Change direction of profile", c("No change","Reverse profile"), selected="No change")

 })
 
 output$trim_status_of_profile <- renderText({
   req(input$Analyzed_sample_selector)
   paste("Profile has been trimmed by","left:", trim_status_profile()$trim_left, "right:",trim_status_profile()$trim_right)
 })
 
 
 #Recalculate distance
 output$recalculate_distance<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   
   rec <- analyzer_overwatch$analyzed
   rec <- rec %>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, recalc_distance)%>%
     summarise(recalc_distance=first(recalc_distance))
   checkboxInput("recalculate_distance", "Recalculate distance?",value=rec$recalc_distance)
 })
 
 
 #Datafilter: Changepoint
 #check if changepoint is enabled
 output$change_points_check<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   change_check <- analyzer_overwatch$analyzed
   change_check <- change_check%>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, changepoints)%>%
     summarise(changepoints=first(changepoints))
   checkboxInput("change_points_check", "Enable Changepoints?", value=change_check$changepoints)
 }) 
 #check which changepoint method is chosen
 output$change_method<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   change_methodselect <- analyzer_overwatch$analyzed
   change_methodselect <- change_methodselect%>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, change_method)%>%
     summarise(change_method=first(change_method))
   selectInput("change_method",label="Method",choices= c("PELT", "AMOC", "SegNeigh", "BinSeg"), selected=change_methodselect$change_method)
 }) 
 #check which penalty value is chosen
 output$change_penalty<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   change_penalty <- analyzer_overwatch$analyzed
   change_penalty <- change_penalty %>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, manual_pen)%>%
     summarise(manual_pen=first(manual_pen))
   numericInput("manual_pen",label="Penalty value",value=change_penalty$manual_pen,step=0.00001,min=0,max=0.01)
 }) 
 #Check to plot Changepoint lines
 output$changepoint_plotting_check<- renderUI ({
   #Check if available
   req(input$Analyzed_sample_selector)
   changepoint_plotting_checking <- analyzer_overwatch$analyzed
   changepoint_plotting_checking <- changepoint_plotting_checking%>% group_by(name) %>%
     filter(name==input$Analyzed_sample_selector)%>%
     select(name, changepoint_plotting)%>%
     summarise(changepoint_plotting=first(changepoint_plotting))
   checkboxInput("changepoint_plotting_check", "Plot Changepoints?", value=changepoint_plotting_checking$changepoint_plotting)
 }) 
  
####Project management code ####
  readSettings <- reactive({
    settingsdf <- read.csv(file.path("Projects",input$project.name,paste0(input$project.name,"_settings.csv")),header=TRUE)
  })
  # take the settings object and set the settings to the defaults
  observe({
    input$project.name
    settingsdf <- readSettings()
    updateTextInput(session, "username",value = settingsdf$username)
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
    updateNumericInput(session,"raw88lowerthresh",value=settingsdf$raw88lowerthresh)
    updateNumericInput(session,"raw88upperthresh",value=settingsdf$raw88upperthresh)
    updateNumericInput(session,"average_num",value=settingsdf$average_num)
    updateNumericInput(session,"spline_k",value=settingsdf$spline_k)
    updateNumericInput(session,"outlier_num",value=settingsdf$outlier_num)
    updateRadioButtons(session,"CI",selected=settingsdf$CI)
    updateNumericInput(session,"speed",value=settingsdf$runspeed)
    updateNumericInput(session,"fluency",value=settingsdf$fluency)
    updateNumericInput(session,"spotsize",value=settingsdf$spotsize)
    updateNumericInput(session,"speed",value=settingsdf$speed)
    updateNumericInput(session,"energy",value=settingsdf$energy)
    updateNumericInput(session,"integration",value=settingsdf$integration)
    updateTextInput(session,"sampletype",value=settingsdf$sampletype)
    updateTextInput(session,"sampledirection",value=settingsdf$sampledirection)
    updateRadioButtons(session,"analysistype",selected=settingsdf$analysistype)
    updateNumericInput(session,"blanktime",value=settingsdf$blanktime)
    updateNumericInput(session,"Sr8688ratio",value=settingsdf$Sr8688ratio)
    updateNumericInput(session,"Rb8587ratio",value=settingsdf$Rb8587ratio)
    updateTextInput(session,"defrange1",value=settingsdf$range1_label)
    updateTextInput(session,"defrange2",value=settingsdf$range2_label)
    updateTextInput(session,"defrange3",value=settingsdf$range3_label)
    updateTextInput(session,"defrange4",value=settingsdf$range4_label)
    updateTextInput(session,"defrange5",value=settingsdf$range5_label)
    updateTextInput(session,"defrange6",value=settingsdf$range6_label)
    updateTextInput(session,"defrange7",value=settingsdf$range7_label)
    updateTextInput(session,"defrange8",value=settingsdf$range8_label)
  })
  
  observe({
    input$project.name
    updateTextInput(session,"range_one_label",value=input$defrange1)
    updateTextInput(session,"range_two_label",value=input$defrange2)
    updateTextInput(session,"range_three_label",value=input$defrange3)
    updateTextInput(session,"range_four_label",value=input$defrange4)
    updateTextInput(session,"range_five_label",value=input$defrange5)
    updateTextInput(session,"range_six_label",value=input$defrange6)
    updateTextInput(session,"range_seven_label",value=input$defrange7)
    updateTextInput(session,"range_eight_label",value=input$defrange8)
  })
  
  observeEvent(input$updatesettings,{
    updateTextInput(session,"range_one_label",value=input$defrange1)
    updateTextInput(session,"range_two_label",value=input$defrange2)
    updateTextInput(session,"range_three_label",value=input$defrange3)
    updateTextInput(session,"range_four_label",value=input$defrange4)
    updateTextInput(session,"range_five_label",value=input$defrange5)
    updateTextInput(session,"range_six_label",value=input$defrange6)
    updateTextInput(session,"range_seven_label",value=input$defrange7)
    updateTextInput(session,"range_eight_label",value=input$defrange8)
  })
  
  # create a new project and create the files and subdirectories 
  observeEvent(input$save,{
    settings <- cbind(input$raw88,input$raw87,input$raw86,input$raw85,input$raw84,
                      input$raw83,input$cyclesec,input$vskip,input$header,input$sep,
                      input$smoother,input$raw88lowerthresh,input$raw88upperthresh,
                      input$average_num,input$spline_k,input$outlier_num,
                      input$CI,input$speed,input$fluency,input$spotsize, input$energy, input$integration, 
                      input$sampletype,input$analysistype,input$sampledirection, input$blanktime, input$Sr8688ratio, input$Rb8587ratio,input$username, input$defrange1, input$defrange2, input$defrange3, input$defrange4, input$defrange5, input$defrange6, input$defrange7, input$defrange8)
    colnames(settings) <- c("raw88","raw87","raw86","raw85","raw84","raw83",
                            "cyclesec","vskip","header","sep","smoother",
                            "raw88lowerthresh", "raw88upperthresh", "average_num","spline_k",
                            "outlier_num","CI","speed","fluency","spotsize","energy","integration", "sampletype", "analysistype", "sampledirection", "blanktime", "Sr8688ratio", "Rb8587ratio","username", "range1_label", "range2_label","range3_label", "range4_label", "range5_label", "range6_label", "range7_label", "range8_label")
    if(dir.exists(paste0("Projects/",input$new.project))==FALSE){
      dir.create(paste0("Projects/",input$new.project))
      dir.create(paste0("Projects/",input$new.project,"/Plots"))
      dir.create(paste0("Projects/",input$new.project,"/Plots/87Sr86Sr_reduced"))
      dir.create(paste0("Projects/",input$new.project,"/Plots/87Sr86Sr_analyzed"))
      dir.create(paste0("Projects/",input$new.project,"/Plots/totalSr"))
      dir.create(paste0("Projects/",input$new.project,"/Plots/83Kr"))
      dir.create(paste0("Projects/",input$new.project,"/Plots/85Rb88Sr"))
      dir.create(paste0("Projects/",input$new.project,"/Data"))
      write.table(settings,file.path("Projects",input$new.project,paste0(input$new.project,"_settings.csv")),row.names=FALSE,col.names=TRUE,sep=",")}
  })
  
  # update the selected project to the newly created project upon creation (this is turn will read in the new project settings and update them)  
  observeEvent(input$save,{
    updateSelectInput(session,inputId = 'project.name',label = "Select Project",choices = list.files(path = file.path(".","Projects")),selected=input$new.project)
  })
  
  # button to allow user to update the project settings. Will overwrite the current settings csv file in a project folder.
  observeEvent(input$updatesettings,{
    t <- try({
    settings <- cbind(input$raw88,input$raw87,input$raw86,input$raw85,input$raw84,
                      input$raw83,input$cyclesec,input$vskip,input$header,input$sep,
                      input$smoother,input$raw88lowerthresh,input$raw88upperthresh,
                      input$average_num,input$spline_k,input$outlier_num,
                      input$CI,input$speed,input$fluency,input$spotsize, input$energy, input$integration, 
                      input$sampletype, input$analysistype, input$sampledirection, input$blanktime, input$Sr8688ratio, input$Rb8587ratio,input$username, input$defrange1, input$defrange2, input$defrange3, input$defrange4, input$defrange5, input$defrange6, input$defrange7, input$defrange8)
    colnames(settings) <- c("raw88","raw87","raw86","raw85","raw84","raw83",
                            "cyclesec","vskip","header","sep","smoother",
                            "raw88lowerthresh", "raw88upperthresh", "average_num","spline_k",
                            "outlier_num","CI","speed","fluency","spotsize","energy","integration", "sampletype", "analysistype", "sampledirection", "blanktime", "Sr8688ratio", "Rb8587ratio","username","range1_label", "range2_label","range3_label", "range4_label", "range5_label", "range6_label", "range7_label", "range8_label")
    write.table(settings,file.path("Projects",input$project.name,paste0(input$project.name,"_settings.csv")),row.names=FALSE,col.names=TRUE,sep=",")
    silent=TRUE})
    # if there is no error print a success message otherwise print an error message
    if ("try-error" %in% class(t)){
      save_settings_failed_notification <- showNotification(paste("File access denied: Saving failed. Is the file open?"), duration = 8, type="error")
    } else {
      save_settings_notification <- showNotification(paste("Project settings saved"), duration = 8, type="message")
    }
  })
  
  # project wide comments input and display
  observeEvent(input$comment,{
    t <- try({
    date <- Sys.Date()
    comment <- input$project.comment
    comments <- cbind.data.frame(date,comment)
    if(file.exists(file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")))==FALSE){
      write.table(comments,file=file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),col.names = c("Date","Comment"),row.names=FALSE,sep=",")
    }else{
      write.table(comments,file=file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),append=TRUE,col.names = FALSE,row.names=FALSE,sep=",") 
    }
    silent=TRUE})
    if ("try-error" %in% class(t)){
      save_settings_failed_notification <- showNotification(paste("File access denied: Saving failed. Is the file open?"), duration = 8, type="error")
    } else {
      save_settings_notification <- showNotification(paste("Comment saved"), duration = 8, type="message")
    }
  })
  
  
  output$comments <-  DT::renderDataTable({
    input$comment
    t <- try(comments <- read.csv(file.path("Projects",input$project.name,paste0(input$project.name,"_comments.csv")),header=TRUE),silent=TRUE)
    if ("try-error" %in% class(t)){
      return()
    } else {
      DT::datatable(comments, options = list(pageLength = 10,searching=FALSE))
    }
  })
  
####Data analyzer code####

#create a reactiveValues to store the analyzed and edited data.
analyzer_overwatch <-reactiveValues(analyzed=NULL)
  
####upload the reduced data file####
  analyzed_data <- reactive({

    analyzed_data <- input$processed_file
    if(is.null(analyzed_data )){return()} 
    analyzed_data <- read.table(analyzed_data$datapath, header = TRUE,  stringsAsFactors=FALSE, sep = ",", na.strings = "NA")
    analyzed_data <- analyzed_data %>% group_by(name) %>%
                                        mutate (Sample_ID=as.character(Sample_ID))%>%  
                                        mutate (Run_ID=as.character(Run_ID))%>%  
                                        mutate (comment=as.character(comment))%>% 
                                        mutate (flag_review=as.logical(flag_review))%>% 
                                        mutate (profile_direction=as.character(profile_direction))%>%  
                                        mutate (recalc_distance=as.logical(recalc_distance))%>%  
                                        mutate (CycleSecs=as.numeric(CycleSecs))%>% 
                                        mutate (Distance=as.numeric(Distance))%>% 
                                        mutate (totalSr=as.numeric(totalSr))%>% 
                                        mutate (Raw83=as.numeric(Raw83))%>% 
                                        mutate (Rb85Sr88=as.numeric(Rb85Sr88))%>% 
                                        mutate (Sr87Sr86=as.numeric(Sr87Sr86))%>% 
                                        mutate (Sr87Sr86_outlier=as.numeric(Sr87Sr86_outlier))%>% 
                                        mutate (Sr87Sr86_MA=as.numeric(Sr87Sr86_MA))%>% 
                                        mutate (Sr87Sr86_MA_sds=as.numeric(Sr87Sr86_MA_sds))%>% 
                                        mutate (Sr87Sr86_MA_ses=as.numeric(Sr87Sr86_MA_ses))%>% 
                                        mutate (Sr87Sr86_spline=as.numeric(Sr87Sr86_spline))%>% 
                                        mutate (Sr87Sr86_spline_ses=as.numeric(Sr87Sr86_spline_ses))%>% 
                                        mutate (trim_left=as.numeric(trim_left))%>% 
                                        mutate (trim_right=as.numeric(trim_right))%>%
                                        mutate (changepoints=as.logical(changepoints))%>%
                                        mutate (manual_pen=as.numeric(manual_pen))%>%
                                        mutate (change_method=as.character(change_method))%>%
                                        mutate (changepoint_plotting=as.logical(changepoint_plotting))%>%
                                        mutate (changepoint_number=as.numeric(changepoint_number)) %>%
                                        mutate (changepoint_mean=as.numeric(changepoint_mean))%>%
                                        mutate (region_number=as.numeric(region_number))%>%
                                        mutate (region_name=as.character(region_name))%>%
                                        mutate (region_mean=as.numeric(region_mean))%>%
                                        mutate (region_sd=as.numeric(region_sd))%>%
                                        mutate (region_mindistance=as.numeric(region_mindistance))%>%
                                        mutate (region_maxdistance=as.numeric(region_maxdistance))

    
    #Select to which Sr87Sr86 values filtering will be applied
    if(input$smoother=="Data_points"){
      analyzed_data<- analyzed_data %>% mutate (Sr87Sr86_analysis=Sr87Sr86)
    }
    if(input$smoother=="MA"){
    analyzed_data<- analyzed_data %>% mutate (Sr87Sr86_analysis=Sr87Sr86_MA)
    }
    if(input$smoother=="spline"){
      analyzed_data <- analyzed_data %>% mutate(Sr87Sr86_analysis=Sr87Sr86_spline)
    }
    
    analyzer_overwatch$analyzed <- analyzed_data
    
    return(analyzed_data)})
 
  
#Edit the sample
  edit_data <- reactive({
    edit_data <- analyzer_overwatch$analyzed
    edit_data <- edit_data %>% group_by(name)
    
    if(is.null(edit_data)){return()} 
    
    edit_data <-edit_data %>% mutate(Sample_ID=replace(Sample_ID,name==input$Analyzed_sample_selector,input$Sample_ID))%>% #Edit the Sample ID
    mutate(trim_left=replace(trim_left,name==input$Analyzed_sample_selector,input$trim_left+trim_left))%>% #Add trim values to the dataframe
      mutate(trim_right=replace(trim_right,name==input$Analyzed_sample_selector,input$trim_right+trim_right))%>%
      mutate(comment=replace(comment,name==input$Analyzed_sample_selector,input$Sample_comment))%>% #Add the comment to the sample
      mutate(flag_review=replace(flag_review,name==input$Analyzed_sample_selector,input$flag_review))%>%#Add flag for review?
      filter(!(name==input$Analyzed_sample_selector & Distance<input$trim_left | Distance >max(Distance)-input$trim_right))
    
    # Recalculate distance by the left trim value
    if(input$recalculate_distance){
      edit_data <-edit_data %>% mutate(Distance=replace(Distance,name==input$Analyzed_sample_selector,Distance-input$trim_left))
    }
    else{
      edit_data <-edit_data %>% mutate(recalc_distance=replace(recalc_distance,name==input$Analyzed_sample_selector,FALSE))
    }
    
    #Reverse the profile if selected
    if(input$profile_direction_selector=="Reverse profile"&status_profile()=="Profile has been reversed"){
      edit_data <-edit_data %>%
        mutate(profile_direction=replace(profile_direction,name==input$Analyzed_sample_selector,"Profile is normal"))%>%
        mutate(Distance=replace(Distance,name==input$Analyzed_sample_selector,rev(Distance)))
    }
    if(input$profile_direction_selector=="Reverse profile"&status_profile()=="Profile is normal"){
      edit_data <-edit_data %>%
        mutate(profile_direction=replace(profile_direction,name==input$Analyzed_sample_selector,"Profile has been reversed"))%>%
        mutate(Distance=replace(Distance,name==input$Analyzed_sample_selector,rev(Distance)))
    }
    
    else{edit_data <-edit_data %>%mutate(profile_direction=replace(profile_direction,name==input$Analyzed_sample_selector,"Profile is normal"))
    }
    

 #Data filters
    
  #Calculate an average across the entire (trimmed) profile
    edit_data <-edit_data %>% mutate(Sr87Sr86_profile_average = mean(Sr87Sr86_analysis, na.rm=TRUE))
    
  #Changepoints for selected sample
    if(!is.null(input$change_points_check)){
    
    if(input$change_points_check){
     #Store the changepoint analysis for the selected sample
       edit_data_filter <- edit_data %>%filter(name==input$Analyzed_sample_selector)
     #Perform changepoint analysis
      change <-cpt.mean(edit_data_filter$Sr87Sr86_analysis,penalty = "Manual",  pen.value = input$manual_pen, method = input$change_method, test.stat = "Normal", class = TRUE, param.estimates = TRUE)
      changeing_points <-cbind(change@cpts,change@param.est$mean)
  
      #Add results to dataframe and calculate mean value of Changepoint split dataset
      edit_data <- edit_data %>%
        
        #Add new changepoints
        mutate (changepoints=replace(changepoints,name==input$Analyzed_sample_selector,TRUE))%>% 
        mutate (change_method=replace(change_method,name==input$Analyzed_sample_selector,input$change_method)) %>% 
        mutate (manual_pen=replace(manual_pen, name==input$Analyzed_sample_selector,input$manual_pen))%>%
        mutate (changepoint_plotting=replace(changepoint_plotting, name==input$Analyzed_sample_selector,input$changepoint_plotting_check))%>%
        #Adding the changepoints back into the dataframe
       group_by(name, Distance)%>%
        mutate (changepoint_number=replace(changepoint_number,name==input$Analyzed_sample_selector, which(changeing_points[,1]>=row_num)[1]))%>%
        mutate (changepoint_number=replace(changepoint_number, name==input$Analyzed_sample_selector, na.fill(changepoint_number, fill=max(changepoint_number))))%>%
        ungroup(Distance)%>%
        #Calculate the mean over the changepoint split dataset
       group_by(changepoint_number) %>%
     mutate (changepoint_mean=replace(changepoint_mean, name==input$Analyzed_sample_selector, mean(Sr87Sr86_analysis)))%>%
        ungroup()
    }
      else{
        edit_data <- edit_data %>%
          #clear old changepoints
          mutate (changepoints=replace(changepoints,name==input$Analyzed_sample_selector, FALSE))%>% 
          mutate (change_method=replace(change_method,name==input$Analyzed_sample_selector,NA)) %>% 
          mutate (manual_pen=replace(manual_pen, name==input$Analyzed_sample_selector,input$manual_pen))%>%
          mutate (changepoint_plotting=replace(changepoint_plotting, name==input$Analyzed_sample_selector,FALSE))
      }
      }
    
    #Manual region filter
    edit_data <-edit_data %>%
      
      #Clear region names and numbers
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector,0)) %>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector,NA))%>%       
      
      #Add region names
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_one_min<=Distance & input$range_one_max>=Distance, input$range_one_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_two_min<=Distance & input$range_two_max>=Distance, input$range_two_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_three_min<=Distance & input$range_three_max>=Distance, input$range_three_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_four_min<=Distance & input$range_four_max>=Distance, input$range_four_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_five_min<=Distance & input$range_five_max>=Distance, input$range_five_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_six_min<=Distance & input$range_six_max>=Distance, input$range_six_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_seven_min<=Distance & input$range_seven_max>=Distance, input$range_seven_label, region_name)))%>%
      mutate(region_name=replace(region_name,name==input$Analyzed_sample_selector, ifelse(input$range_eight_min<=Distance & input$range_eight_max>=Distance, input$range_eight_label, region_name)))%>%
      #Add region numbers
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_one_min<=Distance & input$range_one_max>=Distance, 1, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_two_min<=Distance & input$range_two_max>=Distance, 2, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_three_min<=Distance & input$range_three_max>=Distance, 3, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_four_min<=Distance & input$range_four_max>=Distance, 4, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_five_min<=Distance & input$range_five_max>=Distance, 5, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_six_min<=Distance & input$range_six_max>=Distance, 6, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_seven_min<=Distance & input$range_seven_max>=Distance, 7, region_number)))%>%
      mutate(region_number=replace(region_number,name==input$Analyzed_sample_selector, ifelse(input$range_eight_min<=Distance & input$range_eight_max>=Distance, 8, region_number)))%>%
      #Group by region names and calculate mean and sd
      group_by(name,region_number)%>%
      mutate(region_mean=replace(region_mean,name==input$Analyzed_sample_selector, mean(Sr87Sr86_analysis)))%>%
      mutate(region_sd=replace(region_sd,name==input$Analyzed_sample_selector, sd(Sr87Sr86_analysis)))%>%
      mutate(region_mindistance=replace(region_mindistance,name==input$Analyzed_sample_selector, min(Distance)))%>%
      mutate(region_maxdistance=replace(region_maxdistance,name==input$Analyzed_sample_selector, max(Distance)))%>%
      ungroup()
    
    #Order the dataframe by ascending distance
    edit_data <- edit_data%>%arrange (name, Distance)
    return(edit_data)})

  
  
  #Saving edits from the profile to the overwatch
    observeEvent(input$save_file_edits, {
      analyzer_overwatch$analyzed <-edit_data()
  })
  
   # Direction status of the profile
    status_profile <- reactive({
    profile_status_var <- analyzer_overwatch$analyzed
      if(is.null(status_profile)){return()} 
      profile_status_var<- profile_status_var %>% group_by(name) %>%
        filter(name==input$Analyzed_sample_selector)%>%
        select(name, profile_direction)%>%
        distinct(profile_direction)
      status_profile <-profile_status_var$profile_direction
      return(status_profile)})  
    
    # Trim status of the profile
    trim_status_profile <- reactive({
      trim_status_profile_var <- analyzer_overwatch$analyzed
      if(is.null(trim_status_profile )){return()} 
      trim_status_profile_var<- trim_status_profile_var %>% group_by(name) %>%
        filter(name==input$Analyzed_sample_selector)%>%
        select(name, trim_right,trim_left)
      trim_status_profile <- distinct(trim_status_profile_var)
      
      return(trim_status_profile)})  
    
    

####Manual filter selection####

# Reset the manual filter ranges on click of button or on new sample selection
observe({
  req(input$Analyzed_sample_selector)
  input$Analyzed_sample_selector
  input$save_file_edits
  #Find the changes in region numbers, names, min and max distance and create a summary table for lookup
  range_observer <-analyzer_overwatch$analyzed %>%
    filter(name==input$Analyzed_sample_selector)%>%
    select (name, Distance, region_number, region_name)%>%
    group_by(region_number)%>%
    mutate (min_distance=min(Distance))%>%
    mutate (max_distance=max(Distance))%>%
    select (-Distance)%>%
    distinct()%>%
    ungroup()
  
  range1_data<-range_observer%>%filter(region_number==1)
  range2_data<-range_observer%>%filter(region_number==2)
  range3_data<-range_observer%>%filter(region_number==3)
  range4_data<-range_observer%>%filter(region_number==4)
  range5_data<-range_observer%>%filter(region_number==5)
  range6_data<-range_observer%>%filter(region_number==6)
  range7_data<-range_observer%>%filter(region_number==7)
  range8_data<-range_observer%>%filter(region_number==8)
  
  updateTextInput(session,"range_one_label", label=NULL, value=ifelse(nrow(range1_data)==0, input$defrange1, range1_data$region_name))
  updateNumericInput(session,"range_one_min","",value=ifelse(nrow(range1_data)==0, -1, range1_data$min_distance))
  updateNumericInput(session,"range_one_max","",value=ifelse(nrow(range1_data)==0, -1, range1_data$max_distance))
  
  updateTextInput(session,"range_two_label", label=NULL, value=ifelse(nrow(range2_data)==0, input$defrange2, range2_data$region_name))
  updateNumericInput(session,"range_two_min","",value=ifelse(nrow(range2_data)==0, -1, range2_data$min_distance))
  updateNumericInput(session,"range_two_max","",value=ifelse(nrow(range2_data)==0, -1, range2_data$max_distance))
  
  updateTextInput(session,"range_three_label", label=NULL, value=ifelse(nrow(range3_data)==0, input$defrange3, range3_data$region_name))
  updateNumericInput(session,"range_three_min","",value=ifelse(nrow(range3_data)==0, -1, range3_data$min_distance))
  updateNumericInput(session,"range_three_max","",value=ifelse(nrow(range3_data)==0, -1, range3_data$max_distance))
  
  updateTextInput(session,"range_four_label", label=NULL, value=ifelse(nrow(range4_data)==0, input$defrange4, range4_data$region_name))
  updateNumericInput(session,"range_four_min","",value=ifelse(nrow(range4_data)==0, -1, range4_data$min_distance))
  updateNumericInput(session,"range_four_max","",value=ifelse(nrow(range4_data)==0, -1, range4_data$max_distance))
  
  updateTextInput(session,"range_five_label", label=NULL, value=ifelse(nrow(range5_data)==0, input$defrange5, range5_data$region_name))
  updateNumericInput(session,"range_five_min","",value=ifelse(nrow(range5_data)==0, -1, range5_data$min_distance))
  updateNumericInput(session,"range_five_max","",value=ifelse(nrow(range5_data)==0, -1, range5_data$max_distance))
  
  updateTextInput(session,"range_six_label", label=NULL, value=ifelse(nrow(range6_data)==0, input$defrange6, range6_data$region_name))
  updateNumericInput(session,"range_six_min","",value=ifelse(nrow(range6_data)==0, -1, range6_data$min_distance))
  updateNumericInput(session,"range_six_max","",value=ifelse(nrow(range6_data)==0, -1, range6_data$max_distance))
  
  updateTextInput(session,"range_seven_label", label=NULL, value=ifelse(nrow(range7_data)==0, input$defrange7, range7_data$region_name))
  updateNumericInput(session,"range_seven_min","",value=ifelse(nrow(range7_data)==0, -1, range7_data$min_distance))
  updateNumericInput(session,"range_seven_max","",value=ifelse(nrow(range7_data)==0, -1, range7_data$max_distance))
  
  updateTextInput(session,"range_eight_label", label=NULL, value=ifelse(nrow(range8_data)==0, input$defrange8, range8_data$region_name))
  updateNumericInput(session,"range_eight_min","",value=ifelse(nrow(range8_data)==0, -1, range8_data$min_distance))
  updateNumericInput(session,"range_eight_max","",value=ifelse(nrow(range8_data)==0, -1, range8_data$max_distance))
  
})


    
# action to read brushed values into the corresponding ranges
observeEvent(input$readregion1, {
  updateNumericInput(session,  "range_one_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_one_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion2, {
  updateNumericInput(session,  "range_two_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_two_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion3, {
  updateNumericInput(session,  "range_three_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_three_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion4, {
  updateNumericInput(session,  "range_four_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_four_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion5, {
  updateNumericInput(session,  "range_five_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_five_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion6, {
  updateNumericInput(session,  "range_six_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_six_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion7, {
  updateNumericInput(session,  "range_seven_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_seven_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})
observeEvent(input$readregion8, {
  updateNumericInput(session,  "range_eight_min", label = NULL, value = input$analyzed_plot_brush$xmin,
                     min = NULL, max = NULL, step = NULL)
  updateNumericInput(session,  "range_eight_max", label = NULL, value = input$analyzed_plot_brush$xmax,
                     min = NULL, max = NULL, step = NULL)
})

# sets the reac values #
reac <- reactiveValues()

# This event will fire for a change to the ranges
observe({
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
})


#Reset ranges
observeEvent(input$reset_ranges,{
  req(input$Analyzed_sample_selector)
  updateTextInput(session,"range_one_label", label=NULL, value="Range 1")
  updateNumericInput(session,"range_one_min","",value=-1)
  updateNumericInput(session,"range_one_max","",value=-1)
  
  updateTextInput(session,"range_two_label", label=NULL, value="Range 2")
  updateNumericInput(session,"range_two_min","",value=-1)
  updateNumericInput(session,"range_two_max","",value=-1)
  
  updateTextInput(session,"range_three_label", label=NULL, value="Range 3")
  updateNumericInput(session,"range_three_min","",value=-1)
  updateNumericInput(session,"range_three_max","",value=-1)
  
  updateTextInput(session,"range_four_label", label=NULL, value="Range 4")
  updateNumericInput(session,"range_four_min","",value=-1)
  updateNumericInput(session,"range_four_max","",value=-1)
  
  updateTextInput(session,"range_five_label", label=NULL, value="Range 5")
  updateNumericInput(session,"range_five_min","",value=-1)
  updateNumericInput(session,"range_five_max","",value=-1)
  
  updateTextInput(session,"range_six_label", label=NULL, value="Range 6")
  updateNumericInput(session,"range_six_min","",value=-1)
  updateNumericInput(session,"range_six_max","",value=-1)
  
  updateTextInput(session,"range_seven_label", label=NULL, value="Range 7")
  updateNumericInput(session,"range_seven_min","",value=-1)
  updateNumericInput(session,"range_seven_max","",value=-1)
  
  updateTextInput(session,"range_eight_label", label=NULL, value="Range 8")
  updateNumericInput(session,"range_eight_min","",value=-1)
  updateNumericInput(session,"range_eight_max","",value=-1)
})


#Create the data tables
# Reduced data table
#all reduced data
output$reduced_data_table_all <-  DT::renderDataTable({
  reduced_data_table_all <- processed_data_cleaned()
  if(is.null(reduced_data_table_all)){return()}
  DT::datatable(reduced_data_table_all, options = list(pageLength = 20))
})
#all analyzed data
  output$analyzed_data_table_all <-  DT::renderDataTable({
    analyzed_data_table_all <- analyzer_overwatch$analyzed
    if(is.null(analyzed_data_table_all)){return()}
    DT::datatable(analyzed_data_table_all, options = list(pageLength = 20))
  })
#summary table
  output$analyzed_data_table_summary <-  DT::renderDataTable({
    req(analyzer_overwatch$analyzed)
    analyzed_data_table_summary <- analyzer_overwatch$analyzed
    analyzed_data_table_summary <- analyzed_data_table_summary %>% select(name, Sample_ID, region_number, region_name, region_mindistance, region_maxdistance, region_mean, region_sd) %>% 
      group_by (region_name) %>%
      distinct (name, .keep_all = TRUE) %>%
      mutate (region_mean=round(region_mean, 5))%>%
      mutate (region_sd=round(region_sd, 5))%>%
      mutate (region_mindistance=round(region_mindistance,0))%>%
      mutate (region_maxdistance=round(region_maxdistance,0))
    if(is.null(analyzed_data_table_summary)){return()}
    DT::datatable(analyzed_data_table_summary, options = list(pageLength = 20))
  })

    
  #### Stop shiny app when closing the browser ####
  session$onSessionEnded(stopApp)
})

shinyApp(ui=ui,server=server)


