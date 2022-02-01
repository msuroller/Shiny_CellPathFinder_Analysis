library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
options(shiny.maxRequestSize = 100*1024^2)

ui <- dashboardPage(
    dashboardHeader(title = "GraphMaster 9000"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "input_tab", icon = icon("home")),
            menuItem("Graphs", tabName = "graph_tab", icon = icon("chart-bar"))
        )
    ),
    dashboardBody(
        tabItems(
            tabItem(tabName = "input_tab",
                    wellPanel(fileInput(inputId = "cell_stat",
                                        label = "Upload a Cell_Stats File", 
                                        accept = ".csv"),
                              actionButton(inputId = "action_table", label = "Submit")
                    ),
                    fluidRow(
                        box(
                            width = 12,
                            dataTableOutput(outputId = "table1")
                        )
                    ),
                    fluidRow(
                        box(
                          title = strong("Select the channels used."),
                          width = 4,
                          column(width = 5,
                            checkboxInput(inputId = "brightfield",
                                          label = "Brightfield",
                                          value = T),
                            checkboxInput(inputId = "b",
                                          label = "Blue",
                                          value = T),
                            checkboxInput(inputId = "g",
                                          label = "Green",
                                          value = T),
                            checkboxInput(inputId = "r",
                                          label = "Red",
                                          value = T),
                            checkboxInput(inputId = "fr",
                                          label = "Far Red",
                                          value = T)
                          ),
                          column(width = 7,
                                 selectInput(inputId = "b_dye",
                                           label = "Blue channel dye (optional)",
                                           choices = c("", "Hoechst")),
                                 selectInput(inputId = "g_dye",
                                           label = "Green channel dye (optional)",
                                           choices = c("","DiO", "Rhodamine123")),
                                 selectInput(inputId = "r_dye",
                                           label = "Red channel dye (optional)",
                                           choices = c("","MitoTacker Orange", "MitoTracker Red")),
                                 selectInput(inputId = "fr_dye",
                                           label = "Far Red channel dye (optional)",
                                           choices = c("","CyP-AP BioTracker", "CellRox"))
                                 )
                        ),
                        box(
                            width = 4,
                            textInput(inputId = "plate", 
                                      label = "Type the plate number or leave blank if no plate number. (optional)", 
                                      value = ""),
                            textInput(inputId = "con", 
                                      label = "Type the name of your control treatment. (optional)", 
                                      value = ""),
                            textInput(inputId = "background", 
                                      label = "Type the name of the treatment you would like subtracted as background. (optional)", 
                                      value = ""),
                            textInput(inputId = "doses",
                                      label = "Type a comma separated list of your doses (optional). 
                                      Any treatments you don't want included on the line graph set as 0......i.e. 0, 1e-5, 0.0001, 0.0002",
                                      value = ""),
                            column(width = 6,
                                   numericInput(inputId = "filter_missing_spheroids_low",
                                                label = "Only include wells with a cell count ",
                                                value = 0),
                            ),
                            column(width = 6,
                                   numericInput(inputId = "filter_missing_spheroids_high",
                                                label = "between these 2 numbers(optional)",
                                                value = 1e10),
                            ),
                        ),
                        box(
                            width = 4,
                            textInput(inputId = "cc_header", 
                                      label = "Copy and paste the name of the column for Cell Count.", 
                                      value = "X.Cell..Count"),
                            textInput(inputId = "b_header", 
                                      label = "Copy and paste the name of the column for Blue fluorescence.", 
                                      value = "X.Cell...Nucleus..MeanIntensity.CH2..AVG."),
                            textInput(inputId = "g_header", 
                                      label = "Copy and paste the name of the column for Green fluorescence.", 
                                      value = "X.Cell...Cellbody..MeanIntensity.CH3..AVG."),
                            textInput(inputId = "r_header", 
                                      label = "Copy and paste the name of the column for Red fluorescence.", 
                                      value = "X.Cell...Cellbody..MeanIntensity.CH4..AVG."),
                            textInput(inputId = "fr_header", 
                                      label = "Copy and paste the name of the column for Far Red fluorescence.", 
                                      value = "X.Cell...Cellbody..MeanIntensity.CH5..AVG."),
                            actionButton(inputId = "action", label = "Submit")
                        )
                    ),
            ),
            tabItem(tabName = "graph_tab",
                fluidPage(
                    fluidRow(
                        tabBox(
                            title = strong("Bar Graphs"), id = "tabset1",
                            tabPanel("Cell Count", h5(downloadButton("downloadcc1", "Download"), align = "right"), plotOutput(outputId = "cc_hist")),
                            tabPanel("Blue", h5(downloadButton("downloadb1", "Download"), align = "right"), plotOutput(outputId = "b_hist")),
                            tabPanel("Green", h5(downloadButton("downloadg1", "Download"), align = "right"), plotOutput(outputId = "g_hist")),
                            tabPanel("Red", h5(downloadButton("downloadr1", "Download"), align = "right"), plotOutput(outputId = "r_hist")),
                            tabPanel("Far Red", h5(downloadButton("downloadfr1", "Download"), align = "right"), plotOutput(outputId = "fr_hist"))
                        ),
                        tabBox(
                            title = strong("Normalized Bar Graphs"), id = "tabset2",
                            tabPanel("Cell Count", h5(downloadButton("downloadcc2", "Download"), align = "right"), plotOutput(outputId = "cc_hist_2")),
                            tabPanel("Blue", h5(downloadButton("downloadb2", "Download"), align = "right"), plotOutput(outputId = "b_hist_2")),
                            tabPanel("Green", h5(downloadButton("downloadg2", "Download"), align = "right"), plotOutput(outputId = "g_hist_2")),
                            tabPanel("Red", h5(downloadButton("downloadr2", "Download"), align = "right"), plotOutput(outputId = "r_hist_2")),
                            tabPanel("Far Red", h5(downloadButton("downloadfr2", "Download"), align = "right"), plotOutput(outputId = "fr_hist_2"))
                        )
                        
                    ),
                    fluidRow(
                        tabBox(
                            title = strong("Line Graphs"), id = "tabset3",
                            tabPanel("Cell Count", h5(downloadButton("downloadcc3", "Download"), align = "right"), plotOutput(outputId = "cc_hist_3")),
                            tabPanel("Blue", h5(downloadButton("downloadb3", "Download"), align = "right"), plotOutput(outputId = "b_hist_3")),
                            tabPanel("Green", h5(downloadButton("downloadg3", "Download"), align = "right"), plotOutput(outputId = "g_hist_3")),
                            tabPanel("Red", h5(downloadButton("downloadr3", "Download"), align = "right"), plotOutput(outputId = "r_hist_3")),
                            tabPanel("Far Red", h5(downloadButton("downloadfr3", "Download"), align = "right"), plotOutput(outputId = "fr_hist_3"))
                        ),
                        box(
                            title = strong("Download Output CSV file"), width = 6,
                            h5(downloadButton("download_csv", "Download"), align = "center")
                            
                        )
                    )
                )
            )
        )
    )
)

server <- function(input, output) {
    observeEvent(input$action_table, {
        validate(need(input$cell_stat$datapath != "", "Need to upload file"))
        showNotification("Submitted")
        #Read in the data file selected.
        Cell_Stat <- read.csv(input$cell_stat$datapath, header = T)
        #Display the table after hitting submit.
        output$table1 <- DT::renderDataTable(
            DT::datatable(
                Cell_Stat, extensions = 'FixedColumns', options = list(
                    pageLength = 3,
                    bSort = FALSE,
                    scrollX = TRUE,
                    fixedColumns = TRUE
                    
                )
            )
        )
    })
    
    #Once the 2nd submit button is clicked then do all that below.
    observeEvent(input$action,{
        Cell_Stat <- read.csv(input$cell_stat$datapath, header = T)
        con <- isolate(input$con)
        bg <- isolate(input$background)
        Plate <- isolate(input$plate)
        
        ifelse(input$b_dye != "", b_dye <-  input$b_dye, b_dye <- "Blue Channel")
        ifelse(input$g_dye != "", g_dye <-  input$g_dye, g_dye <-  "Green Channel")
        ifelse(input$r_dye != "", r_dye <-  input$r_dye, r_dye <-  "Red Channel")
        ifelse(input$fr_dye != "", fr_dye <-  input$fr_dye, fr_dye <-  "Far Red Channel")
        
        #If the channel box is checked then rename that column. <- WORKING
        Cell_Stat <- rename(Cell_Stat, 
                            cell_count = if(input$brightfield == T) isolate(input$cc_header),
                            b_avg = if(input$b == T) isolate(input$b_header),
                            g_avg = if(input$g == T) isolate(input$g_header),
                            r_avg = if(input$r == T) isolate(input$r_header),
                            fr_avg = if(input$fr == T) isolate(input$fr_header)
                            )
        
        #This section uses regular expressions to mine the image date to automate file names later on in the program. <- WORKING
        date <- str_extract(Cell_Stat$DateTime[1], "[0-9]{4}.[0-9]{2}.[0-9]{2}")%>%
            str_replace_all("-", "_")
        #This one is specifically for labeling graphs and making them look nicer.
        cleandate <- date%>%
            str_replace_all("_", "/")
        
        #fms=filter missing spheroGraphSeriesNos. This excludes data from any well below a certain cell count threshold. <- WORKING
        #Normally this value sits at around 1000 but may need to be tweaked. 
        #You can double check that it's filtering accurately by comparing the excluded rows with pictures of the plate.
     
        if (input$brightfield == T){
            fms <- Cell_Stat%>%
                filter(cell_count >= input$filter_missing_spheroids_low)%>%
                filter(cell_count <= input$filter_missing_spheroids_high)
        }
        else(fms <- Cell_Stat)
        
        #This adds the mean DMSO value for each channel, then makes a new column dividing all the other treatments by the mean DMSO value,
        #then finds the standard deviations of those values.
        con_norm <- fms #con_norm = control normalized  <- WORKING

        if (input$brightfield == T){ # <- WORKING
            con_norm <- mutate(con_norm, 
                               cc_sub = cell_count - mean(cell_count[Compound == bg]),
                               cc_sub_norm =100* cc_sub/mean(cc_sub[Compound == con]),
                               cc_con = (cell_count/mean(cell_count[Compound == con])))
        }
        
        if(input$b == T){
            con_norm <- mutate(con_norm, 
                               b_sub = b_avg - mean(b_avg[Compound == bg]),
                               b_sub_norm =100* b_sub/mean(b_sub[Compound == con]),
                               b_con = b_avg/mean(b_avg[Compound == con]))
        }
        
        if(input$g == T){
            con_norm <- mutate(con_norm, 
                               g_sub = g_avg - mean(g_avg[Compound == bg]),
                               g_sub_norm =100* g_sub/mean(g_sub[Compound == con]),
                               g_con = g_avg/mean(g_avg[Compound == con]))
        }
        
        if(input$r == T){
            con_norm <- mutate(con_norm,
                               r_sub = r_avg - mean(r_avg[Compound == bg]),
                               r_sub_norm =100* r_sub/mean(r_sub[Compound == con]),
                               r_con = r_avg/mean(r_avg[Compound == con]))
        }
        
        if(input$fr == T){
            con_norm <- mutate(con_norm, 
                               fr_sub = fr_avg - mean(fr_avg[Compound == bg]),
                               fr_sub_norm =100* fr_sub/mean(fr_sub[Compound == con]),
                               fr_con = fr_avg/mean(fr_avg[Compound == con]))
        }
        else(con_norm <- con_norm)
        
        
        
        #This makes a dataframe with means and stdevs of each treatment and control normalized treatments. <- WORKING
        sum_stat <- con_norm%>%  #sum_stat = Summary Statistics
            group_by(GraphSeriesNo, Compound, Dose)%>%
            summarise(#Ridiculously large chuck of ifelse statements, first up is cell count.
                cc_mean = ifelse(input$brightfield == T, mean(cell_count, na.rm = TRUE), 0),
                cc_stdev = ifelse(input$brightfield == T, sd(cell_count, na.rm = TRUE), 0),
                cc_mean_con = ifelse(input$brightfield == T, mean(cc_con), 0),
                cc_stdev_con = ifelse(input$brightfield == T, sd(cc_con), 0),
                cc_mean_sub = ifelse(input$brightfield == T, mean(cc_sub_norm), 0), 
                cc_stdev_sub = ifelse(input$brightfield == T, sd(cc_sub_norm), 0),
                #Then on to Blue
                b_mean = ifelse(input$b == T, mean(b_avg, na.rm = TRUE), 0),
                b_stdev = ifelse(input$b == T, sd(b_avg, na.rm = TRUE), 0),
                b_mean_con = ifelse(input$b == T, mean(b_con), 0),
                b_stdev_con = ifelse(input$b == T, sd(b_con), 0),
                b_mean_sub = ifelse(input$b == T, mean(b_sub_norm), 0), 
                b_stdev_sub = ifelse(input$b == T, sd(b_sub_norm), 0),
                #Now it's Green's turn
                g_mean = ifelse(input$g == T, mean(g_avg, na.rm=TRUE), 0),
                g_stdev = ifelse(input$g == T, sd(g_avg, na.rm=TRUE), 0),
                g_mean_con = ifelse(input$g == T, mean(g_con), 0),
                g_stdev_con = ifelse(input$g == T, sd(g_con), 0),
                g_mean_sub = ifelse(input$g == T, mean(g_sub_norm), 0), 
                g_stdev_sub = ifelse(input$g == T, sd(g_sub_norm), 0),
                #And Red
                r_mean = ifelse(input$r == T, mean(r_avg, na.rm=TRUE), 0),
                r_stdev = ifelse(input$r == T, sd(r_avg, na.rm=TRUE), 0),
                r_mean_con = ifelse(input$r == T, mean(r_con), 0),
                r_stdev_con = ifelse(input$r == T, sd(r_con), 0),
                r_mean_sub = ifelse(input$r == T, mean(r_sub_norm), 0), 
                r_stdev_sub = ifelse(input$r == T, sd(r_sub_norm), 0),
                #Finally Far Red
                fr_mean = ifelse(input$fr == T, mean(fr_avg, na.rm=TRUE), 0),
                fr_stdev = ifelse(input$fr == T, sd(fr_avg, na.rm=TRUE), 0),
                fr_mean_con = ifelse(input$fr == T, mean(fr_con), 0),
                fr_stdev_con = ifelse(input$fr == T, sd(fr_con), 0),
                fr_mean_sub = ifelse(input$fr == T, mean(fr_sub_norm), 0), 
                fr_stdev_sub = ifelse(input$fr == T, sd(fr_sub_norm), 0)
            )
        output$download_csv <- downloadHandler(
            filename = function() {
                paste0(date,"_", Plate,"_Data_Analysis_Stats.csv")
            },
            content = function(file) {
                write.csv(sum_stat, file)
            }
        )
        
        #####Start of "line graphs" section.
        #Takes user input for doses
        if (isolate(input$doses) != ""){
          doses <- as.numeric(unlist(strsplit(c(isolate(input$doses)), ",")))
          sum_stat$Dose = doses
        }
        
        filter_sum_stat <- sum_stat%>%
            filter(Dose != 0)

        #Need to ask for user input for these ones.
        lowerxlim = -5
        upperxlim = -3
        lx = seq(.000001,.001, by = 0.00001) #List x
        
        #Graphing regular and control normalized bar graphs<- Working
        style <- theme(plot.title = element_text(face="bold", hjust = 0.5), 
                       axis.title.y = element_text(face="bold"), 
                       axis.text.x = element_text(size=9, face="bold", angle=30, hjust = 1, vjust = 1), 
                       plot.margin = margin(10,10,10,50, "pt"))
        
        style2 <- theme(plot.title = element_text(face="bold", hjust = 0.5, size = 16), 
                        axis.title = element_text(face="bold", size = 14), 
                        axis.text = element_text(size = 14, face = "bold", color = "black"), 
                        axis.line = element_line(size = 1.25), 
                        axis.ticks = element_line(size = 1.25), 
                        axis.ticks.length = unit(10, "pt"), 
                        legend.justification=c(0,0), 
                        legend.position=c(1,1), 
                        plot.margin = margin(10,10,10,50, "pt"))
        
        
        if(input$brightfield == T){
            cc_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(GraphSeriesNo), y=cc_mean), fill="grey")+
                geom_errorbar(aes(x = factor(GraphSeriesNo), ymin = cc_mean - cc_stdev,  ymax = cc_mean + cc_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate, " ",Plate, " Mean Cell Count"), x=NULL, y = "Cell Count")
            output$cc_hist <- renderPlot({cc_graph})
            output$downloadcc1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Cell_Count_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = cc_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                cc_con_graph <- ggplot(data = sum_stat)+
                    geom_col(mapping = aes(x=factor(GraphSeriesNo), y = cc_mean_con), fill = "grey")+
                    geom_errorbar(aes(x=factor(GraphSeriesNo), ymin = cc_mean_con - cc_stdev_con, ymax = cc_mean_con + cc_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = sum_stat$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," ", con, " Normalized Cell Count"), x = NULL,y = paste0("Normalized Cell Count"))
                output$cc_hist_2 <- renderPlot({cc_con_graph})
                output$downloadcc2 <- downloadHandler(
                    filename = function() {
                        paste0(date,"_", Plate,"_Cell_Count_Control_Graph.svg")
                    },
                    content = function(file) {
                        ggsave(file, plot = cc_con_graph, width = 8, height = 5, device = svg)
                    }
                )
                if(bg != ""){
                    cc_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = cc_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = cc_mean_sub - cc_stdev_sub, ymax = cc_mean_sub + cc_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = cc_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate," Cell Count"),
                             x="Mefenamanic Acid Dose",
                             y = "Cell Count Mean")+
                        theme_classic()+
                        style2
                    output$cc_hist_3 <- renderPlot({cc_bg_graph})
                    output$downloadcc3 <- downloadHandler(
                        filename = function() {
                            paste0(date,"_", Plate,"_Cell_Count_Line_Graph.svg")
                        },
                        content = function(file) {
                            ggsave(file, plot = cc_bg_graph, width = 8, height = 5, device = svg)
                        }
                    )

                }
            }
        }
        
        if(input$b == T){
            b_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(GraphSeriesNo), y=b_mean), fill="blue")+
                geom_errorbar(aes(x = factor(GraphSeriesNo), ymin = b_mean - b_stdev,  ymax = b_mean + b_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate, " ", b_dye, " Mean Intensity"), x = NULL , y = paste0(b_dye," Mean Intensity"))
            output$b_hist <- renderPlot({b_graph})
            output$downloadb1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Blue_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = b_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                b_con_graph <- ggplot(data = sum_stat)+
                    geom_col(mapping = aes(x=factor(GraphSeriesNo), y = b_mean_con), fill = "blue")+
                    geom_errorbar(aes(x=factor(GraphSeriesNo), ymin = b_mean_con - b_stdev_con, ymax = b_mean_con + b_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = sum_stat$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," ",con," Normalized ", b_dye, " Mean Intensity"), x = NULL, y = paste0(b_dye, " Normalized Mean Intensity"))
                output$b_hist_2 <- renderPlot({b_con_graph})
                output$downloadb2 <- downloadHandler(
                    filename = function() {
                        paste0(date,"_", Plate,"_Blue_Control_Graph.svg")
                    },
                    content = function(file) {
                        ggsave(file, plot = b_con_graph, width = 8, height = 5, device = svg)
                    }
                )
                if(bg != ""){
                    b_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = b_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = b_mean_sub - b_stdev_sub, ymax = b_mean_sub + b_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = b_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate, " ", b_dye, " Mean Intensity"),
                             x="Dose",
                             y = paste0(b_dye," Mean Intensity"))+
                        theme_classic()+
                        style2
                    output$b_hist_3 <- renderPlot({b_bg_graph})
                    output$downloadb3 <- downloadHandler(
                        filename = function() {
                            paste0(date,"_", Plate,"_Blue_Line_Graph.svg")
                        },
                        content = function(file) {
                            ggsave(file, plot = b_bg_graph, width = 8, height = 5, device = svg)
                        }
                    )
                    
                }
            }
        }
        
        if(input$g == T){
            g_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(GraphSeriesNo), y=g_mean), fill="green")+
                geom_errorbar(aes(x = factor(GraphSeriesNo), ymin = g_mean - g_stdev,  ymax = g_mean + g_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate, " ", g_dye, " Mean Intensity"), x = NULL , y = paste0(g_dye," Mean Intensity"))
            output$g_hist <- renderPlot({g_graph})
            output$downloadg1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Green_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = g_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                g_con_graph <- ggplot(data = sum_stat)+
                    geom_col(mapping = aes(x=factor(GraphSeriesNo), y = g_mean_con), fill = "green")+
                    geom_errorbar(aes(x=factor(GraphSeriesNo), ymin = g_mean_con - g_stdev_con, ymax = g_mean_con + g_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = sum_stat$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," ", con," Normalized ", g_dye, " Mean Intensity"), x = NULL, y = paste0(g_dye," Normalized Mean Intensity"))
                output$g_hist_2 <- renderPlot({g_con_graph})
                output$downloadg2 <- downloadHandler(
                    filename = function() {
                        paste0(date,"_", Plate,"_Green_control_Graph.svg")
                    },
                    content = function(file) {
                        ggsave(file, plot = g_con_graph, width = 8, height = 5, device = svg)
                    }
                )
                if(bg != ""){
                    g_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = g_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = g_mean_sub - g_stdev_sub, ymax = g_mean_sub + g_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = g_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate, " ", g_dye, " Mean Intensity"),
                             x="Dose",
                             y = paste0(g_dye," Mean Intensity"))+
                        theme_classic()+
                        style2
                    output$g_hist_3 <- renderPlot({g_bg_graph})
                    output$downloadg3 <- downloadHandler(
                        filename = function() {
                            paste0(date,"_", Plate,"_Green_Graph.svg")
                        },
                        content = function(file) {
                            ggsave(file, plot = g_bg_graph, width = 8, height = 5, device = svg)
                        }
                    )

                }
            }
        }
        
        if(input$r == T){
            r_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(GraphSeriesNo), y=r_mean), fill="red")+
                geom_errorbar(aes(x = factor(GraphSeriesNo), ymin = r_mean - r_stdev,  ymax = r_mean + r_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate, " ", r_dye, " Mean Intensity"), x = NULL , y = paste0(r_dye," Mean Intensity"))
            output$r_hist <- renderPlot({r_graph})
            #Download button for the graph
            output$downloadr1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Red_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = r_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                r_con_graph <- ggplot(data = sum_stat)+
                    geom_col(mapping = aes(x=factor(GraphSeriesNo), y = r_mean_con), fill = "red")+
                    geom_errorbar(aes(x=factor(GraphSeriesNo), ymin = r_mean_con - r_stdev_con, ymax = r_mean_con + r_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = sum_stat$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," ", con," Normalized ", r_dye, " Mean Intensity"), x = NULL, y = paste0(r_dye, " Normalized Mean Intensity"))
                output$r_hist_2 <- renderPlot({r_con_graph})
                output$downloadr2 <- downloadHandler(
                    filename = function() {
                        paste0(date,"_", Plate,"_Red_Control_Graph.svg")
                    },
                    content = function(file) {
                        ggsave(file, plot = r_con_graph, width = 8, height = 5, device = svg)
                    }
                )
                if(bg != ""){
                    r_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = r_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = r_mean_sub - r_stdev_sub, ymax = r_mean_sub + r_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = r_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate, " ", r_dye, " Mean Intensity"),
                             x="Dose",
                             y = paste0(r_dye, " Mean Intensity"))+
                        theme_classic()+
                        style2
                    output$r_hist_3 <- renderPlot({r_bg_graph})
                    output$downloadr3 <- downloadHandler(
                        filename = function() {
                            paste0(date,"_", Plate,"_Red_Line_Graph.svg")
                        },
                        content = function(file) {
                            ggsave(file, plot = r_bg_graph, width = 8, height = 5, device = svg)
                        }
                    )

                }
            }
        }
        
        if(input$fr == T){
            fr_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(GraphSeriesNo), y=fr_mean), fill="purple")+
                geom_errorbar(aes(x = factor(GraphSeriesNo), ymin = fr_mean - fr_stdev,  ymax = fr_mean + fr_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate, " ", fr_dye, " Mean Intensity"), x = NULL , y = paste0(fr_dye, " Mean Intensity"))
            output$fr_hist <- renderPlot({fr_graph})
            output$downloadfr1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Far_Red_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = fr_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                fr_con_graph <- ggplot(data = sum_stat)+
                    geom_col(mapping = aes(x=factor(GraphSeriesNo), y = fr_mean_con), fill = "purple")+
                    geom_errorbar(aes(x=factor(GraphSeriesNo), ymin = fr_mean_con - fr_stdev_con, ymax = fr_mean_con + fr_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = sum_stat$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," ", con," Normalized ", fr_dye, " Mean Intensity"), 
                                 x = NULL,
                                 y = paste0(fr_dye, " Normalized Mean Intensity"))
                output$fr_hist_2 <- renderPlot({fr_con_graph})
                output$downloadfr2 <- downloadHandler(
                    filename = function() {
                        paste0(date,"_", Plate,"_Far_Red_Control_Graph.svg")
                    },
                    content = function(file) {
                        ggsave(file, plot = fr_graph, width = 8, height = 5, device = svg)
                    }
                )
                if(bg != ""){
                    fr_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = fr_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = fr_mean_sub - fr_stdev_sub, ymax = fr_mean_sub + fr_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = fr_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate, " ", fr_dye, " Mean Intensity"),
                             x="Dose",
                             y = paste0(fr_dye, " Mean Intensity"))+
                        theme_classic()+
                        style2
                    output$fr_hist_3 <- renderPlot({fr_bg_graph})
                    output$downloadfr3 <- downloadHandler(
                        filename = function() {
                            paste0(date,"_", Plate,"_Far_Red_Graph.svg")
                        },
                        content = function(file) {
                            ggsave(file, plot = fr_graph, width = 8, height = 5, device = svg)
                        }
                    )

                }
            }
        }
        
    })
}

shinyApp(ui, server)