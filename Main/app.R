library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

?sidebarMenu

ui <- dashboardPage(
    dashboardHeader(title = "GraphMaster 9000"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("The Boring Tab", tabName = "input_tab", icon = icon("dashboard")),
            menuItem("Pretty Graphs", tabName = "graph_tab", icon = icon("th"))
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
                                          value = T),
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
                            
                            numericInput(inputId = "filter_missing_spheroids",
                                         label = "Any well with a cell count below this number will not be counted. Used to filter missing spheroids",
                                         value = 1000),
                            actionButton(inputId = "action", label = "Submit")
                        )
                    ),
                    # fluidRow(
                    #     box(
                    #         width = 12,
                    #         dataTableOutput(outputId = "Cell_Stat")
                    #     )
                    # ),
                    # fluidRow(
                    #     box(
                    #         textOutput(outputId = "date"),
                    #         textOutput(outputId = "cleandate"),
                    #         textOutput(outputId = "control")
                    #     )
                    # ),
                    # fluidRow(
                    #     box(
                    #         width = 12,
                    #         dataTableOutput(outputId = "fms")
                    #     )
                    # ),
                    # fluidRow(
                    #     box(
                    #         width = 12,
                    #         dataTableOutput(outputId = "con_norm")
                    #     )
                    # ),
                    # fluidRow(
                    #     box(
                    #         width = 12,
                    #         dataTableOutput(outputId = "sum_stat")
                    #     )
                    # )
            ),
            tabItem(tabName = "graph_tab",
                    tabBox(
                        title = strong("Bar Graphs"), id = "tabset1", height = "250px",
                        tabPanel("Cell Count", plotOutput(outputId = "cc_hist")),
                        tabPanel("Blue", plotOutput(outputId = "b_hist")),
                        tabPanel("Green", plotOutput(outputId = "g_hist")),
                        tabPanel("Red", plotOutput(outputId = "r_hist")),
                        tabPanel("Far Red", plotOutput(outputId = "fr_hist"))
                    )
                    
            )
        )
    )
)

server <- function(input, output) {
    observeEvent(input$action_table, {
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
        Cell_Stat <- rename(Cell_Stat, ID = GraphSeriesNo)
        
        #####Displays what the user entered as a control.
        #output$control <- renderText(con)
        
        #If the channel box is checked then rename that column. <- WORKING
        
        if (input$brightfield == T){
            Cell_Stat <- rename(Cell_Stat,
                                cell_count = isolate(input$cc_header))
        }
        
        if (input$b == T){
            Cell_Stat <- rename(Cell_Stat,
                                b_avg = isolate(input$b_header))
        }
        if (input$g == T){
            Cell_Stat <- rename(Cell_Stat,
                                g_avg = isolate(input$g_header))
        }
        if (input$r == T){
            Cell_Stat <- rename(Cell_Stat,
                                r_avg = isolate(input$r_header))
        }
        if (input$fr == T){
            Cell_Stat <- rename(Cell_Stat,
                                fr_avg = isolate(input$fr_header))
        }
        
        ##### Second Table Output (Cell_Stat)
        # output$Cell_Stat <- DT::renderDataTable(
        #     DT::datatable(
        #         Cell_Stat, extensions = 'FixedColumns', options = list(
        #             pageLength = 3,
        #             bSort = FALSE,
        #             scrollX = TRUE,
        #             fixedColumns = TRUE
        #             
        #         )
        #     )
        # )
        
        #This section uses regular expressions to mine the image date to automate file names later on in the program. <- WORKING
        date <- str_extract(Cell_Stat$DateTime[1], "[0-9]{4}.[0-9]{2}.[0-9]{2}")%>%
            str_replace_all("-", "_")
        #This one is specifically for labeling graphs and making them look nicer.
        cleandate <- date%>%
            str_replace_all("_", "/")
        
        # output$date <- renderText(date)
        # output$cleandate <- renderText(cleandate)
        
        
        #fms=filter missing spheroids. This excludes data from any well below a certain cell count threshold. <- WORKING
        #Normally this value sits at around 1000 but may need to be tweaked. 
        #You can double check that it's filtering accurately by comparing the excluded rows with pictures of the plate.
        
        if (input$brightfield == T){
            fms <- Cell_Stat%>%
                filter(cell_count >= input$filter_missing_spheroids)
        }
        else(fms <- Cell_Stat)
        
        ####### Third Table Output (fms)
        # output$fms<- DT::renderDataTable(
        #     DT::datatable(
        #         fms, extensions = 'FixedColumns', options = list(
        #             pageLength = 3,
        #             bSort = FALSE,
        #             scrollX = TRUE,
        #             fixedColumns = TRUE
        #             
        #         )
        #     )
        # )
        
        
        #This adds the mean DMSO value for each channel, then makes a new column dividing all the other treatments by the mean DMSO value,
        #then finds the standard deviations of those values.
        con_norm <- fms  #con_norm = control normalized  <- WORKING
        
        if (input$brightfield == T){ # <- WORKING
            con_norm <- mutate(con_norm, cell_count_con = (cell_count/mean(cell_count[Compound == con])))
        }
        
        if(input$b == T){
            con_norm <- mutate(con_norm, b_con = b_avg/mean(b_avg[Compound == con]))
        }
        
        if(input$g == T){
            con_norm <- mutate(con_norm, g_con = g_avg/mean(g_avg[Compound == con]))
        }
        
        if(input$r == T){
            con_norm <- mutate(con_norm,
                               r_sub = r_avg - mean(r_avg[Compound == bg]),
                               r_sub_norm =100* r_sub/mean(r_sub[Compound == con]),
                               r_con = r_avg/mean(r_avg[Compound == con]))
        }
        
        if(input$fr == T){
            con_norm <- mutate(con_norm, fr_con = fr_avg/mean(fr_avg[Compound == con]))
        }
        else(con_norm <- con_norm)
        
        
        ######### Fourth Table Output (con_norm)
        # output$con_norm <- DT::renderDataTable(
        #     DT::datatable(
        #         con_norm, extensions = 'FixedColumns', options = list(
        #             pageLength = 3,
        #             bSort = FALSE,
        #             scrollX = TRUE,
        #             fixedColumns = TRUE
        #             
        #         )
        #     )
        # )
        
        #This makes a dataframe with means and stdevs of each treatment and control normalized treatments. <- WORKING
        sum_stat <- con_norm%>%  #sum_stat = Summary Statistics
            group_by(ID, Compound, Dose)%>%
            summarise(#Ridiculously large chuck of ifelse statements, first up is cell count.
                cell_count_mean = ifelse(input$brightfield == T, mean(cell_count, na.rm = TRUE), 0),
                cell_count_stdev = ifelse(input$brightfield == T, sd(cell_count, na.rm = TRUE), 0),
                cell_count_mean_con = ifelse(input$brightfield == T, mean(cell_count_con), 0),
                cell_count_stdev_con = ifelse(input$brightfield == T, sd(cell_count_con), 0),
                #Then on to Blue
                b_mean = ifelse(input$b == T, mean(b_avg, na.rm = TRUE), 0),
                b_stdev = ifelse(input$b == T, sd(b_avg, na.rm = TRUE), 0),
                b_mean_con = ifelse(input$b == T, mean(b_con), 0),
                b_stdev_con = ifelse(input$b == T, sd(b_con), 0),
                #Now it's Green's turn
                g_mean = ifelse(input$g == T, mean(g_avg, na.rm=TRUE), 0),
                g_stdev = ifelse(input$g == T, sd(g_avg, na.rm=TRUE), 0),
                g_mean_con = ifelse(input$g == T, mean(g_con), 0),
                g_stdev_con = ifelse(input$g == T, sd(g_con), 0),
                #And Red
                r_mean = ifelse(input$r == T, mean(r_avg, na.rm=TRUE), 0),
                r_stdev = ifelse(input$r == T, sd(r_avg, na.rm=TRUE), 0),
                r_mean_con = ifelse(input$r == T, mean(r_con), 0),
                r_stdev_con = ifelse(input$r == T, sd(r_con), 0),
                #Finally Far Red
                fr_mean = ifelse(input$fr == T, mean(fr_avg, na.rm=TRUE), 0),
                fr_stdev = ifelse(input$fr == T, sd(fr_avg, na.rm=TRUE), 0),
                fr_mean_con = ifelse(input$fr == T, mean(fr_con), 0),
                fr_stdev_con = ifelse(input$fr == T, sd(fr_con), 0)
            )
        
        
        ######### Fifth Table Output (con_norm)
        #output$sum_stat <- DT::renderDataTable(
        #     DT::datatable(
        #         sum_stat, extensions = 'FixedColumns', options = list(
        #             pageLength = 3,
        #             bSort = FALSE,
        #             scrollX = TRUE,
        #             fixedColumns = TRUE
        #             
        #         )
        #     )
        # )
        
        
        style <- theme(plot.title = element_text(face="bold", hjust = 0.5), axis.title.y = element_text(face="bold"), axis.text.x = element_text(size=9, face="bold", angle=30, hjust = 1, vjust = 1), plot.margin = margin(10,10,10,50, "pt"))
        if(input$brightfield == T){
            cell_count <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=cell_count_mean), fill="grey")+
                geom_errorbar(aes(x = factor(ID), ymin = cell_count_mean - cell_count_stdev,  ymax = cell_count_mean + cell_count_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate, " ",Plate, " Cell Count"), x=NULL, y = "Cell Count")
            output$cc_hist <- renderPlot({cell_count})
        }
        
        if(input$b == T){
            blue <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=b_mean), fill="blue")+
                geom_errorbar(aes(x = factor(ID), ymin = b_mean - b_stdev,  ymax = b_mean + b_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Blue Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$b_hist <- renderPlot({blue})
        }
        
        if(input$g == T){
            green <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=g_mean), fill="green")+
                geom_errorbar(aes(x = factor(ID), ymin = g_mean - g_stdev,  ymax = g_mean + g_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Green Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$g_hist <- renderPlot({green})
        }
        
        if(input$r == T){
            red <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=r_mean), fill="red")+
                geom_errorbar(aes(x = factor(ID), ymin = r_mean - r_stdev,  ymax = r_mean + r_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Red Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$r_hist <- renderPlot({red})
        }
        
        if(input$fr == T){
            far_red <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=fr_mean), fill="purple")+
                geom_errorbar(aes(x = factor(ID), ymin = fr_mean - fr_stdev,  ymax = fr_mean + fr_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Far Red Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$fr_hist <- renderPlot({far_red})
        }
        
    })
}

shinyApp(ui, server)