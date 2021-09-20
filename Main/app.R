library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)

ui <- dashboardPage(
    dashboardHeader(title = "GraphMaster 9000"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "input_tab", icon = icon("dashboard")),
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
                        ),
                        #This is to look at the table "no_con".
                        # fluidRow(
                        #     box(
                        #         dataTableOutput(outputId = "table2")
                        #     )
                        # )
                    ),
            ),
            tabItem(tabName = "graph_tab",
                fluidPage(
                    fluidRow(
                        tabBox(
                            title = strong("Bar Graphs"), id = "tabset1",
                            tabPanel("Cell Count", plotOutput(outputId = "cc_hist")),
                            tabPanel("Blue", plotOutput(outputId = "b_hist")),
                            tabPanel("Green", plotOutput(outputId = "g_hist")),
                            tabPanel("Red", 
                                     h5(downloadButton("download1", "Download"), align = "right"), 
                                     plotOutput(outputId = "r_hist")),
                            tabPanel("Far Red", plotOutput(outputId = "fr_hist"))#,
                            #tabPanel(downloadButton("download1", "Download"))
                        ),
                        tabBox(
                            title = strong("Control Normalized Bar Graphs"), id = "tabset2",
                            tabPanel("Cell Count", plotOutput(outputId = "cc_hist_con")),
                            tabPanel("Blue", plotOutput(outputId = "b_hist_con")),
                            tabPanel("Green", plotOutput(outputId = "g_hist_con")),
                            tabPanel("Red", plotOutput(outputId = "r_hist_con")),
                            tabPanel("Far Red", plotOutput(outputId = "fr_hist_con")),
                            tabPanel(downloadButton("download2", "Download"))
                        )
                        
                    ),
                    fluidRow(
                        tabBox(
                            title = strong("Boring Graphs"), id = "tabset3",
                            tabPanel("Cell Count", plotOutput(outputId = "cc_hist_3")),
                            tabPanel("Blue", plotOutput(outputId = "b_hist_3")),
                            tabPanel("Green", plotOutput(outputId = "g_hist_3")),
                            tabPanel("Red", plotOutput(outputId = "r_hist_3")),
                            tabPanel("Far Red", plotOutput(outputId = "fr_hist_3")),
                            tabPanel(downloadButton("download3", "Download"))
                        )
                    )
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
        
        #If the channel box is checked then rename that column. <- WORKING
        Cell_Stat <- rename(Cell_Stat, ID = GraphSeriesNo,
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
        
        #fms=filter missing spheroids. This excludes data from any well below a certain cell count threshold. <- WORKING
        #Normally this value sits at around 1000 but may need to be tweaked. 
        #You can double check that it's filtering accurately by comparing the excluded rows with pictures of the plate.
        
        if (input$brightfield == T){
            fms <- Cell_Stat%>%
                filter(cell_count >= input$filter_missing_spheroids)
        }
        else(fms <- Cell_Stat)
        
        #This adds the mean DMSO value for each channel, then makes a new column dividing all the other treatments by the mean DMSO value,
        #then finds the standard deviations of those values.
        con_norm <- fms #con_norm = control normalized  <- WORKING

        if (input$brightfield == T){ # <- WORKING
            con_norm <- mutate(con_norm, 
                               cell_count_sub = cell_count - mean(cell_count[Compound == bg]),
                               cell_count_sub_norm =100* cell_count_sub/mean(cell_count_sub[Compound == con]),
                               cell_count_con = (cell_count/mean(cell_count[Compound == con])))
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
            group_by(ID, Compound, Dose)%>%
            summarise(#Ridiculously large chuck of ifelse statements, first up is cell count.
                cell_count_mean = ifelse(input$brightfield == T, mean(cell_count, na.rm = TRUE), 0),
                cell_count_stdev = ifelse(input$brightfield == T, sd(cell_count, na.rm = TRUE), 0),
                cell_count_mean_con = ifelse(input$brightfield == T, mean(cell_count_con), 0),
                cell_count_stdev_con = ifelse(input$brightfield == T, sd(cell_count_con), 0),
                cell_count_mean_sub = ifelse(input$brightfield == T, mean(cell_count_sub_norm), 0), 
                cell_count_stdev_sub = ifelse(input$brightfield == T, sd(cell_count_sub_norm), 0),
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
        no_con <- sum_stat%>% #A dataframe without the control treatment
            filter(Compound != con)
        
        #####Start of "boring graphs" section.
        #Need to automate the dose column
        sum_stat$Dose = c(1e-9,0,30,55,80,105,130,155,180,195,390,0)*1e-6
        max_dose <- max(sum_stat$Dose)
        filter_sum_stat <- sum_stat%>%
            filter(Dose != 0)

        #Need to ask for user input for these ones.
        lowerxlim = -5
        upperxlim = -3
#       logp3 = log10(newc)
        lx = seq(.000001,.001, by = 0.00001) #List x

        # output$table2 <- DT::renderDataTable(
        #     DT::datatable(
        #         no_con, extensions = 'FixedColumns', options = list(
        #             pageLength = 3,
        #             bSort = FALSE,
        #             scrollX = TRUE,
        #             fixedColumns = TRUE
        #
        #         )
        #     )
        # )
        
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
            cell_count_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=cell_count_mean), fill="grey")+
                geom_errorbar(aes(x = factor(ID), ymin = cell_count_mean - cell_count_stdev,  ymax = cell_count_mean + cell_count_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate, " ",Plate, " Cell Count"), x=NULL, y = "Cell Count")
            output$cc_hist <- renderPlot({cell_count_graph})
            if(con != ""){
                cell_count_con_graph <- ggplot(data = no_con)+
                    geom_col(mapping = aes(x=factor(ID), y = cell_count_mean_con), fill = "grey")+
                    geom_errorbar(aes(x=factor(ID), ymin = cell_count_mean_con - cell_count_stdev_con, ymax = cell_count_mean_con + cell_count_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = no_con$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," Normalized Cell Count"), x = NULL,y = paste0(con, " Normalzed Cell Count"))
                output$cc_hist_con <- renderPlot({cell_count_con_graph})
                if(bg != ""){
                    cell_count_bg_graph <- ggplot(data = filter_sum_stat)+
                        geom_point(filter_sum_stat,
                                   mapping = aes(x = log10(Dose), y = cell_count_mean_sub),
                                   size = 3)+
                        geom_errorbar(filter_sum_stat,
                                      mapping = aes(x = log10(Dose), ymin = cell_count_mean_sub - cell_count_stdev_sub, ymax = cell_count_mean_sub + cell_count_stdev_sub),
                                      width = 0.04)+
                        geom_smooth(filter_sum_stat,
                                    mapping = aes(x = log10(Dose), y = cell_count_mean_sub),
                                    se = F, color = "black")+
                        scale_x_continuous(labels = scales::math_format())+
                        coord_cartesian(xlim = c(lowerxlim,upperxlim),
                                        ylim = c(0, 100),
                                        expand = FALSE)+
                        labs(title = paste0(cleandate," ",Plate," Cell Count Mean"),
                             x="Mefenamanic Acid Dose",
                             y = "Cell Count")+
                        theme_classic()+
                        style2
                    output$cc_hist_3 <- renderPlot({cell_count_bg_graph})
                    
                }
            }
        }
        
        if(input$b == T){
            blue_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=b_mean), fill="blue")+
                geom_errorbar(aes(x = factor(ID), ymin = b_mean - b_stdev,  ymax = b_mean + b_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Blue Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$b_hist <- renderPlot({blue_graph})
            if(con != ""){
                b_con_graph <- ggplot(data = no_con)+
                    geom_col(mapping = aes(x=factor(ID), y = b_mean_con), fill = "blue")+
                    geom_errorbar(aes(x=factor(ID), ymin = b_mean_con - b_stdev_con, ymax = b_mean_con + b_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = no_con$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," Normalized Blue Channel Mean Intensity"), x = NULL, y = paste0(con, " Normalzed Mean Intensity"))
                output$b_hist_con <- renderPlot({b_con_graph})
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
                        labs(title = paste0(cleandate," ",Plate," Blue Channel Mean Intensity"),
                             x="Mefenamanic Acid Dose",
                             y = "Blue Fluorescence")+
                        theme_classic()+
                        style2
                    output$b_hist_3 <- renderPlot({b_bg_graph})
                    
                }
            }
        }
        
        if(input$g == T){
            green_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=g_mean), fill="green")+
                geom_errorbar(aes(x = factor(ID), ymin = g_mean - g_stdev,  ymax = g_mean + g_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Green Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$g_hist <- renderPlot({green_graph})
            if(con != ""){
                g_con_graph <- ggplot(data = no_con)+
                    geom_col(mapping = aes(x=factor(ID), y = g_mean_con), fill = "green")+
                    geom_errorbar(aes(x=factor(ID), ymin = g_mean_con - g_stdev_con, ymax = g_mean_con + g_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = no_con$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," Normalized Green Channel Mean Intensity"), x = NULL, y = paste0(con, " Normalzed Mean Intensity"))
                output$g_hist_con <- renderPlot({g_con_graph})
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
                        labs(title = paste0(cleandate," ",Plate," Green Channel Mean Intensity"),
                             x="Mefenamanic Acid Dose",
                             y = "Rhodamine123 Fluorescence")+
                        theme_classic()+
                        style2
                    output$g_hist_3 <- renderPlot({g_bg_graph})
                    
                }
            }
        }
        
        if(input$r == T){
            red_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=r_mean), fill="red")+
                geom_errorbar(aes(x = factor(ID), ymin = r_mean - r_stdev,  ymax = r_mean + r_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Red Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$r_hist <- renderPlot({red_graph})
            #Download button for the graph
            output$download1 <- downloadHandler(
                filename = function() {
                    paste0(date,"_", Plate,"_Red_Graph.svg")
                },
                content = function(file) {
                    ggsave(file, plot = red_graph, width = 8, height = 5, device = svg)
                }
            )
            if(con != ""){
                r_con_graph <- ggplot(data = no_con)+
                    geom_col(mapping = aes(x=factor(ID), y = r_mean_con), fill = "red")+
                    geom_errorbar(aes(x=factor(ID), ymin = r_mean_con - r_stdev_con, ymax = r_mean_con + r_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = no_con$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," Normalized Red Channel Mean Intensity"), x = NULL, y = paste0(con, " Normalzed Mean Intensity"))
                output$r_hist_con <- renderPlot({r_con_graph})
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
                        labs(title = paste0(cleandate," ",Plate," Red Channel Mean Intensity"),
                             x="Mefenamanic Acid Dose",
                             y = "MitoTracker Fluorescence")+
                        theme_classic()+
                        style2
                    output$r_hist_3 <- renderPlot({r_bg_graph})
                    
                }
            }
        }
        
        if(input$fr == T){
            far_red_graph <- ggplot(data=sum_stat)+
                geom_col(mapping=aes(x=factor(ID), y=fr_mean), fill="purple")+
                geom_errorbar(aes(x = factor(ID), ymin = fr_mean - fr_stdev,  ymax = fr_mean + fr_stdev), width = 0.2)+
                scale_x_discrete(labels = sum_stat$Compound)+
                style + labs(title = paste0(cleandate," ",Plate," Far Red Channel Mean Intensity"), x = NULL , y = "Mean Intensity")
            output$fr_hist <- renderPlot({far_red_graph})
            if(con != ""){
                fr_con_graph <- ggplot(data = no_con)+
                    geom_col(mapping = aes(x=factor(ID), y = fr_mean_con), fill = "purple")+
                    geom_errorbar(aes(x=factor(ID), ymin = fr_mean_con - fr_stdev_con, ymax = fr_mean_con + fr_stdev_con), width = 0.2)+
                    scale_x_discrete(labels = no_con$Compound)+
                    style + labs(title = paste0(cleandate, " ",Plate," Normalized Far Red Channel Mean Intensity"), x = NULL,y = paste0(con, " Normalzed Mean Intensity"))
                output$fr_hist_con <- renderPlot({fr_con_graph})
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
                        labs(title = paste0(cleandate," ",Plate," Far Red Channel Mean Intensity"),
                             x="Mefenamanic Acid Dose",
                             y = "CellRox Fluorescence")+
                        theme_classic()+
                        style2
                    output$fr_hist_3 <- renderPlot({fr_bg_graph})
                    
                }
            }
        }
        
            
            #####Experimenting with zip files
        
    # graph_list <- list(red_graph, blue_graph)
    # 
    # 
    #     output$download1 <- downloadHandler(
    #         filename = function() {
    #             paste0("Extract.zip")
    #         },
    #         content = function(file) {
    #             owd <- setwd(tempdir())
    #             on.exit(setwd(owd))
    #             files <- NULL;
    #             for (i in 1:length(graph_list)){
    #                 fileName <- paste("graph_0",i,".png",sep = "")
    #                 ggsave(file, plot = graph_list[[i]], width = 8, height = 5, device = "png")
    #                 files <- c(fileName, files)
    #                 #<- c(ggsave(file, plot = graph_list[[i]], width = 8, height = 5, device = "png"), files)
    #                 #c(paste("output_file/", graph_list[[i]], sep = ""), files)
    #             }
    #             system2("zip", args=(paste(file,files,sep=" ")))
    #         }
    #         
            # content = function(file){
            #     #go to a temp dir to avoid permission issues
            #     
            #     #loop through the sheets
            #     for (i in 1:input$sheet){
            #         #write each sheet to a csv file, save the name
            #         fileName <- paste(input$text,"_0",i,".csv",sep = "")
            #         write.table(data()$wb[i],fileName,sep = ';', row.names = F, col.names = T)
            #         files <- c(fileName,files)
            #     }
            #     #create the zip file
            #     zip(file,files)
            # }
        
    })
}

shinyApp(ui, server)