### Dashboard of asylum applicants and positive decisions. @DToshkov. First version from April 2023.
# Libraries and functions -----------------------------------------------------------------------
library(shiny)
library(cartography)
library(rgdal)
library(dplyr)
library(DT)
library(stringr)

f <- function (x) {format(x, big.mark=',')}
url <- "https://twitter.com/intent/tweet?text=Dashboard%20of%20asylum%20data%20in%20the%20EU&url=https://anonyms.shinyapps.io/asylum/"
url2 <- "https://github.com/demetriodor/asylum_dashboard"
options(scipen=999)
# Load data ---------------------------------------------------------------
load('./data/asylum2012-2022.RData') # the asylum data
#daty<-data.frame(daty)
eu<-cartography::nuts0.spdf # the map

# create a dictionary of labels for pretty printing
labels<- c(
  "aseekers" = "asylum applicants",
  "aperpop" =   "asylum applicants per capita",
  "apergdp" =   "asylum applicants per GDP",
  "firstaseekers" =   "asylum applicants (first instance)",
  "firstaperpop" =   "asylum applicants (first instance) per capita",
  "firstapergdp" = "asylum applicants (first instance) per GDP",
  
  "aseekersdec" = "total positive decisions",
  "aperpopdec" =   "total positive decisions per capita",
  "apergdpdec" =   "total positive decisions per GDP",
  "aseekersdectemp" = "total positive decisions\n(incl. temporary protection)",
  "aperpopdectemp" =   "total positive decisions\n (incl. temporary protection) per capita",
  "apergdpdectemp" =   "total positive decisions\n (incl. temporary protection) per GDP",
  "firstaseekersdec" =   "Geneva Convention status grants",
  "firstaperpopdec" =   "Geneva Convention status grants per capita",
  "firstapergdpdec" = "Geneva Convention status grants per GDP"
)

# User interface (UI) ----------------------------------------------------------------------
ui <- fluidPage(
  ## CSS modifications ---------------------------------------------------------
  tags$head(tags$style(type='text/css', ".slider-animate-button { font-size: 20pt !important; }")),
  tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
  
  ## Dashboard title ---------------------------------------------------------
  titlePanel(windowTitle = 'Asylum applications in the EU+',
             title = fluidRow(
               column(10, strong("Asylum applicants and positive decisions in the Common European Asylum System (EU+), 2011-2022,"), (" based on data from"), a(href='https://eurostat.eu/', "Eurostat")), 
               column(2, div(img(height = 0.8*100, width = 0.8*155, src = "eulogo.png", class = "pull-right")))
             )
  ),
  ## Sidebar layout ---------------------------------------------------------
  sidebarLayout(
    ## User interface panel ---------------------------------------------------------
    sidebarPanel(width=3,
                 radioButtons(inputId = "variables",
                              label = h4("Choose a variable:"),
                              choices = c("Asylum applicants" = "apps",
                                          "Positive decisions" = "decs"), 
                              selected = 'apps', inline=TRUE),
                 hr(), 
                 helpText(h4("Type of applications:")),
                 checkboxInput(inputId='atype', 
                               label='First instance applicants only', value = FALSE),
                 hr(), 
                 helpText(h4("Type of positive decisions:")),
                 radioButtons(inputId='dectype', label='',
                              c("All forms of protection" = "alldecs",
                                "Geneva Convention status grants only" = "geneva",
                                "Include temporary protection (2022)" = "temp"),
                              selected = 'alldecs'),
                 p(),
                 hr(), 
                 sliderInput(inputId="years", 
                             label=h4("Choose year:"),
                             min = 2011, max = 2022,
                             value = 2011, step=1, sep='',
                             animate=TRUE),
                 hr(), 
                 radioButtons(inputId = "adjustment",
                              label = h4("Choose adjustment:"),
                              choices = c("per capita (x 100,000)" = "aperpop",
                                          "per GDP (€ billion)" = "apergdp",
                                          'raw numbers' = 'aseekers'), 
                              selected = 'aperpop', inline=TRUE),
                 hr(),
                 # Buttons
                 downloadButton("save",  "Download the map"),
                 downloadButton("save2", "Download the barplot"), 
                 p(),
                 downloadButton("save3", "Inequality plot (pop)"),
                 downloadButton("save4", "Inequality plot (GDP)"),
                 p(),
                 hr(),
                 helpText(h4("Share and/or contribute:")),
                 actionButton("twitter_share",
                              label = "Share",
                              icon = icon("twitter"),
                              onclick = sprintf("window.open('%s')", url)),
                 
                 actionButton("github_link",
                              label = "Code",
                              icon = icon("github"),
                              onclick = sprintf("window.open('%s')", url2)),
                 
                 bookmarkButton(id = "bookmark1", label='Bookmark'),
                 hr()
    ),
    ## Structure of the main panel ---------------------------------------------------------
    mainPanel(width=9,
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("mymap", width = "100%", height = 700), plotOutput('barplot', width = "100%", height = 700))
              ),
              hr(),
              htmlOutput("summary"),
              hr(),
              htmlOutput("notes"),
              hr(),
              fluidRow(
                splitLayout(cellWidths = c("50%", "50%"), plotOutput("inequalityplot.pop", width = "100%", height = 700), plotOutput("inequalityplot.gdp", width = "100%", height = 700))
              ),
              hr(),
              htmlOutput("notes2"),
              hr(),
              DT::dataTableOutput(outputId = 'asylum_table')
    )  
  )
)
# Server side -----------------------------------------------------------------------
server <- function(input, output, session) {
  setBookmarkExclude(c("bookmark1"))
  observeEvent(input$bookmark1, {session$doBookmark()})
  # Get the user inputs in variables ----------------------------------------
  year_u <- reactive({input$years})
  adj_u <- reactive({input$adjustment})
  vartype <- reactive({input$variables})
  atype <- reactive ({input$atype})
  dectype <- reactive ({input$dectype})
  #dectypetemp <- reactive ({input$dectypetemp})
  
  # Subset the asylum data based on the user input (year) ----------------------------------------
  temp_data <- reactive ({
    m <- daty %>% 
      dplyr::filter(year==year_u()) %>% 
      dplyr::select (id, country, aseekers, aperpop, apergdp, firstaseekers, firstaperpop, firstapergdp,
                     aseekersdec, aperpopdec, apergdpdec, aseekersdectemp, aperpopdectemp, apergdpdectemp, 
                     firstaseekersdec, firstaperpopdec, firstapergdpdec,
                     aseekers.inequality.pop, firstaseekers.inequality.pop, aseekersdec.inequality.pop, aseekersdectemp.inequality.pop, firstaseekersdec.inequality.pop,
                     aseekers.inequality.gdp, firstaseekers.inequality.gdp, aseekersdec.inequality.gdp, aseekersdectemp.inequality.gdp, firstaseekersdec.inequality.gdp) %>% 
      data.frame()
    m
  })
  # Plot I: Map -------------------------------------------------------------
  plotInput <- function(){
    variable_a = ifelse (atype()==FALSE, adj_u(), paste0("first", adj_u()))
    variable_d = ifelse (dectype()=='alldecs', paste0(adj_u(), 'dec'), 
                         ifelse(dectype()=='geneva', 
                                paste0("first", adj_u(), 'dec'), 
                                paste0(adj_u(), 'dectemp')))
    variable = ifelse (vartype()=='apps', variable_a, variable_d)
    
    xmax = max(daty[,variable])
    breaks = round(c(as.vector(quantile(daty[,variable], probs=seq(0,1,0.05), na.rm=T))),0)
    
    col.label.main = ifelse (vartype()=='apps', 'red.pal','blue.pal')
    col.label.opp = ifelse (vartype()=='apps', 'blue.pal','red.pal')
    cols <- rev(carto.pal(pal1 = col.label.main, n1 = 20, pal2 = col.label.opp, n2 = 0, transparency=FALSE) )[-1]
    
    par(mar = c(0,0,4,0))
    plot(eu, border = NA, col = NA, bg = NA, main = paste0(str_to_sentence(labels[variable]), " in the EU+\n", year_u()))
    choroLayer(spdf = eu, # SpatialPolygonsDataFrame of the regions
               df =  temp_data(),
               var = variable, #variable
               breaks = breaks, # list of breaks
               col = cols, # colors 
               border = "grey80", # color of the polygons borders
               lwd = 0.5, # width of the borders
               legend.pos = c(5650310, 3093962), # position of the legend
               legend.title.txt = "", # title of the legend
               legend.values.rnd = 0, # number of decimal in the legend values
               legend.title.cex=1.,
               legend.values.cex=1,
               legend.border='grey80',
               legend.horiz=FALSE,
               add = TRUE) # add the layer to the current plot
    
  }
  
  # Plot II: Barplot --------------------------------------------------------
  plotInput2 <- function(){
    variable_a = ifelse (atype()==FALSE, adj_u(), paste0("first", adj_u()))
    
    variable_d = ifelse (dectype()=='alldecs', paste0(adj_u(), 'dec'), 
                         ifelse(dectype()=='geneva', 
                                paste0("first", adj_u(), 'dec'), 
                                paste0(adj_u(), 'dectemp')))
    
    
    variable = ifelse (vartype()=='apps', variable_a, variable_d)
    
    col = ifelse (vartype()=='apps', '#E60D0C', '#307BA5')
    
    temp <- temp_data() 
    temp <- temp[order(temp[variable], decreasing = TRUE),]
    x.max = max(temp[,variable])
    
    par(oma=c(0,0,0,0), # size of the outer margins in lines of text
        mar=c(3,6,5,1), # number of lines of margin to be specified on the four sides of the plot 
        bty='n', # no box
        cex = 1, # magnification of text and symbols
        xpd = FALSE, # clipping of plotting to the figure region
        ann = FALSE
    )
    
    plot(NULL, ylim=c(1, dim(temp)[1]), xlim=c(0, x.max), yaxt = 'n', xaxt = 'n') 
    
    title(main=paste0('Number of ', labels[variable] , " in the EU+\n", "during ", year_u()), font=3, cex.lab=1.25)
    
    seqs <- seq(0,x.max, ifelse(x.max>500000, 100000, ifelse(x.max>50000, 5000, ifelse(x.max>5000, 1000, ifelse(x.max>500, 100, 20)))))
    
    axis (1, 
          line = 0, # position
          tck = -0.01,
          lwd = 1,
          col = 'grey', # the actual axis (line) 
          col.axis = 'black', # colors of the actual labels
          cex.axis = 1, 
          font=2, # font type (bold)
          at=seqs, # where to put labels  
          labels=f(seqs), # text of labels 
          las=1 # orientation of the labels
    )
    
    abline(v=seqs, col='grey', lwd=1)
    
    rect (ybottom=(1:dim(temp)[1])-0.42, ytop=(1:dim(temp)[1]) + 0.42, xleft = rep(0,dim(temp)[1]), xright = temp[,variable], col= col, border ='grey80', lwd=1.5)
    
    par(xpd = TRUE)
    for (i in 1:dim(temp)[1]){
      text (temp$country[i], y = i, x = -0, pos=2, font=1, cex=1.2, col='black')
    }
  }
  # Plot III: Inequality per population -------------------------------------
  plotInput3 <- function(){
    
    variable_c = ifelse (atype()==FALSE, paste0('aseekers.inequality.pop'), paste0('firstaseekers.inequality.pop'))
    
    variable_d = ifelse (dectype()=='alldecs', paste0('aseekersdec.inequality.pop'), 
                         ifelse(dectype()=='geneva', 
                                paste0('firstaseekersdec.inequality.pop'), 
                                paste0('aseekersdectemp.inequality.pop')))
    
    
    
    variable2 = ifelse (vartype()=='apps', variable_c, variable_d)
    
    main_c = ifelse (atype()==FALSE, paste0('asylum applicants'), paste0('asylum applicants (first instance)'))
    
    main_d = ifelse (dectype()=='alldecs', paste0('positive decisions'), 
                     ifelse(dectype()=='geneva', 
                            paste0('Geneva Convention status grants'), 
                            paste0('positive decisions\n(incl. temp. protection)')))
    
    
    main2 = ifelse (vartype()=='apps', main_c, main_d)
    
    temp <- temp_data()
    temp <- temp[order(temp[variable2], decreasing = TRUE),]
    x.min = floor(min(temp[,variable2]))
    x.max = ceiling(max(temp[,variable2]))
    
    par(oma=c(0,0,0,0), # size of the outer margins in lines of text
        mar=c(3,6,5,1), # number of lines of margin to be specified on the four sides of the plot 
        bty='n', # no box
        cex = 1, # magnification of text and symbols
        xpd = FALSE, # clipping of plotting to the figure region
        ann = FALSE
    )
    
    plot(NULL, ylim=c(1, dim(temp)[1]), xlim=c(x.min, x.max), yaxt = 'n', xaxt = 'n') 
    
    title(main=paste0('Inequality in the distribution of ', main2, ' in the EU+\nduring ', year_u(), ", relative to population"), font=3, cex.lab=1.25)
    
    seqs <- seq(x.min, x.max,1)
    axis (1, 
          line = 0, # position
          tck = -0.01,
          lwd = 1,
          col = 'grey', # the actual axis (line) 
          col.axis = 'black', # colors of the actual labels
          cex.axis = 1, 
          font=2, # font type (bold)
          at=seqs, # where to put labels  
          labels=seqs , # text of labels 
          las=1 # orientation of the labels
    )
    abline(v=seqs, col='grey', lwd=1)
    abline(v=0, col='black', lwd=3)
    
    points (y=1:dim(temp)[1], x = temp[,variable2], col= ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), bg=ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), cex=2, pch=21)
    segments (y0=1:dim(temp)[1], x0 = temp[,variable2], x1 = rep(0, dim(temp)[1]), col= ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), lwd=2)
    
    par(xpd = TRUE)
    for (i in 1:dim(temp)[1]){text (temp$country[i], y = i, x = x.min, pos=2, font=1, cex=1.2, col='black')}
  }
  
  # Plot IV: Inequality per GDP -------------------------------------
  plotInput4 <- function(){
    
    variable_c = ifelse (atype()==FALSE, paste0('aseekers.inequality.gdp'), paste0('firstaseekers.inequality.gdp'))
    variable_d = ifelse (dectype()=='alldecs', paste0('aseekersdec.inequality.gdp'), 
                         ifelse(dectype()=='geneva', 
                                paste0('firstaseekersdec.inequality.gdp'), 
                                paste0('aseekersdectemp.inequality.gdp')))
    variable2 = ifelse (vartype()=='apps', variable_c, variable_d)
    
    main_c = ifelse (atype()==FALSE, paste0('asylum applicants'), paste0('asylum applicants (first instance)'))
    main_d = ifelse (dectype()=='alldecs', paste0('positive decisions'), 
                     ifelse(dectype()=='geneva', 
                            paste0('Geneva Convention status grants'), 
                            paste0('positive decisions\n(incl. temp. protection)')))
    
    main2 = ifelse (vartype()=='apps', main_c, main_d)
    
    temp <- temp_data()
    temp <- temp[order(temp[variable2], decreasing = TRUE),]
    x.min = floor(min(temp[,variable2]))
    x.max = ceiling(max(temp[,variable2]))
    
    par(oma=c(0,0,0,0), # size of the outer margins in lines of text
        mar=c(3,6,5,1), # number of lines of margin to be specified on the four sides of the plot 
        bty='n', # no box
        cex = 1, # magnification of text and symbols
        xpd = FALSE, # clipping of plotting to the figure region
        ann = FALSE
    )
    
    plot(NULL, ylim=c(1, dim(temp)[1]), xlim=c(x.min, x.max), yaxt = 'n', xaxt = 'n') 
    
    title(main=paste0('Inequality in the distribution of ', main2, ' in the EU+\nduring ', year_u(),", relative to GDP"), font=3, cex.lab=1.25)
    
    seqs <- seq(x.min, x.max,1)
    axis (1, 
          line = 0, # position
          tck = -0.01,
          lwd = 1,
          col = 'grey', # the actual axis (line) 
          col.axis = 'black', # colors of the actual labels
          cex.axis = 1, 
          font=2, # font type (bold)
          at=seqs, # where to put labels  
          labels=seqs , # text of labels 
          las=1 # orientation of the labels
    )
    abline(v=seqs, col='grey', lwd=1)
    abline(v=0, col='black', lwd=3)
    
    points (y=1:dim(temp)[1], x = temp[,variable2], col= ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), bg= ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), cex=2, pch=21)
    segments (y0=1:dim(temp)[1], x0 = temp[,variable2], x1 = rep(0, dim(temp)[1]), col= ifelse(temp[,variable2]>0, '#307BA5', '#E60D0C'), lwd=2)
    
    par(xpd = TRUE)
    for (i in 1:dim(temp)[1]){text (temp$country[i], y = i, x = x.min, pos=2, font=1, cex=1.2, col='black')}
  }
  
  # Print plots I (map), II (barplot), III and IV  ------------------------------------------------------
  output$mymap <- renderPlot({print(plotInput())})
  output$barplot <- renderPlot({print(plotInput2())})
  output$inequalityplot.pop <- renderPlot({print(plotInput3())})
  output$inequalityplot.gdp <- renderPlot({print(plotInput4())})
  
  # Text: summary -----------------------------------------------------------
  output$summary <- renderText({ 
    paste0('<strong>Summary:</strong> There were a total of ', f(sum(temp_data()$aseekers, na.rm=TRUE)), ' asylum applicants in the ', dim(temp_data())[1], ' countries of the Common European Asylum Area in ', year_u(), '. ', 
           temp_data() %>% arrange (aseekers) %>% select (country) %>% tail(1), ' registered the greatest number of applicants: ', f(temp_data() %>% arrange (aseekers) %>% select (aseekers) %>% tail(1)), '. 
        Adjusted for population, ', 
        temp_data() %>% arrange (aperpop) %>% select (country) %>% tail(1), ' received the most, with ', f(temp_data() %>% arrange (aperpop) %>% select (aperpop) %>% tail(1)), ' applicants per 100,000 people. 
        Adjusted for wealth, ', 
        temp_data() %>% arrange (apergdp) %>% select (country) %>% tail(1), ' received the most, with ', f(temp_data() %>% arrange (apergdp) %>% select (apergdp) %>% tail(1)), ' applicants per € billion of GDP. ',
        'In ', year_u(),  ', a total of ', f(sum(temp_data()$aseekersdec, na.rm=TRUE)), ' positive decisions were granted in the Common European Asylum Area. During the year, ',
        f(sum(temp_data()$firstaseekersdec, na.rm=TRUE)),  ' applicants received refugee status accoriding to the Geneva Convention. ', 
        temp_data() %>% arrange (aseekersdec) %>% select (country) %>% tail(1), ' granted the greatest number of positive decisions: ', f(temp_data() %>% arrange (aseekersdec) %>% select (aseekersdec) %>% tail(1)) ,
        '. Adjusted for population, ', temp_data() %>% arrange (aperpopdec) %>% select (country) %>% tail(1),
        ' granted the most, with ', f(temp_data() %>% arrange (aperpopdec) %>% select (aperpopdec) %>% tail(1)), ' positive decisions per 100,000 people. 
        Adjusted for wealth, ', temp_data() %>% arrange (apergdpdec) %>% select (country) %>% tail(1), ' granted the most, with ', f(temp_data() %>% arrange (apergdpdec) %>% select (apergdpdec) %>% tail(1)), ' positive decisions per € billion of GDP.')
  })
  
  # Text: Notes I -----------------------------------------------------------
  output$notes <- renderText({ 
    paste('<strong>Notes:</strong> This dashboard shows the distribution of asylum applicants and positive decisions in the Common European Asylum Space, which 
        covers the member states of the European Union, Switzerland, Liechtenstein, Norway and Iceland (EU+), from 2011 until 2022. Asylum applicants are
        <i>"third-country national or stateless person having submitted an application for international protection or having been included in such an 
        application as a family member during the reference period."</i> By default, first-time and subsequent applicants are included: tick the checkbox in the control panel on the left 
        to include only first-time applicants. 
        The data comes from the Eurostat <a target="_blank" href="https://ec.europa.eu/eurostat/databrowser/view/MIGR_ASYAPPCTZA__custom_316637/bookmark/table/">migr_asyappctza</a> table.
        Positive decisions refer to first instance decisions on asylum applications granting refugee (Geneva Convention) status, subsidiary protection or authorisation 
        to stay for humanitarian reasons. To include <i>only</i> Geneva Convention status decisions, choose the corresponding option from the control panel on the left.
        You can also select the option to include temporary protection in the total number of positive decisions, which is offered primarily to Ukranian citizens in the aftermath of the war with Russia (data is available only for 2022).
        The data comes from the Eurostat <a target="_blank" href="https://ec.europa.eu/eurostat/databrowser/view/MIGR_ASYDCFSTA__custom_1392414/default/table">migr_asydcfsta</a> and 
        <a target="_blank" href="https://ec.europa.eu/eurostat/databrowser/view/migr_asytpfm/default/table">migr_asytpfm</a> tables.
        The number of asylum applicants and positive decisions can be adjusted for population and wealth of the receiving countries. The population adjustment calculates the numbers for 100,000 inhabinants. 
        The wealth adjustment calculates the numbers per billion euros of gross domestic product at market prices (GDP). The colors of the choropleth map correspond to the 20 5% quantiles of the observed distribution of the relevant vairable for the entire period 2011-2022. 
        The range of the x-axis of the barplot is <i>not</i> kept constant over different years.')
  })
  
  # Text: Notes II -----------------------------------------------------------
  output$notes2 <- renderText({ 
    paste('<strong>Notes:</strong> Inequality in the distribution of asylum applicants and decisions is calculated in the following way: If a country has 10% of the population (or 10% of the GDP) of all EU+ countries,
        it is expected to receive 10% of the asylum applicants and to grant 10% of the positive decisions in the EU+ area during a year. If it does so, it gets a value of `0` in the plot. If it
        does more than its expected share, it gets a positive value. If it does less than its expected share, it gets a negative value. The vertical gridlines at +1 and -1 indicate a where a country that does
        twice as much and twice as little (relative to its expected share) would be. The plot on the left takes into account population shares, and the plot on the right takes into account GDP shares. 
        In technical terms, the inequality measure is calculated as the base-2 log of the ratio of the country`s share of asylum applicants (or positive decisions) to the country`s population (or GDP) share. 
        (1 has been been added the numbers of applicants and positive decisions in order to avoid zeros in the dataset, for which the log function is undefined). 
        For example, in 2022 Austria registered 108,780 asylum applicants, which makes for 11% of the 992,005 applicants received in the EU+ countries during the year. At the same time, Austria`s 
        population is 8,978,929 people, which makes for 1.95% of the 461,029,891 people living in the EU+ countries. Hence, Austria has registered more than five times its `fair` share of asylum applicants 
        relative to its population. (The base-2 log of 11/1.95 is 2.5, which is where Austria`s dot is on the left plot for 2022). At the same time, in 2022 Denmark granted protection to 515 asylum applicants, which makes for 0.16% of all positive decisions in the EU+ countries. 
        At the same time, Denmark`s share of the EU+ GDP is 2.19%. Hence, Denmark has granted 13 times less positive asylum decisions than its `fair` share relative to its wealth. 
        (The base-2 log of 0.16/2.19 is -3.77, which is where Denmark is on the right plot for 2022).
        <br/>
        <br/>
        This dashboard has been put together by <a target="_blank" href="https://twitter.com/DToshkov">Dimiter Toshkov</a> with <code>R</code>, <code>RStudio</code> and <code>Shiny</code>. 
        Data is accessed via the <code>eurostat</code> package. Last updated: April 2023. 
        Send feedback and suggestions <a target="_blank" href = "mailto:demetriodor@gmail.com?subject = Feedback&body = Message">here</a>.')
  })
  
  
  # Download handlers -------------------------------------------------------
  output$save <- downloadHandler(
    file = "asylum_map.png", 
    content = function(file) {
      png(file = file, width=900)
      plotInput()
      dev.off()
    })
  
  output$save2 <- downloadHandler(
    file = "asylum_barplot.png" , 
    content = function(file) {
      png(file = file, width=900, height=600)
      plotInput2()
      dev.off()
    })    
  
  output$save3 <- downloadHandler(
    file = "asylum_inequality_pop.png" , 
    content = function(file) {
      png(file = file, width=900, height=600)
      plotInput3()
      dev.off()
    })    
  
  output$save4 <- downloadHandler(
    file = "asylum_inequality_gdp.png" , 
    content = function(file) {
      png(file = file, width=900, height=600)
      plotInput4()
      dev.off()
    })    
  
  # Table with the data: compose and run -----------------------------------------------------
  asylum_table <- reactive({
    daty %>% 
      dplyr::filter(year==year_u()) %>% 
      dplyr::rename( Country = country, Year = year,
                     `Asylum applicants` = aseekers,
                     `Asylum applicants per capita` = aperpop,
                     `Asylum applicants per GDP` = apergdp,
                     `Asylum applicants (first instance)` = firstaseekers,
                     `Asylum applicants (first instance) per capita` = firstaperpop,
                     `Asylum applicants (first instance) per GDP` = firstapergdp,
                     
                     `Total positive decisions` = aseekersdec,
                     `Total positive decisions per capita` = aperpopdec,
                     `Total positive decisions per GDP` = apergdpdec,
                     `Geneva Convention status grants` = firstaseekersdec,
                     `Geneva Convention status grants per capita` = firstaperpopdec,
                     `Geneva Convention status grants per GDP` = firstapergdpdec
      ) %>%
      dplyr::select (id, Country, Year, `Asylum applicants`, `Asylum applicants per capita`, `Asylum applicants per GDP`, 
                     `Asylum applicants (first instance)`, `Asylum applicants (first instance) per capita`, `Asylum applicants (first instance) per GDP`,
                     `Total positive decisions`, `Total positive decisions per capita`, `Total positive decisions per GDP`,
                     `Geneva Convention status grants`, `Geneva Convention status grants per capita`, `Geneva Convention status grants per GDP`) 
  })
  
  output$asylum_table = DT::renderDataTable(
    asylum_table(), extensions = 'Buttons', 
    options = list(pageLength = 32,
                   searching = FALSE,
                   dom = 'Bfrtip',
                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print')))
  
}

# Run the app -------------------------------------------------------------
enableBookmarking(store = "server")
shinyApp(ui, server)
