library(reshape2)
library(shiny)
library(ggplot2)

life <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2/API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv', skip = 4)
continent <- read.csv('API_SP.DYN.LE00.IN_DS2_en_csv_v2/Metadata_Country_API_SP.DYN.LE00.IN_DS2_en_csv_v2.csv')
fertility <- read.csv('API_SP.DYN.TFRT.IN_DS2_en_csv_v2/API_SP.DYN.TFRT.IN_DS2_en_csv_v2.csv', skip = 4)
population <- read.csv('API_SP.POP.TOTL_DS2_en_csv_v2.csv')

life <- merge(x = life, y = continent[,1:2], by = 'Country.Code', all.x = TRUE)
fertility <- merge(x = fertility, y = continent[,1:2], by = 'Country.Code', all.x = TRUE)
population <- merge(x = population, y = continent[,1:2], by = 'Country.Code', all.x = TRUE)

life <- life[, -which((names(life) %in% c('X', 'Indicator.Name', 'Indicator.Code', 'X2015', 'X2016', 'Country.Code')))]
fertility <- fertility[, -which((names(fertility) %in% c('X', 'Indicator.Name', 'Indicator.Code', 'X2015', 'X2016', 'Country.Code')))]
population <- population[, -which((names(population) %in% c('X', 'Indicator.Name', 'Indicator.Code', 'X2015', 'X2016', 'Country.Code')))]

names(life)[2:56] <- substring(names(life)[2:56], 2)
names(fertility)[2:56] <- substring(names(fertility)[2:56], 2)
names(population)[2:56] <- substring(names(population)[2:56], 2)

life_reshape <- melt(life, id = c('Country.Name', 'Region'))
fertility_reshape <- melt(fertility, id = c('Country.Name', 'Region'))
population_reshape <- melt(population, id = c('Country.Name', 'Region'))

names(life_reshape)[3] <- 'year'
names(fertility_reshape)[3] <- 'year'
names(population_reshape)[3] <- 'year'
names(life_reshape)[4] <- 'life_expectancy'
names(fertility_reshape)[4] <- 'fertility_rate'
names(population_reshape)[4] <- 'population'

joined_data <- merge(life_reshape, fertility_reshape, by = c("Country.Name", "Region", "year"))
joined_data <- merge(joined_data, population_reshape, by = c("Country.Name", "Region", "year"))
joined_data[joined_data==""] <- NA
joined_data <- na.omit(joined_data)


ui <- fluidPage(
  headerPanel("Homework2"),
  sidebarPanel(
    sliderInput(inputId = 'Year', label = 'Select Year', value = 1960, min = 1960, 
                max = 2014, step = 1, animate = animationOptions(interval = 600)),
    sliderInput(inputId = 'dotSize', label = 'Change dot size', 1,5,1),
    checkboxGroupInput("Region", label = "Select Region", choices = c("South Asia","Europe & Central Asia","Middle East & North Africa East Asia & Pacific",
                                                                      "Sub-Saharan Africa","Latin America & Caribbean","North America")), width = 4
  ),
  mainPanel(plotOutput('plot', hover = "plot_hover"), uiOutput("hover_info"))
)

server <- function(input, output) {
  sliderYear <- reactive({input$Year})
  boxRegion <- reactive({input$Region})
  dotSize <- reactive({input$dotSize})
  
  output$plot <- renderPlot(
    if (is.null(boxRegion())) {
      {ggplot() + 
          geom_point(data = subset(joined_data, year == sliderYear()), aes(x = life_expectancy, y = fertility_rate, colour = Region, size = population)) +
          xlim(0, 90) + ylim(0, 9) + scale_size(guide = 'none') + ggtitle(label = "Life Expectancy vs Fertility Rate") + theme(plot.title = element_text(size = 30), axis.title=element_text(size=20), legend.text = element_text(size=15), legend.title = element_text(size=20)) + scale_size(range = c(1,6)*dotSize(), guide = 'none')
          }
    } else {
      {ggplot() + 
          geom_point(data = subset(joined_data, year == sliderYear() & Region %in% boxRegion()), aes(x = life_expectancy, y = fertility_rate, colour = Region, size = population)) +
          xlim(0, 90) + ylim(0, 9) + scale_size(guide = 'none') + 
          geom_point(data = subset(joined_data, year == sliderYear() & !Region %in% boxRegion()), aes(x = life_expectancy, y = fertility_rate, size = population, alpha = 0.2)) + scale_alpha(guide = 'none') +
          ggtitle(label = "Life Expectancy vs Fertility Rate") + theme(plot.title = element_text(size = 30), axis.title=element_text(size=20), legend.text = element_text(size=15), legend.title = element_text(size=20)) + scale_size(range = c(1,6)*dotSize(), guide = 'none')}
    }
    )
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(joined_data, hover, threshold = 5, maxpoints = 1, addDist = TRUE)
    
    if (nrow(point) == 0) return(NULL)
    
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")

    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country.Name,
                    "<b> life_expectancy: </b>", point$life_expectancy,
                    "<b> fertility_rate: </b>", point$fertility_rate)))
    )
  })
}

shinyApp(ui = ui, server = server)