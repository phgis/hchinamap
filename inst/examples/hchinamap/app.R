library(shiny)
library(hchinamap)
library(dplyr)
library(magrittr)
library(colourpicker)
ui <- fluidPage(
    # Application title
    titlePanel("Hchinamap Shiny Example"),
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "title",
                label = "Input chart title: ",
                value = "Map Example",
                placeholder = "Map Example"
            ),
            selectInput(
                inputId = "region",
                label = "Chose region parameter：",
                choices = c("China", "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin", "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi", "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan", "Chongqing", "Sichuan", "Guizhou", "Yunan", "Tibet", "Shaanxi", "Gansu", "Qinghai", "Ningxia", "Xinjiang", "Taiwan", "Hong Kong", "Macao"),
                selected = "China"
            ),
            selectInput(
                inputId = "theme",
                label = "Choose a theme: ",
                choices = c("darkgreen", "darkblue", "avocado", "darkunica", "gray", "gridlight", "grid", "sandsignika", "sunset"),
                selected = "sunset"
            ),
            textInput(
                inputId = "height",
                label = "Chart Height: ",
                placeholder = "500px",
                value = "500px"
            ),
            textInput(
                inputId = "width",
                label = "Chart Width: ",
                placeholder = "100%",
                value = "100%"
            ),
            textInput(
                inputId = "itermName",
                label = "Data attributes in tooltip: ",
                placeholder = "Random data",
                value = "Random data"
            ),
            selectInput(
                inputId = "titleAlign",
                label = "The horizontal position of the title: ",
                choices = c("left", "center", "right"),
                selected = 'center'
            ),
            textInput(
                inputId = "titleSize",
                label = "The size of the title:",
                placeholder = "20px",
                value = "20px"
            ),
            colourInput(
                inputId = "titleColor",
                label = "The color of the title:",
                value = "#333333",
                showColour = "background",
                allowTransparent = TRUE
            ),
            textInput(
                inputId = "subtitle",
                label = "Chart subtitle: ",
                placeholder = "https://www.czxa.top",
                value = ""
            ),
            selectInput(
                inputId = "subtitleAlign",
                label = "The horizontal position of the subtitle: ",
                choices = c("left", "center", "right"),
                selected = "center"
            ),
            colourInput(
                inputId = "subtitleColor",
                label = "Subtitle color: ",
                value = "#666666",
                showColour = "background",
                allowTransparent = TRUE
            ),
            textInput(
                inputId = "min",
                label = "Minimum scale: ",
                placeholder = "0",
                value = "0"
            ),
            colourInput(
                inputId = "minColor",
                label = "The color at the minimum of the scale: ",
                value = "rgb(255,255,255)",
                showColour = "background",
                allowTransparent = TRUE
            ),
            colourInput(
                inputId = "maxColor",
                label = "The color at the maximum of the scale: ",
                value = "#006cee",
                showColour = "background",
                allowTransparent = TRUE
            ),
            selectInput(
                inputId = "legendLayout",
                label = "The direction of the legend: ",
                choices = c("horizontal", "vertical"),
                selected = "horizontal"
            ),
            selectInput(
                inputId = "legendAlign",
                label = "Horizontal position of the legend: ",
                choices = c("left", "center", "right"),
                selected = "left"
            ),
            textInput(
                inputId = "legendTitle",
                label = "Title of the legend: ",
                placeholder = "",
                value = ""
            ),
            selectInput(
                inputId = "legendVerticalAlign",
                label = "Vertical position of the legend: ",
                choices = c("top", "center", "bottom"),
                selected = "center"
            ),
            colourInput(
                inputId = "hoverColor",
                label = "The color of the area when the mouse is over: ",
                value = "#a4edba",
                showColour = "background",
                allowTransparent = TRUE
            )
        ),
        mainPanel(
           hchinamapOutput("chinamap", width = "100%",
                height = "500px")
        )
    )
)
server <- function(input, output){
    china <- reactive({
        chinadf %>%
            dplyr::filter(region == input$region)
    })
    output$chinamap <- renderHchinamap({
        hchinamap(name = china()$name,
                  value = china()$value,
                  region = input$region,
                  width = input$width, height = input$height,
                  title = input$title,
                  subtitle = input$subtitle,
                  itermName = input$itermName,
                  titleAlign = input$titleAlign,
                  titleSize = input$titleSize,
                  titleColor = input$titleColor,
                  subtitleAlign = input$subtitleAlign,
                  subtitleSize = input$subtitleSize,
                  subtitleColor = input$subtitleColor,
                  min = input$min,
                  minColor = input$minColor,
                  maxColor = input$maxColor,
                  legendLayout = input$legendLayout,
                  legendAlign = input$legendAlign,
                  legendTitle = input$legendTitle,
                  legendVerticalAlign = input$legendVerticalAlign,
                  hoverColor = input$hoverColor,
                  theme = input$theme)
    })
}

# Run the application
shinyApp(ui = ui, server = server)
