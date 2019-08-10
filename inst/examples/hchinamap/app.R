library(shiny)
library(hchinamap)
library(dplyr)
library(magrittr)
library(colourpicker)
ui <- fluidPage(
    # Application title
    titlePanel("hchinamap 示例"),
    sidebarLayout(
        sidebarPanel(
            textInput(
                inputId = "title",
                label = "输入地图标题：",
                value = "地图示例",
                placeholder = "地图示例"
            ),
            selectInput(
                inputId = "region",
                label = "选择国家或省份：",
                choices = c("中国", "安徽", "澳门", "北京", "重庆", "福建", "甘肃", "广东", "广西", "贵州", "海南", "河北", "河南", "黑龙江", "湖北", "湖南", "吉林", "江苏", "江西", "辽宁", "内蒙古", "宁夏", "青海", "山东", "山西", "陕西", "上海", "四川", "天津", "西藏", "香港", "新疆", "浙江", "云南"),
                selected = "中国"
            ),
            selectInput(
                inputId = "theme",
                label = "选择主题：",
                choices = c("darkgreen", "darkblue", "avocado", "darkunica", "gray", "gridlight", "grid", "sandsignika", "sunset"),
                selected = "sunset"
            ),
            textInput(
                inputId = "height",
                label = "图表高度：",
                placeholder = "500px",
                value = "500px"
            ),
            textInput(
                inputId = "width",
                label = "图表宽度：",
                placeholder = "100%",
                value = "100%"
            ),
            textInput(
                inputId = "itermName",
                label = "提示框里数据的名称：",
                placeholder = "随机数据",
                value = "随机数据"
            ),
            selectInput(
                inputId = "titleAlign",
                label = "标题的水平位置：",
                choices = c("left", "center", "right"),
                selected = 'center'
            ),
            textInput(
                inputId = "titleSize",
                label = "标题的大小：",
                placeholder = "20px",
                value = "20px"
            ),
            colourInput(
                inputId = "titleColor",
                label = "标题的颜色：",
                value = "#333333",
                showColour = "background",
                allowTransparent = TRUE
            ),
            textInput(
                inputId = "subtitle",
                label = "图表副标题：",
                placeholder = "https://www.czxa.top",
                value = ""
            ),
            selectInput(
                inputId = "subtitleAlign",
                label = "副标题的水平位置：",
                choices = c("left", "center", "right"),
                selected = "center"
            ),
            colourInput(
                inputId = "subtitleColor",
                label = "副标题的颜色：",
                value = "#666666",
                showColour = "background",
                allowTransparent = TRUE
            ),
            textInput(
                inputId = "min",
                label = "标度的最小值",
                placeholder = "0",
                value = "0"
            ),
            colourInput(
                inputId = "minColor",
                label = "标度的最小值处的颜色：",
                value = "rgb(255,255,255)",
                showColour = "background",
                allowTransparent = TRUE
            ),
            colourInput(
                inputId = "maxColor",
                label = "标度的最大值处的颜色：",
                value = "#006cee",
                showColour = "background",
                allowTransparent = TRUE
            ),
            selectInput(
                inputId = "legendLayout",
                label = "图例的方向：",
                choices = c("horizontal", "vertical"),
                selected = "horizontal"
            ),
            selectInput(
                inputId = "legendAlign",
                label = "图例的水平位置：",
                choices = c("left", "center", "right"),
                selected = "left"
            ),
            textInput(
                inputId = "legendTitle",
                label = "图例的标题",
                placeholder = "",
                value = ""
            ),
            selectInput(
                inputId = "legendVerticalAlign",
                label = "图例的竖直位置：",
                choices = c("top", "center", "bottom"),
                selected = "center"
            ),
            colourInput(
                inputId = "hoverColor",
                label = "鼠标掠过时区域的颜色：",
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
