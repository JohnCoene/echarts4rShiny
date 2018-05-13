library(shiny)
library(dplyr)
library(shinybulma)
library(echarts4r)

ui <- bulmaPage(
  theme = "materia",
  bulmaNavbar(
    bulmaNavbarBrand(
       bulmaNavbarItem(
        "echarts4r",
        href = "home"
       )
    ),
    bulmaNavbarMenu(
      bulmaNavbarItem(
        "home"
      ),
      bulmaNavbarItem(
        "redraw"
      ),
      bulmaNavbarItem(
        "loading"
      ),
      bulmaNavbarItem(
        "data"
      ),
      bulmaNavbarItem(
        "events"
      )
    )
  ),
  bulmaNav(
    "home",
    bulmaHero(
      fullheight = TRUE,
      bulmaHeroBody(
        bulmaContainer(
          bulmaTitle("echarts4r*"),
          bulmaSubtitle("using shiny*")
        )
      )
    )
  ),
  bulmaNav(
    "redraw",
    bulmaContainer(
      br(),
      bulmaTitle("Redraw"),
      p(
        code("echarts4r"), "lets you redraw you chart in a neat manner (without redrawing the whole base chart), which makes for neat animations"
      ),
      br(),
      p("Hit \"redraw\" to see it in action"),
      br(),
      bulmaActionButton("redrawButton", label = "redraw"),
      br(), 
      bulmaColumns(
        bulmaColumn(
          echarts4rOutput("redrawSectionYes")
        ),
        bulmaColumn(
          echarts4rOutput("redrawSectionNo")
        )
      ),
      p(
        "Use",
        code("echarts4r"),
        "like you normally would but pass",
        code("dispose = TRUE"),
        "when you initialise the chart with",
        code("e_charts"), "or", code("e_chart.")
      )
    )
  ),
  bulmaNav(
    "loading",
    bulmaContainer(
      br(),
      bulmaTitle("Loading"),
      p("If your plot or data takes a while to generate you can add a loading icon."),
      br(),
      bulmaActionButton("generate", label = "generate"),
      br(),
      bulmaColumns(
        bulmaColumn(
          echarts4rOutput("showloading")
        ),
        bulmaColumn(
          echarts4rOutput("hideloading")
        )
      )
    )
  ),
  bulmaNav(
    "data",
    bulmaContainer(
      br(),
      bulmaTitle("Add data"),
      bulmaActionButton("updateButton", label = "update"),
      echarts4rOutput("updateSection")
    )
  ),
  bulmaNav(
    "events",
    bulmaContainer(
      br(),
      bulmaTitle("Events"),
      p(
        code("echarts4r"),
        "lets you capture elements selected, or cliked."
      ),
      bulmaColumns(
        bulmaColumn(
          echarts4rOutput("proxies")
        ),
        bulmaColumn(
          bulmaSubtitle("Brushed"),
          verbatimTextOutput("brushed"),
          bulmaSubtitle("Legend change"),
          verbatimTextOutput("selected"),
          bulmaSubtitle("Mouseover"),
          verbatimTextOutput("mouseover")
        )
      )
    )
  )
)

server <- function(input, output) {

  init <- data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10), z = rnorm(10, 2, 5))

  update_data <- reactive({
     set.seed(sample(1:1000, 1))
     data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10), z = rnorm(10, 2, 5))
  })

  output$redrawSectionYes <- renderEcharts4r({
    input$redrawButton
    mtcars %>%
      sample_n(10) %>%
      e_charts(mpg, dispose = FALSE) %>%
      e_line(qsec) %>%
      e_title("Redraw")
  })

  output$redrawSectionNo <- renderEcharts4r({
    input$redrawButton
    mtcars %>%
      sample_n(10) %>%
      e_charts(mpg) %>%
      e_line(qsec) %>%
      e_title("No redraw")
  })

  output$showloading <- renderEcharts4r({
    input$generate
    Sys.sleep(2)
    mtcars %>%
      sample_n(15) %>%
      e_charts(mpg, dispose = FALSE) %>%
      e_line(qsec) %>%
      e_show_loading() %>%
      e_title("Loading")
  })

  output$hideloading <- renderEcharts4r({
    input$generate
    Sys.sleep(2)
    mtcars %>%
      sample_n(15) %>%
      e_charts(mpg, dispose = FALSE) %>%
      e_line(qsec) %>%
      e_title("No loading")
  })

  output$updateSection <- renderEcharts4r({
    init %>%
      e_charts(x) %>%
      e_scatter(y, z)
  })

  observeEvent(input$updateButton, {
    echarts4rProxy("updateSection") %>%
      e_append2_p_(0, update_data(), "x", "y", "z")
  })

  output$proxies <- renderEcharts4r({
    init %>%
      e_charts(x) %>%
      e_scatter(y, z) %>%
      e_scatter(z, y) %>%
      e_brush()
  })

  output$brushed <- renderPrint({
    input$proxies_brush
  })

  output$selected <- renderPrint({
    input$proxies_legend_change
  })

  output$mouseover <- renderPrint({
    input$proxies_mouseover_data
  })

}

shinyApp(ui, server)