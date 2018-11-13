library(shiny)
library(dplyr)
library(shinybulma)
library(echarts4r)

ui <- bulmaPage(
  theme = "materia",
  tags$head(
    tags$style(".hero{background-color:#293c55!important;}")
  ),
  bulmaNavbar(
    bulmaNavbarBrand(
       bulmaNavbarItem(
        "echarts4r*",
        href = "home"
       ),
       bulmaNavbarBurger()
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
        "graph"
      ),
      bulmaNavbarItem(
        "events"
      )
    )
  ),
  bulmaNav(
    "home",
    bulmaHero(
      color = "info",
      fullheight = TRUE,
      bulmaHeroBody(
        bulmaContainer(
          bulmaTitle("echarts4r"),
          bulmaSubtitle("using shiny*"),
          img(src = "https://echarts4r.john-coene.com/reference/figures/logo.png"),
          br(),
          br(),
          a(href = "http://echarts4r.john-coene.com/", target = "blank", icon("desktop fa-lg"), " Website"),
          br(),
          a(href = "https://github.com/JohnCoene/echarts4rShiny", target = "blank", tags$i(class = "fab fa-github fa-lg"), " Source code")
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
      echarts4rOutput("updateSection"),
      p(
        "Use", code("e_append1p_p"), "or", code("e_append2p_p"), "to add data to a proxy",
        "(", code("echarts4rProxy"), ")."
      )
    )
  ),
  bulmaNav(
    "graph",
    bulmaContainer(
      br(),
      bulmaTitle("Nodes adjacency"),
      bulmaColumns(
        bulmaColumn(
          bulmaActionButton("focusButton", label = "focus")
        ),
        bulmaColumn(
          bulmaActionButton("unfocusButton", label = "unfocus")
        ),
        bulmaColumn(
          bulmaSelectInput(
            "node", 
            "node", 
            choices = 1:100
          )
        )
      ),
      echarts4rOutput("graphSection")
    )
  ),
  bulmaNav(
    "events",
    bulmaContainer(
      br(),
      bulmaTitle("Events"),
      p(
        code("echarts4r"),
        "lets you capture elements selected, or cliked (many more detailed on",
        tags$a(
          "the package website",
          href = "https://echarts4r.john-coene.com/articles/shiny.html"
        )
        ,")."
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
      ),
      bulmaColumns(
        bulmaColumn(
          bulmaSubtitle("Clicked data point"),
          verbatimTextOutput("clickedData")
        ),
        bulmaColumn(
          bulmaSubtitle("Clicked serie"),
          verbatimTextOutput("clickedSerie")
        ),
        bulmaColumn(
          bulmaSubtitle("Clicked Row"),
          verbatimTextOutput("clickedRow")
        )
      )
    )
  )
)

server <- function(input, output) {

  init <- data.frame(x = rnorm(10, 5, 2), y = rnorm(10, 50, 10), z = rnorm(10, 2, 5))

  update_data <- eventReactive(input$updateButton, {
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
  
  output$clickedData <- renderPrint({
    input$proxies_clicked_data
  })
  
  output$clickedSerie <- renderPrint({
    input$proxies_clicked_serie
  })
  
  output$clickedRow <- renderPrint({
    input$proxies_clicked_row
  })
  
  value <- rnorm(10, 10, 2)
  
  nodes <- data.frame(
    name = paste0(LETTERS, 1:100),
    value = value,
    size = value,
    grp = rep(c("grp A", "grp B"), 5),
    stringsAsFactors = FALSE
  )
  
  edges <- data.frame(
    source = sample(nodes$name, 150, replace = TRUE),
    target = sample(nodes$name, 150, replace = TRUE),
    stringsAsFactors = FALSE
  )
  
  output$graphSection <- renderEcharts4r({
    e_charts() %>% 
      e_graph() %>% 
      e_graph_nodes(nodes, name, value, size, grp) %>% 
      e_graph_edges(edges, source, target)
  })
  
  observeEvent(input$focusButton, {
    echarts4rProxy("graphSection") %>% 
      e_focus_adjacency_p(seriesIndex = 0, index = input$node)
  })
  
  observeEvent(input$unfocusButton, {
    echarts4rProxy("graphSection") %>% 
      e_unfocus_adjacency_p(seriesIndex = 0)
  })

}

shinyApp(ui, server)