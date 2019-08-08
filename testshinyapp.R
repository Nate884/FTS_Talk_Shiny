library(shiny)
library(bs4Dash)
library(ggplot2)

sidebar <- bs4DashSidebar(
  skin = "light",
  status = "primary",
  title = "NATE'S AWESOME APP",
  brandColor = "primary",
  elevation = 3,
  opacity = 0.8,
  bs4SidebarUserPanel(
    img = "https://image.flaticon.com/icons/svg/1149/1149168.svg", 
    text = "Let's Do Analysis!"
  ),
  bs4SidebarMenu(
    bs4SidebarHeader("Big Time"),
    bs4SidebarMenuItem(
      "Input Data",
      tabName = "item1",
      icon = "sliders"
    ),
    bs4SidebarMenuItem(
      "More Options",
      tabName = "Some Other Junk",
      icon = "id-card"
    )
  )
)

body <- bs4DashBody(
    fluidPage(
      
      # Copy the line below to make a checkbox
        plotOutput("plot"),
      checkboxInput("checkbox", 
                    label = "K-Means Random Forest Deep Learning Tensored-Flavored Generalized Additive Model ", value = TRUE),
       
    )
  )

server <- function(input,output){
  g <- ggplot(mpg, aes(class)) + geom_bar(aes(fill = drv))
  output$plot <- renderPlot({
    if(input$checkbox){g}
    })
}

shiny::shinyApp(
  ui = bs4DashPage(
    old_school = FALSE,
    sidebar_collapsed = FALSE,
    controlbar_collapsed = FALSE,
    title = "Basic Dashboard",
    navbar = bs4DashNavbar(),
    sidebar = sidebar,
    controlbar = NULL,
    footer = bs4DashFooter(),
    body = body
  ),
  server = server
)
