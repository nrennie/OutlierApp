#' UI function
#' @import igraph
#' @export

app_ui <- function(request) {
  shinydashboard::dashboardPage(
  shinydashboard::dashboardHeader(title = ""),
  shinydashboard::dashboardSidebar(
    shiny::h3(shiny::textOutput("date_text"),
              align = "center"),
    #choose level of clustering on the network
    shiny::radioButtons("rb", "Display legs by:",
                 c("Network" = "Network",
                   "Line" = "Line",
                   "Cluster" = "Cluster",
                   "Leg" = "Leg")),
    #if choose clustering, allow a choice of threshold
    shiny::uiOutput("threshold_check"),
    #outlier threshold
    shiny::sliderInput("perc_out", "Show outliers with probability higher than:",
                min = 0, max = 1, value = 0),
    #max number of outliers
    shiny::sliderInput("max_outs", "Maximum number of outliers:",
                min = 1, max = 50, value = 10)
  ),

  shinydashboard::dashboardBody(
    dashboardthemes::shinyDashboardThemes(
      theme = "grey_light"
    ),
    shiny::fluidRow(
      #visualise network
      shinydashboard::box(title = "Network",
                 height=600,
                 status = "primary",
                 width = 6,
                 visNetwork::visNetworkOutput("network"),
                 shiny::textOutput("txt")),
      #plot outliers
      shiny::uiOutput("tabUI")
    ),
    shinydashboard::box(
      title = "Outliers",
      width=NULL,
      height=400,
      status = "primary", shiny::div(style = 'height:330px; overflow-y: scroll',
                                     shiny::checkboxInput("upcoming",
                                                          label="Show only upcoming departures"),
                                     DT::dataTableOutput('view_data'))
    )
  )
)
}
