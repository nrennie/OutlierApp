#' Server function
#' @param input,output,session Internal parameter for {shiny}
#' @import visNetwork ggplot2
#' @export

app_server <- function(input, output, session) {
  #choose correlation threshold
  output$threshold_check = shiny::renderUI({
    if (input$rb == "Cluster") {
      shiny::sliderInput("threshold",
                         "Correlation Threshold:",
                         min = 0,
                         max = 1,
                         value = 0.7)
    }
  })

  clusters_corr <- shiny::reactive({
    if (input$rb == "Cluster"){
      clusters_corr <- mst_clustering_threshold(corr_matrix = OutlierApp::corr_matrix,
                                                leg_names = OutlierApp::leg_names,
                                                connections = OutlierApp::connections,
                                                corr_threshold = input$threshold)$cluster_list
    }
  })

  output$network <- visNetwork::renderVisNetwork({
    #calculate clusters if necessary
    if (input$rb == "Cluster") {
      network_edges$cluster <- as.numeric(unlist(sapply(1:length(clusters_corr()),
                                                function(x) rep(x, length(clusters_corr()[[x]])))))
    }
    #choose colours
    network_edges$color <- col_choice(type=input$rb, network_edges)
    network_edges$id <- network_edges$leg
    #visualise network
    visNetwork::visNetwork(network_nodes, network_edges) %>%
      visNetwork::visEdges(smooth = list(enabled = T, type = 'dynamic'), arrows = 'to') %>%
      visNetwork::visLayout(randomSeed = 16) %>%
      visNetwork::visOptions(highlightNearest = F) %>%
      visNetwork::visPhysics(solver = "barnesHut", barnesHut = list(springConstant = 0.002)) %>%
      visNetwork::visEvents(select = "function(data) {
                Shiny.onInputChange('current_edges_selection', data.edges);
                ;}")

  })

  leg_selected <- shiny::reactive({
    input$current_edges_selection[1]
  })

  all_legs_selected <- shiny::reactive({
    if (!is.null(leg_selected())){
      if (input$rb == "Leg"){
        e <- leg_selected()
      }
      if (input$rb == "Cluster"){
        e <- clusters_corr()[[which(lapply(clusters_corr(),
                                           function(x) leg_selected() %in% x) == TRUE)]]
      }
      if (input$rb == "Line"){
        line_clusters <- line_clustering(leg_names)
        e <- line_clusters[[which(lapply(line_clusters,
                                         function(x) leg_selected() %in% x) == TRUE)]]
      }
      if (input$rb == "Network"){
        e <- network_edges$leg
      }
      e
    }
  })

  leg_selected_name <- shiny::reactive({
    if (!is.null(leg_selected())){
      paste(network_edges$from[which(as.vector(network_edges$leg) == leg_selected())],
            "to", network_edges$to[which(as.vector(network_edges$leg) == leg_selected())])
    }
  })

  leg_selected_data <- shiny::reactive({
    fname <- system.file("extdata", paste(input$leg_plot,"_extrap_data_20190725.rds", sep=""),
                         package = "OutlierApp")
    d <- readRDS(fname)
    d
  })

  leg_selected_status <- shiny::reactive({
    fname <- system.file("extdata", paste(input$leg_plot,"_which_extrap_20190725.rds", sep=""),
                        package = "OutlierApp")
    dcol <- readRDS(fname)
    dcol
  })

  leg_zn <- shiny::reactive({
    if (!is.null(leg_selected())){
      zn <- lapply(all_legs_selected(),
                   function(x) readRDS(
                     system.file("extdata", paste(x,"_diffs_20190725.rds", sep=""),
                                 package = "OutlierApp")))
      names(zn) <- all_legs_selected()
      zn
    }
  })

  clusters_zn <- shiny::reactive({
    if (!is.null(leg_selected())){
      b <- merge_differences(leg_zn())
      colnames(b) <- all_legs_selected()
      b
    }
  })

  probs <- shiny::reactive({
    if (!is.null(leg_selected())){
      gpd_probs(clusters_zn())
    }
  })

  outliers <- shiny::reactive({
    if (!is.null(leg_selected())){
      if (input$upcoming == FALSE){
        outliers <- which(probs()[,2] > input$perc_out)[1:min(input$max_outs,
                                                              length(which(probs()[,2] > input$perc_out)))]
      }
      if (input$upcoming == TRUE){
        outliers <- which(probs()[,2] > input$perc_out & as.Date(as.numeric(probs()[,1]),
                                                                 origin="1970-01-01") > today_date)[1:min(input$max_outs,
                                                                                                          length(which(probs()[,2] > input$perc_out)))]
      }
      outliers
    }
  })

  probs_table <- shiny::reactive({
    if (!is.null(leg_selected())){
      tab <- probs()[outliers(),]
      tab2 <- matrix(tab, ncol=3, nrow=length(outliers()))
      tab2[,2] <- round(as.numeric(tab2[,2]), 3)*100
      tab3 <- data.frame(as.Date(as.numeric(tab2[,1]),origin="1970-01-01"),
                         tab2[,2:3],
                         sapply(
                           as.Date
                                (as.numeric(
                                  tab2[,1]),
                                  origin="1970-01-01"),
                           function(x) c("Bookings Open", "Departed")[as.numeric(x <= today_date)+1]))
      colnames(tab3) <-  c("Dep. Date", "Outlier Prob (%)", "Leg(s) Affected", "Status")
      tab3
    }
  })

  output$view_data <- DT::renderDataTable({
    DT::datatable(probs_table(), options = list(dom = 'ft'))
  })


  output$tabSelected <- shiny::renderText({
    paste("You have selected:", input$tab)
  })

  output$date_text <- shiny::renderText({
    paste("Date:", today_date)
  })

  output$txt <- shiny::renderText({
    paste("You have selected:", leg_selected_name())
  })

  output$outliers <- plotly::renderPlotly({
    if (!is.null(leg_selected())){
      if (input$extrap_outliers == "extrap"){
        df <- leg_selected_data()
        colnames(df) <- 1:18
        df <- reshape::melt(df)
        dcol <- leg_selected_status()
        dfcol <- reshape::melt(dcol)
        df$depid <- dfcol$value
        dat <- tibble::tibble(
          time = df$X2,
          Bookings = ceiling(df$value),
          Departure = as.Date(df$X1,origin="1970-01-01"),
          depid = df$depid
        )
        p <- ggplot(data = dat,
                    mapping = aes(x = rev(time),
                                  y = Bookings,
                                  group = Departure,
                                  col=depid)) +
          geom_line() +
          theme_light() +
          xlab("Booking Intervals before Departure") +
          ylab("Bookings") +
          scale_x_reverse() +
          scale_colour_manual("",
                              values=c("Extrapolated"="purple4",
                                       "Observed"=alpha(c("grey"),0.3))) +
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                legend.text=element_text(size=14),
                plot.background = element_rect(fill = "transparent", color = NA),
                legend.background = element_rect(color = NA,fill="transparent"),
                legend.box.background = element_rect(fill = "transparent",color=NA),
                legend.position=c(0,1),legend.justification=c(0,1),
                legend.title=element_blank(),
                legend.key = element_blank())
        plotly::ggplotly(p, tooltip = c("Departure", "y"))
      }
      else {
        df <- leg_selected_data()
        colnames(df) <- 1:18
        df <- reshape::melt(df)
        choose_colours <- rep(NA, nrow(leg_selected_data()))
        probs_selected <- which(rownames(leg_selected_data()) %in% probs_table()[,1])
        choose_colours[probs_selected] <- na.omit(as.numeric(probs_table()[,2]))
        df$depid <- choose_colours
        dat <- tibble::tibble(
          time = df$X2,
          Bookings = ceiling(df$value),
          Departure = as.Date(df$X1,origin="1970-01-01"),
          depid = df$depid
        )
        p <- ggplot(data = dat,
                    mapping = aes(x = rev(time),
                                  y = Bookings,
                                  group = Departure,
                                  col=depid)) +
          geom_line() +
          theme_light() +
          xlab("Booking Intervals before Departure") +
          ylab("Bookings") +
          scale_x_reverse() +
          scale_colour_gradient2(low="yellow", mid="orange", high="red",
                                 midpoint=50, limits = c(0.1, 100),
                                 na.value = alpha(c("grey"),0.3)) +
          theme(axis.text=element_text(size=14),
                axis.title=element_text(size=14),
                legend.text=element_text(size=14),
                plot.background = element_rect(fill = "transparent", color = NA),
                legend.background = element_rect(color = NA,fill="transparent"),
                legend.box.background = element_rect(fill = "transparent",color=NA),
                legend.position="none",
                legend.justification=c(0,1),
                legend.title=element_blank(),
                legend.key = element_blank())
        plotly::ggplotly(p, tooltip = c("Departure", "y"))
      }
    }
  })

  output$tabUI <- shiny::renderUI({
    if (length(all_legs_selected()) >= 1){
      shinydashboard::box(title = "Bookings",
                 width = 6,
                 height = 600,
                 status = "primary",
          shiny::selectInput("leg_plot",
                             label = "View bookings for leg:",
                             choices = all_legs_selected()),
          shiny::radioButtons("extrap_outliers",
                              label = "Highlight:",
                              choices = c("Extrapolation" = "extrap", "Outliers" = "plot_outs"),
                              inline = TRUE),
          plotly::plotlyOutput("outliers")
      )
    }
    else{
      shinydashboard::box(title = "Bookings",
                 width = 6,
                 height = 600,
                 status = "primary")
    }
  })


}
