library(tidyverse)
library(shiny)
library(bslib)
library(histoslider)

###########################
### Setup and data prep ###
###########################

flights = read_rds("flights.rds")

ggplot2::theme_set(theme_minimal())

pal = leaflet::colorFactor(c("#66c2a5", "#fc8d62", "#8da0cb"), unique(flights$origin))

origins = flights |> 
  select(origin, starts_with(c("start"))) |>
  distinct() |>
  transmute(
    origin = as_factor(origin),
    geometry = map2(
      start_lon, start_lat, ~sf::st_point(c(.x, .y))
    ) |> sf::st_as_sfc(crs = 4326)
  ) |>
  sf::st_as_sf()

dests = flights |> 
  select(dest, starts_with(c("end"))) |>
  distinct() |>
  transmute(
    dest = as_factor(dest),
    geometry = map2(
      end_lon, end_lat, ~sf::st_point(c(.x, .y))
    ) |> sf::st_as_sfc(crs = 4326)
  ) |>
  sf::st_as_sf()

CHOICES = list(
  origin = c(
    "Choose origin(s)" = "",
    "O'Hare" = "ORD",
    "Midway" = "MDW",
    "Rockford" = "RFD"
  ),
  dest_name = c(
    "Choose destination(s)" = "",
    sort(unique(flights$dest_name))
  ),
  carrier_name = c(
    "Choose carrier(s)" = "",
    unique(flights$carrier_name)
  )
)



######################
### Side bar setup ###
######################

sidebar_acc = accordion(
  #open = c("Origin", "Destination"),
  accordion_panel(
    "Flight Path",
    icon = fontawesome::fa("plane-departure"),
    actionLink(
      "flight_path_reset", "Reset",
      style = htmltools::css(
        position = "absolute",
        right = "1rem",
        text_decoration = "none",
        font_weight = 700,
        font_size = ".875rem"
      )
    ),
    selectizeInput(
      "origin", "Origin",
      choices = CHOICES$origin,
      multiple = TRUE,
      options = list(plugins = "remove_button")
    ),
    selectizeInput(
      "dest_name", "Destination",
      choices = CHOICES$dest_name,
      multiple = TRUE,
      options = list(plugins = "remove_button")
    ),
    selectizeInput(
      "carrier_name", "Carrier",
      choices = CHOICES$carrier_name,
      multiple = TRUE,
      options = list(plugins = "remove_button")
    )
  ),
  accordion_panel(
    "Flight time",
    icon = fontawesome::fa("clock"),
    input_histoslider(
      "air_time", "Air time",
      flights$air_time,
      height = 150,
      #options = list(handleLabelFormat = "0d")
    ),
    input_histoslider(
      "sched_dep_time", "Departure time",
      flights$sched_dep_time,
      height = 150,
      options = list(handleLabelFormat = "0d")
    ),
    input_histoslider(
      "sched_arr_time", "Arrival time",
      flights$sched_arr_time,
      height = 150,
      options = list(handleLabelFormat = "0d")
    ),
    input_histoslider(
      "date", "Date",
      flights$date,
      height = 150,
      breaks = "months",
      options = list(handleLabelFormat = "%b %e")
    )
  )
)


#######################
### nav panel setup ###
#######################

delay_panel = nav_panel(
  "Delay overview",
  uiOutput("value_boxes"),
  layout_columns(
    card(
      full_screen = TRUE,
      card_header(
        "Flight paths",
        tooltip(
          bsicons::bs_icon("info-circle", title = "About marker areas"),
          "Marker areas are proportional to mean arrival delay"
        )
      ),
      leaflet::leafletOutput("flight_paths")
    ), 
    card(
      full_screen = TRUE,
      card_header(
        "Average delay by category",
        popover(
          bsicons::bs_icon("gear", title = "Settings"),
          selectInput(
            "avg_delay_category", "Category",
            c("Carrier", "Month", "Weekday")
          ),
          radioButtons(
            "avg_delay_type", "Delay type",
            c("Arrival", "Departure"),
            inline = TRUE
          )
        )
      ),
      plotOutput("delay_plot")
    )
  )
)


################
### UI setup ###
################

ui = page_navbar(
  theme = bs_theme(
    preset = "shiny"
  ),
  lang = "en",
  title = tags$span(
    tags$img(
      src = "logo.png",
      width = "46px",
      height = "auto",
      class = "me-3",
      alt = "Shiny hex logo"
    ),
    "Chicago Flights"
  ),
  sidebar = sidebar(width = 275, sidebar_acc),
  nav_spacer(),
  delay_panel
  #nav_item(
  #  input_dark_mode(id = "dark_mode", mode = "light")
  #)
)


####################
### server setup ###
####################

server = function(input, output, session) {
  
  # Reset button observer
  observe({
    updateSelectInput(
      inputId = "origin",
      choices = CHOICES$origin
    )
    updateSelectInput(
      inputId = "dest_name",
      choices = CHOICES$dest_name
    )
    updateSelectInput(
      inputId = "carrier_name",
      choices = CHOICES$carrier_name
    )
  }) |>
    bindEvent(input$flight_path_reset)
  
  # Flights with all filters applied (i.e., data used for value boxes/plots)
  flight_dat = reactive({
    between2 = function(x, r) {
      if (length(x) != 0)
        dplyr::between(x, r[1], r[2])
      else
        logical()
    } 
    
    d = flights |>
      filter(between2(sched_dep_time, input$sched_dep_time)) |>
      filter(between2(sched_arr_time, input$sched_arr_time)) |>
      filter(between2(date, input$date)) |>
      filter(between2(air_time, input$air_time))
    
    if (!is.null(input$origin))
      d = filter(d, origin %in% input$origin)
    
    if (!is.null(input$dest_name))
      d = filter(d, dest_name %in% input$dest_name)
    
    if (!is.null(input$carrier_name))
      d = filter(d, carrier_name %in% input$carrier_name)

    d
  })
  
  # Calculate summary values for the value boxes
  summary_vals = reactive({
    d = flight_dat()
    validate(need(nrow(d) > 0, "No flights match the selected filters"))
    
    list(
      n              = scales::comma(nrow(d)),
      n_dest         = length(unique(d$dest_name)),
      n_carriers     = length(unique(d$carrier_name)),
      dep_delay      = round(mean(d$dep_delay, na.rm = T), 0),
      dep_delay_perc = round(100 * sum(d$dep_delay > 0, na.rm = T) / nrow(d), 1),
      arr_delay      = round(mean(d$arr_delay, na.rm = T), 0),
      arr_delay_perc = round(100 * sum(d$arr_delay > 0, na.rm = TRUE) / nrow(d), 1)
    )
  })
  
  # Render value boxes
  output$value_boxes = renderUI({
    vals = summary_vals()
    
    n_flights = value_box(
      "A TOTAL OF",
      paste(vals$n, "flights"),
      paste("Across", vals$n_dest, "destinations"),
      tags$p(paste(
        "On", vals$n_carriers, "different carriers"
      )),
      showcase = bsicons::bs_icon("airplane")
    )
    
    late = if (vals$dep_delay > 0) "late" else "early"
    delay_dep = value_box(
      "AVERAGE DEPARTURE",
      paste(vals$dep_delay, "mins", late),
      paste0(vals$dep_delay_perc, "% of flights depart ", late),
      showcase = bsicons::bs_icon("hourglass-split")
    )
    
    late = if (vals$arr_delay > 0) "late" else "early"
    delay_arr = value_box(
      "AVERAGE ARRIVAL",
      paste(vals$arr_delay, "mins", late),
      paste0(vals$arr_delay_perc, "% of flights arrive ", late),
      showcase = bsicons::bs_icon("hourglass-bottom")
    )
    
    layout_columns(n_flights, delay_dep, delay_arr)
  })
  
  
  # Flight path plot
  output$flight_paths = leaflet::renderLeaflet({
    routes = flight_dat() |> 
      select(origin, dest, ends_with(c("lat","lon"))) |>
      distinct() |>
      mutate(
        start = map2(start_lon, start_lat, c),
        end = map2(end_lon, end_lat, c),
      ) |>
      transmute(
        origin = origin,
        route = paste(origin, "->", dest),
        geometry = map2(
          start, end, ~sf::st_linestring(rbind(.x, .y))
        ) |> sf::st_as_sfc(crs = 4326)
      ) |>
      sf::st_as_sf()
    
    
    leaflet::leaflet() |>
      leaflet::addTiles() |>
      leaflet::addPolylines(
        data = routes, weight=3, popup = ~route, color = ~pal(origin), opacity = 0.5
      ) |>
      leaflet::addCircleMarkers(
        data = origins, radius=10, fillColor = ~pal(origin), color="black", fillOpacity = 1, opacity = 1, popup = ~origin, weight=1
      ) |>
      leaflet::addCircleMarkers(
        data = dests, radius=1/2, color = "black", popup = ~dest
      )
  })
  
  # Delay plot
  output$delay_plot = renderPlot({
    d = flight_dat()
    validate(need(nrow(d) > 0, "No flights match the selected filters"))
    
    d = switch(
      input$avg_delay_category,
      Weekday = group_by(d, y = lubridate::wday(date, label = TRUE)),
      Month = group_by(d, y = lubridate::month(date, label = TRUE)),
      Carrier = group_by(d, y = carrier_name),
      stop("Category of ", input$avg_delay_category, "not implemented")
    )
    
    d = switch(
      input$avg_delay_type,
      Arrival = summarise(d, avg = mean(arr_delay, na.rm = TRUE), .groups = "drop"),
      Departure = summarise(d, avg = mean(dep_delay, na.rm = TRUE), .groups = "drop"),
    )
    
    d |>
      arrange(avg) |>
      mutate(y = as_factor(y)) |>
      ggplot(aes(x = avg, y = y)) +
        geom_bar(stat="identity") +
        labs(
          y = "", x = "Delay (mins)",
          title = paste("Average", tolower(input$avg_delay_type), "delay")
        )
  })
}

shinyApp(ui, server)
