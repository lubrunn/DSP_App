#' Stock Calculations
#'


#german stock dataset filtered according to selected date
#' @export
#' @rdname stock_calculations
stock_dataset_DE <- function(stock,date1,date2){
  symbols <- c(COMPONENTS_DE()[["Symbol"]],"GDAXI")[c(COMPONENTS_DE()[["Company.Name"]],"GDAXI") %in% stock]
  data <- filter(load_all_stocks_DE(),name %in% symbols)
  data %>% filter(Dates >= date1 & Dates <= date2)
}


#' @export
#' @rdname stock_calculations
create_hover_info_DE <- function(hoverinput,stockdata){
  hover <- hoverinput
  point <- nearPoints(stockdata, hover, threshold = 10, maxpoints = 1, addDist = TRUE)
  if (nrow(point) == 0) return(NULL)

  # calculate point position INSIDE the image as percent of total dimensions
  # from left (horizontal) and from top (vertical)
  left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
  top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)

  # calculate distance from left and bottom side of the picture in pixels
  left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
  top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)

  # create style property fot tooltip
  # background color is set so tooltip is a bit transparent
  # z-index is set so we are sure are tooltip will be on top
  style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.6); ",
                  "left:", left_px + 2, "px; top:", top_px + 60, "px;")

  # actual tooltip created as wellPanel
  wellPanel(
    style = style,
    p(HTML(paste0("<b> Company: </b>", point$name, "<br/>",
                  "<b> Date: </b>", point$Date, "<br/>",
                  "<b> Price: </b>", point$Close., "<br/>")))
  )
}



