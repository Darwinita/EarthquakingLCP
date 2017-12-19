#' @name eq_clean_data
#'
#' @title eq_clean_data
#'
#' @description A read data file function into a cleaned version of a data frame.
#'
#' @details \code{eq_clean_data} reads a filename from the U.S. National Oceanographic
#'  and Atmospheric Administration (NOAA) on signicant earthquakes around the world,
#'  into a clean version of an R data frame. It stops if the file does not exist.
#' The cleaned version of the data frame has:
#' i) the year, month and day  converted to the Date class, after dealing with negative years (BCE)
#' ii) EQ_PRIMARY, TOTAL_DEATHS, YEAR, LATITUDE and LONGITUDE columns converted to numeric class.
#' iii) A clean LOCATION_NAME column by calling the eq_location_clean function.
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#'
#' @param filename Is a character string giving the file name of the input data
#'   file.
#'
#' @return A clean version of an R data frame with the NOAA data contained in filename.
#'
#' @importFrom readr read_delim
#' @importFrom dplyr mutate
#' @importFrom stringr str_pad
#' @importFrom tidyr unite
#' @importFrom dplyr select
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate years
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' eq_clean_data("signif.txt")
#' }
#'
#' \dontrun{
#' eq_clean_data()
#' }
#'
#' @export
eq_clean_data <- function(filename) {
        if(!file.exists(filename))
                stop("file '", filename, "' does not exist")
        NOAAdata <- suppressMessages({
                readr::read_delim(filename, progress = TRUE, delim = "\t")
        })

        NOAAdata <- NOAAdata %>%
                dplyr::mutate(year = ifelse(YEAR <0, "0000", stringr::str_pad(as.character(YEAR),4,"left","0")),
                              bcyear = ifelse(YEAR <0, YEAR , 0)
                ) %>%
                tidyr::unite(datetime, year, MONTH, DAY, remove = FALSE) %>%
                dplyr::mutate(datetemp = lubridate::parse_date_time(datetime, "Ymd", truncated = 2),
                              LONGITUDE = as.numeric(LONGITUDE),
                              LATITUDE = as.numeric(LATITUDE),
                              EQ_PRIMARY = as.numeric(EQ_PRIMARY),
                              TOTAL_DEATHS = as.numeric(TOTAL_DEATHS),
                              YEAR = as.numeric(YEAR)
                ) %>%
                dplyr::mutate(DATE = datetemp + lubridate::years(bcyear)) %>%
                dplyr::select(-year, -bcyear, -datetime, -datetemp)

        cleandata <- eq_location_clean(NOAAdata)
        dplyr::tbl_df(cleandata)
}



#' @name eq_location_clean
#'
#' @title eq_location_clean
#'
#' @description A function to clean the \code{LOCATION_NAME} column from the
#' NOAA data frame.
#'
#' @details \code{eq_location_clean} cleans the \code{LOCATION_NAME} column from
#' the NOAA data frame by stripping out the country name (including the colon)
#' and converting names to title case.
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @param NOAAdata Is a data frame in dplyr format of NOAA data.
#'
#' @return A version of the NOAA data frame with a clean formatted \code{LOCATION_NAME}
#'  column
#'
#' @importFrom dplyr mutate_
#' @importFrom stringr str_replace
#' @importFrom stringr str_trim
#' @importFrom stringr str_to_title
#' @importFrom dplyr tbl_df
#'
#' @examples
#' \dontrun{
#' eq_location_clean(NOAAdf)
#' }
#'
#' \dontrun{
#' eq_location_clean()
#' }
#'
#' @export
eq_location_clean <- function(NOAAdata) {
        NOAAdata <- suppressMessages({
                NOAAdata %>%
                dplyr::mutate_(LOCATION_NAME = ~LOCATION_NAME %>%
                                      # remove everything before :
                                      stringr::str_replace(".+\\:", "") %>%
                                      # remove everything after ,;([
                                      stringr::str_replace("[\\,\\;\\(\\[].+", "") %>%
                                      # remove leading/trainling whitespaces
                                      stringr::str_trim("both") %>%
                                      # Convert to tittle case
                                      stringr::str_to_title(LOCATION_NAME)
                              )
        })
        dplyr::tbl_df(NOAAdata)
}

#' @name geom_timeline
#'
#' @title geom_timeline
#'
#' @description A function to plot a timeline of earthquakes.
#'
#' @details \code{geom_timeline} plots a timeline of earthquakes ranging from
#' \code{xmin} to \code{xmax} dates with a point for each earthquake.
#' Each point-earthquake is colored according to the number of fatal
#' casualties and its size indicates the magnitudes in Richter's scale.
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @inheritParams ggplot2::geom_point
#'
#' @param stat Statistical transformation. No data transformation if "identity".
#' @param xmin  (optional): minimum date for earthquakes
#' @param xmax  (optional): maximum date for earthquakes
#' @param ... Additional arguments
#'
#' @section Aesthetics for \code{geom_timeline}, with the required ones indicated in bold:
#' \itemize{
#'  \item \strong{\code{x}}: \code{DATE}
#'  \item \code{y}: \code{COUNTRY}, or any other factor for stratification
#'  \item \code{size}: \code{EQ_PRIMARY}
#'  \item \code{color}: \code{TOTAL_DEATHS}
#'  \item \code{fill}: \code{TOTAL_DEATHS}
#'  \item \code{alpha}: transparency = 0-1
#' }
#'
#' @return A ggplot2 layer
#'
#' @import ggplot2
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' xmin = lubridate::ymd_hm("1016-01-01",truncated = 2)
#' xmax = lubridate::ymd_hm("2016-01-01",truncated = 2)
#' NOAAdf <- eq_clean_data("signif.txt")
#' NOAAdfSp <- NOAAdf %>%
#'   dplyr::filter(COUNTRY %in% c("SPAIN", "IRAN", "BELGIUM"))
#' ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
#'                             y = COUNTRY
#' )) +
#'   geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
#'                     fill = TOTAL_DEATHS),
#'                 xmin = xmin, xmax = xmax) +
#'   theme_timeline() +
#'   ggplot2::labs(size = "Richter scale value", color = "# deaths")
#' }
#'
#' @export
geom_timeline <- function(mapping = NULL,
                          data = NULL,
                          stat = 'timeline',
                          position = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE,
                          xmin = NULL,
                          xmax = NULL,
                          na.rm = FALSE,
                          ...) {
        ggplot2::layer(
                geom = GeomTimeline,
                stat = stat,
                mapping = mapping,
                data = data,
                position = position,
                show.legend = show.legend,
                inherit.aes = inherit.aes,
                params = list(
                  xmin = xmin,
                  xmax = xmax,
                  na.rm = na.rm,
                  ...)
        )
}

#' @name GeomTimeline
#' @title GeomTimeline
#' @description geom_timeline-ggproto. See \code{\link{geom_timeline}}.
#' @import ggplot2
#' @import grid
GeomTimeline <- ggplot2::ggproto( "GeomTimeline", ggplot2::Geom,
        required_aes = c("x"),
        optional_aes = c("y"),
        default_aes = ggplot2::aes(color = "grey", size = 1, shape = 21, alpha = 0.3, fill = "grey", stroke = 1, y = 0.5),
        draw_key = ggplot2::draw_key_point,
        draw_group = function(data, panel_params, coord) {
          coords <- coord$transform(data, panel_params)
          grid::gList(
                  grid::segmentsGrob(
                  x0 = coords$x,
                  y0 = coords$y,
                  x1 = coords$x,
                  y1 = coords$y,
                  gp = grid::gpar(col = "lightgrey", alpha = 0.3)
          ),
          grid::pointsGrob(
                  x = coords$x,
                  y = coords$y,
                  pch = coords$shape,
                  size = grid::unit(coords$size,"mm"),
            gp = grid::gpar(
              col = coords$colour,
              fill = coords$fill,
              alpha = coords$alpha
            )
          )
         )
        }
)

#' @name geom_timeline_label
#'
#' @title geom_timeline_label
#'
#' @description A geom for labelling earthquake timeline plots created using
#' \code{\link{geom_timeline}}.
#'
#' @details \code{geom_timeline_label}  adds a vertical line to each earthquake data
#' point with a text label (e.g. \code{LOCATION_NAME}). There should be an option to
#' subset to n_max number of earthquakes,sorted by any measure of magnitude
#' (e.g. \code{EQ_PRIMARY}).
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @inheritParams ggplot2::geom_point
#' @inheritParams geom_timeline
#'
#' @param n_max: An integer corresponding to the number of top earthquakes to label,
#' sorted by measure of magnitude.
#' @param ... Additional arguments
#'
#' @section Aesthetics for \code{geom_timeline}, with the required ones indicated in bold:
#' \itemize{
#'  \item \strong{\code{x}}: \code{DATE}
#'  \item \strong{\code{label}}: \code{LOCATION_NAME} or any labels to be added
#'  \item \code{y}: \code{COUNTRY} or another factor for stratification
#'  \item  \code{measure}: \code{EQ_PRIMARY} or any other measure of magnitude to be passed
#'  \item \code{color}
#'  \item \code{fill}
#'  \item \code{size}
#'  \item \code{alpha}
#' }
#'
#' @return A ggplot2 graphical object with labelled geom_timeline plots.
#'
#' @import ggplot2
#' @importFrom dplyr filter
#'
#' @examples
#' \dontrun{
#' xmin = lubridate::ymd_hm("1016-01-01",truncated = 2)
#' xmax = lubridate::ymd_hm("2016-01-01",truncated = 2)
#' NOAAdf <- eq_clean_data("signif.txt")
#' NOAAdfSp <- NOAAdf %>%
#'   dplyr::filter(COUNTRY %in% c("SPAIN", "IRAN", "BELGIUM"))
#' ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
#'                             y = COUNTRY,
#' )) +
#'   geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
#'                     fill = TOTAL_DEATHS),
#'                 xmin = xmin, xmax = xmax) +
#'   geom_timeline_label(aes(label = LOCATION_NAME, measure = EQ_PRIMARY),
#'                       xmin = xmin, xmax = xmax, n_max = 5) +
#'   theme_timeline() +
#'   ggplot2::labs(size = "Richter scale value", fill = "# deaths")
#' }
#'
#' @export
geom_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                stat = 'timeline_label',
                                position = "identity",
                                show.legend = NA,
                                inherit.aes = TRUE,
                                xmin = NULL,
                                xmax = NULL,
                                n_max = NULL,
                                na.rm = FALSE,
                                ...) {

        ggplot2::layer(
            geom = GeomTimelineLabel,
            stat = stat,
            mapping = mapping,
            data = data,
            position = position,
            show.legend = show.legend,
            inherit.aes = inherit.aes,
            params = list(
              xmin = xmin,
              xmax = xmax,
              n_max = n_max,
              na.rm = na.rm,
              ...)
        )
}


#' @name GeomTimelineLabel
#' @title GeomTimelineLabel
#' @description geom_timeline_label-ggproto. See \code{\link{geom_timeline_label}}.
#' @import ggplot2
#' @import grid
GeomTimelineLabel <- ggplot2::ggproto( "GeomTimelineLabel", ggplot2::Geom,
        required_aes = c("x", "label"),
        default_aes = ggplot2::aes(y = NULL, measure = NULL, alpha = 0.3, color = "grey", fill = "grey", lty = 1, lwd = 1),
        draw_key = ggplot2::draw_key_blank,
        draw_group = function(data, panel_params, coord) {
          coords <- coord$transform(data, panel_params)
          grid::gList(
            grid::segmentsGrob(
              x0 = coords$x,
              y0 = coords$y,
              x1 = coords$x,
              y1 = coords$y + .1,
              gp = grid::gpar(
                col = "lightgrey",
                lty = coords$lty,
                lwd = coords$lwd
              )
            ),
            grid::textGrob(
              label = coords$label,
              x = coords$x,
              y = coords$y + .11,
              just = c('left', 'center'),
              rot = 45,
              gp = grid::gpar(
                fontsize = grid::unit(10, "char")
              )
            )
          )
        }

)

#' @name eq_map
#'
#' @title eq_map
#'
#' @description A function to create a leaflet map of earthquakes and annotations.
#'
#' @details \code{eq_map} creates a map with the epicenters
#' (\code{LATITUDE} & \code{LONGITUDE}) of selected earthquakes and annotates
#' each point with a pop up window containing annotation data stored in a
#' column of the data frame (e.g. \code{DATE})
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @param data A cleaned version of a NOAA data frame
#' @param annot_col A character. The column of the data frame for which to annotate the
#' pop up window for the selected earthquake.
#'
#' @return A leaflet map with earthquakes and annotations.
#'
#' @importFrom leaflet leaflet
#' @importFrom leaflet addTiles
#' @importFrom leaflet addCircleMarkers
#' @importFrom dplyr filter
#' @importFrom lubridate year
#'
#' @examples
#' \dontrun{
#' NOAAdf <- eq_clean_data("signif.txt")
#' NOAAdf %>%
#'         dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'         eq_map(annot_col = "DATE")
#' }
#'
#' @export
eq_map <- function(data, annot_col) {
        map <- data %>%
                leaflet::leaflet() %>%
                leaflet::addTiles() %>%
                leaflet::addCircleMarkers(lng = ~LONGITUDE,
                                          lat = ~LATITUDE,
                                          radius = ~EQ_PRIMARY,
                                          weight = 1,
                                          popup = data[[annot_col]])

        map
}


#' @name eq_create_label
#'
#' @title eq_create_label
#'
#' @description A function to create labels for popups of earthquakes in leaflet maps.
#'
#' @details \code{eq_create_label} pust together a character string for each earthquake
#' that will show the cleaned location (\code{LOCATION_NAME}, as cleaned by \code{eq_location_clean}),
#' the magnitude (e.g., \code{EQ_PRIMARY}), and the total number of deaths (e.g.,
#' \code{TOTAL_DEATHS}), with boldface labels for each. If an earthquake has missing
#' values for any of these, both the label and the value should be skipped.
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @param data A cleaned version of a NOAA data frame
#'
#' @return A character vector of strings in html format to label the popups in
#'   \code{\link{eq_map}}
#'
#' @importFrom dplyr filter
#' @importFrom dplyr mutate
#' @importFrom lubridate year
#'
#' @examples
#' \dontrun{
#' NOAAdf <- eq_clean_data("signif.txt")
#' NOAAdf %>%
#'         dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
#'         dplyr::mutate(popup_text = eq_create_label(.)) %>%
#'         eq_map(annot_col = "popup_text")
#' }
#'
#' @export
eq_create_label <- function(data) {
        popup_labels <- with(data, {
                label1 <- ifelse(is.na(LOCATION_NAME), "",
                                paste("<strong>Location: </strong>",
                                      LOCATION_NAME))
                label2 <- ifelse(is.na(EQ_PRIMARY), "",
                                paste("<strong>Magnitude: </strong>",
                                      EQ_PRIMARY))
                label3 <- ifelse(is.na(TOTAL_DEATHS), "",
                                paste("<strong>Total deaths: </strong>",
                                      TOTAL_DEATHS))
                paste(label1, label2, label3, sep = '<br/>')
        })

}

### Accessory functions

#' @name theme_timeline
#' @title theme_timeline
#' @description An accessory function to format the theme of \code{geom_timeline}
#' earhtquakes plots as described in the instructions
#' @import ggplot2
#' @export
theme_timeline <- function() {
        ggplot2::theme(
                plot.background = ggplot2::element_blank(),
                panel.background = ggplot2::element_blank(),
                legend.key = ggplot2::element_blank(),
                axis.title.x = element_text(face = "bold"),
                axis.title.y = ggplot2::element_blank(),
                axis.line.x = ggplot2::element_line(size = 1),
                axis.line.y = ggplot2::element_blank(),
                axis.ticks.y = ggplot2::element_blank(),
                legend.position = "bottom"
        )
}

#' @name stat_timeline
#'
#' @title stat_timeline
#'
#' @description A stat to enable \code{\link{geom_timeline}} to use date ranges
#' as parameters
#'
#' @details  \code{stat_timeline} transforms data passed to extract and plot
#' earthquakes between dates \code{xmin} and \code{xmax} passed as parameters.
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_timeline
#'
#' @return
#'
#' @importFrom ggplot2 layer
stat_timeline <- function(mapping = NULL,
                          data = NULL,
                          geom = 'timeline',
                          position = "identity",
                          show.legend = NA,
                          inherit.aes = TRUE,
                          xmin = NULL,
                          xmax = NULL,
                          na.rm = FALSE,
                          ...) {
        ggplot2::layer( geom = geom,
                        stat = StatTimeline,
                        mapping = mapping,
                        data = data,
                        position = position,
                        show.legend = show.legend,
                        inherit.aes = inherit.aes,
                        params = list(xmin = xmin,
                                      xmax = xmax,
                                      na.rm = na.rm,
                                      ...)
        )
}

#' @name StatTimeline
#' @title StatTimeline
#' @import ggplot2
#' @description An accessory stat to range dates between \code{xmin} and \code{xmax}
StatTimeline <- ggplot2::ggproto('StatTimeline', ggplot2::Stat,
                                 required_aes = c('x'),
                                 compute_group = function(data, scales, params, xmin, xmax) {
                                         if(!('y' %in% names(data))) {
                                                 data$y <- 1
                                         }
                                         return(data[data$x >= xmin & data$x <= xmax,])
                                 }
)

#' @name stat_timeline_label
#'
#' @title stat_timeline_label
#'
#' @description A stat to enable \code{\link{geom_timeline}} to plot date between
#' the ranges  \code{xmin} and \code{xmax}, as well as to label top \code{n_max}
#'  events
#'
#' @references
#'   \url{https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}
#'
#' @inheritParams ggplot2::stat_identity
#' @inheritParams geom_timeline
#' @inheritParams geom_timeline_label
#'
#' @import ggplot2
stat_timeline_label <- function(mapping = NULL,
                                data = NULL,
                                geom = 'timeline_label',
                                position = 'identity',
                                show.legend = NA,
                                inherit.aes = TRUE,
                                xmin = NULL,
                                xmax = NULL,
                                n_max = NULL,
                                na.rm = FALSE,
                                ...) {
        ggplot2::layer(geom = geom,
                       stat = StatTimelineLabel,
                       mapping = mapping,
                       data = data,
                       position = position,
                       show.legend = show.legend,
                       inherit.aes = inherit.aes,
                       params = list(xmin = xmin,
                                     xmax = xmax,
                                     n_max = n_max,
                                     na.rm = na.rm,
                                     ...)
        )
}

#' @name StatTimelineLabel
#' @title StatTimelineLabel
#' @description This function  creates the ggproto object for
#' \code{\link{stat_timeline_label}} to extract data according to \code{n_max},
#' \code{xmin} and \code{xmax} and measure and plot the corrresponding grid grob
#' for \code{\link{stat_timeline}}
#' @import ggplot2
StatTimelineLabel <- ggplot2::ggproto('StatTimelineLabel',ggplot2::Stat,
                                      required_aes = c('x'),
                                      compute_group = function(data, scales, params, n_max, xmin, xmax) {
                                              if(!('y' %in% names(data))) {
                                                      data$y <- 1
                                              }
                                              if(!is.null(xmin)) {
                                                      data <- data[data$x >= xmin, ]
                                              }
                                              if(!is.null(xmax)) {
                                                      data <- data[data$x <= xmax, ]
                                              }
                                              if(is.null(n_max) | !('measure' %in% names(data))){
                                                      return(data)
                                              } else {
                                                      data <- data[order(data$measure, decreasing = TRUE),][1:min(n_max,nrow(data)),]
                                                      return(data)
                                              }
                                      }
)
