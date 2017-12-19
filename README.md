# EarthquakingLCP

The R package `EarthquakingLCP` implements seven exported useful functions (plus four internal unimported ones) to process and analyze data from the **U.S. National Oceanographic and Atmospheric Administration (NOAA)** on signicant earthquakes around the world. Please, visit [Significant Earthquake Database]({https://www.ngdc.noaa.gov/nndc/struts/form?t=101650&s=1&d=1}) for more informartion on the NOAA dataset. The functions are the following:

1. `eq_clean_data()`: A read data file function into a cleaned version of a data frame.
2. `eq_location_clean()`: A function to clean the `LOCATION_NAME` column from the
NOAA data frame.
3. `geom_timeline()`: A function to plot a timeline of earthquakes.
4. `geom_timeline_label()`: A geom for labelling earthquake timeline plots created using
 `geom_timeline()`.
5. `eq_map()`: A function to create a leaflet map of earthquakes and annotations.
6. `eq_create_label()`: A function to create labels for popups of earthquakes in leaflet maps.
7. `theme_timeline`: An accessory function to format the theme of `geom_timeline` earhtquakes plots.

The R package `EarthquakingLCP` has the following main goals:

* Obtaining/downloading the dataset and cleaning up some of the variables.

* Visualize some of the information in the NOAA earthquakes dataset, i.e.,  i) times at which earthquakes occur within certain countries, and ii) their magnitudes (i.e. Richter scale value) and the number of deaths associated with each earthquake.

* Mapping the earthquake epicenters and providing some annotations with the mapped data.

## Example

Obtaining/downloading the dataset and cleaning up some of the variables:

```{r example_1, eval=FALSE}
NOAAdf <- EarthquakingLCP::eq_clean_data(filename)
cleanNOAAdf <- EarthquakingLCP::eq_location_clean(NOAAdf)
summary(cleanNOAAdf)
```

Visualizing some of the information in the NOAA earthquakes dataset, i.e.,  i) times at which earthquakes occur within certain countries, and ii) their magnitudes (i.e. Richter scale value) and the number of deaths associated with each earthquake.

```{r example_2, eval=FALSE}
xmin = lubridate::ymd_hm("1016-01-01",truncated = 2)
xmax = lubridate::ymd_hm("2016-01-01",truncated = 2)
NOAAdf <- EarthquakingLCP::eq_clean_data(filename)
NOAAdfSp <- NOAAdf %>%
   dplyr::filter(COUNTRY %in% c("SPAIN", "IRAN", "BELGIUM"))
 ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
                             y = COUNTRY)
                 ) +
   EarthquakingLCP::geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
                     fill = TOTAL_DEATHS),
                 xmin = xmin, xmax = xmax) +
   EarthquakingLCP::geom_timeline_label(aes(label = LOCATION_NAME, measure = EQ_PRIMARY),
        xmin = xmin, xmax = xmax, n_max = 5) +
   EarthquakingLCP::theme_timeline() +
   ggplot2::labs(size = "Richter scale value", color = "# deaths")
```

Mapping the earthquake epicenters and providing some annotations with the mapped data.

```{r example_3, eval=FALSE}
NOAAdf <- EarthquakingLCP::eq_clean_data(filename)
NOAAdf %>%
         dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
         dplyr::mutate(popup_text = EarthquakingLCP::eq_create_label(.)) %>%
         EarthquakingLCP::eq_map(annot_col = "popup_text")
```

