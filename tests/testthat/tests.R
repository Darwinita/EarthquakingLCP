library(EarthquakingLCP)
context("Test functions in the package EarthquakingLCP")
library(dplyr)
library(ggplot2)
library(lubridate)
library(leaflet)


filename <- system.file("extdata", "signif.txt", package = "EarthquakingLCP")
# NOAAdf <- eq_clean_data(filename)
NOAAdfSp <- NOAAdf %>%
        dplyr::filter(COUNTRY %in% c("SPAIN", "IRAN", "BELGIUM"))

xmin = lubridate::ymd_hm("1016-01-01",truncated = 2)
xmax = lubridate::ymd_hm("2016-01-01",truncated = 2)



test_that("eq_clean_data returns a data frame, DATE is POSIXct class, and LATITUDE, LONGITUDE, EQ_PRIMARY,TOTAL_DEATHS and YEAR are numeric", {
        expect_is(eq_clean_data(filename), c("tbl_df","tbl","data.frame"))
        expect_is(eq_clean_data(filename)$DATE, c("POSIXct","POSIXt"))
        expect_is(eq_clean_data(filename)$LATITUDE, "numeric")
        expect_is(eq_clean_data(filename)$LONGITUDE, "numeric")
        expect_is(eq_clean_data(filename)$EQ_PRIMARY, "numeric")
        expect_is(eq_clean_data(filename)$TOTAL_DEATHS, "numeric")
        expect_is(eq_clean_data(filename)$YEAR, "numeric")
})


test_that("eq_location_clean returns a data frame and a character LOCATION_NAME", {
        expect_is(eq_location_clean(NOAAdf), c("tbl_df","tbl","data.frame"))
        expect_is(eq_location_clean(NOAAdf)$LOCATION_NAME, "character")
})

test_that("geom_timeline returns an object of ggplot class", {
        ggallin1 <- ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
                                                           y = COUNTRY)
        ) +
                geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
                                                            fill = TOTAL_DEATHS),
                                               xmin = xmin, xmax = xmax) +
                ggplot2::labs(size = "Richter scale value", color = "# deaths")
        expect_is(ggallin1, c("gg", "ggplot"))
})

test_that("geom_timeline_label returns an object of ggplot class", {
        ggallin2 <- ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
                                                                 y = COUNTRY)
        ) +
                geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
                                                            fill = TOTAL_DEATHS),
                                               xmin = xmin, xmax = xmax) +
                geom_timeline_label(aes(label = LOCATION_NAME, measure = EQ_PRIMARY),
                                                     xmin = xmin, xmax = xmax, n_max = 5) +
                ggplot2::labs(size = "Richter scale value", color = "# deaths")
        expect_is(ggallin2, "ggplot")
})

test_that("theme_timeline returns an object of ggplot class", {
        ggallin3 <- ggplot2::ggplot(data = NOAAdfSp, ggplot2::aes(x = DATE,
                                                                  y = COUNTRY)
        ) +
                geom_timeline(ggplot2::aes(size = EQ_PRIMARY,
                                                            fill = TOTAL_DEATHS),
                                               xmin = xmin, xmax = xmax) +
                geom_timeline_label(aes(label = LOCATION_NAME, measure = EQ_PRIMARY),
                                                     xmin = xmin, xmax = xmax, n_max = 5) +
                theme_timeline() +
                ggplot2::labs(size = "Richter scale value", color = "# deaths")
        expect_is(ggallin3, c("gg", "ggplot"))
})

test_that("eq_map returns an object of leaflet class", {
        leaflet <- NOAAdf %>%
                dplyr::filter(COUNTRY == "MEXICO" & lubridate::year(DATE) >= 2000) %>%
                eq_map(annot_col = "DATE")
        expect_is(leaflet, c("leaflet", "htmlwidget"))
})

test_that("eq_create_label returns character vector", {
        expect_is(eq_create_label(NOAAdf), "character")
})
