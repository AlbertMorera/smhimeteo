library(testthat)

test_that("get_smhi_station() throws an error for an invalid parameter", {
  expect_error(
    get_smhi_station("9999"),
    class = "smhi_invalid_parameter"
  )
})

test_that("get_smhi_period() throws errors for invalid station or parameter", {
  expect_error(
    get_smhi_period("invalid_station", "1"),
    class = "smhi_invalid_station"
  )
  expect_error(
    get_smhi_period("159880", "invalid_param"),
    class = "smhi_invalid_parameter"
  )
})

test_that("get_smhi_data() throws an error for invalid period", {
  expect_error(
    get_smhi_data("159880", "1", "not-a-period"),
    class = "smhi_invalid_period"
  )
})

test_that("get_smhi_data() throws an error for an invalid parameter (not existing)", {
  expect_error(
    get_smhi_data("159880", "9999", "latest-day"),
    class = "smhi_invalid_parameter"
  )
})

test_that("get_smhi_data2() throws errors like get_smhi_data()", {
  fake_tbl <- tibble::tibble(
    station_id = "invalid_station",
    parameter_id = "1"
  )
  expect_error(
    get_smhi_data2(fake_tbl, "latest-day"),
    class = "smhi_invalid_station"
  )
})

test_that("get_smhi_closest() warns when no stations are found within the radius", {
  pt <- sf::st_sf(geometry = sf::st_sfc(sf::st_point(c(0, 90)), crs = 4326))
  expect_warning(
    get_smhi_closest(pt, parameter_key = "1", period_name = "latest-day", max_radius = 1),
    class = "smhi_no_stations"
  )
})

test_that("check_valid_period() throws an error for an invalid period", {
  expect_error(
    check_valid_period("bad-period"),
    class = "smhi_invalid_period"
  )
})
