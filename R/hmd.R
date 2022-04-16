
#' Set the username and password to access the Human Mortality Database
#'
#' @param username The username (typically your email) that you signed up for HMD.
#' @param password The password used to access HMD.
#'
#' @export
hmd_session <- function(username = Sys.getenv("HMD_USERNAME"),
                        password = Sys.getenv("HMD_PASSWORD")) {
  .hmd$username <- username
  .hmd$password <- password
  invisible(list(username = username, password = password))
}

hmd_handle <- function(username, password) {
  h <- curl::new_handle()
  curl::handle_setopt(handle = h,
                      httpauth = 1,
                      userpwd = paste0(username, ":", password))
  h

}

#' Fetch the data from the Human Mortality Database
#'
#' @param country The country code to fetch the data for. The input may be
#'   a named or unnamed character vector. If the vector is named, then the
#'   name is used as the label.
#' @param stats The statistics to extract. The available statistics are:
#'   "birth", "death", "life_expectancy", "exposure_to_risk", "population",
#'   and "life_tables".
#' @param range_year,range_age A single integer indicating the period or
#'   cohort year/age range.
#' @param user Either a named list of `username` and `password` to access the
#'  HMD or if `NULL` then the username and password set by `hmd_user` is
#'  used instead. The recommended approach is just to set the user session
#'  with `hmd_user()`.
#' @return A humble object.
#' @seealso hmd_birth
#' @export
hmd_data <- function(country,
                     stats = "death_rate",
                     year_range = 1,
                     age_range = 1,
                     user = NULL) {

  if(!is.null(user)) {
    hmd_session(user$username, user$password)
  }
  res <- map(stats, function(astat) {
    switch(astat,
           "birth" = hmd_birth(country),
           "death" = hmd_death(country, age_range, year_range),
           "life_expectancy" = hmd_life_expectancy(country, year_range),
           "exposure_to_risk" = hmd_exposure_to_risk(country, age_range, year_range),
           "population" = hmd_population(country, age_range),
           "life_tables" = hmd_life_table(country, age_range, year_range),
           "death_rate" = hmd_death_rate(country, age_range, year_range)
           )
  })
  new_humble(Reduce(merge, res))
}

hmd_life_expectancy <- function(country, year_range = 1) {
  country_labels <- names(country) %||% country
  period <- ifelse(year_range==1, "", paste0("_1x", year_range))
  filename <- paste0("E0per", period, ".txt")
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          filename,
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                     "lifeexp_female", "lifeexp_male", "lifeexp_total", "country")
  new_humble(res)
}

hmd_birth <- function(country) {
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          "Births.txt",
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c("year", "birth_female", "birth_male", "birth_total", "country")
  new_humble(res)
}

hmd_life_table <- function(country, age_range = 1, year_range = 1) {
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  # total
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("bltper_", period, ".txt"),
                                                          country_labels[i]))
  total <- do.call("rbind", out)
  idx <- 3:(ncol(total) - 1)

  colnames(total)[1:2] <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                          ifelse(age_range > 1, paste0("age_range_", age_range), "age"))
  colnames(total)[idx] <- paste0(colnames(total)[idx], "_total")
  # female
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("mltper_", period, ".txt"),
                                                          country_labels[i]))
  male <- do.call("rbind", out)
  colnames(male)[1:2] <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                            ifelse(age_range > 1, paste0("age_range_", age_range), "age"))
  colnames(male)[idx] <- paste0(colnames(male)[idx], "_male")
  # female
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("fltper_", period, ".txt"),
                                                          country_labels[i]))
  female <- do.call("rbind", out)
  colnames(female)[1:2] <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                            ifelse(age_range > 1, paste0("age_range_", age_range), "age"))
  colnames(female)[idx] <- paste0(colnames(female)[idx], "_female")

  new_humble(merge(merge(female, male), total))
}

hmd_population <- function(country, age_range = 1) {
  country_labels <- names(country) %||% country
  appendix <- ifelse(age_range==1, "", age_range)
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("Population", appendix),
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c("year",
                     ifelse(age_range > 1, paste0("age_range_", age_range), "age"),
                     "pop_female", "pop_male", "pop_total", "country")
  new_humble(res)
}

hmd_death <- function(country, age_range = 1, year_range = 1) {
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("Deaths_", period, ".txt"),
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                     ifelse(age_range > 1, paste0("age_range_", age_range), "age"),
                     "death_female", "death_male", "death_total", "country")
  new_humble(res)
}

hmd_exposure_to_risk <- function(country, age_range = 1, year_range = 1) {
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("Exposures_", period, ".txt"),
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                     ifelse(age_range > 1, paste0("age_range_", age_range), "age"),
                     "exprisk_female", "exprisk_male", "exprisk_total", "country")
  new_humble(res)
}

hmd_death_rate <- function(country, age_range = 1, year_range = 1) {
  period <- paste0(age_range, "x", year_range)
  country_labels <- names(country) %||% country
  out <- map(1:length(country), function(i) read_hmd_file(country[i],
                                                          paste0("Mx_", period, ".txt"),
                                                          country_labels[i]))
  res <- do.call("rbind", out)
  colnames(res) <- c(ifelse(year_range > 1, paste0("year_range_", year_range), "year"),
                     ifelse(age_range > 1, paste0("age_range_", age_range), "age"),
                     "deathrate_female", "deathrate_male", "deathrate_total", "country")
  new_humble(res)
}

read_hmd_file <- function(country, filename, label = country) {
  url <- url_mortality(country, filename)
  if(is.null(.hmd$username) | is.null(.hmd$password)) {
    abort("Use `hmd_session()` to set the username and password")
  }
  con <- curl::curl(url, handle = hmd_handle(.hmd$username, .hmd$password))
  open(con)
  data <- utils::read.table(con, skip = 2, header = TRUE, na.strings = ".")
  close(con)
  if("Age" %in% colnames(data) && !any(grepl("-", data$Age))) {
    data$Age[data$Age=="110+"] <- "110"
    data$Age <- as.integer(data$Age)
  }
  data$country <- label
  data
}

url_mortality <- function(country, filename) {
  paste0("https://mortality.org/hmd/", country, "/STATS/", filename)
}

#' @export
new_humble <- function(.data, ..., class = NULL) {
  tibble::new_tibble(.data, ..., class = c(class, "humble"))
}

#' @importFrom tibble tbl_sum
#' @export
tbl_sum.humble <- function(.data) {
  res <- c("A humble" = dim_desc(.data),
           "Countries" = paste(unique(.data$country), collapse = ", "))
  if("year" %in% colnames(.data)) {
    res <- c(res, "Year" = paste0(min(.data$year), "-", max(.data$year)))
  }
  if("age" %in% colnames(.data)) {
    res <- c(res, "Age" = paste0(min(.data$age), "-", max(.data$age)))
  }

  res
}

