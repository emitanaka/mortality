
#' Convert the death rate humble data to demogdata
#'
#' Note that data should containly only one country or supply the country to
#' select in the `country` argument.
#'
#' @param data A humble object from `hmd_data()` with
#'  `stats = c("death_rate", "exposures_to_risk")`.
#' @param country A single country within the data. If `data` contains only
#'  a single country, then there is no need to supply this.
#'
#' @export
as_demogdata <- function(data, country = NULL) {
  if(is.null(country)) {
    if(length(unique(data$country)) > 1) abort("There is more than 1 country. Please select a country using the `country` argument.")
    data <- dat
  } else {
    if(length(country)!=1) abort("You can only supply one country!")
    if(!country %in% data$country) abort("The country you supplied is not in the data.")
    dat <- data[data$country==country, ]
  }
  # is the data wide form?
  if(any(grepl("female", colnames(dat)))) {
    dat <- dat[order(dat$year, dat$age),]
    year <- unique(dat$year)
    age <- unique(dat$age)
    n <- length(year)
    m <- length(age)
    structure(list(type = "mortality",
                   label = dat$country[1],
                   lambda = 0,
                   year = year,
                   age = age,
                   pop = list(female = matrix(dat$exprisk_female, m, n,
                                              dimnames = list(age, year)),
                              male = matrix(dat$exprisk_male, m, n,
                                            dimnames = list(age, year)),
                              total = matrix(dat$exprisk_total, m, n,
                                             dimnames = list(age, year))),
                   rate = list(female = matrix(dat$deathrate_female, m, n,
                                               dimnames = list(age, year)),
                               male = matrix(dat$deathrate_male, m, n,
                                             dimnames = list(age, year)),
                               total = matrix(dat$deathrate_total, m, n,
                                              dimnames = list(age, year)))),
              class = "demogdata")
  } else {
    idx_female <- which(dat$sex=="female")
    idx_male <- which(dat$sex=="male")
    idx_total <- which(dat$sex=="total")
    out <- data.frame(year = dat$year[idx_female],
               age = dat$age[idx_female],
               country = dat$country[1],
               deathrate_female = dat$deathrate[idx_female],
               deathrate_male = dat$deathrate[idx_male],
               deathrate_total = dat$deathrate[idx_total],
               exprisk_female = dat$exprisk[idx_female],
               exprisk_male = dat$exprisk[idx_male],
               exprisk_total = dat$exprisk[idx_total])
    as_demogdata(out, country)
  }
}

