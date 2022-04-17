
<!-- README.md is generated from README.Rmd. Please edit that file -->

# mortality

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
<!-- badges: end -->

This R-package fetches and organises the human mortality data from the
[Human Mortality Database](mortality.org) in a tidy fashion. For a more
richer and complete package, you should have a look at the `demography`
package.

The `mortality` package is a modern re-imagination and extension of the
functions `hmd.mx`, `hmd.e0`, and `hmd.pop` in the `demography` package.
Specifically:

-   import of `RCurl` is replaced by `curl` where the latter is a modern
    version of the former with zero import,
-   the fetched data is a special type of `tibble`, called `humble`
    (think human life is short so stay humble – okay, I’ll see my way
    out 😅), instead of a list of class `demogdata`,
-   it grabs data that are more than just period 1x1, and
-   <strike>it provides a helper function to convert `demogdata` to a
    `humble` table. </strike> (not implemented yet)

## Installation

You can install the development version of mortality as below:

``` r
install.packages("remotes")
remotes::install_github("emitanaka/mortality")
```

## Usage

``` r
library(mortality)
```

You first need to [register as a
user](https://www.mortality.org/mp/auth.pl) (if you haven’t already) at
the Human Mortality Database. To now set a session with your username
and password, fill in your details below and run the code.

``` r
hmd_session(username = <YOUR USERNAME>,
            password = <YOUR PASSWORD>)
```

Alternatively, you can store your username and password in the
`.Renviron` file containing below:

    HMD_USERNAME=<YOUR USERNAME>
    HMD_PASSWORD=<YOUR PASSWORD>

then simply just run the command before just once before getting the
data:

``` r
hmd_session()
```

All data are obtained via `hmd_data()`.

``` r
hmd_data("AUS", stats = "death")
#> # A humble:  10,989 x 6
#> # Countries: AUS
#> # Year:      1921-2019
#> # Age:       0-110
#>     year   age death_female death_male death_total country
#>    <int> <int>        <dbl>      <dbl>       <dbl> <chr>  
#>  1  1921     0       3842.      5125.        8967. AUS    
#>  2  1921     1        719.       890.        1610. AUS    
#>  3  1921     2        330.       359.         689. AUS    
#>  4  1921     3        166.       250.         416. AUS    
#>  5  1921     4        190.       197.         387. AUS    
#>  6  1921     5        149.       153.         302. AUS    
#>  7  1921     6        150.       137.         287. AUS    
#>  8  1921     7        109.       118.         227. AUS    
#>  9  1921     8         81.0      114.         195. AUS    
#> 10  1921     9         78.0       82.2        160. AUS    
#> # … with 10,979 more rows
```

The function offers support for multiple countries:

``` r
hmd_data(c("AUS", "JPN"), stats = "death")
#> # A humble:  19,203 x 6
#> # Countries: AUS, JPN
#> # Year:      1921-2020
#> # Age:       0-110
#>     year   age death_female death_male death_total country
#>    <int> <int>        <dbl>      <dbl>       <dbl> <chr>  
#>  1  1921     0       3842.      5125.        8967. AUS    
#>  2  1921     1        719.       890.        1610. AUS    
#>  3  1921     2        330.       359.         689. AUS    
#>  4  1921     3        166.       250.         416. AUS    
#>  5  1921     4        190.       197.         387. AUS    
#>  6  1921     5        149.       153.         302. AUS    
#>  7  1921     6        150.       137.         287. AUS    
#>  8  1921     7        109.       118.         227. AUS    
#>  9  1921     8         81.0      114.         195. AUS    
#> 10  1921     9         78.0       82.2        160. AUS    
#> # … with 19,193 more rows
```

or multiple statistics:

``` r
hmd_data(c("AUS", "JPN"), stats = c("death", "death_rate"))
#> # A humble:  19,203 x 9
#> # Countries: AUS, JPN
#> # Year:      1921-2020
#> # Age:       0-110
#>     year   age country death_female death_male death_total deathrate_female
#>    <int> <int> <chr>          <dbl>      <dbl>       <dbl>            <dbl>
#>  1  1921     0 AUS           3842.     5125.       8967.            0.0600 
#>  2  1921     1 AUS            719.      890.       1610.            0.0121 
#>  3  1921    10 AUS             69.0      85.2       154.            0.00125
#>  4  1921   100 AUS              2         6.02        8.02          0.170  
#>  5  1921   101 AUS              5         4.01        9.01          0.560  
#>  6  1921   102 AUS              2         1           3             0.453  
#>  7  1921   103 AUS              0         1           1             0      
#>  8  1921   104 AUS              1         1           2             1.04   
#>  9  1921   105 AUS              0         0           0            NA      
#> 10  1921   106 AUS              0         0           0            NA      
#> # … with 19,193 more rows, and 2 more variables: deathrate_male <dbl>,
#> #   deathrate_total <dbl>
```

or relabeling of the countries like below:

``` r
hmd_data(c("Australia" = "AUS", "Japan" = "JPN"), stats = "population")
#> # A humble:  19,536 x 6
#> # Countries: Australia, Japan
#> # Year:      1921-2021
#> # Age:       0-110
#>    year    age pop_female pop_male pop_total country  
#>    <chr> <int>      <dbl>    <dbl>     <dbl> <chr>    
#>  1 1921      0     62848.   65851.   128699. Australia
#>  2 1921      1     57777.   60217.   117994. Australia
#>  3 1921      2     56941.   59047.   115988. Australia
#>  4 1921      3     58272.   60218.   118490. Australia
#>  5 1921      4     58719.   60773.   119492. Australia
#>  6 1921      5     59888.   61687.   121574. Australia
#>  7 1921      6     61034.   62333.   123366. Australia
#>  8 1921      7     59455.   60776.   120231. Australia
#>  9 1921      8     57736.   58806.   116542. Australia
#> 10 1921      9     56181.   57140.   113321. Australia
#> # … with 19,526 more rows
```

You can also get a different age or year range:

``` r
hmd_data(c("Australia" = "AUS", "Japan" = "JPN"), 
         stats = "exposure_to_risk", year_range = 5, age_range = 5)
#> # A humble:  840 x 6
#> # Countries: Australia, Japan
#>    year_range_5 age_range_5 exprisk_female exprisk_male exprisk_total country  
#>    <chr>        <chr>                <dbl>        <dbl>         <dbl> <chr>    
#>  1 1921-1924    0                  256600.      267063.       523663. Australia
#>  2 1921-1924    1-4                957177.      994721.      1951898. Australia
#>  3 1921-1924    5-9               1180057.     1213273.      2393330. Australia
#>  4 1921-1924    10-14             1096076.     1123037.      2219113. Australia
#>  5 1921-1924    15-19              968522.      997946.      1966469. Australia
#>  6 1921-1924    20-24              925053.      914989.      1840042. Australia
#>  7 1921-1924    25-29              941637.      894549.      1836186. Australia
#>  8 1921-1924    30-34              915723.      926174.      1841897. Australia
#>  9 1921-1924    35-39              806054.      842748.      1648803. Australia
#> 10 1921-1924    40-44              681851.      719764.      1401615. Australia
#> # … with 830 more rows
```

# Related packages

-   `demography`
-   `HMDHFDplus`
-   `MortalityLaws`
-   (developmental) `tidylife`
