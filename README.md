
<!-- README.md is generated from README.Rmd. Please edit that file -->

# JobSearchAssistance

The user benefit from an artificial intelligence to assist him in his
job search or paid assignments.

The tool allows you to collect, analyze and classify job offers
according to your own tastes. Matching an offer allows the algorithm to
know your preferences.

The project is divided into three phases:

  - Data collection using web scrapping with the get() function.
  - The app() function allows you to swap and classify the offers.
  - Finally, the algo() function analyze the remaining offers and
    predict a sensitivity score.

# Application

## Identify research using keywords

``` r
lapply(list.files("inst/", full.names = T), source)
```

``` r
params = list(
  user_id   = "tintin",
  mots_cles = c("data+scientist", "Statisticien")
  )
```

## Scrape a list of offers

``` r
get(mc = params$mots_cles, id = params$user_id)
```

## Manually classify (swap) the offers

``` r
app(id = params$user_id)
```

## Learn from the result and predict the classification of unclassified

``` r
algo(id = params$user_id)
```

## Offer the most likely to please offers

``` r
read_rds("data/top/prev.RDS") %>% 
  slice_head(n = 10)
#> # A tibble: 10 x 10
#>    id    title localisation desc  note  link  entreprise date       score_pred
#>    <chr> <chr> <chr>        <chr> <lgl> <chr> <chr>      <date>          <dbl>
#>  1 134   ÉCON~ Paris        prés~ NA    http~ <NA>       NA              0.905
#>  2 379   Ingé~ France       entr~ NA    http~ <NA>       2020-05-03      0.614
#>  3 344   DATA~ Paris        prés~ NA    http~ Banque de~ 2020-04-26      0.568
#>  4 260   Data~ 75 - Paris ~ data~ NA    http~ <NA>       NA              0.546
#>  5 382   Data~ 75 - Paris ~ data~ NA    http~ <NA>       2020-05-02      0.546
#>  6 396   Data~ 13 - MARSEI~ cons~ NA    http~ <NA>       2020-04-30      0.391
#>  7 412   Data~ 75 - PARIS ~ emag~ NA    http~ <NA>       2020-04-29      0.368
#>  8 332   DEVE~ 92 - ISSY L~ eure~ NA    http~ <NA>       2020-04-23      0.309
#>  9 328   Data~ 75 - Paris ~ desc~ NA    http~ <NA>       2020-04-24      0.287
#> 10 265   Data~ France       resu~ NA    http~ <NA>       NA              0.252
#> # ... with 1 more variable: note_pred <dbl>
```
