
# Assistant de recherche d'emploi

L'utilisateur bénéficie d'une intelligence artificielle pour l'aider dans sa recherche d'emploi ou de missions rémunérées.

L'outil permet de collecter, analyser et classer les offres d'emploi selon ses propres goûts. Correspondre à une offre permet à l'algorithme de connaître vos préférences.

Le projet est divisé en trois phases :

- Collecte de données en utilisant le web scraping avec la fonction get().
- La fonction app() permet de faire des glisser-déposer et de classer les offres.
- Enfin, la fonction algo() analyse les offres restantes et prédit un score de sensibilité.
The project is divided into three phases:

# Application

## Identification de la recherche à l'aide de mots clés

``` r
lapply(list.files("inst/", full.names = T), source)
```

``` r
params = list(
  user_id   = "tintin",
  mots_cles = c("data+scientist", "Statisticien")
  )
```

## Scraper une liste d'offres

``` r
get(mc = params$mots_cles, id = params$user_id)
```

## Classification manuelle (glisser-déposer) des offres

``` r
app(id = params$user_id)
```

## Apprendre des résultats et prédire la classification des non classifiés

``` r
algo(id = params$user_id)
```

## Restituer les offres les plus susceptibles de plaire

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
