Analyzing Forest Fire Data
================
Eriz Tolay
9/14/2021

## Analyzing Forest Fire Data

Forest fires can create ecological problems and endanger human lives and
property. Understanding when they occur and what causes them is
important for managing them. The data we’ll be working with in this
project is associated with a scientific research paper on predicting the
occurrence of forest fires in Portugal using modeling techniques.

``` r
forest_fires <- read_csv("forestfires.csv")
```

    ## Rows: 517 Columns: 13

    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr  (2): month, day
    ## dbl (11): X, Y, FFMC, DMC, DC, ISI, temp, RH, wind, rain, area

    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

`month` and `day` are character vartiables, but we know that there is an
inherent order to them. We’ll convert these variables into factors so
that they’ll be sorted into the correct order when we plot them.

This project will assume that Sunday is the first day of the week.

``` r
month_order <- c("jan", "feb", "mar",
                 "apr", "may", "jun",
                 "jul", "aug", "sep",
                 "oct", "nov", "dec")
dow_order <- c("sun", "mon", "tue", "wed", "thu", "fri", "sat")
forest_fires <- forest_fires %>% 
  mutate(
    month = factor(month, levels = month_order),
    day = factor(day, levels = dow_order)
  )
```

When it comes to understanding forest fires and what can be done to
manage them, it is helpful to have an idea of when the fires are most
likely to occur.

We’ve seen two variables concerning time: month and day. These two
columns allow us to ask:

*Which months do forest fires happen the most? *Which days of the week
do forest fires happen the most?

``` r
fires_month <- forest_fires %>%
  group_by(month) %>%
  summarise(
    count = n()
  )

fires_day <- forest_fires %>%
  group_by(day) %>%
  summarise(
    count = n()
  )
```

``` r
fires_month %>%
  ggplot(aes(x=month, y= count)) +
  geom_col() +
  labs(
    title = "Number of Fires per month",
    x = "Month",
    y = "Total Fires"
  )
```

![](Analyzing-Forest-Fires_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

As it is clear in the chart, most fires seem to happen during the months
of August and September.

``` r
fires_day %>%
  ggplot(aes(x=day, y= count)) +
  geom_col() +
  labs(
    title = "Number of Fires per day",
    x = "Day",
    y = "Total Fires"
  )
```

![](Analyzing-Forest-Fires_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

When it comes to the days of the week, the risk of fire is significantly
higher on Sundays, Fridays and Saturdays.

To explore the temporal patterns of forest fire occurrence the bar
charts reveal, we should look more closely at how the variables that
relate to forest fires vary by month and by day of the week. We should
see how each of the other variables in the dataset relates to month.
We’ll exclude day for now since it’s really the months that can vary a
lot between seasons.

``` r
forest_fires_month <- forest_fires %>%
  pivot_longer(
    cols = c("FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain"),
    names_to = "variables",
    values_to = "values"
  )

forest_fires_month %>% 
  ggplot(aes(x = month, y = values)) +
  geom_boxplot() +
  facet_wrap(vars(variables), scale = "free_y") +
  labs(
    title = "Variable changes over month",
    x = "Month",
    y = "Variable value"
  )
```

![](Analyzing-Forest-Fires_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

Looks like Tempereature, DC(Drought Code Index) and DMC(Duff Moisture
Code index) are directly related with the number of fires in the summer
months.

Let’s investigate further! So far, we’ve only looked at the relationship
between the variables in the data and the frequency of forest fires.
Fires can also range in intensity too, so it might be useful to know
what factors influence this as well.

Looking at the data immediately though, there is no variable that
describes just “severity”. In this data set, the area variable contains
data on the number of hectares of forest that burned during the forest
fire. We’ll use this variable as an indicator of the severity of the
fire. The idea behind using area as a proxy is that worse fires will
result in a larger burned area. Of course, this won’t be true in all
cases, but it is a reasonable assumption to make.

``` r
forest_fires_month %>% 
  ggplot(aes(x = values, y = area)) +
  geom_point() +
  facet_wrap(vars(variables), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```

![](Analyzing-Forest-Fires_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

It’s hard to understand relationships between variables when you look at
these plots. There are a few points representing very large values of
area, and many points representing values of area that are zero or close
to zero. As a result, most points are clustered around the bottom of the
plots.

``` r
forest_fires_month %>%  
  filter(area < 300) %>% 
  ggplot(aes(x = values, y = area)) +
  geom_point() +
  facet_wrap(vars(variables), scales = "free_x") +
  labs(
    title = "Relationships between other variables and area burned (area < 300)",
    x = "Value of column",
    y = "Area burned (hectare)"
  )
```

![](Analyzing-Forest-Fires_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

After the outlier is removed, we can see the true relationship of the
features and the area of forest fire

Category of relationship with forest fire area (revised):

1.  Positive strong : FFMC
2.  Positive weak : DC and temp
