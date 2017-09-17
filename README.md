---
title: "model.plot"
author: "Hayley Arader"
output: html_document
---



# Welcome!
Welcome to `model.plot`! `model.plot` is an R package designed to make it easier to plot, interact with, and interpret your basic model results. 

# Installation

```r
devtools::install_github("harader/model.plot")
libary(model.plot)
```

# Usage
Let's load some data from the ISLR package and play around


```r
library(ISLR)
summary(College)
```

This looks like an interesting data set about colleges! Let's subset to only private colleges, and run a regression on out of state tuition:


```r
my_model <- lm(Outstate ~ perc.alumni + Grad.Rate + Top10perc + PhD + Terminal, data = College[College$Private == "Yes", ])

# Standard plot
plot(my_model)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)

Cool! Now let's say we want to see all variables, not just where p < .05

```r
plot(my_model, p = 1)
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

And now a bar plot...

```r
bar_plot(my_model)
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png)

And a table...

```r
stat_table(my_model)
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

And a histogram of residuals...

```r
plot_residuals(my_model)
```

```
## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)
Woohoo! Happy Plotting!
