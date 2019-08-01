#This program was written following Peter DeWitt's online tutorial
#On creating tidy summary statistic tables using qrwaps2
#https://cran.r-project.org/web/packages/qwraps2/vignettes/summary-statistics.html

set.seed(42)
library(magrittr)
library(qwraps2)

options(qwraps2_markup = "markdown")

mtcars2 <- dplyr::mutate(mtcars,
                         cyl_factor = factor(cyl,
                                             levels = c(6,4,8),
                                             labels = paste(c(6,4,8), "cylinders")),
                         cyl_character = paste(cyl, "cylinders"))

#This line will report the mean of a given variable along with its
#Standard deviation in parentheses
#mean_sd(mtcars2$mpg, denote_sd = 'paren')

#Creating a list of lists to report summary statistics in a table
our_summary1 <-
  list("Miles Per Gallon" =
         list("min" = ~ min(.data$mpg),
              "max" = ~ max(.data$mpg),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$mpg)),
       "Displacement" = 
         list("min" = ~ min(.data$disp),
              "median" = ~ median(.data$disp),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$disp)),
       "Weight (1000 lbs)" =
         list("min" = ~ min(.data$wt),
              "max" = ~ max(.data$wt),
              "mean (sd)" = ~ qwraps2::mean_sd(.data$wt)),
       "Forward Gears" = 
         list("Three" = ~ qwraps2::n_perc0(.data$gear == 3),
              "Four" = ~ qwraps2::n_perc0(.data$gear == 4),
              "Five" = ~ qwraps2::n_perc0(.data$gear == 5))
       )

#The function summary_table is used to build the actual function
whole <- summary_table(mtcars2, our_summary1)
print(whole)

#Applying the same summary statistics to the data grouped by number of cylinders
by_cyl <- summary_table(dplyr::group_by(mtcars2, cyl_factor), our_summary1)
print(by_cyl)

#Merging the two tables together
both <- cbind(whole, by_cyl)
print(both)

#Changing the table and column names
print(both,
      rtitle = "Summary Statistics",
      cnames=c("Col 0", "Col 1", "Col 2", "Col 3"))