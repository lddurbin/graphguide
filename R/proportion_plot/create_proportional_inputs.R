

library(dplyr)
library(magrittr)
library(ggplot2)

## Given an input quantity n, this function will provide a vector of
## proportions increasing at an increasing rate, until the inflection point
triangle_increase <- function(n) {
  if (n %% 2 == 0) {
    increase = choose(seq(n/2), 2)
  } else {
    increase = choose(seq((n+1)/2), 2)
  }
  return(increase)
}

## Given an input quantity n, this function will provide a vector of
## proportions increasing at a decreasing rate, following the inflection point  
triangle_decrease <- function(n) rev(triangle_increase(n))

## These are test calls to ensure the function acts as intended
triangle_increase(16)
triangle_decrease(15)

## Given an input quantity n, this function will provide a vector of
## proportions for all values.
increase_by <- function(n) {
  return(
    c(triangle_increase(n), triangle_decrease(n))
  )
}

## This is a test call to ensure the function acts as intended
increase_by(15)

## Given a preferred quantity and a difference between start and end points,
## This function provides a "base unit" to increase values.
increase_level <- function(n, difference) {
  marginal_increase_value = sum(increase_by(n))
  level = difference/marginal_increase_value
  return(level)
}

## Test
increase_level(15, 42)

## This function will provide the "increase level" that any subgroup will
## add from one value to the next.
create_increase_vector <- function(n, difference) {
  level = increase_level(n, difference)
  by = increase_by(n)
  return(level*by)
}

## Test
create_increase_vector(16, 42)

## This function will create a set of values that can be shown in a plot.
create_slope_vector <- function(num_values, start_value, end_value) {
  total_difference = abs(start_value - end_value)
  increase_vector = create_increase_vector(num_values, total_difference)
  slope_vector = c()
  current_value = start_value
  for (i in 1:num_values) {
    current_value = current_value + increase_vector[i]
    slope_vector = c(slope_vector, current_value)
  }
  return(slope_vector)
}

## Test
create_slope_vector(num_values = 16, start_value = 24, end_value = 66)

## This is the main function
## This function consumes create_slope_vector (immediately above)
## The output will provide start, end, and intermediate points for each group,
## using a combination of rate of change as well as subgroups below this group.
create_slope_values <- function(num_values, start_value, end_value, baseline = 0) {
  slope_values = create_slope_vector(
    num_values = num_values,
    start_value = start_value + baseline,
    end_value = end_value + baseline
  )
}

## Unlike the other functions, this function will create an output vector
## of values that change at a constant rate. This is necessary for x values.
create_constant_vector <- function(num_values, start_value, end_value) {
  total_difference = abs(start_value - end_value)
  iteration_increase = total_difference/(num_values - 1)
  constant_vector = c()
  for (i in 1:num_values) {
    if (i == 1) {
      current_value = start_value
    } else {
      current_value = current_value + iteration_increase
    }
    constant_vector = c(constant_vector, current_value)
  }
  return(constant_vector) 
}

##############################################################################
## Sample Application
## 
## The code above is what we use to create values for the proportion plot
## from any input data. The code below will apply the functions above, showing
## how simple it is to apply these functions using your own datasets.
other_race_start = 6
other_race_end = 15

caucasian_start = 17
caucasian_end = 60

hispanic_start = 33
hispanic_end = 12

black_start = 44
black_end = 23

## At the "visually lowest stage" of the plot, values are drawn from zero (e.g. baseline)
other_race_values = create_slope_values(
  num_values = 16, 
  start_value = other_race_start, 
  end_value = other_race_end,
  baseline = 0
)

## We should produce values based on "start" and "end points" on the graph
## but we also need to make a note that these values are above "other race values"
caucasian_values = create_slope_values(
  num_values = 16, 
  start_value = caucasian_start, 
  end_value = caucasian_end,
  baseline = other_race_start
)

## We should produce values based on "start" and "end points" on the graph
## but we also need to make a note that these values are above two other groups
hispanic_values = create_slope_values(
  num_values = 16, 
  start_value = hispanic_start, 
  end_value = hispanic_end,
  baseline = other_race_start + caucasian_start
)

## Based on the order, we know that all y coordinates will be 100
black_values = create_slope_values(
  num_values = 16,
  start_value = 100,
  end_value = 100
)

## The x coordinates should be evenly spaced, unlike y coordinates
x_coordinates = create_constant_vector(
  num_values = 16,
  start_value = 0,
  end_value = 100
)

## This is the final data.frame from which all inputs are provided
input_df <- data.frame(
  x_percent = x_coordinates,
  other_races_y_value = other_race_values,
  caucasian_y_value = caucasian_values,
  hispanic_y_value = hispanic_values,
  black_y_value = black_values
)


