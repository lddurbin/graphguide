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

## Given an input quantity n, this function will provide a vector of
## proportions for all values.
increase_by <- function(n) {
  return(
    c(triangle_increase(n), triangle_decrease(n))
  )
}

## Given a preferred quantity and a difference between start and end points,
## This function provides a "base unit" to increase values.
increase_level <- function(n, difference) {
  marginal_increase_value = sum(increase_by(n))
  level = difference/marginal_increase_value
  return(level)
}

## This function will provide the "increase level" that any subgroup will
## add from one value to the next.
create_increase_vector <- function(n, difference) {
  level = increase_level(n, difference)
  by = increase_by(n)
  return(level*by)
}

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
