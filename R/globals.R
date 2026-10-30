#' @importFrom utils globalVariables
NULL

# Declare global variables to avoid R CMD check NOTEs for dplyr column names
utils::globalVariables(c(
 "geoid",
  "is_rural",
  "value"
))
