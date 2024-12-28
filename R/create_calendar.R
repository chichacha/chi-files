#' Generate a Custom Calendar Data Frame
#'
#' This function creates a detailed calendar data frame for a specified year 
#' and calendar type (either 4-4-5 or 4-5-4 format). The resulting data frame 
#' includes additional metadata such as weeks, periods, and quarters.
#'
#' @param year Integer. The year for which the calendar should be generated.
#' @param calendar_type Character. The calendar type to use: either "445" 
#'   (4-4-5 calendar) or "454" (4-5-4 calendar). Defaults to "445".
#' @param col_pal Character vector. A custom color palette for visualizing 
#'   weeks and periods. Defaults to a Tableau-inspired color palette.
#' @param ... Additional arguments for future extensions (currently unused).
#'
#' @return A tibble with the following columns:
#'   \describe{
#'     \item{dt}{Date column representing each day of the year.}
#'     \item{yr}{ISO year corresponding to the date.}
#'     \item{wk}{ISO week number.}
#'     \item{wday}{Day of the week as a factor (e.g., Monday, Tuesday).}
#'     \item{p}{Period number in the selected calendar format.}
#'     \item{q}{Quarter number in the selected calendar format.}
#'     \item{color_num}{Numeric code representing weeks and periods.}
#'     \item{color_wk}{Color value for visualization based on `col_pal`.}
#'   }
#' @export
#'
#' @examples
#' # Generate a 4-4-5 calendar for 2025
#' create_calendar(year = 2025, calendar_type = "445")
#'
#' # Generate a 4-5-4 calendar with a custom color palette
#' custom_palette <- c("#FF5733", "#33FF57", "#3357FF")
#' create_calendar(year = 2025, calendar_type = "454", col_pal = custom_palette)
create_calendar <- function(year, calendar_type = "445", col_pal = NULL, ...) {
  if (is.null(col_pal)) {
    col_pal <- ggthemes::tableau_color_pal("Hue Circle")(19)[c(1:13)]
  }
  
  cal <- tibble(
    dt = seq.Date(ymd(paste0(year-1, "-12-01")), ymd(paste0(year, "-12-31")), by = "day")
  )
  
  wpq <- switch(calendar_type,
                "445" = tibble(
                  wk = 1:53,
                  p = sort(c(rep(1:12, each = 4), c(3, 6, 9, 12, 12))),
                  q = sort(c(rep(1:4, each = 13), c(4)))
                ),
                "454" = tibble(
                  wk = 1:53,
                  p = sort(c(rep(1:12, each=4), c(2, 5, 8, 11, 12))),
                  q = sort(c(rep(1:4, each = 13), c(4)))
                ),
                stop("Invalid calendar type. Choose '445' or '454'.")
  )
  
  cal <- cal |>
    mutate(
      yr = isoyear(dt),
      wk = isoweek(dt),
      wday = wday(dt, label = TRUE)
    ) |>
    left_join(wpq, by = "wk") |>
    mutate(color_num=wk + p + q) |>
    mutate(color_wk = colourvalues::color_values(color_num, 
                                                 palette=farver::decode_colour(col_pal))) |>
    filter(yr==year)
  
  return(cal)
}

