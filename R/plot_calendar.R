#' Plot a Custom Calendar
#'
#' This function visualizes a calendar generated by the `create_calendar` function. 
#' The calendar is displayed as a grid with weeks and days, highlighting periods, 
#' quarters, and color-coded weeks for clarity.
#'
#' @param cal A tibble containing calendar data, such as the output from the `create_calendar` function. 
#'   The data should include columns like `wday`, `color_num`, `color_wk`, `dt`, `wk`, and `p`.
#'
#' @returns A ggplot2 object visualizing the calendar as a tiled grid.
#' 
#' @export
#'
#' @examples
#' # Example: Plot a 4-4-5 calendar for 2025
#' calendar_445 <- create_calendar(year = 2025, calendar_type = "445")
#' plot_calendar(calendar_445)
#'
#' # Example: Plot a 4-5-4 calendar for 2025
#' calendar_454 <- create_calendar(year = 2025, calendar_type = "454")
#' plot_calendar(calendar_454)
plot_calendar <- function(cal) {
  cal |>
    ggplot(aes(x=wday,y=color_num)) +
    geom_tile(aes(fill=I(color_wk)), color="#ffffffde") +
    geom_text(aes(label=day(dt),color=I(if_else(month(dt)==p,"#ffffff","#e3e3e3"))),  family="Roboto Condensed") +
    geom_text(aes(label=str_c("W",wk),color=I(color_wk),x=0),
              family="Roboto Condensed", 
              data = . %>% count(wk,color_num,color_wk,p,q))+
    geom_blank(aes(x=-1)) +
    scale_y_reverse(breaks=NULL) +
    scale_x_discrete(labels=~str_sub(.,1L,2L)) +
    facet_wrap(~p,ncol=3, scales="free") +
    cowplot::theme_minimal_grid(font_family="Roboto Condensed",line_size=0) +
    labs(x="",y="")
}

  