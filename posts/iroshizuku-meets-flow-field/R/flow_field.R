# ==============================================================================
# Iroshizuku Meets Flow Fields
# Flow-field utilities
# ==============================================================================
#
# Machinery used by the accompanying Quarto post.
#
# Conceptual pipeline:
#
#   starting points
#        ↓
#   flow field
#        ↓
#   walker
#        ↓
#   paths
#        ↓
#   colour + linewidth
#        ↓
#   ggplot
#
# ==============================================================================


# Palette ----------------------------------------------------------------------

# Pilot Iroshizuku-inspired ink palette
ink_palette <- c(
  "#1255A2", "#04318E", "#0368B4", "#00A0DF",
  "#028986", "#1A7DA5", "#016D8C", "#1C3A65",
  "#077D5E", "#007E4F", "#037261", "#1E1D1E",
  "#6A869A", "#94BD4E", "#D9DA26", "#ED7E93",
  "#765FA8", "#660D5B", "#E12E2C", "#EA5A10",
  "#EF881F", "#F0B018", "#5B4532", "#674F4D"
)


# Starting positions -----------------------------------------------------------

#' Create phyllotactic starting points
#'
#' Uses the golden angle to arrange particles in the familiar
#' sunflower-like phyllotactic pattern.
#'
#' @param n Number of particles.
#' @param scale Spacing between particles.
#'
#' @return Tibble containing id, radius, angle, x0 and y0.
make_phyllotaxis <- function(
    n = 500,
    scale = 0.06
) {
  
  i <- seq_len(n)
  
  # Golden angle: approximately 137.5 degrees.
  golden_angle <- pi * (3 - sqrt(5))
  
  tibble::tibble(
    id = i,
    r = scale * sqrt(i),
    theta = i * golden_angle,
    x0 = r * cos(theta),
    y0 = r * sin(theta)
  )
}


# Flow fields ------------------------------------------------------------------

# Each field takes an (x, y) position and returns a direction in radians.
#
# Think of these as different "laws of physics" for the particles.


#' Circular vortex
field_vortex <- function(x, y) {
  atan2(y, x) + pi / 2
}


#' Outward spiral
field_spiral <- function(x, y) {
  atan2(y, x) + pi / 2 + 0.25
}


#' Outward radial field
field_outward <- function(x, y) {
  atan2(y, x)
}


#' Inward radial field
field_inward <- function(x, y) {
  atan2(y, x) + pi
}


#' Gentle two-dimensional wave
field_wavy <- function(x, y) {
  sin(x * 0.2) + cos(y * 0.6)
}


#' Stronger two-dimensional wave
field_wavy_strong <- function(x, y) {
  sin(x * 0.8) + cos(y * 0.8)
}


#' Horizontal wave
field_horizontal_wave <- function(x, y) {
  sin(y * 0.8)
}


#' Vertical wave
field_vertical_wave <- function(x, y) {
  cos(x * 0.8)
}


#' Radial ripple
field_ripple <- function(x, y) {
  sin(sqrt(x^2 + y^2) * 2)
}


#' Interference field
field_interference <- function(x, y) {
  sin(x * 0.7) * cos(y * 0.7) * pi
}


#' Diagonal wave
field_diagonal <- function(x, y) {
  sin((x + y) * 0.5)
}


# Particle walking -------------------------------------------------------------

#' Walk one particle through a flow field
#'
#' Starting at (x0, y0), repeatedly asks the field for the direction
#' at the particle's current position and moves one small step.
#'
#' @param x0 Initial x position.
#' @param y0 Initial y position.
#' @param field Function taking x and y and returning an angle in radians.
#' @param n_steps Number of positions in the trajectory.
#' @param step_size Distance travelled at each step.
#'
#' @return Tibble containing step, x and y.
walk_field <- function(
    x0,
    y0,
    field,
    n_steps = 120,
    step_size = 0.015
) {
  
  # Pre-allocate storage for the trajectory.
  x <- numeric(n_steps)
  y <- numeric(n_steps)
  
  # Drop the particle into the field. 💧
  x[1] <- x0
  y[1] <- y0
  
  for (i in 2:n_steps) {
    
    # Ask the field which direction to travel.
    angle <- field(
      x[i - 1],
      y[i - 1]
    )
    
    # Convert the angle into x/y movement.
    x[i] <- x[i - 1] +
      cos(angle) * step_size
    
    y[i] <- y[i - 1] +
      sin(angle) * step_size
  }
  
  tibble::tibble(
    step = seq_len(n_steps),
    x = x,
    y = y
  )
}


# Generate paths ---------------------------------------------------------------

#' Send many phyllotactic particles through a flow field
#'
#' @param field Flow-field function.
#' @param n Number of particles.
#' @param scale Spacing of the phyllotactic starting positions.
#' @param n_steps Number of steps per particle.
#' @param step_size Distance travelled per step.
#' @param palette Vector of colours.
#' @param seed Random seed used for colour assignment.
#'
#' @return Tibble containing all particle trajectories.
make_flow_paths <- function(
    field,
    n = 500,
    scale = 0.06,
    n_steps = 120,
    step_size = 0.015,
    palette = ink_palette,
    seed = 77
) {
  
  set.seed(seed)
  
  seeds <- make_phyllotaxis(
    n = n,
    scale = scale
  ) |>
    dplyr::mutate(
      ink = sample(
        palette,
        size = dplyr::n(),
        replace = TRUE
      )
    )
  
  seeds |>
    dplyr::select(
      id,
      x0,
      y0,
      ink
    ) |>
    purrr::pmap_dfr(
      function(id, x0, y0, ink) {
        
        walk_field(
          x0 = x0,
          y0 = y0,
          field = field,
          n_steps = n_steps,
          step_size = step_size
        ) |>
          dplyr::mutate(
            id = id,
            ink = ink
          )
      }
    )
}


# Plotting ---------------------------------------------------------------------

#' Plot flow-field paths
#'
#' @param paths Output from make_flow_paths().
#' @param linewidth Width of the paths.
#' @param alpha Transparency of the paths.
#'
#' @return ggplot object.
plot_flow_paths <- function(
    paths,
    linewidth = 0.4,
    alpha = 0.6
) {
  
  ggplot2::ggplot(
    paths,
    ggplot2::aes(
      x = x,
      y = y,
      group = id,
      colour = ink
    )
  ) +
    ggplot2::geom_path(
      linewidth = linewidth,
      alpha = alpha
    ) +
    ggplot2::scale_colour_identity() +
    ggplot2::coord_equal() +
    ggplot2::theme_void()
}


# Convenience wrapper ----------------------------------------------------------

#' Create and plot flow-field art
#'
#' Convenience wrapper around make_flow_paths() and plot_flow_paths().
draw_flow_field <- function(
    field,
    n = 500,
    scale = 0.06,
    n_steps = 120,
    step_size = 0.015,
    palette = ink_palette,
    seed = 77,
    linewidth = 0.4,
    alpha = 0.6
) {
  
  paths <- make_flow_paths(
    field = field,
    n = n,
    scale = scale,
    n_steps = n_steps,
    step_size = step_size,
    palette = palette,
    seed = seed
  )
  
  plot_flow_paths(
    paths,
    linewidth = linewidth,
    alpha = alpha
  )
}