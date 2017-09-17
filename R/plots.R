#' @importFrom magrittr %>%
#' @export
magrittr::`%>%`

#' Check model class
#'
#' Functions to check if an object is a linear model object.
#'
#' @param m any \code{R} object
#' @export
is_lm <- function(m) {
  inherits(m, "lm")
}

#' Method to convert a linear model object into a data-frame
#'
#' @param m A \code{lm} object
#'
#' @method as.data.frame lm
#' @export
as.data.frame.lm <- function(m) {
  wrangle_data(m)
}

#' Method to convert a linear model object into a plot
#'
#' @param m A \code{lm} object
#'
#' @method plot lm
#' @export
plot.lm <- function(m, ...) {
  arrow_plot(m, ...)
}

#' Model theme
#'
#' Custom theme function for plotting
#' @export
theme_model_plot <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(family = "Raleway"),
                   strip.background = ggplot2::element_rect(fill = NA, color = "grey50"),
                   plot.caption = ggplot2::element_text(color = "grey50", size = 9),
                   plot.subtitle = ggplot2::element_text(face = "italic"),
                   panel.border = ggplot2::element_blank())
}


#' Overwrite ggplot
#'
#' @param ... additional args passed to ggplot
#'
#' @export
ggplot <- function(...) ggplot2::ggplot(...) + theme_model_plot()

#' Wrangle data
#'
#' Turn model output into understandable and interactable data frame
#'
#' @param m A \code{lm} object
#' @export
wrangle_data <- function(m) {

  df <- summary(m)$coefficients %>%
    as.data.frame() %>%
    tibble::rownames_to_column()
  colnames(df) <- c("var", "estimate", "std_error", "t_value", "p_value")
  df

}

#' Bar plot
#'
#' Function to plot model output estimates as a bar plot
#'
#' @param m A \code{lm} object
#' @export
bar_plot <- function(m) {

  as.data.frame(m) %>%
    dplyr::filter(var != "(Intercept)") %>%
    dplyr::mutate(p_value = factor(ifelse(p_value < .01, "p < .01",
                                   ifelse(p_value < .05, "p < .05",
                                          ifelse(p_value < .1, "p < .1",
                                                 "p >= .1"))),
                                   levels = c("p < .01", "p < .05", "p < .1", "p >= .1"))) %>%
    ggplot(ggplot2::aes(x = reorder(var, p_value),
                        y = estimate,
                        fill = estimate)) +
    ggplot2::geom_bar(stat = "identity", alpha = .8) +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = estimate - 1.96*std_error,
                               ymax = estimate + 1.96*std_error),
                           size = 2,
                           width = 0) +
    ggplot2::labs(x = "Independent Variables, Ordered by p-value",
                  y = "Estimated Effect",
                  title = "Estimated Effects",
                  subtitle = "Error bars represent a standard confidence interval",
                  caption = stringr::str_wrap(paste0(c(m$terms[[2]],m$terms[[3]]), collapse = " ~ "), 60)) +
    ggplot2::guides(fill = FALSE) +
    ggplot2::coord_flip() +
    ggplot2::facet_grid(p_value~., scales = "free_y", space = "free")

}

#' Find stars
#'
#' Find signficance stars depending on p-values
#' @param x numeric
#' @export
star_lookup <- function(x) {
  stars <- c("***", "**", "*", ".", " ", " ")
  mycut <- cut(x, breaks = c(0, 0.001, 0.01, 0.05, 0.1, 10000),
      labels = FALSE, include.lowest = TRUE)
  stars[mycut]
}

#' Arrow plot
#'
#' Plot a model with arrows.
#' In this plot, the arrows can represent both the direction and magnitude of the effects.
#' The shaded rectangles represent a standard confidence interval.
#' If the shaded interval crosses the dotted line, there is no significant evidence suggesting the independent variable has a nonzero effect.
#'
#' @param m A \code{lm} object
#' @param p p-value cutoff, highest p-value of independent variables you'd like to keep in your plot. defaults to .05
#'
#' @export
arrow_plot <- function(m, p = .05) {

  df <- as.data.frame(m) %>%
    dplyr::mutate(p_value_cutoff = factor(ifelse(p_value < .01, "p < .01",
                                                 ifelse(p_value < .05, "p < .05",
                                                        ifelse(p_value < .1, "p < .1",
                                                               "p >= .1"))),
                                          levels = c("", "p < .01", "p < .05", "p < .1", "p >= .1")))

  intercept <- df[df$var == "(Intercept)", ]

  df %>%
    dplyr::filter(p_value <= p, var != "(Intercept)") %>%
    dplyr::mutate(intercept = intercept$estimate) %>%
    ggplot(ggplot2::aes(x = reorder(var, p_value),
                        fill = estimate, color = estimate)) +
    ggplot2::geom_hline(linetype = "dashed", color = "grey50",
                        yintercept = intercept$estimate) +
    ggplot2::geom_segment(ggplot2::aes(xend = var, y = intercept,
                              yend = intercept + estimate),
                          arrow = ggplot2::arrow(length = ggplot2::unit(.25, "cm")),
                          size = 1) +
    ggplot2::labs(subtitle = "Shaded areas represent a standard confidence interval",
                  y = "Baseline + Estimated Effect",
                  title = paste0("Estimated Effects on ", m$terms[[2]]),
                  caption = paste0(c(m$terms[[2]],m$terms[[3]]), collapse = " ~ "),
                  x = paste0("Independent variables where p <= ", p)) +
    ggplot2::geom_segment(
      ggplot2::aes(xend = var,
                   y = intercept + estimate + 1.96*std_error,
                   yend = intercept + estimate - 1.96*std_error),
      alpha = .25, size = 6) +
    ggplot2::theme(panel.border = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_text(angle = 30, hjust = 1)) +
    ggplot2::facet_grid(.~p_value_cutoff, scales = "free_x", space = "free") +
    ggplot2::guides(color = F)

}

#' Plot a histogram of residuals
#'
#' @param m A \code{lm} object
#' @export
plot_residuals <- function(m, outlier = FALSE) {

  ggplot(data = data.frame(m$residuals),
         ggplot2::aes(x = m.residuals)) +
    ggplot2::geom_histogram() +
    ggplot2::labs(title = "Distribution of Residuals",
                  subtitle = "Actual Value - Fitted Values",
                  x = NULL) +
    ggplot2::geom_vline(color = "white", xintercept = 0)

}

#' Residual dots
#'
#' Plot actual vs fitted values, with orange lines to represent residuals.
#' Warning: this plot will look funky if you have a binary dependent variable!
#'
#' @param m A \code{lm} object
#' @export
residual_dots <- function(m) {

  m$model %>%
    dplyr::mutate(fitted_values = m$fitted.values,
                  residuals = m$residuals) %>%
    dplyr::rename_("yvar" = m$terms[[2]]) %>%
    ggplot(
      ggplot2::aes(x = fitted_values, y = yvar)) +
    ggplot2::geom_point() +
    ggplot2::geom_segment(color = civis.deckR::civisorange,
                          ggplot2::aes(x = fitted_values,
                                       xend = fitted_values + residuals,
                                       y = yvar, yend = yvar)) +
    ggplot2::geom_abline(color = "grey50", linetype = "dashed", slope = 1) +
    ggplot2::labs(title = "Residuals",
                  x = "Fitted Values",
                  y = "Actual Values")

}

#' Function to make a standard regression output table
#'
#' @param m A \code{lm} object
#' @export
stat_table <- function(m) {
  as.data.frame(m) %>%
    dplyr::mutate(estimate = paste0(round(estimate, 4), star_lookup(p_value)),
                  error = paste0("(", round(std_error, 4), ")")) %>%
    dplyr::select(var, estimate, error) %>%
    tidyr::gather(stat, value, estimate:error) %>%
    ggplot(ggplot2::aes(x = var, y = 1, group = stat)) +
    ggplot2::geom_text(ggplot2::aes(label = value,
                                    size = stat,
                                    fontface = ifelse(stat == "estimate", "plain", "italic")),
                       family = "Raleway",
                       position = ggplot2::position_dodge(width = 1),
                       hjust = 0) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   axis.text.x = ggplot2::element_blank(),
                   axis.ticks.x = ggplot2::element_blank(),
                   title = ggplot2::element_text(size = 8),
                   plot.caption = ggplot2::element_text(hjust = 0)) +
    ggplot2::coord_flip() +
    ggplot2::scale_size_manual(values = c(3, 4), guide = FALSE) +
    ggplot2::labs(
      title = stringr::str_wrap(
        paste0(c(m$terms[[2]],m$terms[[3]]), collapse = " ~ "),
        width = 30),
      x = NULL,
      y = NULL,
      caption = paste0("R-squared: ", round(summary(m)$r.squared, 4))) +
    ggplot2::ylim(c(.9, 2))

}
