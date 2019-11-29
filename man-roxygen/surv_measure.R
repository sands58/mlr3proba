#' <% int_str = "* `integrated` :: `logical(1)` \\cr If `TRUE` (default), returns the integrated score, otherwise returns the score corresponding to the `times` argument." %>
#' <% times_str = "* `times` :: `vector()` \\cr Numeric vector of times where to evaluate the score (provide only one for best performance). If missing then returns score at all unique time-points in test set." %>
#  <% thresh_str = "* `lp_thresh` :: `numeric(1)` \\cr Determines where to threshold the linear predictor for calculating the TPR/TNR. If missing then returns score at all possible thresholds." %>
#' <% type_str = "* `type` :: `character(1)` \\cr Determines the type of score, one of: 'cumulative', 'incident'." %>
#' <% eps_str = "* `eps` :: `numeric(1)` \\cr Very small number to set zero-valued predicted probabilities to, in order to prevent errors in log(0) calculation." %>
#'
#' @include MeasureSurv.R
#' @title <%=title%> Survival Measure
#' @usage NULL
#' @format [R6::R6Class()] inheriting from <%=inherit%>.
#' @aliases <%= paste("mlr_measures", shortname, sep = "_")%>
#' @description
#'
#' @section Construction:
#' ```
#' <%=fullname%>$new(<%= if(exists("pars")) pars %>)
#' mlr_measures$get("<%=shortname%>")
#' msr("<%=shortname%>")
#' ```
#'
#' <%= if(exists("int_par")) paste(int_str, "\\cr") %>
#' <%= if(exists("times_par")) paste(times_str, "\\cr") %>
#' <%= if(exists("thresh_par")) paste(thresh_str, "\\cr") %>
#' <%= if(exists("type_par")) paste(type_str, "\\cr") %>
#' <%= if(exists("eps_par")) paste(eps_str, "\\cr") %>
#'
#' @section Meta Information:
#' <% meas = get(fullname)$new() %>
#' * Type: `"surv"`
#' * Range: <%= format_range(meas$range) %>
#' * Minimize: `<%=meas$minimize%>`
#' * Required prediction: `<%=meas$predict_type%>`
#'
#' @family survival measures
#' @template seealso_measure