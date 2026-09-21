#' @keywords internal
"_PACKAGE"

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL

# NSE (non-standard evaluation) column names used by dplyr / ggplot2 across the
# package. Declaring them keeps `R CMD check` from raising "no visible binding
# for global variable" NOTEs for bare column references and the backtick-named
# summary columns produced by summarise().
utils::globalVariables(c(
  # raw get_counts() columns
  "taxa", "clade", "sample", "N", "amount", "amount_cl", ".total",
  # summarise() output columns referenced by backtick name
  "sum(N)", "mean(amount)", "mean(amount_cl)",
  # reshaped / plotting columns
  "value", "name", "x", "y", "ymax", "ymin", "split", "sample2",
  "logAC", "p", "sd", "Axis.1", "Axis.2", "group", "cluster",
  # ggraph node/edge computed aesthetics
  "vals"
))
