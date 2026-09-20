# Internal helpers for optional ggviolinbox (combined half-violin / half-boxplot)
# rendering used by df2alpha(), df2alpha_summary() and df2barplot().
#
# ggviolinbox (https://github.com/dsmutin/ggviolinbox) is a soft dependency
# listed under Suggests. None of the default (`violinbox = FALSE`) code paths
# touch it; it is only required when a caller explicitly opts in.

# Assemble the ggviolinbox geom layer(s) for a "violinbox" plot.
#
# `violinbox` is TRUE / "combined" (a single ggviolinbox::geom_violinboxplot)
# or "halves" (a ggviolinbox::geom_halfviolin + ggviolinbox::geom_halfboxplot
# pair). `box_side` / `violin_side` are "left" / "right" and choose which side
# of the category each geom is drawn on. `mapping` is an optional [ggplot2::aes]
# (e.g. the fill mapping) forwarded to the geoms. The categorical variable must
# already be on the x aesthetic (use [ggplot2::coord_flip] for horizontal
# layouts), per the ggviolinbox README.
violinbox_geom <- function(violinbox, box_side = "left", violin_side = "right",
                           mapping = NULL, alpha = 0.3, show.legend = NA) {
  if (!requireNamespace("ggviolinbox", quietly = TRUE)) {
    stop(
      "`violinbox` rendering requires the 'ggviolinbox' package, which is not ",
      "installed.\n  Install it with:\n",
      "    devtools::install_github(\"dsmutin/ggviolinbox\")",
      call. = FALSE)
  }

  sides <- c("left", "right")
  if (!isTRUE(box_side %in% sides) || !isTRUE(violin_side %in% sides)) {
    stop("`box_side` and `violin_side` must each be \"left\" or \"right\".",
         call. = FALSE)
  }

  mode <- if (isTRUE(violinbox)) "combined" else as.character(violinbox)

  if (identical(mode, "combined")) {
    list(ggviolinbox::geom_violinboxplot(
      mapping = mapping, boxplot = box_side, violinplot = violin_side,
      trim = FALSE, outlier.alpha = 0, alpha = alpha,
      show.legend = show.legend))
  } else if (identical(mode, "halves")) {
    list(
      ggviolinbox::geom_halfviolin(
        mapping = mapping, panel = violin_side, trim = FALSE,
        alpha = alpha, show.legend = show.legend),
      ggviolinbox::geom_halfboxplot(
        mapping = mapping, panel = box_side, outlier.alpha = 0,
        alpha = alpha, show.legend = show.legend))
  } else {
    stop(
      "`violinbox` must be FALSE, TRUE, \"combined\" or \"halves\"; got \"",
      mode, "\".", call. = FALSE)
  }
}
