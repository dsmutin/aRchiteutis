# NetCoMi-based microbial network construction / analysis support.
#
# NetCoMi (github.com/stefpeschel/NetCoMi) is an optional (Suggests) dependency
# that lives OUTSIDE Imports: every entry point here is guarded by
# `requireNamespace("NetCoMi")` and errors with an actionable install hint if it
# is missing. `df2netcomi()` builds and analyses a network with
# NetCoMi::netConstruct() + netAnalyze(); `df2netcomi_graph()` bridges the
# resulting association matrix into a `tidygraph::tbl_graph` so it can be drawn
# with the SAME `df2ggraph()` engine as the native tidygraph path (unified look).
# All accept df / phyloseq / matrix inputs like the rest of the package.

# Error out early (with a helpful message) if NetCoMi is not installed.
.archi_require_netcomi <- function() {
  if (!requireNamespace("NetCoMi", quietly = TRUE)) {
    stop("The 'NetCoMi' package is required for this function but is not ",
         "installed. Install it from GitHub with:\n",
         "  if (!requireNamespace('remotes')) install.packages('remotes')\n",
         "  remotes::install_github('stefpeschel/NetCoMi', ",
         "dependencies = c('Depends', 'Imports', 'LinkingTo'))",
         call. = FALSE)
  }
}

# Extract the taxon-by-taxon association matrix from a NetCoMi microNet object,
# preferring the signed association estimates (so edge sign survives the bridge).
.archi_netcomi_assomat <- function(net) {
  for (slot in c("assoMat1", "assoEst1", "adjaMat1", "corrMat1")) {
    m <- net[[slot]]
    if (!is.null(m)) return(as.matrix(m))
  }
  stop("Could not find an association / adjacency matrix in the NetCoMi ",
       "object (looked for assoMat1 / assoEst1 / adjaMat1).", call. = FALSE)
}

#' Build and analyse a microbial network with NetCoMi
#'
#' Constructs a taxon-by-taxon association network with
#' `NetCoMi::netConstruct()` and analyses it with `NetCoMi::netAnalyze()`,
#' returning both objects so networks can be inspected or compared. NetCoMi is
#' an optional dependency: this function errors with an install hint when it is
#' not available.
#'
#' Correlation-based measures (`"pearson"`, `"spearman"`) are recommended as
#' they need no extra compiled dependencies; measures such as `"spieceasi"` /
#' `"spring"` additionally require the `SpiecEasi` / `SPRING` packages.
#'
#' @param df A numeric taxa-by-sample matrix, e.g. from [df_untidy()]. A
#'   [phyloseq::phyloseq] object or a long `df` from [get_counts()] is also
#'   accepted and reduced to a matrix first (a plain matrix is used as-is).
#' @param clade Character. Clade letter used when reducing a `df` / phyloseq
#'   object to a matrix (default `"G"`, genus).
#' @param measure Association measure passed to `NetCoMi::netConstruct()`
#'   (default `"pearson"`).
#' @param top Integer or `FALSE`. Keep only the `top` most abundant taxa when
#'   reducing a `df` / phyloseq object.
#' @param sparsMethod Sparsification method for `NetCoMi::netConstruct()`
#'   (default `"threshold"`).
#' @param thresh Numeric association threshold used when
#'   `sparsMethod = "threshold"` (default `0.3`).
#' @param verbose Verbosity level forwarded to NetCoMi (default `0`).
#' @param seed Integer random seed for reproducibility (default `42`).
#' @param netConstruct_args,netAnalyze_args Optional named lists of extra
#'   arguments forwarded to `NetCoMi::netConstruct()` / `NetCoMi::netAnalyze()`.
#'   `netAnalyze()` is called with `graphlet = FALSE` by default (faster, fewer
#'   optional dependencies); pass `netAnalyze_args = list(graphlet = TRUE)` to
#'   enable the graphlet-correlation analysis.
#' @param ... Passed to the internal matrix coercion (see [df_untidy()]).
#'
#' @return A list with elements `net` (the `microNet` object from
#'   `netConstruct()`) and `props` (the `microNetProps` object from
#'   `netAnalyze()`).
#'
#' @seealso [df2netcomi_graph()] to bridge the result into the [df2ggraph()]
#'   engine.
#'
#' @examples
#' \dontrun{
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12)
#' res <- df2netcomi(mat, measure = "pearson")
#' res$props
#' }
#'
#' @export
df2netcomi <- function(df, clade = "G", measure = "pearson", top = FALSE,
                       sparsMethod = "threshold", thresh = 0.3, verbose = 0,
                       seed = 42, netConstruct_args = list(),
                       netAnalyze_args = list(), ...) {
  .archi_require_netcomi()

  mat <- as_samovar_matrix(df, clade = clade, top = top, scale = FALSE, ...)
  mat <- as.matrix(mat)
  # NetCoMi expects samples in rows and taxa in columns.
  counts <- t(mat)

  nc_args <- utils::modifyList(
    list(data = counts, dataType = "counts", measure = measure,
         sparsMethod = sparsMethod, thresh = thresh, verbose = verbose,
         seed = seed),
    netConstruct_args)
  net <- do.call(NetCoMi::netConstruct, nc_args)

  # Default to graphlet = FALSE: the graphlet-correlation (GCM) analysis is
  # mainly for comparing two networks, is comparatively slow, and pulls extra
  # optional dependencies. Callers can re-enable it via netAnalyze_args.
  na_args <- utils::modifyList(list(net = net, graphlet = FALSE),
                               netAnalyze_args)
  props <- do.call(NetCoMi::netAnalyze, na_args)

  list(net = net, props = props)
}

#' Bridge a NetCoMi network into a tidygraph / ggraph visualisation
#'
#' Extracts the association matrix from a NetCoMi network and feeds it through
#' the same tidygraph engine as [df2graph()], so a NetCoMi-constructed network
#' can be drawn with [df2ggraph()] for a look consistent with the native path.
#'
#' @param x One of: the list returned by [df2netcomi()]; a NetCoMi `microNet`
#'   object; a `df` / [phyloseq::phyloseq] object / matrix (in which case
#'   [df2netcomi()] is run first); or a `tbl_graph` (returned as-is).
#' @param threshold Numeric in `[0, 1]`. Absolute-association edge threshold
#'   applied when building the graph.
#' @param cluster_method Community-detection method (see [df2graph()]).
#' @param plot Logical. When `TRUE`, return a [df2ggraph()] plot; when `FALSE`
#'   (default) return the annotated [tidygraph::tbl_graph].
#' @param ... Passed to [df2netcomi()] (when `x` is raw data) and to
#'   [df2ggraph()] (when `plot = TRUE`).
#'
#' @return A [tidygraph::tbl_graph] (when `plot = FALSE`) or a
#'   [ggplot2::ggplot] (when `plot = TRUE`).
#'
#' @seealso [df2netcomi()], [df2graph()], [df2ggraph()].
#'
#' @examples
#' \dontrun{
#' path <- system.file("extdata", package = "aRchiteutis")
#' df <- get_counts(path = path, pattern = "m[13][124]_", trim_char = "_")
#' mat <- df_untidy(df, clade = "G", top = 12)
#' res <- df2netcomi(mat, measure = "pearson")
#' g <- df2netcomi_graph(res)          # a tbl_graph
#' df2netcomi_graph(res, plot = TRUE)  # a ggraph plot
#' }
#'
#' @export
df2netcomi_graph <- function(x, threshold = 0.3, cluster_method = "louvain",
                             plot = FALSE, ...) {
  if (inherits(x, "tbl_graph")) {
    g <- x
  } else {
    net <- .archi_extract_micronet(x, ...)
    cmat <- .archi_netcomi_assomat(net)
    g <- .archi_cormat_to_tbl_graph(cmat, threshold = threshold,
                                    cluster_method = cluster_method)
  }

  if (isTRUE(plot)) df2ggraph(g, ...) else g
}

# Resolve `x` to a NetCoMi microNet object, running df2netcomi() on raw data.
.archi_extract_micronet <- function(x, ...) {
  if (is.list(x) && !is.null(x$net)) return(x$net)
  if (inherits(x, "microNet")) return(x)
  .archi_require_netcomi()
  # Only forward args df2netcomi() understands (drop df2ggraph-only ones).
  dots <- list(...)
  nc_formals <- names(formals(df2netcomi))
  dots <- dots[intersect(names(dots), nc_formals)]
  res <- do.call(df2netcomi, c(list(df = x), dots))
  res$net
}
