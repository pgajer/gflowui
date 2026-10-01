# Reuse the ID-keyed sparse abundance asset for endpoint inspection. Coordinate
# transforms apply to the selected row only; no per-layout matrix copies.
gflowui_endpoint_abundance_profile <- function(vertex, vertex_ids, asset,
    coordinate = "abundance", reference_taxon = NULL) {
  if (is.null(asset) || length(vertex) != 1L || !is.finite(vertex) ||
      vertex != floor(vertex) || vertex < 1L || vertex > length(vertex_ids)) return(NULL)
  row <- match(vertex_ids[[vertex]], asset$sample_ids)
  if (is.na(row)) return(NULL)
  if (!coordinate %in% c("abundance", "ratio", "sqrt_ratio"))
    stop("Unknown endpoint profile coordinate system.")
  taxa <- asset$taxon_names[asset$indices[[row]]]
  values <- asset$abundances[[row]]
  pretty <- function(x) gsub("_", " ", x, fixed = TRUE)
  value_label <- "relative abundance"
  pure_label <- NULL
  if (coordinate != "abundance") {
    ref <- match(reference_taxon, taxa)
    if (length(ref) != 1L || is.na(ref) || values[[ref]] <= 0) return(NULL)
    denominator <- values[[ref]]
    values <- values[-ref] / denominator
    taxa <- paste0(pretty(taxa[-ref]), " / ", pretty(reference_taxon))
    value_label <- "reference ratio"
    if (coordinate == "sqrt_ratio") {
      values <- sqrt(values)
      taxa <- paste0("sqrt(", taxa, ")")
      value_label <- "square-root reference ratio"
    }
    pure_label <- paste0("Pure ", pretty(reference_taxon), " (chart origin)")
  }
  labels <- pretty(taxa)
  take <- head(seq_along(values), 5L)
  profile <- data.frame(rank = seq_along(take), feature = taxa[take],
    taxonomy = labels[take], abundance = values[take], stringsAsFactors = FALSE)
  eligible <- which(values >= 0.05)
  if (!length(eligible) && length(values)) eligible <- 1L
  label <- if (length(values)) paste(labels[head(eligible, 2L)], collapse = "; ") else pure_label
  list(vertex = as.integer(vertex), sample_id = vertex_ids[[vertex]], label = label,
    profile = profile, profile_value_label = value_label, source_kind = "live",
    empty_profile_message = if (!length(values)) "All non-reference coordinates are zero at this pure-reference composition." else NULL,
    source_detail = paste0("Phylotype profile: ", value_label,
      "; top positive coordinates; label uses up to two coordinates >= 0.05 (otherwise the largest)."))
}
