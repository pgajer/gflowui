gflowui_basin_export_column <- function(
    table,
    name,
    default = NA_character_) {
  if (name %in% names(table)) {
    return(table[[name]])
  }
  rep(default, nrow(table))
}

gflowui_basin_export_characteristics <- function(result) {
  if (!is.list(result) ||
      !is.data.frame(result$all_table) ||
      nrow(result$all_table) < 1L) {
    stop(
      "The active basin complex has no full characteristics table to export.",
      call. = FALSE
    )
  }
  table <- result$all_table
  required <- c(
    "display.label", "type", "rank", "extremum.vertex",
    "extremum.value", "primary.support.size", "primary.support.mass",
    "prominence"
  )
  missing <- setdiff(required, names(table))
  if (length(missing) > 0L) {
    stop(
      sprintf(
        "The full basin table is missing required columns: %s.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
  type <- as.character(table$type)
  type[type == "max"] <- "maximum"
  type[type == "min"] <- "minimum"
  data.frame(
    extremum_basin = as.character(table$display.label),
    extremum_type = type,
    rank = suppressWarnings(as.integer(table$rank)),
    extremum_vertex = suppressWarnings(as.integer(table$extremum.vertex)),
    extremum_value = suppressWarnings(as.numeric(table$extremum.value)),
    support = suppressWarnings(as.integer(table$primary.support.size)),
    mass = suppressWarnings(as.numeric(table$primary.support.mass)),
    prominence = suppressWarnings(as.numeric(table$prominence)),
    raw_support = suppressWarnings(as.integer(
      gflowui_basin_export_column(table, "raw.support.size", NA_integer_)
    )),
    raw_mass = suppressWarnings(as.numeric(
      gflowui_basin_export_column(table, "raw.support.mass", NA_real_)
    )),
    retained_support = suppressWarnings(as.integer(
      gflowui_basin_export_column(
        table,
        "retained.support.size",
        NA_integer_
      )
    )),
    retained_mass = suppressWarnings(as.numeric(
      gflowui_basin_export_column(
        table,
        "retained.support.mass",
        NA_real_
      )
    )),
    allocated_mass = suppressWarnings(as.numeric(
      gflowui_basin_export_column(table, "raw.allocated.mass", NA_real_)
    )),
    assignment_status = as.character(gflowui_basin_export_column(
      table,
      "assignment.status"
    )),
    retention_status = as.character(gflowui_basin_export_column(
      table,
      "retention.status"
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

gflowui_basin_export_internal_mapping <- function(result) {
  table <- result$all_table
  data.frame(
    extremum_basin = as.character(table$display.label),
    internal_key = as.character(gflowui_basin_export_column(table, "key")),
    basin_id = as.character(gflowui_basin_export_column(table, "basin.id")),
    extremum_id = as.character(gflowui_basin_export_column(
      table,
      "extremum.id"
    )),
    parent_basin_id = as.character(gflowui_basin_export_column(
      table,
      "parent.basin.id"
    )),
    extremum_vertex = suppressWarnings(as.integer(table$extremum.vertex)),
    extremum_vertex_id = as.character(gflowui_basin_export_column(
      table,
      "extremum.vertex.id"
    )),
    reconstruction_method = as.character(gflowui_basin_export_column(
      table,
      "method"
    )),
    ranking_measure = as.character(gflowui_basin_export_column(
      table,
      "rank.measure"
    )),
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
}

gflowui_basin_export_column_definitions <- function() {
  data.frame(
    column = c(
      "extremum_basin", "extremum_type", "rank", "extremum_vertex",
      "extremum_value", "support", "mass", "prominence", "raw_support",
      "raw_mass", "retained_support", "retained_mass", "allocated_mass",
      "assignment_status", "retention_status"
    ),
    definition = c(
      "Readable direction-specific label: M for maxima and m for minima.",
      "Whether the basin is associated with a local maximum or minimum.",
      "Direction-specific rank under the recorded ranking measure.",
      "Internal integer index of the representative extremum vertex.",
      "Raw field value at the representative extremum.",
      "Number of vertices uniquely assigned to the basin.",
      "Normalized mass of vertices uniquely assigned to the basin.",
      paste(
        "Extremum-to-merge field difference from the exact plateau-aware",
        "merge tree."
      ),
      "Number of vertices in the raw, potentially overlapping support.",
      "Normalized mass in the raw, potentially overlapping support.",
      "Number of vertices in retained overlapping coverage.",
      "Normalized mass in retained overlapping coverage.",
      "Mass allocated through canonical basin membership.",
      "Canonical primary-assignment status.",
      "Canonical retention status."
    ),
    stringsAsFactors = FALSE
  )
}

gflowui_basin_json_safe <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  if (inherits(x, "POSIXt")) {
    return(format(x, "%Y-%m-%dT%H:%M:%S%z"))
  }
  if (is.factor(x)) {
    return(as.character(x))
  }
  if (is.data.frame(x)) {
    return(lapply(x, gflowui_basin_json_safe))
  }
  if (is.list(x)) {
    return(lapply(x, gflowui_basin_json_safe))
  }
  if (is.atomic(x)) {
    return(x)
  }
  as.character(x)
}

gflowui_basin_export_provenance <- function(
    result,
    characteristics,
    exported_at = Sys.time()) {
  identity <- result$construction_identity %||% list()
  record <- identity$record %||% list()
  graph <- record$graph %||% list()
  ranking <- result$ranking_resolved %||%
    result$summary$rank.resolved %||%
    list()
  package_version <- function(package) {
    if (!requireNamespace(package, quietly = TRUE)) {
      return(NA_character_)
    }
    as.character(utils::packageVersion(package))
  }
  provenance <- list(
    schema = "gflowui_basin_export_bundle/1",
    exported_at = format(exported_at, "%Y-%m-%dT%H:%M:%S%z"),
    export_scope = list(
      rows = "all basins in the active basin complex",
      inspector_filters_ignored = TRUE,
      top_k_ignored = TRUE,
      basin_selections_ignored = TRUE,
      display_colors_ignored = TRUE,
      coordinate_scale = "raw"
    ),
    counts = list(
      total = nrow(characteristics),
      maximum = sum(characteristics$extremum_type == "maximum"),
      minimum = sum(characteristics$extremum_type == "minimum")
    ),
    source = list(
      project_id = as.character(result$project_id %||% record$project.id %||% ""),
      graph_set_id = as.character(
        result$graph_set_id %||% record$graph.set.id %||% ""
      ),
      graph_k = suppressWarnings(as.integer(
        result$graph_k %||% graph$graph.k %||% NA_integer_
      )),
      estimate_key = as.character(result$source_key %||% record$source.key %||% ""),
      estimate_label = as.character(result$source_label %||% ""),
      estimate_fingerprint = as.character(
        result$source_fingerprint %||% record$source.fingerprint %||% ""
      )
    ),
    graph_identity = list(
      graph_fingerprint = as.character(graph$graph.fingerprint %||% ""),
      topology_fingerprint = as.character(
        graph$topology.fingerprint %||% ""
      ),
      vertex_id_fingerprint = as.character(
        graph$vertex.id.fingerprint %||% ""
      ),
      display_vertex_id_fingerprint = as.character(
        graph$display.vertex.id.fingerprint %||% ""
      )
    ),
    reconstruction = list(
      fingerprint = as.character(identity$fingerprint %||% ""),
      parameters = record$construction %||% list(
        method = "trajectory_flow",
        direction = "both",
        modulation = "CLOSEST",
        plateau.policy = "connected_exact"
      ),
      prominence_method = as.character(
        result$prominence_method %||% "superlevel_merge_tree"
      ),
      gflow_build_id = as.character(
        result$build_identity$build.id %||% record$gflow.build.id %||% ""
      ),
      gflow_runtime_id = as.character(
        result$build_identity$runtime$id %||%
          record$gflow.runtime.id %||%
          ""
      )
    ),
    ranking = list(
      requested = as.character(
        result$rank_by %||% "primary.support.mass"
      ),
      maximum = as.character(ranking[["max"]] %||% ""),
      minimum = as.character(ranking[["min"]] %||% "")
    ),
    mass_provenance = result$summary$mass.provenance %||% NULL,
    data_fingerprint = gflowui_basin_sha256(characteristics),
    software = list(
      R = as.character(getRversion()),
      gflowui = package_version("gflowui"),
      gflow = package_version("gflow"),
      dgraphs = package_version("dgraphs")
    ),
    files = c(
      "basin_characteristics.csv",
      "basin_analysis.rds",
      "basin_internal_mapping.csv",
      "basin_column_definitions.csv",
      "basin_provenance.json",
      "README.txt"
    )
  )
  gflowui_basin_json_safe(provenance)
}

gflowui_resolve_basin_export_directory <- function(path) {
  path <- trimws(as.character(path %||% ""))
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("Enter a destination directory for the basin bundle.", call. = FALSE)
  }
  expanded <- path.expand(path)
  if (!dir.exists(expanded)) {
    stop(
      sprintf("The basin bundle directory does not exist: %s", expanded),
      call. = FALSE
    )
  }
  resolved <- normalizePath(expanded, winslash = "/", mustWork = TRUE)
  if (file.access(resolved, 2L) != 0L) {
    stop(
      sprintf("The basin bundle directory is not writable: %s", resolved),
      call. = FALSE
    )
  }
  resolved
}

gflowui_basin_export_slug <- function(x, fallback) {
  value <- enc2utf8(as.character(x %||% ""))
  if (length(value) < 1L || is.na(value[[1L]])) {
    value <- ""
  } else {
    value <- value[[1L]]
  }
  value <- iconv(value, from = "", to = "ASCII//TRANSLIT", sub = "")
  value <- tolower(gsub("[^A-Za-z0-9]+", "-", value))
  value <- gsub("(^-+|-+$)", "", value)
  if (!nzchar(value)) {
    value <- fallback
  }
  substr(value, 1L, 48L)
}

gflowui_unique_basin_bundle_path <- function(
    directory,
    result,
    exported_at = Sys.time()) {
  project <- gflowui_basin_export_slug(result$project_id, "project")
  source <- gflowui_basin_export_slug(result$source_label, "estimate")
  fingerprint <- as.character(
    result$construction_identity$fingerprint %||% ""
  )
  fingerprint <- gsub("[^A-Fa-f0-9]", "", fingerprint)
  if (!nzchar(fingerprint)) {
    fingerprint <- "nohash"
  }
  stem <- sprintf(
    "gflowui_basin_%s_%s_%s_%s",
    project,
    source,
    format(exported_at, "%Y%m%d_%H%M%S"),
    substr(fingerprint, 1L, 10L)
  )
  candidate <- file.path(directory, paste0(stem, ".zip"))
  suffix <- 2L
  while (file.exists(candidate)) {
    candidate <- file.path(
      directory,
      sprintf("%s_%d.zip", stem, suffix)
    )
    suffix <- suffix + 1L
  }
  candidate
}

gflowui_write_basin_export_bundle <- function(
    result,
    destination,
    exported_at = Sys.time()) {
  if (!requireNamespace("zip", quietly = TRUE)) {
    stop(
      "The zip package is required to save a basin export bundle.",
      call. = FALSE
    )
  }
  directory <- gflowui_resolve_basin_export_directory(destination)
  characteristics <- gflowui_basin_export_characteristics(result)
  mapping <- gflowui_basin_export_internal_mapping(result)
  definitions <- gflowui_basin_export_column_definitions()
  provenance <- gflowui_basin_export_provenance(
    result,
    characteristics,
    exported_at = exported_at
  )
  staging <- tempfile("gflowui-basin-export-")
  if (!dir.create(staging, recursive = TRUE, showWarnings = FALSE)) {
    stop("Could not create a temporary basin export directory.", call. = FALSE)
  }
  on.exit(unlink(staging, recursive = TRUE, force = TRUE), add = TRUE)
  utils::write.csv(
    characteristics,
    file.path(staging, "basin_characteristics.csv"),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  utils::write.csv(
    mapping,
    file.path(staging, "basin_internal_mapping.csv"),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  utils::write.csv(
    definitions,
    file.path(staging, "basin_column_definitions.csv"),
    row.names = FALSE,
    na = "",
    fileEncoding = "UTF-8"
  )
  saveRDS(
    list(
      basin_characteristics = characteristics,
      basin_internal_mapping = mapping,
      column_definitions = definitions,
      provenance = provenance
    ),
    file.path(staging, "basin_analysis.rds"),
    version = 3L
  )
  writeLines(
    jsonlite::toJSON(
      provenance,
      auto_unbox = TRUE,
      pretty = TRUE,
      null = "null",
      na = "null",
      digits = NA
    ),
    file.path(staging, "basin_provenance.json"),
    useBytes = TRUE
  )
  writeLines(
    c(
      "gflowui full basin-characteristics export",
      "",
      sprintf("Exported rows: %d", nrow(characteristics)),
      paste(
        "Scope: every maximum and minimum basin in the active basin complex;",
        "Inspector filters, top-K settings, selections, and colors are ignored."
      ),
      paste(
        "Coordinates: all numeric values are raw.",
        "Log10 settings in the plot workspace are display-only."
      ),
      "",
      "Files:",
      "- basin_characteristics.csv: readable analysis table.",
      "- basin_analysis.rds: R-native tables and provenance.",
      "- basin_internal_mapping.csv: internal IDs for traceability.",
      "- basin_column_definitions.csv: analysis-column definitions.",
      "- basin_provenance.json: source, graph, reconstruction, and software metadata."
    ),
    file.path(staging, "README.txt"),
    useBytes = TRUE
  )
  target <- gflowui_unique_basin_bundle_path(
    directory,
    result,
    exported_at = exported_at
  )
  temporary.zip <- tempfile(
    ".gflowui-basin-export-",
    tmpdir = directory,
    fileext = ".zip"
  )
  on.exit(unlink(temporary.zip, force = TRUE), add = TRUE)
  files <- list.files(staging, full.names = TRUE)
  zip::zipr(
    zipfile = temporary.zip,
    files = files,
    root = staging,
    include_directories = FALSE
  )
  if (!file.exists(temporary.zip) ||
      !is.finite(file.info(temporary.zip)$size) ||
      file.info(temporary.zip)$size < 1L) {
    stop("The basin export ZIP could not be created.", call. = FALSE)
  }
  if (!file.rename(temporary.zip, target)) {
    if (!file.copy(temporary.zip, target, overwrite = FALSE)) {
      stop("The basin export ZIP could not be moved into place.", call. = FALSE)
    }
    unlink(temporary.zip, force = TRUE)
  }
  indexed <- gflowui_index_basin_export(
    target,
    expected_fingerprint = as.character(
      result$construction_identity$fingerprint %||% ""
    )
  )
  list(
    path = indexed$path,
    zip_sha256 = indexed$zip_sha256,
    reconstruction_fingerprint =
      indexed$reconstruction_fingerprint,
    indexed = isTRUE(indexed$indexed),
    row_count = nrow(characteristics),
    maximum_count = sum(characteristics$extremum_type == "maximum"),
    minimum_count = sum(characteristics$extremum_type == "minimum"),
    files = basename(files)
  )
}
