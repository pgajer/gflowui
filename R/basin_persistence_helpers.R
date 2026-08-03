.gflowui_basin_export_index_schema <- "gflowui_basin_export_index/1"
.gflowui_basin_disk_cache_schema <- "gflowui_basin_disk_cache/1"

gflowui_basin_storage_root <- function() {
  custom <- getOption("gflowui.basin_storage_dir", NULL)
  if (is.character(custom) && length(custom) > 0L &&
      !is.na(custom[[1L]]) && nzchar(custom[[1L]])) {
    return(normalizePath(
      path.expand(custom[[1L]]),
      winslash = "/",
      mustWork = FALSE
    ))
  }
  file.path(gflowui_projects_data_dir(), "basins")
}

gflowui_basin_atomic_save_rds <- function(object, path) {
  path <- as.character(path %||% "")
  if (length(path) != 1L || is.na(path) || !nzchar(path)) {
    stop("A persistence path is required.", call. = FALSE)
  }
  directory <- dirname(path)
  if (!dir.exists(directory) &&
      !dir.create(directory, recursive = TRUE, showWarnings = FALSE)) {
    stop(
      sprintf("Could not create the persistence directory: %s", directory),
      call. = FALSE
    )
  }
  temporary <- tempfile(
    ".gflowui-atomic-",
    tmpdir = directory,
    fileext = ".rds"
  )
  on.exit(unlink(temporary, force = TRUE), add = TRUE)
  saveRDS(object, temporary, version = 3L)
  if (!file.exists(temporary) ||
      !is.finite(file.info(temporary)$size) ||
      file.info(temporary)$size < 1L) {
    stop("The temporary persistence file was not created.", call. = FALSE)
  }
  moved <- file.rename(temporary, path)
  if (!isTRUE(moved) || !file.exists(path)) {
    stop(
      sprintf("Could not atomically persist: %s", path),
      call. = FALSE
    )
  }
  normalizePath(path, winslash = "/", mustWork = TRUE)
}

gflowui_basin_export_index_path <- function() {
  file.path(gflowui_basin_storage_root(), "exports", "index.rds")
}

gflowui_empty_basin_export_entries <- function() {
  data.frame(
    reconstruction_fingerprint = character(),
    path = character(),
    zip_sha256 = character(),
    exported_at = character(),
    indexed_at = character(),
    project_id = character(),
    estimate_label = character(),
    data_fingerprint = character(),
    label_basis = character(),
    stringsAsFactors = FALSE
  )
}

gflowui_empty_basin_export_index <- function() {
  list(
    schema = .gflowui_basin_export_index_schema,
    updated_at = "",
    entries = gflowui_empty_basin_export_entries()
  )
}

gflowui_sanitize_basin_export_entries <- function(entries) {
  template <- gflowui_empty_basin_export_entries()
  if (!is.data.frame(entries) || nrow(entries) < 1L) {
    return(template)
  }
  for (name in names(template)) {
    if (!(name %in% names(entries))) {
      entries[[name]] <- ""
    }
    entries[[name]] <- as.character(entries[[name]])
  }
  entries <- entries[, names(template), drop = FALSE]
  valid <- grepl(
    "^[A-Fa-f0-9]{64}$",
    entries$reconstruction_fingerprint
  ) & nzchar(entries$path) &
    grepl("^[A-Fa-f0-9]{64}$", entries$zip_sha256)
  entries <- entries[valid, , drop = FALSE]
  entries <- entries[!duplicated(entries$path, fromLast = TRUE), , drop = FALSE]
  rownames(entries) <- NULL
  entries
}

gflowui_read_basin_export_index <- function() {
  path <- gflowui_basin_export_index_path()
  if (!file.exists(path)) {
    return(gflowui_empty_basin_export_index())
  }
  index <- tryCatch(readRDS(path), error = function(e) NULL)
  if (!is.list(index) ||
      !identical(
        as.character(index$schema %||% ""),
        .gflowui_basin_export_index_schema
      )) {
    unlink(path, force = TRUE)
    return(gflowui_empty_basin_export_index())
  }
  index$entries <- gflowui_sanitize_basin_export_entries(index$entries)
  index$schema <- .gflowui_basin_export_index_schema
  index$updated_at <- as.character(index$updated_at %||% "")
  index
}

gflowui_write_basin_export_index <- function(index) {
  index <- list(
    schema = .gflowui_basin_export_index_schema,
    updated_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    entries = gflowui_sanitize_basin_export_entries(index$entries)
  )
  gflowui_basin_atomic_save_rds(index, gflowui_basin_export_index_path())
  invisible(index)
}

gflowui_read_basin_zip_provenance <- function(path) {
  listing <- tryCatch(
    utils::unzip(path, list = TRUE),
    error = function(e) NULL
  )
  if (!is.data.frame(listing) ||
      sum(as.character(listing$Name) == "basin_provenance.json") != 1L) {
    stop(
      "The ZIP does not contain one basin_provenance.json file.",
      call. = FALSE
    )
  }
  connection <- unz(path, "basin_provenance.json", open = "rb")
  on.exit(close(connection), add = TRUE)
  jsonlite::fromJSON(connection, simplifyVector = FALSE)
}

gflowui_validate_basin_export_bundle <- function(
    path,
    expected_fingerprint = NULL,
    expected_sha256 = NULL,
    expected_label_basis = NULL) {
  path <- path.expand(as.character(path %||% ""))
  invalid <- function(reason) {
    list(
      valid = FALSE,
      reason = as.character(reason),
      path = path,
      zip_sha256 = "",
      provenance = NULL
    )
  }
  if (length(path) != 1L || is.na(path) || !nzchar(path) ||
      !file.exists(path) || dir.exists(path)) {
    return(invalid("The indexed ZIP is unavailable."))
  }
  path <- normalizePath(path, winslash = "/", mustWork = TRUE)
  sha256 <- tryCatch(
    digest::digest(path, algo = "sha256", file = TRUE),
    error = function(e) ""
  )
  if (!grepl("^[A-Fa-f0-9]{64}$", sha256)) {
    return(invalid("The ZIP SHA-256 could not be computed."))
  }
  expected_sha256 <- as.character(expected_sha256 %||% "")
  if (nzchar(expected_sha256) &&
      !identical(tolower(sha256), tolower(expected_sha256))) {
    return(invalid("The ZIP SHA-256 no longer matches the export index."))
  }
  provenance <- tryCatch(
    gflowui_read_basin_zip_provenance(path),
    error = function(e) e
  )
  if (inherits(provenance, "error") || !is.list(provenance)) {
    return(invalid(
      if (inherits(provenance, "error")) {
        conditionMessage(provenance)
      } else {
        "The ZIP provenance is unreadable."
      }
    ))
  }
  if (!identical(
      as.character(provenance$schema %||% ""),
      "gflowui_basin_export_bundle/1"
    )) {
    return(invalid("The ZIP export schema is unsupported."))
  }
  fingerprint <- as.character(
    provenance$reconstruction$fingerprint %||% ""
  )
  if (!grepl("^[A-Fa-f0-9]{64}$", fingerprint)) {
    return(invalid("The ZIP has no valid reconstruction fingerprint."))
  }
  expected_fingerprint <- as.character(expected_fingerprint %||% "")
  if (nzchar(expected_fingerprint) &&
      !identical(tolower(fingerprint), tolower(expected_fingerprint))) {
    return(invalid(
      "The ZIP reconstruction fingerprint does not match the active complex."
    ))
  }
  label.basis <- as.character(provenance$labeling$basis %||% "")
  expected.label.basis <- as.character(expected_label_basis %||% "")
  if (nzchar(expected.label.basis) &&
      !identical(label.basis, expected.label.basis)) {
    return(invalid(
      "The ZIP basin-label basis does not match the active label setting."
    ))
  }
  list(
    valid = TRUE,
    reason = "",
    path = path,
    zip_sha256 = tolower(sha256),
    reconstruction_fingerprint = tolower(fingerprint),
    label_basis = label.basis,
    provenance = provenance
  )
}

gflowui_index_basin_export <- function(
    path,
    expected_fingerprint = NULL,
    expected_label_basis = NULL) {
  validation <- gflowui_validate_basin_export_bundle(
    path,
    expected_fingerprint = expected_fingerprint,
    expected_label_basis = expected_label_basis
  )
  if (!isTRUE(validation$valid)) {
    stop(
      sprintf(
        "The basin export could not be indexed: %s",
        validation$reason
      ),
      call. = FALSE
    )
  }
  provenance <- validation$provenance
  index <- gflowui_read_basin_export_index()
  entries <- index$entries
  entries <- entries[
    normalizePath(
      path.expand(entries$path),
      winslash = "/",
      mustWork = FALSE
    ) != validation$path,
    ,
    drop = FALSE
  ]
  record <- data.frame(
    reconstruction_fingerprint =
      validation$reconstruction_fingerprint,
    path = validation$path,
    zip_sha256 = validation$zip_sha256,
    exported_at = as.character(provenance$exported_at %||% ""),
    indexed_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    project_id = as.character(provenance$source$project_id %||% ""),
    estimate_label = as.character(
      provenance$source$estimate_label %||% ""
    ),
    data_fingerprint = as.character(
      provenance$data_fingerprint %||% ""
    ),
    label_basis = as.character(
      provenance$labeling$basis %||% ""
    ),
    stringsAsFactors = FALSE
  )
  index$entries <- rbind(entries, record)
  gflowui_write_basin_export_index(index)
  c(validation, list(indexed = TRUE))
}

gflowui_find_basin_export <- function(
    reconstruction_fingerprint,
    label_basis = NULL) {
  fingerprint <- tolower(as.character(
    reconstruction_fingerprint %||% ""
  ))
  not_found <- function() {
    list(
      found = FALSE,
      path = "",
      zip_sha256 = "",
      provenance = NULL
    )
  }
  if (!grepl("^[a-f0-9]{64}$", fingerprint)) {
    return(not_found())
  }
  index <- gflowui_read_basin_export_index()
  entries <- index$entries
  candidates <- which(
    tolower(entries$reconstruction_fingerprint) == fingerprint
  )
  label.basis <- as.character(label_basis %||% "")
  if (nzchar(label.basis)) {
    candidates <- candidates[
      entries$label_basis[candidates] == label.basis
    ]
  }
  if (length(candidates) < 1L) {
    return(not_found())
  }
  candidates <- rev(candidates)
  invalid <- integer()
  match <- NULL
  for (row in candidates) {
    validation <- gflowui_validate_basin_export_bundle(
      entries$path[[row]],
      expected_fingerprint = fingerprint,
      expected_sha256 = entries$zip_sha256[[row]],
      expected_label_basis = if (nzchar(label.basis)) {
        label.basis
      } else {
        NULL
      }
    )
    if (isTRUE(validation$valid)) {
      match <- validation
      break
    }
    invalid <- c(invalid, row)
  }
  if (length(invalid) > 0L) {
    index$entries <- entries[-invalid, , drop = FALSE]
    try(gflowui_write_basin_export_index(index), silent = TRUE)
  }
  if (is.null(match)) {
    return(not_found())
  }
  c(list(found = TRUE), match)
}

gflowui_basin_disk_cache_path <- function(reconstruction_fingerprint) {
  fingerprint <- tolower(as.character(
    reconstruction_fingerprint %||% ""
  ))
  if (!grepl("^[a-f0-9]{64}$", fingerprint)) {
    return("")
  }
  file.path(
    gflowui_basin_storage_root(),
    "canonical_cache",
    paste0(
      "v",
      sub("^.*/", "", .gflowui_basin_disk_cache_schema)
    ),
    paste0(fingerprint, ".rds")
  )
}

gflowui_valid_cached_basin_object <- function(
    object,
    expected_field_fingerprint,
    require_merge_tree = FALSE) {
  if (!inherits(object, "basin_complex") ||
      !identical(as.character(object$status %||% ""), "ok") ||
      !is.data.frame(object$basin.table)) {
    return(FALSE)
  }
  if (isTRUE(require_merge_tree) &&
      !identical(
        as.character(object$method %||% object$parameters$method %||% ""),
        "superlevel_merge_tree"
      )) {
    return(FALSE)
  }
  values <- object$field$input.values
  if (is.null(values) || any(!is.finite(as.numeric(values)))) {
    return(FALSE)
  }
  identical(
    gflowui_basin_field_fingerprint(values),
    as.character(expected_field_fingerprint)
  )
}

gflowui_load_basin_disk_cache <- function(
    reconstruction_fingerprint,
    cache_key,
    field) {
  path <- gflowui_basin_disk_cache_path(reconstruction_fingerprint)
  not_found <- function(reason = "") {
    list(
      found = FALSE,
      reason = as.character(reason),
      path = path,
      basin = NULL,
      prominence_complex = NULL
    )
  }
  if (!nzchar(path) || !file.exists(path)) {
    return(not_found())
  }
  envelope <- tryCatch(readRDS(path), error = function(e) e)
  field_fingerprint <- gflowui_basin_field_fingerprint(field)
  valid <- !inherits(envelope, "error") &&
    is.list(envelope) &&
    identical(
      as.character(envelope$schema %||% ""),
      .gflowui_basin_disk_cache_schema
    ) &&
    identical(
      tolower(as.character(
        envelope$construction_fingerprint %||% ""
      )),
      tolower(as.character(reconstruction_fingerprint))
    ) &&
    identical(
      as.character(envelope$cache_key %||% ""),
      as.character(cache_key)
    ) &&
    identical(
      as.character(envelope$field_fingerprint %||% ""),
      field_fingerprint
    ) &&
    gflowui_valid_cached_basin_object(
      envelope$basin,
      field_fingerprint
    ) &&
    gflowui_valid_cached_basin_object(
      envelope$prominence_complex,
      field_fingerprint,
      require_merge_tree = TRUE
    )
  if (!isTRUE(valid)) {
    unlink(path, force = TRUE)
    return(not_found("The disk cache entry was invalidated."))
  }
  list(
    found = TRUE,
    reason = "",
    path = normalizePath(path, winslash = "/", mustWork = TRUE),
    basin = envelope$basin,
    prominence_complex = envelope$prominence_complex,
    created_at = as.character(envelope$created_at %||% "")
  )
}

gflowui_write_basin_disk_cache <- function(
    reconstruction_fingerprint,
    cache_key,
    field,
    basin,
    prominence_complex) {
  path <- gflowui_basin_disk_cache_path(reconstruction_fingerprint)
  if (!nzchar(path)) {
    return(list(written = FALSE, path = "", reason = "No cache identity."))
  }
  field_fingerprint <- gflowui_basin_field_fingerprint(field)
  if (!gflowui_valid_cached_basin_object(basin, field_fingerprint) ||
      !gflowui_valid_cached_basin_object(
        prominence_complex,
        field_fingerprint,
        require_merge_tree = TRUE
      )) {
    return(list(
      written = FALSE,
      path = path,
      reason = "The canonical basin objects did not pass cache validation."
    ))
  }
  envelope <- list(
    schema = .gflowui_basin_disk_cache_schema,
    construction_fingerprint = tolower(as.character(
      reconstruction_fingerprint
    )),
    cache_key = as.character(cache_key),
    field_fingerprint = field_fingerprint,
    created_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    basin = basin,
    prominence_complex = prominence_complex
  )
  saved <- tryCatch(
    gflowui_basin_atomic_save_rds(envelope, path),
    error = function(e) e
  )
  if (inherits(saved, "error")) {
    return(list(
      written = FALSE,
      path = path,
      reason = conditionMessage(saved)
    ))
  }
  list(written = TRUE, path = saved, reason = "")
}
