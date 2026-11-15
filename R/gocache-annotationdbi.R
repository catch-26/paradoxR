gocache_go_tables <- c(BP = "go_bp_all", CC = "go_cc_all", MF = "go_mf_all")

gocache_column_table <- function(x, column) {
  define_tables <- utils::getFromNamespace(".defineTables", "AnnotationDbi")
  table <- define_tables(x)[[column]]
  if (length(table) != 2L || startsWith(table[1], "go")) {
    return(NULL)
  }
  table
}

gocache_sql_ok <- function(x, keys, column, keytype, multiVals, dots) {
  length(dots) == 0L &&
    identical(keytype, "GOALL") &&
    identical(multiVals, "list") &&
    is.character(keys) && length(keys) > 0L && !anyDuplicated(keys) &&
    is.character(column) && length(column) == 1L &&
    methods::is(x, "OrgDb")
}

gocache_mapids_sql <- function(x, keys, column) {
  table <- gocache_column_table(x, column)
  con <- AnnotationDbi::dbconn(x)
  if (is.null(table) || !all(gocache_go_tables %in% DBI::dbListTables(con))) {
    return(NULL)
  }
  used <- vapply(gocache_go_tables, function(go_table) {
    ids <- DBI::dbGetQuery(con, paste0("SELECT DISTINCT go_id FROM ", go_table))
    any(ids$go_id %in% keys)
  }, logical(1))
  if (!any(used)) {
    values <- rep(list(NA_character_), length(keys))
    names(values) <- keys
    return(values)
  }
  sources <- paste0("SELECT DISTINCT _id, go_id FROM ", gocache_go_tables[used], collapse = " UNION ALL ")
  sql <- paste0(
    "SELECT a._id AS id, a.go_id AS go_id, k.", table[2], " AS value, k.rowid AS value_row ",
    "FROM (", sources, ") a LEFT JOIN ", table[1], " k ON a._id = k._id"
  )
  rows <- DBI::dbGetQuery(con, sql)
  rows$key <- match(rows$go_id, keys)
  rows <- rows[!is.na(rows$key), ]
  rows <- rows[order(rows$key, rows$id, rows$value_row, method = "radix"), ]
  value_id <- match(rows$value, unique(rows$value))
  rows <- rows[!duplicated(cbind(rows$key, value_id)), ]
  values <- split(as.character(rows$value), factor(rows$key, levels = seq_along(keys)))
  values[lengths(values) == 0L] <- list(NA_character_)
  names(values) <- keys
  values
}

gocache_mapids <- function(x, keys, column, keytype, ..., multiVals) {
  dots <- list(...)
  if (gocache_sql_ok(x, keys, column, keytype, multiVals, dots)) {
    value <- tryCatch(gocache_mapids_sql(x, keys, column), error = function(e) NULL)
    if (!is.null(value)) {
      return(value)
    }
  }
  AnnotationDbi::mapIds(x, keys = keys, column = column, keytype = keytype, ..., multiVals = multiVals)
}

gocache_unique <- function(x, ...) {
  if (!is.data.frame(x) || ...length() > 0L || ncol(x) == 0L ||
    !all(vapply(x, is.atomic, logical(1)))) {
    return(unique(x, ...))
  }
  id <- rep(1L, nrow(x))
  for (column in x) {
    value <- match(column, unique(column))
    id <- id * (max(value, 0L) + 1) + value
    id <- match(id, unique(id))
  }
  x[!duplicated(id), , drop = FALSE]
}

gocache_swaps <- list(
  list(from = quote(AnnotationDbi::mapIds), to = quote(gocache_mapids)),
  list(from = quote(unique), to = quote(gocache_unique))
)

gocache_swap <- function(expr) {
  if (missing(expr)) {
    return(quote(expr = ))
  }
  for (swap in gocache_swaps) {
    if (identical(expr, swap$from)) {
      return(swap$to)
    }
  }
  if (is.call(expr)) {
    return(as.call(lapply(as.list(expr), gocache_swap)))
  }
  expr
}

gocache_fast_go_data <- function(original) {
  new_body <- gocache_swap(body(original))
  if (identical(new_body, body(original))) {
    return(original)
  }
  fast <- original
  env <- new.env(parent = environment(original))
  env$gocache_mapids <- gocache_mapids
  env$gocache_unique <- gocache_unique
  body(fast) <- new_body
  environment(fast) <- env
  fast
}

gocache_compute_ic <- function(goAnno, ont) {
  gosemsim <- asNamespace("GOSemSim")
  if (!exists(".GOSemSimEnv", envir = .GlobalEnv)) {
    get(".initial", envir = gosemsim)()
  }
  gotbl <- get("gotbl", envir = get(".GOSemSimEnv", envir = .GlobalEnv))
  goids <- unique(gotbl[gotbl$Ontology == ont, "go_id"])
  counted <- table(goAnno$GO)
  gocount <- as.vector(counted)
  names(gocount) <- names(counted)
  missing_ids <- setdiff(goids, names(counted))
  zeros <- double(length(missing_ids))
  names(zeros) <- missing_ids
  gocount <- c(gocount, zeros)
  offsprings <- get("getOffsprings", envir = gosemsim)(ont)
  offsprings <- offsprings[match(goids, names(offsprings))]
  parent <- rep(seq_along(goids), lengths(offsprings))
  child <- gocount[unlist(offsprings, use.names = FALSE)]
  keep <- !is.na(child)
  offspring_count <- vapply(
    split(unname(child[keep]), factor(parent[keep], levels = seq_along(goids))),
    sum,
    numeric(1)
  )
  cnt <- gocount[goids] + offspring_count
  names(cnt) <- goids
  -log(cnt / sum(gocount))
}
