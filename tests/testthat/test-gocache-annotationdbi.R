test_that("the SQL GO lookup returns mapIds() values without repeats", {
  skip_if_no_bioc("org.Hs.eg.db")
  skip_if_no_bioc("GO.db")
  db <- org.Hs.eg.db::org.Hs.eg.db
  terms <- AnnotationDbi::Ontology(GO.db::GOTERM)
  keys <- c(names(terms)[terms == "CC"][1:200], "GO:0000000")
  for (column in c("ENTREZID", "SYMBOL", "ENSEMBL")) {
    stock <- suppressMessages(
      AnnotationDbi::mapIds(db, keys = keys, column = column, keytype = "GOALL", multiVals = "list")
    )
    fast <- gocache_mapids(db, keys = keys, column = column, keytype = "GOALL", multiVals = "list")
    expect_identical(fast, lapply(stock, unique))
  }
})

test_that("other lookups fall back to mapIds()", {
  skip_if_no_bioc("org.Hs.eg.db")
  db <- org.Hs.eg.db::org.Hs.eg.db
  keys <- c("1", "2", "9")
  expect_identical(
    suppressMessages(gocache_mapids(db, keys = keys, column = "SYMBOL", keytype = "ENTREZID", multiVals = "first")),
    suppressMessages(AnnotationDbi::mapIds(db, keys = keys, column = "SYMBOL", keytype = "ENTREZID", multiVals = "first"))
  )
})

test_that("get_GO_data() builds the same GO data with the SQL lookup", {
  skip_if_no_bioc("clusterProfiler")
  skip_if_no_bioc("org.Hs.eg.db")
  original <- get("get_GO_data", envir = asNamespace("clusterProfiler"))
  if (gocache_is_replacement(original)) {
    original <- gocache_original("clusterProfiler", "get_GO_data")
  }
  testthat::skip_if_not(
    compat_port_applies("bioc322-getgodata-sql", original) || compat_port_applies("bioc323-getgodata-sql", original),
    "port not verified for these versions"
  )
  fast <- gocache_fast_go_data(original)
  expect_false(identical(body(fast), body(original)))
  build <- function(fun) {
    if (exists(".GO_clusterProfiler_Env", envir = .GlobalEnv)) {
      rm(".GO_clusterProfiler_Env", envir = .GlobalEnv)
    }
    value <- fun(org.Hs.eg.db::org.Hs.eg.db, "CC", "SYMBOL")
    if (is.environment(value)) as.list(value, all.names = TRUE, sorted = TRUE) else value
  }
  expect_identical(build(fast), build(original))
})

test_that("information content matches GOSemSim", {
  skip_if_no_bioc("GOSemSim")
  skip_if_no_bioc("org.Hs.eg.db")
  original <- get("computeIC", envir = asNamespace("GOSemSim"))
  if (gocache_is_replacement(original)) {
    original <- gocache_original("GOSemSim", "computeIC")
  }
  testthat::skip_if_not(compat_port_applies("bioc322-computeic", original), "port not verified for these versions")
  annotation <- AnnotationDbi::select(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = AnnotationDbi::keys(org.Hs.eg.db::org.Hs.eg.db, keytype = "ENTREZID")[1:2000],
    keytype = "ENTREZID",
    columns = c("GO", "ONTOLOGY")
  ) |>
    suppressMessages()
  annotation <- annotation[!is.na(annotation$GO) & annotation$ONTOLOGY == "MF", ]
  expect_identical(gocache_compute_ic(annotation, "MF"), original(annotation, "MF"))
})

test_that("the data frame unique() keeps the same rows and row names", {
  x <- data.frame(a = c("x", "y", "x", NA, NA), b = c(1, 2, 1, 3, 3), c = c("p", "q", "p", "r", "r"))
  expect_identical(gocache_unique(x), unique(x))
  expect_identical(gocache_unique(x[, 2:1]), unique(x[, 2:1]))
  expect_identical(gocache_unique(x[0, ]), unique(x[0, ]))
  expect_identical(gocache_unique(c(1, 1, 2)), unique(c(1, 1, 2)))
})
