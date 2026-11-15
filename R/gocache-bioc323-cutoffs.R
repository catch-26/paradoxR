gocache323_cutoffs_are_labels <- function(enrich_go) {
  if (!"enrichit" %in% loadedNamespaces()) {
    return(FALSE)
  }
  ora_gson <- get0("ora_gson", envir = asNamespace("enrichit"), inherits = FALSE)
  compat_port_applies("bioc323-ora-gson", ora_gson) &&
    compat_port_applies("bioc323-enrichgo-cutoffs", enrich_go)
}
