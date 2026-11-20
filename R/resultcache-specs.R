# Functions whose results paradoxR stores. Each entry names the function and
# says what its result depends on beyond its arguments:
#
# * `packages`: packages it may use that are not in its Depends, Imports or
#   LinkingTo (for example Suggests that change the result when installed);
# * `options`: regular expressions for option names it reads;
# * `files`: a function of the argument list returning the paths of every
#   file it reads, so that their content becomes part of the key;
# * `project`: a function of the argument list that replaces large objects by
#   the parts the function reads, so that changes elsewhere in the object do
#   not count as a change. It must keep everything the function can read;
#   when in doubt, keep more.
# * `bypass`: a function of the argument list that returns TRUE when the call
#   must not be cached.
#
# Only add functions that are slow, return the same value for the same inputs
# and have no side effects beyond messages, warnings, options and the random
# number state, and check them with identical() against the original.

resultcache_specs <- function() {
  list(
    "seurat-findallmarkers" = list(
      pkg = "Seurat",
      name = "FindAllMarkers",
      packages = c("SeuratObject", "presto", "limma", "MAST", "DESeq2", "glmGamPoi"),
      options = c("^Seurat\\.", "^future\\."),
      project = resultcache_seurat_markers_inputs
    ),
    "diffbind-dba-count" = list(
      pkg = "DiffBind",
      name = "dba.count",
      packages = character(),
      options = c("^DiffBind\\.", "^mc\\.cores$"),
      files = resultcache_dba_files
    )
  )
}

# The parts of a Seurat object that FindAllMarkers() and FindMarkers() read:
# cell names and identities, the assay (all layers) or the reduction tested,
# the metadata columns named in `group.by` and `latent.vars` (all metadata and
# reductions when `latent.vars` is set, since FetchData() can reach either),
# the cluster tree for `node`, and the logged commands (they read the
# normalisation method from them) without their time stamps.
resultcache_seurat_markers_inputs <- function(args) {
  object <- args$object
  if (!methods::is(object, "Seurat")) {
    return(args)
  }
  dots <- args[["..."]]
  reduction <- if (is.list(dots)) dots$reduction else NULL
  assay <- args$assay
  if (is.null(assay)) {
    assay <- SeuratObject::DefaultAssay(object)
  }
  meta <- methods::slot(object, "meta.data")
  latent <- if (is.list(dots)) dots$latent.vars else NULL
  columns <- intersect(unlist(c(args$group.by, latent)), colnames(meta))
  commands <- lapply(methods::slot(object, "commands"), function(command) {
    if (methods::is(command, "SeuratCommand")) {
      list(name = command@name, assay = command@assay.used, params = command@params)
    } else {
      command
    }
  })
  args$object <- list(
    class = class(object),
    version = as.character(methods::slot(object, "version")),
    cells = colnames(object),
    idents = SeuratObject::Idents(object),
    default_assay = assay,
    data = if (is.null(reduction)) object[[assay]] else object[[reduction]],
    meta = if (is.null(latent)) meta[, columns, drop = FALSE] else meta,
    reductions = if (is.null(latent)) NULL else methods::slot(object, "reductions"),
    tree = if (is.null(args$node)) NULL else SeuratObject::Tool(object, slot = "BuildClusterTree"),
    commands = commands
  )
  args
}

resultcache_bam_files <- function(paths) {
  paths <- paths[!is.na(paths) & nzchar(paths)]
  index <- c(paste0(paths, ".bai"), sub("\\.bam$", ".bai", paths), paste0(paths, ".csi"))
  c(paths, index[file.exists(index)])
}

resultcache_dba_files <- function(args) {
  dba <- args$DBA
  files <- character()
  if (is.list(dba) && is.matrix(dba$class)) {
    rows <- intersect(c("bamRead", "bamControl", "Spikein"), rownames(dba$class))
    files <- as.character(dba$class[rows, , drop = FALSE])
  }
  peaks <- args$peaks
  if (is.character(peaks)) {
    files <- c(files, peaks)
  }
  resultcache_bam_files(unique(files))
}
