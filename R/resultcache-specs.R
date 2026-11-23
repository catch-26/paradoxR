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
#   must not be cached, for example because its input is too small to be
#   worth hashing. It runs before anything else, on the arguments as passed.
# * `seed`: the name of the function's own seed argument. When it is set, the
#   function sets the random number state itself, so the state it was called
#   with is not part of the key.
# * `restore`: a function of the value, the call frame, the caller's call
#   and the original function that puts back what the function takes from
#   the caller's own expressions (through substitute() or match.call()),
#   since the original is called with the argument names.
#
# S3 methods are patched where Seurat and others do the heavy work, so that
# the stored value is a plain result (a DimReduc, graphs, a model) rather
# than a whole object with parts the function never read.
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
    ),
    "seurat-runpca" = list(
      pkg = "Seurat",
      name = "RunPCA.default",
      packages = "SeuratObject",
      options = "^Seurat\\.",
      seed = "seed.use"
    ),
    "seurat-runumap" = list(
      pkg = "Seurat",
      name = "RunUMAP.default",
      packages = "SeuratObject",
      options = c("^Seurat\\.", "^future\\."),
      seed = "seed.use",
      bypass = resultcache_umap_bypass
    ),
    "seurat-findneighbors" = list(
      pkg = "Seurat",
      name = "FindNeighbors.default",
      packages = "SeuratObject",
      options = c("^Seurat\\.", "^future\\."),
      bypass = function(args) isTRUE(args$cache.index) || !is.null(args$index)
    ),
    "seurat-sctransform" = list(
      pkg = "Seurat",
      name = "SCTransform.default",
      packages = c("SeuratObject", "glmGamPoi"),
      options = c("^Seurat\\.", "^future\\."),
      seed = "seed.use"
    ),
    "seurat-ccaintegration" = resultcache_seurat_integration("CCAIntegration"),
    "seurat-rpcaintegration" = resultcache_seurat_integration("RPCAIntegration"),
    "seurat-jointpcaintegration" = resultcache_seurat_integration("JointPCAIntegration"),
    "seurat-harmonyintegration" = resultcache_seurat_integration("HarmonyIntegration", "harmony"),
    "csaw-windowcounts" = list(
      pkg = "csaw",
      name = "windowCounts",
      packages = character(),
      options = character(),
      files = function(args) resultcache_bam_files(as.character(args$bam.files))
    ),
    "csaw-regioncounts" = list(
      pkg = "csaw",
      name = "regionCounts",
      packages = character(),
      options = character(),
      files = function(args) resultcache_bam_files(as.character(args$bam.files))
    ),
    "genomicalignments-summarizeoverlaps" = list(
      pkg = "GenomicAlignments",
      name = ".dispatchBamFiles",
      packages = character(),
      options = character(),
      files = function(args) resultcache_bam_files(unname(BiocGenerics::path(args$reads)))
    ),
    "chipseeker-gettagmatrix" = list(
      pkg = "ChIPseeker",
      name = "getTagMatrix",
      packages = "TxDb.Hsapiens.UCSC.hg19.knownGene",
      options = character(),
      files = resultcache_peak_file
    ),
    "enrichedheatmap-normalizetomatrix" = list(
      pkg = "EnrichedHeatmap",
      name = "normalizeToMatrix",
      packages = character(),
      options = character(),
      restore = resultcache_heatmap_names
    ),
    "signac-featurematrix" = list(
      pkg = "Signac",
      name = "FeatureMatrix",
      packages = character(),
      options = c("^future\\."),
      files = resultcache_fragment_files
    ),
    "gsva-gsva" = list(
      pkg = "GSVA",
      name = "gsva,gsvaParam",
      packages = character(),
      options = character(),
      bypass = function(args) !is.matrix(methods::slot(args$param, "exprData"))
    ),
    "sva-combatseq" = list(
      pkg = "sva",
      name = "ComBat_seq",
      packages = character(),
      options = character()
    ),
    "deseq2-lfcshrink" = list(
      pkg = "DESeq2",
      name = "lfcShrink",
      packages = c("apeglm", "ashr"),
      options = character(),
      project = resultcache_lfcshrink_inputs
    ),
    "stats-hclust" = list(
      pkg = "stats",
      name = "hclust",
      packages = character(),
      options = character(),
      bypass = function(args) !inherits(args$d, "dist") || attr(args$d, "Size") < 5000L,
      restore = resultcache_restore_call
    ),
    "mass-kde2d" = list(
      pkg = "MASS",
      name = "kde2d",
      packages = character(),
      options = character(),
      bypass = function(args) {
        n <- if (is.null(args$n)) 25 else args$n
        as.numeric(length(args$x)) * prod(rep_len(n, 2L)) < 2e8
      }
    )
  )
}

# lfcShrink() never reads the fitted dispersion function of the
# DESeqDataSet; it is a closure with a new environment on every DESeq() run,
# so only its attributes enter the key.
resultcache_lfcshrink_inputs <- function(args) {
  dds <- args$dds
  if (!methods::is(dds, "DESeqDataSet")) {
    return(args)
  }
  slots <- setdiff(methods::slotNames(dds), "dispersionFunction")
  fun <- methods::slot(dds, "dispersionFunction")
  args$dds <- c(
    list(class = class(dds), dispersion_function = if (is.function(fun)) attributes(utils::removeSource(fun)) else fun),
    lapply(stats::setNames(slots, slots), methods::slot, object = dds)
  )
  args
}

# Puts back the call that functions such as hclust() record with
# match.call(), as the caller wrote it.
resultcache_restore_call <- function(value, frame, call, original) {
  if (is.list(value) && !is.null(value$call)) {
    value$call <- match.call(original, call)
  }
  value
}

resultcache_seurat_integration <- function(name, packages = character()) {
  list(
    pkg = "Seurat",
    name = name,
    packages = c("SeuratObject", packages),
    options = c("^Seurat\\.", "^future\\.")
  )
}

# uwot is the only UMAP back end whose result is reproducible from R; a
# returned or reused model holds an Annoy index that cannot be stored.
resultcache_umap_bypass <- function(args) {
  method <- if (is.null(args$umap.method)) "uwot" else args$umap.method
  !identical(method, "uwot") || isTRUE(args$return.model) || !is.null(args$reduction.model)
}

# normalizeToMatrix() records the expressions passed as `signal` and
# `target`.
resultcache_heatmap_names <- function(value, frame, call, original) {
  if (inherits(value, "normalizedMatrix")) {
    attr(value, "signal_name") <- deparse(eval(quote(substitute(signal)), frame))
    attr(value, "target_name") <- deparse(eval(quote(substitute(target)), frame))
  }
  value
}

resultcache_peak_file <- function(args) {
  if (is.character(args$peak)) args$peak else character()
}

resultcache_fragment_files <- function(args) {
  fragments <- args$fragments
  if (!is.list(fragments)) {
    fragments <- list(fragments)
  }
  paths <- unlist(lapply(fragments, function(fragment) {
    if (methods::is(fragment, "Fragment")) methods::slot(fragment, "path") else character()
  }))
  c(paths, paste0(paths, ".tbi"))
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
