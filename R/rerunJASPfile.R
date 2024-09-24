
fromJSON <- function(x) jsonlite::fromJSON(x, TRUE, FALSE, FALSE)
toJSON   <- function(x) jsonlite::toJSON(x, auto_unbox = TRUE, digits = NA, null = "null")

loadDatasetFromJaspFile <- function(archiveContents) {

  fileIndex <- which(archiveContents[["path"]] == "internal.sqlite")
  if (length(fileIndex) != 1L) {
    cli::cli_alert_info("Could not find \"internal.sqlite\" inside the jasp file.")
    return(data.frame())
  }

  dataDir <- file.path(tempdir(), "jaspData")
  if (!dir.exists(dataDir))
    dir.create(dataDir)

  archive::archive_extract(file, dir = dataDir, files = fileIndex)

  dbcon <- RSQLite::dbConnect(drv = RSQLite::SQLite(), dbname = file.path(dataDir, "internal.sqlite"))
  tables <- RSQLite::dbListTables(dbcon)

  lDataFrames <- vector("list", length = length(tables))

  ## create a data.frame for each table
  for (i in seq_along(tables))
    lDataFrames[[i]] <- RSQLite::dbGetQuery(conn = dbcon, statement = paste0("SELECT * FROM '", tables[[i]], "'"))

  dataset <- data.frame(row.names = seq_len(nrow(lDataFrames[[2]])))
  for (i in seq_len(nrow(lDataFrames[[1]]))) {

    name <- lDataFrames[[1]]$name[i]
    type <- lDataFrames[[1]]$columnType[i]

    if (type == "scale") {
      dataset[[name]] <- as.numeric(lDataFrames[[2]][[paste0("Column_", i, "_DBL")]])
      dataset[[name]][is.nan(dataset[[name]])] <- NA
    } else if (type == "ordinal" || type == "nominal") {

      constructor <- if (type == "ordinal") ordered else factor

      labels <- lDataFrames[[5]]$label[lDataFrames[[5]]$columnId == i]
      dataset[[name]] <- if (length(labels) > 0)
        constructor(lDataFrames[[2]][[paste0("Column_", i, "_INT")]], labels = labels)
      else constructor(lDataFrames[[2]][[paste0("Column_", i, "_DBL")]])
    } else {
      stop("Unknown column type: ", type)
    }

  }

  return(dataset)

}

rerunJASPfile <- function(file) {

  archiveContents <- archive::archive(file)
  fileIndex <- which(archiveContents[["path"]] == "analyses.json")

  if (length(fileIndex) != 1L)
    stop("Could not find a file \"analyses.json\" inside the jasp file.")

  fileCon <- archive::archive_read(file, file = fileIndex, mode = "r")

  on.exit({
    if (isOpen(fileCon)) # jsonlite automatically closes the connection
      close(fileCon)
  })

  contents <- fromJSON(fileCon)

  analysisNames <- vapply(contents$analyses, FUN = `[[`, "name", FUN.VALUE = character(1L))
  hasResults    <- vapply(contents$analyses, FUN = function(x) length(x[["results"]]) > 0, FUN.VALUE = logical(1L))

  dataset <- loadDatasetFromJaspFile(archiveContents)

  module <- getPkgOption("module.dirs")
  # TODO: intersect functions in module with function calls in analyses and inform about results

  idx <- which(hasResults)
  analysisNames <- analysisNames[idx]

  for (i in seq_along(analysisNames)) {

    analysisName <- analysisNames[i]
    analysisIdx  <- idx[i]

    cli::cli_alert_info("Analysis: {analysisName} {i} of {length(analysisNames)}")

    options    <- contents[["analyses"]][[analysisIdx]][["options"]]
    oldResults <- contents[["analyses"]][[analysisIdx]][["results"]]

    newResults <- runAnalysis(analysisName, dataset, options, view = FALSE, quiet = TRUE)

    # TODO: function to be written
    compareResults(oldResults, newResults)

  }

}


