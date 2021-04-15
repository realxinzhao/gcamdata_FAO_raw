#Some functions are duplicated or adapted from FAOSTAT package

FAOsearch <- function (code = NULL, dataset = NULL, topic = NULL, latest = FALSE, full = TRUE)
{
  FAOxml <- XML::xmlParse("http://fenixservices.fao.org/faostat/static/bulkdownloads/datasets_E.xml")
  metadata <- XML::xmlToDataFrame(FAOxml, stringsAsFactors = FALSE)
  names(metadata) <- tolower(gsub("\\.", "_", names(metadata)))
  if (!is.null(code)) {
    metadata <- metadata[code == metadata[, "datasetcode"],
    ]
  }
  if (!is.null(dataset)) {
    metadata <- metadata[grep(dataset, metadata[, "datasetname"],
                              ignore.case = TRUE), ]
  }
  if (!is.null(topic)) {
    metadata <- metadata[grep(topic, metadata[, "topic"],
                              ignore.case = TRUE), ]
  }
  if (latest == TRUE) {
    metadata <- metadata[order(metadata$DateUpdate, decreasing = TRUE),
    ]
  }
  if (full == FALSE) {
    return(metadata[, c("datasetcode", "datasetname",
                        "dateupdate")])
  }
  if (full == TRUE) {
    return(metadata)
  }
  else (return("Invalid query"))
}

download_faostat_bulk <- function(code, data_folder){
  metadata <- FAOsearch(code = code)
  url_bulk = metadata$filelocation
  file_name <- basename(url_bulk)
  download.file(url_bulk, file.path(data_folder, file_name))
}

read_faostat_bulk <- function(zip_file_name){
  csv_file_name <- gsub(".zip$", ".csv", basename(zip_file_name))
  df <- readr::read_csv(unz(zip_file_name, csv_file_name), col_types = NULL)
  names(df) <- tolower(gsub("\\.", "_", names(df)))
  return(df)
}

get_faostat_bulk <- function (code, data_folder, download = F){
  assert_that(is.character(code))
  assert_that(is.character(data_folder))
  assert_that(is.logical(download))

  metadata <- FAOsearch(code = code)

  if (isTRUE(download)) {
    download_faostat_bulk(code = code, data_folder = data_folder)
  } else {
    assert_that(file.exists(file.path(data_folder, basename(metadata$filelocation))))
  }

  output <- read_faostat_bulk(file.path(data_folder, basename(metadata$filelocation)))
  return(output)
}


