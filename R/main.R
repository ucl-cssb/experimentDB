#' Make experiment database
#'
#' Compile plate layout .csv files into a single .csv file to use for experiment
#' filtering. Writes a .csv file to one directory up from the first wildcard in
#' the given path.
#'
#' @param path character vector of patterns for relative or absolute filepaths.
#' Missing values will be ignored.
#'
#' @export
#'
make_experiment_db <- function(path = "*/*/data/*/metadata"){
  layout_files <- list.files(path = Sys.glob(path),
                             pattern = utils::glob2rx("*layout*.csv"),
                             full.names = T, all.files = T)

  experiment_db <- c()
  for(file in layout_files){
    layout <- utils::read.csv(file)
    splt_name <- unlist(strsplit(x = file, split = "/", ))
    experiment_path <- do.call(file.path, as.list(splt_name[1:(length(splt_name)-2)]))
    layout$experiment_path <- experiment_path
    experiment_db <- dplyr::bind_rows(experiment_db, layout)
  }

  # construct output directory one directory up from location of first wildcard
  # in 'path'
  splt_path <- unlist(strsplit(path, "/"))
  first_star_idx <- which(splt_path == "*")[1]
  if(first_star_idx == 1){
    out_dir <- "."
  } else {
    out_dir <- do.call(file.path, as.list(splt_path[1:(first_star_idx - 1)]))
  }

  utils::write.csv(x = experiment_db,
                   file = file.path(out_dir, "experiment_database.csv"),
                   row.names = F)
}


#' Apply filter to experiment database
#'
#' filter the experiment database by the available columns
#'
#' @param filters string specifying the filter conditions
#' @param experiment_db experiment database file
#'
#' @return data.frame containing the filtered experiment database
#' @export
#' @importFrom dplyr %>%
#'
filter_experiment_db <- function(filters, experiment_db = "experiment_database.csv"){
  experiment_db <- utils::read.csv(experiment_db)

  tryCatch(
    expr = {
      filtered_locations <- experiment_db %>%
        dplyr::filter(!!str2lang(filters))
    },
    error = function(cond){
      message("Filter parameter missing.")
      message(cond)
    }
  )

  return(filtered_locations)
}


#' Filter experiments without constructing a database
#'
#' @param filters string specifying the filter conditions
#' @param path character vector of patterns for relative or absolute filepaths. Missing values will be ignored.
#'
#' @return data.frame containing a filtered experiment database
#' @export
#' @importFrom dplyr %>%
#'
filter_experiments <- function(filters, path = "*/*/data/*/metadata"){
  layout_files <- list.files(path = Sys.glob(path),
                             pattern = utils::glob2rx("*layout*.csv"),
                             full.names = T, all.files = T)

  filtered_locations <- c()
  for(file in layout_files){
    layout <- utils::read.csv(file)

    # apply filters
    temp <- c()
    tryCatch(
      expr = {
        temp <- layout %>%
          dplyr::filter(!!str2lang(filters))
      },
      error = function(cond){
        message(paste("Filter parameter missing from file: ", file, sep = ""))
        # message(cond)
      }
    )

    # append to filtered table
    if(!is.null(temp)){
      if(nrow(temp) > 0){
        splt_name <- unlist(strsplit(x = file, split = "/", ))
        experiment_path <- do.call(file.path, as.list(splt_name[1:(length(splt_name)-2)]))
        temp$experiment_path <- experiment_path
        filtered_locations <- dplyr::bind_rows(filtered_locations, temp)
      }
    }
  }

  return(filtered_locations)
}

#' Get filtered experimental data
#'
#' @param filtered_locations data.frame returned by filter_experiment_db or filter_experiments
#'
#' @return data.frame containing filtered experimental data
#' @export
#' @importFrom dplyr %>%
#' @importFrom rlang .data
get_filtered_data <- function(filtered_locations){
  all_data <- c()
  for(path in unique(filtered_locations$experiment_path)){
    files <- Sys.glob(file.path(path, "processed", "*.csv"))
    experiment_data <- c()
    for(file in files){
      temp <- c()
      data <- utils::read.csv(file)

      if("well" %in% names(data)){
        temp <- dplyr::inner_join((filtered_locations %>% dplyr::filter(.data$experiment_path == path)),
                                 data)

        if(!is.null(temp)){
          if(nrow(temp) > 0){
            if(is.null(experiment_data)){
              experiment_data <- temp
            } else {
              experiment_data <- suppressMessages(dplyr::full_join(experiment_data, temp))
            }
          }
        }
      }
    }
    all_data <- dplyr::bind_rows(all_data, experiment_data)
  }
  return(all_data)
}
