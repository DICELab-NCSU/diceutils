#' Export Google Drive folder, with conversion to locally-editable file types
#'
#' @param url URL of a Google Drive folder.
#' @param dir Local path where subfolders and files will be downloaded.
#' @param quiet Logical, whether to suppress googledrive messages during each download.
#' @param progress Logical, whether to show a download progress bar.
#'
#' @returns Nothing
#' @export
#'
#' @examples
#' export_drive()
#'
export_drive <- function(url, dir = ".", quiet = TRUE, progress = TRUE) {
  require(googledrive)
  require(dplyr)
  require(purrr)

  if (!dir.exists(dir)) {
    message("Directory ", dir, " does not exist.")
    md <- menu(c("Yes", "No (exit)"), title = "Do you want to create it?")
    if (md == 1L) {
      dir.create(dir, recursive = TRUE)
    } else {
      return(invisible(NULL))
    }
  }

  if (quiet) local_drive_quiet()

  # define path builder
  mk_path <- function(id, id_to_name, parent_lookup) {
    parent_id <- parent_lookup[[id]]
    if (is.na(parent_id) || !parent_id %in% names(id_to_name)) {
      return(id_to_name[[id]])
    } else {
      return(file.path(mk_path(parent_id, id_to_name, parent_lookup), id_to_name[[id]]))
    }
  }
  # fetch resources
  id <- googledrive::as_id(url)
  raw <- googledrive::drive_ls(path = id, recursive = TRUE) |>
    dplyr::mutate(type = purrr::map_chr(.x = drive_resource,
                                        .f = ~.x[["mimeType"]]),
                  parent = purrr::map_chr(.x = drive_resource,
                                          .f = ~unlist(.x[["parents"]])),
                  dnld = purrr::map_lgl(.x = drive_resource,
                                        .f = ~dplyr::case_when(
                                          any(c("webContentLink",
                                                "exportLinks") %in% names(.x)) ~ TRUE,
                                          TRUE ~ NA
                                        )))
  # map paths
  nn <- sum(grepl("\\/", raw$name))
  if (nn) warning(nn, " item names containing '/' found, replaced with '&'.")
  id_to_name <- setNames(trimws(gsub("\\/", "&", raw$name)), raw$id)
  parent_lookup <- setNames(raw$parent, raw$id)
  res <- dplyr::rowwise(raw) |>
    dplyr::mutate(path = file.path(dir, mk_path(id, id_to_name, parent_lookup))) |>
    ungroup()
  # make file structure
  fold <- filter(res, type == "application/vnd.google-apps.folder") |>
    pull(path)
  for (i in fold) {
    if (!dir.exists(i)) dir.create(i, recursive = TRUE)
  }
  # download accessible files
  direct <- dplyr::filter(res, dnld)
  purrr::map2(.x = direct$id, .y = direct$path,
              .f = ~googledrive::drive_download(as_id(.x), path = .y),
              .progress = progress)
}
