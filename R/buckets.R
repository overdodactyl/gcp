#' Check if a directory or file is a mountpoint
#'
#' @param path A path to a file or directory
#'
#' @return logical
#' @export
is_mountpoint <- function(path) {
  mount <- system(paste0("mountpoint ", path), intern = TRUE)
  grepl("is a mountpoint", mount)
}


#' Unmount a directory or file
#'
#' @param path A path to a file or directory
#'
#' @return Silently returns `path`
#' @export
bucket_unmount <- function(path = Sys.getenv("GCP_MOUNTPOINT")) {
  system(glue::glue("fusermount -u {path}"))
  invisible(path)
}


#' Mount a directory from a Google Bucket
#'
#' @param path Path to a "directory" in google
#' @param new_path Path for the bucket to be mounted to
#' @param bucket Name of bucket
#' @param force Logical. If `new_path` is already a mountpoint, should it be replaced?
#' @param quiet Logical.  Should detailed information be printed
#'
#' @return Invisibly returns `new_path`
#' @export
bucket_mount_dir <- function(path = Sys.getenv("GCP_DIRECTORY"),
                             new_path = Sys.getenv("GCP_MOUNTPOINT"),
                             bucket = Sys.getenv("GCP_BUCKET"),
                             force = FALSE,
                             quiet = FALSE) {


  fs::dir_create(new_path)

  if (is_mountpoint(new_path)) {

    if (!force) {
      usethis::ui_stop(
        paste(
          usethis::ui_path(path),
          "is already a mountpoint. Use",
          usethis::ui_code("force = FALSE"),
          "if you want to mount here."
        )
      )
    } else {
      if (!quiet) {
        usethis::ui_info("Unmounting previously mountpoint")
      }
      bucket_unmount(new_path)
    }
  }

  path <- remove_leading_slash(path)
  path <- add_trailing_slash(path)

  cmd <- glue::glue("gcsfuse --only-dir {path} --implicit-dirs {bucket} {new_path}")
  res <- system(cmd, intern = TRUE)

  status <- attr(res, "status")

  if (is.null(status) & !quiet) {
    for (message in res) {

      if (grepl("has been successfully mounted", message)) {
        usethis::ui_done(message)
      } else {
        usethis::ui_info(message)
      }

    }
  } else {
    usethis::ui_warn("Could not mount")
  }

  invisible(new_path)


}

#' Return tidy bucket path
#'
#' Path without the preceding "gs://" or bucket name
#'
#' @param path Path
#' @param bucket Bucket name
#'
#' @return A path
#' @export
bucket_path_tidy <- function(path, bucket = Sys.getenv("GCP_BUCKET")) {
  path <- gsub(paste0("gs://", bucket), "", path)
  fs::as_fs_path(path)
}

#' Return a full bucket path
#'
#' Path with the preceding "gs://" and bucket name
#'
#' @param path Path
#' @param bucket Bucket name
#'
#' @return A path
#' @export
bucket_path <- function(path, bucket = Sys.getenv("GCP_BUCKET")) {
  path <- remove_leading_slash(path)
  paste0("gs://", bucket, "/", path)
}

#' List files in a bucket
#'
#' `bucket_ls` is equivalent to `gsutil ls` command.  It returns a vector of paths of
#'     files and directories.
#'
#' @param path A path
#' @param bucket A bucket
#'
#' @return
#' @export
bucket_ls <- function(path = "", bucket = Sys.getenv("GCP_BUCKET")) {

  cmd <- glue::glue("gsutil ls  gs://{bucket}/{path}")

  res <- system(cmd, intern = TRUE)

  bucket_path_tidy(res)

}


#' List Files
#'
#' `bucket_info` is equivalent to `gsutil -ls -l`
#'
#' @param path A path
#' @param bucket A bucket
#' @param recurse If `TRUE`, recurse fully.
#'
#' @return
#' @export
bucket_info <- function(path = "", bucket = Sys.getenv("GCP_BUCKET"), recurse = FALSE) {

  recurse <- ifelse(recurse, "-r", "")

  cmd <- glue::glue("gsutil ls -l {recurse} gs://{bucket}/{path}")

  res <- system(cmd, intern = TRUE)

  res <- trimws(res)

  # Last element is a summary
  res <- head(res, -1)

  split <- strsplit(res, "  ")

  df <- as.data.frame(do.call(rbind, split))

  names(df) <- c("size", "birth_time", "path")


  df$size <- fs::as_fs_bytes(df$size)
  df$birth_time <- strptime(df$birth_time, "%Y-%m-%dT%H:%M:%SZ")

  df$type <- ifelse(grepl("/$", df$path), "directory", "file")
  df$type <- factor(df$type, c("directory", "file"))

  df$path <- bucket_path_tidy(df$path)

  df <- df[,c("path", "type", "size", "birth_time")]

  df <- df[!grepl(":$", df$path), ]

  tibble::as_tibble(df)


}










