library(stringr)
library(dplyr)
library(glue)
library(osfr)

## Download and upload file

process_excel_file <- function(data_folder,
                               old_version,
                               gsheet_link = "https://docs.google.com/spreadsheets/d/e/2PACX-1vTHmMJRD2rVTS_l_NjKleAWuSiOyds7InL5aB8zU82enT6lLscTDgIjvjfbm2Wrx15YdyHX_BjKAxLM/pub?output=xlsx",
                               ...) {

  archive_folder <- osfr::osf_ls_files(data_folder, type = "folder") %>%
    dplyr::filter(name == "Archive")

  data_file <- osfr::osf_ls_files(data_folder) %>%
    dplyr::filter(name == "FReD.xlsx")

  temp_dir <- tempdir()

  osfr::osf_download(data_file, temp_dir, conflicts = "overwrite")
  file.rename(file.path(temp_dir, "FReD.xlsx"), file.path(temp_dir, glue("FReD_{old_version}.xlsx")))
  download.file(gsheet_link, file.path(temp_dir, "FReD.xlsx"), mode = "wb")

  dat <- try(openxlsx::read.xlsx(file.path(temp_dir, "FReD.xlsx"), sheet = 1))

  if (inherits(dat, "try-error")) {
    download.file(gsheet_link, file.path(temp_dir, "FReD.xlsx"), mode = "wb")
    dat <- try(openxlsx::read.xlsx(file.path(temp_dir, "FReD.xlsx"), sheet = 1))
    if (inherits(dat, "try-error")) {
      stop("Error reading the new data file. Please check the status of the download link and try again.")
    }
    message("Google Sheets download initially failed. Second attempt succeeded, but please check result.")

  }

  osfr::osf_upload(archive_folder, file.path(temp_dir, glue("FReD_{old_version}.xlsx")), conflicts = "overwrite")
  osfr::osf_upload(data_folder, file.path(temp_dir, "FReD.xlsx"), conflicts = "overwrite")
}

prepare_changelog <- function (release_notes,
                               version_type,
                               data_folder,
                               changelog_file = "https://osf.io/fj3xc") {

  temp_dir <- tempdir()
  osfr::osf_download(osfr::osf_retrieve_file(changelog_file), temp_dir, conflicts = "overwrite")
  markdown_content <- readLines(file.path(temp_dir, "change_log.md"), warn = FALSE) %>% paste(collapse = "\n")

  current_version <- extract_current_version(markdown_content)
  next_version <-  increment_version(current_version, version_type)

  markdown_content <- update_markdown_metadata(markdown_content = markdown_content, new_version = next_version, release_notes = release_notes)

  return(list(old_version = current_version, new_changelog = markdown_content))

}

upload_changelog <- function(changelog,
                             data_folder) {
  temp_dir <- tempdir()
  writeLines(changelog, file.path(temp_dir, "change_log.md"))
  osfr::osf_upload(data_folder, file.path(temp_dir, "change_log.md"), conflicts = "overwrite")

}

release_new_version <- function(release_notes,
                                version_type = c("minor", "major", "patch"),
                                osf_project = "9r62x",
                                osf_folder = "0 Data",
                                osf_token = Sys.getenv("OSF_TOKEN"), ...) {
  if (osf_token == "") {
    stop("Please provide the osf_token argument or set the the environment variable OSF_TOKEN")
  }
  osfr::osf_auth(osf_token)
  osf_project <- osfr::osf_retrieve_node(osf_project)
  data_folder <- osfr::osf_ls_files(osf_project, type = "folder") %>%
    dplyr::filter(name == osf_folder)
  changelog <- prepare_changelog(release_notes = release_notes, version_type = version_type, data_folder, ...)
  process_excel_file(data_folder = data_folder, old_version = changelog$old_version, ...)
  upload_changelog(changelog = changelog$new_changelog, data_folder)
  message("Successfully released version ", increment_version(changelog$old_version, version_type))
}

# Function to extract the current version from Markdown
extract_current_version <- function(markdown_content) {
  version_pattern <- "(?<=\\*\\*Version:\\*\\* )\\d+\\.\\d+\\.\\d+"
  version_matches <- str_extract(markdown_content, version_pattern)
  version_matches
}

# Function to increment version number
increment_version <- function(version, version_type = c("minor", "major", "patch")) {
  version_type <- version_type[1]
  version_numbers <- as.numeric(str_split(version, "[.]")[[1]])
  if (version_type == "major") {
    version_numbers[1] <- version_numbers[1] + 1
    version_numbers[2] <- 0
    version_numbers[3] <- 0
  } else if (version_type == "minor") {
    version_numbers[2] <- version_numbers[2] + 1
    version_numbers[3] <- 0
  } else if (version_type == "patch") {
    version_numbers[3] <- version_numbers[3] + 1
  }
  paste(version_numbers, collapse = ".")
}

update_markdown_metadata <- function(markdown_content, new_version, release_notes, release_date = Sys.Date()) {
  # Format the release date
  formatted_date <- format(release_date, "%Y-%m-%d")

  # Define a more robust pattern that matches the version line, with an optional existing date
  version_pattern <- "(### Current Version\\n- \\*\\*Version:\\*\\* )[\\d\\.v]+(?: \\(.*?\\))?"
  # Create the replacement string with the new version and date
  version_replacement <- glue("\\1{new_version} ({formatted_date})")

  # Update the version section using the new pattern and replacement
  markdown_content <- sub(version_pattern, version_replacement, markdown_content, perl = TRUE)
  # --- END OF MODIFIED PART ---

  # Correctly format and insert the new release notes, now including the date
  new_notes_formatted <- paste0("    ", str_replace_all(release_notes, "\n", "\n    "))
  new_notes_section <- glue("### Latest Release Notes\n- **Notes for Version {new_version} ({formatted_date})**\n{new_notes_formatted}\n\n### Previous Release Notes")


  # Move the old "Latest Release Notes" to "Previous Release Notes" and insert the new release notes
  markdown_content <- sub("### Latest Release Notes", new_notes_section, markdown_content)
  previous_notes <- str_extract(markdown_content, "(?<=### Latest Release Notes\n).+?(?=### Previous Release Notes)")

  if (!is.na(previous_notes)) {
    markdown_content <- sub("### Previous Release Notes", glue("{previous_notes}\n### Previous Release Notes"), markdown_content)
  }

  split_content <- str_split(markdown_content, "(### Previous Release Notes\n)", n = 2, simplify = TRUE)
  if (length(split_content) > 1) {
    split_content[2] <- split_content[2] %>% str_remove_all("### Previous Release Notes\n") %>% str_replace_all("\n\n", "\n")
    markdown_content <- paste0(split_content[1], "### Previous Release Notes\n", split_content[2])
  }
  markdown_content
}
