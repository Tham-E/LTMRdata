# --- Check previous data ---
# Check existing data with the newest data filtered to the same dates. Theoretically,
# there shouldn't be any changes; if there are, why?

generateComparisonReport <- function(newData, oldData, idCols = c("SampleID", "Taxa", "Length", "Count")) {
  # Helper function to generate hash and record IDs dynamically
  # Record IDs need to be distinct per sample, not just per sampling (currently is for publication)
  prepareData <- function(data, idCols) {
    data %>%
      # Concatenate all columns to create a basis for the hash
      tidyr::unite("allData", dplyr::everything(), remove = FALSE, sep = "|") %>%
      # Map the hash function
      dplyr::mutate(rowHash = purrr::map_chr(allData, rlang::hash)) %>%
      # Dynamically unite the specified ID columns
      tidyr::unite("recordId", dplyr::all_of(idCols), remove = FALSE, sep = "-")
  }
  # Prepare both tables
  processedNew <- prepareData(newData, idCols)
  processedOld <- prepareData(oldData, idCols)
  # 1. Identify New Records
  newIds <- setdiff(processedNew$recordId, processedOld$recordId)
  cat(length(newIds), "new datapoints to add.\n")
  # 2. Identify Changed Records
  newHashes <- processedNew %>% dplyr::select(recordId, rowHash)
  oldHashes <- processedOld %>% dplyr::select(recordId, rowHash)
  changedRecords <- newHashes %>%
    dplyr::inner_join(oldHashes, by = "recordId", suffix = c("_New", "_Old")) %>%
    dplyr::filter(rowHash_New != rowHash_Old)
  if (nrow(changedRecords) == 0) {
    cat("No changed records found.\n")
    return(NULL)
  }
  # 3. Investigation & Comparison Report
  diffCheckNew <- processedNew %>%
    dplyr::filter(recordId %in% changedRecords$recordId) %>%
    dplyr::select(-allData, -rowHash)
  diffCheckOld <- processedOld %>%
    dplyr::filter(recordId %in% changedRecords$recordId) %>%
    dplyr::select(-allData, -rowHash)
  comparisonReport <- diffCheckNew %>%
    # Join old and new values, using an explicit underscore boundary
    dplyr::inner_join(diffCheckOld, by = "recordId", suffix = c("_New", "_Old")) %>%
    # Pivot all columns (except recordId) to evaluate them in pairs
    tidyr::pivot_longer(
      cols = -recordId,
      names_to = c(".value", "version"),
      names_pattern = "(.*)_(New|Old)"
    ) %>%
    dplyr::group_by(recordId) %>%
    dplyr::summarise(dplyr::across(-version, ~ {
      # Use the 'version' column to safely grab the correct values
      newVal <- .x[version == "New"]
      oldVal <- .x[version == "Old"]
      if (!isTRUE(all.equal(newVal, oldVal))) {
        paste0("WAS: ", as.character(oldVal), " | NOW: ", as.character(newVal))
      } else {
        NA_character_
      }
    }), .groups = "drop") %>%
    # Drop columns entirely filled with NAs (the ones that didn't change)
    dplyr::select(recordId, tidyselect::where(~ !all(is.na(.))))
  return(comparisonReport)
}


# Run before updating the data object to the new data
# report<- generateComparisonReport(Salvage, LTMRdata::Salvage,
#                                   idCols = c("SampleID", "Taxa", "Length", "Count"))
