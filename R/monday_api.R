#' Fetch all data from a Monday.com board
#'
#' @param board_id Monday board ID
#' @param group_title Title of the group to fetch items from (e.g., "Current")
#'
#' @return data.frame with all columns from the board
#' @export
fetch_monday_board <- function(board_id, group_title) {

 token <- Sys.getenv("MONDAY_API_TOKEN")

  if (nchar(token) == 0) {
    stop("MONDAY_API_TOKEN environment variable not set")
  }

  monday_request <- function(query) {
    resp <- httr2::request("https://api.monday.com/v2") |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", token),
        "API-Version"   = "2023-10",
        "Content-Type"  = "application/json"
      ) |>
      httr2::req_body_json(list(query = query)) |>
      httr2::req_perform()

    httr2::resp_body_json(resp, simplifyVector = FALSE)
  }

  # Get group ID for the specified group title
  grp_query <- sprintf('query { boards(ids: [%s]) { groups { id title } } }', board_id)
  grp_resp <- monday_request(grp_query)
  groups <- grp_resp$data$boards[[1]]$groups

  grp_df <- do.call(rbind, lapply(groups, function(g) {
    data.frame(id = g$id, title = g$title, stringsAsFactors = FALSE)
  }))

  target_group <- grp_df[grp_df$title == group_title, "id"]

  if (length(target_group) == 0) {
    stop(sprintf("Group '%s' not found. Available groups: %s",
                 group_title, paste(grp_df$title, collapse = ", ")))
  }

  # Fetch items with ALL column values
  items_query <- sprintf('
query {
  boards(ids: [%s]) {
    items_page(limit: 500, query_params: {rules: [{column_id: "group", compare_value: ["%s"]}]}) {
      cursor
      items {
        id
        name
        created_at
        updated_at
        state
        column_values {
          id
          type
          text
          value
          ... on BoardRelationValue {
            display_value
          }
          ... on MirrorValue {
            display_value
          }
        }
      }
    }
  }
}', board_id, target_group)

  items_resp <- monday_request(items_query)
  items <- items_resp$data$boards[[1]]$items_page$items

  if (length(items) == 0) {
    warning(sprintf("No items found in group '%s'", group_title))
    return(data.frame())
  }

  # Build data.frame row by row
  rows <- lapply(items, function(item) {
    row <- list(
      item_id = item$id,
      name = item$name,
      created_at = item$created_at,
      updated_at = item$updated_at,
      state = item$state
    )

    for (cv in item$column_values) {
      val <- if (!is.null(cv$display_value) && cv$display_value != "") {
        cv$display_value
      } else if (!is.null(cv$text) && cv$text != "") {
        cv$text
      } else {
        NA_character_
      }
      row[[cv$id]] <- val
    }

    as.data.frame(row, stringsAsFactors = FALSE)
  })

  df <- dplyr::bind_rows(rows)

  message(sprintf("Fetched %d items from Monday board %s, group '%s'",
                  nrow(df), board_id, group_title))

  return(df)
}

#' Convert a Monday "Year Joined RIN" date value to an integer year
#'
#' @param x Date or POSIXt vector, or a character vector holding ISO dates from
#'   the Monday API ("2026-09-03") or display dates from an XLSX export ("Sep 3, 2026")
#'
#' @return integer vector of years, NA where x is missing, empty or unparseable
#' @noRd
.parse_year_joined <- function(x) {

  if (inherits(x, c("Date", "POSIXt"))) {
    return(as.integer(format(x, "%Y")))
  }

  x <- trimws(as.character(x))
  year <- rep(NA_integer_, length(x))

  is_iso <- grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}", x)
  year[is_iso] <- as.integer(substr(x[is_iso], 1, 4))

  is_display <- !is_iso & !is.na(x) & nzchar(x)
  year[is_display] <- as.integer(format(as.Date(x[is_display], format = "%b %d, %Y"), "%Y"))

  year
}

#' Fetch "Year Joined RIN" for every item on a Monday.com board
#'
#' Unlike `fetch_monday_board()`, this is not limited to one group, so communities
#' that have moved out of the "Current" group (e.g., to "Former") are included.
#'
#' @param board_id Monday board ID
#' @param date_column_id API id of the "Year Joined RIN" date column (default "date4")
#'
#' @return data.frame with columns `monday_id` (item id) and `year_joined` (integer year, NA if empty)
#' @export
fetch_monday_year_joined <- function(board_id, date_column_id = "date4") {

  token <- Sys.getenv("MONDAY_API_TOKEN")

  if (nchar(token) == 0) {
    stop("MONDAY_API_TOKEN environment variable not set")
  }

  monday_request <- function(query) {
    resp <- httr2::request("https://api.monday.com/v2") |>
      httr2::req_method("POST") |>
      httr2::req_headers(
        "Authorization" = paste("Bearer", token),
        "API-Version"   = "2023-10",
        "Content-Type"  = "application/json"
      ) |>
      httr2::req_body_json(list(query = query)) |>
      httr2::req_perform()

    resp <- httr2::resp_body_json(resp, simplifyVector = FALSE)

    if (!is.null(resp$errors)) {
      stop("Monday API error: ", jsonlite::toJSON(resp$errors, auto_unbox = TRUE))
    }

    resp
  }

  item_fields <- sprintf('cursor items { id column_values(ids: ["%s"]) { text } }', date_column_id)

  resp <- monday_request(sprintf(
    'query { boards(ids: [%s]) { items_page(limit: 500) { %s } } }', board_id, item_fields
  ))
  page <- resp$data$boards[[1]]$items_page
  items <- page$items

  while (!is.null(page$cursor)) {
    resp <- monday_request(sprintf(
      'query { next_items_page(limit: 500, cursor: "%s") { %s } }', page$cursor, item_fields
    ))
    page <- resp$data$next_items_page
    items <- c(items, page$items)
  }

  df <- dplyr::bind_rows(lapply(items, function(item) {
    date_text <- if (length(item$column_values) > 0) item$column_values[[1]]$text else NULL
    data.frame(
      monday_id = item$id,
      date_text = if (is.null(date_text)) NA_character_ else date_text,
      stringsAsFactors = FALSE
    )
  }))

  df$year_joined <- .parse_year_joined(df$date_text)

  message(sprintf("Fetched Year Joined RIN for %d items from Monday board %s (%d with a value)",
                  nrow(df), board_id, sum(!is.na(df$year_joined))))

  df[, c("monday_id", "year_joined")]
}
