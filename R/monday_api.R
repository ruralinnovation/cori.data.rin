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
