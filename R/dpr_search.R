#' search data from newstapa data site
#' 
#' @importFrom httr GET content
#' @importFrom rvest html_nodes html_text
#' @export
dpr_search <- function(query, allow_nested = F) {
  tar <- "https://data.newstapa.org/datasets?q="
  tar <- paste0(tar, query)
  res <- httr::GET(tar)
  hobj <- httr::content(res)
  
  title <- dpr_title(hobj)
  
  tagsr <- dpr_tags_raw(hobj)
  tags <- dpr_tags(tagsr)
  if (!allow_nested) {
    tags <- sapply(tags, function(x) paste0(x, collapse = ", "))
  }
  
  dr <- dpr_description_raw(hobj)
  
  from <- dpr_from(dr)
  desc <- dpr_description(dr)
  when <- dpr_date_created(dr)
  
  last <- dpr_last_updated(hobj)
  last_updated <- substr(last, 10, nchar(last))
  last_updated <- lubridate::ymd(last_updated)
  
  tibble::tibble(title,
                 from,
                 when,
                 last_updated,
                 desc,
                 tags)
}

dpr_parse <- function(node_default, attr_default) {
  func <- function(hobj, node = node_default, attr = attr_default) {
    nodes <- rvest::html_nodes(hobj, node)
    if (attr == "") {
      value <- rvest::html_text(nodes)
    } else {
      value <- rvest::html_attr(attr)
    }
    return(value)
  }
  return(func)
}

dpr_title <- dpr_parse("h3", "")
dpr_last_updated <- dpr_parse("p.last-updated", "")
dpr_tags_raw <- dpr_parse("div.tags", "")

dpr_tags <- function(trchr) {
  spchr <- strsplit(trchr, "\n")
  sapply(spchr, function(x) trimws(x)[nchar(trimws(x))>0])
}

dpr_description_raw <- function(hobj) {
  nodes <- rvest::html_nodes(hobj, "p.description")
  char <- as.character(nodes)
  return(char)
}

dpr_from <- function(drchr) {
  tar <- strsplit(drchr, "<br>")
  fir <- sapply(tar, function(x) x[2])
  res <- gsub("<.*?>","",fir)
  norm <- unescape_html(res)
  return(substr(norm, 5, nchar(norm)))
}

dpr_description <- function(drchr) {
  tar <- strsplit(drchr, "<br>")
  fir <- sapply(tar, function(x) x[4])
  res <- gsub("<.*?>","",fir)
  norm <- unescape_html(res)
  return(norm)
}

dpr_date_created <- function(drchr) {
  tar <- strsplit(drchr, "<br>")
  fir <- sapply(tar, function(x) x[1])
  res <- gsub("<.*?>","",fir)
  res <- substr(res, 7, nchar(res))
  return(res)
}
