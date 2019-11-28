unescape_html <- function(str) {
  textxml <- paste0("<x>", str, "</x>")
  sapply(textxml, function(x) xml2::xml_text(xml2::read_html(x)))
}
