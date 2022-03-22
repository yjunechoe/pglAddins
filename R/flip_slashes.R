str_flip_slashes <- function(x) {
  if (grepl("[/\\]", x)){
    forwards <- stringi::stri_locate_all_fixed(x, '/', omit_no_match = TRUE)
    backwards <- stringi::stri_locate_all_fixed(x, '\\', omit_no_match = TRUE)
    stringi::stri_sub_all(x, forwards) <- "\\"
    stringi::stri_sub_all(x, backwards) <- "/"
  }
  x
}

#' Flip direction of slashes
#'
#' @return NULL
#' @export
flip_slashes <- function() {
  doc_context <- rstudioapi::getActiveDocumentContext()
  selections <- doc_context$selection
  if (length(selections) == 1 && selections[[1]]$text == "") {
    clipboard_context <- utils::readClipboard()[[1]]
    rstudioapi::insertText(selections$range, str_flip_slashes(clipboard_context), doc_context$id)
  } else {
    for (selection in selections){
      rstudioapi::modifyRange(selection$range, str_flip_slashes(selection$text), doc_context$id)
    }
  }
  invisible(NULL)
}
