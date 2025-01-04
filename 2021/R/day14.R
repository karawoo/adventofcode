#' Day 14: Extended Polymerization
#'
#' [Extended Polymerization](https://adventofcode.com/2021/day/14)
#'
#' @name day14
#' @rdname day14
#' @field template Starting template
#' @field fun_call Function call to `switch()` to convert two-letter pairs
#'   according to rules
#' @export
#' @examples
#' \dontrun{
#' p <- Polymer$new()
#' p$take_steps(10)
#' p$get_answer()
#' }
Polymer <- R6::R6Class("Polymer", list(
  template = readLines(
    system.file("input14.txt", package = "adventofcode2021"),
    n = 1
  ),
  fun_call = {
    x <- readLines(system.file("input14.txt", package = "adventofcode2021"))
    x <- x[3:length(x)]
    src <- substr(x, 1, 2)
    dest <- paste0(substr(x, 1, 1), substr(x, 7, 7), substr(x, 2, 2))
    subs <- setNames(as.list(c(quote(tmp), dest)), c("EXPR", src))
    rlang::call2(quote(switch), !!!subs)
  },

  #' @description take one step
  step = function() {
    res <- vector(mode = "character", length = nchar(self$template) - 1)
    for (i in seq_len(nchar(self$template))[-nchar(self$template)]) {
      tmp <- substr(self$template, i, i + 1)
      if (i == 1) {
        res[i] <- rlang::eval_bare(self$fun_call)
      } else {
        res[i] <- sub(".", "", rlang::eval_bare(self$fun_call))
      }
    }
    self$template <- paste(res, collapse = "")
    invisible(self)
  },

  #' @description take n steps
  #' @param n number of steps to take
  take_steps = function(n = 10) {
    replicate(n = n, self$step())
    invisible(self)
  },

  #' @description Calculate count of most common letter minus least common letter
  get_answer = function() {
    tab <- table(stringr::str_split(self$template, "", nchar(self$template)))
    max(tab) - min(tab)
  }
))
