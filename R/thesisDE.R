#' Create a paged HTML thesis document suitable for printing
#'
#' This output format is similar to \code{\link{html_paged}}. The only
#' difference is in the default stylesheets and Pandoc template. See
#' \url{https://pagedown.rbind.io/thesis-paged/} for an example.
#' @param ...,css,template Arguments passed to \code{\link{html_paged}()}.
#' @return An R Markdown output format.
#' @import pagedown
#' @export
thesis_de_paged = function(
  ..., css = c('thesis'), template = pkg_resource('html', 'thesisDE.html')
) {
  pagedown::html_paged(..., css = css, template = template)
}

#' Create a paged HTML document suitable for printing
#'
#' This is an output format based on \code{bookdown::html_document2} (which
#' means you can use those Markdown features added by \pkg{bookdown}). The HTML
#' output document is split into multiple pages via a JavaScript library
#' \pkg{paged.js}. These pages contain elements commonly seen in PDF documents,
#' such as page numbers and running headers.
#'
#' When a path or an url is passed to the \code{front_cover} or \code{back_cover}
#' argument, several CSS variables are created. They are named \code{--front-cover}
#' and \code{--back-cover} and can be used as value for the CSS property \code{background-image}.
#' For example, \code{background-image: var(--front-cover);}. When a vector of
#' paths or urls is used as a value for \code{front_cover} or \code{back_cover},
#' the CSS variables are suffixed with an index: \code{--front-cover-1},
#' \code{--front-cover-2}, etc.
#'
#' @param ... Arguments passed to
#'   \code{bookdown::\link[bookdown]{html_document2}}.
#' @param css A character vector of CSS file paths. If a path does not contain
#'   the \file{.css} extension, it is assumed to be a built-in CSS file. For
#'   example, \code{default-fonts} means the file
#'   \code{pagedown:::pkg_resource('css', 'default-fonts.css')}. To see all
#'   built-in CSS files, run \code{pagedown:::list_css()}.
#' @param theme The Bootstrap theme. By default, Bootstrap is not used.
#' @param template The path to the Pandoc template to convert Markdown to HTML.
#' @param csl The path of the Citation Style Language (CSL) file used to format
#'   citations and references (see the \href{https://pandoc.org/MANUAL.html#citations}{Pandoc documentation}).
#' @param front_cover,back_cover Paths or urls to image files to be used
#'   as front or back covers. Theses images are available through CSS variables
#'   (see Details).
#' @references \url{https://pagedown.rbind.io}
#' @return An R Markdown output format.
#' @import stats utils
#' @export
html_paged = function(
  ..., css = c('default-fonts', 'default-page', 'default'), theme = NULL,
  template = pkg_resource('html', 'paged.html'), csl = NULL,
  front_cover = NULL, back_cover = NULL
) {
  html_format(
    ..., css = css, theme = theme, template = template, .pagedjs = TRUE,
    .pandoc_args = c(
      lua_filters('uri-to-fn.lua', 'loft.lua', 'footnotes.lua'), # uri-to-fn.lua must come before footnotes.lua
      if (!is.null(csl)) c('--csl', csl),
      pandoc_chapter_name_args(),
      pandoc_covers_args(front_cover, back_cover)
    ),
    .dependencies = covers_dependencies(front_cover, back_cover)
  )
}
html_format = function(
  ..., self_contained = TRUE, anchor_sections = FALSE, mathjax = 'default', css, template, pandoc_args = NULL,
  .dependencies = NULL, .pagedjs = FALSE, .pandoc_args = NULL, .test = FALSE
) {
  if (!identical(mathjax, 'local')) {
    if (identical(mathjax, 'default'))
      mathjax = rmarkdown:::default_mathjax()

    # workaround the rmarkdown warning when self_contained is TRUE
    # see https://github.com/rstudio/pagedown/issues/128#issuecomment-518371613
    if (isTRUE(self_contained) && !is.null(mathjax)) {
      pandoc_args = c(pandoc_args, paste0('--mathjax=', mathjax))
      mathjax = NULL # let rmarkdown believe that we do not use MathJax
    }
  }

  css2 = grep('[.]css$', css, value = TRUE, invert = TRUE)
  css  = setdiff(css, css2)
  check_css(css2)

  pandoc_args = c(
    .pandoc_args,
    pandoc_args,
    # use the pagebreak pandoc filter provided by rmarkdown 1.16:
    if (isTRUE(.pagedjs)) pandoc_metadata_arg('newpage_html_class', 'page-break-after')
  )

  html_document2 = function(..., extra_dependencies = list()) {
    bookdown::html_document2(..., extra_dependencies = c(
      extra_dependencies, .dependencies,
      pagedown_dependency(xfun::with_ext(css2, '.css'), .pagedjs, .test)
    ))
  }

  fmt = html_document2(
    ..., self_contained = self_contained, anchor_sections = anchor_sections, mathjax = mathjax, css = css,
    template = template, pandoc_args = pandoc_args
  )

  # Deactivate the use of the shadow DOM by the flextable package (see https://github.com/rstudio/pagedown/issues/216)
  # We may remove that when https://gitlab.pagedmedia.org/tools/pagedjs/issues/148 will be solved
  if (isTRUE(.pagedjs)) {
    fmt$knitr$opts_chunk[['ft.shadow']] = FALSE
  }

  fmt
}


pkg_resource = function(...) {
  system.file('resources', ..., package = 'pagedown', mustWork = TRUE)
}

lua_filters = function(...) {
  c(rbind("--lua-filter", pkg_resource('lua', c(...))))
}

list_css = function() {
  css = list.files(pkg_resource('css'), '[.]css$', full.names = TRUE)
  setNames(css, gsub('.css$', '', basename(css)))
}

check_css = function(css) {
  valid = names(list_css())
  if (length(invalid <- setdiff(css, valid)) == 0) return()
  invalid = invalid[1]
  maybe = sort(agrep(invalid, valid, value = TRUE))[1]
  hint = if (is.na(maybe)) '' else paste0('; did you mean "', maybe, '"?')
  stop(
    '"', invalid, '" is not a valid built-in CSS filename', if (hint != "") hint else ".",
    " Use `pagedown:::list_css()` to view all built-in CSS filenames.", call. = FALSE
  )
}

merge_list = function(x, y) {
  x[names(y)] = y
  x
}

to_json = function(x, ..., auto_unbox = TRUE, null = 'null') {
  jsonlite::toJSON(x, ..., auto_unbox = auto_unbox, null = null)
}

`%n%` = knitr:::`%n%`

run_servr = function() {
  # see https://github.com/rstudio/httpuv/issues/250
  later::with_loop(later::global_loop(), httpuv::service(NA))
}
