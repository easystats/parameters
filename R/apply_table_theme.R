.apply_table_theme <- function(out, x, theme = "default", row_header_pos = NULL) {
  insight::check_if_installed("tinytable")

  switch(theme,
    grid = {
      out <- tinytable::tt(out, theme = "grid")
    },
    striped = {
      out <- tinytable::tt(out, theme = "striped")
    },
    bootstrap = {
      out <- tinytable::tt(out, theme = "bootstrap")
    },
    darklines = {
      # borders for sub headings
      if (!is.null(row_header_pos) && length(row_header_pos) > 1) {
        out <- tinytable::style_tt(
          out,
          i = row_header_pos[2:length(row_header_pos)],
          line = "b",
          line_color = "#cccccc",
          line_width = 0.05
        )
      }
      # top table border
      out <- tinytable::style_tt(
        out,
        i = -1,
        line = "t",
        line_width = 0.2,
        line_color = "#444444"
      )
      # table border between headers for model names and column headers
      out <- tinytable::style_tt(
        out,
        i = -1,
        j = 2:ncol(x),
        line = "b",
        line_color = "#999999"
      )
      # bottom table border
      out <- tinytable::style_tt(
        out,
        i = nrow(x) + length(row_header_pos),
        line_width = 0.15,
        line = "b",
        line_color = "#444444"
      )
    },
    # default theme
    {
      # borders for sub headings
      if (!is.null(row_header_pos) && length(row_header_pos) > 1) {
        out <- tinytable::style_tt(
          out,
          i = row_header_pos[2:length(row_header_pos)],
          line = "b",
          line_color = "#d4d4d4",
          line_width = 0.05
        )
      }
      # top table border
      out <- tinytable::style_tt(
        out,
        i = -1,
        line = "t",
        line_color = "#d4d4d4"
      )
      # table border between headers for model names and column headers
      out <- tinytable::style_tt(
        out,
        i = -1,
        j = 2:ncol(x),
        line = "b",
        line_color = "#d4d4d4"
      )
      # bottom table border
      out <- tinytable::style_tt(
        out,
        i = nrow(x) + length(row_header_pos),
        line = "b",
        line_color = "#d4d4d4"
      )
    }
  )
  out
}
