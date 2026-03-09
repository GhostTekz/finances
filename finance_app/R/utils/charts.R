# R/utils/charts.R — Reusable plotly chart functions

library(plotly)
library(dplyr)

#' Income vs Expense grouped bar chart
#' df must have columns: month, type (income/expense), total
chart_income_expense_bar <- function(df) {
  inc <- df[df$type == "income",  ]
  exp <- df[df$type == "expense", ]

  plot_ly() %>%
    add_bars(data = inc, x = ~month, y = ~total,
             name = "Income",   marker = list(color = "#00D97E")) %>%
    add_bars(data = exp, x = ~month, y = ~total,
             name = "Expenses", marker = list(color = "#E63757")) %>%
    layout(
      barmode    = "group",
      xaxis      = list(title = ""),
      yaxis      = list(title = "Amount ($)"),
      legend     = list(orientation = "h", y = 1.1),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      margin     = list(t = 20)
    )
}

#' Category donut / pie chart
#' df must have columns: category (or name), total, color (optional)
chart_category_pie <- function(df) {
  if ("category" %in% names(df)) {
    labels <- df$category
  } else if ("name" %in% names(df)) {
    labels <- df$name
  } else {
    labels <- seq_len(nrow(df))
  }

  colors <- if ("color" %in% names(df) && !all(is.na(df$color))) df$color else NULL

  p <- plot_ly(
    labels  = labels,
    values  = df$total,
    type    = "pie",
    hole    = 0.4,
    textinfo = "label+percent",
    hovertemplate = "%{label}: $%{value:,.2f}<extra></extra>"
  )
  if (!is.null(colors)) {
    p <- p %>% layout(marker = list(colors = colors))
  }
  p %>% layout(
    showlegend    = FALSE,
    margin        = list(t = 0, b = 0, l = 0, r = 0),
    paper_bgcolor = "rgba(0,0,0,0)",
    plot_bgcolor  = "rgba(0,0,0,0)"
  )
}

#' Line chart for net worth / trend
chart_line_trend <- function(df, x_col, y_col,
                              title = "", color = "#2C7BE5") {
  plot_ly(df, x = as.formula(paste0("~", x_col)),
              y = as.formula(paste0("~", y_col)),
          type = "scatter", mode = "lines+markers",
          line   = list(color = color, width = 2),
          marker = list(color = color, size = 5)) %>%
    layout(
      title  = list(text = title, font = list(size = 13)),
      xaxis  = list(title = ""),
      yaxis  = list(title = ""),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
}

#' Return an empty plotly chart with a centered message
plotly_empty_msg <- function(msg = "No data available.") {
  plot_ly() %>%
    layout(
      xaxis      = list(visible = FALSE),
      yaxis      = list(visible = FALSE),
      annotations = list(list(
        text      = msg,
        xref      = "paper", yref = "paper",
        x = 0.5, y = 0.5, showarrow = FALSE,
        font = list(size = 14, color = "#6E84A3")
      )),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)"
    )
}
