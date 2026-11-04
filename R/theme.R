# =============================================================================
# theme.R: shared VKM / FinnPRIO look (source of truth: host_map/4_scripts/theme.R).
# All UI colours live here. Clean white + one accent blue:
#   * white surfaces, dark grey text, hairline borders
#   * blue only on interactive elements; very light blue for hover/selection
#   * data colours (plots, maps) are separate and stay data-specific
# Self-contained: colours AND header/footer layout are added to the bslib theme,
# so no extra CSS file is needed. Needs www/vkm_logo.svg for the logo.
# =============================================================================

HM_COLOURS <- list(
  paper     = "#FFFFFF",
  text      = "#2B2F36",   # dark grey body text
  muted     = "#6B7280",   # labels, secondary text
  hairline  = "#E3E6EA",   # borders, dividers
  subtle    = "#F7F7F7",   # faint fills (striping, footer)
  blue      = "#2160D6",   # the accent: interactive elements only
  blue_soft = "#EAF1FD"    # hover / selection
)

hm_theme <- function(col = HM_COLOURS, ...) {
  rules <- sprintf("
    body { background: %1$s; color: %2$s; -webkit-font-smoothing: antialiased; }

    /* header: logo | hairline | title + subtitle */
    .vkm-header { display: flex; align-items: center; gap: 18px; flex-wrap: wrap; padding: 14px 24px 12px;
                  background: %1$s; border-bottom: 1px solid %4$s; }
    .vkm-logo { height: 44px; width: auto; display: block; }
    .vkm-sep { width: 1px; height: 36px; background: %4$s; }
    .vkm-t { font-size: 20px; font-weight: 600; line-height: 1.2; color: %2$s; }
    .vkm-subtitle { font-size: 13px; color: %3$s; }

    /* footer */
    .vkm-footer { display: flex; justify-content: space-between; flex-wrap: wrap; gap: 8px; font-size: 13px;
                  padding: 12px 24px; background: %1$s; border-top: 1px solid %4$s; color: %3$s; }
    .vkm-footer a { color: %6$s; }

    /* controls */
    .form-control, .form-select, .selectize-input { background: %1$s; border: 1px solid %4$s; color: %2$s; }
    .form-control:focus, .selectize-input.focus { border-color: %6$s; box-shadow: 0 0 0 3px %7$s; }
    input[type=checkbox], input[type=radio] { accent-color: %6$s; }
    .shiny-input-container input[type=checkbox]:checked, .shiny-input-container input[type=radio]:checked,
    .form-check-input:checked { background-color: %6$s !important; border-color: %6$s !important; }
    .selectize-dropdown .active { background: %7$s; color: %2$s; }
    .selectize-control.multi .selectize-input > div { background: %7$s; color: %2$s; border-radius: 4px; }

    /* buttons: blue = the action; secondary actions quiet with blue text */
    .btn.btn-primary { background-color: %6$s; border-color: %6$s; color: #FFFFFF; font-weight: 600; }
    .btn.btn-primary:hover, .btn.btn-primary:focus { background-color: %6$s; filter: brightness(0.92); color: #FFFFFF; }
    .btn.btn-secondary, .btn-default:not(.btn-primary), .btn-outline-secondary { background-color: %1$s; border: 1px solid %4$s; color: %6$s; }
    .btn.btn-secondary:hover, .btn-default:not(.btn-primary):hover, .btn-outline-secondary:hover { background-color: %7$s; border-color: %7$s; color: %6$s; }
    .btn:focus-visible, .nav-link:focus-visible, a:focus-visible { outline: 2px solid %6$s; outline-offset: 2px; }

    /* tabs */
    .nav-link { color: %3$s; }
    .nav-link:hover { color: %2$s; }
    .nav-tabs .nav-link.active, .nav-underline .nav-link.active, .nav-pills .nav-link.active { color: %6$s; }
    .nav-pills .nav-link.active { background-color: %7$s; }

    /* cards, tables */
    .card { border-color: %4$s; box-shadow: none; }
    .card-header { background: %1$s; border-bottom: 1px solid %4$s; }
    .bslib-sidebar-layout > .sidebar { background: %1$s; border-right: 1px solid %4$s; }
    table.dataTable thead th { color: %3$s; font-weight: 600; border-bottom: 1px solid %4$s !important; }
    table.dataTable.stripe tbody tr.odd > *, table.dataTable.display tbody tr.odd > *,
    table.dataTable.table-striped > tbody > tr:nth-of-type(odd) > * { box-shadow: inset 0 0 0 9999px %5$s; }
    table.dataTable tbody tr:hover > * { box-shadow: inset 0 0 0 9999px %7$s !important; }
    table.dataTable tbody tr.selected > * { box-shadow: inset 0 0 0 9999px %7$s !important; color: %2$s !important; }
    .dataTables_wrapper .pagination .page-item.active .page-link { background-color: %6$s; border-color: %6$s; color: #FFFFFF; }
    .dataTables_wrapper .pagination .page-link { color: %6$s; }
    .alert { border-radius: 4px; }
    @media (max-width: 768px) { .vkm-header { padding: 10px 14px; } .vkm-sep { display: none; } }
  ", col$paper, col$text, col$muted, col$hairline, col$subtle, col$blue, col$blue_soft)

  bslib::bs_theme(
    version = 5, bg = col$paper, fg = col$text,
    primary = col$blue, secondary = col$muted, info = col$blue,
    light = col$subtle, dark = col$text,
    base_font = bslib::font_collection("system-ui", "-apple-system", "Segoe UI", "Roboto", "Helvetica Neue", "Arial", "sans-serif"),
    ...
  ) |>
    bslib::bs_add_variables(
      "border-color" = col$hairline, "link-color" = col$blue, "link-hover-color" = col$blue, "text-muted" = col$muted,
      .where = "declarations"
    ) |>
    bslib::bs_add_rules(rules)
}

#' White header: VKM logo (from www/), hairline divider, app title and subtitle.
vkm_header <- function(title, subtitle = NULL, www = "www") {
  logo <- c(file.path(www, "vkm_logo.svg"), file.path(www, "vkm_logo.png"))
  logo <- logo[file.exists(logo)][1]
  shiny::div(class = "vkm-header",
    if (!is.na(logo)) shiny::tags$img(src = basename(logo), class = "vkm-logo",
                                      alt = "VKM – Vitenskapskomiteen for mat og miljø"),
    if (!is.na(logo)) shiny::div(class = "vkm-sep"),
    shiny::div(class = "vkm-title",
      shiny::div(class = "vkm-t", title),
      if (!is.null(subtitle)) shiny::div(class = "vkm-subtitle", subtitle)))
}

#' Footer: "{app name} is part of the FinnPRIO project. Questions: Daniel Flø, Daniel.Flo@vkm.no"
vkm_footer <- function(app_name, name = "Daniel Flø", email = "Daniel.Flo@vkm.no", extra = NULL) {
  shiny::div(class = "vkm-footer",
    shiny::span(sprintf("%s is part of the FinnPRIO project. Questions: %s, ", app_name, name),
                shiny::tags$a(href = paste0("mailto:", email), email)),
    if (!is.null(extra)) shiny::span(extra),
    shiny::span("VKM"))
}
