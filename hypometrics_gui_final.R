##### hypometrics Shiny GUI
#### Graphical user interface for the hypometrics R package
#### Authors: Maya Jhyount and Dr Gilberte Martine-Edith
#### Provides an interactive interface to the core functions of the hypometrics package
#### for integrated analysis of CGM, Fitbit activity, Fitbit sleep, and PRH data

### Dependencies: shiny, bslib, ggplot2, plotly, dplyr, tidyr, DT, lubridate, hypometrics, UpSetR
### Run with: shiny::runApp("hypometrics_gui_final.R")

### Loading required packages
library(shiny)      # Shiny web application framework
library(bslib)      # Bootstrap 5 UI theming
library(ggplot2)    # Plot construction
library(plotly)     # Interactive plot conversion via ggplotly()
library(dplyr)      # Data manipulation
library(tidyr)      # Data reshaping for long-format plotting
library(DT)         # Interactive data tables
library(lubridate)  # Datetime handling, used specifically for parsing Interval objects returned by sdhDetection()
library(hypometrics) # Core analysis package providing all CGM, sleep, activity and PRH functions

### Defining global constants used throughout the app

## Conversion factor for glucose unit handling
# cgmSummarise() requires mg/dL input regardless of the GlucoseUnit argument so mmol/L data is multiplied by this before the call and divided after
MMOL_TO_MGDL <- 18.0182

## Brand colours taken from the hypometrics package hex logo
BRAND_RED       <- "#B5280D" # Primary colour used for headings, buttons and borders
BRAND_RED_LIGHT <- "#E8836D" # Lighter tint used for hover states and highlight boxes

### Loading hypometrics example datasets at startup
## These populate the "Load example data" option on the Data tab so users can explore the GUI before uploading their own files
# cgm is the pre-interpolated version of raw_cgm, shown in the CGM (interp.) preview tab as a reference
data(raw_cgm,           package = "hypometrics")
data(cgm,               package = "hypometrics")
data(raw_sleep,         package = "hypometrics")
data(raw_step,          package = "hypometrics")
data(raw_hr,            package = "hypometrics")
data(raw_checkin,       package = "hypometrics")
data(raw_motif_segment, package = "hypometrics")
# Note: raw_promis, raw_eq5d5l and raw_wpai are not loaded as PRO measures are outside the current scope of this GUI

### Defining reusable helper functions for the UI and server
## These are defined before the UI and server blocks as R reads top to bottom
## and both sections reference these functions at definition time

# Creates a consistently styled CSV download button
download_btn_ui <- function(id, label = "Download CSV") {
  downloadButton(id, label, class = "btn-outline-secondary btn-sm mt-2")
}

## Styled message box wrappers used to give users feedback after analysis steps
# CSS classes for these are defined in the tags$head section
interp_box <- function(...) div(class = "interp-box", ...)           # neutral/informational
warn_box   <- function(...) div(class = "interp-box flag-warn", ...) # flags an issue
ok_box     <- function(...) div(class = "interp-box flag-ok", ...)   # confirms a pass

# Tip shown below multi-participant plots explaining the plotly legend interaction
# Users often do not discover this feature without being prompted
legend_tip <- function() {
  div(style = paste0("font-size:0.78rem; color:#777; margin-top:4px;
                      border-left:3px solid ", BRAND_RED_LIGHT, ";
                      padding-left:8px;"),
      "💡 Tip: click a participant name in the legend to show or hide their data.
     Double-click to isolate a single participant."
  )
}

### Shared ggplot2 theme applied to every plot in the GUI
## base_size = 14 scales all text elements proportionally across the whole theme
## All colours set explicitly for consistency with the white app background
# axis.text.x is angled at 30 degrees so date labels do not overlap on crowded x-axes
light_theme <- function() {
  theme_minimal(base_size = 14) +
    theme(
      plot.background   = element_rect(fill = "white", colour = NA),
      panel.background  = element_rect(fill = "white", colour = NA),
      text              = element_text(colour = "#1a1a1a", size = 14),
      axis.text         = element_text(colour = "#333333", size = 13),
      axis.text.x       = element_text(colour = "#333333", size = 13,
                                       angle = 30, hjust = 1),
      axis.title        = element_text(colour = "#1a1a1a", size = 14),
      legend.text       = element_text(colour = "#1a1a1a", size = 13),
      legend.title      = element_text(colour = "#1a1a1a", size = 13),
      legend.background = element_rect(fill = "white", colour = NA),
      legend.key        = element_rect(fill = "white", colour = NA),
      panel.grid.major  = element_line(colour = "#e5e5e5"),
      panel.grid.minor  = element_line(colour = "#f0f0f0"),
      strip.text        = element_text(colour = "#1a1a1a", size = 13),
      plot.title        = element_text(colour = "#1a1a1a", size = 14,
                                       face = "bold")
    )
}

### Applies plotly font and colour overrides after ggplotly() conversion
## ggplotly() does not reliably inherit ggplot theme settings, especially font sizes and axis colours
## Applied on every interactive plot as: ggplotly(p) %>% plotly_light()
plotly_light <- function(p) {
  p %>% layout(
    paper_bgcolor = "#ffffff",
    plot_bgcolor  = "#ffffff",
    font          = list(color = "#1a1a1a", size = 14),
    xaxis         = list(tickfont  = list(color = "#333333", size = 13),
                         titlefont = list(color = "#1a1a1a", size = 14),
                         tickangle = -30),
    yaxis         = list(tickfont  = list(color = "#333333", size = 13),
                         titlefont = list(color = "#1a1a1a", size = 14)),
    legend        = list(font = list(color = "#1a1a1a", size = 13)),
    hoverlabel    = list(bgcolor = BRAND_RED_LIGHT,
                         font    = list(color = "#ffffff"))
  )
}

### Converts lubridate Interval objects returned by sdhDetection() into readable strings
## DT::datatable cannot render Interval objects directly so conversion is needed for display
## Only the display copy uses this - the raw output is kept in rv$sdh_raw with Intervals intact
## so that sdhSummarise() receives the correct input type when called
interval_to_char <- function(x) {
  tryCatch(
    paste0(format(int_start(x), "%Y-%m-%d %H:%M"), " to ",
           format(int_end(x),   "%Y-%m-%d %H:%M")),
    error = function(e) as.character(x)
  )
}

# Fixed colour palette used across all plots via scale_colour_manual() and scale_fill_manual()
# Defining once here ensures all participants always map to the same colour across every tab
participant_colours <- c("#B5280D", "#2471A3", "#E8836D", "#8E44AD")

# Rounds any numeric column whose name contains "glucose", "glu", "BG", "bg" or "sgv" to 2dp
# Applied inside mk_dt() before every table render so glucose values are always consistent
round_glucose_cols <- function(df) {
  df <- as.data.frame(df)
  glucose_cols <- grep("glucose|glu|BG|bg|sgv", names(df), ignore.case=TRUE, value=TRUE)
  for (col in glucose_cols) {
    if (is.numeric(df[[col]])) df[[col]] <- round(df[[col]], 2)
  }
  df
}

### General-purpose DataTable renderer used for all output tables
## No row cap applied - DT handles pagination so all rows remain accessible
## scrollX allows wide tables to scroll horizontally rather than overflow the card
mk_dt <- function(df, page = 5) {
  datatable(round_glucose_cols(df),
            options = list(pageLength = page, scrollX = TRUE,
                           autoWidth = TRUE), rownames = FALSE)
}

# Creates a PNG download button styled consistently with the CSV download buttons
# Used on every plot output across all tabs
plot_download_btn <- function(id, label = "Download PNG") {
  downloadButton(id, label,
                 icon  = icon("image"),
                 class = "btn-outline-secondary btn-sm mt-2")
}

# Creates a styled sub-heading with a bold red title and grey description below
# Used in left-panel parameter cards to label each analysis function's controls
section_header <- function(title, description) {
  div(style = "margin-bottom:12px;",
      h5(title, style = paste0("color:", BRAND_RED,
                               "; font-weight:700; margin-bottom:4px;")),
      p(description, style = "font-size:0.82rem; color:#555; margin:0;")
  )
}

##### UI
#### Defines the visual layout and all user-facing components of the app
#### The UI is a tabsetPanel with one tab per analysis type

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "flatly",
                   primary = "#B5280D", secondary = "#555555"),
  
  tags$head(tags$style(HTML(paste0("
    body { background-color:#ffffff; color:#1a1a1a; }
    .nav-tabs .nav-link.active {
      border-bottom:3px solid ", BRAND_RED, " !important;
      color:", BRAND_RED, " !important; font-weight:600; }
    .nav-tabs .nav-link { color:#333; }
    .card { border:1px solid #ddd; box-shadow:0 1px 4px rgba(0,0,0,0.06); }
    .card-header { font-weight:700; letter-spacing:0.03em; font-size:0.85rem;
      text-transform:uppercase; color:", BRAND_RED, ";
      background-color:#fafafa; border-bottom:1px solid #eee; }
    .flag-ok   { color:#E8836D; font-weight:600; }
    .flag-warn { color:#B5280D; font-weight:600; }
    .section-label { font-size:0.72rem; text-transform:uppercase;
      letter-spacing:0.08em; color:#666; margin-bottom:4px; }
    .status-bar { background:#f5f5f5; border:1px solid #ddd; border-radius:6px;
      padding:8px 14px; font-size:0.85rem; margin-bottom:10px; color:#333; }
    h4.tab-title { font-size:1.1rem; font-weight:700;
      color:", BRAND_RED, "; margin-bottom:6px; }
    .interp-box { background:#fef9f9; border-left:3px solid ", BRAND_RED, ";
      padding:10px 14px; border-radius:4px; font-size:0.875rem;
      margin-top:8px; color:#333; }
    .btn-primary { background-color:", BRAND_RED, " !important;
      border-color:", BRAND_RED, " !important;
      color: #ffffff !important; }
    .btn-primary:hover { background-color:#8f1e09 !important;
      border-color:#8f1e09 !important; color: #ffffff !important; }
    .btn-primary:active, .btn-primary:focus {
      color: #ffffff !important; }
    hr { border-color:#eee; }
    /* DT pagination - override teal with brand red using high specificity */
    table.dataTable + div .dataTables_paginate span .paginate_button,
    div.dataTables_paginate span a.paginate_button,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current,
    .dataTables_wrapper .dataTables_paginate .paginate_button.current:hover {
      background: #B5280D !important; border-color: #B5280D !important;
      color: #fff !important; border-radius: 3px !important; }
    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {
      background: #E8836D !important; border-color: #E8836D !important;
      color: #fff !important; }
    /* All Bootstrap secondary/default buttons */
    .btn-default, .btn-secondary, .btn-outline-secondary {
      border-color: #B5280D !important; color: #B5280D !important; }
    .btn-default:hover, .btn-secondary:hover, .btn-outline-secondary:hover {
      background-color: #E8836D !important; color: #fff !important;
      border-color: #E8836D !important; }
    /* Download button icon colour */
    .btn-outline-secondary .glyphicon,
    .btn-outline-secondary i { color: #B5280D !important; }

    /* ---- Pagination - nuclear override ---- */
    /* Target every possible Bootstrap + DT pagination selector */
    .pagination > .active > a,
    .pagination > .active > a:focus,
    .pagination > .active > a:hover,
    .pagination > .active > span,
    .pagination > .active > span:focus,
    .pagination > .active > span:hover,
    ul.pagination li.active a,
    ul.pagination li.active span {
      background-color: #B5280D !important;
      border-color: #B5280D !important;
      color: #ffffff !important;
    }
    .pagination > li > a:hover,
    .pagination > li > span:hover,
    ul.pagination li a:hover {
      background-color: #E8836D !important;
      border-color: #E8836D !important;
      color: #ffffff !important;
    }
    /* DT specific paginate buttons (non-Bootstrap render) */
    .dataTables_paginate .paginate_button.current,
    .dataTables_paginate .paginate_button.current:hover {
      background: #B5280D !important;
      border: 1px solid #B5280D !important;
      color: #fff !important;
    }
    .dataTables_paginate .paginate_button:not(.current):not(.disabled):hover {
      background: #E8836D !important;
      border: 1px solid #E8836D !important;
      color: #fff !important;
    }
  ")))),
  
  fluidRow(
    column(12,
           div(style = paste0("padding:16px 0 12px 0; border-bottom:2px solid ",
                              BRAND_RED, "; margin-bottom:20px;"),
               h3("hypometrics",
                  style = paste0("display:inline; font-weight:800; color:", BRAND_RED, ";")),
               span(" - Integrated CGM, Activity, Sleep & Person-Reported Hypoglycaemia Explorer",
                    style = "color:#555; font-size:0.92rem; margin-left:10px;")
           )
    )
  ),
  
  # Runtime CSS + JS injection - ensures pagination colour fires after
  # DT and Bootstrap have loaded their own stylesheets
  tags$head(
    tags$style(HTML("
      .btn { color: inherit; }
      .btn-primary, .btn-primary:visited { color: #fff !important; }
      /* Flatly Bootstrap pagination active state */
      .page-item.active .page-link {
        background-color: #B5280D !important;
        border-color: #B5280D !important;
        color: #fff !important;
      }
      .page-link:hover {
        background-color: #E8836D !important;
        border-color: #E8836D !important;
        color: #fff !important;
      }
      .page-link { color: #B5280D !important; }
    ")),
    # JavaScript that runs after page fully loads to force pagination colour
    # This is needed because Bootstrap flatly compiles active pagination colour
    # into its own stylesheet with higher specificity
    tags$script(HTML("
      document.addEventListener('DOMContentLoaded', function() {

        // --- Pagination colour override ---
        var paginStyle = document.createElement('style');
        paginStyle.innerHTML =
          '.pagination .active a, .pagination .active span, ' +
          '.pagination .active a:hover, .pagination .active span:hover, ' +
          'ul.pagination li.active a, ul.pagination li.active span { ' +
          '  background-color: #B5280D !important; ' +
          '  border-color: #B5280D !important; ' +
          '  color: #fff !important; } ' +
          '.pagination li a:hover, ul.pagination li a:hover { ' +
          '  background-color: #E8836D !important; ' +
          '  border-color: #E8836D !important; ' +
          '  color: #fff !important; }';
        document.head.appendChild(paginStyle);

        // --- File input Browse button replacement ---
        // Inserts a visible styled button before each file input.
        // The native input is hidden but still functional (click triggers picker).
        function styleFileInputs() {
          // Shiny renders file inputs inside a structure:
          // div.shiny-input-container > div.input-group > input[type=file]
          // We target the div.input-group, hide it, and insert our
          // custom button before it so the native UI disappears entirely.
          document.querySelectorAll('input[type=file]').forEach(function(input) {
            if (input.dataset.styled) return;
            input.dataset.styled = '1';

            // Walk up to find Shiny input-group wrapper
            var inputGroup = input.closest('.input-group') || input.parentNode;
            var container  = inputGroup.parentNode;

            // Create our styled button
            var btn = document.createElement('button');
            btn.type = 'button';
            btn.textContent = 'Select file(s)';
            btn.style.cssText = [
              'background-color:#B5280D',
              'color:#ffffff',
              'border:none',
              'padding:5px 16px',
              'border-radius:4px',
              'cursor:pointer',
              'font-size:0.85rem',
              'font-weight:600',
              'white-space:nowrap',
              'display:inline-block',
              'margin-bottom:4px'
            ].join(';');
            btn.addEventListener('mouseover', function() {
              this.style.backgroundColor = '#8f1e09';
            });
            btn.addEventListener('mouseout', function() {
              this.style.backgroundColor = '#B5280D';
            });
            btn.addEventListener('click', function() { input.click(); });

            // Filename display
            var label = document.createElement('div');
            label.style.cssText = 'font-size:0.82rem; color:#555; margin-top:2px;';
            label.textContent = 'No file selected';

            input.addEventListener('change', function() {
              if (this.files.length === 0) {
                label.textContent = 'No file selected';
              } else if (this.files.length === 1) {
                label.textContent = this.files[0].name;
              } else {
                label.textContent = this.files.length + ' files selected';
              }
            });

            // Hide the entire Shiny input-group (the native Browse button + text)
            inputGroup.style.display = 'none';

            // Insert our button and label before the hidden input-group
            container.insertBefore(label, inputGroup);
            container.insertBefore(btn, label);
          });
        }

        styleFileInputs();

        // Re-run when Shiny conditionally renders new file inputs
        new MutationObserver(function() {
          styleFileInputs();
        }).observe(document.body, { childList: true, subtree: true });
      });
    "))
  ),
  
  tabsetPanel(id = "main_tabs",
              
              # TAB 1: DATA
              tabPanel("Data",
                       br(),
                       h4("Load data", class = "tab-title"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Data source"),
                                  radioButtons("data_source", NULL,
                                               choices  = c(
                                                 "Load example data (hypometrics package)" = "example",
                                                 "Upload a combined file (all data types in one CSV)" = "combined",
                                                 "Upload separate files (one per data type)" = "separate"
                                               ),
                                               selected = "example"),
                                  hr(),
                                  
                                  # Example data panel - shown when "Load example data" is selected
                                  conditionalPanel("input.data_source == 'example'",
                                                   interp_box(
                                                     tags$b("hypometrics package example datasets"),
                                                     tags$ul(style="margin-top:8px;margin-bottom:0;padding-left:18px;",
                                                             tags$li(tags$b("raw_cgm / cgm:"),
                                                                     " Simulated CGM data at 5-minute intervals."),
                                                             tags$li(tags$b("raw_sleep:"),
                                                                     " Simulated Fitbit sleep data including onset, offset and duration per night."),
                                                             tags$li(tags$b("raw_step:"),
                                                                     " Simulated Fitbit minute-level step count data."),
                                                             tags$li(tags$b("raw_hr:"),
                                                                     " Simulated Fitbit minute-level heart rate data."),
                                                             tags$li(tags$b("Person-reported hypoglycaemia - retrospective:"),
                                                                     " Simulated PRH data from daily morning check-in questionnaires."),
                                                             tags$li(tags$b("Person-reported hypoglycaemia - real time:"),
                                                                     " Simulated PRH data from the uMotif app symptom flower.")
                                                     )
                                                   )
                                  ),
                                  
                                  # Combined file upload panel - single file containing all data types
                                  conditionalPanel("input.data_source == 'combined'",
                                                   div(
                                                     style = paste0("background:#fff8f8; border:1px solid ", BRAND_RED_LIGHT,
                                                                    "; border-radius:4px; padding:10px 12px; margin-bottom:10px;"),
                                                     tags$b("How to use the combined file upload:"),
                                                     tags$ol(style="margin:6px 0 0 0; padding-left:16px; font-size:0.82rem; color:#333;",
                                                             tags$li("Select the ", tags$b("delimiter"), " your file(s) use - open the file
                           in a text editor to check. Commas between values = comma; semicolons
                           = semicolon; spaces or tabs = tab."),
                                                             tags$li("Select ", tags$b("one or more files"), " using the button below.
                           To select multiple files, hold ", tags$b("Ctrl"), " (Windows/Linux)
                           or ", tags$b("Cmd"), " (Mac) while clicking. Each file will be treated
                           as a separate participant if it has no ID column."),
                                                             tags$li("The app will ", tags$b("automatically detect"), " column names and
                           show dropdowns based on the first file uploaded. All files must have
                           the same column structure."),
                                                             tags$li("Select the ", tags$b("glucose unit"), ". Values above 30 = mg/dL;
                           values below 30 = mmol/L."),
                                                             tags$li("If files have no participant ID column, each filename is used
                           as the participant ID (e.g. participant1.csv → ID: participant1)."),
                                                             tags$li("Step count and heart rate columns are ", tags$b("auto-extracted"),
                                                                     " if present (steps, heart_rate, hr, bpm)."),
                                                             tags$li("Click ", tags$b("Load data"), " when ready.")
                                                     )
                                                   ),
                                                   div(class="section-label", "Step 1 - File delimiter"),
                                                   selectInput("upload_delim", NULL,
                                                               choices = c(
                                                                 "Comma  (,)  - standard CSV" = ",",
                                                                 "Semicolon  (;)  - common in European exports" = ";",
                                                                 "Tab  - TSV files" = "	",
                                                                 "Pipe  (|)" = "|"
                                                               ),
                                                               selected = ","),
                                                   div(class="section-label",
                                                       "Step 2 - Upload combined file(s)"),
                                                   p(style="font-size:0.78rem;color:#888;margin:-4px 0 4px 0;",
                                                     "Hold Ctrl / Cmd to select multiple files at once."),
                                                   p(style="font-size:0.78rem;color:#888;margin-bottom:4px;",
                                                     "💡 If your file has no participant ID column, select ",
                                                     strong("'(none)'"), " in the ID dropdown that appears after
                uploading - the filename will be used as the participant ID
                (e.g. participant1.csv → ID: participant1)."),
                                                   fileInput("file_cgm", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE, placeholder = "Select one or more files..."),
                                                   uiOutput("cgm_col_mapper_ui")
                                  ),
                                  
                                  # Separate files upload panel - one file per data type
                                  conditionalPanel("input.data_source == 'separate'",
                                                   div(
                                                     style = paste0("background:#fff8f8; border:1px solid ", BRAND_RED_LIGHT,
                                                                    "; border-radius:4px; padding:10px 12px; margin-bottom:10px;"),
                                                     tags$b("How to use separate file uploads:"),
                                                     tags$ol(style="margin:6px 0 0 0; padding-left:16px; font-size:0.82rem; color:#333;",
                                                             tags$li("Select the ", tags$b("delimiter"), " your files use.
                           All files must use the same delimiter."),
                                                             tags$li("For each data type, you can upload ", tags$b("multiple files"),
                                                                     " at once by holding ", tags$b("Ctrl"), " (Windows/Linux) or ",
                                                                     tags$b("Cmd"), " (Mac) while selecting. All files of the same
                           type will be combined - use this to compare participants
                           stored in separate files."),
                                                             tags$li("Only CGM is required - all others are optional."),
                                                             tags$li("After uploading CGM files, use the dropdowns to confirm the
                           timestamp, glucose, and ID columns. Select the correct ",
                                                                     tags$b("glucose unit"), " (mg/dL if values above 30)."),
                                                             tags$li("If a file has no participant ID column, its filename is
                           used as the ID."),
                                                             tags$li("Click ", tags$b("Load data"), " when all files are uploaded.")
                                                     )
                                                   ),
                                                   div(class="section-label", "File delimiter (applies to all files)"),
                                                   selectInput("upload_delim", NULL,
                                                               choices = c(
                                                                 "Comma  (,)  - standard CSV" = ",",
                                                                 "Semicolon  (;)  - common in European exports" = ";",
                                                                 "Tab  - TSV files" = "	",
                                                                 "Pipe  (|)" = "|"
                                                               ),
                                                               selected = ","),
                                                   hr(),
                                                   div(class="section-label", tags$b("CGM file(s)"), " - required"),
                                                   p(style="font-size:0.78rem;color:#888;margin:-4px 0 4px 0;",
                                                     "Hold Ctrl / Cmd to select multiple files."),
                                                   p(style="font-size:0.78rem;color:#888;margin-bottom:4px;",
                                                     "💡 If your file has no participant ID column, select ",
                                                     strong("'(none)'"), " in the ID dropdown - the filename will
                be used as the participant ID."),
                                                   fileInput("file_cgm", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE),
                                                   uiOutput("cgm_col_mapper_ui"),
                                                   hr(),
                                                   div(class="section-label", tags$b("Sleep file(s)"), " - optional"),
                                                   p(style="font-size:0.78rem;color:#888;margin:-4px 0 4px 0;",
                                                     "Hold Ctrl / Cmd to select multiple files."),
                                                   fileInput("file_sleep", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE),
                                                   uiOutput("sleep_col_mapper_ui"),
                                                   hr(),
                                                   div(class="section-label", tags$b("Step count file(s)"), " - optional"),
                                                   p(style="font-size:0.78rem;color:#888;margin:-4px 0 4px 0;",
                                                     "Hold Ctrl / Cmd to select multiple files."),
                                                   fileInput("file_step", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE),
                                                   uiOutput("step_col_mapper_ui"),
                                                   hr(),
                                                   div(class="section-label", tags$b("Heart rate file(s)"), " - optional"),
                                                   p(style="font-size:0.78rem;color:#888;margin:-4px 0 4px 0;",
                                                     "Hold Ctrl / Cmd to select multiple files."),
                                                   fileInput("file_hr", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE),
                                                   uiOutput("hr_col_mapper_ui"),
                                                   hr(),
                                                   div(class="section-label", tags$b("Daily check-in"), " - optional"),
                                                   fileInput("file_checkin", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE),
                                                   hr(),
                                                   div(class="section-label", tags$b("uMotif segment"), " - optional"),
                                                   fileInput("file_motif", NULL, accept = c(".csv",".txt"),
                                                             multiple = TRUE)
                                  ),
                                  
                                  br(),
                                  # Global glucose unit selector - applies to all tabs and plots.
                                  # Data is stored in its original unit; conversion only happens
                                  # when cgmSummarise() requires mg/dL internally.
                                  div(class="section-label", "Display glucose unit"),
                                  selectInput("global_glucose_unit", NULL,
                                              choices = c(
                                                "mmol/L (standard UK/EU)" = "mmol/L",
                                                "mg/dL (standard US)"     = "mg/dL"
                                              ),
                                              selected = "mmol/L"
                                  ),
                                  p(style="font-size:0.75rem;color:#888;margin-top:-4px;margin-bottom:8px;",
                                    "Select the unit your glucose data is in. The GUI will display all
               glucose values in this unit. Data is NOT automatically converted
               on upload - select to match your file."),
                                  br(),
                                  uiOutput("participant_filter_ui"),
                                  br(),
                                  actionButton("load_data", "Load data", class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Data preview"),
                                  uiOutput("data_status"),
                                  tabsetPanel(
                                    tabPanel("CGM (raw)",    br(), DTOutput("preview_cgm")),
                                    tabPanel("CGM (interp.)",br(), DTOutput("preview_cgm_interp")),
                                    tabPanel("Sleep",        br(), DTOutput("preview_sleep")),
                                    tabPanel("Steps",        br(), DTOutput("preview_step")),
                                    tabPanel("Heart rate",   br(), DTOutput("preview_hr")),
                                    tabPanel("PRH - real time",       br(), DTOutput("preview_motif")),
                                    tabPanel("PRH - retrospective",   br(), DTOutput("preview_checkin"))
                                  )
                                )
                         )
                       )
              ),
              
              # TAB 2: CGM
              tabPanel("CGM",
                       br(),
                       h4("Continuous Glucose Monitoring (CGM) analysis", class="tab-title"),
                       p("Data quality checking, gap interpolation, summary statistics, and
         detection and summarisation of sensor-detected hypoglycaemia (SDH).",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       
                       # Section 1: cgmCheck
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Data quality check"),
                                  section_header("CGM data coverage and missingness",
                                                 "Calculates daily CGM hours available, gap counts, gap durations,
               and percentage missingness. Broken down overall and by sleep status
               where sleep data is available."),
                                  br(),
                                  actionButton("run_cgm_check","Check CGM data quality",
                                               class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("CGM missingness summary (cgmCheck)"),
                                  p("Percentage of missing CGM data overall and by sleep status per participant.",
                                    style="font-size:0.82rem;color:#555;"),
                                  tableOutput("cgm_check_table"),
                                  download_btn_ui("dl_cgm_check")
                                ),
                                br(),
                                card(
                                  card_header("Distribution of missing values"),
                                  p("Time series with missing data regions highlighted. Each vertical bar
               represents a gap in the CGM trace.",
                                    style="font-size:0.82rem;color:#555;"),
                                  div(style="display:flex;align-items:center;gap:12px;margin-bottom:6px;",
                                      div(class="section-label",style="margin:0;white-space:nowrap;",
                                          "Participant to display:"),
                                      uiOutput("cgm_miss_id_ui")
                                  ),
                                  plotOutput("cgm_miss_plot", height="260px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgm_miss"))
                                )
                         )
                       ),
                       br(),
                       
                       # Section 2: Interpolation + cgmSummarise
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Gap interpolation & summary statistics"),
                                  section_header("Interpolate missing glucose values",
                                                 "Fills implicit gaps and applies linear interpolation to short gaps.
               Gaps longer than the maximum duration are left as NA."),
                                  checkboxInput("cgm_interpolate","Apply linear interpolation",value=TRUE),
                                  div(class="section-label","Min gap to fill (min)"),
                                  numericInput("cgm_mingap",NULL,value=2,  min=1,max=10,step=1),
                                  div(class="section-label","Max gap to fill (min)"),
                                  numericInput("cgm_maxgap",NULL,value=30, min=5,max=60,step=5),
                                  div(class="section-label","Output granularity (min)"),
                                  numericInput("cgm_gran",  NULL,value=1,  min=1,max=5, step=1),
                                  p("Min/Max gap and granularity converted to seconds internally (×60).",
                                    style="font-size:0.75rem;color:#888;"),
                                  hr(),
                                  section_header("Key CGM metrics (cgmSummarise)",
                                                 "Summarises interpolated CGM data. Glucose mean/SD/median displayed
               in mmol/L. Time-in-range columns use mg/dL thresholds internally."),
                                  hr(),
                                  actionButton("run_cgm","Run CGM analysis",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Glucose trace (post-interpolation)"),
                                  uiOutput("cgm_interp_note"),
                                  div(style="margin-top:8px;",
                                      div(class="section-label","Select participant to display"),
                                      selectInput("cgm_trace_id", NULL, choices=NULL, width="200px")
                                  ),
                                  fluidRow(
                                    column(10,
                                           plotlyOutput("cgm_trace_plot", height="420px"),
                                           tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgm_trace"))
                                    ),
                                    column(2,
                                           div(style="padding-top:80px;",
                                               uiOutput("cgm_threshold_legend")
                                           )
                                    )
                                  )
                                ),
                                br(),
                                card(
                                  card_header("CGM summary statistics (cgmSummarise)"),
                                  DTOutput("cgm_summary_table"),
                                  download_btn_ui("dl_cgm_summary")
                                )
                         )
                       ),
                       br(),
                       
                       # Section 3: SDH Detection
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Hypoglycaemia detection"),
                                  section_header("Detect sensor-detected hypoglycaemia (SDH) episodes",
                                                 "Identifies episodes where glucose stays at or below the threshold
               for a minimum duration. Returns one row per episode."),
                                  uiOutput("hypo_thresh_ui"),
                                  div(class="section-label","Min episode duration (min)"),
                                  numericInput("sdh_dur",    NULL,value=15, min=5,  max=60, step=5),
                                  div(class="section-label","Add sleep status to episodes?"),
                                  selectInput("sdh_sleep",NULL,
                                              choices=c("No"="no","Yes (requires sleep data)"="yes"),selected="no"),
                                  hr(),
                                  actionButton("run_sdh","Detect hypoglycaemia episodes",
                                               class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("SDH episodes (sdhDetection)"),
                                  uiOutput("sdh_flags"),
                                  br(),
                                  DTOutput("sdh_table")
                                ),
                                tags$div(style="margin-top:8px; margin-bottom:4px;",
                                         download_btn_ui("dl_sdh_episodes"))
                         )
                       ),
                       br(),
                       
                       # Section 4: SDH Summary
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Hypoglycaemia summary"),
                                  section_header("Summarise SDH episodes per participant (sdhSummarise)",
                                                 "Takes the episode-level sdhDetection output and produces a
               per-participant summary: total counts, day vs night breakdown,
               mean duration, and number of long episodes.
               Run 'Detect hypoglycaemia episodes' first."),
                                  div(class="section-label","Minimum duration for 'long' episode (min)"),
                                  numericInput("sdh_long",NULL,value=120,min=30,max=240,step=30),
                                  br(),
                                  actionButton("run_sdh_sum","Summarise SDH episodes",
                                               class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("SDH summary per participant (sdhSummarise)"),
                                  DTOutput("sdh_summary_table"),
                                  download_btn_ui("dl_sdh_summary")
                                )
                         )
                       )
              ),
              
              # TAB 3: PHYSICAL ACTIVITY
              tabPanel("Physical Activity",
                       br(),
                       h4("Physical activity analysis", class="tab-title"),
                       p("Visualises Fitbit step count and heart rate data using activityVisualise().
         Use the dropdown below to switch between step count and heart rate plots.",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("Activity visualisation options",
                                                 "Choose between step count and heart rate data. The time resolution
               controls whether data is shown for the full study period, by week,
               or by day. Use 'Page number' to navigate when viewing week or day
               breakdowns."),
                                  div(class="section-label","Time resolution"),
                                  selectInput("act_timebreak",NULL,
                                              choices=c("Full period"="no","By week"="week","By day"="day"),
                                              selected="no"),
                                  div(class="section-label","Page number (for week/day view)"),
                                  numericInput("act_page",NULL,value=1,min=1,max=20,step=1),
                                  div(class="section-label","Participant to display"),
                                  uiOutput("act_id_ui"),
                                  hr(),
                                  section_header("Summary table options",
                                                 "Choose whether the step and heart rate summary tables show
               daily or weekly averages per participant."),
                                  div(class="section-label","Summary period"),
                                  radioButtons("act_summary_period", NULL,
                                               choices  = c("Daily averages" = "daily",
                                                            "Weekly averages" = "weekly"),
                                               selected = "daily",
                                               inline   = TRUE),
                                  hr(),
                                  actionButton("run_activity","Plot activity",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Step count visualisation (activityVisualise)"),
                                  plotOutput("activity_step_vis_plot", height="260px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_act_step"))
                                ),
                                br(),
                                card(
                                  card_header("Heart rate visualisation (activityVisualise)"),
                                  plotOutput("activity_hr_vis_plot", height="260px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_act_hr"))
                                ),
                                br(),
                                card(
                                  card_header("Step count summary per participant"),
                                  uiOutput("activity_steps_caption"),
                                  tableOutput("activity_steps_table"),
                                  download_btn_ui("dl_steps_summary")
                                ),
                                br(),
                                card(
                                  card_header("Heart rate summary per participant"),
                                  uiOutput("activity_hr_caption"),
                                  tableOutput("activity_hr_table"),
                                  download_btn_ui("dl_hr_summary")
                                ),
                                br(),
                                card(
                                  card_header("Overall activity summary"),
                                  tableOutput("activity_overall_table"),
                                  download_btn_ui("dl_overall_summary")
                                )
                         )
                       )
              ),
              
              # TAB 4: SLEEP
              tabPanel("Sleep",
                       br(),
                       h4("Sleep analysis", class="tab-title"),
                       p("Covers sleep data categorisation, summary statistics, and visualisation
         of sleep onset and offset patterns using Fitbit sleep data.",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("Sleep analysis functions",
                                                 "sleepCategorise() organises raw Fitbit sleep records into main
               sleep periods. sleepSummarise() computes average sleep metrics
               per participant. sleepVisualise() plots the distribution of sleep
               onset and offset times."),
                                  hr(),
                                  section_header("sleepSummarise plot options",
                                                 "Adjust the dashed threshold line on the average sleep metrics chart."),
                                  div(class="section-label","Threshold line - min hours asleep (dashed)"),
                                  numericInput("sleep_threshold_h", NULL, value=6, min=3, max=10, step=0.5),
                                  hr(),
                                  hr(),
                                  actionButton("run_sleep","Run sleep analysis",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Sleep periods (sleepCategorise)"),
                                  p("Main sleep periods from raw Fitbit data. NAs = nights without
               detailed stage data.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("sleep_cat_table"),
                                  download_btn_ui("dl_sleep_cat")
                                ),
                                br(),
                                card(
                                  card_header("Sleep summary per participant (sleepSummarise)"),
                                  DTOutput("sleep_summary_table"),
                                  download_btn_ui("dl_sleep_summary")
                                ),
                                br(),
                                card(
                                  card_header("Average sleep metrics (sleepSummarise)"),
                                  legend_tip(),
                                  br(),
                                  plotlyOutput("sleep_plot", height="260px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_sleep_plot"))
                                ),
                                br(),
                                card(
                                  card_header("Sleep onset and offset distribution (sleepVisualise)"),
                                  div(
                                    style = "background:#fef9f9; border:1px solid #E8836D; border-radius:4px;
                       padding:10px 14px; margin-bottom:12px; font-size:0.85rem;",
                                    fluidRow(
                                      column(6,
                                             div(class="section-label", "Display"),
                                             checkboxInput("sleep_vis_all",
                                                           "Show all participants",
                                                           # Defaults to FALSE (unticked): VisualiseAll=TRUE is a confirmed bug
                                                           # in hypometrics::sleepVisualise itself (mismatched scale_x_time
                                                           # breaks/labels in that branch) - the single-participant view
                                                           # (VisualiseAll=FALSE) is the one that actually works.
                                                           value = FALSE)
                                      ),
                                      column(6,
                                             conditionalPanel("!input.sleep_vis_all",
                                                              div(class="section-label", "Participant"),
                                                              uiOutput("sleep_id_ui")
                                             )
                                      )
                                    ),
                                    p(style="font-size:0.78rem; color:#888; margin:4px 0 0 0;",
                                      "Plots the distribution of times when participant(s) went to
                 bed and woke up. Uncheck to view a single participant.")
                                  ),
                                  plotOutput("sleep_vis_plot", height="420px", width="100%"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_sleep_vis"))
                                )
                         )
                       )
              ),
              
              # TAB 5: PERSON-REPORTED HYPOGLYCAEMIA
              tabPanel("Person-Reported Hypoglycaemia",
                       br(),
                       h4("Person-reported hypoglycaemia (PRH) analysis", class="tab-title"),
                       p("Cleans, links, and summarises PRH data from real-time uMotif app reports
         and retrospective daily check-in questionnaires.",
                         style="color:#555;font-size:0.875rem;margin-bottom:8px;"),
                       div(
                         style=paste0("background:#fff8f8; border-left:4px solid ",BRAND_RED,
                                      "; padding:8px 14px; margin-bottom:16px; font-size:0.85rem;"),
                         "💡 ", tags$b("Note:"),
                         " The tables and summary plots are shown in the right panel below.
          The symptom visualisation (prhVisualise) is at the bottom of this page
          in its own section - scroll down to access it after running the analysis."
                       ),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("PRH analysis pipeline",
                                                 "Step 1: umotifClean() cleans raw uMotif files (motif and check-in).
               Step 2: prhLink() links real-time and retrospective reports.
               Step 3: prhSummarise() produces per-participant PRH counts.
               Step 4: prhVisualise() plots symptom combinations or prevalence
               by glucose range."),
                                  div(class="section-label","Add sleep status to PRH episodes?"),
                                  selectInput("prh_sleep",NULL,
                                              choices=c("No"="no","Yes (requires sleep data)"="yes"),selected="no"),
                                  div(class="section-label","Add sleep summary to PRH summary?"),
                                  selectInput("prh_sleep_sum",NULL,
                                              choices=c("No"="no","Yes"="yes"),selected="no"),
                                  div(
                                    style="background:#fff8f8;border:1px solid #E8836D;border-radius:4px;
                     padding:8px 10px;margin-top:4px;font-size:0.78rem;color:#555;",
                                    tags$b("Note: "), "The sleep status options above require the sleep
              data to contain a ", code("levels.data"), " column produced by the
              full Fitbit data pipeline. These options are not compatible with all
              sleep data formats. If you see an error, keep both set to 'No'."
                                  ),
                                  hr(),
                                  actionButton("run_prh","Run PRH analysis",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Cleaned real-time PRH data (umotifClean - motif)"),
                                  p("Real-time hypoglycaemia reports from the uMotif app symptom flower.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("prh_motif_table")
                                ),
                                tags$div(style="margin-top:8px;margin-bottom:12px;",
                                         download_btn_ui("dl_prh_motif")),
                                card(
                                  card_header("Cleaned retrospective PRH data (umotifClean - check-in)"),
                                  p("Hypoglycaemia episodes reported via daily morning check-in questionnaires.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("prh_checkin_table")
                                ),
                                tags$div(style="margin-top:8px;margin-bottom:12px;",
                                         download_btn_ui("dl_prh_checkin")),
                                card(
                                  card_header("Linked PRH episodes (prhLink)"),
                                  p("Real-time and retrospective reports linked by participant and timestamp.
               Episodes reported within 1 hour by both methods are matched on one row.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("prh_linked_table")
                                ),
                                tags$div(style="margin-top:8px;margin-bottom:12px;",
                                         download_btn_ui("dl_prh_linked")),
                                card(
                                  card_header("PRH summary per participant (prhSummarise)"),
                                  uiOutput("prh_flags"),
                                  br(),
                                  DTOutput("prh_summary_table")
                                ),
                                tags$div(style="margin-top:8px;margin-bottom:12px;",
                                         download_btn_ui("dl_prh_summary")),
                                br(),
                                card(
                                  card_header("PRH day vs night"),
                                  p(style="font-size:0.82rem;color:#555;",
                                    "Grouped bar chart showing the number of PRH episodes occurring
               during the day and night per participant."),
                                  plotlyOutput("prh_plot", height="240px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_prh_daynight"))
                                ),
                         )
                       ),
                       
                       # Separate fluidRow for prhVisualise - this ensures the options panel
                       # sits in a true left column (col-4) directly beside the plot (col-8),
                       # not nested inside the right column of the row above.
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Visualisation options"),
                                  tags$b(style=paste0("color:",BRAND_RED,";font-size:0.95rem;"),
                                         "Choose visualisation type"),
                                  br(),
                                  p(style="font-size:0.82rem;color:#555;margin:6px 0 4px 0;",
                                    tags$b("Symptom combinations (upset plot):"),
                                    " shows which combinations of symptoms were reported together.
               Requires the UpSetR package to be installed
               (install.packages('UpSetR'))."),
                                  p(style="font-size:0.82rem;color:#555;margin:4px 0 8px 0;",
                                    tags$b("Symptom frequency by glucose (heatmap):"),
                                    " shows how often each symptom occurred across different
               glucose concentration ranges."),
                                  radioButtons("prh_vis_type", NULL,
                                               choices = c(
                                                 "Symptom combinations (upset plot)" = "upset",
                                                 "Symptom frequency by glucose (heatmap)" = "heatmap"
                                               ),
                                               selected = "heatmap"
                                  )
                                )
                         ),
                         column(8,
                                card(
                                  card_header("PRH symptom visualisation (prhVisualise)"),
                                  p("Uses the raw motif segment data (umotifClean input) as input.",
                                    style="font-size:0.82rem;color:#888;"),
                                  uiOutput("prh_vis_height_ui")
                                )
                         )
                       )
              ),
              
              # TAB 6: CGM–SLEEP
              tabPanel("CGM–Sleep",
                       br(),
                       h4("CGM and sleep linkage", class="tab-title"),
                       p("Links CGM timestamps with sleep status using cgmsleepLink(), tagging each
         reading as Asleep, Awake, or NA. For a selected participant, cgmVisualise()
         then plots the glucose trace with sleep periods shown as shaded regions.",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("Link CGM with sleep data",
                                                 "cgmsleepLink() tags each CGM timestamp as Asleep, Awake, or NA.
               cgmVisualise() plots the glucose trace with sleep periods shaded."),
                                  div(
                                    style=paste0("background:#fff8f8; border:1px solid ", BRAND_RED_LIGHT,
                                                 "; border-radius:4px; padding:10px; margin-bottom:10px; font-size:0.82rem;"),
                                    tags$b("Select a participant below"),
                                    " to view their individual glucose trace with sleep periods overlaid.",
                                    br(), br(),
                                    "💡 In all plots, click a participant in the legend to hide/show
               their data. Double-click to isolate one participant."
                                  ),
                                  div(class="section-label","Participant for cgmVisualise"),
                                  uiOutput("link_id_ui"),
                                  div(class="section-label","Time resolution"),
                                  selectInput("link_timebreak", NULL,
                                              choices  = c("Full period"="no","By week"="week","By day"="day"),
                                              selected = "no"),
                                  conditionalPanel("input.link_timebreak != 'no'",
                                                   div(class="section-label","Page number"),
                                                   numericInput("link_pagenum", NULL, value=1, min=1, max=30, step=1)
                                  ),
                                  hr(),
                                  actionButton("run_link","Run CGM–Sleep linkage",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Linked CGM data with sleep status (cgmsleepLink)"),
                                  p("Each CGM timestamp is tagged as Asleep, Awake, or NA based on
               Fitbit sleep records.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("link_table"),
                                  download_btn_ui("dl_link_table")
                                ),
                                br(),
                                card(
                                  card_header("Glucose trace with sleep periods (cgmVisualise)"),
                                  p("Grey shaded regions = sleep periods recorded by Fitbit.
               Use the participant dropdown on the left to switch between participants.
               In week or day view, sleep periods are shown as shaded blocks
               across the time window - each block represents one sleep period.",
                                    style="font-size:0.82rem;color:#555;"),
                                  plotOutput("cgm_sleep_vis_plot", height="300px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgm_sleep"))
                                )
                                
                         )
                       )
              ),
              
              # TAB 7: CGM–ACTIVITY
              tabPanel("CGM–Activity",
                       br(),
                       h4("CGM and activity linkage", class="tab-title"),
                       p("Links CGM timestamps with step count and heart rate data using
         cgmactivityLink(). The CGM trace and selected activity metric are
         displayed on separate panels for direct comparison.",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("Link CGM with activity",
                                                 "cgmactivityLink() matches each CGM timestamp with the corresponding
               step count and heart rate from Fitbit minute-level data.
               All three plots (glucose, steps, heart rate) are shown together
               for direct comparison."),
                                  div(class="section-label","Participant to display"),
                                  uiOutput("cgmact_id_ui"),
                                  div(class="section-label","Time resolution"),
                                  selectInput("cgmact_timebreak",NULL,
                                              choices=c("Full period"="no","By week"="week","By day"="day"),
                                              selected="no"),
                                  div(class="section-label","Page number (for week/day view)"),
                                  numericInput("cgmact_pagenum",NULL,value=1,min=1,max=52,step=1),
                                  hr(),
                                  actionButton("run_cgmact","Run CGM–Activity linkage",
                                               class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Glucose trace"),
                                  plotlyOutput("cgmact_cgm_plot", height="200px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgmact_cgm"))
                                ),
                                br(),
                                card(
                                  card_header("Step count trace"),
                                  plotlyOutput("cgmact_step_plot", height="200px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgmact_step"))
                                ),
                                br(),
                                card(
                                  card_header("Heart rate trace"),
                                  plotlyOutput("cgmact_hr_plot", height="200px"),
                                  tags$div(style="margin-top:6px;", plot_download_btn("dl_png_cgmact_hr"))
                                ),
                                br(),
                                card(
                                  card_header("Download all three plots as one PNG"),
                                  p(style="font-size:0.82rem;color:#555;margin:0;",
                                    "Downloads glucose, steps and heart rate traces as a single combined PNG."),
                                  tags$div(style="margin-top:6px;",
                                           plot_download_btn("dl_png_cgmact_all", "Download all 3 plots (PNG)"))
                                ),
                                br(),
                                card(
                                  card_header("Linked CGM–activity data (cgmactivityLink)"),
                                  DTOutput("cgmact_table"),
                                  download_btn_ui("dl_cgmact_table")
                                )
                         )
                       )
              ),
              
              # TAB 8: CGM–PRH
              tabPanel("CGM–PRH",
                       br(),
                       h4("CGM and person-reported hypoglycaemia linkage", class="tab-title"),
                       p("Aligns CGM time series with PRH episodes using cgmprhLink().
         PRH episodes are marked on the glucose trace as black triangles with timestamps.",
                         style="color:#555;font-size:0.875rem;margin-bottom:20px;"),
                       fluidRow(
                         column(4,
                                card(
                                  card_header("Parameters"),
                                  section_header("Link CGM with PRH data",
                                                 "cgmprhLink() requires the CGM dataset and the linked PRH dataset
               from prhLink(). Run the Person-Reported Hypoglycaemia tab first,
               then return here."),
                                  interp_box(
                                    "⚠ Run the Person-Reported Hypoglycaemia tab first to generate
               the prhLink output before clicking below."
                                  ),
                                  div(class="section-label", "Participant to display"),
                                  uiOutput("cgmprh_id_ui"),
                                  div(class="section-label", "Time resolution"),
                                  selectInput("cgmprh_timebreak", NULL,
                                              choices  = c("Full period"="no","By week"="week","By day"="day"),
                                              selected = "no"),
                                  conditionalPanel("input.cgmprh_timebreak != 'no'",
                                                   div(class="section-label", "Page number"),
                                                   numericInput("cgmprh_pagenum", NULL, value=1, min=1, max=30, step=1)
                                  ),
                                  br(),
                                  actionButton("run_cgmprh","Run CGM–PRH linkage",class="btn-primary w-100")
                                )
                         ),
                         column(8,
                                card(
                                  card_header("Linked CGM–PRH data (cgmprhLink)"),
                                  p("CGM timestamps with nearest PRH episode aligned.",
                                    style="font-size:0.82rem;color:#555;"),
                                  DTOutput("cgmprh_table"),
                                  download_btn_ui("dl_cgmprh_table")
                                ),
                                br(), br(),
                                uiOutput("cgmprh_plot")
                         )
                       )
              ),
              
              # TAB 9: ABOUT
              tabPanel("About",
                       br(),
                       card(
                         card_header("About this app"),
                         tags$h5(style=paste0("color:",BRAND_RED,";font-weight:700;font-size:0.95rem;margin:4px 0 1px 0;"), "About hypometrics"),
                         p(style="font-size:0.88rem;margin:2px 0;",
                           "The ", code("hypometrics"), " package was developed by Gilberte Martine-Edith
           to support analysis of data from the Hypo-METRICS study. Hypo-METRICS was a
           multinational 10-week observational study (2020-2022) recruiting over 600 adults
           with type 1 or insulin-treated type 2 diabetes across 9 UK and EU sites, using
           CGM, Fitbit, and a smartphone app to record hypoglycaemia."),
                         tags$hr(style="margin:2px 0;"),
                         tags$h5(style=paste0("color:",BRAND_RED,";font-weight:700;font-size:0.95rem;margin:4px 0 1px 0;"), "About this GUI"),
                         p(style="font-size:0.88rem;margin:2px 0;",
                           "Developed by ", strong("Maya Jhyount"),
                           " under the supervision of ", strong("Gilberte Martine-Edith"), ".
           Tabs: Data, CGM, Physical Activity, Sleep, Person-Reported Hypoglycaemia,
           CGM-Sleep, CGM-Activity, CGM-PRH, About."),
                         tags$hr(style="margin:2px 0;"),
                         tags$h5(style=paste0("color:",BRAND_RED,";font-weight:700;font-size:0.95rem;margin:4px 0 1px 0;"), "Uploading your own data"),
                         p(style="font-size:0.88rem;margin:2px 0;",
                           "No column renaming required. The column mapping interface reads your file headers
           and auto-detects common names. Select your glucose unit on the Data tab.
           If no participant ID column exists, the filename is used as the ID.
           Multiple files can be selected at once with Ctrl/Cmd+click."),
                         tags$hr(style="margin:2px 0;"),
                         tags$h5(style=paste0("color:",BRAND_RED,";font-weight:700;font-size:0.95rem;margin:4px 0 1px 0;"), "How to cite"),
                         p(style="font-size:0.88rem;font-weight:700;margin:2px 0 1px 0;", "This GUI:"),
                         div(style="background:#f9f9f9;border-left:3px solid #ddd;padding:4px 10px;font-size:0.88rem;font-style:italic;margin:0 0 4px 0;",
                             "Jhyount, M. and Martine-Edith, G. (2026) ",
                             em("hypometrics Shiny GUI: A graphical interface for integrated CGM, activity,
             sleep and person-reported hypoglycaemia analysis"),
                             ". Version 1.0.0. Leicester: University of Leicester.",
                             " Available at: ",
                             tags$a("github.com/leicester-cdag/hypometrics",
                                    href="https://github.com/leicester-cdag/hypometrics",
                                    target="_blank", style="font-style:normal;")
                         ),
                         p(style="font-size:0.88rem;font-weight:700;margin:2px 0 1px 0;", "hypometrics R package:"),
                         div(style="background:#f9f9f9;border-left:3px solid #ddd;padding:4px 10px;font-size:0.88rem;font-style:italic;margin:0 0 4px 0;",
                             "Martine-Edith, G. (2024) ",
                             em("hypometrics: Tools for integrated analysis of CGM, sleep, activity
             and person-reported hypoglycaemia data from the Hypo-METRICS study"),
                             ". Leicester: University of Leicester.",
                             " Available at: ",
                             tags$a("https://github.com/leicester-cdag/hypometrics",
                                    href="https://github.com/leicester-cdag/hypometrics",
                                    target="_blank", style="font-style:normal;"),),
                         tags$hr(style="margin:2px 0;"),
                         tags$h5(style=paste0("color:",BRAND_RED,";font-weight:700;font-size:0.95rem;margin:4px 0 1px 0;"), "Links"),
                         p(style="font-size:0.88rem;margin:2px 0;",
                           tags$a("hypometrics documentation",
                                  href="https://leicester-cdag.github.io/hypometrics/index.html",
                                  target="_blank"),
                           " | ",
                           tags$a("GitHub source code",
                                  href="https://github.com/leicester-cdag/hypometrics",
                                  target="_blank")
                         ),
                         tags$hr(style="margin:2px 0;"),
                         p(style="font-size:0.8rem;color:#999;margin:2px 0;",
                           em("Version: 1.0.0 - ", Sys.Date()))
                       )
              )
  )
)

##### SERVER
#### All reactive logic, data processing and output rendering
#### Uses reactiveValues (rv) as the central data store so writing to any rv field
#### automatically re-triggers every output that depends on it

server <- function(input, output, session) {
  
  ### Reactive data store - all loaded datasets are held here
  ## Writing to any field (e.g. rv$cgm <- ...) automatically invalidates every output that reads from it
  rv <- reactiveValues(
    cgm             = NULL,  # raw CGM data (id, cgm_timestamp, glucose)
    sleep           = NULL,  # Fitbit sleep records
    step            = NULL,  # minute-level step counts
    hr              = NULL,  # minute-level heart rate
    checkin         = NULL,  # daily check-in PRH questionnaire data
    motif           = NULL,  # raw uMotif segment data (required by prhVisualise)
    loaded          = FALSE, # TRUE after Load data is clicked
    sdh_raw         = NULL,  # raw sdhDetection output - Interval objects intact for sdhSummarise
    motif_clean     = NULL,  # cleaned motif from umotifClean - used internally only
    prh_linked_data = NULL   # prhLink output - passed to CGM-PRH tab
  )
  
  ## Shared helpers used across the Data tab - both the actual load pipeline (triggered by
  ## the Load Data button) and the column-mapper preview UI (shown as soon as a file is
  ## selected, before Load Data is clicked) need to read files the same way, so these are
  ## defined once at server scope rather than duplicated in each place.
  
  ## read_csv_smart - some clinical exports (notably LibreView CGM downloads) put a single
  ## title line (e.g. just the patient ID) before the actual tab/comma-separated header row,
  ## AND may use a different delimiter than the one selected in the UI. Both problems produce
  ## the same symptom (file reads as one garbage column), so this handles them together:
  ## 1. Try the user-selected delimiter first.
  ## 2. If that produces only 1 field on the second line (the more reliable "real data" line -
  ##    a title line can legitimately have 1 field even with the right delimiter), the
  ##    delimiter itself is probably wrong - try comma/tab/semicolon/pipe and use whichever
  ##    splits line 2 into the most fields.
  ## 3. Once a working delimiter is settled on, check if line 1 has far fewer fields than
  ##    line 2 under THAT delimiter - if so, it's a title line, so skip it.
  read_csv_smart <- function(path, delim, fname = "") {
    tryCatch({
      first_lines <- readLines(path, n = 2, warn = FALSE)
      if (length(first_lines) < 2) {
        return(read.csv(path, sep = delim, stringsAsFactors = FALSE, check.names = FALSE))
      }
      count_fields <- function(line, d) length(strsplit(line, d, fixed = TRUE)[[1]])
      n2_selected <- count_fields(first_lines[2], delim)
      actual_delim <- delim
      if (n2_selected <= 1) {
        candidates <- c(",", "\t", ";", "|")
        field_counts <- vapply(candidates, function(d) count_fields(first_lines[2], d), integer(1))
        best <- candidates[which.max(field_counts)]
        if (field_counts[which.max(field_counts)] > 1) {
          actual_delim <- best
          delim_label <- switch(best, ","="comma", "\t"="tab", ";"="semicolon", "|"="pipe", best)
          showNotification(
            paste0("\"", fname, "\" didn't look right with the selected delimiter - ",
                   "detected and used ", delim_label, " instead."),
            type = "warning", duration = 8)
        }
      }
      n1 <- count_fields(first_lines[1], actual_delim)
      n2 <- count_fields(first_lines[2], actual_delim)
      skip_n <- if (n1 <= 1 && n2 > 1) 1 else 0
      read.csv(path, sep = actual_delim, stringsAsFactors = FALSE,
              check.names = FALSE, skip = skip_n)
    }, error = function(e) NULL)
  }

  ## extract_participant_id - when a file has no id column, many wearable-study datasets
  ## many wearable-study datasets name files as "<ParticipantID>_<datatype>_<date>.csv" - using the
  ## whole filename as the ID means the same person gets a DIFFERENT id in every data type
  ## (steps vs heart vs sleep files all have different suffixes), which silently breaks every
  ## cross-type link in the app (participant filters, activity plots, CGM-linked tabs).
  ## Taking just the leading underscore-delimited token keeps the same person's ID consistent
  ## across all their files, as long as that first token is the participant identifier -
  ## which is the common convention. Falls back to the full filename if there's no underscore.
  extract_participant_id <- function(fname) {
    base <- tools::file_path_sans_ext(basename(fname))
    first_token <- strsplit(base, "_")[[1]][1]
    if (!is.na(first_token) && nchar(first_token) > 0) first_token else base
  }

  ## find_col - matches a column name against a list of candidates case-insensitively.
  ## Real exports vary casing in ways a fixed list of exact names won't catch
  ## (e.g. "dateTime" vs "datetime", "Heart Rate" vs "heart_rate") - this avoids
  ## a valid column being missed just because of how it happens to be capitalised.
  ## Returns the actual column name found in df, or NA if none of the candidates match.
  find_col <- function(df, candidates) {
    nm <- names(df)
    hit <- match(tolower(candidates), tolower(nm))
    hit <- hit[!is.na(hit)]
    if (length(hit) > 0) nm[hit[1]] else NA_character_
  }

  ## smart_parse_datetime - tries several common export formats in turn instead of relying on
  ## as.POSIXct()'s single default format. Different wearables export timestamps differently
  ## (Fitbit uses ISO 8601 with "T", Garmin-style exports often use "DD/MM/YYYY HH:MM"), so a
  ## single fixed format silently produces NA for anything that isn't that one format.
  smart_parse_datetime <- function(x) {
    x_iso <- gsub("T", " ", x)
    out <- suppressWarnings(lubridate::parse_date_time(
      x_iso,
      orders = c("Ymd HMS", "Ymd HM", "dmy HMS", "dmy HM", "mdy HMS", "mdy HM"),
      quiet = TRUE
    ))
    as.POSIXct(out)
  }

  ## parse_time_with_filename_date - some per-day exports (e.g. minute-level step/HR files)
  ## record only time-of-day ("00:01:00"), with the actual date only present in the
  ## filename (e.g. "participant1_steps_2018-06-13.csv"). smart_parse_datetime alone can't
  ## recover a date that was never in the column - this extracts a YYYY-MM-DD pattern from
  ## the filename and combines it with the time column to build a full timestamp. Falls back
  ## to smart_parse_datetime alone (in case the column already has a full date) if the
  ## filename has no date pattern or the column doesn't look like pure time-of-day.
  parse_time_with_filename_date <- function(x, fname) {
    looks_like_time_only <- all(grepl("^\\d{1,2}:\\d{2}(:\\d{2})?$", trimws(as.character(x))))
    date_match <- regmatches(fname, regexpr("\\d{4}-\\d{2}-\\d{2}", fname))
    if (looks_like_time_only && length(date_match) > 0 && nchar(date_match) > 0) {
      smart_parse_datetime(paste(date_match, x))
    } else {
      smart_parse_datetime(as.character(x))
    }
  }

  ## sanitize_names - replaces blank/NA column names with placeholders
  ## Some exports (e.g. Garmin) leave a stray blank column from a trailing comma in the header.
  ## Downstream dplyr/tidyr functions (filter, pivot_longer, as_tibble) refuse to run on data
  ## frames with NA or "" column names, raising an opaque error far from the actual cause.
  ## Fixing the name here means that error surfaces as a validation message instead of a crash.
  sanitize_names <- function(df, fname) {
    nm <- names(df)
    blank <- is.na(nm) | nm == ""
    if (any(blank)) {
      nm[blank] <- paste0("unnamed_col_", seq_along(nm)[blank])
      names(df) <- nm
      showNotification(
        paste0("Note: \"", fname, "\" had ", sum(blank),
               " unnamed column(s) in its header - these were kept but not used."),
        type = "warning", duration = 8)
    }
    df
  }

  ## require_cols - checks that at least one column exists for each required field
  ## after process_fn has run its column-name mapping. Each element of `needed` is a
  ## character vector of acceptable names for that field (process_fn maps these to the
  ## exact hypometrics column name, so by this point the mapped name is what's checked).
  ## Returns NULL and shows an informative notification (expected vs found) if the file
  ## doesn't match the expected schema, rather than letting a malformed frame through.
  require_cols <- function(df, needed, type_label, fname) {
    missing_fields <- names(needed)[!vapply(needed, function(opts) any(opts %in% names(df)), logical(1))]
    if (length(missing_fields) > 0) {
      showNotification(
        div(
          strong(paste0(fname, " does not look like ", type_label, " data.")), br(),
          paste0("Missing: ", paste(missing_fields, collapse = ", ")), br(),
          paste0("Columns found: ", paste(names(df), collapse = ", ")), br(),
          "This file was not loaded. See the About tab for the expected format."
        ),
        type = "error", duration = 15)
      return(NULL)
    }
    df
  }
  
  ### Load data observer - fires when the user clicks "Load data"
  ## Branches on the selected upload mode: example datasets, combined file, or separate files
  ## After loading, updates the participant filter checkboxes and the CGM trace dropdown
  observeEvent(input$load_data, {
    if (input$data_source == "example") {
      # Loading the hypometrics example datasets directly - these are already correctly formatted
      rv$cgm     <- raw_cgm
      rv$sleep   <- raw_sleep
      rv$step    <- raw_step
      rv$hr      <- raw_hr
      rv$checkin <- raw_checkin
      rv$motif   <- raw_motif_segment
    } else if (input$data_source %in% c("combined", "separate")) {
      ## CGM is the anchor dataset most tabs link against, but sleep/steps/HR/PRH previews
      ## should still work without it (e.g. when testing one data type in isolation) - so this
      ## warns rather than blocking the load. Individual CGM-dependent outputs guard for
      ## rv$cgm being NULL themselves further down.
      cgm_provided <- !is.null(input$file_cgm)
      if (!cgm_provided) {
        showNotification(
          "No CGM file selected - CGM, and any CGM-linked tabs (CGM-Sleep, CGM-Activity, CGM-PRH), won't be available. Other uploaded data types will still load and preview normally.",
          type = "warning", duration = 10)
      }
      ## Helper function for reading a single uploaded CSV file with the chosen delimiter
      read_delim_safe <- function(fi, delim = ",") {
        if (is.null(fi)) return(NULL)
        tryCatch(
          read.csv(fi$datapath, sep = delim, stringsAsFactors = FALSE,
                   check.names = FALSE),
          error = function(e) {
            showNotification(paste("Error reading file:", e$message), type="error")
            NULL
          }
        )
      }
      
      delim <- input$upload_delim
      
      ## CGM upload and auto-extraction - supports multiple files selected at once
      if (!is.null(input$file_cgm)) {
        
        ## Processes a single uploaded CGM file - called via lapply across all uploaded files
        ## input$file_cgm is a dataframe with one row per file when multiple=TRUE is used
        process_one_cgm <- function(file_row) {
          raw <- read_csv_smart(file_row$datapath, delim, file_row$name)
          if (is.null(raw)) {
            showNotification(paste0("Error reading ", file_row$name), type = "error")
          }
          if (is.null(raw)) return(NULL)
          
          all_cols <- names(raw)
          df <- raw
          
          # Participant ID assignment - four-tier priority:
          # 1. Mapper dropdown explicitly set to a column: use that column
          # 2. Mapper set to __none__: always use the filename (e.g. participant1.csv -> participant1)
          # 3. Mapper not yet rendered (NULL): auto-detect common ID column names
          # 4. Nothing found: fall back to filename
          pid_from_filename <- extract_participant_id(file_row$name)
          
          if (!is.null(input$cgm_map_id) && input$cgm_map_id == "__none__") {
            # User explicitly selected "none" - always use filename
            df$id <- pid_from_filename
          } else if (!is.null(input$cgm_map_id) &&
                     nchar(input$cgm_map_id) > 0 &&
                     input$cgm_map_id %in% all_cols) {
            # User selected a specific column
            df$id <- as.character(df[[input$cgm_map_id]])
          } else {
            # Mapper not rendered yet - try auto-detect. Skip a candidate if it's clearly a
            # per-row serial number rather than a participant grouping id (e.g. LibreView's "ID"
            # column, which numbers every reading individually) - a real participant id should
            # repeat across many rows, not be near-unique per row.
            id_candidates <- intersect(
              c("id","ID","patient_id","subject_id","participant","paciente"), all_cols)
            auto_id_col <- NA
            for (cand in id_candidates) {
              if (length(unique(df[[cand]])) < 0.5 * nrow(df)) { auto_id_col <- cand; break }
            }
            if (!is.na(auto_id_col)) {
              df$id <- as.character(df[[auto_id_col]])
            } else {
              # No suitable ID column found - use filename as participant ID
              df$id <- pid_from_filename
            }
          }
          
          ## Timestamp column - use the mapper dropdown if set, otherwise auto-detect common names
          # gsub("T", " ", ...) handles ISO 8601 format (e.g. 2019-07-03T11:15:00) which POSIXct requires as a space
          ts_col <- if (!is.null(input$cgm_map_ts) &&
                        nchar(input$cgm_map_ts) > 0 &&
                        input$cgm_map_ts %in% all_cols) {
            input$cgm_map_ts
          } else {
            # "Hora" (Spanish for "time") covers LibreView exports; "bg_ts" covers this
            # dataset's simple two-column blood-glucose export (bg_ts, value)
            find_col(df, c("cgm_timestamp","time","timestamp","datetime","date_time","Hora","bg_ts"))
          }
          if (!is.na(ts_col) && !is.null(ts_col)) {
            df$cgm_timestamp <- smart_parse_datetime(as.character(df[[ts_col]]))
          }
          
          ## Glucose column - use the mapper dropdown if set, otherwise auto-detect common names
          # If the mapper was not interacted with, detect units from the data range (>30 = mg/dL)
          gluc_col <- if (!is.null(input$cgm_map_glucose) &&
                          nchar(input$cgm_map_glucose) > 0 &&
                          input$cgm_map_glucose %in% all_cols) {
            input$cgm_map_glucose
          } else {
            # "Historico glucosa" = LibreView's continuous automatic sensor readings (what CGM
            # analysis needs); "Glucosa leida" = manual scans only, listed as a lower-priority
            # fallback since it's sparser and not a true continuous trace. "value" covers simple
            # two-column exports (bg_ts, value) - safe here since this is the CGM upload slot
            # specifically, same reasoning as the step-count slot's "value" recognition.
            find_col(df, c("glucose","gl","sgv","BG","bg","value",
                          "Hist\u00f3rico glucosa (mg/dL)","Glucosa le\u00edda (mg/dL)"))
          }
          gluc_unit <- if (!is.null(input$cgm_map_unit)) input$cgm_map_unit else "mmol/L"
          if (!is.na(gluc_col) && !is.null(gluc_col)) {
            df$glucose <- as.numeric(df[[gluc_col]])
            if (is.null(input$cgm_map_unit) &&
                max(df$glucose, na.rm = TRUE) > 30) gluc_unit <- "mg/dL"
            # No conversion at upload - data stored in original units.
            # User selects display unit via global_glucose_unit on Data tab.
          }
          
          list(raw = raw, df = df, all_cols = all_cols, gluc_unit = gluc_unit)
        }
        
        # Process every uploaded file using the function above and filter out any that failed
        all_results <- lapply(seq_len(nrow(input$file_cgm)), function(i) {
          process_one_cgm(input$file_cgm[i, ])
        })
        all_results <- Filter(Negate(is.null), all_results)
        
        if (length(all_results) > 0) {
          # Row-bind CGM data from all files
          # If multiple files produce the same ID, append filename to make unique
          file_names <- sapply(seq_len(nrow(input$file_cgm)), function(i)
            tools::file_path_sans_ext(basename(input$file_cgm$name[i])))
          
          cgm_list <- lapply(seq_along(all_results), function(idx) {
            r <- all_results[[idx]]
            df <- r$df
            if (!all(c("id","cgm_timestamp","glucose") %in% names(df))) return(NULL)
            df <- df[, c("id","cgm_timestamp","glucose"), drop = FALSE]
            df
          })
          cgm_list <- Filter(Negate(is.null), cgm_list)
          
          if (length(cgm_list) > 0) {
            # Check whether multiple files produced the same participant ID
            id_per_file <- sapply(cgm_list, function(df) unique(df$id)[1])
            if (anyDuplicated(id_per_file) && length(cgm_list) > 1) {
              # Append the source filename to make each ID unique across files
              cgm_list <- lapply(seq_along(cgm_list), function(idx) {
                df <- cgm_list[[idx]]
                df$id <- paste0(df$id, "_", file_names[idx])
                df
              })
              showNotification(
                paste0("Multiple files had the same participant ID. ",
                       "Filenames appended to make IDs unique."),
                type = "warning", duration = 7)
            }
            rv$cgm <- do.call(rbind, cgm_list)
            showNotification(
              paste0("✓ CGM loaded - ", nrow(rv$cgm), " rows across ",
                     length(unique(rv$cgm$id)), " participant(s): ",
                     paste(unique(rv$cgm$id), collapse=", ")),
              type = "message", duration = 6)
          }
          
          # Notify glucose unit if conversion happened
          units_used <- unique(sapply(all_results, function(r) r$gluc_unit))
          if ("mg/dL" %in% units_used) {
            showNotification(
              "Glucose converted from mg/dL to mmol/L for all files.",
              type = "message", duration = 5)
          }
          
          ## Auto-extracting step count data from the combined file if a recognised column name is found
          # IDs are taken from rv$cgm after deduplication to keep all data types consistent
          step_dfs <- lapply(seq_along(all_results), function(idx) {
            r <- all_results[[idx]]
            step_candidates <- c("steps","step_count","Steps","stepcount","step","STEPS")
            step_col <- intersect(step_candidates, r$all_cols)[1]
            df <- r$df
            if (!is.na(step_col) && "cgm_timestamp" %in% names(df)) {
              # Use the same IDs that ended up in rv$cgm after dedup
              file_ids <- unique(rv$cgm$id)
              use_id   <- if (length(file_ids) >= idx) file_ids[idx] else df$id[1]
              s <- data.frame(
                id             = use_id,
                step_timestamp = df$cgm_timestamp,
                count          = as.numeric(r$raw[[step_col]]),
                stringsAsFactors = FALSE
              )
              s[!is.na(s$count), ]
            } else NULL
          })
          step_dfs <- Filter(Negate(is.null), step_dfs)
          if (length(step_dfs) > 0) {
            rv$step <- do.call(rbind, step_dfs)
            showNotification(
              paste0("✓ Steps extracted - ", nrow(rv$step), " rows across ",
                     length(unique(rv$step$id)), " participant(s)."),
              type = "message", duration = 5)
          }
          
          ## Auto-extracting heart rate data from the combined file if a recognised column name is found
          hr_dfs <- lapply(seq_along(all_results), function(idx) {
            r <- all_results[[idx]]
            hr_candidates <- c("heart_rate","hr","HeartRate","heartrate",
                               "bpm","pulse","HR","heart rate")
            hr_col <- intersect(hr_candidates, r$all_cols)[1]
            df <- r$df
            if (!is.na(hr_col) && "cgm_timestamp" %in% names(df)) {
              file_ids <- unique(rv$cgm$id)
              use_id   <- if (length(file_ids) >= idx) file_ids[idx] else df$id[1]
              h <- data.frame(
                id           = use_id,
                hr_timestamp = df$cgm_timestamp,
                heart_rate   = as.numeric(r$raw[[hr_col]]),
                stringsAsFactors = FALSE
              )
              h[!is.na(h$heart_rate), ]
            } else NULL
          })
          hr_dfs <- Filter(Negate(is.null), hr_dfs)
          if (length(hr_dfs) > 0) {
            rv$hr <- do.call(rbind, hr_dfs)
            showNotification(
              paste0("✓ Heart rate extracted - ", nrow(rv$hr), " rows across ",
                     length(unique(rv$hr$id)), " participant(s)."),
              type = "message", duration = 5)
          }
          
          all_cols_first <- all_results[[1]]$all_cols
          showNotification(
            paste0("Columns in file(s): ", paste(all_cols_first, collapse=", ")),
            type = "message", duration = 8)
        }
      }
      
      ## read_multi - reads and row-binds multiple uploaded files of the same data type.
      ## Used for all separate-mode uploads: sleep, steps, HR, check-in and motif
      ## process_fn maps common column name variants to the expected hypometrics column names
      ## and returns NULL (via require_cols) if the file doesn't match the expected schema
      read_multi <- function(fi, delim, process_fn) {
        if (is.null(fi)) return(NULL)
        dfs <- lapply(seq_len(nrow(fi)), function(i) {
          raw <- read_csv_smart(fi$datapath[i], delim, fi$name[i])
          if (is.null(raw)) {
            showNotification(paste0("Could not read \"", fi$name[i], "\""),
                             type = "error", duration = 10)
            return(NULL)
          }
          raw <- sanitize_names(raw, fi$name[i])
          process_fn(raw, fi$name[i])
        })
        dfs <- Filter(Negate(is.null), dfs)
        if (length(dfs) > 0) do.call(rbind, dfs) else NULL
      }
      
      if (input$data_source == "separate" && !is.null(input$file_sleep)) {
        rv$sleep <- read_multi(input$file_sleep, delim, function(df, fname) {
          if (!"id" %in% names(df)) {
            # When there's exactly one sleep file and exactly one CGM file, assume they're the
            # same participant and reuse CGM's derived ID - inconsistent filename conventions
            # across data types (e.g. "glucose_p1.csv" vs "p1_sleepdata.csv") can otherwise
            # derive completely different tokens for what is actually the same person.
            single_case <- !is.null(input$file_cgm) && nrow(input$file_cgm) == 1 &&
                          nrow(input$file_sleep) == 1
            df$id <- if (single_case) extract_participant_id(input$file_cgm$name[1])
                     else extract_participant_id(fname)
          }

          ## Detect a one-row-per-night sleep summary export (e.g. Garmin-style): each row
          ## already represents a whole night, with a start timestamp (start_date_ts), total
          ## duration (duration_in_sec) and stage breakdowns in seconds. Much simpler than the
          ## per-segment format below since no reconstruction from individual segments is
          ## needed - the fields just need relabelling/deriving into the schema
          ## sleepCategorise() expects.
          gs_start_col  <- find_col(df, c("start_date_ts"))
          gs_dur_col    <- find_col(df, c("duration_in_sec"))
          if (!is.na(gs_start_col) && !is.na(gs_dur_col)) {
            gs_deep_col   <- find_col(df, c("deep_sleep_s"))
            gs_light_col  <- find_col(df, c("light_sleep_s"))
            gs_rem_col    <- find_col(df, c("rem_sleep_s"))
            gs_awake_col  <- find_col(df, c("awake_s"))
            gs_unmeas_col <- find_col(df, c("unmeasurable_sleep_s"))

            night_start <- smart_parse_datetime(df[[gs_start_col]])
            dur_s       <- as.numeric(df[[gs_dur_col]])
            night_end   <- night_start + dur_s

            zeros <- rep(0, nrow(df))
            asleep_s <- rowSums(cbind(
              if (!is.na(gs_deep_col))  as.numeric(df[[gs_deep_col]])  else zeros,
              if (!is.na(gs_light_col)) as.numeric(df[[gs_light_col]]) else zeros,
              if (!is.na(gs_rem_col))   as.numeric(df[[gs_rem_col]])   else zeros
            ), na.rm = TRUE)
            awake_s <- rowSums(cbind(
              if (!is.na(gs_awake_col))  as.numeric(df[[gs_awake_col]])  else zeros,
              if (!is.na(gs_unmeas_col)) as.numeric(df[[gs_unmeas_col]]) else zeros
            ), na.rm = TRUE)

            out <- data.frame(
              id            = df$id,
              logId         = as.numeric(night_start),
              dateOfSleep   = format(as.Date(night_start), "%Y-%m-%d"),
              startTime     = format(night_start, "%Y-%m-%d %H:%M:%S"),
              endTime       = format(night_end,   "%Y-%m-%d %H:%M:%S"),
              duration      = dur_s * 1000,
              timeInBed     = dur_s / 60,
              minutesAsleep = asleep_s / 60,
              minutesAwake  = awake_s / 60,
              stringsAsFactors = FALSE
            )
            showNotification(
              paste0("\"", fname, "\" recognised as a one-row-per-night sleep summary export (",
                     nrow(out), " night(s)) - converted directly."),
              type = "message", duration = 7)
            return(out)
          }

          ## Detect the raw per-segment sleep export format: each row is one
          ## contiguous state segment within a night (dateTime = segment start, Duration = length
          ## in seconds, State = asleep/awake/restless), rather than one row per whole sleep event.
          ## When detected, collapse all segments in the file into a single summarised sleep
          ## period spanning the earliest segment start to the latest segment end, with
          ## minutesAsleep/minutesAwake derived from the segment states - this is what
          ## sleepCategorise() actually expects (one row per night), just built from raw segments
          ## instead of already being in that shape.
          seg_ts_col    <- find_col(df, c("dateTime"))
          seg_dur_col   <- find_col(df, c("Duration","duration_s","duration"))
          seg_state_col <- find_col(df, c("State","state"))
          already_event_format <- any(!is.na(find_col(df, c("startTime","start_time","onset"))))

          if (!is.na(seg_ts_col) && !is.na(seg_dur_col) && !is.na(seg_state_col) &&
              !already_event_format) {
            seg_start <- smart_parse_datetime(df[[seg_ts_col]])
            seg_dur_s <- as.numeric(df[[seg_dur_col]])
            seg_end   <- seg_start + seg_dur_s
            seg_state <- tolower(trimws(as.character(df[[seg_state_col]])))

            valid <- !is.na(seg_start) & !is.na(seg_dur_s)
            if (any(valid)) {
              asleep_s <- sum(seg_dur_s[valid & seg_state == "asleep"], na.rm = TRUE)
              awake_s  <- sum(seg_dur_s[valid & seg_state %in% c("awake","restless")], na.rm = TRUE)
              night_start <- min(seg_start[valid], na.rm = TRUE)
              night_end   <- max(seg_end[valid],   na.rm = TRUE)

              df <- data.frame(
                id            = unique(df$id)[1],
                # logId must be NUMERIC (confirmed against hypometrics::raw_sleep) - Fitbit's own
                # logId is a large numeric id, so build one from the start time as epoch seconds,
                # which is guaranteed unique per night for a given participant.
                logId         = as.numeric(night_start),
                # dateOfSleep must be a CHARACTER string like "2026-01-01", not a Date object -
                # confirmed against hypometrics::raw_sleep, which stores it as chr not Date.
                dateOfSleep   = format(as.Date(night_start), "%Y-%m-%d"),
                # startTime/endTime must be character strings, not POSIXct - sleepCategorise
                # re-parses them internally via lubridate::ymd_hms(), which (confirmed via
                # console testing) returns NA when given an already-built POSIXct object in
                # this R/lubridate version, but works correctly on a plain formatted string.
                startTime     = format(night_start, "%Y-%m-%d %H:%M:%S"),
                endTime       = format(night_end,   "%Y-%m-%d %H:%M:%S"),
                # duration must be in MILLISECONDS, not seconds - confirmed against
                # hypometrics::raw_sleep (e.g. 531 min timeInBed = 31,860,000 ms duration).
                duration      = as.numeric(difftime(night_end, night_start, units = "secs")) * 1000,
                timeInBed     = as.numeric(difftime(night_end, night_start, units = "mins")),
                minutesAsleep = asleep_s / 60,
                minutesAwake  = awake_s  / 60,
                stringsAsFactors = FALSE
              )
              showNotification(
                paste0("\"", fname, "\" was a raw per-segment sleep export - converted into ",
                       "one summarised sleep period (", round(asleep_s/3600, 1), "h asleep)."),
                type = "message", duration = 7)
              return(df)
            }
          }

          st_col <- find_col(df, c("startTime","start_time","start_date_ts","onset","sleep_onset"))
          # Character strings, not POSIXct - see note above on sleepCategorise's internal parsing
          if (!is.na(st_col)) df$startTime <- format(smart_parse_datetime(df[[st_col]]), "%Y-%m-%d %H:%M:%S")
          en_col <- find_col(df, c("endTime","end_time","offset","sleep_offset"))
          if (!is.na(en_col)) df$endTime <- format(smart_parse_datetime(df[[en_col]]), "%Y-%m-%d %H:%M:%S")
          dt_col <- find_col(df, c("dateOfSleep","date"))
          # dateOfSleep must be a CHARACTER string like "2026-01-01", not a Date object -
          # confirmed against hypometrics::raw_sleep, which stores it as chr not Date.
          if (!is.na(dt_col)) df$dateOfSleep <- format(as.Date(df[[dt_col]]), "%Y-%m-%d")
          # logId is required by sleepCategorise/sleepVisualise but plenty of non-Fitbit exports
          # won't have it - synthesise one if it's genuinely missing. Must be NUMERIC (confirmed
          # against hypometrics::raw_sleep), so use start time as epoch seconds.
          if (!"logId" %in% names(df) && "startTime" %in% names(df)) {
            df$logId <- as.numeric(as.POSIXct(df$startTime, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
          }
          # hypometrics::sleepCategorise expects Fitbit-style rows: a start time, end time and
          # date for each sleep event. If none of the recognised name variants were found above,
          # and this wasn't a raw per-segment file either, this genuinely isn't a format the
          # app can interpret - reject it here with a clear message rather than passing it
          # through to crash later inside sleepCategorise() or a table render.
          require_cols(df,
                       needed = list(
                         "start time"    = c("startTime"),
                         "end time"      = c("endTime"),
                         "date of sleep" = c("dateOfSleep")
                       ),
                       type_label = "Fitbit-format sleep",
                       fname = fname)
        })
        if (!is.null(rv$sleep))
          showNotification(paste0("✓ Sleep loaded - ", nrow(rv$sleep),
                                  " nights, ", length(unique(rv$sleep$id)), " participant(s)."),
                           type="message", duration=5)
      }
      
      ## Step count upload - separate mode only
      if (input$data_source == "separate" && !is.null(input$file_step)) {
        rv$step <- read_multi(input$file_step, delim, function(df, fname) {
          if (!"id" %in% names(df)) {
            single_case <- !is.null(input$file_cgm) && nrow(input$file_cgm) == 1 &&
                          nrow(input$file_step) == 1
            df$id <- if (single_case) extract_participant_id(input$file_cgm$name[1])
                     else extract_participant_id(fname)
          }
          ts_col <- find_col(df, c("step_timestamp","time","timestamp","datetime","activity_ts"))
          if (!is.na(ts_col) && !"step_timestamp" %in% names(df))
            df$step_timestamp <- parse_time_with_filename_date(df[[ts_col]], fname)
          # "value" is included here because this is the step-upload slot specifically -
          # some raw per-minute Fitbit exports use a generic dateTime/value
          # pair per data stream, so "value" only means step count in this context.
          count_col <- find_col(df, c("steps","count","step_count","value"))
          if (!is.na(count_col) && !"count" %in% names(df))
            df$count <- as.numeric(df[[count_col]])
          require_cols(df,
                       needed = list(
                         "timestamp"  = c("step_timestamp"),
                         "step count" = c("count")
                       ),
                       type_label = "step count",
                       fname = fname)
        })
        if (!is.null(rv$step))
          showNotification(paste0("✓ Steps loaded - ", nrow(rv$step),
                                  " rows, ", length(unique(rv$step$id)), " participant(s)."),
                           type="message", duration=5)
      }
      
      ## Heart rate upload - separate mode only
      if (input$data_source == "separate" && !is.null(input$file_hr)) {
        rv$hr <- read_multi(input$file_hr, delim, function(df, fname) {
          if (!"id" %in% names(df)) {
            single_case <- !is.null(input$file_cgm) && nrow(input$file_cgm) == 1 &&
                          nrow(input$file_hr) == 1
            df$id <- if (single_case) extract_participant_id(input$file_cgm$name[1])
                     else extract_participant_id(fname)
          }
          ts_col <- find_col(df, c("hr_timestamp","time","timestamp","datetime","activity_ts"))
          if (!is.na(ts_col) && !"hr_timestamp" %in% names(df))
            df$hr_timestamp <- parse_time_with_filename_date(df[[ts_col]], fname)
          hr_col <- find_col(df, c("heart_rate","hr","heartrate","heart rate","bpm","value"))
          if (!is.na(hr_col) && !"heart_rate" %in% names(df))
            df$heart_rate <- as.numeric(df[[hr_col]])
          require_cols(df,
                       needed = list(
                         "timestamp"   = c("hr_timestamp"),
                         "heart rate"  = c("heart_rate")
                       ),
                       type_label = "heart rate",
                       fname = fname)
        })
        if (!is.null(rv$hr))
          showNotification(paste0("✓ HR loaded - ", nrow(rv$hr),
                                  " rows, ", length(unique(rv$hr$id)), " participant(s)."),
                           type="message", duration=5)
      }
      
      ## Check-in and uMotif segment uploads - separate mode only, passed through as-is
      if (input$data_source == "separate") {
        if (!is.null(input$file_checkin))
          rv$checkin <- read_multi(input$file_checkin, delim,
                                   function(df, fname) df)
        if (!is.null(input$file_motif))
          rv$motif   <- read_multi(input$file_motif,   delim,
                                   function(df, fname) df)
      }
    }
    # Mark data as loaded - this triggers the participant filter and preview tables to render
    rv$loaded <- TRUE
    
    # Guard: if rv$cgm is NULL after loading (e.g. wrong delimiter selected), show a clear error
    if (!is.null(rv$cgm) && nrow(rv$cgm) > 0) {
      ids <- unique(rv$cgm$id)
      updateCheckboxGroupInput(session, "participant_filter",
                               choices=ids, selected=ids)
      updateSelectInput(session, "cgm_trace_id", choices=ids, selected=ids[1])
    } else if (input$data_source != "example" && !is.null(input$file_cgm)) {
      # Only fire this if a CGM file was actually uploaded but failed to produce usable data -
      # if the user simply didn't provide one, the earlier "No CGM file selected" warning
      # already covers it and this would just be a confusing, redundant second message.
      showNotification(
        paste0("CGM data could not be loaded. Please check: ",
               "(1) the correct delimiter is selected, ",
               "(2) the timestamp and glucose columns are correctly mapped."),
        type = "error", duration = 12)
    }
  })
  
  ### Column mapper - renders dropdown menus after a file is uploaded
  ## Lets users confirm which column maps to each required field (timestamp, glucose, participant ID)
  ## Only fires once a file is uploaded and uses the first file's headers
  ## All files are assumed to share the same column structure
  
  # Helper: read just the header row of an uploaded file - uses the same smart reader as the
  # actual load pipeline so the preview mapper handles stray title lines and wrong delimiters
  # the same way the real upload does, rather than failing on files the load step can handle.
  read_header <- function(fi, delim = ",") {
    if (is.null(fi)) return(NULL)
    df <- read_csv_smart(fi$datapath, delim, fi$name)
    if (is.null(df)) return(NULL)
    names(df)
  }
  
  ## adaptive_date_breaks - fixed "1 day" tick spacing looks fine over a week or two but
  ## becomes an illegible smear of overlapping labels over a full study period spanning
  ## months. Scales the break spacing to how many days are actually being shown.
  adaptive_date_breaks <- function(xmin, xmax) {
    span_days <- as.numeric(difftime(xmax, xmin, units = "days"))
    if (span_days <= 14)      "1 day"
    else if (span_days <= 60) "1 week"
    else if (span_days <= 180) "2 weeks"
    else                       "1 month"
  }
  
  find_col_name <- function(cols, candidates) {
    hit <- match(tolower(candidates), tolower(cols))
    hit <- hit[!is.na(hit)]
    if (length(hit) > 0) cols[hit[1]] else NA_character_
  }
  
  # CGM column mapper
  output$cgm_col_mapper_ui <- renderUI({
    req(input$file_cgm)
    # When multiple files selected, show mapper based on first file only
    # All files must share the same column structure
    first_file <- if (is.data.frame(input$file_cgm)) input$file_cgm[1,] else input$file_cgm
    cols <- read_header(first_file,
                        if (!is.null(input$upload_delim)) input$upload_delim else ",")
    if (is.null(cols)) return(p("Could not read file headers.", style="color:red;"))
    cols_with_none <- c("(none - use filename as ID)" = "__none__", cols)
    div(style = "background:#f9f9f9; border:1px solid #ddd; border-radius:4px;
                 padding:10px; margin-bottom:6px;",
        p(style="font-size:0.78rem;font-weight:600;color:#555;margin:0 0 8px 0;",
          paste0("Columns detected: ", paste(cols, collapse=", "))),
        fluidRow(
          column(6,
                 div(class="section-label","Participant ID column"),
                 selectInput("cgm_map_id", NULL, choices = cols_with_none,
                             # Deliberately does NOT default to a plain "id"/"ID" column even if
                             # present - that's frequently a per-row serial number in clinical
                             # exports (e.g. LibreView), not a participant identifier. Only
                             # defaults to more specific, unambiguous names; otherwise falls back
                             # to filename-derived ID, which is safer than guessing wrong.
                             selected = {
                               id_default <- find_col_name(cols, c("patient_id","subject_id","participant","paciente"))
                               if (!is.na(id_default)) id_default else "__none__"
                             })
          ),
          column(6,
                 div(class="section-label","Timestamp column"),
                 selectInput("cgm_map_ts", NULL, choices = cols,
                             # "Hora" (Spanish "time") covers LibreView exports - matches the
                             # same candidate list used by the actual upload auto-detect, so the
                             # mapper's default selection and the real processing agree.
                             selected = {
                               ts_default <- find_col_name(cols, c("cgm_timestamp","time","timestamp","datetime","Hora","bg_ts"))
                               if (!is.na(ts_default)) ts_default else cols[1]
                             })
          )
        ),
        fluidRow(
          column(6,
                 div(class="section-label","Glucose column"),
                 selectInput("cgm_map_glucose", NULL, choices = cols,
                             # Same widened candidate list as the actual upload auto-detect,
                             # including LibreView's Spanish glucose column names.
                             selected = {
                               gl_default <- find_col_name(cols, c("glucose","gl","sgv","BG","bg","value",
                                                        "Hist\u00f3rico glucosa (mg/dL)","Glucosa le\u00edda (mg/dL)"))
                               if (!is.na(gl_default)) gl_default else cols[1]
                             })
          ),
          column(6,
                 div(class="section-label","Glucose unit"),
                 selectInput("cgm_map_unit", NULL,
                             choices = c("mmol/L", "mg/dL"),
                             selected = if (any(grepl("glucose",cols,ignore.case=TRUE))) {
                               # Auto-detect: if max of glucose col > 30, likely mg/dL
                               tryCatch({
                                 df_peek <- read.csv(input$file_cgm$datapath,
                                                     sep = if (!is.null(input$upload_delim)) input$upload_delim else ",",
                                                     nrows = 5, stringsAsFactors = FALSE, check.names = FALSE)
                                 gcol <- intersect(c("glucose","Glucose","gl","sgv"), names(df_peek))[1]
                                 if (!is.na(gcol) && max(as.numeric(df_peek[[gcol]]), na.rm=TRUE) > 30)
                                   "mg/dL" else "mmol/L"
                               }, error = function(e) "mmol/L")
                             } else "mmol/L")
          )
        )
    )
  })
  
  # Sleep column mapper - simpler, just shows columns for reference
  output$sleep_col_mapper_ui <- renderUI({
    req(input$file_sleep)
    cols <- read_header(input$file_sleep,
                        if (!is.null(input$upload_delim)) input$upload_delim else ",")
    if (is.null(cols)) return(NULL)
    div(style = "background:#f9f9f9; border:1px solid #ddd; border-radius:4px;
                 padding:8px 10px; margin-bottom:6px; font-size:0.78rem; color:#555;",
        p(style="margin:0;", tags$b("Columns detected: "),
          paste(cols, collapse=", ")),
        p(style="margin:4px 0 0 0; color:#888;",
          "App will auto-detect: id, dateOfSleep/date, startTime/onset,
         endTime/offset, timeInBed, minutesAsleep, minutesAwake.")
    )
  })
  
  # Step column mapper
  output$step_col_mapper_ui <- renderUI({
    req(input$file_step)
    cols <- read_header(input$file_step,
                        if (!is.null(input$upload_delim)) input$upload_delim else ",")
    if (is.null(cols)) return(NULL)
    div(style = "background:#f9f9f9; border:1px solid #ddd; border-radius:4px;
                 padding:8px 10px; margin-bottom:6px; font-size:0.78rem; color:#555;",
        p(style="margin:0;", tags$b("Columns detected: "),
          paste(cols, collapse=", ")),
        p(style="margin:4px 0 0 0; color:#888;",
          "App will auto-detect timestamp (time/timestamp/step_timestamp) and
         step count (steps/count/step_count).")
    )
  })
  
  # HR column mapper
  output$hr_col_mapper_ui <- renderUI({
    req(input$file_hr)
    cols <- read_header(input$file_hr,
                        if (!is.null(input$upload_delim)) input$upload_delim else ",")
    if (is.null(cols)) return(NULL)
    div(style = "background:#f9f9f9; border:1px solid #ddd; border-radius:4px;
                 padding:8px 10px; margin-bottom:6px; font-size:0.78rem; color:#555;",
        p(style="margin:0;", tags$b("Columns detected: "),
          paste(cols, collapse=", ")),
        p(style="margin:4px 0 0 0; color:#888;",
          "App will auto-detect timestamp and heart rate (heart_rate/hr/bpm).")
    )
  })
  
  # Participant filter checkbox
  output$participant_filter_ui <- renderUI({
    req(rv$loaded)
    ids <- unique(rv$cgm$id)
    tagList(
      div(class="section-label","Filter by participant"),
      checkboxGroupInput("participant_filter", NULL,
                         choices=ids, selected=ids, inline=TRUE),
      p(style="font-size:0.75rem;color:#888;margin-top:4px;",
        "Tick/untick to include or exclude participants. ",
        tags$b("After changing the filter, re-run the analysis on each tab"),
        " to update the outputs.")
    )
  })
  
  ### Reactive subset of rv$cgm filtered to the ticked participant checkboxes
  ## Used by CGM interpolation and SDH detection so analysis only runs on selected participants
  cgm_filtered <- reactive({
    req(rv$cgm, input$participant_filter)
    rv$cgm %>% filter(id %in% input$participant_filter)
  })
  
  # Participant dropdowns for individual tabs
  output$act_id_ui    <- renderUI({
    req(rv$loaded)
    # Union of IDs actually present in step and HR data - not just CGM - so activity data
    # with its own participant IDs (e.g. filename-derived, when no id column was in the file)
    # can still be selected here even if those IDs don't appear in the CGM data.
    ids <- unique(c(
      if (!is.null(rv$step)) rv$step$id else NULL,
      if (!is.null(rv$hr))   rv$hr$id   else NULL,
      if (!is.null(rv$cgm))  rv$cgm$id  else NULL
    ))
    selectInput("act_id", NULL, choices = ids)
  })
  output$sleep_id_ui  <- renderUI({
    req(rv$loaded)
    # sleepVisualise's StudyID applies to sleep data specifically, so this should list
    # participants present in rv$sleep, not require CGM to be loaded alongside it.
    ids <- if (!is.null(rv$sleep)) unique(rv$sleep$id) else unique(rv$cgm$id)
    selectInput("sleep_id", NULL, choices = ids)
  })
  output$link_id_ui   <- renderUI({ req(rv$loaded); selectInput("link_id",  NULL, choices=unique(rv$cgm$id)) })
  output$cgmact_id_ui <- renderUI({ req(rv$loaded); selectInput("cgmact_id",NULL, choices=unique(rv$cgm$id)) })
  
  observe({
    req(rv$loaded)
    updateSelectInput(session, "cgm_trace_id", choices=unique(rv$cgm$id))
  })
  
  ### Data preview tab renders - tables update reactively when the participant filter changes
  # Status bar shown above the data preview panels
  # Displays row counts per data type and flags auto-extracted step/HR data
  output$data_status <- renderUI({
    if (!rv$loaded) {
      div(class="status-bar","No data loaded yet. Select a source and click Load data.")
    } else {
      div(class="status-bar",
          tags$span("✓ Data loaded - ", style=paste0("color:",BRAND_RED,";")),
          paste0(
            "CGM: ",   if (!is.null(rv$cgm))   nrow(rv$cgm)   else 0, " rows | ",
            "Sleep: ", if (!is.null(rv$sleep)) nrow(rv$sleep) else 0, " nights | ",
            "Steps: ", if (!is.null(rv$step))  nrow(rv$step)  else 0, " rows",
            if (!is.null(rv$step) && !is.null(rv$cgm) &&
                nrow(rv$step) > 0 && nrow(rv$cgm) > 0 &&
                "step_timestamp" %in% names(rv$step) &&
                isTRUE(rv$step$step_timestamp[1] == rv$cgm$cgm_timestamp[1]))
              " (auto-extracted)" else "", " | ",
            "HR: ",    if (!is.null(rv$hr))    nrow(rv$hr)    else 0, " rows",
            if (!is.null(rv$hr) && !is.null(rv$cgm) &&
                nrow(rv$hr) > 0 && nrow(rv$cgm) > 0 &&
                "hr_timestamp" %in% names(rv$hr) &&
                isTRUE(rv$hr$hr_timestamp[1] == rv$cgm$cgm_timestamp[1]))
              " (auto-extracted)" else ""
          )
      )
    }
  })
  
  # Filters any dataframe with an "id" column to the currently ticked participant checkboxes
  # Used by all preview tables so unchecking a participant immediately hides their rows
  filter_by_participant <- function(df) {
    if (!is.null(input$participant_filter) &&
        length(input$participant_filter) > 0 &&
        "id" %in% names(df)) {
      df %>% filter(id %in% input$participant_filter)
    } else {
      df
    }
  }
  
  output$preview_cgm <- renderDT({
    req(rv$cgm)
    mk_dt(filter_by_participant(rv$cgm))
  })
  
  output$preview_cgm_interp <- renderDT({
    # Show interpolated data if available, otherwise package example as reference
    if (!is.null(cgm_results()) && !is.null(cgm_results()$interp)) {
      mk_dt(filter_by_participant(cgm_results()$interp))
    } else {
      mk_dt(cgm)
    }
  })
  
  output$preview_sleep <- renderDT({
    req(rv$sleep)
    mk_dt(filter_by_participant(rv$sleep))
  })
  
  output$preview_step <- renderDT({
    req(rv$step)
    mk_dt(filter_by_participant(rv$step))
  })
  
  output$preview_hr <- renderDT({
    req(rv$hr)
    mk_dt(filter_by_participant(rv$hr))
  })
  
  output$preview_checkin <- renderDT({
    req(rv$checkin)
    mk_dt(filter_by_participant(rv$checkin))
  })
  
  output$preview_motif <- renderDT({
    req(rv$motif)
    mk_dt(filter_by_participant(rv$motif))
  })
  
  ### CGM data quality check
  ## cgmCheck() only returns daily CGM hours available so gap statistics (count, duration, % missing)
  ## are derived manually from the raw CGM data using rle() to detect consecutive NA runs
  ## The missingness plot uses actual timestamps so gaps appear at the correct position in the trace
  cgm_check_results <- eventReactive(input$run_cgm_check, {
    req(rv$cgm)
    result <- tryCatch({
      res <- hypometrics::cgmCheck(DataFrame = rv$cgm, CheckAll = TRUE)
      as.data.frame(res)
    }, error = function(e) {
      showNotification(paste("cgmCheck error:", e$message), type = "error",
                       duration = 8)
      NULL
    })
    result
  })
  
  # CGM missingness summary table
  # Derives gap stats from the raw CGM data since cgmCheck only returns daily hours
  output$cgm_check_table <- renderTable({
    req(rv$cgm)
    df <- rv$cgm
    # Filter to selected participant for readability
    if (!is.null(input$cgm_miss_id) && input$cgm_miss_id != "__all__") {
      df <- df %>% filter(id == input$cgm_miss_id)
    } else if (!is.null(input$participant_filter) && length(input$participant_filter) > 0) {
      df <- df %>% filter(id %in% input$participant_filter)
    }
    ids <- unique(df$id)
    
    rows <- lapply(ids, function(pid) {
      d <- df %>% filter(id == pid) %>% arrange(cgm_timestamp)
      total_rows   <- nrow(d)
      na_rows      <- sum(is.na(d$glucose))
      pct_missing  <- round(100 * na_rows / total_rows, 1)
      
      # Gap = consecutive NA runs
      rle_res  <- rle(is.na(d$glucose))
      gap_idx  <- which(rle_res$values == TRUE)
      n_gaps   <- length(gap_idx)
      
      # Duration: each gap row = 1 granularity unit (assume 1 min from interpolation)
      # Use time diff between rows if available
      time_diffs <- as.numeric(diff(d$cgm_timestamp), units="mins")
      median_interval <- if (length(time_diffs)>0) median(time_diffs, na.rm=TRUE) else 1
      gap_lengths <- rle_res$lengths[gap_idx] * median_interval
      mean_gap_dur <- if (n_gaps > 0) round(mean(gap_lengths), 1) else 0
      
      data.frame(
        Participant          = pid,
        `Total readings`     = total_rows,
        `Missing readings`   = na_rows,
        `Gaps (N)`           = n_gaps,
        `Mean gap duration (min)` = mean_gap_dur,
        `% missing`          = pct_missing,
        check.names = FALSE
      )
    })
    do.call(rbind, rows)
  }, striped=TRUE, hover=TRUE)
  
  output$cgm_miss_id_ui <- renderUI({
    req(rv$cgm)
    ids <- unique(rv$cgm$id)
    selectInput("cgm_miss_id", NULL, choices = c("All participants" = "__all__", ids),
                selected = "__all__", width = "200px")
  })
  
  # Missingness distribution plot - highlights gap regions on glucose trace
  output$cgm_miss_plot <- renderPlot({
    req(rv$cgm)
    df <- rv$cgm
    # Filter to selected participant for readability
    if (!is.null(input$cgm_miss_id) && input$cgm_miss_id != "__all__") {
      df <- df %>% filter(id == input$cgm_miss_id)
    } else if (!is.null(input$participant_filter) && length(input$participant_filter) > 0) {
      df <- df %>% filter(id %in% input$participant_filter)
    }
    ids <- unique(df$id)
    
    # One facet per participant showing glucose trace with NA regions highlighted
    df <- df %>%
      arrange(id, cgm_timestamp) %>%
      group_by(id) %>%
      mutate(row_n = row_number()) %>%
      ungroup()
    
    # Build gap rectangles
    gap_rects <- df %>%
      group_by(id) %>%
      mutate(is_na = is.na(glucose),
             gap_id = cumsum(c(0, diff(is_na) != 0))) %>%
      filter(is_na) %>%
      group_by(id, gap_id) %>%
      summarise(xmin = min(row_n), xmax = max(row_n), .groups="drop")
    
    y_lab <- if (!is.null(input$global_glucose_unit) &&
                 input$global_glucose_unit == "mg/dL") "Glucose (mg/dL)" else "Glucose (mmol/L)"
    
    # Build gap rectangles using actual timestamps (not row index)
    # so the x-axis shows dates matching the rest of the GUI
    df2 <- df %>% arrange(id, cgm_timestamp) %>% group_by(id) %>%
      mutate(is_na = is.na(glucose)) %>% ungroup()
    
    gap_ts <- df2 %>%
      group_by(id) %>%
      mutate(gap_id = cumsum(c(0, diff(is_na) != 0))) %>%
      filter(is_na) %>%
      group_by(id, gap_id) %>%
      summarise(
        xmin = min(cgm_timestamp),
        xmax = max(cgm_timestamp),
        .groups = "drop"
      )
    
    p <- ggplot(df2, aes(x = cgm_timestamp, y = glucose)) +
      # Highlight missing regions as tall orange/red bars like imputeTS style
      { if (nrow(gap_ts) > 0)
        geom_rect(data = gap_ts,
                  aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
                  fill = "#E8836D", alpha = 0.6, inherit.aes = FALSE)
        else list() } +
      # Non-missing values as points (imputeTS style) connected by lines
      geom_line(colour = "#2471A3", linewidth = 0.4, na.rm = TRUE, alpha = 0.8) +
      geom_point(data = df2 %>% filter(!is.na(glucose)),
                 aes(x = cgm_timestamp, y = glucose),
                 colour = "#2471A3", size = 0.6, na.rm = TRUE) +
      scale_x_datetime(date_breaks = adaptive_date_breaks(min(df2$cgm_timestamp, na.rm=TRUE),
                                                          max(df2$cgm_timestamp, na.rm=TRUE)),
                       date_labels = "%d %b") +
      facet_wrap(~id, ncol = 1, scales = "fixed") +
      light_theme() +
      theme(axis.text.x = element_text(angle = 30, hjust = 1)) +
      labs(x = NULL, y = y_lab,
           caption = "Orange/red shaded bars = missing data gaps")
    print(p)
  })
  
  output$dl_cgm_check <- downloadHandler(
    filename = function() paste0("cgm_missingness_", Sys.Date(), ".csv"),
    content  = function(f) {
      req(cgm_check_results())
      write.csv(cgm_check_results(), f, row.names=FALSE)
    }
  )
  
  ### CGM interpolation and summary statistics
  ## cgmSummarise() requires mg/dL input regardless of the GlucoseUnit argument
  ## If the display unit is mmol/L, glucose is converted to mg/dL before the call
  ## and the returned concentration columns (mean, SD, median etc.) are converted back for display
  ## Time-in-range columns are left in mg/dL as the package uses mg/dL thresholds (70, 180, 250) internally
  ### CGM analysis reactive - runs cgmInterpolate then cgmSummarise on button click
  ## Returns a list: interp (interpolated CGM dataframe), summary (cgmSummarise output), n_filled (gap count)
  ## eventReactive means it only re-runs when the button is clicked, not on every parameter change
  cgm_results <- eventReactive(input$run_cgm, {
    req(cgm_filtered())
    
    cgm_interp <- tryCatch(
      hypometrics::cgmInterpolate(
        DataFrame   = cgm_filtered(),
        Interpolate = input$cgm_interpolate,
        MinGap      = input$cgm_mingap * 60,
        MaxGap      = input$cgm_maxgap * 60,
        Granularity = input$cgm_gran   * 60
      ),
      error=function(e){ showNotification(paste("cgmInterpolate:",e$message),type="error"); NULL }
    )
    req(cgm_interp)
    
    n_filled <- max(0L,
                    sum(is.na(cgm_filtered()$glucose)) - sum(is.na(cgm_interp$glucose)))
    
    disp_unit <- if (!is.null(input$global_glucose_unit)) input$global_glucose_unit else "mmol/L"
    
    # cgmSummarise always needs mg/dL internally.
    # If user's data is mmol/L, convert for the call then convert results back.
    # If user's data is mg/dL, pass directly.
    if (disp_unit == "mmol/L") {
      cgm_for_sum <- cgm_interp %>% mutate(glucose = glucose * MMOL_TO_MGDL)
    } else {
      cgm_for_sum <- cgm_interp
    }
    
    cgm_sum <- tryCatch(
      # Try with full arguments first
      hypometrics::cgmSummarise(
        DataFrame=cgm_for_sum, GlucoseUnit="mg/dL",
        InterQuartileRange=c(25,75), InRange=c(70,180),
        AboveRange=c(180,250), BelowRange=c(70,54)
      ),
      error=function(e){
        # Fallback: try with minimal arguments in case function signature changed
        tryCatch(
          hypometrics::cgmSummarise(DataFrame=cgm_for_sum, GlucoseUnit="mg/dL"),
          error=function(e2){
            showNotification(paste("cgmSummarise:", e2$message), type="error", duration=10)
            NULL
          }
        )
      }
    )
    
    if (!is.null(cgm_sum) && disp_unit == "mmol/L") {
      cgm_sum <- cgm_sum %>% mutate(
        mean_glu   = round(mean_glu   / MMOL_TO_MGDL, 2),
        sd_glu     = round(sd_glu     / MMOL_TO_MGDL, 2),
        median_glu = round(median_glu / MMOL_TO_MGDL, 2),
        Q1_glu     = round(Q1_glu     / MMOL_TO_MGDL, 2),
        Q3_glu     = round(Q3_glu     / MMOL_TO_MGDL, 2)
      )
    }
    list(interp=cgm_interp, summary=cgm_sum, n_filled=n_filled)
  })
  
  output$cgm_interp_note <- renderUI({
    r <- cgm_results()
    interp_box(paste0(r$n_filled," gap reading(s) filled. Granularity: ",
                      input$cgm_gran," min. Max gap: ",input$cgm_maxgap," min."))
  })
  
  output$cgm_trace_plot <- renderPlotly({
    req(cgm_results()$interp)
    # Use first participant if cgm_trace_id not yet set
    sel_id <- if (!is.null(input$cgm_trace_id) && nchar(input$cgm_trace_id) > 0) {
      input$cgm_trace_id
    } else {
      cgm_results()$interp$id[1]
    }
    df <- cgm_results()$interp %>% filter(id == sel_id)
    validate(need(nrow(df) > 0, "No data for selected participant."))
    # Determine threshold values based on selected display unit
    disp_unit_trace <- if (!is.null(input$global_glucose_unit)) input$global_glucose_unit else "mmol/L"
    if (disp_unit_trace == "mmol/L") {
      th_very_low <- 3.0;  th_low <- 3.9
      th_high     <- 10.0; th_very_high <- 13.9
      y_lab <- "Glucose (mmol/L)"
    } else {
      th_very_low <- 54;  th_low <- 70
      th_high     <- 180; th_very_high <- 250
      y_lab <- "Glucose (mg/dL)"
    }
    x_min <- min(df$cgm_timestamp, na.rm=TRUE)
    
    # Stagger label offsets so closely spaced lines (2.8 and 3.9) don't overlap
    off <- if (disp_unit_trace == "mmol/L") 0.25 else 5
    
    p <- ggplot(df, aes(x=cgm_timestamp, y=glucose)) +
      # AGP threshold lines - colours match the ambulatory glucose profile standard:
      # Very High (>13.9) = orange, High (10-13.9) = amber/yellow,
      # Low (3.0-3.9) = red, Very Low (<3.0) = dark red
      geom_hline(yintercept=th_very_high, colour="#E67E22", linewidth=0.8, linetype="solid") +
      geom_hline(yintercept=th_high,      colour="#F4D03F", linewidth=0.8, linetype="solid") +
      geom_hline(yintercept=th_low,       colour="#C0392B", linewidth=0.8, linetype="solid") +
      geom_hline(yintercept=th_very_low,  colour="#7B241C", linewidth=0.8, linetype="solid") +
      # Labels staggered to avoid overlap - positioned with bg for readability
      # Threshold labels are shown in a separate legend key to the right of the plot
      # (see cgm_threshold_legend renderUI) so they don't overlap the glucose trace
      # Glucose trace
      geom_line(colour="#1B3A6B", linewidth=0.6, na.rm=TRUE) +
      scale_x_datetime(date_breaks=adaptive_date_breaks(x_min, max(df$cgm_timestamp, na.rm=TRUE)),
                       date_labels="%d %b") +
      # Extend y-axis so the Very High label is never clipped by the plot boundary
      scale_y_continuous(
        limits = c(
          min(df$glucose, na.rm=TRUE) - off * 5,
          th_very_high + off * 3
        )
      ) +
      light_theme() +
      theme(axis.text.x=element_text(angle=30, hjust=1)) +
      labs(x=NULL, y=y_lab, title=paste0("Participant: ", sel_id))
    ggplotly(p) %>% plotly_light()
  })
  
  # External threshold legend shown beside the CGM trace plot
  output$cgm_threshold_legend <- renderUI({
    disp_unit <- if (!is.null(input$global_glucose_unit)) input$global_glucose_unit else "mmol/L"
    if (disp_unit == "mmol/L") {
      vals <- list(
        list(v="13.9", label="Very High", colour="#E67E22"),
        list(v="10.0", label="High",      colour="#B7950B"),
        list(v="3.9",  label="Low",       colour="#C0392B"),
        list(v="3.0",  label="Very Low",  colour="#7B241C")
      )
    } else {
      vals <- list(
        list(v="250", label="Very High", colour="#E67E22"),
        list(v="180", label="High",      colour="#B7950B"),
        list(v="70",  label="Low",       colour="#C0392B"),
        list(v="54",  label="Very Low",  colour="#7B241C")
      )
    }
    div(
      style="border:1px solid #ddd; border-radius:4px; padding:10px 8px;
             background:#fafafa; font-size:0.8rem;",
      tags$p(style="font-weight:700; margin:0 0 8px 0; color:#333;",
             "Glucose thresholds"),
      tagList(lapply(vals, function(v) {
        div(style="display:flex; align-items:center; margin-bottom:8px; gap:6px;",
            div(style=paste0("width:20px; height:3px; background:", v$colour,
                             "; flex-shrink:0;")),
            div(
              tags$b(style=paste0("color:", v$colour, ";"), v$v),
              br(),
              span(style="color:#555;", v$label)
            )
        )
      }))
    )
  })
  
  output$cgm_summary_table <- renderDT({
    req(cgm_results()$summary); mk_dt(cgm_results()$summary)
  })
  # Download handler for the cgmSummarise output table
  output$dl_cgm_summary <- downloadHandler(
    filename=function() paste0("cgm_summary_",Sys.Date(),".csv"),
    content=function(f) write.csv(cgm_results()$summary,f,row.names=FALSE))
  
  ### Sensor-detected hypoglycaemia (SDH) detection and summary
  ## sdhDetection() returns an sdh_interval column as a lubridate Interval object
  output$hypo_thresh_ui <- renderUI({
    # Was previously a static mmol/L-only input with no relation to the unit the data is
    # actually stored in - since rv$cgm$glucose keeps whatever unit was uploaded (no
    # auto-conversion), this widget needs to match that unit or the threshold comparison
    # in sdhDetection() is comparing against a value in the wrong scale entirely.
    is_mgdl <- !is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL"
    if (is_mgdl) {
      tagList(
        div(class="section-label","Glucose threshold (mg/dL)"),
        # 70 mg/dL = the standard clinical Level 1 hypoglycaemia threshold, equivalent to 3.9 mmol/L
        numericInput("hypo_thresh", NULL, value=70, min=36, max=90, step=2)
      )
    } else {
      tagList(
        div(class="section-label","Glucose threshold (mmol/L)"),
        numericInput("hypo_thresh", NULL, value=3.9, min=2.0, max=5.0, step=0.1)
      )
    }
  })
  
  ## DT cannot render Interval objects so interval_to_char() converts them to strings for display
  ## The raw output is stored in rv$sdh_raw with Intervals intact so sdhSummarise() receives the correct input
  observeEvent(input$run_sdh, {
    req(cgm_results())
    sdh_raw <- tryCatch(
      hypometrics::sdhDetection(
        DataFrame        = cgm_results()$interp,
        DetectionLimit   = input$hypo_thresh,
        DetectionDuration= input$sdh_dur,
        AddSleepStatus   = input$sdh_sleep
      ),
      error=function(e){ showNotification(paste("sdhDetection:",e$message),type="error"); NULL }
    )
    rv$sdh_raw <- sdh_raw   # raw output kept for sdhSummarise
  })
  
  sdh_display <- reactive({
    req(rv$sdh_raw)
    df <- rv$sdh_raw
    if ("sdh_interval" %in% names(df)) {
      df <- df %>% mutate(sdh_interval=interval_to_char(sdh_interval))
    }
    df
  })
  
  output$sdh_table <- renderDT({
    req(sdh_display())
    mk_dt(sdh_display())
  })
  
  output$sdh_flags <- renderUI({
    req(rv$sdh_raw)
    if (nrow(rv$sdh_raw)==0) {
      ok_box("✓ No hypoglycaemic episodes detected at current settings.")
    } else {
      warn_box(paste0("⚠ ",nrow(rv$sdh_raw)," episode(s) detected at ≤",
                      input$hypo_thresh,
                      if (!is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL")
                        " mg/dL for ≥" else " mmol/L for ≥",
                      input$sdh_dur," min."))
    }
  })
  
  output$dl_sdh_episodes <- downloadHandler(
    filename=function() paste0("sdh_episodes_",Sys.Date(),".csv"),
    content=function(f) write.csv(sdh_display(),f,row.names=FALSE))
  
  # sdhSummarise reactive - uses rv$sdh_raw which keeps lubridate Intervals intact as required by the function
  sdh_sum_results <- eventReactive(input$run_sdh_sum, {
    # Must use rv$sdh_raw (Interval intact), NOT the display version
    validate(need(!is.null(rv$sdh_raw) && nrow(rv$sdh_raw) > 0,
                  "No SDH episodes found. Run 'Detect hypoglycaemia episodes' first."))
    tryCatch(
      hypometrics::sdhSummarise(
        DataFrame      = rv$sdh_raw,
        DetectionLimit = as.character(input$hypo_thresh),
        LongDuration   = input$sdh_long,
        AddSleepSummary= "no"
      ),
      error=function(e){ showNotification(paste("sdhSummarise:",e$message),type="error"); NULL }
    )
  })
  
  output$sdh_summary_table <- renderDT({
    req(sdh_sum_results()); mk_dt(sdh_sum_results())
  })
  # Download handler for the sdhSummarise output table
  output$dl_sdh_summary <- downloadHandler(
    filename=function() paste0("sdh_summary_",Sys.Date(),".csv"),
    content=function(f) write.csv(sdh_sum_results(),f,row.names=FALSE))
  
  ### Physical activity tab
  ## activityVisualise() returns a ggplot so theme overrides and date scales are applied on top of its output
  ## If activityVisualise() fails (e.g. non-Fitbit format) a plain ggplot fallback renders daily totals instead
  ## Summary tables react directly to rv$step/rv$hr so they update when the daily/weekly toggle changes
  # Validates that step and HR data are available and bundles them with the selected participant ID
  # Both plot outputs and summary tables depend on this reactive so they only render after the button is clicked
  activity_results <- eventReactive(input$run_activity, {
    req(input$act_id)
    # Step and HR plots each only need their own data type - a participant with steps but
    # no HR (or vice versa) should still get the plot that data supports.
    list(step=rv$step, hr=rv$hr, id=input$act_id)
  })
  
  # Step count visualisation - tries activityVisualise first,
  # falls back to plain ggplot for non-Fitbit formatted data
  output$activity_step_vis_plot <- renderPlot({
    req(activity_results(), rv$step)
    r <- activity_results()
    p <- tryCatch(
      hypometrics::activityVisualise(
        DataFrame  = r$step,
        DataType   = "stepcount",
        TimeBreak  = input$act_timebreak,
        PageNumber = input$act_page,
        StudyID    = r$id
      ),
      error = function(e) NULL   # silent - fallback below
    )
    # axis_theme overrides applied on top of activityVisualise ggplot output
    axis_theme <- theme(
      axis.text.x  = element_text(size = 13, angle = 30, hjust = 1),
      axis.text.y  = element_text(size = 13),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
    
    if (!is.null(p)) {
      # activityVisualise returns a ggplot - add theme overrides and more x breaks
      brk <- if (!is.null(input$act_timebreak) && input$act_timebreak != "no") "1 day"
             else adaptive_date_breaks(min(r$step$step_timestamp, na.rm=TRUE),
                                       max(r$step$step_timestamp, na.rm=TRUE))
      print(
        p +
          scale_x_datetime(date_breaks = brk, date_labels = "%d %b") +
          axis_theme
      )
    } else {
      # Fallback: plain ggplot bar chart from rv$step
      df <- r$step %>%
        filter(id == r$id) %>%
        mutate(date = as.Date(step_timestamp)) %>%
        group_by(date) %>%
        summarise(steps = sum(count, na.rm = TRUE), .groups = "drop")
      validate(need(nrow(df) > 0, "No step data available for this participant."))
      brk <- if (!is.null(input$act_timebreak) && input$act_timebreak != "no") "1 day"
             else adaptive_date_breaks(min(df$date), max(df$date))
      print(
        ggplot(df, aes(x = date, y = steps)) +
          geom_col(fill = BRAND_RED, alpha = 0.8) +
          scale_x_date(date_breaks = brk, date_labels = "%d %b") +
          light_theme() +
          axis_theme +
          labs(x = NULL, y = "Daily steps",
               title = paste0("Step count - Participant: ", r$id),
               caption = "Daily totals aggregated from 5-min interval data")
      )
    }
  })
  
  # Heart rate visualisation - tries activityVisualise first, falls back to ggplot
  output$activity_hr_vis_plot <- renderPlot({
    req(activity_results(), rv$hr)
    r <- activity_results()
    p <- tryCatch(
      hypometrics::activityVisualise(
        DataFrame  = r$hr,
        DataType   = "heartrate",
        TimeBreak  = input$act_timebreak,
        PageNumber = input$act_page,
        StudyID    = r$id
      ),
      error = function(e) NULL   # silent - fallback below
    )
    axis_theme <- theme(
      axis.text.x  = element_text(size = 13, angle = 30, hjust = 1),
      axis.text.y  = element_text(size = 13),
      axis.title.x = element_text(size = 14),
      axis.title.y = element_text(size = 14)
    )
    
    if (!is.null(p)) {
      brk <- if (!is.null(input$act_timebreak) && input$act_timebreak != "no") "1 day"
             else adaptive_date_breaks(min(r$hr$hr_timestamp, na.rm=TRUE),
                                       max(r$hr$hr_timestamp, na.rm=TRUE))
      print(
        p +
          scale_x_datetime(date_breaks = brk, date_labels = "%d %b") +
          axis_theme
      )
    } else {
      # Fallback: plain ggplot line chart from rv$hr
      df <- r$hr %>%
        filter(id == r$id) %>%
        mutate(date = as.Date(hr_timestamp)) %>%
        group_by(date) %>%
        summarise(mean_hr = mean(heart_rate, na.rm = TRUE), .groups = "drop")
      validate(need(nrow(df) > 0, "No heart rate data available for this participant."))
      brk <- if (!is.null(input$act_timebreak) && input$act_timebreak != "no") "1 day"
             else adaptive_date_breaks(min(df$date), max(df$date))
      print(
        ggplot(df, aes(x = date, y = mean_hr)) +
          geom_line(colour = "#2471A3", linewidth = 0.8) +
          geom_point(colour = "#2471A3", size = 2) +
          scale_x_date(date_breaks = brk, date_labels = "%d %b") +
          light_theme() +
          axis_theme +
          labs(x = NULL, y = "Mean daily heart rate (bpm)",
               title = paste0("Heart rate - Participant: ", r$id),
               caption = "Daily means aggregated from 5-min interval data")
      )
    }
  })
  
  # Steps summary - per participant
  # Caption showing current period selection
  output$activity_steps_caption <- renderUI({
    period <- if (!is.null(input$act_summary_period)) input$act_summary_period else "daily"
    p(style="font-size:0.78rem;color:#888;margin-bottom:6px;",
      if (period == "daily") "Showing mean, max and min across all days per participant."
      else "Showing mean steps per week per participant.")
  })
  
  output$activity_steps_table <- renderTable({
    req(rv$step)
    validate(need(all(c("step_timestamp","count") %in% names(rv$step)),
                 "Step data is missing a recognised timestamp or step count column - re-check the file on the Data tab."))
    period <- if (!is.null(input$act_summary_period)) input$act_summary_period else "daily"
    
    daily <- rv$step %>%
      mutate(date = as.Date(step_timestamp)) %>%
      group_by(id, date) %>%
      summarise(steps = sum(count, na.rm = TRUE), .groups = "drop")
    
    if (period == "daily") {
      # Daily: mean, max, min per participant
      daily %>%
        group_by(id) %>%
        summarise(
          "Mean daily steps" = format(round(mean(steps)), big.mark = ","),
          "Max daily steps"  = format(max(steps),         big.mark = ","),
          "Min daily steps"  = format(min(steps),         big.mark = ","),
          .groups = "drop"
        ) %>%
        rename("Participant" = id)
    } else {
      # Weekly: sum steps per ISO week, then mean across weeks per participant
      daily %>%
        mutate(week = format(date, "%Y-W%V")) %>%
        group_by(id, week) %>%
        summarise(weekly_steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
        group_by(id) %>%
        summarise(
          "Mean weekly steps" = format(round(mean(weekly_steps)), big.mark = ","),
          "Max weekly steps"  = format(max(weekly_steps),         big.mark = ","),
          "Min weekly steps"  = format(min(weekly_steps),         big.mark = ","),
          .groups = "drop"
        ) %>%
        rename("Participant" = id)
    }
  }, striped = TRUE, hover = TRUE)
  
  output$activity_hr_caption <- renderUI({
    period <- if (!is.null(input$act_summary_period)) input$act_summary_period else "daily"
    p(style="font-size:0.78rem;color:#888;margin-bottom:6px;",
      if (period == "daily") "Showing mean, max and min daily HR per participant."
      else "Showing mean HR per week per participant.")
  })
  
  # HR summary - per participant, daily or weekly
  output$activity_hr_table <- renderTable({
    req(rv$hr)
    validate(need(all(c("hr_timestamp","heart_rate") %in% names(rv$hr)),
                 "Heart rate data is missing a recognised timestamp or heart rate column - re-check the file on the Data tab."))
    period <- if (!is.null(input$act_summary_period)) input$act_summary_period else "daily"
    
    daily_hr <- rv$hr %>%
      mutate(date = as.Date(hr_timestamp)) %>%
      group_by(id, date) %>%
      summarise(mean_hr = mean(heart_rate, na.rm = TRUE), .groups = "drop")
    
    if (period == "daily") {
      daily_hr %>%
        group_by(id) %>%
        summarise(
          "Mean daily HR (bpm)" = round(mean(mean_hr), 1),
          "Max daily HR (bpm)"  = round(max(mean_hr),  1),
          "Min daily HR (bpm)"  = round(min(mean_hr),  1),
          .groups = "drop"
        ) %>%
        rename("Participant" = id)
    } else {
      daily_hr %>%
        mutate(week = format(date, "%Y-W%V")) %>%
        group_by(id, week) %>%
        summarise(weekly_hr = mean(mean_hr, na.rm = TRUE), .groups = "drop") %>%
        group_by(id) %>%
        summarise(
          "Mean weekly HR (bpm)" = round(mean(weekly_hr), 1),
          "Max weekly HR (bpm)"  = round(max(weekly_hr),  1),
          "Min weekly HR (bpm)"  = round(min(weekly_hr),  1),
          .groups = "drop"
        ) %>%
        rename("Participant" = id)
    }
  }, striped = TRUE, hover = TRUE)
  
  # Overall summary - adapts label to selected period
  output$activity_overall_table <- renderTable({
    req(rv$step, rv$hr)
    period <- if (!is.null(input$act_summary_period)) input$act_summary_period else "daily"
    period_label <- if (period == "daily") "daily" else "weekly"
    
    step_mean <- rv$step %>%
      mutate(date = as.Date(step_timestamp)) %>%
      group_by(id, date) %>%
      summarise(steps = sum(count, na.rm = TRUE), .groups = "drop") %>%
      { if (period == "weekly") {
        mutate(., week = format(date, "%Y-W%V")) %>%
          group_by(week) %>%
          summarise(steps = sum(steps, na.rm = TRUE), .groups = "drop") %>%
          pull(steps) %>% mean() %>% round()
      } else pull(., steps) %>% mean() %>% round() }
    
    hr_mean <- rv$hr %>%
      pull(heart_rate) %>% mean(na.rm = TRUE) %>% round(1)
    
    data.frame(
      Metric = c(paste0("Overall mean ", period_label, " steps"),
                 paste0("Overall mean ", period_label, " HR (bpm)")),
      Value  = c(format(step_mean, big.mark = ","), hr_mean)
    )
  }, striped = TRUE, hover = TRUE)
  
  output$dl_steps_summary <- downloadHandler(
    filename=function() paste0("steps_summary_",Sys.Date(),".csv"),
    content=function(f){
      out <- rv$step %>% mutate(date=as.Date(step_timestamp)) %>%
        group_by(id,date) %>% summarise(steps=sum(count,na.rm=TRUE),.groups="drop")
      write.csv(out,f,row.names=FALSE)
    })
  # Download handler for the heart rate summary table
  output$dl_hr_summary <- downloadHandler(
    filename=function() paste0("hr_summary_",Sys.Date(),".csv"),
    content=function(f){
      out <- rv$hr %>% mutate(date=as.Date(hr_timestamp)) %>%
        group_by(id,date) %>% summarise(mean_hr=mean(heart_rate,na.rm=TRUE),.groups="drop")
      write.csv(out,f,row.names=FALSE)
    })
  
  output$dl_overall_summary <- downloadHandler(
    filename=function() paste0("overall_activity_summary_",Sys.Date(),".csv"),
    content=function(f){
      req(rv$step, rv$hr)
      step_mean <- rv$step %>%
        mutate(date=as.Date(step_timestamp)) %>%
        group_by(id,date) %>% summarise(steps=sum(count,na.rm=TRUE),.groups="drop") %>%
        pull(steps) %>% mean() %>% round()
      hr_mean <- rv$hr %>% pull(heart_rate) %>% mean(na.rm=TRUE) %>% round(1)
      out <- data.frame(
        Metric=c("Overall mean daily steps","Overall mean daily HR (bpm)"),
        Value=c(format(step_mean,big.mark=","), hr_mean)
      )
      write.csv(out, f, row.names=FALSE)
    })
  
  ### Sleep analysis tab
  ## sleepCategorise() and sleepSummarise() run together when the button is clicked
  ## sleepVisualise() is decoupled from the run button so the all/individual toggle
  ## updates the histogram immediately without re-running the full sleep pipeline
  ## sleepVisualise returns a base R plot (not ggplot) so par() is used to set text size before printing
  # Runs sleepCategorise and sleepSummarise when "Run sleep analysis" is clicked.
  # sleepVisualise is handled in a separate renderPlot that reacts directly
  # to the all/individual toggle so the histogram updates without re-running.
  # Runs sleepCategorise and sleepSummarise when the button is clicked
  sleep_results <- eventReactive(input$run_sleep, {
    req(rv$sleep)
    # Show column names to help diagnose format issues with the sleep data
    showNotification(paste("Sleep columns:", paste(names(rv$sleep), collapse=", ")),
                     type="message", duration=8)
    
    cat_df <- tryCatch(
      hypometrics::sleepCategorise(DataFrame=rv$sleep),
      error=function(e){
        showNotification(paste("sleepCategorise:", e$message), type="error", duration=10)
        NULL
      }
    )
    sum_df <- tryCatch(
      hypometrics::sleepSummarise(DataFrame=rv$sleep),
      error=function(e){
        showNotification(paste("sleepSummarise:", e$message), type="error", duration=10)
        NULL
      }
    )
    vis_p <- tryCatch(
      hypometrics::sleepVisualise(
        DataFrame   =rv$sleep,
        VisualiseAll=isTRUE(input$sleep_vis_all),
        StudyID     =if (!isTRUE(input$sleep_vis_all)) isolate(input$sleep_id) else ""
      ),
      error=function(e){
        if (isTRUE(input$sleep_vis_all)) {
          showNotification(
            "The \"Show all participants\" view has a known bug in the hypometrics package itself. Untick the box and select a participant instead.",
            type = "error", duration = 12)
        } else {
          showNotification(paste("sleepVisualise:", e$message), type="error", duration=10)
        }
        NULL
      }
    )
    list(raw=rv$sleep, categorised=cat_df, summary=sum_df, vis=vis_p)
  })
  
  output$sleep_cat_table <- renderDT({
    req(sleep_results()$categorised); mk_dt(sleep_results()$categorised)
  })
  # Summary table from sleepSummarise - nights with data, avg asleep/awake/in-bed
  output$sleep_summary_table <- renderDT({
    req(sleep_results()$summary)
    display_df <- sleep_results()$summary %>%
      rename("Participant"="id","Nights missing"="n_nights_missing",
             "Nights with data"="n_nights_with_sleep_data",
             "Avg in bed (h)"="average_time_in_bed_hours",
             "Avg asleep (h)"="average_time_asleep_hours",
             "Avg awake (h)"="average_time_awake_hours")
    datatable(display_df, options=list(pageLength=5,scrollX=FALSE), rownames=FALSE)
  })
  # Grouped bar chart of sleepSummarise output with user-adjustable threshold line
  output$sleep_plot <- renderPlotly({
    df <- sleep_results()$summary; req(df)
    df_long <- df %>%
      select(id,average_time_asleep_hours,average_time_awake_hours,
             average_time_in_bed_hours) %>%
      pivot_longer(-id,names_to="metric",values_to="hours") %>%
      mutate(metric=recode(metric,
                           average_time_asleep_hours="Avg asleep (h)",
                           average_time_awake_hours ="Avg awake (h)",
                           average_time_in_bed_hours="Avg in bed (h)"))
    threshold <- if (!is.null(input$sleep_threshold_h)) input$sleep_threshold_h else 6
    p <- ggplot(df_long,aes(x=metric,y=hours,fill=id)) +
      geom_col(position="dodge",alpha=0.85) +
      geom_hline(yintercept=threshold,linetype="dashed",colour="#e07b54",linewidth=0.6) +
      annotate("text", x=0.5, y=threshold+0.15,
               label=paste0(threshold, "h threshold"),
               colour="#e07b54", hjust=0, size=5, fontface="bold") +
      scale_fill_manual(values=participant_colours) +
      light_theme() +
      labs(x=NULL,y="Hours",fill="Participant")
    ggplotly(p) %>% plotly_light()
  })
  output$sleep_vis_plot <- renderPlot({
    # This plot reacts to toggles directly so users see changes immediately
    # without needing to click run again. Requires sleep data to be loaded.
    req(rv$sleep)
    vis <- tryCatch(
      hypometrics::sleepVisualise(
        DataFrame    = rv$sleep,
        VisualiseAll = input$sleep_vis_all,
        StudyID      = if (!isTRUE(input$sleep_vis_all) &&
                           !is.null(input$sleep_id) &&
                           nchar(input$sleep_id) > 0) {
          input$sleep_id
        } else ""
      ),
      error = function(e) {
        # VisualiseAll=TRUE has a confirmed bug in hypometrics::sleepVisualise itself
        # (mismatched scale_x_time breaks/labels when ggplot auto-generates break positions) -
        # give a clear explanation pointing at the working alternative instead of the raw
        # internal ggplot error, which otherwise reads as "In index: 1" with no clear cause.
        if (isTRUE(input$sleep_vis_all)) {
          showNotification(
            "The \"Show all participants\" view has a known bug in the hypometrics package itself (a mismatch in its internal plotting code). Untick the box and select a participant instead - the single-participant view works correctly.",
            type = "error", duration = 12)
        } else {
          showNotification(paste("sleepVisualise:", e$message), type="error")
        }
        NULL
      }
    )
    req(vis)
    old_par <- par(no.readonly = TRUE)
    on.exit(par(old_par))
    par(cex = 1.4, cex.axis = 1.3, cex.lab = 1.4, cex.main = 1.4)
    print(vis)
  }, res = 96)
  # Download handlers for sleep output tables
  output$dl_sleep_summary <- downloadHandler(
    filename=function() paste0("sleep_summary_",Sys.Date(),".csv"),
    content=function(f) write.csv(sleep_results()$summary,f,row.names=FALSE))
  output$dl_sleep_cat <- downloadHandler(
    filename=function() paste0("sleep_categorised_",Sys.Date(),".csv"),
    content=function(f) write.csv(sleep_results()$categorised,f,row.names=FALSE))
  
  ### Person-reported hypoglycaemia (PRH) tab
  ## Pipeline order: umotifClean (motif) -> umotifClean (check-in) -> prhLink -> prhSummarise -> prhVisualise
  ## prhVisualise requires the RAW uMotif segment data (rv$motif) not the cleaned version
  ## because it needs the original uMotif column structure (UserId, SegmentId, Value)
  ## The vis type toggle (upset/heatmap) is decoupled from the run button so switching it updates the plot immediately
  ### PRH pipeline reactive - runs on button click in the correct order
  ## motif_clean is stored in rv so prhVisualise can access it independently of the run button
  ## prh_linked_data is stored in rv so the CGM-PRH tab can access it without re-running the pipeline
  prh_results <- eventReactive(input$run_prh, {
    req(rv$checkin, rv$motif)
    
    checkin_clean <- tryCatch(
      hypometrics::umotifClean(DataFrame=rv$checkin,FileType="checkin"),
      error=function(e){ showNotification(paste("umotifClean (checkin):",e$message),type="error"); NULL }
    )
    motif_clean <- tryCatch(
      hypometrics::umotifClean(DataFrame=rv$motif,FileType="motif"),
      error=function(e){ showNotification(paste("umotifClean (motif):",e$message),type="error"); NULL }
    )
    req(checkin_clean, motif_clean)
    
    # Store motif_clean for prhVisualise - it needs raw uMotif columns
    rv$motif_clean <- as.data.frame(motif_clean)
    
    # prhLink: if user wants sleep status, validate the sleep dataframe first
    # prhLink requires sleep data with startTime and endTime as POSIXct
    add_sleep <- input$prh_sleep
    sleep_df  <- ""
    if (add_sleep == "yes") {
      if (!is.null(rv$sleep) && nrow(rv$sleep) > 0) {
        # Ensure required columns exist and are correct type
        sl <- rv$sleep
        required_sleep_cols <- c("id", "startTime", "endTime")
        missing_sl <- setdiff(required_sleep_cols, names(sl))
        if (length(missing_sl) > 0) {
          showNotification(
            paste0("Sleep data is missing columns needed by prhLink: ",
                   paste(missing_sl, collapse=", "),
                   ". Running without sleep status."),
            type = "warning", duration = 8)
          add_sleep <- "no"
        } else {
          # Ensure POSIXct - prhLink uses interval arithmetic on these
          sl$startTime <- as.POSIXct(sl$startTime)
          sl$endTime   <- as.POSIXct(sl$endTime)
          sleep_df <- sl
        }
      } else {
        showNotification(
          "Sleep data not loaded - running prhLink without sleep status.",
          type = "warning", duration = 6)
        add_sleep <- "no"
      }
    }
    
    # If sleep was requested, attempt prhLink with sleep first.
    # If it fails with levels.data error, automatically retry without sleep.
    prh_linked <- tryCatch(
      hypometrics::prhLink(
        MotifDataFrame   = motif_clean,
        CheckinDataFrame = checkin_clean,
        AddSleepStatus   = add_sleep,
        SleepDataFrame   = sleep_df
      ),
      warning = function(w) {
        showNotification(paste0("prhLink warning: ", conditionMessage(w)),
                         type = "warning", duration = 6)
        suppressWarnings(
          hypometrics::prhLink(
            MotifDataFrame   = motif_clean,
            CheckinDataFrame = checkin_clean,
            AddSleepStatus   = add_sleep,
            SleepDataFrame   = sleep_df
          )
        )
      },
      error = function(e) {
        msg <- conditionMessage(e)
        if (is.null(msg) || nchar(trimws(msg)) == 0) {
          msg <- "prhLink failed. Try setting both sleep options to 'No'."
        } else if (grepl("levels.data", msg, fixed = TRUE)) {
          msg <- paste0("prhLink: the sleep data format is not compatible with ",
                        "the sleep status option (missing 'levels.data' column). ",
                        "Set both sleep options to 'No' and re-run.")
        }
        showNotification(msg, type = "error", duration = 12)
        NULL
      }
    )
    req(prh_linked)
    rv$prh_linked_data <- as.data.frame(prh_linked)
    
    # prhSummarise: AddSleepSummary = "yes" only works if prhLink was run
    # with AddSleepStatus = "yes" - guard against mismatch
    add_sleep_sum <- input$prh_sleep_sum
    if (add_sleep_sum == "yes" && add_sleep == "no") {
      showNotification(
        "Sleep summary requires sleep status to be added in prhLink.
         Setting AddSleepSummary to 'no'.",
        type = "warning", duration = 6)
      add_sleep_sum <- "no"
    }
    
    prh_sum <- tryCatch(
      hypometrics::prhSummarise(
        DataFrame       = prh_linked,
        AddSleepSummary = add_sleep_sum
      ),
      error = function(e) {
        showNotification(paste0("prhSummarise error: ", e$message),
                         type = "error", duration = 8)
        NULL
      }
    )
    
    # prhVisualise requires the RAW uMotif segment data (raw_motif_segment)
    # which has columns: UserId, uMotifTime, SegmentId, Value
    vis_type <- if (!is.null(input$prh_vis_type)) input$prh_vis_type else "heatmap"
    
    prh_vis <- tryCatch({
      if (vis_type == "upset") {
        if (!requireNamespace("UpSetR", quietly = TRUE)) {
          showNotification(
            "UpSetR package is not installed. Install it with install.packages('UpSetR') then re-run.",
            type = "error", duration = 10)
          NULL
        } else {
          hypometrics::prhVisualise(
            DataFrame    = rv$motif,
            GraphType    = "upset",
            VisualiseAll = TRUE
          )
        }
      } else {
        hypometrics::prhVisualise(
          DataFrame    = rv$motif,
          GraphType    = "heatmap",
          VisualiseAll = TRUE
        )
      }
    }, error = function(e) {
      showNotification(paste("prhVisualise:", e$message), type="error"); NULL
    })
    
    list(motif_clean  =as.data.frame(motif_clean),
         checkin_clean=as.data.frame(checkin_clean),
         linked       =as.data.frame(prh_linked),
         summary      =prh_sum,
         vis          =prh_vis)
  })
  
  output$prh_motif_table   <- renderDT({ req(prh_results()$motif_clean);   mk_dt(prh_results()$motif_clean) })
  # PRH tables - rendered directly from prh_results() list
  output$prh_checkin_table <- renderDT({ req(prh_results()$checkin_clean); mk_dt(prh_results()$checkin_clean) })
  output$prh_linked_table  <- renderDT({ req(prh_results()$linked);        mk_dt(prh_results()$linked) })
  
  output$prh_summary_table <- renderDT({
    req(prh_results()$summary)
    display_df <- prh_results()$summary %>%
      rename("Participant"="id","Total PRH"="n_prh_all",
             "Symptomatic"="n_prh_symptomatic","Prevented"="n_prh_prevented",
             "Night"="n_prh_night","Day"="n_prh_day")
    datatable(display_df,options=list(pageLength=5,scrollX=FALSE),rownames=FALSE)
  })
  
  output$prh_plot <- renderPlotly({
    df <- prh_results()$summary; req(df)
    df_long <- df %>%
      select(id,n_prh_night,n_prh_day) %>%
      pivot_longer(-id,names_to="period",values_to="count") %>%
      mutate(period=recode(period,n_prh_night="Night",n_prh_day="Day"))
    p <- ggplot(df_long,aes(x=id,y=count,fill=period)) +
      geom_col(position="dodge",alpha=0.85) +
      scale_fill_manual(values=c("Night"="#2471A3","Day"=BRAND_RED)) +
      light_theme() +
      labs(x="Participant",y="Number of PRH episodes",fill="Period")
    ggplotly(p) %>% plotly_light()
  })
  
  # Dynamic height for PRH visualisation panel
  # UpSetR plots need more vertical space to be readable
  output$prh_vis_height_ui <- renderUI({
    # UpSetR text is fixed-size internally - making the plot taller and using
    # lower res (larger pixels) is the only way to make its labels more readable.
    h <- if (!is.null(input$prh_vis_type) && input$prh_vis_type == "upset") {
      "600px"
    } else {
      "440px"
    }
    tagList(
      plotOutput("prh_vis_plot", height = h),
      tags$div(style="margin-top:6px;", plot_download_btn("dl_png_prh_vis"))
    )
  })
  
  output$prh_vis_plot <- renderPlot({
    # React directly to vis type toggle so switching updates immediately
    req(rv$motif)
    vis_type <- if (!is.null(input$prh_vis_type)) input$prh_vis_type else "heatmap"
    
    if (vis_type == "heatmap") {
      vis <- tryCatch(
        hypometrics::prhVisualise(
          DataFrame    = rv$motif,
          GraphType    = "heatmap",
          VisualiseAll = TRUE
        ),
        error = function(e) {
          showNotification(paste("prhVisualise:", e$message), type="error")
          NULL
        }
      )
      req(vis)
      tryCatch(
        print(vis + theme(
          axis.text   = element_text(size = 12),
          axis.title  = element_text(size = 13),
          legend.text = element_text(size = 11)
        )),
        error = function(e) print(vis)
      )
    } else {
      # UpSetR upset plot
      # text.scale is not exposed through prhVisualise so we use width=900px
      # to give the fixed-size internal labels more room to breathe
      if (!requireNamespace("UpSetR", quietly = TRUE)) {
        showNotification(
          "UpSetR not installed. Run: install.packages('UpSetR') then re-run.",
          type = "error", duration = 10)
        return(NULL)
      }
      vis <- tryCatch(
        hypometrics::prhVisualise(
          DataFrame    = rv$motif,
          GraphType    = "upset",
          VisualiseAll = TRUE
        ),
        error = function(e) {
          showNotification(paste("prhVisualise (upset):", e$message), type="error")
          NULL
        }
      )
      req(vis)
      print(vis)
    }
  }, res = 96, width = 1200)
  
  output$prh_flags <- renderUI({
    df <- prh_results()$summary; req(df)
    high <- df %>% filter(n_prh_all>5)
    if (nrow(high)>0)
      warn_box(paste0("⚠ ",paste(high$id,collapse=", "),
                      " reported >5 PRH episodes during the study period."))
    else
      ok_box("✓ All participants reported ≤5 PRH episodes.")
  })
  
  output$dl_prh_motif    <- downloadHandler(filename=function() paste0("prh_motif_",  Sys.Date(),".csv"), content=function(f) write.csv(prh_results()$motif_clean,  f,row.names=FALSE))
  # Download handlers for all PRH output tables
  output$dl_prh_checkin  <- downloadHandler(filename=function() paste0("prh_checkin_",Sys.Date(),".csv"), content=function(f) write.csv(prh_results()$checkin_clean,f,row.names=FALSE))
  output$dl_prh_linked   <- downloadHandler(filename=function() paste0("prh_linked_", Sys.Date(),".csv"), content=function(f) write.csv(prh_results()$linked,       f,row.names=FALSE))
  output$dl_prh_summary  <- downloadHandler(filename=function() paste0("prh_summary_",Sys.Date(),".csv"), content=function(f) write.csv(prh_results()$summary,      f,row.names=FALSE))
  
  ### CGM-Sleep linkage tab
  ## cgmsleepLink() tags every CGM timestamp as Asleep, Awake or NA based on Fitbit sleep records
  ## cgmVisualise() with AddSleep="yes" overlays sleep periods as grey shaded regions on the glucose trace
  ## The TimeBreak and PageNumber controls allow full period, week-level or day-level views
  ## Day view uses HH:MM x-axis labels so the exact sleep and wake times are visible
  # Links CGM timestamps with Fitbit sleep status - output used for both the table and cgmVisualise plot
  link_results <- eventReactive(input$run_link, {
    req(rv$cgm, rv$sleep)
    linked <- tryCatch(
      hypometrics::cgmsleepLink(
        CgmDataFrame  =rv$cgm %>% filter(id %in% input$participant_filter),
        SleepDataFrame=rv$sleep
      ),
      error=function(e){ showNotification(paste("cgmsleepLink:",e$message),type="error"); NULL }
    )
    req(linked)
    
    overnight <- linked %>%
      filter(sleep_status=="Asleep") %>%
      mutate(night=as.Date(cgm_timestamp)) %>%
      group_by(id,night) %>%
      summarise(mean_glucose=round(mean(glucose,na.rm=TRUE),2),
                sd_glucose  =round(sd(glucose,  na.rm=TRUE),2),
                min_glucose =round(min(glucose, na.rm=TRUE),2),
                max_glucose =round(max(glucose, na.rm=TRUE),2),
                .groups="drop")
    
    sleep_out <- rv$sleep %>%
      mutate(night=as.Date(dateOfSleep)) %>%
      select(id,night,minutesAsleep,minutesAwake,timeInBed)
    
    list(linked   =linked,
         overnight=left_join(overnight,sleep_out,by=c("id","night")))
  })
  
  output$link_table <- renderDT({ req(link_results()$linked); mk_dt(link_results()$linked) })
  
  # cgmVisualise - single participant selected via dropdown
  output$cgm_sleep_vis_plot <- renderPlot({
    req(link_results()$linked, input$link_id)
    page_n <- if (!is.null(input$link_timebreak) && input$link_timebreak != "no") {
      if (!is.null(input$link_pagenum)) input$link_pagenum else 1
    } else NA_real_
    
    # cgmVisualise hardcodes its y-axis label, threshold lines (3.9/10) and tick breaks
    # (3, 3.9, 10, 15) assuming mmol/L input - it has no unit parameter at all. If the data
    # is actually mg/dL, convert it to mmol/L just for this call so those fixed reference
    # lines and axis breaks are meaningful, rather than being crushed near zero on an
    # axis scaled for values in the hundreds.
    vis_df <- link_results()$linked
    if (!is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL") {
      vis_df$glucose <- vis_df$glucose / 18.0182
    }
    
    p <- tryCatch(
      hypometrics::cgmVisualise(
        DataFrame =vis_df,
        StudyID   =input$link_id,
        TimeBreak =if (!is.null(input$link_timebreak)) input$link_timebreak else "no",
        PageNumber=page_n,
        AddSleep  ="yes"
      ),
      error=function(e){ showNotification(paste("cgmVisualise:",e$message),type="error"); NULL }
    )
    req(p)
    # For day view, show time on x-axis; for week/full period show dates
    tb <- if (!is.null(input$link_timebreak)) input$link_timebreak else "no"
    if (tb == "day") {
      x_breaks <- "2 hours"
      x_fmt    <- "%H:%M"
    } else {
      rng <- range(link_results()$linked$cgm_timestamp, na.rm = TRUE)
      x_breaks <- adaptive_date_breaks(rng[1], rng[2])
      x_fmt    <- "%d %b"
    }
    is_mgdl_link <- !is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL"
    print(
      p +
        scale_x_datetime(date_breaks = x_breaks, date_labels = x_fmt) +
        # The underlying data was converted to mmol/L above so cgmVisualise's hardcoded
        # threshold lines (3.9/10) land in the right place - this relabels the same axis
        # tick positions with their mg/dL equivalents so the displayed numbers match the
        # unit selected on the Data tab, without disturbing where the lines actually sit.
        { if (is_mgdl_link)
          list(scale_y_continuous(breaks = c(3, 3.9, 10, 15),
                                  labels = function(x) round(x * 18.0182)),
               ylab("Glucose (mg/dL)"))
        } +
        theme(
          axis.text.x  = element_text(size = 13, angle = 30, hjust = 1),
          axis.text.y  = element_text(size = 13),
          axis.title   = element_text(size = 14)
        )
    )
  })
  
  output$dl_link_table <- downloadHandler(
    filename=function() paste0("cgmsleep_linked_",Sys.Date(),".csv"),
    content=function(f) write.csv(link_results()$linked,f,row.names=FALSE))
  
  ### CGM-Activity linkage tab
  ## All three plots (glucose, steps, HR) are shown simultaneously for direct visual comparison
  ## Each plot calls cgmactivityLink() independently as the function only links one data type at a time
  ## A plain ggplot fallback renders daily totals if cgmactivityLink() fails (e.g. timestamp misalignment)
  ### CGM-Activity reactive - validates CGM data for the selected participant
  ## Each individual plot (glucose, steps, HR) then runs its own cgmactivityLink() call
  ## so all three always render together without needing a data type selection
  cgmact_results <- eventReactive(input$run_cgmact, {
    req(rv$cgm, input$cgmact_id)
    cgm_use <- rv$cgm %>%
      filter(id == input$cgmact_id) %>%
      filter(!is.na(cgm_timestamp) & is.finite(as.numeric(cgm_timestamp))) %>%
      arrange(cgm_timestamp)
    validate(need(nrow(cgm_use) > 0,
                  paste0("No CGM data found for participant ", input$cgmact_id, ".")))
    cgm_use
  })
  
  # Shared x-axis range computed once from CGM data so all three plots align
  # x_min floored to midnight of first day so all plots start at 00:00
  cgmact_x_range <- reactive({
    req(cgmact_results())
    xmin_raw <- min(cgmact_results()$cgm_timestamp, na.rm=TRUE)
    xmax_raw <- max(cgmact_results()$cgm_timestamp, na.rm=TRUE)
    list(
      xmin = as.POSIXct(format(xmin_raw, "%Y-%m-%d"), tz="UTC"),
      xmax = xmax_raw
    )
  })
  
  # Applies the Time resolution / Page number controls to compute which window of the full
  # period is currently visible, and the matching axis breaks/labels - shared by all three
  # plots (and their PNG downloads) so glucose, steps and HR always show the same window.
  cgmact_page_range <- reactive({
    full <- cgmact_x_range()
    tb   <- if (!is.null(input$cgmact_timebreak)) input$cgmact_timebreak else "no"
    page <- if (!is.null(input$cgmact_pagenum))   input$cgmact_pagenum   else 1
    
    if (tb == "day") {
      xmin <- full$xmin + (page - 1) * 86400
      xmax <- xmin + 86400
      x_breaks <- "2 hours"; x_fmt <- "%H:%M"
    } else if (tb == "week") {
      xmin <- full$xmin + (page - 1) * 7 * 86400
      xmax <- xmin + 7 * 86400
      x_breaks <- "1 day"; x_fmt <- "%d %b"
    } else {
      xmin <- full$xmin
      xmax <- full$xmax
      x_breaks <- adaptive_date_breaks(xmin, xmax); x_fmt <- "%d %b"
    }
    # Don't let a week/day page extend past the actual end of the data
    xmax <- min(xmax, full$xmax)
    
    list(xmin=xmin, xmax=xmax, x_breaks=x_breaks, x_fmt=x_fmt)
  })
  
  ## Shared step/HR data for the current participant - filtered, validated and sorted once
  ## here rather than independently in each of the four places that need it (live step plot,
  ## live HR plot, the combined 3-plot download, and the individual PNG downloads). Being a
  ## plain reactive(), Shiny caches the result and only recomputes when rv$step/input$cgmact_id
  ## actually change, so switching tabs or re-rendering the same participant is free.
  cgmact_step_data <- reactive({
    req(input$cgmact_id)
    step_df <- if (!is.null(rv$step)) rv$step %>% filter(id == input$cgmact_id) else NULL
    validate(need(!is.null(step_df) && nrow(step_df) > 0,
                  "No step data for this participant."))
    step_df %>%
      filter(!is.na(step_timestamp) & is.finite(as.numeric(step_timestamp))) %>%
      arrange(step_timestamp)
  })
  
  cgmact_hr_data <- reactive({
    req(input$cgmact_id)
    hr_df <- if (!is.null(rv$hr)) rv$hr %>% filter(id == input$cgmact_id) else NULL
    validate(need(!is.null(hr_df) && nrow(hr_df) > 0,
                  "No heart rate data for this participant."))
    hr_df %>%
      filter(!is.na(hr_timestamp) & is.finite(as.numeric(hr_timestamp))) %>%
      arrange(hr_timestamp)
  })
  
  output$cgmact_cgm_plot <- renderPlotly({
    df <- cgmact_results()
    validate(need(!is.null(df) && nrow(df) > 0, "No CGM data available."))
    df <- df[!is.na(df$cgm_timestamp), ]
    is_mgdl <- !is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL"
    # 70 mg/dL = the standard clinical Level 1 hypoglycaemia threshold, equivalent to 3.9 mmol/L -
    # this was previously hardcoded to 3.9 regardless of unit, so on mg/dL data (values in the
    # hundreds) the line was crushed near zero and effectively invisible.
    hypo_line_act <- if (is_mgdl) 70 else 3.9
    p <- ggplot(df, aes(x=cgm_timestamp, y=glucose)) +
      geom_line(colour="#1B3A6B", linewidth=0.6, na.rm=TRUE) +
      geom_hline(yintercept=hypo_line_act, linetype="dashed", colour="#2471A3") +
      scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                       limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
      light_theme() +
      theme(axis.text.x  = element_text(angle=30, hjust=1, size=11),
            axis.text.y  = element_text(size=11),
            axis.title   = element_text(size=12),
            plot.title   = element_text(size=13)) +
      labs(x=NULL,
           y=if(is_mgdl) "Glucose (mg/dL)" else "Glucose (mmol/L)",
           title=paste0("CGM - ", input$cgmact_id))
    ggplotly(p) %>% plotly_light()
  })
  
  # Steps plot - shown alongside the glucose and heart rate plots for direct comparison
  output$cgmact_step_plot <- renderPlotly({
    req(cgmact_results())
    # Need step-linked data specifically
    cgm_use <- cgmact_results()
    step_df <- cgmact_step_data()
    linked_s <- tryCatch(
      hypometrics::cgmactivityLink(CgmDataFrame=cgm_use,
                                   ActivityDataFrame=step_df,
                                   DataType="stepcount"),
      error=function(e){ showNotification(paste("cgmactivityLink (steps):", e$message), type="error", duration=10); NULL }
    )
    validate(need(!is.null(linked_s) && "step_count" %in% names(linked_s),
                  "Could not link step data."))
    df <- linked_s[!is.na(linked_s$cgm_timestamp),]
    p <- ggplot(df, aes(x=cgm_timestamp, y=step_count)) +
      geom_line(colour="#2471A3", linewidth=0.6, na.rm=TRUE) +
      scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                       limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
      light_theme() +
      theme(axis.text.x  = element_text(angle=30, hjust=1, size=11),
            axis.text.y  = element_text(size=11),
            axis.title   = element_text(size=12),
            plot.title   = element_text(size=13)) +
      labs(x=NULL, y="Steps (per 5-min)",
           title=paste0("Steps - ", input$cgmact_id))
    ggplotly(p) %>% plotly_light()
  })
  
  # HR plot - shown alongside the glucose and step plots for direct comparison
  output$cgmact_hr_plot <- renderPlotly({
    req(cgmact_results())
    cgm_use <- cgmact_results()
    hr_df <- cgmact_hr_data()
    linked_h <- tryCatch(
      hypometrics::cgmactivityLink(CgmDataFrame=cgm_use,
                                   ActivityDataFrame=hr_df,
                                   DataType="heartrate"),
      error=function(e){ showNotification(paste("cgmactivityLink (heart rate):", e$message), type="error", duration=10); NULL }
    )
    validate(need(!is.null(linked_h) && "heart_rate" %in% names(linked_h),
                  "Could not link heart rate data."))
    df <- linked_h[!is.na(linked_h$cgm_timestamp),]
    p <- ggplot(df, aes(x=cgm_timestamp, y=heart_rate)) +
      geom_line(colour="#E8836D", linewidth=0.6, na.rm=TRUE) +
      scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                       limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
      light_theme() +
      theme(axis.text.x  = element_text(angle=30, hjust=1, size=11),
            axis.text.y  = element_text(size=11),
            axis.title   = element_text(size=12),
            plot.title   = element_text(size=13)) +
      labs(x=NULL, y="Heart rate (bpm)",
           title=paste0("Heart rate - ", input$cgmact_id))
    ggplotly(p) %>% plotly_light()
  })
  
  output$cgmact_table <- renderDT({ req(cgmact_results()); mk_dt(cgmact_results()) })
  # Download handler for the CGM-Activity linked data table
  output$dl_cgmact_table <- downloadHandler(
    filename=function() paste0("cgmactivity_cgm_",Sys.Date(),".csv"),
    content=function(f) write.csv(as.data.frame(cgmact_results()),f,row.names=FALSE))
  
  # Combined download for all three CGM-Activity plots stacked vertically
  output$dl_png_cgmact_all <- downloadHandler(
    filename = function() paste0("cgmactivity_all_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$cgm, rv$step, rv$hr, input$cgmact_id)
      pid    <- input$cgmact_id
      disp_u <- if (!is.null(input$global_glucose_unit)) input$global_glucose_unit else "mmol/L"
      y_lab  <- if (disp_u == "mg/dL") "Glucose (mg/dL)" else "Glucose (mmol/L)"
      
      # Reuse the same cached, filtered data as the live plots rather than re-deriving it
      cgm_dl <- cgmact_results()
      validate(need(nrow(cgm_dl) > 0, paste0("No CGM data for ", pid)))
      
      # Use the same page window as the interactive plots so the download always matches
      x_min <- cgmact_page_range()$xmin
      x_max <- cgmact_page_range()$xmax
      x_brk <- cgmact_page_range()$x_breaks
      x_fmt <- cgmact_page_range()$x_fmt
      
      p1 <- ggplot(cgm_dl, aes(x=cgm_timestamp, y=glucose)) +
        geom_line(colour="#1B3A6B", linewidth=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=x_brk, date_labels=x_fmt,
                         limits=c(x_min, x_max)) +
        light_theme() +
        labs(x=NULL, y=y_lab, title=paste0("CGM - ", pid))
      
      step_df <- rv$step %>% filter(id==pid) %>%
        filter(!is.na(step_timestamp) & is.finite(as.numeric(step_timestamp))) %>%
        arrange(step_timestamp)
      linked_s <- tryCatch(
        hypometrics::cgmactivityLink(CgmDataFrame=cgm_dl,
                                     ActivityDataFrame=step_df, DataType="stepcount"),
        error=function(e) NULL)
      p2 <- if (!is.null(linked_s) && "step_count" %in% names(linked_s)) {
        ggplot(linked_s[!is.na(linked_s$cgm_timestamp),], aes(x=cgm_timestamp, y=step_count)) +
          geom_line(colour="#2471A3", linewidth=0.6, na.rm=TRUE) +
          scale_x_datetime(date_breaks=x_brk, date_labels=x_fmt,
                           limits=c(x_min, x_max)) +
          light_theme() +
          labs(x=NULL, y="Steps (per 5-min)", title=paste0("Steps - ", pid))
      } else {
        ggplot() + annotate("text",x=0,y=0,label="Step data unavailable") + light_theme()
      }
      
      hr_df <- rv$hr %>% filter(id==pid) %>%
        filter(!is.na(hr_timestamp) & is.finite(as.numeric(hr_timestamp))) %>%
        arrange(hr_timestamp)
      linked_h <- tryCatch(
        hypometrics::cgmactivityLink(CgmDataFrame=cgm_dl,
                                     ActivityDataFrame=hr_df, DataType="heartrate"),
        error=function(e) NULL)
      p3 <- if (!is.null(linked_h) && "heart_rate" %in% names(linked_h)) {
        ggplot(linked_h[!is.na(linked_h$cgm_timestamp),], aes(x=cgm_timestamp, y=heart_rate)) +
          geom_line(colour="#E8836D", linewidth=0.6, na.rm=TRUE) +
          scale_x_datetime(date_breaks=x_brk, date_labels=x_fmt,
                           limits=c(x_min, x_max)) +
          light_theme() +
          labs(x=NULL, y="Heart rate (bpm)", title=paste0("Heart rate - ", pid))
      } else {
        ggplot() + annotate("text",x=0,y=0,label="HR data unavailable") + light_theme()
      }
      
      # Stack all three plots with a single shared x-axis at the bottom only.
      # Top two plots have their x-axis text and title hidden so only the
      # bottom plot (HR) shows dates, giving a clean single x-axis appearance.
      p1_no_x <- p1 + theme(
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      )
      p2_no_x <- p2 + theme(
        axis.text.x  = element_blank(),
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank()
      )
      
      combined <- tryCatch({
        if (requireNamespace("patchwork", quietly=TRUE)) {
          patchwork::wrap_plots(p1_no_x, p2_no_x, p3, ncol=1) +
            patchwork::plot_layout(heights=c(1, 1, 1.2))
        } else if (requireNamespace("gridExtra", quietly=TRUE)) {
          gridExtra::arrangeGrob(p1_no_x, p2_no_x, p3, ncol=1)
        } else {
          p1
        }
      }, error=function(e) p1)
      
      ggsave(file, plot=combined, width=14, height=12, dpi=150, bg="white", device="png")
    }
  )
  
  ### CGM-PRH linkage tab
  ## Requires the PRH tab to have been run first so rv$prh_linked_data is populated
  ## One separate plot is generated per participant so glucose traces are never overlaid
  ## PRH episodes are marked as black triangles using scale_shape_manual so they appear correctly in the legend
  # Links CGM timestamps with PRH episodes - requires the PRH tab to have been run first
  cgmprh_results <- eventReactive(input$run_cgmprh, {
    validate(need(!is.null(rv$prh_linked_data),
                  "Please run the Person-Reported Hypoglycaemia tab first."))
    linked <- tryCatch(
      hypometrics::cgmprhLink(
        CgmDataFrame=rv$cgm %>% filter(id %in% input$participant_filter),
        PrhDataFrame=rv$prh_linked_data
      ),
      error=function(e){ showNotification(paste("cgmprhLink:",e$message),type="error"); NULL }
    )
    req(linked); linked
  })
  
  output$cgmprh_table <- renderDT({
    req(cgmprh_results()); mk_dt(as.data.frame(cgmprh_results()))
  })
  
  # Participant dropdown for CGM-PRH tab - populated from cgmprh_results
  output$cgmprh_id_ui <- renderUI({
    req(cgmprh_results())
    ids <- unique(as.data.frame(cgmprh_results())$id)
    selectInput("cgmprh_id", NULL, choices=ids, selected=ids[1])
  })
  
  output$cgmprh_plot <- renderUI({
    df <- as.data.frame(cgmprh_results())
    req("cgm_timestamp" %in% names(df), "glucose" %in% names(df))
    
    # Filter to selected participant only
    req(input$cgmprh_id)
    df <- df %>% filter(id == input$cgmprh_id)
    
    disp_unit_prh <- if (!is.null(input$global_glucose_unit)) input$global_glucose_unit else "mmol/L"
    hypo_line <- if (disp_unit_prh == "mmol/L") 3.9 else 70
    y_lab_prh <- if (disp_unit_prh == "mmol/L") "Glucose (mmol/L)" else "Glucose (mg/dL)"
    
    prh_col <- intersect(c("checkin_prh_timestamp","motif_prh_timestamp"),names(df))[1]
    df$has_prh <- if (!is.na(prh_col)) !is.na(df[[prh_col]]) else FALSE
    
    # Apply time resolution paging (same logic as CGM-Sleep tab)
    tb <- if (!is.null(input$cgmprh_timebreak)) input$cgmprh_timebreak else "no"
    if (tb != "no") {
      page_n <- if (!is.null(input$cgmprh_pagenum)) input$cgmprh_pagenum else 1
      all_dates <- sort(unique(as.Date(df$cgm_timestamp)))
      if (tb == "week") {
        weeks <- split(all_dates, ceiling(seq_along(all_dates)/7))
        wk <- weeks[[min(page_n, length(weeks))]]
        df <- df %>% filter(as.Date(cgm_timestamp) %in% wk)
      } else if (tb == "day") {
        day_sel <- all_dates[min(page_n, length(all_dates))]
        df <- df %>% filter(as.Date(cgm_timestamp) == day_sel)
      }
    }
    validate(need(nrow(df) > 0, "No data for selected participant/page."))
    
    participants <- unique(df$id)
    # One plotlyOutput per participant (now just one since filtered above)
    plot_list <- lapply(seq_along(participants), function(i) {
      pid    <- participants[i]
      colour <- participant_colours[((i-1) %% length(participant_colours)) + 1]
      pid_df <- df %>% filter(id == pid)
      prh_df <- pid_df %>% filter(has_prh)
      
      output_id <- paste0("cgmprh_plot_", gsub("[^A-Za-z0-9]", "_", pid))
      
      output[[output_id]] <- renderPlotly({
        # Add a dummy PRH column to the main df so the legend entry appears
        # even when overlaid on the continuous glucose line
        pid_df2 <- pid_df %>%
          mutate(prh_episode = ifelse(has_prh, glucose, NA_real_))
        
        p <- ggplot(pid_df2, aes(x=cgm_timestamp)) +
          # Glucose trace in participant colour
          geom_line(aes(y=glucose), colour=colour, linewidth=0.6, na.rm=TRUE) +
          # Hypo threshold line
          geom_hline(yintercept=hypo_line, linetype="dashed",
                     colour="#2471A3", linewidth=0.5) +
          # PRH episode triangles - black, smaller
          # text aesthetic carries the PRH timestamp into the plotly hover tooltip
          geom_point(
            data = pid_df2 %>% filter(!is.na(prh_episode)),
            aes(y=prh_episode, shape="PRH episode",
                text=paste0("PRH timestamp: ", format(cgm_timestamp, "%d %b %Y %H:%M"))),
            colour="#000000", size=3, stroke=1
          ) +
          scale_shape_manual(
            name   = NULL,
            values = c("PRH episode" = 17),
            labels = c("PRH episode" = "▲ PRH episode")
          ) +
          # x-axis format adapts to the selected time resolution:
          # daily view shows HH:MM times, weekly/full shows dates
          scale_x_datetime(
            date_breaks = if (tb == "day") "2 hours"
                         else adaptive_date_breaks(min(pid_df2$cgm_timestamp, na.rm=TRUE),
                                                   max(pid_df2$cgm_timestamp, na.rm=TRUE)),
            date_labels = if (tb == "day") "%H:%M" else "%d %b"
          ) +
          light_theme() +
          theme(
            axis.text.x  = element_text(angle=30, hjust=1),
            legend.position = "top",
            legend.text  = element_text(size=13, face="bold")
          ) +
          labs(x=NULL, y=y_lab_prh,
               title=paste0("Participant: ", pid,
                            if (tb != "no") paste0(" - page ", if (!is.null(input$cgmprh_pagenum)) input$cgmprh_pagenum else 1) else ""))
        
        # tooltip="text" ensures the PRH timestamp appears in the hover box
        ggplotly(p, tooltip=c("x","y","text")) %>% plotly_light()
      })
      
      dl_id <- paste0("dl_png_cgmprh_", gsub("[^A-Za-z0-9]", "_", pid))
      
      output[[dl_id]] <- downloadHandler(
        filename = function() paste0("cgmprh_", pid, "_", Sys.Date(), ".png"),
        content  = function(file) {
          df_dl <- as.data.frame(cgmprh_results()) %>% filter(id == pid)
          prh_col_dl <- intersect(c("checkin_prh_timestamp","motif_prh_timestamp"),
                                  names(df_dl))[1]
          df_dl$has_prh <- if (!is.na(prh_col_dl)) !is.na(df_dl[[prh_col_dl]]) else FALSE
          disp_unit_dl <- if (!is.null(input$global_glucose_unit))
            input$global_glucose_unit else "mmol/L"
          hypo_dl <- if (disp_unit_dl == "mmol/L") 3.9 else 70
          y_lab_dl <- if (disp_unit_dl == "mmol/L") "Glucose (mmol/L)" else "Glucose (mg/dL)"
          p_dl <- ggplot(df_dl, aes(x=cgm_timestamp)) +
            geom_line(aes(y=glucose), colour=colour, linewidth=0.6, na.rm=TRUE) +
            geom_hline(yintercept=hypo_dl, linetype="dashed",
                       colour="#2471A3", linewidth=0.5) +
            { if (any(df_dl$has_prh))
              geom_point(data=df_dl %>% filter(has_prh),
                         aes(y=glucose, shape="PRH episode"),
                         colour="#000000", size=4, stroke=1.5)
              else list() } +
            scale_shape_manual(name=NULL, values=c("PRH episode"=17)) +
            scale_x_datetime(date_breaks=adaptive_date_breaks(min(df_dl$cgm_timestamp, na.rm=TRUE),
                                                              max(df_dl$cgm_timestamp, na.rm=TRUE)),
                             date_labels="%d %b") +
            light_theme() +
            theme(axis.text.x=element_text(angle=30, hjust=1),
                  legend.position="top") +
            labs(x=NULL, y=y_lab_dl, title=paste0("Participant: ", pid))
          ggsave(file, plot=p_dl, width=12, height=4, dpi=150, bg="white")
        }
      )
      
      tagList(
        div(style="margin-top:18px;",
            card(
              card_header(paste0("CGM trace with PRH episodes - ", pid)),
              plotlyOutput(output_id, height="300px"),
              tags$div(style="margin-top:6px;", downloadButton(dl_id, "Download PNG",
                                                               icon=icon("image"), class="btn-outline-secondary btn-sm mt-2"))
            )
        )
      )
    })
    
    tagList(plot_list)
  })
  
  output$dl_cgmprh_table <- downloadHandler(
    filename=function() paste0("cgmprh_linked_",Sys.Date(),".csv"),
    content=function(f) write.csv(as.data.frame(cgmprh_results()),f,row.names=FALSE))
  
  ### PNG download handlers for all plots across all tabs
  ## Base R plots are re-rendered into a png() device
  ## Plotly plots are rebuilt as ggplot and saved with ggsave() - more reliable than webshot2
  ## which requires an external Chrome installation that users may not have
  
  ## Helper function for saving ggplot objects to PNG with consistent dimensions
  save_gg <- function(file, p, width=12, height=5, dpi=150) {
    ggsave(file, plot=p, width=width, height=height, dpi=dpi,
           bg="white", device="png")
  }
  
  # CGM missingness plot
  output$dl_png_cgm_miss <- downloadHandler(
    filename = function() paste0("cgm_missingness_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$cgm)
      df <- rv$cgm
      if (!is.null(input$cgm_miss_id) && input$cgm_miss_id != "__all__")
        df <- df %>% filter(id == input$cgm_miss_id)
      df2 <- df %>% arrange(id, cgm_timestamp) %>%
        group_by(id) %>% mutate(is_na=is.na(glucose)) %>% ungroup()
      gap_ts <- df2 %>%
        group_by(id) %>%
        mutate(gap_id=cumsum(c(0, diff(is_na) != 0))) %>%
        filter(is_na) %>%
        group_by(id, gap_id) %>%
        summarise(xmin=min(cgm_timestamp), xmax=max(cgm_timestamp), .groups="drop")
      y_lab <- if (!is.null(input$global_glucose_unit) &&
                   input$global_glucose_unit == "mg/dL") "Glucose (mg/dL)" else "Glucose (mmol/L)"
      p <- ggplot(df2, aes(x=cgm_timestamp, y=glucose)) +
        { if (nrow(gap_ts) > 0)
          geom_rect(data=gap_ts, aes(xmin=xmin, xmax=xmax, ymin=-Inf, ymax=Inf),
                    fill="#E8836D", alpha=0.6, inherit.aes=FALSE)
          else list() } +
        geom_line(colour="#2471A3", linewidth=0.4, na.rm=TRUE, alpha=0.8) +
        geom_point(data=df2 %>% filter(!is.na(glucose)),
                   aes(x=cgm_timestamp, y=glucose),
                   colour="#2471A3", size=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=adaptive_date_breaks(min(df2$cgm_timestamp, na.rm=TRUE),
                                                          max(df2$cgm_timestamp, na.rm=TRUE)),
                         date_labels="%d %b") +
        facet_wrap(~id, ncol=1, scales="fixed") +
        light_theme() +
        labs(x=NULL, y=y_lab,
             caption="Orange/red shaded bars = missing data gaps")
      save_gg(file, p, width=14, height=5)
    }
  )
  
  # CGM glucose trace (rebuilt as ggplot)
  output$dl_png_cgm_trace <- downloadHandler(
    filename = function() paste0("cgm_trace_", Sys.Date(), ".png"),
    content  = function(file) {
      req(cgm_results()$interp, input$cgm_trace_id)
      sel_id <- input$cgm_trace_id
      df <- cgm_results()$interp %>% filter(id == sel_id)
      disp_unit <- if (!is.null(input$global_glucose_unit))
        input$global_glucose_unit else "mmol/L"
      if (disp_unit == "mmol/L") {
        th_vh <- 13.9; th_h <- 10.0; th_l <- 3.9; th_vl <- 3.0
        y_lab <- "Glucose (mmol/L)"
      } else {
        th_vh <- 250; th_h <- 180; th_l <- 70; th_vl <- 54
        y_lab <- "Glucose (mg/dL)"
      }
      p <- ggplot(df, aes(x=cgm_timestamp, y=glucose)) +
        geom_hline(yintercept=th_vh, colour="#E67E22", linewidth=0.8) +
        geom_hline(yintercept=th_h,  colour="#F4D03F", linewidth=0.8) +
        geom_hline(yintercept=th_l,  colour="#C0392B", linewidth=0.8) +
        geom_hline(yintercept=th_vl, colour="#7B241C", linewidth=0.8) +
        geom_line(colour="#1B3A6B", linewidth=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=adaptive_date_breaks(min(df$cgm_timestamp, na.rm=TRUE),
                                                          max(df$cgm_timestamp, na.rm=TRUE)),
                         date_labels="%d %b") +
        scale_y_continuous(limits=c(min(df$glucose, na.rm=TRUE) - 1,
                                    th_vh + 2)) +
        light_theme() +
        labs(x=NULL, y=y_lab, title=paste0("Participant: ", sel_id))
      save_gg(file, p, width=14, height=6)
    }
  )
  
  # Activity step count plot
  output$dl_png_act_step <- downloadHandler(
    filename = function() paste0("activity_steps_", Sys.Date(), ".png"),
    content  = function(file) {
      req(activity_results())
      r  <- activity_results()
      df <- r$step %>% filter(id == r$id) %>%
        mutate(date=as.Date(step_timestamp)) %>%
        group_by(date) %>%
        summarise(steps=sum(count, na.rm=TRUE), .groups="drop")
      p <- ggplot(df, aes(x=date, y=steps)) +
        geom_col(fill=BRAND_RED, alpha=0.8) +
        scale_x_date(date_breaks=adaptive_date_breaks(min(df$date), max(df$date)), date_labels="%d %b") +
        light_theme() +
        labs(x=NULL, y="Daily steps",
             title=paste0("Step count - Participant: ", r$id))
      save_gg(file, p, width=14, height=5)
    }
  )
  
  # Activity heart rate plot
  output$dl_png_act_hr <- downloadHandler(
    filename = function() paste0("activity_hr_", Sys.Date(), ".png"),
    content  = function(file) {
      req(activity_results())
      r  <- activity_results()
      df <- r$hr %>% filter(id == r$id) %>%
        mutate(date=as.Date(hr_timestamp)) %>%
        group_by(date) %>%
        summarise(mean_hr=mean(heart_rate, na.rm=TRUE), .groups="drop")
      p <- ggplot(df, aes(x=date, y=mean_hr)) +
        geom_line(colour="#E8836D", linewidth=0.8) +
        geom_point(colour="#E8836D", size=2) +
        scale_x_date(date_breaks=adaptive_date_breaks(min(df$date), max(df$date)), date_labels="%d %b") +
        light_theme() +
        labs(x=NULL, y="Mean daily heart rate (bpm)",
             title=paste0("Heart rate - Participant: ", r$id))
      save_gg(file, p, width=14, height=5)
    }
  )
  
  # Sleep summary bar chart
  output$dl_png_sleep_plot <- downloadHandler(
    filename = function() paste0("sleep_summary_", Sys.Date(), ".png"),
    content  = function(file) {
      req(sleep_results()$summary)
      df <- sleep_results()$summary
      threshold <- if (!is.null(input$sleep_threshold)) input$sleep_threshold else 6
      df_long <- df %>%
        select(id, `Avg asleep (h)`=avg_asleep_h,
               `Avg awake (h)`=avg_awake_h,
               `Avg in bed (h)`=avg_inbed_h) %>%
        tidyr::pivot_longer(-id, names_to="metric", values_to="hours")
      p <- ggplot(df_long, aes(x=metric, y=hours, fill=id)) +
        geom_col(position="dodge") +
        geom_hline(yintercept=threshold, linetype="dashed",
                   colour="#e07b54", linewidth=1) +
        annotate("text", x=0.5, y=threshold+0.15,
                 label=paste0(threshold, "h threshold"),
                 colour="#e07b54", hjust=0, size=5, fontface="bold") +
        scale_fill_manual(values=participant_colours) +
        light_theme() +
        labs(x=NULL, y="Hours", fill="Participant")
      save_gg(file, p, width=10, height=6)
    }
  )
  
  # Sleep onset/offset visualisation (base R package plot - use png device)
  output$dl_png_sleep_vis <- downloadHandler(
    filename = function() paste0("sleep_visualise_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$sleep)
      vis <- tryCatch(
        hypometrics::sleepVisualise(
          DataFrame    = rv$sleep,
          VisualiseAll = isTRUE(input$sleep_vis_all),
          StudyID      = if (!isTRUE(input$sleep_vis_all) &&
                             !is.null(input$sleep_id)) input$sleep_id else ""
        ),
        error = function(e) NULL
      )
      req(vis)
      png(file, width=1400, height=700, res=120)
      par(cex=1.4, cex.axis=1.3, cex.lab=1.4)
      print(vis)
      dev.off()
    }
  )
  
  # PRH day vs night bar chart
  output$dl_png_prh_daynight <- downloadHandler(
    filename = function() paste0("prh_daynight_", Sys.Date(), ".png"),
    content  = function(file) {
      req(prh_results()$linked)
      df <- prh_results()$linked %>%
        group_by(id, night_status) %>%
        summarise(n=n(), .groups="drop") %>%
        rename(Period=night_status)
      p <- ggplot(df, aes(x=id, y=n, fill=Period)) +
        geom_col(position="dodge") +
        scale_fill_manual(values=c("Day"="#B5280D","Night"="#2471A3")) +
        light_theme() +
        labs(x="Participant", y="Number of PRH episodes")
      save_gg(file, p, width=8, height=5)
    }
  )
  
  # PRH symptom visualisation (base R package plot)
  output$dl_png_prh_vis <- downloadHandler(
    filename = function() paste0("prh_visualise_", Sys.Date(), ".png"),
    content  = function(file) {
      req(rv$motif)
      vis_type <- if (!is.null(input$prh_vis_type)) input$prh_vis_type else "heatmap"
      vis <- tryCatch(
        hypometrics::prhVisualise(
          DataFrame    = rv$motif,
          GraphType    = vis_type,
          VisualiseAll = TRUE
        ),
        error = function(e) NULL
      )
      req(vis)
      if (vis_type == "heatmap") {
        p <- tryCatch(
          vis + theme(axis.text=element_text(size=12),
                      axis.title=element_text(size=13)),
          error = function(e) vis
        )
        save_gg(file, p, width=10, height=7)
      } else {
        png(file, width=1400, height=800, res=96)
        print(vis)
        dev.off()
      }
    }
  )
  
  # CGM-Sleep glucose trace with sleep periods
  output$dl_png_cgm_sleep <- downloadHandler(
    filename = function() paste0("cgm_sleep_", Sys.Date(), ".png"),
    content  = function(file) {
      req(link_results()$linked, input$link_id)
      page_n <- if (!is.null(input$link_timebreak) && input$link_timebreak != "no") {
        if (!is.null(input$link_pagenum)) input$link_pagenum else 1
      } else NA_real_
      # Same mg/dL -> mmol/L conversion as the on-screen plot - see note there for why
      vis_df <- link_results()$linked
      if (!is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL") {
        vis_df$glucose <- vis_df$glucose / 18.0182
      }
      p <- tryCatch(
        hypometrics::cgmVisualise(
          DataFrame  = vis_df,
          StudyID    = input$link_id,
          TimeBreak  = if (!is.null(input$link_timebreak)) input$link_timebreak else "no",
          PageNumber = page_n,
          AddSleep   = "yes"
        ),
        error = function(e) NULL
      )
      req(p)
      tb <- if (!is.null(input$link_timebreak)) input$link_timebreak else "no"
      x_fmt <- if (tb == "day") "%H:%M" else "%d %b"
      x_brk <- if (tb == "day") "2 hours" else {
        rng <- range(vis_df$cgm_timestamp, na.rm = TRUE)
        adaptive_date_breaks(rng[1], rng[2])
      }
      is_mgdl_dl <- !is.null(input$global_glucose_unit) && input$global_glucose_unit == "mg/dL"
      p <- p + scale_x_datetime(date_breaks=x_brk, date_labels=x_fmt) +
        { if (is_mgdl_dl)
          list(scale_y_continuous(breaks = c(3, 3.9, 10, 15),
                                  labels = function(x) round(x * 18.0182)),
               ylab("Glucose (mg/dL)"))
        } +
        theme(axis.text.x=element_text(size=13, angle=30, hjust=1),
              axis.text.y=element_text(size=13))
      png(file, width=1600, height=600, res=120)
      print(p)
      dev.off()
    }
  )
  
  # CGM-Activity glucose trace
  output$dl_png_cgmact_cgm <- downloadHandler(
    filename = function() paste0("cgmact_glucose_", Sys.Date(), ".png"),
    content  = function(file) {
      req(cgmact_results(), input$cgmact_id)
      df <- cgmact_results()
      disp_unit <- if (!is.null(input$global_glucose_unit))
        input$global_glucose_unit else "mmol/L"
      y_lab <- if (disp_unit == "mg/dL") "Glucose (mg/dL)" else "Glucose (mmol/L)"
      p <- ggplot(df, aes(x=cgm_timestamp, y=glucose)) +
        geom_line(colour="#1B3A6B", linewidth=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                         limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
        light_theme() +
        labs(x=NULL, y=y_lab,
             title=paste0("CGM - ", input$cgmact_id))
      save_gg(file, p, width=14, height=5)
    }
  )
  
  # CGM-Activity step count trace
  output$dl_png_cgmact_step <- downloadHandler(
    filename = function() paste0("cgmact_steps_", Sys.Date(), ".png"),
    content  = function(file) {
      req(cgmact_results(), rv$step, input$cgmact_id)
      step_df <- cgmact_step_data()
      cgm_use <- cgmact_results()
      linked_s <- tryCatch(
        hypometrics::cgmactivityLink(CgmDataFrame=cgm_use,
                                     ActivityDataFrame=step_df,
                                     DataType="stepcount"),
        error=function(e) NULL
      )
      req(linked_s, "step_count" %in% names(linked_s))
      df <- linked_s[!is.na(linked_s$cgm_timestamp), ]
      p <- ggplot(df, aes(x=cgm_timestamp, y=step_count)) +
        geom_line(colour="#2471A3", linewidth=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                         limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
        light_theme() +
        labs(x=NULL, y="Step count (per 5-min)",
             title=paste0("Steps - ", input$cgmact_id))
      save_gg(file, p, width=14, height=5)
    }
  )
  
  # CGM-Activity heart rate trace
  output$dl_png_cgmact_hr <- downloadHandler(
    filename = function() paste0("cgmact_hr_", Sys.Date(), ".png"),
    content  = function(file) {
      req(cgmact_results(), rv$hr, input$cgmact_id)
      hr_df <- cgmact_hr_data()
      cgm_use <- cgmact_results()
      linked_h <- tryCatch(
        hypometrics::cgmactivityLink(CgmDataFrame=cgm_use,
                                     ActivityDataFrame=hr_df,
                                     DataType="heartrate"),
        error=function(e) NULL
      )
      req(linked_h, "heart_rate" %in% names(linked_h))
      df <- linked_h[!is.na(linked_h$cgm_timestamp), ]
      p <- ggplot(df, aes(x=cgm_timestamp, y=heart_rate)) +
        geom_line(colour="#E8836D", linewidth=0.6, na.rm=TRUE) +
        scale_x_datetime(date_breaks=cgmact_page_range()$x_breaks, date_labels=cgmact_page_range()$x_fmt,
                         limits=c(cgmact_page_range()$xmin, cgmact_page_range()$xmax)) +
        light_theme() +
        labs(x=NULL, y="Heart rate (bpm)",
             title=paste0("Heart rate - ", input$cgmact_id))
      save_gg(file, p, width=14, height=5)
    }
  )
  
}

##### Launching the application
shinyApp(ui, server)