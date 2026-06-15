# =============================================================
# oncoscopR Shiny app
# Audit / live evaluation dashboard for haematological tumour centres
# =============================================================
#
# UI is the verbatim layout from the v5 single-file app, with TWO sanctioned
# additions in the sidebar:
#   - data-source controls (cohort upload, optional tumorboard CSV upload,
#     example-data button)
#   - replacement content for the `path_info` slot (same widget id, new text)
#
# All backend logic is in oncoscopR:: — see ?onc_run_app and ?onc_example_path.

ui <- shiny::fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  title = "Hämatologisches Tumorzentrum – Auditor-Auswertung",
  bslib::layout_sidebar(
    sidebar = bslib::sidebar(
      width = 390,
      shiny::h4("Auditor-App"),

      # --- sanctioned addition: data source -----------------------------
      bslib::card(
        bslib::card_header("Datenquelle"),
        shiny::fileInput(
          "cohort_file",
          "Kohorten-Excel (.xlsx) hochladen",
          accept = c(".xlsx", ".xls",
                     "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet")
        ),
        shiny::fileInput(
          "tumorboard_file",
          "Optional: Tumorboardbeschlüsse CSV laden",
          accept = c(".csv", "text/csv")
        ),
        shiny::actionButton(
          "load_example",
          "Beispieldaten laden",
          class = "btn-secondary"
        )
      ),
      # ------------------------------------------------------------------

      shiny::verbatimTextOutput("path_info"),
      shiny::actionButton("reload", "Daten neu einlesen", class = "btn-primary"),
      shiny::br(), shiny::br(),
      shiny::helpText(
        "Datenquelle: hochgeladene Excel-Datei oder bundle-Beispieldaten. ",
        "Keine Daten werden außerhalb der Session gespeichert."
      ),
      shiny::hr(),
      shiny::h5("Globale Filter"),
      shiny::uiOutput("global_filters"),
      shiny::hr(),
      shiny::h5("Freitextsuche"),
      shiny::textInput("search_text",
                       "Suche in Name, Diagnose, Kodierung, Therapie",
                       value = ""),
      shiny::hr(),
      shiny::downloadButton("download_filtered",
                            "Gefilterte Patiententabelle CSV")
    ),
    shiny::mainPanel(
      shiny::tabsetPanel(
        shiny::tabPanel(
          "Auditor-Dashboard",
          shiny::br(),
          bslib::layout_column_wrap(
            width = 1/4,
            bslib::value_box("Patienten gesamt", shiny::textOutput("n_total"),
                             showcase = bsicons::bs_icon("people")),
            bslib::value_box("Primärfälle", shiny::textOutput("n_primaer"),
                             showcase = bsicons::bs_icon("clipboard2-pulse")),
            bslib::value_box("Patientenfälle", shiny::textOutput("n_patientenfall"),
                             showcase = bsicons::bs_icon("hospital")),
            bslib::value_box("Psychoonkologie", shiny::textOutput("n_psycho"),
                             showcase = bsicons::bs_icon("heart-pulse"))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Fälle nach Diagnose/Kodierung"),
                                         shiny::plotOutput("plot_diagnosis", height = 430))),
            shiny::column(6, bslib::card(bslib::card_header("Jährliche Fallzahlen"),
                                         shiny::plotOutput("plot_year", height = 430)))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Qualitäts-/Versorgungsindikatoren"),
                                         DT::DTOutput("indicator_table"))),
            shiny::column(6, bslib::card(bslib::card_header("Schnellfragen für Auditoren"),
                                         shiny::uiOutput("quick_questions")))
          )
        ),
        shiny::tabPanel(
          "Einfache Abfragen",
          shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              bslib::card(
                bslib::card_header("Auditorfrage auswählen"),
                shiny::selectInput(
                  "simple_question", "Vordefinierte Frage",
                  choices = c(
                    "Psychoonkologisches Screening" = "psycho",
                    "HIV/Hepatitis Screening" = "hivhep",
                    "Diagnose gezielt auswählen" = "diagnose_select",
                    "Multiples Myelom" = "myelom",
                    "Hodgkin-Lymphom" = "hodgkin",
                    "Tumorkonferenz erfolgt" = "tumorkonferenz",
                    "Sozialdienst angebunden" = "sozialdienst",
                    "Primärfälle" = "primaerfall",
                    "Patientenfälle" = "patientenfall",
                    "Eigene Abfrage" = "custom"
                  ),
                  selected = "psycho"
                ),
                shiny::uiOutput("simple_query_ui"),
                shiny::checkboxInput("simple_only_unique",
                                     "Nur eindeutige Namen zählen", FALSE),
                shiny::actionButton("run_simple_query",
                                    "Abfrage ausführen", class = "btn-primary"),
                shiny::br(), shiny::br(),
                shiny::downloadButton("download_simple_query", "Trefferliste CSV")
              )
            ),
            shiny::column(
              8,
              bslib::value_box("Ergebnis", shiny::textOutput("simple_query_result"),
                               showcase = bsicons::bs_icon("search")),
              shiny::br(),
              bslib::card(bslib::card_header("Zusammenfassung"),
                          DT::DTOutput("simple_query_summary")),
              shiny::br(),
              bslib::card(bslib::card_header("Trefferliste"),
                          DT::DTOutput("simple_query_table"))
            )
          )
        ),
        shiny::tabPanel(
          "Patientenliste", shiny::br(),
          shiny::helpText("Eine Zeile auswählen: Der Patient wird automatisch in den Tab 'Tumorboardbeschlüsse' übernommen."),
          DT::DTOutput("table")
        ),
        shiny::tabPanel(
          "Tumorboardbeschlüsse", shiny::br(),
          shiny::fluidRow(
            shiny::column(
              4,
              bslib::card(
                bslib::card_header("Beschluss für Patienten erfassen"),
                shiny::verbatimTextOutput("tb_storage_info"),
                shiny::uiOutput("tb_patient_ui"),
                shiny::dateInput("tb_date", "Datum Tumorboard",
                                 value = Sys.Date(),
                                 format = "dd.mm.yyyy", language = "de"),
                shiny::textAreaInput("tb_decision",
                                     "Tumorboardbeschluss / Empfehlung",
                                     value = "", rows = 7,
                                     placeholder = "z.B. Vorstellung Referenzpathologie, Therapieempfehlung, Studienprüfung, Re-Staging, supportive Maßnahmen ..."),
                shiny::textInput("tb_responsible",
                                 "Verantwortlich / Eintrag durch", value = ""),
                shiny::actionButton("save_tb",
                                    "Beschluss speichern", class = "btn-primary"),
                shiny::br(), shiny::br(),
                shiny::downloadButton("download_tb",
                                      "Tumorboardbeschlüsse CSV")
              )
            ),
            shiny::column(
              8,
              bslib::layout_column_wrap(
                width = 1/3,
                bslib::value_box("Beschlüsse gesamt", shiny::textOutput("tb_n_total")),
                bslib::value_box("Beschlüsse Patient", shiny::textOutput("tb_n_patient")),
                bslib::value_box("Patienten mit Beschluss", shiny::textOutput("tb_n_patients"))
              ),
              shiny::br(),
              bslib::card(bslib::card_header("Beschlüsse des ausgewählten Patienten"),
                          DT::DTOutput("tb_patient_table")),
              shiny::br(),
              bslib::card(bslib::card_header("Alle dokumentierten Tumorboardbeschlüsse"),
                          DT::DTOutput("tb_all_table"))
            )
          )
        ),
        shiny::tabPanel(
          "Kaplan–Meier", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("KM-Einstellungen"),
              shiny::uiOutput("km_ui"),
              shiny::actionButton("run_km", "KM aktualisieren", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_km_plot", "KM Plot PNG")
            )),
            shiny::column(8, bslib::card(
              bslib::card_header("Kaplan–Meier Plot"),
              shiny::plotOutput("km_plot", height = 560)
            ))
          )
        ),
        shiny::tabPanel(
          "OPS 8-544 Therapieblöcke", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("Filter Therapieblöcke"),
              shiny::verbatimTextOutput("therapy_source_info"),
              shiny::uiOutput("therapy_filters"),
              shiny::br(),
              shiny::downloadButton("download_therapy_blocks", "Therapieblöcke CSV")
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box("OPS-8-544-Blöcke", shiny::textOutput("n_therapy_blocks")),
              bslib::value_box("Patienten", shiny::textOutput("n_therapy_patients")),
              bslib::value_box("Therapieprotokolle", shiny::textOutput("n_therapy_protocols"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Wie viele Blöcke von welcher Therapie?"),
                                         DT::DTOutput("therapy_protocol_table"))),
            shiny::column(6, bslib::card(bslib::card_header("Blöcke nach Diagnose"),
                                         DT::DTOutput("therapy_diagnosis_table")))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Blöcke je Therapieprotokoll"),
                                         shiny::plotOutput("therapy_protocol_plot", height = 520))),
            shiny::column(6, bslib::card(bslib::card_header("Monatliche OPS-8-544-Blöcke"),
                                         shiny::plotOutput("therapy_month_plot", height = 520)))
          ),
          shiny::br(),
          bslib::card(bslib::card_header("Detailtabelle der gezählten Blöcke"),
                      DT::DTOutput("therapy_block_details"))
        ),
        shiny::tabPanel(
          "OPS 1-941 Diagnostik", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("Filter komplexe Diagnostik"),
              shiny::verbatimTextOutput("diagnostic_source_info"),
              shiny::uiOutput("diagnostic_filters"),
              shiny::br(),
              shiny::downloadButton("download_diagnostic_blocks",
                                    "Komplexe Diagnostik CSV")
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box("OPS-1-941-Fälle", shiny::textOutput("n_diagnostic_blocks")),
              bslib::value_box("Patienten", shiny::textOutput("n_diagnostic_patients")),
              bslib::value_box("Diagnosen", shiny::textOutput("n_diagnostic_diagnoses"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Komplexe Diagnostik nach Bereich"),
                                         DT::DTOutput("diagnostic_component_table"))),
            shiny::column(6, bslib::card(bslib::card_header("Komplexe Diagnostik nach Diagnose"),
                                         DT::DTOutput("diagnostic_diagnosis_table")))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Diagnostikbereiche"),
                                         shiny::plotOutput("diagnostic_component_plot", height = 520))),
            shiny::column(6, bslib::card(bslib::card_header("Monatliche OPS-1-941-Fälle"),
                                         shiny::plotOutput("diagnostic_month_plot", height = 520)))
          ),
          shiny::br(),
          bslib::card(bslib::card_header("Detailtabelle der komplexen Diagnostiken"),
                      DT::DTOutput("diagnostic_block_details"))
        ),
        shiny::tabPanel(
          "Oncoprint Mutationen", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("Oncoprint-Filter"),
              shiny::helpText("Quelle: Spalte 'Krankheitsspezifische hematol Resultate'. NA/negative Befunde werden nicht geplottet. Deletionen, Zugewinne, Translokationen/Rearrangements/Brüche, Loss und komplexer Karyotyp werden nur tabellarisch aufgeführt."),
              shiny::uiOutput("oncoprint_filters"),
              shiny::actionButton("run_oncoprint", "Oncoprint aktualisieren", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_oncoprint_data", "Mutationsdaten CSV"),
              shiny::downloadButton("download_structural_data", "Struktur-/Zytogenetik CSV"),
              shiny::downloadButton("download_oncoprint_plot", "Oncoprint PNG")
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box("Fälle mit Alterationen", shiny::textOutput("n_onco_patients")),
              bslib::value_box("Alterationen", shiny::textOutput("n_onco_alterations")),
              bslib::value_box("Entitäten", shiny::textOutput("n_onco_entities"))
            ))
          ),
          shiny::br(),
          bslib::card(bslib::card_header("Oncoprint – nur echte Mutationen/Varianten"),
                      shiny::plotOutput("oncoprint_plot", height = 720)),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Top-Mutationen nach Entität"),
                                         DT::DTOutput("oncoprint_summary_table"))),
            shiny::column(6, bslib::card(bslib::card_header("Detaildaten Mutationen"),
                                         DT::DTOutput("oncoprint_detail_table")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header("Strukturelle Befunde aus Mutationsspalte – nur tabellarisch"),
                      DT::DTOutput("oncoprint_structural_table"))
        ),
        shiny::tabPanel(
          "Zytogenetik", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("Zytogenetik-Filter"),
              shiny::helpText("Quelle: separate Spalte 'Zytogenetik'. NA und negative Befunde werden ausgeblendet. Die Darstellung ist bewusst tabellarisch/als Balkendiagramm, nicht im Oncoprint."),
              shiny::uiOutput("cyto_filters"),
              shiny::actionButton("run_cyto", "Zytogenetik aktualisieren", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_cyto_data", "Zytogenetik CSV")
            )),
            shiny::column(8, bslib::layout_column_wrap(
              width = 1/3,
              bslib::value_box("Fälle mit Zytogenetik", shiny::textOutput("n_cyto_patients")),
              bslib::value_box("Zytogenetik-Befunde", shiny::textOutput("n_cyto_alterations")),
              bslib::value_box("Entitäten", shiny::textOutput("n_cyto_entities"))
            ))
          ),
          shiny::br(),
          shiny::fluidRow(
            shiny::column(6, bslib::card(bslib::card_header("Top-Zytogenetik gesamt"),
                                         shiny::plotOutput("cyto_plot", height = 520))),
            shiny::column(6, bslib::card(bslib::card_header("Zytogenetik nach Entität"),
                                         DT::DTOutput("cyto_summary_table")))
          ),
          shiny::br(),
          bslib::card(bslib::card_header("Detaildaten Zytogenetik"),
                      DT::DTOutput("cyto_detail_table"))
        ),
        shiny::tabPanel(
          "Boxplots", shiny::br(),
          shiny::fluidRow(
            shiny::column(4, bslib::card(
              bslib::card_header("Boxplot-Einstellungen"),
              shiny::uiOutput("box_ui"),
              shiny::actionButton("run_box", "Boxplot aktualisieren", class = "btn-primary"),
              shiny::br(), shiny::br(),
              shiny::downloadButton("download_box_plot", "Boxplot PNG")
            )),
            shiny::column(8, bslib::card(bslib::card_header("Boxplot"),
                                         shiny::plotOutput("box_plot", height = 540)))
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {

  # ------------------------------------------------------------------
  # Data source: upload OR example button
  # ------------------------------------------------------------------
  current_path <- shiny::reactiveVal(NULL)
  current_label <- shiny::reactiveVal("Keine Daten geladen.")

  shiny::observeEvent(input$cohort_file, {
    f <- input$cohort_file
    if (is.null(f) || !nzchar(f$datapath)) return()
    current_path(f$datapath)
    current_label(paste0("Hochgeladen: ", f$name))
  })

  shiny::observeEvent(input$load_example, {
    p <- oncoscopR::onc_example_path()
    current_path(p)
    current_label("Beispieldaten geladen (synthetisch).")
  })

  output$path_info <- shiny::renderText({
    paste0("Datenquelle:\n", current_label())
  })

  # ------------------------------------------------------------------
  # Data readers — eventReactive on the reload button OR a new path
  # ------------------------------------------------------------------
  data_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) return(data.frame())
      oncoscopR::onc_read_cohort(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  therapy_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) {
        out <- data.frame()
        attr(out, "source_label") <- "Keine Datenquelle geladen"
        return(out)
      }
      oncoscopR::onc_read_therapy(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  diagnostic_raw <- shiny::eventReactive(
    list(input$reload, current_path()),
    {
      p <- current_path()
      if (is.null(p)) {
        out <- data.frame()
        attr(out, "source_label") <- "Keine Datenquelle geladen"
        return(out)
      }
      oncoscopR::onc_read_diagnostics(p, verbose = FALSE)
    },
    ignoreNULL = FALSE
  )

  # ------------------------------------------------------------------
  # Helper aliases — keep server bodies short
  # ------------------------------------------------------------------
  find_col   <- oncoscopR:::.find_col
  as_yesno   <- oncoscopR:::.as_yesno
  as_event01 <- oncoscopR:::.as_event01
  n_distinct_nonempty <- oncoscopR:::.n_distinct_nonempty

  output$global_filters <- shiny::renderUI({
    df <- data_raw()
    shiny::req(nrow(df) > 0)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    year_choices <- sort(unique(stats::na.omit(df$behandlungsjahr)))
    diag_choices <- if (!is.null(diagnosis_col)) {
      sort(unique(stats::na.omit(df[[diagnosis_col]])))
    } else character(0)
    shiny::tagList(
      shiny::selectizeInput("year_filter", "Behandlungsjahr",
                            choices = year_choices, selected = year_choices,
                            multiple = TRUE),
      shiny::selectizeInput("diagnosis_filter", "Diagnose/Kodierung",
                            choices = diag_choices, selected = diag_choices,
                            multiple = TRUE),
      shiny::checkboxInput("only_primaer", "Nur Primärfälle", FALSE),
      shiny::checkboxInput("only_patientenfall", "Nur Patientenfälle", FALSE),
      shiny::checkboxInput("only_inhouse_therapy", "Nur Therapie am Haus", FALSE)
    )
  })

  data_filtered <- shiny::reactive({
    df <- data_raw()
    if (nrow(df) == 0) return(df)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (!is.null(input$year_filter) && length(input$year_filter) > 0) {
      df <- dplyr::filter(df, .data$behandlungsjahr %in% as.integer(input$year_filter))
    }
    if (!is.null(diagnosis_col) &&
        !is.null(input$diagnosis_filter) && length(input$diagnosis_filter) > 0) {
      df <- dplyr::filter(df, .data[[diagnosis_col]] %in% input$diagnosis_filter)
    }
    if (isTRUE(input$only_primaer) && "primaerfall" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$primaerfall) %in% TRUE)
    }
    if (isTRUE(input$only_patientenfall) && "patientenfall" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$patientenfall) %in% TRUE)
    }
    if (isTRUE(input$only_inhouse_therapy) && "therapie_inhouse" %in% names(df)) {
      df <- dplyr::filter(df, as_yesno(.data$therapie_inhouse) %in% TRUE)
    }
    if (!is.null(input$search_text) && nzchar(input$search_text)) {
      search_cols <- intersect(
        c("name", "diagnose", "kodierung", "therapie", "info_weitere_betreuung"),
        names(df)
      )
      pattern <- stringr::regex(input$search_text, ignore_case = TRUE)
      df <- dplyr::filter(
        df,
        dplyr::if_any(dplyr::all_of(search_cols),
                      ~ stringr::str_detect(as.character(.x), pattern))
      )
    }
    df
  })

  output$n_total <- shiny::renderText({
    format(nrow(data_filtered()), big.mark = ".")
  })
  output$n_primaer <- shiny::renderText({
    df <- data_filtered()
    if (!"primaerfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$primaerfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })
  output$n_patientenfall <- shiny::renderText({
    df <- data_filtered()
    if (!"patientenfall" %in% names(df)) return("n/a")
    format(sum(as_yesno(df$patientenfall) %in% TRUE, na.rm = TRUE), big.mark = ".")
  })
  output$n_psycho <- shiny::renderText({
    df <- data_filtered()
    if (!"psychoonkologie" %in% names(df)) return("n/a")
    n <- sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE)
    denom <- nrow(df)
    paste0(n, " / ", denom, " (",
           scales::percent(n / max(denom, 1), accuracy = 0.1), ")")
  })

  output$plot_diagnosis <- shiny::renderPlot({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    shiny::validate(shiny::need(
      !is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."
    ))
    df |>
      dplyr::count(.data[[diagnosis_col]], sort = TRUE) |>
      dplyr::slice_head(n = 20) |>
      ggplot2::ggplot(ggplot2::aes(
        x = stats::reorder(.data[[diagnosis_col]], .data$n), y = .data$n
      )) +
      ggplot2::geom_col() +
      ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = NULL, y = "Anzahl", title = "Top-Diagnosen/Kodierungen")
  })

  output$plot_year <- shiny::renderPlot({
    df <- data_filtered()
    shiny::validate(shiny::need(
      "behandlungsjahr" %in% names(df), "Kein Behandlungsjahr ableitbar."
    ))
    df |>
      dplyr::filter(!is.na(.data$behandlungsjahr)) |>
      dplyr::count(.data$behandlungsjahr) |>
      ggplot2::ggplot(ggplot2::aes(x = factor(.data$behandlungsjahr),
                                   y = .data$n)) +
      ggplot2::geom_col() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = "Jahr", y = "Anzahl", title = "Fallzahlen nach Jahr")
  })

  output$indicator_table <- DT::renderDT({
    df <- data_filtered()
    indicators <- c(
      "tumorkonferenz", "fallbesprechung", "psychoonkologie", "sozialdienst",
      "komplexe_diagnostik_nach_ops_1_940", "histologie_inhouse",
      "histologie_referenzpathologie", "studie", "zahnarzt_mkg",
      "bisphosphonate_denosumab", "hiv_hepatitis"
    )
    indicators <- intersect(indicators, names(df))
    out <- lapply(indicators, function(v) {
      val <- df[[v]]
      yes <- sum(as_yesno(val) %in% TRUE, na.rm = TRUE)
      documented <- sum(!is.na(val) & trimws(as.character(val)) != "")
      data.frame(
        Indikator = v, Positiv = yes, Dokumentiert = documented,
        Gesamt = nrow(df),
        Anteil_positiv = ifelse(nrow(df) > 0, yes / nrow(df), NA_real_)
      )
    }) |> dplyr::bind_rows()
    DT::datatable(out, rownames = FALSE,
                  options = list(pageLength = 12, scrollX = TRUE)) |>
      DT::formatPercentage("Anteil_positiv", 1)
  })

  output$quick_questions <- shiny::renderUI({
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (is.null(diagnosis_col)) {
      return(shiny::helpText("Keine Diagnose-/Kodierungsspalte gefunden."))
    }
    hl_2025 <- df |>
      dplyr::filter(.data$behandlungsjahr == 2025,
                    stringr::str_detect(
                      tolower(as.character(.data[[diagnosis_col]])),
                      "hodgkin|hl"
                    )) |> nrow()
    mm <- df |>
      dplyr::filter(stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])),
        "myelom|multiple myeloma|mm"
      )) |> nrow()
    psycho <- if ("psychoonkologie" %in% names(df)) {
      sum(as_yesno(df$psychoonkologie) %in% TRUE, na.rm = TRUE)
    } else NA_integer_
    tk <- if ("tumorkonferenz" %in% names(df)) {
      sum(as_yesno(df$tumorkonferenz) %in% TRUE, na.rm = TRUE)
    } else NA_integer_
    shiny::HTML(paste0(
      "<ul>",
      "<li><b>Hodgkin-Lymphome 2025:</b> ", hl_2025, " Fälle im aktuellen Filter.</li>",
      "<li><b>Patienten mit psychoonkologischem Screening:</b> ", psycho, " Fälle.</li>",
      "<li><b>Patienten/Fälle mit Tumorkonferenz:</b> ", tk, " Fälle.</li>",
      "<li><b>Multiple-Myelom-Fälle:</b> ", mm, " Fälle. Für PFS-Kurve Diagnosefilter auf Myelom setzen und im KM-Tab PFS auswählen.</li>",
      "</ul>"
    ))
  })

  # ------------------------------------------------------------------
  # Einfache Abfragen (server logic — verbatim from v5, namespaced)
  # ------------------------------------------------------------------
  output$simple_query_ui <- shiny::renderUI({
    df <- data_filtered()
    shiny::req(nrow(df) > 0)
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    if (identical(input$simple_question, "diagnose_select")) {
      shiny::validate(shiny::need(!is.null(diagnosis_col),
                                  "Keine Spalte 'diagnose' oder 'kodierung' gefunden."))
      diag_vals <- sort(unique(stats::na.omit(as.character(df[[diagnosis_col]]))))
      shiny::tagList(
        shiny::helpText("Hier können gezielt dokumentierte Diagnosen aus der Spalte 'Diagnose' ausgewählt werden. Mehrfachauswahl ist möglich."),
        shiny::selectizeInput("diagnosis_query_values", "Diagnose(n)",
                              choices = diag_vals, selected = character(0),
                              multiple = TRUE,
                              options = list(placeholder = "z.B. Multiples Myelom auswählen")),
        shiny::checkboxInput("diagnosis_query_contains",
                             "Als Textsuche verwenden statt exakter Auswahl", FALSE)
      )
    } else if (identical(input$simple_question, "custom")) {
      text_cols <- names(df)[vapply(df, function(x) {
        is.character(x) || is.factor(x) || is.logical(x) || is.numeric(x)
      }, logical(1L))]
      shiny::tagList(
        shiny::selectInput("custom_col", "Spalte", choices = text_cols,
                           selected = if ("diagnose" %in% text_cols) "diagnose" else text_cols[1]),
        shiny::radioButtons("custom_mode", "Abfragemodus", choices = c(
          "Ja/positiv zählen" = "yesno",
          "Exakter Wert" = "exact",
          "Text enthält" = "contains",
          "Nicht leer/dokumentiert" = "documented"
        ), selected = "yesno"),
        shiny::conditionalPanel(
          condition = "input.custom_mode == 'exact' || input.custom_mode == 'contains'",
          shiny::textInput("custom_value", "Suchwert/Text", value = "")
        )
      )
    } else {
      shiny::helpText("Die Abfrage wird auf die aktuell global gefilterte Patiententabelle angewendet. Beispiel: Jahr 2025 im linken Filter auswählen, dann hier Multiples Myelom zählen.")
    }
  })

  simple_query_data <- shiny::eventReactive(input$run_simple_query, {
    df <- data_filtered()
    diagnosis_col <- find_col(df, c("diagnose", "kodierung"))
    q <- input$simple_question
    shiny::validate(shiny::need(nrow(df) > 0, "Keine Daten nach globalem Filter."))
    result <- df
    label <- ""
    if (q == "psycho") {
      shiny::validate(shiny::need("psychoonkologie" %in% names(df), "Spalte 'psychoonkologie' nicht gefunden."))
      result <- dplyr::filter(df, as_yesno(.data$psychoonkologie) %in% TRUE)
      label <- "Patienten/Fälle mit psychoonkologischem Screening"
    } else if (q == "hivhep") {
      hivhep_cols <- intersect(c("hiv_hepatitis", "hiv", "hep_b", "hep_c", "hepb", "hepc"), names(df))
      shiny::validate(shiny::need(length(hivhep_cols) > 0, "Keine HIV/Hepatitis-Spalten gefunden."))
      result <- dplyr::filter(df, dplyr::if_any(dplyr::all_of(hivhep_cols),
                                                ~ as_yesno(.x) %in% TRUE))
      label <- "Patienten/Fälle mit dokumentiert positivem HIV/Hepatitis-Screening"
    } else if (q == "diagnose_select") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      shiny::validate(shiny::need(!is.null(input$diagnosis_query_values) && length(input$diagnosis_query_values) > 0,
                                  "Bitte mindestens eine Diagnose auswählen."))
      selected_diag <- input$diagnosis_query_values
      if (isTRUE(input$diagnosis_query_contains)) {
        pattern <- paste(stringr::str_escape(selected_diag), collapse = "|")
        result <- dplyr::filter(df, stringr::str_detect(
          tolower(as.character(.data[[diagnosis_col]])),
          stringr::regex(tolower(pattern), ignore_case = TRUE)
        ))
      } else {
        result <- dplyr::filter(df, as.character(.data[[diagnosis_col]]) %in% selected_diag)
      }
      label <- paste0("Patienten/Fälle mit Diagnose: ", paste(selected_diag, collapse = ", "))
    } else if (q == "myelom") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      result <- dplyr::filter(df, stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])),
        "myelom|multiple myeloma|multiples myelom|plasma"
      ))
      label <- "Patienten/Fälle mit Multiplem Myelom"
    } else if (q == "hodgkin") {
      shiny::validate(shiny::need(!is.null(diagnosis_col), "Keine Diagnose-/Kodierungsspalte gefunden."))
      result <- dplyr::filter(df, stringr::str_detect(
        tolower(as.character(.data[[diagnosis_col]])), "hodgkin|hl"
      ))
      label <- "Patienten/Fälle mit Hodgkin-Lymphom"
    } else if (q == "tumorkonferenz") {
      shiny::validate(shiny::need("tumorkonferenz" %in% names(df), "Spalte 'tumorkonferenz' nicht gefunden."))
      result <- dplyr::filter(df, as_yesno(.data$tumorkonferenz) %in% TRUE)
      label <- "Patienten/Fälle mit Tumorkonferenz"
    } else if (q == "sozialdienst") {
      shiny::validate(shiny::need("sozialdienst" %in% names(df), "Spalte 'sozialdienst' nicht gefunden."))
      result <- dplyr::filter(df, as_yesno(.data$sozialdienst) %in% TRUE)
      label <- "Patienten/Fälle mit Sozialdienst"
    } else if (q == "primaerfall") {
      shiny::validate(shiny::need("primaerfall" %in% names(df), "Spalte 'primaerfall' nicht gefunden."))
      result <- dplyr::filter(df, as_yesno(.data$primaerfall) %in% TRUE)
      label <- "Primärfälle"
    } else if (q == "patientenfall") {
      shiny::validate(shiny::need("patientenfall" %in% names(df), "Spalte 'patientenfall' nicht gefunden."))
      result <- dplyr::filter(df, as_yesno(.data$patientenfall) %in% TRUE)
      label <- "Patientenfälle"
    } else if (q == "custom") {
      shiny::validate(shiny::need(!is.null(input$custom_col) && input$custom_col %in% names(df), "Bitte eine gültige Spalte auswählen."))
      col <- input$custom_col; mode <- input$custom_mode; value <- input$custom_value
      if (mode == "yesno") {
        result <- dplyr::filter(df, as_yesno(.data[[col]]) %in% TRUE)
        label <- paste0("Eigene Abfrage: ", col, " = Ja/positiv")
      } else if (mode == "exact") {
        shiny::validate(shiny::need(nzchar(value), "Bitte einen Suchwert eingeben."))
        result <- dplyr::filter(df, tolower(trimws(as.character(.data[[col]]))) == tolower(trimws(value)))
        label <- paste0("Eigene Abfrage: ", col, " = ", value)
      } else if (mode == "contains") {
        shiny::validate(shiny::need(nzchar(value), "Bitte einen Suchtext eingeben."))
        result <- dplyr::filter(df, stringr::str_detect(
          tolower(as.character(.data[[col]])), stringr::fixed(tolower(value))
        ))
        label <- paste0("Eigene Abfrage: ", col, " enthält '", value, "'")
      } else if (mode == "documented") {
        result <- dplyr::filter(df, !is.na(.data[[col]]) & trimws(as.character(.data[[col]])) != "")
        label <- paste0("Eigene Abfrage: ", col, " ist dokumentiert/nicht leer")
      }
    }
    if (isTRUE(input$simple_only_unique) && "name" %in% names(result)) {
      result <- dplyr::distinct(result, .data$name, .keep_all = TRUE)
    }
    attr(result, "query_label") <- label
    attr(result, "denominator") <- if (isTRUE(input$simple_only_unique) && "name" %in% names(df)) {
      dplyr::n_distinct(df$name)
    } else nrow(df)
    result
  }, ignoreNULL = FALSE)

  output$simple_query_result <- shiny::renderText({
    res <- simple_query_data(); denom <- attr(res, "denominator")
    paste0(nrow(res), " / ", denom, " (",
           scales::percent(nrow(res) / max(denom, 1), accuracy = 0.1), ")")
  })
  output$simple_query_summary <- DT::renderDT({
    res <- simple_query_data()
    label <- attr(res, "query_label"); denom <- attr(res, "denominator")
    out <- data.frame(
      Abfrage = label, Treffer = nrow(res), Grundgesamtheit = denom,
      Anteil = ifelse(denom > 0, nrow(res) / denom, NA_real_)
    )
    DT::datatable(out, rownames = FALSE,
                  options = list(dom = "t", scrollX = TRUE)) |>
      DT::formatPercentage("Anteil", 1)
  })
  output$simple_query_table <- DT::renderDT({
    res <- simple_query_data()
    cols_preferred <- intersect(c(
      "name", "geschlecht", "geb_datum", "erstvorstellung", "erstdiagnose",
      "diagnose", "kodierung", "primaerfall", "patientenfall",
      "tumorkonferenz", "psychoonkologie", "hiv_hepatitis", "hiv",
      "hep_b", "hep_c", "pfs", "os"
    ), names(res))
    if (length(cols_preferred) > 0) res <- res[, cols_preferred, drop = FALSE]
    DT::datatable(res, rownames = FALSE,
                  options = list(pageLength = 20, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_simple_query <- shiny::downloadHandler(
    filename = function() paste0("einfache_Abfrage_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(simple_query_data(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  output$table <- DT::renderDT({
    DT::datatable(data_filtered(), rownames = FALSE, selection = "single",
                  options = list(pageLength = 25, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_filtered <- shiny::downloadHandler(
    filename = function() paste0("gefilterte_Daten_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(data_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Tumorboard — in-session reactiveVal, optional upload, no disk writes
  # ------------------------------------------------------------------
  tumorboard_data <- shiny::reactiveVal(oncoscopR::onc_read_tumorboard(NULL))

  shiny::observeEvent(input$tumorboard_file, {
    f <- input$tumorboard_file
    if (is.null(f) || !nzchar(f$datapath)) return()
    tumorboard_data(oncoscopR::onc_read_tumorboard(f$datapath))
    shiny::showNotification(paste0("Tumorboardbeschlüsse geladen: ", f$name),
                            type = "message")
  })

  output$tb_storage_info <- shiny::renderText({
    paste0("Speicherung:\nIn-Session (kein Schreibzugriff auf Festplatte). ",
           "Über 'Tumorboardbeschlüsse CSV' herunterladen oder via Upload ",
           "wiederherstellen.")
  })

  tb_patient_col <- shiny::reactive({
    df <- data_filtered()
    find_col(df, c("patient", "name", "patient_id", "patienten_id", "id"))
  })

  output$tb_patient_ui <- shiny::renderUI({
    df <- data_filtered()
    shiny::req(nrow(df) > 0)
    pcol <- tb_patient_col()
    shiny::validate(shiny::need(!is.null(pcol),
                                "Keine Patientenspalte gefunden. Erwartet z.B. 'Patient', 'Name' oder 'Patient_ID'."))
    choices <- sort(unique(trimws(as.character(df[[pcol]]))))
    choices <- choices[!is.na(choices) & choices != ""]
    shiny::selectizeInput("tb_patient", "Patient aus Patientenliste",
                          choices = choices,
                          selected = if (length(choices)) choices[1] else NULL,
                          multiple = FALSE)
  })

  shiny::observeEvent(input$table_rows_selected, {
    idx <- input$table_rows_selected
    df <- data_filtered()
    pcol <- tb_patient_col()
    if (length(idx) == 1 && !is.null(pcol) && pcol %in% names(df) && nrow(df) >= idx) {
      pat <- trimws(as.character(df[[pcol]][idx]))
      if (!is.na(pat) && nzchar(pat)) {
        shiny::updateSelectizeInput(session, "tb_patient", selected = pat)
      }
    }
  }, ignoreInit = TRUE)

  shiny::observeEvent(input$save_tb, {
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient)) {
      shiny::showNotification("Bitte zuerst einen Patienten auswählen.", type = "error"); return()
    }
    if (is.null(input$tb_decision) || !nzchar(trimws(input$tb_decision))) {
      shiny::showNotification("Bitte einen Tumorboardbeschluss eintragen.", type = "error"); return()
    }
    entry <- data.frame(
      Patient = as.character(input$tb_patient),
      Board_Datum = as.Date(input$tb_date),
      Tumorboardbeschluss = trimws(as.character(input$tb_decision)),
      Verantwortlich = trimws(as.character(input$tb_responsible)),
      Erfasst_am = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
      stringsAsFactors = FALSE
    )
    tumorboard_data(dplyr::bind_rows(tumorboard_data(), entry))
    shiny::updateTextAreaInput(session, "tb_decision", value = "")
    shiny::showNotification("Tumorboardbeschluss gespeichert (in-Session).",
                            type = "message")
  })

  tb_current_patient <- shiny::reactive({
    df <- tumorboard_data()
    if (is.null(input$tb_patient) || !nzchar(input$tb_patient) || nrow(df) == 0) {
      return(df[0, , drop = FALSE])
    }
    df |>
      dplyr::filter(.data$Patient == input$tb_patient) |>
      dplyr::arrange(dplyr::desc(.data$Board_Datum), dplyr::desc(.data$Erfasst_am))
  })

  output$tb_n_total   <- shiny::renderText(format(nrow(tumorboard_data()), big.mark = "."))
  output$tb_n_patient <- shiny::renderText(format(nrow(tb_current_patient()), big.mark = "."))
  output$tb_n_patients <- shiny::renderText({
    df <- tumorboard_data()
    if (nrow(df) == 0 || !"Patient" %in% names(df)) return("0")
    format(n_distinct_nonempty(df$Patient), big.mark = ".")
  })
  output$tb_patient_table <- DT::renderDT({
    DT::datatable(tb_current_patient(), rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE))
  })
  output$tb_all_table <- DT::renderDT({
    df <- tumorboard_data()
    if (nrow(df) > 0) df <- dplyr::arrange(df, dplyr::desc(.data$Board_Datum),
                                           .data$Patient)
    DT::datatable(df, rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE,
                                 searchHighlight = TRUE))
  })
  output$download_tb <- shiny::downloadHandler(
    filename = function() paste0("Tumorboardbeschluesse_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(tumorboard_data(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Kaplan-Meier (uses survminer if available, base ggplot fallback otherwise)
  # ------------------------------------------------------------------
  output$km_ui <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    cat_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1L))]
    diagnose_choices <- if ("diagnose" %in% names(df)) {
      sort(unique(stats::na.omit(as.character(df$diagnose))))
    } else character(0)
    shiny::tagList(
      shiny::selectInput("km_endpoint", "Analyse", choices = c(
        "PFS automatisch: PFS + Rezidiv Event" = "pfs_auto",
        "OS automatisch: OS + Death Event" = "os_auto",
        "Manuell" = "manual"
      ), selected = "pfs_auto"),
      shiny::selectizeInput("km_diagnosis", "Diagnose/Entität für KM-Kurve",
                            choices = c("Alle Diagnosen" = "__all__", diagnose_choices),
                            selected = "__all__", multiple = FALSE),
      shiny::selectInput("km_time", "Zeitvariable manuell", choices = num_cols,
                         selected = if ("pfs" %in% num_cols) "pfs" else num_cols[1]),
      shiny::selectInput("km_event", "Eventvariable manuell", choices = names(df),
                         selected = if ("rezidiv_event" %in% names(df)) "rezidiv_event"
                                    else if ("rezidiv" %in% names(df)) "rezidiv"
                                    else if ("death_event" %in% names(df)) "death_event"
                                    else names(df)[1]),
      shiny::selectInput("km_group", "Gruppe/Stratum optional",
                         choices = c("— keine —", cat_cols), selected = "— keine —"),
      shiny::checkboxInput("km_confint", "Konfidenzintervall", TRUE),
      shiny::checkboxInput("km_risktable", "Risk Table", TRUE),
      shiny::numericInput("km_time_div",
                          "Zeit-Skalierung: 1 = Monate, 12 = Jahre",
                          value = 1, min = 0.0001),
      shiny::textInput("km_title", "Titel", value = "Kaplan–Meier-Kurve"),
      shiny::textInput("km_xlab", "X-Achse", value = "Monate"),
      shiny::textInput("km_ylab", "Y-Achse", value = "Wahrscheinlichkeit")
    )
  })

  km_plot_obj <- shiny::eventReactive(input$run_km, {
    df <- data_filtered()
    shiny::validate(shiny::need(nrow(df) > 1, "Zu wenige Daten nach Filter."))
    if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__" &&
        "diagnose" %in% names(df)) {
      df <- dplyr::filter(df, as.character(.data$diagnose) == input$km_diagnosis)
    }
    endpoint <- input$km_endpoint
    if (endpoint == "pfs_auto") {
      time_col <- "pfs"
      event_col <- if ("rezidiv_event" %in% names(df)) "rezidiv_event" else "rezidiv"
      event_mode <- "auto"
      default_title <- paste0("PFS", if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__") paste0(" – ", input$km_diagnosis) else "")
    } else if (endpoint == "os_auto") {
      time_col <- "os"; event_col <- "death_event"; event_mode <- "auto"
      default_title <- paste0("OS", if (!is.null(input$km_diagnosis) && input$km_diagnosis != "__all__") paste0(" – ", input$km_diagnosis) else "")
    } else {
      shiny::req(input$km_time, input$km_event)
      time_col <- input$km_time; event_col <- input$km_event
      event_mode <- if (event_col == "rezidiv") "date_event" else "auto"
      default_title <- input$km_title
    }
    shiny::validate(
      shiny::need(time_col %in% names(df), paste0("Zeitspalte '", time_col, "' nicht gefunden.")),
      shiny::need(event_col %in% names(df), paste0("Eventspalte '", event_col, "' nicht gefunden."))
    )
    time <- suppressWarnings(as.numeric(df[[time_col]])) / input$km_time_div
    event <- as_event01(df[[event_col]], mode = event_mode)
    keep <- !is.na(time) & !is.na(event) & time >= 0 & event %in% c(0, 1)
    shiny::validate(
      shiny::need(sum(keep) >= 2, paste0(
        "Zu wenige verwertbare KM-Daten. Prüfen: ", time_col, " muss Monate enthalten; ",
        event_col, " muss Ereignis/Zensierung enthalten."
      )),
      shiny::need(sum(event[keep] == 1, na.rm = TRUE) >= 1,
                  "Keine Ereignisse in der Auswahl.")
    )
    km_df <- data.frame(time = time[keep], event = as.integer(event[keep]))
    if (input$km_group != "— keine —") {
      km_df$grp <- as.factor(df[[input$km_group]][keep])
      km_df <- km_df[!is.na(km_df$grp), , drop = FALSE]
      fit <- survival::survfit(survival::Surv(time, event) ~ grp, data = km_df)
    } else {
      fit <- survival::survfit(survival::Surv(time, event) ~ 1, data = km_df)
    }
    title_to_use <- if (!is.null(input$km_title) && nzchar(input$km_title) &&
                        input$km_title != "Kaplan–Meier-Kurve") {
      input$km_title
    } else default_title

    if (requireNamespace("survminer", quietly = TRUE)) {
      survminer::ggsurvplot(
        fit, data = km_df,
        conf.int = isTRUE(input$km_confint),
        risk.table = isTRUE(input$km_risktable),
        ggtheme = ggplot2::theme_minimal(base_size = 13),
        title = title_to_use, xlab = input$km_xlab, ylab = input$km_ylab,
        censor = TRUE, risk.table.height = 0.25
      )
    } else {
      # Fallback when survminer is unavailable.
      .km_base_plot(fit, km_df, title_to_use, input$km_xlab, input$km_ylab,
                    show_ci = isTRUE(input$km_confint))
    }
  }, ignoreNULL = FALSE)

  output$km_plot <- shiny::renderPlot({ g <- km_plot_obj(); shiny::req(g); print(g) })
  output$download_km_plot <- shiny::downloadHandler(
    filename = function() paste0("KM_", Sys.Date(), ".png"),
    content = function(file) {
      g <- km_plot_obj()
      grDevices::png(file, width = 1500, height = 1000, res = 150)
      print(g); grDevices::dev.off()
    }
  )

  # ------------------------------------------------------------------
  # OPS-8-544
  # ------------------------------------------------------------------
  therapy_block_data <- shiny::reactive({
    oncoscopR::onc_prepare_therapy_blocks(therapy_raw())
  })

  output$therapy_source_info <- shiny::renderText({
    blocks <- therapy_block_data()
    paste0(
      attr(therapy_raw(), "source_label"), "\nPatientenspalte erkannt: ",
      ifelse(is.null(attr(blocks, "patient_cols_used")) ||
               attr(blocks, "patient_cols_used") == "", "keine",
             attr(blocks, "patient_cols_used"))
    )
  })

  output$therapy_filters <- shiny::renderUI({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) {
      return(shiny::helpText("Keine Therapieblock-Tabelle gefunden. Bitte die OPS-8-544-Tabelle als Blatt 'Komplexe Chemotherapie' (oder 'Therapie_OPS8544') in die Excel-Datei einfügen."))
    }
    shiny::tagList(
      shiny::selectizeInput("therapy_year_filter", "Jahr",
                            choices = sort(unique(stats::na.omit(blocks$jahr))),
                            selected = sort(unique(stats::na.omit(blocks$jahr))),
                            multiple = TRUE),
      shiny::selectizeInput("therapy_protocol_filter", "Therapieprotokoll",
                            choices = sort(unique(stats::na.omit(blocks$therapieprotokoll))),
                            selected = NULL, multiple = TRUE),
      shiny::selectizeInput("therapy_diagnosis_filter", "Diagnose",
                            choices = sort(unique(stats::na.omit(blocks$diagnose))),
                            selected = NULL, multiple = TRUE),
      shiny::textInput("therapy_search",
                       "Freitextsuche Patient/Therapie/Diagnose", value = "")
    )
  })

  therapy_filtered <- shiny::reactive({
    blocks <- therapy_block_data()
    if (nrow(blocks) == 0) return(blocks)
    if (!is.null(input$therapy_year_filter) && length(input$therapy_year_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$jahr %in% as.integer(input$therapy_year_filter))
    }
    if (!is.null(input$therapy_protocol_filter) && length(input$therapy_protocol_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$therapieprotokoll %in% input$therapy_protocol_filter)
    }
    if (!is.null(input$therapy_diagnosis_filter) && length(input$therapy_diagnosis_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$diagnose %in% input$therapy_diagnosis_filter)
    }
    if (!is.null(input$therapy_search) && nzchar(input$therapy_search)) {
      pat <- tolower(input$therapy_search)
      blocks <- dplyr::filter(blocks, grepl(
        pat,
        tolower(paste(.data$patient, .data$therapieprotokoll, .data$diagnose)),
        fixed = TRUE
      ))
    }
    blocks
  })

  output$n_therapy_blocks <- shiny::renderText(format(nrow(therapy_filtered()), big.mark = "."))
  output$n_therapy_patients <- shiny::renderText({
    blocks <- therapy_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    format(n_distinct_nonempty(blocks$patient), big.mark = ".")
  })
  output$n_therapy_protocols <- shiny::renderText(
    format(length(unique(stats::na.omit(therapy_filtered()$therapieprotokoll))), big.mark = ".")
  )

  output$therapy_protocol_table <- DT::renderDT({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks |>
      dplyr::group_by(.data$therapieprotokoll) |>
      dplyr::summarise(OPS_8_544_Bloecke = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_8_544_Bloecke)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_8_544_Bloecke / sum(.data$OPS_8_544_Bloecke)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$therapy_diagnosis_table <- DT::renderDT({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks |>
      dplyr::group_by(.data$diagnose) |>
      dplyr::summarise(OPS_8_544_Bloecke = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_8_544_Bloecke)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_8_544_Bloecke / sum(.data$OPS_8_544_Bloecke)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$therapy_protocol_plot <- shiny::renderPlot({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks |> dplyr::count(.data$therapieprotokoll, sort = TRUE) |> dplyr::slice_head(n = 20)
    ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$therapieprotokoll, .data$n),
                                      y = .data$n)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = NULL, y = "OPS-8-544-Blöcke", title = "Blöcke nach Therapieprotokoll")
  })

  output$therapy_month_plot <- shiny::renderPlot({
    blocks <- therapy_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine Therapieblöcke im aktuellen Filter."))
    tab <- blocks |> dplyr::filter(!is.na(.data$monat_sort)) |> dplyr::count(.data$monat_sort)
    shiny::validate(shiny::need(nrow(tab) > 0, "Keine verwertbaren Datums-/Monatsangaben."))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$monat_sort, y = .data$n, group = 1)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(x = "Monat", y = "OPS-8-544-Blöcke", title = "Monatliche OPS-8-544-Blöcke")
  })

  output$therapy_block_details <- DT::renderDT({
    blocks <- therapy_filtered()
    show_cols <- intersect(c("datum", "patient", "therapieprotokoll", "diagnose", "zyklus", "jahr"), names(blocks))
    DT::datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_therapy_blocks <- shiny::downloadHandler(
    filename = function() paste0("OPS_8_544_Therapiebloecke_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(therapy_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # OPS-1-941
  # ------------------------------------------------------------------
  diagnostic_block_data <- shiny::reactive({
    oncoscopR::onc_prepare_diagnostic_blocks(diagnostic_raw())
  })

  output$diagnostic_source_info <- shiny::renderText({
    blocks <- diagnostic_block_data()
    paste0(attr(diagnostic_raw(), "source_label"), "\nErkannte Komponenten: ",
           ifelse(is.null(attr(blocks, "component_cols")) ||
                    attr(blocks, "component_cols") == "", "keine",
                  attr(blocks, "component_cols")))
  })

  output$diagnostic_filters <- shiny::renderUI({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) {
      return(shiny::helpText("Keine OPS-1-941-/Komplexe-Diagnostik-Tabelle gefunden. Bitte die Tabelle als Blatt 'Komplexe Diagnostik' in die Excel-Datei einfügen."))
    }
    shiny::tagList(
      shiny::selectizeInput("diagnostic_year_filter", "Jahr",
                            choices = sort(unique(stats::na.omit(blocks$jahr))),
                            selected = sort(unique(stats::na.omit(blocks$jahr))),
                            multiple = TRUE),
      shiny::selectizeInput("diagnostic_diagnosis_filter", "Diagnose",
                            choices = sort(unique(stats::na.omit(blocks$diagnose))),
                            selected = NULL, multiple = TRUE),
      shiny::selectizeInput("diagnostic_component_filter", "Diagnostikbereich",
                            choices = sort(unique(oncoscopR:::.diagnostic_components_long(blocks)$diagnostik_bereich)),
                            selected = NULL, multiple = TRUE),
      shiny::textInput("diagnostic_search", "Freitextsuche Patient/Diagnose", value = "")
    )
  })

  diagnostic_filtered <- shiny::reactive({
    blocks <- diagnostic_block_data()
    if (nrow(blocks) == 0) return(blocks)
    if (!is.null(input$diagnostic_year_filter) && length(input$diagnostic_year_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$jahr %in% as.integer(input$diagnostic_year_filter))
    }
    if (!is.null(input$diagnostic_diagnosis_filter) && length(input$diagnostic_diagnosis_filter) > 0) {
      blocks <- dplyr::filter(blocks, .data$diagnose %in% input$diagnostic_diagnosis_filter)
    }
    if (!is.null(input$diagnostic_component_filter) && length(input$diagnostic_component_filter) > 0) {
      long <- oncoscopR:::.diagnostic_components_long(blocks)
      long <- dplyr::filter(long, .data$diagnostik_bereich %in% input$diagnostic_component_filter)
      keep_pat <- unique(long$patient)
      blocks <- dplyr::filter(blocks, .data$patient %in% keep_pat)
    }
    if (!is.null(input$diagnostic_search) && nzchar(input$diagnostic_search)) {
      pat <- tolower(input$diagnostic_search)
      blocks <- dplyr::filter(blocks, grepl(
        pat, tolower(paste(.data$patient, .data$diagnose)), fixed = TRUE
      ))
    }
    blocks
  })

  diagnostic_components_filtered <- shiny::reactive({
    long <- oncoscopR:::.diagnostic_components_long(diagnostic_filtered())
    if (!is.null(input$diagnostic_component_filter) &&
        length(input$diagnostic_component_filter) > 0 && nrow(long) > 0) {
      long <- dplyr::filter(long, .data$diagnostik_bereich %in% input$diagnostic_component_filter)
    }
    long
  })

  output$n_diagnostic_blocks <- shiny::renderText(format(nrow(diagnostic_filtered()), big.mark = "."))
  output$n_diagnostic_patients <- shiny::renderText({
    blocks <- diagnostic_filtered()
    if (!("patient" %in% names(blocks))) return("0")
    format(n_distinct_nonempty(blocks$patient), big.mark = ".")
  })
  output$n_diagnostic_diagnoses <- shiny::renderText(
    format(length(unique(stats::na.omit(diagnostic_filtered()$diagnose))), big.mark = ".")
  )

  output$diagnostic_component_table <- DT::renderDT({
    long <- diagnostic_components_filtered()
    shiny::validate(shiny::need(nrow(long) > 0, "Keine komplexen Diagnostik-Komponenten im aktuellen Filter."))
    tab <- long |>
      dplyr::group_by(.data$diagnostik_bereich) |>
      dplyr::summarise(Anzahl = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$Anzahl)) |>
      dplyr::mutate(Anteil = scales::percent(.data$Anzahl / sum(.data$Anzahl)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diagnostic_diagnosis_table <- DT::renderDT({
    blocks <- diagnostic_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine komplexen Diagnostiken im aktuellen Filter."))
    tab <- blocks |>
      dplyr::group_by(.data$diagnose) |>
      dplyr::summarise(OPS_1_941_Faelle = dplyr::n(),
                       Patienten = n_distinct_nonempty(.data$patient),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(.data$OPS_1_941_Faelle)) |>
      dplyr::mutate(Anteil = scales::percent(.data$OPS_1_941_Faelle / sum(.data$OPS_1_941_Faelle)))
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 15, scrollX = TRUE))
  })

  output$diagnostic_component_plot <- shiny::renderPlot({
    long <- diagnostic_components_filtered()
    shiny::validate(shiny::need(nrow(long) > 0, "Keine komplexen Diagnostik-Komponenten im aktuellen Filter."))
    tab <- long |> dplyr::count(.data$diagnostik_bereich, sort = TRUE)
    ggplot2::ggplot(tab, ggplot2::aes(x = stats::reorder(.data$diagnostik_bereich, .data$n),
                                      y = .data$n)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(x = NULL, y = "Anzahl", title = "OPS-1-941-Komponenten")
  })

  output$diagnostic_month_plot <- shiny::renderPlot({
    blocks <- diagnostic_filtered()
    shiny::validate(shiny::need(nrow(blocks) > 0, "Keine komplexen Diagnostiken im aktuellen Filter."))
    tab <- blocks |> dplyr::filter(!is.na(.data$monat_sort)) |> dplyr::count(.data$monat_sort)
    shiny::validate(shiny::need(nrow(tab) > 0, "Keine verwertbaren Datums-/Monatsangaben."))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$monat_sort, y = .data$n, group = 1)) +
      ggplot2::geom_line() + ggplot2::geom_point() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(x = "Monat", y = "OPS-1-941-Fälle", title = "Monatliche komplexe Diagnostiken")
  })

  output$diagnostic_block_details <- DT::renderDT({
    blocks <- diagnostic_filtered()
    show_cols <- intersect(
      c("datum", "patient", "diagnose", "primaerfall", "patientenfall",
        "morphologie", "immunphanotypisierung", "zytogenetik",
        "molekulargenetik", "jahr"),
      names(blocks)
    )
    DT::datatable(blocks[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_diagnostic_blocks <- shiny::downloadHandler(
    filename = function() paste0("OPS_1_941_Komplexe_Diagnostik_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(diagnostic_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Oncoprint
  # ------------------------------------------------------------------
  output$oncoprint_filters <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    onco_all <- tryCatch(
      oncoscopR::onc_parse_oncoprint(df, remove_negative = FALSE),
      error = function(e) NULL
    )
    shiny::validate(shiny::need(!is.null(onco_all) && nrow(onco_all) > 0,
                                "Keine verwertbaren Einträge in 'Krankheitsspezifische hematol Resultate'."))
    ent_choices <- sort(unique(stats::na.omit(onco_all$diagnose_label)))
    alt_choices <- onco_all |>
      dplyr::filter(.data$oncoprint_mutation) |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::pull(.data$alteration)
    shiny::tagList(
      shiny::selectizeInput("onco_entity_filter", "Entität/Diagnose",
                            choices = ent_choices, selected = ent_choices, multiple = TRUE),
      shiny::numericInput("onco_top_n", "Top-Alterationen anzeigen",
                          value = 25, min = 5, max = 100, step = 5),
      shiny::checkboxInput("onco_remove_negative", "Negative/NA-Befunde ausblenden", TRUE),
      shiny::selectizeInput("onco_alt_filter", "Optional: bestimmte Mutationen",
                            choices = alt_choices, selected = NULL, multiple = TRUE,
                            options = list(placeholder = "leer = automatisch Top-Alterationen")),
      shiny::checkboxInput("onco_show_patient_names", "Patientennamen in X-Achse anzeigen", FALSE)
    )
  })

  oncoprint_all_filtered <- shiny::reactive({
    df <- data_filtered()
    onco <- tryCatch(
      oncoscopR::onc_parse_oncoprint(df, remove_negative = isTRUE(input$onco_remove_negative)),
      error = function(e) oncoscopR:::.empty_alteration_table(include_oncoprint_flag = TRUE)
    )
    if (!is.null(input$onco_entity_filter) && length(input$onco_entity_filter) > 0) {
      onco <- dplyr::filter(onco, .data$diagnose_label %in% input$onco_entity_filter)
    }
    onco
  })

  oncoprint_long <- shiny::reactive({
    onco <- oncoprint_all_filtered() |> dplyr::filter(.data$oncoprint_mutation)
    if (!is.null(input$onco_alt_filter) && length(input$onco_alt_filter) > 0) {
      onco <- dplyr::filter(onco, .data$alteration %in% input$onco_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$onco_top_n), 25, input$onco_top_n)
      top_alts <- onco |>
        dplyr::count(.data$alteration, sort = TRUE) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::pull(.data$alteration)
      onco <- dplyr::filter(onco, .data$alteration %in% top_alts)
    }
    onco
  })

  oncoprint_structural <- shiny::reactive({
    oncoprint_all_filtered() |>
      dplyr::filter(!.data$oncoprint_mutation) |>
      dplyr::filter(!.data$alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA"))
  })

  output$n_onco_patients <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$patient_label), big.mark = "."))
  output$n_onco_alterations <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$alteration), big.mark = "."))
  output$n_onco_entities <- shiny::renderText(format(dplyr::n_distinct(oncoprint_long()$diagnose_label), big.mark = "."))

  oncoprint_plot_obj <- shiny::eventReactive(input$run_oncoprint, {
    onco <- oncoprint_long()
    shiny::validate(shiny::need(nrow(onco) > 0, "Keine echten Mutationen/Varianten im aktuellen Filter."))
    patient_order <- onco |>
      dplyr::distinct(.data$patient_label, .data$diagnose_label) |>
      dplyr::arrange(.data$diagnose_label, .data$patient_label) |>
      dplyr::mutate(patient_plot = if (isTRUE(input$onco_show_patient_names)) {
        .data$patient_label
      } else {
        paste0("Fall ", dplyr::row_number())
      })
    alt_order <- onco |> dplyr::count(.data$alteration, sort = TRUE) |> dplyr::pull(.data$alteration)
    plot_df <- onco |>
      dplyr::left_join(patient_order, by = c("patient_label", "diagnose_label")) |>
      dplyr::mutate(
        patient_plot   = factor(.data$patient_plot, levels = patient_order$patient_plot),
        alteration     = factor(.data$alteration, levels = rev(alt_order)),
        diagnose_label = factor(.data$diagnose_label, levels = unique(patient_order$diagnose_label))
      )
    ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$patient_plot, y = .data$alteration,
                                          fill = .data$alteration_class)) +
      ggplot2::geom_tile(color = "white", linewidth = 0.25) +
      ggplot2::facet_grid(. ~ diagnose_label, scales = "free_x", space = "free_x") +
      ggplot2::theme_minimal(base_size = 12) +
      ggplot2::theme(
        axis.text.x = ggplot2::element_text(angle = 90, hjust = 1, vjust = 0.5,
                                            size = if (isTRUE(input$onco_show_patient_names)) 7 else 5),
        panel.grid = ggplot2::element_blank(),
        strip.text.x = ggplot2::element_text(face = "bold", size = 10),
        legend.position = "bottom"
      ) +
      ggplot2::labs(
        title = "Oncoprint: echte Mutationen/Varianten",
        subtitle = "NA, negative Befunde und strukturelle/zytogenetische Alterationen sind aus dem Plot ausgeschlossen",
        x = "Patient/Fall", y = "Mutation/Variante", fill = "Typ"
      )
  }, ignoreNULL = FALSE)

  output$oncoprint_plot <- shiny::renderPlot({ p <- oncoprint_plot_obj(); shiny::req(p); print(p) })

  output$oncoprint_summary_table <- DT::renderDT({
    onco <- oncoprint_long()
    shiny::validate(shiny::need(nrow(onco) > 0, "Keine echten Mutationen/Varianten im aktuellen Filter."))
    tab <- onco |>
      dplyr::group_by(.data$diagnose_label, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Alterationstyp = paste(sort(unique(.data$alteration_class)), collapse = ", "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 20, scrollX = TRUE))
  })

  output$oncoprint_detail_table <- DT::renderDT({
    onco <- oncoprint_long()
    show_cols <- intersect(
      c("patient_label", "diagnose_label", "alteration", "alteration_class", "alteration_raw"),
      names(onco)
    )
    DT::datatable(onco[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 20, scrollX = TRUE))
  })

  output$oncoprint_structural_table <- DT::renderDT({
    structural <- oncoprint_structural()
    shiny::validate(shiny::need(nrow(structural) > 0, "Keine strukturellen/zytogenetischen Befunde im aktuellen Filter."))
    tab <- structural |>
      dplyr::group_by(.data$diagnose_label, .data$alteration_class, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Beispiele = paste(head(sort(unique(.data$alteration_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, .data$alteration_class,
                     dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_oncoprint_data <- shiny::downloadHandler(
    filename = function() paste0("Oncoprint_Mutationsdaten_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(oncoprint_long(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )
  output$download_structural_data <- shiny::downloadHandler(
    filename = function() paste0("Strukturelle_Zytogenetische_Befunde_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(oncoprint_structural(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )
  output$download_oncoprint_plot <- shiny::downloadHandler(
    filename = function() paste0("Oncoprint_", Sys.Date(), ".png"),
    content = function(file) {
      p <- oncoprint_plot_obj()
      ggplot2::ggsave(file, p, width = 15, height = 9, dpi = 150)
    }
  )

  # ------------------------------------------------------------------
  # Zytogenetik
  # ------------------------------------------------------------------
  output$cyto_filters <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    cyto_all <- tryCatch(
      oncoscopR::onc_parse_cytogenetics(df, remove_negative = FALSE),
      error = function(e) NULL
    )
    shiny::validate(shiny::need(!is.null(cyto_all) && nrow(cyto_all) > 0,
                                "Keine verwertbaren Einträge in 'Zytogenetik'."))
    ent_choices <- sort(unique(stats::na.omit(cyto_all$diagnose_label)))
    cyto_choices <- cyto_all |>
      dplyr::filter(!.data$alteration_class %in% c("negativ/kein Nachweis", "Nicht verwertbar/NA")) |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::pull(.data$alteration)
    shiny::tagList(
      shiny::selectizeInput("cyto_entity_filter", "Entität/Diagnose",
                            choices = ent_choices, selected = ent_choices, multiple = TRUE),
      shiny::numericInput("cyto_top_n", "Top-Befunde anzeigen",
                          value = 25, min = 5, max = 100, step = 5),
      shiny::checkboxInput("cyto_remove_negative", "Negative/NA-Befunde ausblenden", TRUE),
      shiny::selectizeInput("cyto_alt_filter", "Optional: bestimmte Zytogenetik-Befunde",
                            choices = cyto_choices, selected = NULL, multiple = TRUE,
                            options = list(placeholder = "leer = automatisch Top-Befunde"))
    )
  })

  cyto_all_filtered <- shiny::reactive({
    df <- data_filtered()
    cyto <- tryCatch(
      oncoscopR::onc_parse_cytogenetics(df, remove_negative = isTRUE(input$cyto_remove_negative)),
      error = function(e) oncoscopR:::.empty_alteration_table(include_oncoprint_flag = FALSE,
                                                              raw_name = "zytogenetik_raw")
    )
    if (!is.null(input$cyto_entity_filter) && length(input$cyto_entity_filter) > 0) {
      cyto <- dplyr::filter(cyto, .data$diagnose_label %in% input$cyto_entity_filter)
    }
    if (!is.null(input$cyto_alt_filter) && length(input$cyto_alt_filter) > 0) {
      cyto <- dplyr::filter(cyto, .data$alteration %in% input$cyto_alt_filter)
    } else {
      top_n <- ifelse(is.null(input$cyto_top_n), 25, input$cyto_top_n)
      top_alts <- cyto |>
        dplyr::count(.data$alteration, sort = TRUE) |>
        dplyr::slice_head(n = top_n) |>
        dplyr::pull(.data$alteration)
      cyto <- dplyr::filter(cyto, .data$alteration %in% top_alts)
    }
    cyto
  })

  output$n_cyto_patients <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$patient_label), big.mark = "."))
  output$n_cyto_alterations <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$alteration), big.mark = "."))
  output$n_cyto_entities <- shiny::renderText(format(dplyr::n_distinct(cyto_all_filtered()$diagnose_label), big.mark = "."))

  cyto_plot_obj <- shiny::eventReactive(input$run_cyto, {
    cyto <- cyto_all_filtered()
    shiny::validate(shiny::need(nrow(cyto) > 0, "Keine Zytogenetik-Befunde im aktuellen Filter."))
    tab <- cyto |>
      dplyr::count(.data$alteration, sort = TRUE) |>
      dplyr::arrange(.data$n) |>
      dplyr::mutate(alteration = factor(.data$alteration, levels = .data$alteration))
    ggplot2::ggplot(tab, ggplot2::aes(x = .data$alteration, y = .data$n)) +
      ggplot2::geom_col() + ggplot2::coord_flip() +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::labs(
        title = "Top-Zytogenetik-Befunde",
        subtitle = "Quelle: separate Spalte 'Zytogenetik'",
        x = "Zytogenetik-Befund", y = "Anzahl Fälle/Patienten"
      )
  }, ignoreNULL = FALSE)

  output$cyto_plot <- shiny::renderPlot({ p <- cyto_plot_obj(); shiny::req(p); print(p) })

  output$cyto_summary_table <- DT::renderDT({
    cyto <- cyto_all_filtered()
    shiny::validate(shiny::need(nrow(cyto) > 0, "Keine Zytogenetik-Befunde im aktuellen Filter."))
    tab <- cyto |>
      dplyr::group_by(.data$diagnose_label, .data$alteration_class, .data$alteration) |>
      dplyr::summarise(
        Patienten_Faelle = dplyr::n_distinct(.data$patient_label),
        Beispiele = paste(head(sort(unique(.data$zytogenetik_raw)), 5), collapse = " | "),
        .groups = "drop"
      ) |>
      dplyr::arrange(.data$diagnose_label, .data$alteration_class,
                     dplyr::desc(.data$Patienten_Faelle), .data$alteration)
    DT::datatable(tab, rownames = FALSE, options = list(pageLength = 25, scrollX = TRUE))
  })

  output$cyto_detail_table <- DT::renderDT({
    cyto <- cyto_all_filtered()
    show_cols <- intersect(
      c("patient_label", "diagnose_label", "alteration", "alteration_class", "zytogenetik_raw"),
      names(cyto)
    )
    DT::datatable(cyto[, show_cols, drop = FALSE], rownames = FALSE,
                  options = list(pageLength = 25, scrollX = TRUE))
  })

  output$download_cyto_data <- shiny::downloadHandler(
    filename = function() paste0("Zytogenetik_", Sys.Date(), ".csv"),
    content = function(file) {
      utils::write.csv(cyto_all_filtered(), file, row.names = FALSE,
                       fileEncoding = "UTF-8")
    }
  )

  # ------------------------------------------------------------------
  # Boxplots
  # ------------------------------------------------------------------
  output$box_ui <- shiny::renderUI({
    df <- data_filtered(); shiny::req(nrow(df) > 0)
    num_cols <- names(df)[vapply(df, is.numeric, logical(1L))]
    grp_cols <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1L))]
    shiny::tagList(
      shiny::selectInput("box_y", "Numerische Variable Y", choices = num_cols,
                         selected = if ("pfs" %in% num_cols) "pfs" else num_cols[1]),
      shiny::selectInput("box_x", "Gruppe X", choices = grp_cols,
                         selected = if ("kodierung" %in% grp_cols) "kodierung" else grp_cols[1]),
      shiny::checkboxInput("box_jitter", "Jitter-Punkte anzeigen", TRUE),
      shiny::checkboxInput("box_log", "Y-Achse log10", FALSE),
      shiny::textInput("box_title", "Titel", value = "Boxplot")
    )
  })

  box_plot_obj <- shiny::eventReactive(input$run_box, {
    df <- data_filtered()
    shiny::req(input$box_y, input$box_x)
    shiny::validate(shiny::need(nrow(df) > 1, "Zu wenige Daten nach Filter."))
    plot_df <- data.frame(
      x = as.factor(df[[input$box_x]]),
      y = suppressWarnings(as.numeric(df[[input$box_y]]))
    ) |> dplyr::filter(!is.na(.data$x), !is.na(.data$y))
    shiny::validate(shiny::need(nrow(plot_df) > 1, "Keine verwertbaren Daten für Boxplot."))
    p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = .data$x, y = .data$y)) +
      ggplot2::geom_boxplot(outlier.shape = NA) +
      ggplot2::theme_minimal(base_size = 13) +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1)) +
      ggplot2::labs(title = input$box_title, x = input$box_x, y = input$box_y)
    if (isTRUE(input$box_jitter)) p <- p + ggplot2::geom_jitter(width = 0.15, alpha = 0.6)
    if (isTRUE(input$box_log)) p <- p + ggplot2::scale_y_log10()
    p
  }, ignoreNULL = FALSE)

  output$box_plot <- shiny::renderPlot({ p <- box_plot_obj(); shiny::req(p); print(p) })

  output$download_box_plot <- shiny::downloadHandler(
    filename = function() paste0("Boxplot_", Sys.Date(), ".png"),
    content = function(file) {
      p <- box_plot_obj()
      ggplot2::ggsave(file, p, width = 10, height = 7, dpi = 150)
    }
  )
}

# Fallback KM plot when survminer is absent. Renders a base ggplot KM curve
# with optional CI from broom::tidy(survfit) if broom is available, else
# from a manual computation using survfit$surv/$std.err.
.km_base_plot <- function(fit, km_df, title, xlab, ylab, show_ci = TRUE) {
  s <- summary(fit)
  dat <- data.frame(
    time = s$time, surv = s$surv,
    upper = if (!is.null(s$upper)) s$upper else s$surv,
    lower = if (!is.null(s$lower)) s$lower else s$surv,
    strata = if (!is.null(s$strata)) as.character(s$strata) else "All"
  )
  p <- ggplot2::ggplot(dat, ggplot2::aes(x = .data$time, y = .data$surv,
                                         color = .data$strata, fill = .data$strata)) +
    ggplot2::geom_step() +
    ggplot2::theme_minimal(base_size = 13) +
    ggplot2::labs(title = title, x = xlab, y = ylab)
  if (show_ci) {
    p <- p + ggplot2::geom_ribbon(
      ggplot2::aes(ymin = .data$lower, ymax = .data$upper),
      alpha = 0.15, colour = NA
    )
  }
  p
}

shiny::shinyApp(ui, server)
