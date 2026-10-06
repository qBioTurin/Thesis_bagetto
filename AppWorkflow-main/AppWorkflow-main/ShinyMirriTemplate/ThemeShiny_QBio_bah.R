# ============================================================
#  ThemeShiny_QBio.R — Tema aggiornato (versione minimalista)
#  Usa un unico CSS invece di tre file sovrapposti.
# ============================================================

theme_QBio <- list(
  includeCSS("ShinyMirriTemplate/Style/font.css"),      # font Google (invariato)
  includeCSS("ShinyMirriTemplate/Style/QBio_Theme.css"), # NUOVO — sostituisce Settings + Components + ModernUi
  includeHTML("ShinyMirriTemplate/head.html")            # Google Fonts preconnect (invariato)
)

# ---- Header -------------------------------------------------------
headerPanel <- function(title = "", windowTitle = title) {
  tagList(
    tags$head(tags$title(windowTitle)),
    htmltools::div(
      class = "headerMirri",
      htmltools::img(
        src   = knitr::image_uri("ShinyMirriTemplate/Loghi/loghi_Mirri.png"),
        alt   = "logo",
        style = "height: 2.8em; padding: 4px 0;"
      ),
      h1(title, style = "margin: 0; padding-right: 8px; color: #2E2E3A;")
    )
  )
}

# ---- Footer -------------------------------------------------------
footerPanel <- function(title = "", windowTitle = title) {
  tagList(
    tags$footer(),
    htmltools::div(
      class = "footerMirri",
      htmltools::div(
        class = "row",
        style = "width:100%;",
        htmltools::div(
          class = "col-md-2"   # spacer
        ),
        htmltools::div(
          class = "col-md-3",
          style = "font-family: 'Martel Sans', sans-serif; margin: 7%;",
          h4("SUS-MIRRI.IT Headquarter"),
          h5(HTML("University of Turin<br>
                   Via Verdi 8, 10124, Turin, Italy<br>
                   Scientific Coordinator: Prof. Cristina Varese"))
        ),
        htmltools::div(
          class = "col-md-5",
          style = "font-size: 0.82em; font-family: 'Martel Sans', sans-serif;",
          htmltools::div(
            style = "margin: 5% 2.5%;",
            h5(HTML('This web platform was produced under the workpackage 3 of the project
                     "<strong>Strengthening the MIRRI Italian Research Infrastructure for
                     Sustainable Bioscience and Bioeconomy</strong>" (SUS-MIRRI.IT) and has
                     received funding from the "PNRR M4C2 Iniziativa 3.1 - Infrastrutture di
                     Ricerca (IR), cod. MUR IR0000005".'))
          )
        ),
        htmltools::div(
          class = "col-md-2"   # spacer
        )
      ),
      htmltools::div(
        class = "col-md-12 text-center",
        style = "font-size: 0.82em; width: 100%; padding-top: 8px;",
        h5("© 2024 - SUS-MIRRI.IT mBRC.")
      )
    )
  )
}

# ---- Helpers (invariati, compatibilità con UI.R) ------------------
mainPanel <- function(..., width = 8) {
  htmltools::div(
    class = paste0("col-sm-", width),
    tags$form(role = "main", class = "well", ...)
  )
}

sidebarPanel <- function(..., width = 4) {
  htmltools::div(
    class = paste0("col-sm-", width),
    role  = "complementary",
    ...
  )
}

varbatimTextOutput_h3 <- function(outputId, placeholder = FALSE) {
  h3(
    id    = outputId,
    class = "shiny-text-output",
    class = if (!placeholder) "noplaceholder"
  )
}

varbatimTextOutput_p <- function(outputId, placeholder = FALSE) {
  p(
    id    = outputId,
    class = "shiny-text-output",
    class = if (!placeholder) "noplaceholder"
  )
}
