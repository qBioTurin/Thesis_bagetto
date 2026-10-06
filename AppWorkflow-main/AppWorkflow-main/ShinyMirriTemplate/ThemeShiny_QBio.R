# ── Theme ──────────────────────────────────────────────────────────────────────
# Single CSS entry-point: QBio_Theme.css  (replaces Settings.css +
# Components.css + ModernUi.css + ButtonsNavbar.css)
# font.css is kept for font-class utilities.
# No inline style tags here — all rules live in QBio_Theme.css.
# ───────────────────────────────────────────────────────────────────────────────
theme_QBio <- list(
  includeCSS("ShinyMirriTemplate/Style/QBio_Theme.css"),
  includeCSS("ShinyMirriTemplate/Style/font.css"),
  includeHTML("ShinyMirriTemplate/head.html")
)


# ── Header ─────────────────────────────────────────────────────────────────────
headerPanel <- function(title = "", windowTitle = title) {
  tagList(
    tags$head(
      tags$title(windowTitle),
      tags$style(HTML("
        /* Cat / microbe animation — kept here because it is header-specific */
        .headerMirri .cat-runway {
          position: absolute;
          left: 0; right: 0; bottom: 0;
          height: 160px;
          pointer-events: none;
          z-index: 5;
        }

        .headerMirri .cat-runner,
        .headerMirri .microbe-runner {
          position: absolute;
          bottom: -2px;
          height: 96px;
          width: auto;
          display: none;
          z-index: 6;
          transform-origin: center center;
          filter: drop-shadow(0 1px 1px rgba(0,0,0,0.15));
        }

        .headerMirri .cat-runner  { left: -200px; }
        .headerMirri .microbe-runner {
          left: -200px; bottom: 0;
          z-index: 7;
          transform: scale(0.82);
          filter: drop-shadow(0 1px 1px rgba(0,0,0,0.12));
        }

        .headerMirri .cat-runner.running     { display: block; animation: cat-run-ltr    4s linear forwards; }
        .headerMirri .microbe-runner.running { display: block; animation: microbe-run-ltr 4s linear forwards; }
        .headerMirri .cat-runner.running.rtl     { animation-name: cat-run-rtl;     transform: scaleX(-1); }
        .headerMirri .microbe-runner.running.rtl { animation-name: microbe-run-rtl; transform: scaleX(-1); }

        @keyframes cat-run-ltr     { from { left: -200px; } to { left: calc(100% + 200px); } }
        @keyframes cat-run-rtl     { from { left: calc(100% + 200px); } to { left: -200px; } }
        @keyframes microbe-run-ltr { from { left: -200px; } to { left: calc(100% + 200px); } }
        @keyframes microbe-run-rtl { from { left: calc(100% + 200px); } to { left: -200px; } }
      ")),
      tags$script(HTML("
        document.addEventListener('DOMContentLoaded', function () {
          const cat    = document.getElementById('header-cat');
          const microbe = document.getElementById('header-microbe');
          if (!cat || !microbe || cat.dataset.initialized === '1') return;

          cat.dataset.initialized = '1';

          const speedPxPerSec = 320;
          const travelExtraPx = 160;
          const minCycleMs    = 30000;
          const maxCycleMs    = 60000;
          let direction       = 'ltr';

          function runDurationMs() {
            const runway   = cat.closest('.cat-runway');
            const widthPx  = runway ? runway.clientWidth : window.innerWidth;
            const distance = Math.max(widthPx + travelExtraPx, 320);
            return Math.round((distance / speedPxPerSec) * 1000);
          }

          function nextDelayMs() {
            return Math.floor(Math.random() * (maxCycleMs - minCycleMs + 1)) + minCycleMs;
          }

          function runPair() {
            const dur          = runDurationMs();
            const microbeOffset = direction === 'rtl' ? -90 : 130;

            [cat, microbe].forEach(function(el) {
              el.classList.remove('running', 'rtl');
              void el.offsetWidth;
              el.style.animationDuration = (dur / 1000) + 's';
              if (direction === 'rtl') el.classList.add('rtl');
              el.classList.add('running');
            });
            microbe.style.marginLeft = microbeOffset + 'px';

            window.setTimeout(function() {
              cat.classList.remove('running', 'rtl');
              microbe.classList.remove('running', 'rtl');
              microbe.style.marginLeft = '0px';
              direction = direction === 'ltr' ? 'rtl' : 'ltr';
            }, dur);
          }

          function scheduleNextRun() {
            window.setTimeout(function() { runPair(); scheduleNextRun(); }, nextDelayMs());
          }

          scheduleNextRun();
        });
      "))
    ),
    htmltools::div(
      class = "headerMirri",
      htmltools::img(
        src   = knitr::image_uri("ShinyMirriTemplate/Loghi/loghi_Mirri.png"),
        alt   = "logo",
        style = "height:3em; padding:4px 0 4px 0;"
      ),
      tags$img(
        src   = "image/title.png",
        alt   = "MicroWorkflow",
        style = "max-width:280px; width:28vw; min-width:140px; height:auto; padding:4px 0;"
      ),
      htmltools::div(
        class = "cat-runway",
        tags$img(id = "header-microbe", class = "microbe-runner",
                 src = "gif/bicor.gif",       alt = "running microbe"),
        tags$img(id = "header-cat",    class = "cat-runner",
                 src = "gif/running_cat.gif", alt = "running cat")
      )
    )
  )
}


# ── Footer ─────────────────────────────────────────────────────────────────────
footerPanel <- function(title = "", windowTitle = title) {
  tagList(
    tags$footer(),
    htmltools::div(
      class = "footerMirri",
      htmltools::div(
        class = "row",
        htmltools::div(
          class = "row",
          htmltools::div(class = "col-md-2"),
          htmltools::div(
            class = "col-md-3",
            style = "font-family: 'Martel Sans', sans-serif;",
            htmltools::div(
              style = "margin: 7.6%;",
              h4("SUS-MIRRI.IT Headquarter"),
              h5(HTML("University of Turin<br>
                       Via Verdi 8, 10124, Turin, Italy<br>
                       Scientific Coordinator: Prof. Cristina Varese"),
                 style = "font-size:13px;")
            )
          ),
          htmltools::div(
            class = "col-md-5",
            style = "font-size:13px; font-family:'Martel Sans',sans-serif;",
            htmltools::div(
              style = "margin: 5% 2.5%;",
              h5(class = "text-right",
                 HTML("This web platform was produced under workpackage 3 of the project
                  \"<strong>Strengthening the MIRRI Italian Research Infrastructure for
                  Sustainable Bioscience and Bioeconomy</strong>\" (SUS-MIRRI.IT) and has
                  received funding from the \"PNRR M4C2 Iniziativa 3.1 – Infrastrutture
                  di Ricerca (IR), cod. MUR IR0000005\"."))
            )
          ),
          htmltools::div(class = "col-md-2")
        ),
        htmltools::div(
          class = "col-md-12 text-center",
          style = "font-size:13px;",
          h5("© 2024 – SUS-MIRRI.IT mBRC.")
        )
      )
    )
  )
}


# ── Layout helpers ─────────────────────────────────────────────────────────────
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
  h3(id    = outputId,
     class = "shiny-text-output",
     class = if (!placeholder) "noplaceholder")
}

varbatimTextOutput_p <- function(outputId, placeholder = FALSE) {
  p(id    = outputId,
    class = "shiny-text-output",
    class = if (!placeholder) "noplaceholder")
}
