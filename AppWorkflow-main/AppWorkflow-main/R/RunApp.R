#' @title Run WorkFlow application
#' @description function to run the MicroWorkflow shiny application.
#'
#' @param Null no paramters are needed
#'
#' @author  .....
#' @import shinyFiles shinyjs stringr readxl ggthemes factoextra dplyr openxlsx reshape2 rstatix fs parallel shinybusy caret pheatmap httpuv clusterCrit bslib writexl
#' @rawNamespace import(DT, except=c(dataTableOutput,renderDataTable))
#' @rawNamespace import(shiny,except=runExample)
#' @rawNamespace import(shinyWidgets,except=alert)
#' @rawNamespace import(randomForest,except=combine)
#' @rawNamespace import(ggplot2,except=margin)
#' @rawNamespace import(xfun,except=c(file_exists,dir_create,dir_exists))
#'
#' @examples
#'\dontrun{
#' MicroWorkflow.run()
#' }
#' @export

MicroWorkflow.run <-function()
{
  x = T

  Appui <- system.file("Shiny","ui.R", package = "MicroWorkflow")
  Appserver <- system.file("Shiny","server.R", package = "MicroWorkflow")

  source(Appui)
  source(Appserver)

  app <-shinyApp(ui, server,
                 options =  options(shiny.maxRequestSize=1000*1024^2,
                                    shiny.launch.browser = .rs.invokeShinyWindowExternal)
  )

  app$staticPaths <- list(
    `/` = httpuv::staticPath(system.file("Shiny","www", package = "MicroWorkflow"), indexhtml = FALSE, fallthrough = TRUE)
  )

  browser_opt <- getOption("browser")
  can_launch_browser <- interactive() && !(is.character(browser_opt) && length(browser_opt) > 0 && !nzchar(browser_opt[[1]]))

  runApp(app, launch.browser = can_launch_browser)
}
