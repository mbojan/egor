if (getRversion() >= "2.15.1")
  utils::globalVariables(
    c(
      "clrs",
      "colors_",
      "e_colors",
      "edge.color",
      "edge.width",
      "layout_",
      "vertex.label",
      "vertex.size"
    )
  )

#' `egor` Network Visualization App
#'
#' Launches an interactive Shiny Web App, that creates a list of
#' `igraph` objects from an 'egor' object and offers the user several graphical
#' means of interacting with the visualization parameters for all networks in
#' the `egor` object.
#' @param object An `egor` object.
#' @param shiny_opts `List` of arguments to be passed to `shinyApp()`'s options argument.
#' @examples
#' if(interactive()){
#'   data("egor32")
#'   egor_vis_app(egor32)
#' }
#' @keywords ego-centered network analysis
#' @export
#' @import shiny
#' @importFrom igraph get.vertex.attribute
#' @importFrom igraph get.edge.attribute
#' @importFrom igraph E
#' @importFrom igraph V
#' @importFrom grDevices blues9 grey grey.colors heat.colors rainbow topo.colors
#' @importFrom graphics legend text
#' @importFrom tibble as_tibble
#' @importFrom igraph list.edge.attributes
#' @importFrom igraph list.vertex.attributes
#' @importFrom igraph V<-
#' @importFrom igraph E<-
egor_vis_app <- function(object = NULL,
                         shiny_opts = list(launch.browser = TRUE)) {
  # TODO:
  # make ego grams work for allbus data/ find issue
  
  # App Globals -------------------------------------------------------------
  IDVARS <- list(
    ego = ".egoID",
    alter = ".altID",
    source = ".srcID",
    target = ".tgtID"
  )
  egors <-
    ls(envir = .GlobalEnv)[sapply(mget(ls(envir = .GlobalEnv), envir = .GlobalEnv), function(x)
      class(x)[1] == "egor")]
  col_pal_names <-
    c(
      "Heat Colors",
      "Yellow-Green",
      "Red-Yellow",
      "Blue-Red",
      "Black-White",
      "Greys",
      "Rainbow",
      "Topo Colors"
    )
  
  shiny_opts <- c(shiny_opts, width = "20")
  object_enex <- as.character(enexpr(object))
  
  shinyApp(
    # UI ----------------------------------------------------------------------
    
    ui = fluidPage(
      title  = "egor's Network Visualization App",
      
      
      fluidRow(width = 12,
               plotOutput(
                 "Plot", width = "100%", height = 780
               )),
      
      # Sidebar
      fluidRow(
        width = 12,
        column(
          3,
          selectInput("egor",
                      "Select `egor` object",
                      egors,
                      selected = object_enex),
          uiOutput("choose_nnumber"),
          numericInput("x_dim", "Columns", 3, min = 1),
          numericInput("y_dim", "Rows", 2, min = 1),
          tags$div(
            title = "When including ego make sure that corresponding ego and alter variables have equal names in the `egor` object.",
            tags$div(checkboxInput("include_ego",
                                   "Include Ego",
                                   FALSE),
                     style = "float: left;"),
            tags$div(icon("question-circle"),
                     style = "float: left; margin-top: 12px; margin-left: -200px;")
          )
          
        ),
        column(
          3,
          sliderInput(
            "zoom_factor_v",
            label = "Vertex Size:",
            min = 0,
            max = 75,
            value = 15,
            step = .1
          ),
          sliderInput(
            "zoom_factor_e",
            label = "Edge Width:",
            min = 1,
            max = 10,
            value = 3,
            step = .1
          ),
          sliderInput(
            "font_size",
            label = "Font Size:",
            min = 0.5,
            max = 5,
            value = 1,
            step = .1
          )
        ),
        column(
          6,
          tabsetPanel(
            tabPanel(
              "Vertices",
              column(
                6,
                uiOutput("choose_v.size"),
                uiOutput("choose_v.color"),
                selectInput("v.color_pal",
                            "Color Palette:",
                            choices = col_pal_names)
              ),
              column(
                6,
                uiOutput("choose_v.label"),
                textInput("l.label", "Legend Label:")
              )
            ),
            tabPanel(
              "Edges",
              column(6,
                     uiOutput("choose_e.width"),
                     uiOutput("choose_e.color")),
              column(
                6,
                selectInput("e.color_pal", "Color Palette:", choices = col_pal_names)
              )
            ),
            tabPanel(
              "Plot Options",
              column(
                6,
                helpText("Select ego attributes and results do display on plot."),
                uiOutput("choose_disp.results")
              ),
              column(
                6,
                helpText(
                  "Select a venn and pie variable, to switch plot type to",
                  br(),
                  "'ego-socio-gram'."
                ),
                uiOutput("choose_venn_var"),
                uiOutput("choose_pie_var")
              ),
            ),
            tabPanel(
              "Sort & Filter",
              column(
                6,
                uiOutput("choose_sort_by"),
                uiOutput("choose_filter_var"),
                textInput("filter", "Filter Value:")
              ),
              
              column(
                6,
                uiOutput("choose_box_color"),
                selectInput("box_col_pal",
                            "Color Palette:",
                            choices = col_pal_names)
              )
            ),
            tabPanel("Export",
                     column(
                       3,
                       br(
                       downloadButton("save_plot", label = "Save this Plot")),
                       br(
                       downloadButton("save_all_plots", label = "Save all Plots"))
                     ))
          )
        )
        
      ),
      fluidRow(column(12, inputPanel(
        tags$div(style = "height: 250px; width: 100%;",
                 tags$p("egor's Network Visualization App"))
      )))
    ),
    
    # SERVER ------------------------------------------------------------------
    
    server = function(input, output) {
      values <- reactiveValues(default = 0)
      values$v.size <- "-Select Entry-"
      values$v.color <- "-Select Entry-"
      values$v.label <- "-Select Entry-"
      values$e.width <- "-Select Entry-"
      values$e.color <- "-Select Entry-"
      values$box_color <- "-Select Entry-"
      values$sort_by <- "-Select Entry-"
      values$filter_var <- "-Select Entry-"
      values$filter <- ""
      values$venn_var <- "-Select Entry-"
      values$pie_var <- "-Select Entry-"
      
      values$nnumber <- 1
      values$disp.results <- c()
      
      obj <- reactive({
        get(input$egor, envir = .GlobalEnv)
      })
      
      result_names <- reactive({
        rn <- names(obj()$ego)
      })
      
      r.atts <-
        reactive(make_select_vector(result_names(), e = TRUE))
      
      v.atts <-
        reactive(make_select_vector(names(obj()$alter)))
      e.atts <-
        reactive(make_select_vector(names(obj()$aatie), e = TRUE))
      
      output$choose_nnumber <- renderUI({
        numericInput(
          "nnumber",
          "Network No.:",
          step = input$x_dim * input$y_dim,
          min = 1,
          max = nrow(
            apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter)$ego
          ),
          value = 1
        )
      })
      
      output$choose_filter_var <- renderUI({
        selectInput("filter_var",
                    "Filter by:",
                    choices = (r.atts()))
      })
      outputOptions(output, "choose_filter_var", suspendWhenHidden = FALSE)
      
      output$choose_sort_by <- renderUI({
        selectInput("sort_by", "Sort by:", choices = (r.atts()))
      })
      outputOptions(output, "choose_sort_by", suspendWhenHidden = FALSE)
      
      output$choose_box_color <- renderUI({
        selectInput("box_color", "Highlight:", choices = r.atts())
      })
      outputOptions(output, "choose_box_color", suspendWhenHidden = FALSE)
      
      output$choose_v.size <- renderUI({
        selectInput("v.size", "Vertex Size:", choices = v.atts())
      })
      outputOptions(output, "choose_v.size", suspendWhenHidden = FALSE)
      
      output$choose_v.color <- renderUI({
        selectInput("v.color", "Vertex Color:", choices = v.atts())
      })
      outputOptions(output, "choose_v.color", suspendWhenHidden = FALSE)
      
      output$choose_v.label <- renderUI({
        selectInput("v.label", "Vertex Labels:", choices = v.atts())
      })
      outputOptions(output, "choose_v.label", suspendWhenHidden = FALSE)
      
      output$choose_e.width <- renderUI({
        selectInput("e.width", "Edge Width:", choices = e.atts())
      })
      outputOptions(output, "choose_e.width", suspendWhenHidden = FALSE)
      
      output$choose_e.color <- renderUI({
        selectInput("e.color", "Edge Color:", choices = e.atts())
      })
      outputOptions(output, "choose_e.color", suspendWhenHidden = FALSE)
      
      output$choose_venn_var <- renderUI({
        selectInput("venn_var", "Venn Variable:", choices = v.atts())
      })
      outputOptions(output, "choose_venn_var", suspendWhenHidden = FALSE)
      
      output$choose_pie_var <- renderUI({
        selectInput("pie_var", "Pie Variable:", choices = v.atts())
      })
      outputOptions(output, "choose_pie_var", suspendWhenHidden = FALSE)
      
      output$choose_disp.results <- renderUI({
        selectInput("disp.results",
                    "Results:",
                    choices = result_names(),
                    multiple = TRUE)
      })
      outputOptions(output, "choose_disp.results", suspendWhenHidden = FALSE)
      
      observeEvent(input$box_color, {
        values$box_color <- input$box_color
      })
      observeEvent(input$v.size, {
        values$v.size <- input$v.size
      })
      observeEvent(input$filter, {
        values$filter <- input$filter
      })
      observeEvent(input$filter_var, {
        values$filter_var <- input$filter_var
      })
      observeEvent(input$sort_by, {
        values$sort_by <- input$sort_by
      })
      observeEvent(input$v.color, {
        values$v.color <- input$v.color
      })
      observeEvent(input$v.label, {
        values$v.label <- input$v.label
      })
      observeEvent(input$e.width, {
        values$e.width <- input$e.width
      })
      observeEvent(input$e.color, {
        values$e.color <- input$e.color
      })
      observeEvent(input$nnumber, {
        values$nnumber <- input$nnumber
      })
      observeEvent(input$disp.results, {
        values$disp.results <- input$disp.results
      }, ignoreNULL = FALSE)
      
      observeEvent(input$venn_var, {
        values$venn_var <- input$venn_var
      })
      
      observeEvent(input$pie_var, {
        values$pie_var <- input$pie_var
      })
      
      # plot Output -------------------------------------------------------------
      
      output$Plot <-
        renderPlot({
          plot(
            apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter),
            values$nnumber,
            input$x_dim,
            input$y_dim,
            venn_var = values$venn_var,
            pie_var = values$pie_var,
            vertex_size_var = if (values$v.size != "-Select Entry-")
              values$v.size
            else
              NULL,
            vertex_color_var = if (values$v.color != "-Select Entry-")
              values$v.color
            else
              NULL,
            vertex_color_palette = input$v.color_pal,
            vertex_color_legend_label = input$l.label,
            vertex_label_var =  if (values$v.label != "-Select Entry-")
              input$v.label
            else
              NULL,
            edge_width_var = if (values$e.width != "-Select Entry-")
              values$e.width
            else
              NULL,
            edge_color_var = if (values$e.color != "-Select Entry-")
              values$e.color
            else
              NULL,
            edge_color_palette = input$e.color_pal,
            highlight_box_col_var = if (values$box_color != "-Select Entry-")
              values$box_color
            else
              NULL,
            highlight_box_col_palette = input$box_col_pal,
            res_disp_vars  = values$disp.results,
            vertex_zoom = input$zoom_factor_v,
            edge_zoom = input$zoom_factor_e,
            font_size = input$font_size,
            include_ego = input$include_ego,
            type = if (values$venn_var != "-Select Entry-" &
                       values$pie_var != "-Select Entry-")
              "egogram"
            else
              "egograph"
          )
        })
      
      output$save_all_plots <- downloadHandler(
        filename = function() {
          paste(input$egor, "_plots_export", ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, width = 16, onefile = TRUE)
          for (i in seq(1,
                        nrow(
                          apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter)$ego
                        ),
                        by = input$x_dim * input$y_dim)) {
            plot(
              apply_sort_filter_to_obj(
                obj(),
                values$sort_by,
                values$filter_var,
                values$filter
              ),
              i,
              input$x_dim,
              input$y_dim,
              venn_var = values$venn_var,
              pie_var = values$pie_var,
              vertex_size_var = if (values$v.size != "-Select Entry-")
                values$v.size
              else
                NULL,
              vertex_color_var = if (values$v.color != "-Select Entry-")
                values$v.color
              else
                NULL,
              vertex_color_palette = input$v.color_pal,
              vertex_color_legend_label = input$l.label,
              vertex_label_var =  if (values$v.label != "-Select Entry-")
                input$v.label
              else
                NULL,
              edge_width_var = if (values$e.width != "-Select Entry-")
                values$e.width
              else
                NULL,
              edge_color_var = if (values$e.color != "-Select Entry-")
                values$e.color
              else
                NULL,
              edge_color_palette = input$e.color_pal,
              highlight_box_col_var = if (values$box_color != "-Select Entry-")
                values$box_color
              else
                NULL,
              highlight_box_col_palette = input$box_col_pal,
              res_disp_vars  = values$disp.results,
              vertex_zoom = input$zoom_factor_v,
              edge_zoom = input$zoom_factor_e,
              font_size = input$font_size,
              include_ego = input$include_ego,
              type = if (values$venn_var != "-Select Entry-" &
                         values$pie_var != "-Select Entry-")
                "egogram"
              else
                "egograph"
              
            )
          }
          dev.off()
        }
      )
      
      output$save_plot <- downloadHandler(
        filename = function() {
          paste(input$egor, "_", values$nnumber, ".pdf", sep = "")
        },
        content = function(file) {
          pdf(file, width = 16)
          plot(
            apply_sort_filter_to_obj(obj(), values$sort_by, values$filter_var, values$filter),
            values$nnumber,
            input$x_dim,
            input$y_dim,
            venn_var = values$venn_var,
            pie_var = values$pie_var,
            vertex_size_var = if (values$v.size != "-Select Entry-")
              values$v.size
            else
              NULL,
            vertex_color_var = if (values$v.color != "-Select Entry-")
              values$v.color
            else
              NULL,
            vertex_color_palette = input$v.color_pal,
            vertex_color_legend_label = input$l.label,
            vertex_label_var =  if (values$v.label != "-Select Entry-")
              input$v.label
            else
              NULL,
            edge_width_var = if (values$e.width != "-Select Entry-")
              values$e.width
            else
              NULL,
            edge_color_var = if (values$e.color != "-Select Entry-")
              values$e.color
            else
              NULL,
            edge_color_palette = input$e.color_pal,
            highlight_box_col_var = if (values$box_color != "-Select Entry-")
              values$box_color
            else
              NULL,
            highlight_box_col_palette = input$box_col_pal,
            res_disp_vars  = values$disp.results,
            vertex_zoom = input$zoom_factor_v,
            edge_zoom = input$zoom_factor_e,
            font_size = input$font_size,
            type = if (values$venn_var != "-Select Entry-" &
                       values$pie_var != "-Select Entry-")
              "egogram"
            else
              "egograph"
          )
          dev.off()
        }
      )
      
    },
    options = shiny_opts
  )
}


# Server Functions --------------------------------------------------------

apply_sort_filter_to_obj <- function(x, sort_by, filter_var, filter_) {
  if (sort_by != "-Select Entry-")
    x <- arrange(x, !!sym(sort_by))
  if (filter_var != "-Select Entry-" & filter_ != "")
    x <- filter.egor(x, !!sym(filter_var) %in% !!filter_)
  x
}

make_select_vector <- function(x, e = FALSE) {
  if (e)
    c("-Select Entry-", x[!x %in% IDVARS])
  else
    c("-Select Entry-", "name", x[!x %in% IDVARS])
}

filter_by <- function(df, ...) {
  filter_conditions <- rlang::quos(...)
  dplyr::filter(df, !!!filter_conditions)
}

egor_col_pal <- function(pal_name = "Heat Colors", n) {
  if (pal_name == "Heat Colors")
    # Heat Colors
    pal <- heat.colors(n)
  if (pal_name == "Yellow-Green")
    # Yellow-Green
    pal <- rainbow(n, start = 0.2, end = 0.4)
  if (pal_name == "Red-Yellow")
    # Red-Yellow
    pal <- rainbow(n, start = 0.0, end = 0.2)
  if (pal_name == "Blue-Red")
    # Blue-Red
    pal <- rainbow(n, start = 0.6, end = 1)
  if (pal_name == "Black-White")
    # Black-White
    pal <- grey.colors(n, start = 0, end = 1)
  if (pal_name == "Greys")
    # Greys
    pal <- grey.colors(n)
  if (pal_name == "Rainbow")
    # Rainbow
    pal <- rainbow(n)
  if (pal_name == "Topo Colors")
    # Topo Colors
    pal <- topo.colors(n)
  rev(pal)
}
