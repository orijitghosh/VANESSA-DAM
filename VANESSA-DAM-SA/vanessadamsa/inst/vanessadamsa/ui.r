library(shiny)
library(shinythemes)
library(WaveletComp)
library(dplyr)
library(shinydashboard)
library(shinycustomloader)
library(shinyalert)
library(colourpicker)
library(shinyWidgets)
library(shinyhelper)
library(ggplot2)
source("helpers.R")
shinyUI <-
  (
    navbarPage(
      "VANESSA-DAM-Sleep",
      theme = shinytheme("sandstone"),
      collapsible = TRUE,
      fluid = TRUE,
      position = c("static-top"),
      id = "tabs",
      tabPanel(
        "Data input",
        icon = icon("table"),
        useShinyalert(),
        sidebarPanel(
          width = 2,
          fileInput(
            "data",
            "Choose Monitor Files",
            multiple = TRUE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          # tags$hr(),
          fileInput(
            "meta",
            "Choose Metadata File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ) %>%
            helper(
              type = "inline",
              title = "Metadata file",
              content = c(
                "Your Metadata file should be a comma separated file and have these following <b>six</b> columns:",
                "<i>file</i>, <i>start_datetime</i>, <i>stop_datetime</i>, <i>region_id</i>, <i>genotype</i>, <i>replicate</i>",
                "The Metadata file can be generated from within the app from the <b>DATA FORMATTING</b> tab"
              ),
              size = "m",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "modtau",
            "Modulo tau for wrapping",
            24, 0, 24
          ) %>%
            helper(
              type = "inline",
              title = "Modulo tao for wrapping",
              content = c(
                "Use modulo tao for the particular genotype, this will be used for averaging over subjective days in case your experiment was in DD.",
                "<b>REMEMBER: the same modulo tao will be used for ALL genotypes and monitors. So change this wisely.</b>"
              ),
              size = "m",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "ldperiod",
            "LD cycle period",
            24, 0, 24
          ) %>%
            helper(
              type = "inline",
              title = "LD cycle period",
              content = c(
                "This value will be used to determine light dark shading in the plots."
              ),
              size = "m",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "min",
            "Summary time window in minutes",
            15, 0, 240
          ) %>%
            helper(
              type = "inline",
              title = "Summary window",
              content = c("The app will detect the binning in your data, this bin value will be used to plot graphs"),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "light",
            "Duration of light in hours",
            12, 0, 24
          ) %>%
            helper(
              type = "inline",
              title = "Duration of light in hours",
              content = c(
                "This value will be used for light dark shading of the plots.",
                "The light part will be determined by the starting of your <i>start_datetime</i> value in metadata file."
              ),
              size = "m",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "genotype",
            "Number of monitors",
            1, 1, 12
          ) %>%
            helper(
              type = "inline",
              title = "Number of Monitors",
              content = c("Enter the total number of Monitor files you want to analyze."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "replicate",
            "Number of replicates",
            1, 1, 12
          ),
          numericInput(
            "remove",
            "How many minimum days flies should be alive to be counted?",
            2,
            0,
            30
          ),
          numericInput(
            "start",
            "Starting day",
            1, 1, 30
          ) %>%
            helper(
              type = "inline",
              title = "Starting day",
              content = c("Subset your data, Starting day 1 will be 1. <i>leave out transition days</i>"),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput(
            "end",
            "Ending day",
            3, 1, 30
          ) %>%
            helper(
              type = "inline",
              title = "Ending day",
              content = c("Subset your data, if your data is 8 days long, the 8th day will be ending day 7. <i>leave out transition days</i>"),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          # submitButton("Update Values on all fields", icon("refresh"), width = "200px"),
          withBusyIndicatorUI(
            actionBttn(
              inputId = "cal",
              label = "Start calculations!",
              style = "minimal",
              color = "primary",
              icon = icon("calculator")
            )
          ) %>%
            helper(
              type = "inline",
              title = "Calculations done by pressing this button",
              content = c("By pressing this button you will curate your data (remove data when an individual is dead), remove individuals which were dead before the number of days you specified in the <i>How many minimum days flies should be alive to be counted?</i> box. All calculations will also be done in this period."),
              size = "m",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            )
        ),
        sidebarPanel(
          colourInput("col1", "Select colour", "#005900", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col2", "Select colour", "#2796A3", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col3", "Select colour", "#DF7000", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col4", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col5", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col6", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col7", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col8", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col9", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col10", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col11", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          colourInput("col12", "Select colour", allowTransparent = TRUE, returnName = TRUE),
          width = 2
        ),
        # submitButton("Update Values on all fields", icon("refresh"), width = "200px"),
        mainPanel(box(
          width = 12,
          div(
            style = "overflow-x: scroll",
            DT::dataTableOutput("contents") %>% withLoader(type = "html", loader = "pacman")
          ),
          tags$hr(),
          downloadBttn(
            outputId = "report",
            label = "Generate report",
            style = "minimal",
            color = "primary"
          )
        ))
      ),

      tabPanel(
        "Sleep Profiles",
        icon = icon("chart-area"),
        mainPanel(
          tags$hr(),
          navlistPanel(
            " Figures",
            tabPanel(
              "All ethograms"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All sleep ethograms for all individuals for the chosen days will be shown."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("alletho_height", "height", 2500, 500, 10000, 50),
                  numericInput("alletho_width", "width", 1500, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotalletho",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "alletho"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Curated ethograms"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All curated sleep ethograms for all individuals for the chosen days will be shown."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("curatedetho_height", "height", 2500, 500, 10000, 50),
                  numericInput("curatedetho_width", "width", 1500, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotcuratedetho",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "curatedetho"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Curated ethograms wrapped"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All curated sleep ethograms for all individuals for the chosen days averaged across days will be shown."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("curatedetho_wrap_height", "height", 2500, 500, 10000, 50),
                  numericInput("curatedetho_wrap_width", "width", 1000, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotcuratedetho_wrap",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "curatedetho_wrap"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Plots over days individual"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("Sleep will be plotted for each individual over chosen days."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("popplot_height", "height", 6000, 3000, 10000, 50),
                  numericInput("popplot_width", "width", 1500, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotpopplot",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "popplot",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Average plot wrapped individual"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("Sleep will be plotted for each individual, averaged over chosen days."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("popplotwrap_height", "height", 1400, 800, 3000, 50),
                  numericInput("popplotwrap_width", "width", 1500, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotpopplotwrap",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "popplotwrap",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Average plots over days"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("For each genotype sleep will be plotted over chosen days after averaging over all inidividuals."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("popplot1_height", "height", 300, 100, 1000, 50),
                  numericInput("popplot1_width", "width", 1200, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotpopplot1",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "popplot1",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Average plot wrapped"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("For each genotype sleep will be plotted averaged over chosen days after averaging over all inidividuals."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("popplotwrap1_height", "height", 300, 100, 1000, 50),
                  numericInput("popplotwrap1_width", "width", 1000, 500, 10000, 50),
                  actionBttn(
                    inputId = "plotpopplotwrap1",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "popplotwrap1",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Average plot wrapped polar"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("For each genotype sleep will be plotted averaged over chosen days after averaging over all inidividuals in a circular scale."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("popplotwrap1polar_height", "height", 1000, 300, 1500, 50),
                  numericInput("popplotwrap1polar_width", "width", 1000, 300, 1500, 50),
                  actionBttn(
                    inputId = "plotpopplotwrap1polar",
                    label = "Plot",
                    style = "minimal",
                    color = "primary",
                    icon = icon("forward")
                  )
                ),
                tags$hr(),
                plotOutput(
                  "popplotwrap1polar",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Download data"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All sleep data of each individual will be downloaded as a <b>.csv</b> file. The details of the content and how to navigate through it is available in the tutorial file."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                downloadBttn(
                  outputId = "downloadData_chi_sq",
                  label = "Download data",
                  style = "minimal",
                  color = "primary"
                ),
                downloadBttn(
                  outputId = "downloadData_chi_sq_new",
                  label = "Download data bout details",
                  style = "minimal",
                  color = "primary"
                ),
                tags$hr(),
                DT::dataTableOutput("periodpower") %>% withLoader(type = "html", loader = "pacman")
              )
            )
          )
        )
      ),
      tabPanel("Sleep fractions",
        icon = icon("chart-bar"),
        navlistPanel(
          "Data overview",
          tabPanel(
            "Sleep fraction"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Sleep fraction of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("popplotwrapbox_height", "height", 700, 300, 2000, 50),
                numericInput("popplotwrapbox_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "popplotwrapbox_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotpopplotwrapbox",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("popplotwrapbox") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Sleep fraction summary"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Sleep fractions in light and dark part of the day of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("popplotwrapboxmelt_height", "height", 700, 300, 2000, 50),
                numericInput("popplotwrapboxmelt_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "popplotwrapboxmelt_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotpopplotwrapboxmelt",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("popplotwrapboxmelt") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Time spent sleeping"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Total sleep in different days of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("total_sleep_height", "height", 700, 300, 2000, 50),
                numericInput("total_sleep_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "total_sleep_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plottotal_sleep",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("total_sleep") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Time spent awake"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Total awake time in different days of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("total_awake_height", "height", 700, 300, 2000, 50),
                numericInput("total_awake_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "total_awake_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plottotal_awake",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("total_awake") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Time spent sleeping in day and night"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Total sleep time in light and day parts of different days of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("total_sleep_phase_height", "height", 700, 300, 2000, 50),
                numericInput("total_sleep_phase_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "total_sleep_phase_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plottotal_sleep_phase",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("total_sleep_phase") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Time spent awake in day and night"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Total awake time in light and day parts of different days of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("total_awake_phase_height", "height", 700, 300, 2000, 50),
                numericInput("total_awake_phase_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "total_awake_phase_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plottotal_awake_phase",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("total_awake_phase") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Activity Index"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Activity index (total awake time activity counts/total awake minutes) of different days of different genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("act_index_height", "height", 700, 300, 2000, 50),
                numericInput("act_index_width", "width", 1000, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "act_index_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotact_index",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("act_index") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          widths = c(3, 9)
          # )
        )
      ),
      tabPanel("Bout analysis",
        icon = icon("chart-bar"),
        navlistPanel(
          "Data overview",
          tabPanel(
            "Bouts"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("All bouts of different genotypes will be plotted averaged over days and individuals."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("bout_height", "height", 300, 100, 1000, 50),
                numericInput("bout_width", "width", 1000, 500, 10000, 50),
                actionBttn(
                  inputId = "plotbout",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("bout") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Number of bouts"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Number of bouts of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("numbouts_height", "height", 500, 300, 1000, 50),
                numericInput("numbouts_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "numbouts_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotnumbouts",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("numbouts") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Number of awake bouts"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Number of awake bouts of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("numbouts_awake_height", "height", 500, 300, 1000, 50),
                numericInput("numbouts_awake_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "numbouts_awake_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotnumbouts_awake",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("numbouts_awake") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Mean bout length"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Mean lengths of bouts of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("meanboutlength_height", "height", 500, 300, 2000, 50),
                numericInput("meanboutlength_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "meanboutlength_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotmeanboutlength",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("meanboutlength") %>% withLoader(type = "html", loader = "pacman")
            )
          ),

          tabPanel(
            "Mean awake bout length"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Mean lengths of awake bouts of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("meanboutlength_awake_height", "height", 500, 300, 2000, 50),
                numericInput("meanboutlength_awake_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "meanboutlength_awake_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotmeanboutlength_awake",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("meanboutlength_awake") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Number of bouts in light and dark phase"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Number of bouts separately in the light and dark part of the day of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("numbouts_ld_height", "height", 500, 300, 2000, 50),
                numericInput("numbouts_ld_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "numbouts_ld_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotnumbouts_ld",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("numbouts_ld") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Mean bout length in light and dark phase"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Mean lengths of bouts separately in the light and dark part of the day of differemt genotypes will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("meanboutlength_ld_height", "height", 500, 300, 2000, 50),
                numericInput("meanboutlength_ld_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "meanboutlength_ld_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotmeanboutlength_ld",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("meanboutlength_ld") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Mean bout length distribution"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Distriubtion of mean lengths of bouts of differemt genotypes will be plotted as <i>density ridge plots</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("meanboutlength_distrib_height", "height", 500, 300, 2000, 50),
                numericInput("meanboutlength_distrib_width", "width", 700, 500, 10000, 50),
                actionBttn(
                  inputId = "plotmeanboutlength_distrib",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("meanboutlength_distrib") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Latency to first bout"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Latency to first bout of differemt genotypes will be plotted as violin plots. Latency is calculated as minutes from ZT0. So, naturally the dark phase latency will be much larger, in case you want to calculate dark phase latency, substract total light hours in minutes from the reported dark phase latency value"),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("latency_height", "height", 500, 300, 1000, 50),
                numericInput("latency_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "latency_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotlatency",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("latency") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Latency to first bout in light and dark"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Latency to first bout of differemt genotypes in light and dark phases will be plotted as violin plots."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("latency_ld_height", "height", 500, 300, 1000, 50),
                numericInput("latency_ld_width", "width", 700, 500, 10000, 50),
                awesomeCheckbox(
                  inputId = "latency_ld_text",
                  label = "Print mean values on plot",
                  value = TRUE
                ),
                actionBttn(
                  inputId = "plotlatency_ld",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("latency_ld") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Bout summary"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Number of bouts vs mean lengths of bouts will be plotted as a scattered plot."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("boutsummary_height", "height", 300, 100, 1000, 50),
                numericInput("boutsummary_width", "width", 1000, 500, 10000, 50),
                actionBttn(
                  inputId = "plotboutsummary",
                  label = "Plot",
                  style = "minimal",
                  color = "primary",
                  icon = icon("forward")
                )
              ),
              tags$hr(),
              plotOutput("boutsummary") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          widths = c(3, 9)
        )
      ),
      tabPanel(
        "Data formatting",
        icon = icon("database"),
        sidebarPanel(
          width = 3,
          textInput("monitorname1", label = "Monitor#1 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime1",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          airDatepickerInput(
            inputId = "enddatetime1",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype1_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate1_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype1_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate1_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype1_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate1_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype1_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate1_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname2", label = "Monitor#2 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime2",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime2",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype2_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate2_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype2_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate2_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype2_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate2_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype2_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate2_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname3", label = "Monitor#3 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime3",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime3",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype3_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate3_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype3_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate3_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype3_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate3_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype3_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate3_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname4", label = "Monitor#4 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime4",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime4",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype4_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate4_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype4_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate4_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype4_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate4_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype4_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate4_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname5", label = "Monitor#5 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime5",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          airDatepickerInput(
            inputId = "enddatetime5",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype5_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate5_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype5_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate5_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype5_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate5_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype5_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate5_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname6", label = "Monitor#6 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime6",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime6",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype6_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate6_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype6_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate6_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype6_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate6_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype6_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate6_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname7", label = "Monitor#7 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime7",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime7",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype7_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate7_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype7_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate7_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype7_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate7_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype7_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate7_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname8", label = "Monitor#8 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime8",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime8",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype8_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate8_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype8_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate8_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype8_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate8_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype8_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate8_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname9", label = "Monitor#9 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime9",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          airDatepickerInput(
            inputId = "enddatetime9",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype9_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate9_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype9_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate9_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype9_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate9_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype9_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate9_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname10", label = "Monitor#10 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime10",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime10",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype10_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate10_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype10_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate10_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype10_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate10_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype10_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate10_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname11", label = "Monitor#11 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime11",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime11",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype11_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate11_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype11_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate11_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype11_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate11_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype11_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate11_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          textInput("monitorname12", label = "Monitor#12 name", value = "", ),
          airDatepickerInput(
            inputId = "startdatetime12",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime12",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          textInput("genotype12_1", label = "Genotype for channels 1-8", value = ""),
          textInput("replicate12_1", label = "Replicate for channels 1-8", value = ""),
          textInput("genotype12_2", label = "Genotype for channels 9-16", value = ""),
          textInput("replicate12_2", label = "Replicate for channels 9-16", value = ""),
          textInput("genotype12_3", label = "Genotype for channels 17-24", value = ""),
          textInput("replicate12_3", label = "Replicate for channels 17-24", value = ""),
          textInput("genotype12_4", label = "Genotype for channels 25-32", value = ""),
          textInput("replicate12_4", label = "Replicate for channels 25-32", value = ""),
          hr(),
          downloadBttn(
            outputId = "downloadmetadata",
            label = "Download metadata",
            style = "minimal",
            color = "primary"
          ),
        ),
        mainPanel(box(
          width = 12,
          div(
            style = "overflow-x: scroll",
            actionBttn(
              inputId = "updatemeta",
              label = "Update metadata",
              style = "minimal",
              color = "primary",
              icon = icon("forward")
            ),
            tableOutput("userdata") %>% withLoader(type = "html", loader = "pacman")
          )
        ))
      ),
      tabPanel(
        "Documentation",
        icon = icon("book-open"),
        source("Documentation.R", local = TRUE)[1]
      )
    )
  )
