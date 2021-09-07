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
library(shinyFiles)
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
          shinyDirButton("folder", "Select folder with monitor files", "Please select a folder",
            buttonType = "default",
            icon = icon("folder-open")
          ),
          tags$hr(),
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
            0, 1, 30
          ) %>%
            helper(
              type = "inline",
              title = "Starting day",
              content = c("Subset your data, Starting day 1 will be 0. <i>leave out transition days</i>"),
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
                  numericInput("alletho_width", "width", 1500, 500, 10000, 50)
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
                  numericInput("curatedtho_height", "height", 2500, 500, 10000, 50),
                  numericInput("curatedetho_width", "width", 1500, 500, 10000, 50)
                ),
                tags$hr(),
                plotOutput(
                  "curatedetho"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Average plots over days individual"
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
                  numericInput("popplot_width", "width", 1500, 500, 10000, 50)
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
                  numericInput("popplotwrap_width", "width", 1500, 500, 10000, 50)
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
                  numericInput("popplot1_width", "width", 1200, 500, 10000, 50)
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
                  numericInput("popplotwrap1_width", "width", 1000, 500, 10000, 50)
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
                  numericInput("popplotwrap1polar_height", "height", 700, 300, 1500, 50),
                  numericInput("popplotwrap1polar_width", "width", 700, 300, 1500, 50)
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
                numericInput("popplotwrapbox_width", "width", 1000, 500, 10000, 50)
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
                numericInput("popplotwrapboxmelt_width", "width", 1000, 500, 10000, 50)
              ),
              tags$hr(),
              plotOutput("popplotwrapboxmelt") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          widths = c(3, 9)
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
                numericInput("bout_width", "width", 1000, 500, 10000, 50)
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
                numericInput("numbouts_width", "width", 700, 500, 10000, 50)
              ),
              tags$hr(),
              plotOutput("numbouts") %>% withLoader(type = "html", loader = "pacman")
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
                numericInput("meanboutlength_width", "width", 700, 500, 10000, 50)
              ),
              tags$hr(),
              plotOutput("meanboutlength") %>% withLoader(type = "html", loader = "pacman")
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
                numericInput("numbouts_ld_width", "width", 700, 500, 10000, 50)
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
                numericInput("meanboutlength_ld_width", "width", 700, 500, 10000, 50)
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
                numericInput("meanboutlength_distrib_width", "width", 700, 500, 10000, 50)
              ),
              tags$hr(),
              plotOutput("meanboutlength_distrib") %>% withLoader(type = "html", loader = "pacman")
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
                numericInput("boutsummary_width", "width", 1000, 500, 10000, 50)
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
          withBusyIndicatorUI(
            actionBttn(
              inputId = "do",
              label = "Write metadata file",
              style = "minimal",
              color = "primary",
              icon = icon("pen-nib")
            )
          )
        ),
        mainPanel(box(
          width = 12,
          div(
            style = "overflow-x: scroll",
            tableOutput("userdata") %>% withLoader(type = "html", loader = "pacman")
          )
        ))
      )
    )
  )
