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
          # downloadButton("report", "Generate report"),
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
              plotOutput(
                "alletho",
                height = "2500px",
                width = "1500px"
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "curatedetho",
                height = "2500px",
                width = "1500px"
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "popplot",
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "popplotwrap",
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "popplot1",
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "popplotwrap1",
              ) %>% withLoader(type = "html", loader = "pacman")
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
              plotOutput(
                "popplotwrap1polar",
              ) %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("popplotwrapbox") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("popplotwrapboxmelt") %>% withLoader(type = "html", loader = "pacman")
          ),
          # )
          # ,
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
            plotOutput("bout") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("numbouts") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("meanboutlength") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("numbouts_ld") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("meanboutlength_ld") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("meanboutlength_distrib") %>% withLoader(type = "html", loader = "pacman")
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
            plotOutput("boutsummary") %>% withLoader(type = "html", loader = "pacman")
          ),
          # )
          # ,
          widths = c(3, 9)
        )
      ),
      tabPanel(
        "Data formatting",
        icon = icon("database"),
        sidebarPanel(
          width = 3,
          textInput("monitorname1", label = "Monitor#1 name", value = "", ),
          # hr(),
          airDatepickerInput(
            inputId = "startdatetime1",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          # hr(),
          airDatepickerInput(
            inputId = "enddatetime1",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          # hr(),
          numericInput(
            inputId = "chnl_1_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_1_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype1_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate1_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_1_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_1_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype1_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate1_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_1_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_1_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype1_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate1_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_1_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_1_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype1_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate1_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname2", label = "Monitor#2 name", value = "", ),
          # hr(),
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
          numericInput(
            inputId = "chnl_2_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_2_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype2_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate2_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_2_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_2_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype2_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate2_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_2_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_2_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype2_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate2_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_2_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_2_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype2_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate2_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname3", label = "Monitor#3 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_3_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_3_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype3_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate3_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_3_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_3_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype3_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate3_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_3_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_3_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype3_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate3_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_3_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_3_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype3_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate3_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname4", label = "Monitor#4 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_4_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_4_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype4_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate4_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_4_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_4_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype4_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate4_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_4_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_4_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype4_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate4_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_4_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_4_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype4_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate4_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname5", label = "Monitor#5 name", value = "", ),
          hr(),
          airDatepickerInput(
            inputId = "startdatetime5",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime5",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          numericInput(
            inputId = "chnl_5_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_5_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype5_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate5_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_5_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_5_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype5_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate5_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_5_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_5_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype5_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate5_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_5_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_5_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype5_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate5_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname6", label = "Monitor#6 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_6_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_6_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype6_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate6_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_6_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_6_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype6_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate6_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_6_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_6_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype6_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate6_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_6_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_6_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype6_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate6_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname7", label = "Monitor#7 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_7_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_7_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype7_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate7_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_7_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_7_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype7_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate7_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_7_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_7_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype7_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate7_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_7_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_7_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype7_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate7_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname8", label = "Monitor#8 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_8_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_8_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype8_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate8_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_8_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_8_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype8_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate8_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_8_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_8_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype8_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate8_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_8_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_8_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype8_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate8_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname9", label = "Monitor#9 name", value = "", ),
          hr(),
          airDatepickerInput(
            inputId = "startdatetime9",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          hr(),
          airDatepickerInput(
            inputId = "enddatetime9",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          ),
          numericInput(
            inputId = "chnl_9_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_9_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype9_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate9_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_9_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_9_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype9_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate9_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_9_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_9_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype9_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate9_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_9_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_9_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype9_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate9_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname10", label = "Monitor#10 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_10_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_10_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype10_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate10_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_10_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_10_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype10_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate10_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_10_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_10_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype10_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate10_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_10_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_10_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype10_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate10_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname11", label = "Monitor#11 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_11_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_11_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype11_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate11_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_11_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_11_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype11_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate11_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_11_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_11_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype11_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate11_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_11_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_11_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype11_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate11_4", label = "Replicate", value = ""),
          hr(),
          textInput("monitorname12", label = "Monitor#12 name", value = "", ),
          hr(),
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
          numericInput(
            inputId = "chnl_12_1_s",
            label = "Select start channel",
            value = "1",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_12_1_e",
            label = "Select end channel",
            value = "8",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype12_1", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate12_1", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_12_2_s",
            label = "Select start channel",
            value = "9",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_12_2_e",
            label = "Select end channel",
            value = "16",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype12_2", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate12_2", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_12_3_s",
            label = "Select start channel",
            value = "17",
            min = 17,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_12_3_e",
            label = "Select end channel",
            value = "24",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype12_3", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate12_3", label = "Replicate", value = ""),
          # hr(),
          # hr(),
          numericInput(
            inputId = "chnl_12_4_s",
            label = "Select start channel",
            value = "25",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          numericInput(
            inputId = "chnl_12_4_e",
            label = "Select end channel",
            value = "32",
            min = 1,
            max = 32,
            step = 1
          ),
          # hr(),
          textInput("genotype12_4", label = "Genotype", value = ""),
          # hr(),
          textInput("replicate12_4", label = "Replicate", value = ""),
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
      ),
      tabPanel(
        "Plot height Customization",
        icon = icon("hammer"),
        useShinyalert(),
        sidebarPanel(
          width = 4,
          numericInput(
            "popplot_height",
            "Average plots over days individual",
            6000, 3000, 10000
          ),
          numericInput(
            "popplotwrap_height",
            "Average plot wrapped individual",
            1400, 800, 3000
          ),
          numericInput(
            "popplot1_height",
            "Average plots over days",
            300, 100, 1000
          ),
          numericInput(
            "popplotwrap1_height",
            "Average plot wrapped",
            300, 100, 1000
          )
        ),
        sidebarPanel(
          width = 4,

          numericInput(
            "popplotwrapbox_height",
            "Sleep fraction",
            700, 300, 2000
          ),
          numericInput(
            "popplotwrapboxmelt_height",
            "Sleep fraction summary",
            700, 300, 2000
          )
        ),
        sidebarPanel(
          width = 4,
          numericInput(
            "numbouts_height",
            "Number of bouts",
            500, 300, 1000
          ),
          numericInput(
            "meanboutlength_height",
            "Mean bout length",
            500, 300, 2000
          ),
          numericInput(
            "bout_height",
            "Bouts",
            300, 100, 1000
          ),
          numericInput(
            "boutsummary_height",
            "Bout summary",
            300, 100, 1000
          ),
          numericInput(
            "numbouts_ld_height",
            "Number of bouts in light and dark",
            500, 300, 2000
          ),
          numericInput(
            "meanboutlength_ld_height",
            "Mean bout length in light and dark",
            500, 300, 2000
          ),
          numericInput(
            "meanboutlength_distrib_height",
            "Mean bout length distribution",
            500, 300, 2000
          )
        )
      )
    )
  )