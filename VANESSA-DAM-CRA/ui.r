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
      "VANESSA-DAM-Circadian",
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
            "Modulo tau for actogram",
            24, 0, 24
          ),
          numericInput(
            "ldperiod",
            "LD cycle period",
            24, 0, 24
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
            "ind_act_met",
            "Individual actogram display method (1 = Raw, 2 = Normalized)",
            1,
            1,
            2,
            1
          ),
          numericInput(
            "light",
            "Duration of light in hours",
            12, 0, 24
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
            5, 1, 30
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
              content = c("By pressing this button you will curate your data (remove data when an individual is dead), remove individuals which were dead before the number of days you specified in the <i>How many minimum days flies should be alive to be counted?</i> box"),
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
        "Periodograms",
        icon = icon("chart-area"),
        mainPanel(
          withBusyIndicatorUI(
            actionBttn(
              inputId = "do3",
              label = "Calculate periodograms",
              style = "minimal",
              color = "primary",
              icon = icon("calculator"),
              size = "sm"
            )
          ),
          withBusyIndicatorUI(
            actionBttn(
              inputId = "do4",
              label = "Remove arrythmic individuals from dataset!",
              style = "minimal",
              color = "primary",
              icon = icon("hands-wash"),
              size = "sm"
            )
          ),
          tags$hr(),
          navlistPanel(
            "Periodogram method",
            tabPanel(
              radioGroupButtons(
                inputId = "permethod",
                choices = c("Autocorrelation", "Lomb-Scargle", "Chi-square", "CWT"),
                status = "primary",
                size = "xs",
                individual = TRUE,
                direction = "vertical",
                justified = TRUE,
                width = "30%",
                checkIcon = list(
                  yes = icon("ok",
                    lib = "glyphicon"
                  ),
                  no = icon("remove",
                    lib = "glyphicon"
                  )
                )
              ) %>%
                helper(
                  type = "inline",
                  title = "Peridiogram methods",
                  content = c(
                    "Depending on the type of data choose your method wisely.",
                    "The power from Autocorrelation periodograms can directly be used as <b>'Rhythmicity Index (RI)</b>'.",
                    "Chi-square and CWT methods take some time to calculate.",
                    "Choose upper and lower period limit for period estimation in the boxes below.",
                    "Choose significance threshold as you need."
                  ),
                  size = "m",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                )
            ),
            tabPanel(
              sliderInput(
                "ul", "Upper limit for periodogram", 26, 36, 32, 1
              )
            ),
            tabPanel(
              sliderInput(
                "ll", "Lower limit for periodogram", 10, 20, 16, 1
              )
            ),
            tabPanel(
              sliderInput(
                "alphasig", "Significance threshold for periodogram", 0, 0.1, 0.05, 0.01
              )
            ),
            "Periodogram figures",
            tabPanel(
              "Periodogram all with peaks"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All periodograms for all individuals will be shown. periodograms will be coloured according to the colour scheme selected in the <i>DATA INPUT</i> tab."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("chisqperiodplotallwithpeaks_height", "height", 50, 30, 100, 10),
                  numericInput("chisqperiodplotallwithpeaks_width", "width", 1400, 900, 3000, 50)
                ),
                tags$hr(),
                plotOutput(
                  "chisqperiodplotallwithpeaks"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Periodogram average"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("Periodograms averaged over all individuals for a Monitor will be shown. Different replicates will be plotted separately."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("chisqperiodplotaverage_height", "height", 500, 300, 1500, 50),
                  numericInput("chisqperiodplotaverage_width", "width", 1400, 900, 3000, 50)
                ),
                tags$hr(),
                plotOutput(
                  "chisqperiodplotaverage"
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Periods violin plot"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("Period values of individuals in a monitor will be plotted as violin plots. Each replicate will be plotted separately."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("chisqperiodplotviolin_height", "height", 500, 300, 1500, 50),
                  numericInput("chisqperiodplotviolin_width", "width", 800, 400, 3000, 50),
                  awesomeCheckbox(
                    inputId = "chisqperiodplotviolin_text",
                    label = "Print mean values on plot",
                    value = TRUE
                  )
                ),
                tags$hr(),
                plotOutput(
                  "chisqperiodplotviolin",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Periods distribution"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("Period values of individuals in a monitor will be plotted as violin plots. Each replicate will be plotted separately."),
                  size = "s",
                  buttonLabel = "Okay!",
                  easyClose = TRUE,
                  fade = TRUE
                ),
              mainPanel(
                splitLayout(
                  numericInput("perioddistrib_height", "height", 500, 300, 1500, 50),
                  numericInput("perioddistrib_width", "width", 800, 400, 3000, 50)
                ),
                tags$hr(),
                plotOutput(
                  "perioddistrib",
                ) %>% withLoader(type = "html", loader = "pacman")
              )
            ),
            tabPanel(
              "Download data"
              %>%
                helper(
                  type = "inline",
                  title = "",
                  content = c("All period-power data will be downloaded as a <b>.csv</b> file. The details of the content and how to navigate through it is available in the tutorial file."),
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
                tags$hr(),
                DT::dataTableOutput("periodpower") %>% withLoader(type = "html", loader = "pacman")
              )
            )
          )
        )
      ),
      tabPanel("Actograms",
        icon = icon("chart-bar"),
        navlistPanel(
          "Data overview",
          tabPanel(
            "All ethograms"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("All ethograms will be plotted for all days and all individuals for easy visualization of raw data."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("alletho_height", "height", 2500, 1000, 10000, 50),
                numericInput("alletho_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput("alletho") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "All actograms"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("All actograms will be plotted for all days and all individuals for easy visualization of raw data."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("allacto_height", "height", 2500, 1000, 10000, 50),
                numericInput("allacto_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput("allacto") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Curated ethograms"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("All ethograms will be plotted for curated data."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("curatedetho_height", "height", 2500, 1000, 10000, 50),
                numericInput("curatedetho_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput("curatedetho") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Curated actograms"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("All actograms will be plotted for curated data."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("curatedacto_height", "height", 2500, 1000, 10000, 50),
                numericInput("curatedacto_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput("curatedacto") %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Batch actograms",
          tabPanel(
            "Batch actograms raw"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw batch actograms will be plotted for all monitors and all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("indiv_raw_height", "height", 600, 100, 1500, 50),
                numericInput("indiv_raw_width", "width", 1000, 500, 5000, 50)
              ),
              plotOutput(
                "indiv_raw",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Batch actograms normalized"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized batch actograms will be plotted for all monitors and all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("indiv_avg_height", "height", 600, 100, 1500, 50),
                numericInput("indiv_avg_width", "width", 1000, 500, 5000, 50)
              ),
              plotOutput(
                "indiv_avg",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Actogram panel for all individuals",
          tabPanel(
            "All actograms raw"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw actograms will be plotted for all individuals separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("curatedactoraw_height", "height", 70, 50, 100, 10),
                numericInput("curatedactoraw_width", "width", 1600, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "curatedactoraw"
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "All actograms normalized"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized actograms will be plotted for all individuals separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("curatedactoavg_height", "height", 70, 50, 100, 10),
                numericInput("curatedactoavg_width", "width", 1600, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "curatedactoavg"
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Profile panel for all individuals",
          tabPanel(
            "Raw profile individual"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over days will be plotted for all individuals separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("rawpro_height", "height", 50, 30, 100, 10),
                numericInput("rawpro_width", "width", 1600, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "rawpro",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Normalized profile individual"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized profiles averaged over days will be plotted for all individuals separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgpro_height", "height", 50, 30, 100, 10),
                numericInput("avgpro_width", "width", 1600, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgpro",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Day wise average profiles",
          tabPanel(
            "Average profile day wise raw"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over individuals will be plotted for all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgdaywisepro1_raw_height", "height", 500, 100, 1000, 50),
                numericInput("avgdaywisepro1_raw_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgdaywisepro1_raw",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Average profile day wise normalized"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized profiles averaged over individuals will be plotted for all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgdaywisepro1_height", "height", 500, 100, 1000, 50),
                numericInput("avgdaywisepro1_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgdaywisepro1",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Average profiles",
          tabPanel(
            "Raw profile"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over individuals and days will be plotted for all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("rawpro1_height", "height", 600, 100, 1500, 50),
                numericInput("rawpro1_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "rawpro1",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Normalized profile"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over individuals and days will be plotted for all replicates separately <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgpro1_height", "height", 600, 100, 1500, 50),
                numericInput("avgpro1_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgpro1",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Raw profile all replicate average"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over individuals, days, and replicates <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("rawpro1all_height", "height", 500, 100, 1500, 50),
                numericInput("rawpro1all_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "rawpro1all",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Normalized profile all replicate average"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized profiles averaged over individuals, days, and replicates <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgpro1all_height", "height", 500, 100, 1500, 50),
                numericInput("avgpro1all_width", "width", 1500, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgpro1all",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          "Circular average profiles",
          tabPanel(
            "Average profile circular normalized"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Normalized profiles averaged over individuals, days, and replicates as a polar plot <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgprocircular_height", "height", 700, 500, 5000, 50),
                numericInput("avgprocircular_width", "width", 700, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgprocircular",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          ),
          tabPanel(
            "Average profile circular raw"
            %>%
              helper(
                type = "inline",
                title = "",
                content = c("Raw profiles averaged over individuals, days, and replicates as a polar plot <i>(only for curated data)</i>."),
                size = "s",
                buttonLabel = "Okay!",
                easyClose = TRUE,
                fade = TRUE
              ),
            mainPanel(
              splitLayout(
                numericInput("avgprocircular_raw_height", "height", 700, 500, 5000, 50),
                numericInput("avgprocircular_raw_width", "width", 700, 500, 5000, 50)
              ),
              tags$hr(),
              plotOutput(
                "avgprocircular_raw",
              ) %>% withLoader(type = "html", loader = "pacman")
            )
          )
          # )
          ,
          widths = c(3, 9)
        )
      ),
      tabPanel(
        "Individual actograms",
        icon = icon("chart-line"),
        sidebarPanel(
          width = 2,
          numericInput(
            "ind", "Individual Number",
            1, 1, 384
          ),
          withBusyIndicatorUI(
            actionBttn(
              inputId = "show",
              label = "Show!",
              style = "minimal",
              color = "primary",
              icon = icon("chart-bar")
            )
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("Raw/Normalized <i>(as selected in DATA INPUT tab)</i> actograms and periodograms of each individual will be plotted. Change individual number to be visualized in the numeric input box."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            )
        ),
        mainPanel(
          splitLayout(
            numericInput("indiv1_height", "height", 600, 300, 5000, 50),
            numericInput("indiv1_width", "width", 1200, 500, 5000, 50)
          ),
          tags$hr(),
          plotOutput("indiv1") %>% withLoader(type = "html", loader = "pacman"),
          tags$hr(),
          splitLayout(
            numericInput("chisqperiodplotallwithpeaks1_height", "height", 350, 200, 5000, 50),
            numericInput("chisqperiodplotallwithpeaks1_width", "width", 900, 300, 5000, 50)
          ),
          plotOutput(
            "chisqperiodplotallwithpeaks1"
          ) %>% withLoader(type = "html", loader = "pacman")
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
      ),
      tabPanel(
        "CWT spectograms",
        icon = icon("wave-square"),
        sidebarPanel(
          fileInput(
            "raw",
            "Choose DAM monitor File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          ),
          tags$hr(),
          checkboxInput("header", "Header", FALSE),
          radioButtons(
            "sep",
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = "\t"
          ),
          radioButtons(
            "disp",
            "Display",
            choices = c(
              Head = "head",
              All = "all"
            ),
            selected = "head"
          ),
          numericInput("bin", "bins (min)", 15, 1, 120, 1),
          numericInput("chn", "channel number", 1, 1, 32, 1),
          numericInput("lp", "period lower limit", 16, 14, 18, 1),
          numericInput("up", "period upper limit", 32, 30, 34, 1),
          numericInput("ndays", "Number of days", 10, 2, 40, 1),
          withBusyIndicatorUI(
            actionBttn(
              inputId = "do1",
              label = "Calculate!",
              style = "minimal",
              color = "primary",
              icon = icon("calculator")
            )
          ),
          width = 2
        ),

        mainPanel(tabsetPanel(
          tabPanel(
            "CWT spectogram",
            plotOutput("plot_CWT",
              height = "550px",
              width = "1200px"
            ) %>% withLoader(type = "html", loader = "pacman")
          ),
          tabPanel(
            "CWT wavelet power for period",
            plotOutput("plot_CWT_waveletpower",
              height = "550px",
              width = "1200px"
            ) %>% withLoader(type = "html", loader = "pacman")
          )
        ))
      ),

      tabPanel(
        "Timeseries smoothing",
        icon = icon("filter"),
        sidebarPanel(
          fileInput(
            "raw_smooth",
            "Choose DAM monitor File",
            multiple = FALSE,
            accept = c(
              "text/csv",
              "text/comma-separated-values,text/plain",
              ".csv"
            )
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("A DAM file scanned with DAMScan."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          checkboxInput("header", "Header", FALSE),
          radioButtons(
            "sep",
            "Separator",
            choices = c(
              Comma = ",",
              Semicolon = ";",
              Tab = "\t"
            ),
            selected = "\t"
          ),
          radioButtons(
            "disp",
            "Display",
            choices = c(
              Head = "head",
              All = "all"
            ),
            selected = "head"
          ),
          radioGroupButtons(
            inputId = "smooth_method",
            choices = c("low pass butterworth filter", "kernel smoothing"),
            status = "primary",
            size = "xs",
            individual = TRUE,
            direction = "vertical",
            justified = TRUE,
            width = "30%",
            checkIcon = list(
              yes = icon("ok",
                lib = "glyphicon"
              ),
              no = icon("remove",
                lib = "glyphicon"
              )
            )
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("Choose any one method, depending on prior experience, or take an informed decision by reading up literature. Both methods have been implemented such that phases won't change due to filtering. Currently only low pass butterworth filter is included, high pass filter may be included in the future."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          airDatepickerInput(
            inputId = "startdatetime_smooth",
            value = Sys.Date(),
            label = "Pick start date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("Subsetting your data, use the first day you want to use for average profile. DO NOT USE 00:00:00 as time, else an error will be thrown."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          airDatepickerInput(
            inputId = "enddatetime_smooth",
            value = Sys.Date(),
            label = "Pick end date and time:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("Subsetting your data, use the last day you want to use for average profile. Please ensure you are choosing full cycles, i.e., in multiple of 24 hours (12 AM to 12 AM or 10 AM to 10 AM etc.). DO NOT USE 00:00:00 as time, else an error will be thrown."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          airDatepickerInput(
            inputId = "resultdatetime_smooth",
            value = Sys.Date(),
            label = "Pick an arbitrary date for average profile, keep the time same as of start and end time of your data when subsetting:",
            timepicker = TRUE,
            timepickerOpts = timepickerOptions(timeFormat = "hh:ii:00"),
            update_on = "close",
            addon = "right"
          )
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("Pick an arbitrary date for average profile, keep the time same as of start and end time of your data when subsetting.  DO NOT USE 00:00:00 as time, else an error will be thrown."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput("bin_smooth", "binning of actual data (min)", 1, 1, 120, 1)
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("original binning of your data"),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput("bin_req_smooth", "required binning for data (min)", 1, 1, 32, 1)
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("required binning of your data for average profile"),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput("n_smooth", "filter order or generic filter model", 2, 1, 10, 1),
          numericInput("W_smooth", "critical frequencies of the filter", 0.1, 0, 1, 0.01)
          %>%
            helper(
              type = "inline",
              title = "",
              content = c("W is the critical frequency of the filter and it must be a scalar for a low-pass filter. For digital filters, W must be between 0 and 1, where 1 is the Nyquist frequency."),
              size = "s",
              buttonLabel = "Okay!",
              easyClose = TRUE,
              fade = TRUE
            ),
          numericInput("b_smooth", "kernel smoothing bandwith", 5, 1, 20, 1),
          withBusyIndicatorUI(
            actionBttn(
              inputId = "do_smooth",
              label = "Calculate!",
              style = "minimal",
              color = "primary",
              icon = icon("calculator")
            )
          ),
          width = 2
        ),

        mainPanel(tabsetPanel(
          tabPanel(
            "Smoothened average profile",
            plotOutput("plot_smooth_average",
              height = "550px",
              width = "1200px"
            ) %>% withLoader(type = "html", loader = "pacman"),
            downloadBttn(
              outputId = "smoothened_bf",
              label = "Download bf data",
              style = "minimal",
              color = "primary"
            ),
            downloadBttn(
              outputId = "smoothened_ks",
              label = "Download ks data",
              style = "minimal",
              color = "primary"
            ),
            downloadBttn(
              outputId = "report_smooth",
              label = "Generate report",
              style = "minimal",
              color = "primary"
            )
          ),
          tabPanel(
            "Smoothened individual profile",
            plotOutput("plot_smooth_ind",
              height = "1200px",
              width = "2400px"
            ) %>% withLoader(type = "html", loader = "pacman")
          )
        ))
      )
    )
  )
