column(
  8,
  tags$body(
    h2(strong("VANESSA-DAM-SA")),
    br(),
    tags$img(height = 300, width = 255, src = "VANESSA_hex.png"),
    tags$div(
      tags$p(
        tags$b("Visualization and ANalysis of timE SerieS dAta - Drosophila Activity Monitors (VANESSA-DAM)"), "is a collection of useful tools to visualize and analyze Time series data obtained from
                  Drosophila Activity Monitors ", tags$u("(https://www.trikinetics.com/)"), ". The first in the series of tools is a shiny
                  app for sleep analysis and visualization - VANESSA-DAM for sleep
                  analysis (VANESSA-DAM-SA). For any suggestions, questions, troubleshooting or
                  customization, please contact ", tags$u("arijitghosh2009@gmail.com"),
        "."
      ),
      style = "font-size: 19px;"
    ),
    tags$div(
      tags$p(
        "VANESSA-DAM-SA is dependent on Quentin Geissmann's rethomics family of packages
- behavr, damr, ggetho, sleepr, for some analysis and visualization options. It offers several
advantages over existing tools for circadian rhythm analysis from DAM systems, some
mentionable ones are -",
        tags$ol(
          tags$li("Analysis and visualization of multiple monitor files, genotypes, replicates together in a
high-throughput manner"),
          tags$li("Creating metadata files for information about experiment and better reproducibility."),
          tags$li("Producing high-resolution publication-quality figures with a plethora of customization."),
          tags$li("Data curation - automatic user-defined parameter-based removal of dead and arrhythmic
individuals."),
          tags$li("Sleep profile analysis, various sleep parameter estimation and quantification, bout analysis, latency analysis."),
          tags$li("Visual comparison among genotypes, replicates."),
          tags$li("Dynamic plot resizing and recoloring."),
          tags$li("Reproducible code report so that you can generate the figures and analysis without the
shiny app from RStudio directly."),
          tags$li("Minimizing human errors - no need to tinker with raw data to accommodate analysis tools.")
        )
      ),
      style = "font-size: 19px;"
    ),

    tags$br(),
    tags$div(
      tags$p(strong("How to cite VANESSA-DAM-SA:"), "Arijit Ghosh and Vasu Sheeba, Chronobiology and Behavioural
           Neurogenetics Laboratory, Neuroscience Unit, JNCASR, Bangalore, India
                  (manuscript under review)"),
      style = "font-size: 19px"
    ),


    h3(strong("Contact information:")),
    tags$div(
      tags$p("Arijit Ghosh - arijitghosh2009@gmail.com, arijit@jncasr.ac.in ; Vasu Sheeba - sheeba@jncasr.ac.in."),
      style = "font-size: 18px"
    ),

    h3(strong("Source code and test files:")),
    tags$div(
      tags$p("Source code and test files are available through GitHub at:"),
      style = "font-size: 18px"
    ),



    tags$div(
      tags$a(
        href = "https://github.com/orijitghosh/VANESSA-DAM",
        "VANESSA-DAM GitHub repository"
      ),
      style = "font-size: 18px"
    ),
    tags$div(
      tags$a(
        href = "https://github.com/orijitghosh/VANESSA-DAM/issues",
        "Please raise issues on GitHub page here"
      ),
      style = "font-size: 18px"
    ),

    br(),

    h4(strong("Web browser compatibility")),
    tags$div(
      tags$p("VANESSA-DAM-SA is compatible with all major web browsers (Google Chrome, Mozilla Firefox, Microsoft Edge, Safari), 
                  but its graphical user interface was optimized to work best 
                  in Google Chrome, operating in a full screen mode on a 1080p monitor."),
      style = "font-size: 17px"
    ),

    h1(strong("Easy tutorial to start using VANESSA-DAM-SA")),

    tags$div(
      tags$ol(
        tags$li("Install R (version > 3.6.3) and RStudio (version > 1.2) on your computer."),
        tags$li("Download the zipped folder containing the app and unzip to a folder."),
        tags$li("Open RStudio and Run the startup.r file to install all necessary packages to run the app."),
        tags$li("Restart R by pressing", tags$b("ctrl+shift+F10"), "or from", tags$b("Session > Restart R"), " from RStudio menu. This step is useful to clean the global environment so that previously loaded packages do not interrupt, or mask packages/functions needed for VANESSA-DAM-CRA."),
        tags$li("Set working directory as the home folder of the app (by pressing ", tags$b("ctrl+shift+h"), "OR by using the ", tags$b("setwd()"), "command) and then run the app by typing  ", tags$b("shiny::runApp(launch.browser = T)"), "in the console. OR load the ", tags$b("server.r"), "or ", tags$b("ui.r"), "file in RStudio by double-cliking on any of them and click on the ", tags$b("Run App"), "button in RStudio panel (for better experience select ", tags$i("Run External"), "from the dropdown menu on the right side of the ", tags$b("Run App"), "button beforehand)."),
        tags$li("After the app opens in a browser window, click on the ", tags$b("Understood!"), "button or press ", tags$b("Esc"), "to begin."),
        tags$li("Assuming you have your Monitor files (", tags$i("NS215bCtM006.txt, NS215bCtM009.txt"), "provided with Genotypes ", tags$i("CS-males and CS-females"), "respectively - check the ", tags$b("Notes"), "section for details of the data provided) from DAMScan program, first job is to make a Metadata file (Provided as ", tags$i("Metadata.csv)"), "."),
        tags$li("In this case, we have 2 genotypes in 2 Monitor files, so enter 2 at the ", tags$b("Number of Monitors"), "box. Then go to the ", tags$b("DATA FORMATTING"), "tab."),
        tags$li("Enter name of your first monitor file in the ", tags$b("Monitor#1"), "name box, include extensions also (e.g., if the name of the monitor file Monitor1.txt, then enter the FULL name, including .txt). Fill the next boxes - ", tags$b("Start date and time (If your start date is 1st August 2020 and Lights-on time is 10 AM, then you write 2020-08-01 10:00:00), End date (If your end date is 10th August 2020 - 10 AM, then you write 2020-08-10 10:00:00)"), "with your experimental details as shown above. Enter ", tags$b("Genotype"), "values and ", tags$b("Replicate"), "number (only if your experimental design has multiple replicates for all/some genotypes, else enter any random number for all monitors, it will not affect any calculation) in proper boxes for proper channels (marked as ", tags$b("Genotype for channels 1-8"), "and ", tags$b("Replicate for channels 1-8"), "etc.). A date and time picker has been added to facilitate easy entering of start and end date times without error in formatting. Each monitor can have maximum of 4 genotypes with 8 individuals."),
        tags$strong(tags$li("Note: Creating the metadata file is the single most important step for using this app, any wrong information in the metadata file will wreak havoc on the results and the app may crash. DO NOT change information in the DATA FORMATTING tab once you have entered. For some unknown problem in shiny reactivity, this section does not handle deletion and change of values nicely. Alternatively, you can just create your metadata file by changing data in the already provided metadata file in a good text editor like Notepad++ (DO NOT USE EXCEL, as excel may change the date-time format). The metadata file serves as a base of information for your analysis too and is good for record keeping of your experiments.", style = "font-size: 16px;color: #BA0707")),
        tags$li("After filling in experimental details for all 3 monitors, press the ", tags$b("Update metadata"), "button, and then go to the bottom of the page and click on the ", tags$b("Download metadata"), "button, this will download the Metadata as a CSV file in your desired folder and is ready to be used."),
        tags$li("Go back to the ", tags$b("DATA INPUT"), "tab. Enter choices one by one - ", tags$b("Duration of light in hours"), "in your experiment, ", tags$b("How many minimum days flies should be alive to be counted"), ". Also chose your ", tags$b("starting day"), "and ", tags$b("ending day"), "for analysis (", tags$b("starting day = 1"), "means first day in your data, so if you want to select first five days, ", tags$b("starting day"), "should be 1 and ", tags$b("ending day"), "should be 5). Enter ", tags$b("LD cycle period"), "in your experiment, ", tags$b("Modulo tau"), "for actogram can be changed for visualization purpose later. The same goes for ", tags$b("Summary time window in minutes"), "and ", tags$b("Individual actogram display method"), ". You can change any of these parameters later while looking at plots, the plots will immediately update accordingly. For example, if you're looking at your actograms in 15 minutes bin, and you want to visualize them with 5 minutes bin, you just have to change the value in ", tags$b("Summary time window in minutes"), "box, similarly LD shading can also be changed. You are all set to start analysis now. ", tags$b("ONLY PROCEED FORWARD WHEN YOU HAVE FIXED ALL ANALYSIS PARAMETERS"), ". Select the monitor files to be analyzed by clicking the ", tags$b("Choose Monitor Files"), "button and upload your Metadata file by clicking on the ", tags$b("BROWSE"), "button in the ", tags$b("Choose Metadata File"), "box. Press the ", tags$b("Start Calculations!"), "button, when the calculations are done, you will be notified by a sound. Depending on how long your data is and how many monitors you are analyzing, this step (which involves curation also) will take anything between 5 seconds to 30 seconds typically. To follow this tutorial, enter ", tags$b("Number of monitors"), "as 2 and ", tags$b("Number of replicates"), "as 1."),
        tags$strong(tags$li("Note: Most of the parameters have a small question mark symbol in blue near them, clicking on the symbol will show a modal with information about the parameter. This help is available throughout the app for different parameters, analyses and plots.", style = "font-size: 16px;color: #BA0707")),
        tags$li("Go to the ", tags$b("SLEEP PROFILES"), "tab next. Explore the various analysis and visualization options by clicking on the left panel tabs. Each tab has description associated with the question mark button."),
        tags$li(
          "Go to the ", tags$b("Download data"), "tab to download a csv file for all sleep parameters extracted from your data. There are two different files to be downloaded, one with all sleep parameters, and one with specific bout analysis details. The ", tags$b("total_bout_length"), "parameter in the downloaded files is total sleep time either on whole day or in light and dark phases. ", tags$b("ALL time values are in minutes."),
          tags$li("Go to the ", tags$b("SLEEP FRACTIONS"), "tab to start visualizing sleep fractions as mentioned in different side tabs, feel free to explore."),
          tags$strong(tags$li("Note: sleep_fraction_l denotes sleep in the light part of the day, sleep_fraction_d denotes sleep in the dark part, and sleep_fraction_all denotes total sleep in a day.", style = "font-size: 16px;color: #BA0707")),
          tags$li("Go to the ", tags$b("BOUT ANALYSIS"), "tab to see different plots from sleep architecture analysis.")
        ),
        style = "font-size: 18px; line-height: 1.7;"
      ),

      h3(strong("Usage notes:")),

      tags$div(
        tags$ul(
          tags$li("If you have different replicates, they will be plotted separately."),
          tags$li("In faceted panels, the values on top of each panel are as following: 1-Genotype name, 2- serial ID assigned on raw data, 3-Replicate number."),
          tags$li("All images produced are high-resolution, can be copied onto clipboard, saved as png files and directly used."),
          tags$li("Cleaning up your Monitor file before using is desired, you don't necessarily have to subset your data by date, all data can be in the Monitor file, only the dates you specify in your Metadata file will be used, thus reducing hassle for the user."),
          tags$li("If your run was in DD, please put ", tags$b("Duration of light in hours"), "as 23.99, as it does not take 0 as input, it does not affect any calculations. Also, after you know your average period value in DD, you can change ", tags$b("Modulo tau for actogram"), "and ", tags$b("LD cycle period"), "accordingly, and plots will be updated accordingly."),
          tags$li("The demo data provided is from a locomotor activity run with DAM2 system under LD12:12 for 8 days of CantonS flies (one monitor for males and another for females). Each monitor file has 32 flies loaded onto it (standard format for DAM2 systems)."),
          tags$li("All plots can be resized from the ", tags$b("'height'"), "and ", tags$b("'width'"), "parameters on top of them for best visualization according to the user. To show the plot, you have to click on the ", tags$b("Plot"), "button, and each time you change dimension of the plot, you again have to click the ", tags$b("Plot"), "button."),
          tags$li("8.	Most of the violin plots will be faceted into different violin plots over days, day number will be printed on the right side of each plot.")
        ),
        style = "font-size: 17px; line-height: 1.5;"
      ),


      tags$h3(strong("Data curation and removal of dead animals")),
      tags$div(
        tags$p("The function detects when individuals have died based on their first (very) long bout of immobility.
Following data (which may include spurious artefact of movement) are removed. An animal is scored as dead it does not move more than *one percent of the time* for at least *one day*.
All data following a 'death' event are removed."),
        style = "font-size: 19px"
      ),

      tags$h3(strong("Light onset time")),
      tags$div(
        tags$p("Light onset time or ZT00 is defined by the ", tags$b("start_date_time"), "parameter in the metadata file. Each day's calculations will start from ZT00. In case your run was in DD, please mention ", tags$b("start_date_time"), "as CT00."),
        style = "font-size: 19px"
      ),


      tags$h3(strong("DAM system data acquisition frequency")),
      tags$div(
        tags$p("The DAM system records sums of counts over a Reading Interval. Most of the VANESSA-DAM functions can analyze data recorded 
                at any Reading Interval, or DAM system data acquisition frequency. 
                Although, Sleep Analysis requires precisely 1 min Reading Intervals, Circadian Period Analysis requires 
                Reading Intervals  >= 1 min. We recommend setting the DAM system to 1 min Reading Interval as it allows 
                the most flexibility in VANESSA-DAM data analyses. 
                "),
        style = "font-size: 19px"
      ),
      hr(),

      tags$h3(strong("Acknowledgements")),
      tags$div(
        tags$p("The authors would like to acknowledge the authors of R packages we used in the VANEESA-DAM code: 
          Rethomics - (Giessmann Q et al, 2018),
          Rmarkdown - (Allaire J et al, 2020),
          colourpicker - (Attali D, 2016),
          Shinyjs - (Attali D, 2016),
          Shinyalert - (Attali D and Edwards T, 2020),
          Beepr - (Baath R, 2018),
          Pracma - (Borchers HW, 2019),
          Shinythemes - (Chang W, 2018),
          Shinydashboard - (Chang W and Borges Ribeiro B, 2018),
          shiny - (Chang W et al, 2021),
          data.table - (Dowle M and Srinivasan A, 2020),
          lubridate - (Grolemund G and Wickham H, 2011),
          Shinyhelper - (Mason-Thom C, 2019),
          hms - (Muller K, 2020),
          ShinyWidgets - (Perrier V et al, 2020),
          Showtext - (Qiu Y, 2020),
          WaveletComp - (Roesch A and Schmidbauer H, 2018),
          signal - (signal developers, 2014),
          Shinycustomloader - (Tanaka E and Niichan, 2018),
          Cairo - (Urbanek S and Horner J, 2020),
          ggplot2 - (Wickham H, 2018),
          Tidyr - (Wickham H, 2020),
          Readr - (Wickham H and Hester J, 2020),
          dplyr - (Wickham H et al, 2020),
          ggridges - (Wilke CO, 2021),
          DT - (Xie Y et al, 2020). 
           "),
        style = "font-size: 19px"
      ),


      hr(),


      tags$h3(strong("News, Updates, and Changelogs")),
      tags$div(
        tags$h3(strong("v1.0.3")),
        tags$ul(
          tags$li("First version for manuscript review."),
          tags$li("Not documenting changes from previous experimental releases.")
        ),
        tags$h3(strong("v1.0.2")),
        tags$ul(
          # tags$li("Disambiguation of column names in tables and CSV files"),
          # tags$li("Updated Documentation")
        ),
        style = "font-size: 19px"
      )
    )
  )
)
