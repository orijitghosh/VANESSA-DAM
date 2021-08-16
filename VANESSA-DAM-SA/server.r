library(shiny)
library(data.table)
library(WaveletComp)
library(ggetho)
library(zeitgebr)
library(ggplot2)
library(ggridges)
library(readr)
library(damr)
library(sleepr)
library(behavr)
library(dplyr)
library(shinyalert)
library(colourpicker)
library(beepr)
library(shinyWidgets)
library(shinyhelper)
library(shinyFiles)
library(fs)
shinyServer(function(input, output, session) {
  shinyalert(
    title = "Visual ANalysis of timE SerieS dAta - Drosophila Activity Monitors (VANESSA-DAM) for sleep analysis!",
    text = "<b>This app requires a metadata file for your monitors, to make the metadata files, first visit the Data formatting tab. The metadata files will be created in the home folder of the app. Right now simultaneous analysis and visualization upto twelve genotypes are supported. Contact <i>arijitghosh2009@gmail.com</i> for bugs, suggestions, troubleshooting and customizations.</b>",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
    # type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Understood!",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "./VANESSA_hex.png",
    imageWidth = 200,
    imageHeight = 200,
    animation = TRUE,
    size = "m"
  )
  session$onSessionEnded(stopApp)
  observe_helpers(withMathJax = TRUE)
  observe({
    userdata1 <- matrix(nrow = 32, ncol = 6)
    userdata1[, 1] <- input$monitorname1 ### change monitor name
    userdata1[, 2] <- as.character(input$startdatetime1) ### start date of recording time is ZT0
    userdata1[, 3] <- as.character(input$enddatetime1) ### stop date
    userdata1[(1:8), 5] <- input$genotype1_1 ### name of your genotype
    userdata1[(9:16), 5] <- input$genotype1_2 ### name of your genotype
    userdata1[(17:24), 5] <- input$genotype1_3 ### name of your genotype
    userdata1[(25:32), 5] <- input$genotype1_4 ### name of your genotype
    userdata1[, 4] <- seq(1, 32, by = 1)
    userdata1[(1:8), 6] <- input$replicate1_1
    userdata1[(9:16), 6] <- input$replicate1_2
    userdata1[(17:24), 6] <- input$replicate1_3
    userdata1[(25:32), 6] <- input$replicate1_4
    
    userdata2 <- matrix(nrow = 32, ncol = 6)
    userdata2[, 1] <- input$monitorname2 ### change monitor name
    userdata2[, 2] <- as.character(input$startdatetime2) ### start date of recording time is ZT0
    userdata2[, 3] <- as.character(input$enddatetime2) ### stop date
    userdata2[(1:8), 5] <- input$genotype2_1 ### name of your genotype
    userdata2[(9:16), 5] <- input$genotype2_2 ### name of your genotype
    userdata2[(17:24), 5] <- input$genotype2_3 ### name of your genotype
    userdata2[(25:32), 5] <- input$genotype2_4 ### name of your genotype
    userdata2[, 4] <- seq(1, 32, by = 1)
    userdata2[(1:8), 6] <- input$replicate2_1
    userdata2[(9:16), 6] <- input$replicate2_2
    userdata2[(17:24), 6] <- input$replicate2_3
    userdata2[(25:32), 6] <- input$replicate2_4
    
    userdata3 <- matrix(nrow = 32, ncol = 6)
    userdata3[, 1] <- input$monitorname3 ### change monitor name
    userdata3[, 2] <- as.character(input$startdatetime3) ### start date of recording time is ZT0
    userdata3[, 3] <- as.character(input$enddatetime3) ### stop date
    userdata3[(1:8), 5] <- input$genotype3_1 ### name of your genotype
    userdata3[(9:16), 5] <- input$genotype3_2 ### name of your genotype
    userdata3[(17:24), 5] <- input$genotype3_3 ### name of your genotype
    userdata3[(25:32), 5] <- input$genotype3_4 ### name of your genotype
    userdata3[, 4] <- seq(1, 32, by = 1)
    userdata3[(1:8), 6] <- input$replicate3_1
    userdata3[(9:16), 6] <- input$replicate3_2
    userdata3[(17:24), 6] <- input$replicate3_3
    userdata3[(25:32), 6] <- input$replicate3_4
    
    userdata4 <- matrix(nrow = 32, ncol = 6)
    userdata4[, 1] <- input$monitorname4 ### change monitor name
    userdata4[, 2] <- as.character(input$startdatetime4) ### start date of recording time is ZT0
    userdata4[, 3] <- as.character(input$enddatetime4) ### stop date
    userdata4[(1:8), 5] <- input$genotype4_1 ### name of your genotype
    userdata4[(9:16), 5] <- input$genotype4_2 ### name of your genotype
    userdata4[(17:24), 5] <- input$genotype4_3 ### name of your genotype
    userdata4[(25:32), 5] <- input$genotype4_4 ### name of your genotype
    userdata4[, 4] <- seq(1, 32, by = 1)
    userdata4[(1:8), 6] <- input$replicate4_1
    userdata4[(9:16), 6] <- input$replicate4_2
    userdata4[(17:24), 6] <- input$replicate4_3
    userdata4[(25:32), 6] <- input$replicate4_4
    
    userdata5 <- matrix(nrow = 32, ncol = 6)
    userdata5[, 1] <- input$monitorname5 ### change monitor name
    userdata5[, 2] <- as.character(input$startdatetime5) ### start date of recording time is ZT0
    userdata5[, 3] <- as.character(input$enddatetime5) ### stop date
    userdata5[(1:8), 5] <- input$genotype5_1 ### name of your genotype
    userdata5[(9:16), 5] <- input$genotype5_2 ### name of your genotype
    userdata5[(17:24), 5] <- input$genotype5_3 ### name of your genotype
    userdata5[(25:32), 5] <- input$genotype5_4 ### name of your genotype
    userdata5[, 4] <- seq(1, 32, by = 1)
    userdata5[(1:8), 6] <- input$replicate5_1
    userdata5[(9:16), 6] <- input$replicate5_2
    userdata5[(17:24), 6] <- input$replicate5_3
    userdata5[(25:32), 6] <- input$replicate5_4
    
    userdata6 <- matrix(nrow = 32, ncol = 6)
    userdata6[, 1] <- input$monitorname6 ### change monitor name
    userdata6[, 2] <- as.character(input$startdatetime6) ### start date of recording time is ZT0
    userdata6[, 3] <- as.character(input$enddatetime6) ### stop date
    userdata6[(1:8), 5] <- input$genotype6_1 ### name of your genotype
    userdata6[(9:16), 5] <- input$genotype6_2 ### name of your genotype
    userdata6[(17:24), 5] <- input$genotype6_3 ### name of your genotype
    userdata6[(25:32), 5] <- input$genotype6_4 ### name of your genotype
    userdata6[, 4] <- seq(1, 32, by = 1)
    userdata6[(1:8), 6] <- input$replicate6_1
    userdata6[(9:16), 6] <- input$replicate6_2
    userdata6[(17:24), 6] <- input$replicate6_3
    userdata6[(25:32), 6] <- input$replicate6_4
    
    userdata7 <- matrix(nrow = 32, ncol = 6)
    userdata7[, 1] <- input$monitorname7 ### change monitor name
    userdata7[, 2] <- as.character(input$startdatetime7) ### start date of recording time is ZT0
    userdata7[, 3] <- as.character(input$enddatetime7) ### stop date
    userdata7[(1:8), 5] <- input$genotype7_1 ### name of your genotype
    userdata7[(9:16), 5] <- input$genotype7_2 ### name of your genotype
    userdata7[(17:24), 5] <- input$genotype7_3 ### name of your genotype
    userdata7[(25:32), 5] <- input$genotype7_4 ### name of your genotype
    userdata7[, 4] <- seq(1, 32, by = 1)
    userdata7[(1:8), 6] <- input$replicate7_1
    userdata7[(9:16), 6] <- input$replicate7_2
    userdata7[(17:24), 6] <- input$replicate7_3
    userdata7[(25:32), 6] <- input$replicate7_4
    
    userdata8 <- matrix(nrow = 32, ncol = 6)
    userdata8[, 1] <- input$monitorname8 ### change monitor name
    userdata8[, 2] <- as.character(input$startdatetime8) ### start date of recording time is ZT0
    userdata8[, 3] <- as.character(input$enddatetime8) ### stop date
    userdata8[(1:8), 5] <- input$genotype8_1 ### name of your genotype
    userdata8[(9:16), 5] <- input$genotype8_2 ### name of your genotype
    userdata8[(17:24), 5] <- input$genotype8_3 ### name of your genotype
    userdata8[(25:32), 5] <- input$genotype8_4 ### name of your genotype
    userdata8[, 4] <- seq(1, 32, by = 1)
    userdata8[(1:8), 6] <- input$replicate8_1
    userdata8[(9:16), 6] <- input$replicate8_2
    userdata8[(17:24), 6] <- input$replicate8_3
    userdata8[(25:32), 6] <- input$replicate8_4
    
    userdata9 <- matrix(nrow = 32, ncol = 6)
    userdata9[, 1] <- input$monitorname9 ### change monitor name
    userdata9[, 2] <- as.character(input$startdatetime9) ### start date of recording time is ZT0
    userdata9[, 3] <- as.character(input$enddatetime9) ### stop date
    userdata9[(1:8), 5] <- input$genotype9_1 ### name of your genotype
    userdata9[(9:16), 5] <- input$genotype9_2 ### name of your genotype
    userdata9[(17:24), 5] <- input$genotype9_3 ### name of your genotype
    userdata9[(25:32), 5] <- input$genotype9_4 ### name of your genotype
    userdata9[, 4] <- seq(1, 32, by = 1)
    userdata9[(1:8), 6] <- input$replicate9_1
    userdata9[(9:16), 6] <- input$replicate9_2
    userdata9[(17:24), 6] <- input$replicate9_3
    userdata9[(25:32), 6] <- input$replicate9_4
    
    userdata10 <- matrix(nrow = 32, ncol = 6)
    userdata10[, 1] <- input$monitorname10 ### change monitor name
    userdata10[, 2] <- as.character(input$startdatetime10) ### start date of recording time is ZT0
    userdata10[, 3] <- as.character(input$enddatetime10) ### stop date
    userdata10[(1:8), 5] <- input$genotype10_1 ### name of your genotype
    userdata10[(9:16), 5] <- input$genotype10_2 ### name of your genotype
    userdata10[(17:24), 5] <- input$genotype10_3 ### name of your genotype
    userdata10[(25:32), 5] <- input$genotype10_4 ### name of your genotype
    userdata10[, 4] <- seq(1, 32, by = 1)
    userdata10[(1:8), 6] <- input$replicate10_1
    userdata10[(9:16), 6] <- input$replicate10_2
    userdata10[(17:24), 6] <- input$replicate10_3
    userdata10[(25:32), 6] <- input$replicate10_4
    
    userdata11 <- matrix(nrow = 32, ncol = 6)
    userdata11[, 1] <- input$monitorname11 ### change monitor name
    userdata11[, 2] <- as.character(input$startdatetime11) ### start date of recording time is ZT0
    userdata11[, 3] <- as.character(input$enddatetime11) ### stop date
    userdata11[(1:8), 5] <- input$genotype11_1 ### name of your genotype
    userdata11[(9:16), 5] <- input$genotype11_2 ### name of your genotype
    userdata11[(17:24), 5] <- input$genotype11_3 ### name of your genotype
    userdata11[(25:32), 5] <- input$genotype11_4 ### name of your genotype
    userdata11[, 4] <- seq(1, 32, by = 1)
    userdata11[(1:8), 6] <- input$replicate11_1
    userdata11[(9:16), 6] <- input$replicate11_2
    userdata11[(17:24), 6] <- input$replicate11_3
    userdata11[(25:32), 6] <- input$replicate11_4
    
    userdata12 <- matrix(nrow = 32, ncol = 6)
    userdata12[, 1] <- input$monitorname12 ### change monitor name
    userdata12[, 2] <- as.character(input$startdatetime12) ### start date of recording time is ZT0
    userdata12[, 3] <- as.character(input$enddatetime12) ### stop date
    userdata12[(1:8), 5] <- input$genotype12_1 ### name of your genotype
    userdata12[(9:16), 5] <- input$genotype12_2 ### name of your genotype
    userdata12[(17:24), 5] <- input$genotype12_3 ### name of your genotype
    userdata12[(25:32), 5] <- input$genotype12_4 ### name of your genotype
    userdata12[, 4] <- seq(1, 32, by = 1)
    userdata12[(1:8), 6] <- input$replicate12_1
    userdata12[(9:16), 6] <- input$replicate12_2
    userdata12[(17:24), 6] <- input$replicate12_3
    userdata12[(25:32), 6] <- input$replicate12_4

    userdata <- matrix(nrow = 384, ncol = 6)
    if (input$genotype == 1) {
      userdata <- rbind(userdata1)
    } else if (input$genotype == 2) {
      userdata <- rbind(userdata1, userdata2)
    } else if (input$genotype == 3) {
      userdata <- rbind(userdata1, userdata2, userdata3)
    } else if (input$genotype == 4) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4)
    } else if (input$genotype == 5) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4, userdata5)
    } else if (input$genotype == 6) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4, userdata5, userdata6)
    } else if (input$genotype == 7) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4, userdata5, userdata6, userdata7)
    } else if (input$genotype == 8) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4, userdata5, userdata6, userdata7, userdata8)
    } else if (input$genotype == 9) {
      userdata <- rbind(userdata1, userdata2, userdata3, userdata4, userdata5, userdata6, userdata7, userdata9)
    } else if (input$genotype == 10) {
      userdata <-
        rbind(userdata1, userdata2, userdata3, userdata4, userdata5, userdata6, userdata7, userdata8, userdata9, userdata10)
    } else if (input$genotype == 11) {
      userdata <-
        rbind(
          userdata1,
          userdata2,
          userdata3,
          userdata4,
          userdata5,
          userdata6,
          userdata7,
          userdata8,
          userdata9,
          userdata10,
          userdata11
        )
    } else {
      userdata <-
        rbind(
          userdata1,
          userdata2,
          userdata3,
          userdata4,
          userdata5,
          userdata6,
          userdata7,
          userdata8,
          userdata9,
          userdata10,
          userdata11,
          userdata12
        )
    }

    colnames(userdata) <-
      c(
        "file",
        "start_datetime",
        "stop_datetime",
        "region_id",
        "genotype",
        "replicate"
      )
    output$userdata <- renderTable({
      userdata
    })

    # if (input$genotype == 1) {
    #   toscale1 <- 1
    # } else if (input$genotype == 2) {
    #   toscale1 <- 2
    # } else {
    #   toscale1 <- 3
    # }
    # toscale1 <- input$genotype

    # if (input$replicate == 1) {
    #   toscale2 <- 3
    # } else if (input$replicate == 2) {
    #   toscale2 <- 6
    # } else if (input$replicate == 3) {
    #   toscale2 <- 9
    # } else {
    #   toscale2 <- 12
    # }

    # toscale2 <- input$replicate * input$genotype

    observeEvent(input$do, {
      withBusyIndicatorServer("do", {
        write.csv(
          userdata,
          file = paste(
            "Metadata",
            input$monitorname1,
            input$monitorname2,
            input$monitorname3,
            ".csv",
            sep = ""
          ),
          append = F,
          quote = F,
          sep = ",",
          col.names = T,
          row.names = F
        )
      })
    })


    My_Theme <- theme(
      title = element_text(size = 10, face = "bold"),
      axis.title.x = element_text(size = 10, face = "bold"),
      axis.text.x = element_text(size = 8),
      axis.title.y = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(
        size = 0.125,
        linetype = "solid",
        colour = "grey"
      ),
      panel.grid.minor = element_line(
        size = 0.0625,
        linetype = "solid",
        colour = "grey"
      ),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(
        colour = "black",
        size = 8,
        color = "black",
        face = "bold"
      )
    )

    My_Theme_1 <- theme(
      title = element_text(size = 6, face = "bold"),
      axis.title.x = element_text(size = 4, face = "bold"),
      axis.text.x = element_text(size = 3),
      axis.title.y = element_text(size = 3, face = "bold"),
      axis.text.y = element_text(size = 3),
      legend.text = element_text(size = 3),
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(
        size = 0.025,
        linetype = "solid",
        colour = "grey"
      ),
      panel.grid.minor = element_line(
        size = 0.0125,
        linetype = "solid",
        colour = "grey"
      ),
      strip.background = element_rect(fill = "white"),
      strip.text = element_text(
        colour = "black",
        size = 3,
        color = "black",
        face = "bold"
      )
    )

    My_Theme_2 <- theme(
      title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      # aspect.ratio = 1,
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(
        size = 0.125, linetype = "solid",
        colour = "grey"
      ),
      panel.grid.minor = element_line(
        size = 0.0625, linetype = "solid",
        colour = "grey"
      )
    )

    My_Theme_3 <- theme(
      title = element_text(size = 20, face = "bold"),
      axis.title.x = element_text(size = 16, face = "bold"),
      axis.text.x = element_text(size = 14),
      axis.title.y = element_text(size = 16, face = "bold"),
      axis.text.y = element_text(size = 14),
      legend.text = element_text(size = 14),
      # aspect.ratio = .2,
      legend.position = "right",
      panel.background = element_rect(fill = "white"),
      panel.grid.major = element_line(
        size = 0.125, linetype = "solid",
        colour = "grey"
      ),
      panel.grid.minor = element_line(
        size = 0.0625, linetype = "solid",
        colour = "grey"
      )
    )

    volumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    observe({
      shinyDirChoose(input, "folder", roots = volumes, session = session, restrictions = system.file(package = "base"))
    })
    WD <- parseDirPath(volumes, input$folder)
    req(input$meta)
    metadata <- fread(input$meta$datapath)
    metadata <- na.omit(metadata)
    metadata_proc <- link_dam_metadata(metadata, result_dir = WD)
    output$contents <- DT::renderDataTable(
      metadata,
      filter = list(position = "top", clear = FALSE, plain = TRUE)
    )

    observeEvent(input$cal, {
      withBusyIndicatorServer("cal", {
        req(input$meta)
        dt <- load_dam(metadata_proc, FUN = sleepr::sleep_dam_annotation)
        dt[, moving := activity > 0]
        dt_curated <- curate_dead_animals(dt)
        lifespan_dt <- dt_curated[, .(lifespan = max(t)), by = id]
        valid_ids <- lifespan_dt[lifespan > days(input$remove), id]
        dt_curated <- dt_curated[id %in% valid_ids]
        dt_curated <- dt_curated[t %between% c(days(input$start), days(input$end))]
        # dt_curated <- dt_curated[, Day := (t / days(1)), by = id]
        # dt_curated$Day <- ceiling(dt_curated$Day)
        # dt_curated[, 5][dt_curated[, 5] == input$start] <- input$start + 1
        # setkey(dt_curated, Day, id)
        # # dt_curated[, normact := (activity / sum(activity)) * 100, by = .(id, Day)]
        # setkey(dt_curated, id)
        setbehavr(dt_curated, metadata_proc)
        dt_curated[, uid := 1:.N, meta = T]
        dt_curated[, .(id, uid), meta = T]
        summary_dt <- rejoin(dt_curated[, .(sleep_fraction = mean(asleep)), by = id])
        dt_curated[, phase := ifelse(t %% hours(input$ldperiod) < hours(input$light), "L", "D")]
        summary_dt <- rejoin(dt_curated[, .(
          sleep_fraction_all = mean(asleep),
          sleep_fraction_l = mean(asleep[phase == "L"]),
          sleep_fraction_d = mean(asleep[phase == "D"])
        ), by = id])
        summary_dt_melted <- melt(summary_dt,
          measure.vars = patterns("sleep_fraction_"),
          variable.name = "phase", value.name = "sleep_fraction"
        )
        bout_dt <- bout_analysis(asleep, dt_curated)
        bout_dt <- bout_dt[asleep == TRUE, -"asleep"]
        # bout_dt[, .(n_bouts = .N, mean_bout_length = mean(duration)), by = id]
        bout_summary <- bout_dt[, .(
          latency = t[1], first_bout_length = duration[1],
          latency_to_longest_bout = t[which.max(duration)], length_longest_bout = max(duration),
          n_bouts = .N, mean_bout_length = mean(duration)
        ), by = id]
        bout_dt_new <- bout_dt[, phase_day := ifelse(t %% hours(input$ldperiod) < hours(input$light), "L", "D")]
        bout_summary_phase <- bout_dt_new[, .(n_bouts = .N, mean_bout_length = mean(duration)),
          by = c("id", "phase_day")
        ]
        overall_summary <- summary_dt[bout_summary]
        overall_summary_new <- summary_dt[bout_summary_phase]
        beepr::beep(sound = 10)
      })

      # observeEvent(input$do6, {
      # withBusyIndicatorServer("do6", {
      output$alletho <- renderPlot(
        {
          req(input$meta)
          alletho <- ggetho(dt, aes(z = asleep)) +
            stat_ld_annotations(height = 0.03, l_duration = hours(input$light), period = hours(input$ldperiod)) +
            stat_tile_etho() +
            scale_fill_distiller(palette = "Blues") +
            My_Theme
          alletho
        },
        res = 100
      )
      # })
      # })

      output$curatedetho <- renderPlot(
        {
          req(input$meta)
          ggetho(dt_curated, aes(z = asleep)) +
            stat_ld_annotations(height = 0.03, l_duration = hours(input$light), period = hours(input$ldperiod)) +
            stat_tile_etho() +
            scale_fill_distiller(palette = "Blues") +
            My_Theme
        },
        res = 100
      )

      observeEvent(input$popplot_height, {
        output$popplot <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype), summary_time_window = mins(input$min)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~ genotype + id + replicate, ncol = 1, scales = "free_y")
          },
          res = 70,
          width = 1500,
          height = input$genotype * input$popplot_height
        )
      })

      observeEvent(input$popplotwrap_height, {
        output$popplotwrap <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype),
              time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
            ) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~ genotype + id + replicate, ncol = 4, scales = "free_y")
          },
          res = 70,
          width = 1500,
          height = input$genotype * input$popplotwrap_height
        )
      })

      observeEvent(input$popplot1_height, {
        output$popplot1 <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype), summary_time_window = mins(input$min)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~replicate, ncol = 1, scales = "free_y")
            # + theme(aspect.ratio = 0.2)
          },
          res = 70,
          width = 1200,
          height = input$replicate * input$popplot1_height
        )
      })

      observeEvent(input$popplotwrap1_height, {
        output$popplotwrap1 <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype),
              time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
            ) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~replicate, ncol = 1, scales = "free_y")
            # + theme(aspect.ratio = 0.2)
          },
          res = 70,
          width = 1000,
          height = input$replicate * input$popplotwrap1_height
        )
      })

      output$popplotwrap1polar <- renderPlot(
        {
          req(input$meta)
          ggetho(dt_curated, aes(y = asleep, colour = genotype),
            time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
          ) +
            stat_pop_etho(geom = "bar", alpha = 0.4) +
            stat_ld_annotations(height = 1, alpha = .1, x_limits = c(0, hours(24)), outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
            scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent) +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme +
            facet_wrap(~replicate, ncol = 1) +
            coord_polar(clip = "off")
        },
        res = 70,
        width = 700,
        height = input$replicate * 700
      )

      observeEvent(input$popplotwrapbox_height, {
        output$popplotwrapbox <- renderPlot(
          {
            req(input$meta)
            ggplot(summary_dt_melted, aes(x = genotype, y = sleep_fraction, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              # geom_point(aes(color = genotype, fill = genotype), position = "jitter", alpha = 0.5, size =2) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(fun = mean, geom = "point", aes(color = genotype), size = 3, shape = 23) +
              stat_summary(aes(label = round((..y..), 2), color = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5
              ) +
              scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_2 +
              facet_wrap(~replicate, ncol = 1, scales = "free_y")
          },
          res = 70,
          width = 1000,
          height = input$replicate * input$popplotwrapbox_height
        )
      })

      observeEvent(input$popplotwrapboxmelt_height, {
        output$popplotwrapboxmelt <- renderPlot(
          {
            req(input$meta)
            ggplot(summary_dt_melted, aes(x = phase, y = sleep_fraction, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              # geom_point(aes(color = genotype, fill = genotype), position = "jitter", alpha = 0.5, size =2) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 3, shape = 23
              ) +
              stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5, position = position_dodge(.9)
              ) +
              scale_y_continuous(name = "Fraction of time sleeping", labels = scales::percent) +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_2 +
              facet_wrap(~replicate, ncol = 1, scales = "free_y")
          },
          res = 70,
          width = 1000,
          height = input$replicate * input$popplotwrapboxmelt_height
        )
      })

      observeEvent(input$bout_height, {
        output$bout <- renderPlot(
          {
            req(input$meta)
            ggetho(bout_dt, aes(y = duration / 60, colour = genotype), time_wrap = hours(input$modtau), summary_time_window = mins(input$min)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Bout length (min)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_3 +
              facet_wrap(~replicate, ncol = 1, scales = "free_y")
          },
          res = 70,
          width = 1000,
          height = input$replicate * input$bout_height
        )
      })

      observeEvent(input$boutsummary_height, {
        output$boutsummary <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary, aes(n_bouts, mean_bout_length / 60, colour = genotype)) +
              geom_point(size = 4, alpha = 0.5) +
              scale_x_continuous(name = "Number of bouts") +
              scale_y_continuous(name = "Average bout duration (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 1000,
          height = input$replicate * input$boutsummary_height
        )
      })

      observeEvent(input$numbouts_height, {
        output$numbouts <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary, aes(genotype, n_bouts, colour = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 3, shape = 23
              ) +
              stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5, position = position_dodge(.9)
              ) +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Number of bouts") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 700,
          height = input$replicate * input$numbouts_height
        )
      })

      observeEvent(input$meanboutlength_height, {
        output$meanboutlength <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary, aes(genotype, mean_bout_length / 60, colour = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 3, shape = 23
              ) +
              stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5, position = position_dodge(.9)
              ) +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Mean bout length (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 700,
          height = input$replicate * input$meanboutlength_height
        )
      })

      observeEvent(input$numbouts_ld_height, {
        output$numbouts_ld <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary_new, aes(x = phase_day, y = n_bouts, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              # geom_point(aes(color = genotype, fill = genotype), position = "jitter", alpha = 0.5, size =2) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 3, shape = 23
              ) +
              stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5, position = position_dodge(.9)
              ) +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Number of bouts") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 700,
          height = input$replicate * input$numbouts_ld_height
        )
      })

      observeEvent(input$meanboutlength_ld_height, {
        output$meanboutlength_ld <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary_new, aes(x = phase_day, y = mean_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              # geom_point(aes(color = genotype, fill = genotype), position = "jitter", alpha = 0.5, size =2) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 3, shape = 23
              ) +
              stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                fun = mean, geom = "text",
                size = 5, vjust = -0.5, position = position_dodge(.9)
              ) +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Mean bout length (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 700,
          height = input$replicate * input$meanboutlength_ld_height
        )
      })

      observeEvent(input$meanboutlength_distrib_height, {
        output$meanboutlength_distrib <- renderPlot(
          {
            req(input$meta)
            ggplot(overall_summary, aes((mean_bout_length / 60), genotype, color = genotype, fill = genotype)) +
              geom_density_ridges(
                scale = 1, rel_min_height = 0.01, jittered_points = TRUE,
                position = position_points_jitter(width = 0.2, height = 0),
                point_shape = "|", point_size = 8, point_alpha = 1, alpha = .7
              ) +
              facet_wrap(~replicate, ncol = 1) +
              scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
              scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
              coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
              labs(x = "Period (h)", y = "Genotype") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = 700,
          height = input$replicate * input$meanboutlength_distrib_height
        )
      })

      ######################## for writing table#############################
      pro_chi_sq <- overall_summary
      pro_chi_sq_new <- overall_summary_new
      pro_chi_sq$latency <- pro_chi_sq$latency / 60
      pro_chi_sq$first_bout_length <- pro_chi_sq$first_bout_length / 60
      pro_chi_sq$latency_to_longest_bout <- pro_chi_sq$latency_to_longest_bout / 60
      pro_chi_sq$length_longest_bout <- pro_chi_sq$length_longest_bout / 60
      pro_chi_sq$mean_bout_length <- pro_chi_sq$mean_bout_length / 60
      pro_chi_sq <- as.matrix(pro_chi_sq)
      pro_chi_sq_new$mean_bout_length <- pro_chi_sq_new$mean_bout_length / 60
      pro_chi_sq_new <- as.matrix(pro_chi_sq_new)
      ###########################
      output$periodpower <- DT::renderDataTable(
        pro_chi_sq,
        filter = list(position = "top", clear = FALSE, plain = TRUE)
      )
      output$periodpower_new <- DT::renderDataTable(
        pro_chi_sq_new,
        filter = list(position = "top", clear = FALSE, plain = TRUE)
      )

      #############
      output$downloadData_chi_sq <- downloadHandler(
        filename = function() {
          paste("all data.csv", sep = "")
        },
        content = function(file) {
          write.csv(pro_chi_sq, file, row.names = FALSE)
        }
      )
      output$downloadData_chi_sq_new <- downloadHandler(
        filename = function() {
          paste("all data bout details.csv", sep = "")
        },
        content = function(file) {
          write.csv(pro_chi_sq_new, file, row.names = FALSE)
        }
      )


      ################
    })
    output$report <- downloadHandler(
      # For PDF output, change this to "report.pdf"
      filename = "report.html",
      content = function(file) {
        # Copy the report file to a temporary directory before processing it, in
        # case we don't have write permissions to the current working dir (which
        # can happen when deployed).
        tempReport <- file.path(tempdir(), "report.Rmd")
        file.copy("report.Rmd", tempReport, overwrite = TRUE)

        # Set up parameters to pass to Rmd document
        params <- list(
          meta = input$meta, wd = getwd(), modtau = input$modtau, ldperiod = input$ldperiod,
          min = input$min, light = input$light, genotype = input$genotype,
          replicate = input$replicate, remove = input$remove, start = input$start, end = input$end
        )

        # Knit the document, passing in the `params` list, and eval it in a
        # child of the global environment (this isolates the code in the document
        # from the code in this app).
        rmarkdown::render(tempReport,
          output_file = file,
          params = params,
          envir = new.env(parent = globalenv())
        )
      }
    )
  })
})
