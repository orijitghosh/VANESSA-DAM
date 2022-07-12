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
library(ggforce)
shinyServer(function(input, output, session) {
  shinyalert(
    title = "Visualization and ANalysis of timE SerieS dAta - Drosophila Activity Monitors (VANESSA-DAM) for sleep analysis!",
    text = "<b>This app requires a metadata file for your monitors, to make the metadata files, first visit the Data formatting tab. The metadata files will be created in the home folder of the app. Right now simultaneous analysis and visualization upto twelve genotypes are supported. Contact <i>arijitghosh2009@gmail.com</i> for bugs, suggestions, troubleshooting and customizations.</b>",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = TRUE,
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
    req(
      # input$modtau, input$ldperiod, input$min, input$light,
      input$genotype, input$replicate, input$remove, input$start, input$end
    )
    observeEvent(input$updatemeta, {
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
      userdataforprint <<- userdata
    })

    output$downloadmetadata <- downloadHandler(
      filename = function() {
        paste("Metadata_user.csv", sep = "")
      },
      content = function(file) {
        write.csv(userdataforprint, file, append = F, quote = F, sep = ",", col.names = T, row.names = F)
      }
    )


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

    req(input$meta, input$data)
    metadata <- read.csv(input$meta$datapath)
    metadata <- na.omit(metadata)
    #########From Dean Attali########
    fixUploadedFilesNames <- function(x) {
      if (is.null(x)) {
        return()
      }
      
      oldNames = x$datapath
      newNames = file.path(dirname(x$datapath),
                           x$name)
      file.rename(from = oldNames, to = newNames)
      x$datapath <- newNames
      x
    }
    ###############
    file.copy(fixUploadedFilesNames(input$data)$datapath, ".", recursive = TRUE, overwrite = TRUE)
    metadata_proc <- link_dam_metadata(metadata, result_dir = ".")
    output$contents <- DT::renderDataTable(
      metadata,
      filter = list(position = "top", clear = FALSE, plain = TRUE)
    )

    observeEvent(input$cal, {
      withBusyIndicatorServer("cal", {
        req(input$meta, input$data)
        dt <- load_dam(metadata_proc, FUN = sleepr::sleep_dam_annotation)
        dt[, moving := activity > 0]
        dt_curated <- curate_dead_animals(dt)
        dt_curated[, day := ceiling(t / days(1))]
        dt_curated$day[dt_curated$day == 0] <- 1
        dt_curated[, phase := ifelse(t %% hours(input$ldperiod) > hours(input$light), "Dark", "Light")]
        dt_curated[, phase := factor(phase, levels = c("Light", "Dark"))]
        setbehavr(dt_curated, metadata_proc)
        lifespan_dt <- dt_curated[, .(lifespan = max(t)), by = id]
        valid_ids <- lifespan_dt[lifespan > days(input$remove), id]
        dt_curated <- dt_curated[id %in% valid_ids]
        dt_curated <- dt_curated[day %between% c(input$start, input$end)]
        setbehavr(dt_curated, metadata_proc)
        dt_curated[, uid := 1:.N, meta = T]
        dt_curated[, .(id, uid), meta = T]
        summary_dt <- (dt_curated[, .(
          sleep_fraction_all = mean(asleep),
          sleep_fraction_l = mean(asleep[phase == "Light"]),
          sleep_fraction_d = mean(asleep[phase == "Dark"])
        ), by = c("id", "day")])
        summary_dt <- dt_curated[meta = T][summary_dt]
        summary_dt_melted <- melt(summary_dt,
          measure.vars = patterns("sleep_fraction_"),
          variable.name = "phase", value.name = "sleep_fraction"
        )

        bout_dt <- bout_analysis(asleep, dt_curated)
        bout_dt[, day := ceiling(t / days(1))]
        bout_dt$day[bout_dt$day == 0] <- 1
        setbehavr(bout_dt, metadata_proc)
        bout_dt_awake <- bout_dt[asleep == FALSE]
        bout_dt <- bout_dt[asleep == TRUE, -"asleep"]
        bout_summary <- bout_dt[, .(
          latency = t[1] - ((day - 1) * 86400), first_bout_length = duration[1],
          latency_to_longest_bout = t[which.max(duration)], length_longest_bout = max(duration),
          n_bouts = .N, mean_bout_length = mean(duration), total_bout_length = sum(duration)
        ), by = c("id", "day")]
        bout_summary <- bout_dt[meta = T][bout_summary]
        bout_dt_new <- bout_dt[, phase := ifelse(t %% hours(input$ldperiod) > hours(input$light), "Dark", "Light")]
        bout_dt_new[, phase := factor(phase, levels = c("Light", "Dark"))]
        bout_summary_phase <- bout_dt_new[, .(
          n_bouts = .N, mean_bout_length = mean(duration),
          total_bout_length = sum(duration), latency = t[1] - ((day - 1) * 86400)
        ), by = c("id", "phase", "day")]
        bout_summary_phase <- bout_dt_new[meta = T][bout_summary_phase]
        bout_summary_awake <- bout_dt_awake[, .(
          latency = t[1] - ((day - 1) * 86400), first_bout_length = duration[1],
          latency_to_longest_bout = t[which.max(duration)], length_longest_bout = max(duration),
          n_bouts = .N, mean_bout_length = mean(duration), total_bout_length = sum(duration)
        ), by = c("id", "day")]
        bout_summary_awake <- bout_dt_awake[meta = T][bout_summary_awake]
        bout_dt_new_awake <- bout_dt_awake[, phase := ifelse(t %% hours(input$ldperiod) > hours(input$light), "Dark", "Light")]
        bout_dt_new_awake[, phase := factor(phase, levels = c("Light", "Dark"))]
        bout_summary_phase_awake <- bout_dt_new_awake[, .(
          n_bouts = .N, mean_bout_length = mean(duration),
          total_bout_length = sum(duration)
        ), by = c("id", "phase", "day")]
        bout_summary_phase_awake <- bout_dt_new_awake[meta = T][bout_summary_phase_awake]
        beepr::beep(sound = 10)
      })

      observeEvent(input$plotalletho, {
        output$alletho <- renderPlot(
          {
            req(input$meta)
            alletho <- ggetho(dt, aes(z = asleep), summary_time_window = mins(input$min)) +
              stat_ld_annotations(height = 0.03, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              stat_tile_etho() +
              scale_fill_distiller(palette = "Blues", trans = "reverse") +
              My_Theme
            alletho
          },
          res = 100,
          width = input$alletho_width,
          height = input$alletho_height
        )
      })

      observeEvent(input$plotcuratedetho, {
        output$curatedetho <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(z = asleep), summary_time_window = mins(input$min)) +
              stat_ld_annotations(height = 0.03, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              stat_tile_etho() +
              scale_fill_distiller(palette = "Blues", trans = "reverse") +
              My_Theme
          },
          res = 100,
          width = input$curatedetho_width,
          height = input$curatedetho_height
        )
      })

      observeEvent(input$plotcuratedetho_wrap, {
        output$curatedetho_wrap <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(z = asleep), time_wrap = hours(input$modtau), summary_time_window = mins(input$min)) +
              stat_ld_annotations(height = 0.03, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              stat_tile_etho() +
              scale_fill_distiller(palette = "Blues", trans = "reverse") +
              # scale_fill_viridis_c(option = "B") +
              My_Theme
          },
          res = 100,
          width = input$curatedetho_wrap_width,
          height = input$curatedetho_wrap_height
        )
      })

      observeEvent(input$plotpopplot, {
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
          width = input$popplot_width,
          height = input$genotype * input$popplot_height
        )
      })

      observeEvent(input$plotpopplotwrap, {
        output$popplotwrap <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype),
              time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
            ) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~ genotype + id + replicate, ncol = 4, scales = "free_y")
          },
          res = 70,
          width = input$popplotwrap_width,
          height = input$genotype * input$popplotwrap_height
        )
      })

      observeEvent(input$plotpopplot1, {
        output$popplot1 <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype), summary_time_window = mins(input$min)) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~replicate, scales = "free_y")
          },
          res = 70,
          width = input$popplot1_width,
          height = input$replicate * input$popplot1_height
        )
      })

      observeEvent(input$plotpopplotwrap1, {
        output$popplotwrap1 <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype),
              time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
            ) +
              stat_pop_etho() +
              stat_ld_annotations(height = 1, alpha = 0.1, outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~replicate, scales = "free_y")
          },
          res = 70,
          width = input$popplotwrap1_width,
          height = input$replicate * input$popplotwrap1_height
        )
      })

      observeEvent(input$plotpopplotwrap1polar, {
        output$popplotwrap1polar <- renderPlot(
          {
            req(input$meta)
            ggetho(dt_curated, aes(y = asleep, colour = genotype),
              time_wrap = hours(input$modtau), summary_time_window = mins(input$min)
            ) +
              stat_pop_etho(geom = "bar", alpha = 0.4) +
              stat_ld_annotations(height = 1, alpha = .1, x_limits = c(0, hours(24)), outline = NA, l_duration = hours(input$light), period = hours(input$ldperiod)) +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme +
              facet_wrap(~replicate) +
              coord_polar(clip = "off")
          },
          res = 70,
          width = input$popplotwrap1polar_width,
          height = input$replicate * input$popplotwrap1polar_height
        )
      })

      observeEvent(input$plotpopplotwrapbox, {
        output$popplotwrapbox <- renderPlot(
          {
            req(input$meta)
            ggplot(summary_dt, aes(x = genotype, y = sleep_fraction_all, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +    ######if boxplot needed
              # geom_jitter(aes(colour = genotype), size = 2,  alpha = .5, position = position_jitter(height = .1, width = .1)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(fun = mean, geom = "point", aes(color = genotype), size = 4, shape = 23) +
              {
                if (input$popplotwrapbox_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5
                  )
                }
              } +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_2 +
              facet_grid(day ~ replicate, scales = "free_y")
          },
          res = 70,
          width = input$popplotwrapbox_width,
          height = input$replicate * input$popplotwrapbox_height
        )
      })

      observeEvent(input$plotpopplotwrapboxmelt, {
        output$popplotwrapboxmelt <- renderPlot(
          {
            req(input$meta)
            ggplot(summary_dt_melted, aes(x = phase, y = sleep_fraction, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  ##########if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$popplotwrapboxmelt_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_y_continuous(name = "Fraction of time sleeping") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_2 +
              facet_grid(day ~ replicate, scales = "free_y")
          },
          res = 70,
          width = input$popplotwrapboxmelt_width,
          height = input$replicate * input$popplotwrapboxmelt_height
        )
      })

      observeEvent(input$plotbout, {
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
              facet_grid(~replicate, scales = "free_y")
          },
          res = 70,
          width = input$bout_width,
          height = input$replicate * input$bout_height
        )
      })

      observeEvent(input$plotboutsummary, {
        output$boutsummary <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes(n_bouts, mean_bout_length / 60, colour = genotype, fill = genotype)) +
              geom_point(size = 4, alpha = 0.5) +
              scale_x_continuous(name = "Number of bouts") +
              scale_y_continuous(name = "Average bout duration (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$boutsummary_width,
          height = input$replicate * input$boutsummary_height
        )
      })

      observeEvent(input$plotnumbouts, {
        output$numbouts <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes(genotype, n_bouts, colour = genotype, fill = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$numbouts_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Number of bouts") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$numbouts_width,
          height = input$replicate * input$numbouts_height
        )
      })

      observeEvent(input$plotlatency, {
        output$latency <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes(genotype, latency / 60, colour = genotype, fill = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$latency_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Latency (min)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$latency_width,
          height = input$replicate * input$latency_height
        )
      })

      observeEvent(input$plotlatency_ld, {
        output$latency_ld <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_phase, aes(phase, latency / 60, colour = genotype, fill = genotype)) +
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$latency_ld_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Phase") +
              scale_y_continuous(name = "Latency (min)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$latency_ld_width,
          height = input$replicate * input$latency_ld_height
        )
      })

      observeEvent(input$plotnumbouts_awake, {
        output$numbouts_awake <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_awake, aes(genotype, n_bouts, colour = genotype, fill = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$numbouts_awake_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Number of awake bouts") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$numbouts_awake_width,
          height = input$replicate * input$numbouts_awake_height
        )
      })

      observeEvent(input$plotmeanboutlength, {
        output$meanboutlength <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes(genotype, mean_bout_length / 60, colour = genotype, fill = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$meanboutlength_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Mean bout length (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$meanboutlength_width,
          height = input$replicate * input$meanboutlength_height
        )
      })

      observeEvent(input$plotmeanboutlength_awake, {
        output$meanboutlength_awake <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_awake, aes(genotype, mean_bout_length / 60, colour = genotype, fill = genotype)) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_jitter(height = .2, width = .2)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$meanboutlength_awake_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Mean awake bout length (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$meanboutlength_awake_width,
          height = input$replicate * input$meanboutlength_awake_height
        )
      })

      observeEvent(input$plotnumbouts_ld, {
        output$numbouts_ld <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_phase, aes(x = phase, y = n_bouts, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +   #########if boxplots needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$numbouts_ld_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Number of bouts") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$numbouts_ld_width,
          height = input$replicate * input$numbouts_ld_height
        )
      })

      observeEvent(input$plotmeanboutlength_ld, {
        output$meanboutlength_ld <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_phase, aes(x = phase, y = mean_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  #######if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$meanboutlength_ld_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Mean bout length (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$meanboutlength_ld_width,
          height = input$replicate * input$meanboutlength_ld_height
        )
      })

      observeEvent(input$plotmeanboutlength_distrib, {
        output$meanboutlength_distrib <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes((mean_bout_length / 60), genotype, color = genotype, fill = genotype)) +
              geom_density_ridges(
                scale = 1, rel_min_height = 0.01, jittered_points = TRUE,
                position = position_points_jitter(width = 0.2, height = 0),
                point_shape = "|", point_size = 8, point_alpha = 1, alpha = .7
              ) +
              facet_wrap(day ~ replicate, ncol = 1) +
              scale_y_discrete(expand = c(0, 0)) + # will generally have to set the `expand` option
              scale_x_continuous(expand = c(0, 0)) + # for both axes to remove unneeded padding
              coord_cartesian(clip = "off") + # to avoid clipping of the very top of the top ridgeline
              labs(x = "Mean bout length (m)", y = "Genotype") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$meanboutlength_distrib_width,
          height = input$replicate * input$meanboutlength_distrib_height
        )
      })

      observeEvent(input$plottotal_sleep, {
        output$total_sleep <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary, aes(x = genotype, y = total_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  #######if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$total_sleep_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Total time sleeping (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$total_sleep_width,
          height = input$replicate * input$total_sleep_height
        )
      })

      observeEvent(input$plottotal_awake, {
        output$total_awake <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_awake, aes(x = genotype, y = total_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  #######if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$total_awake_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Total time awake (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$total_awake_width,
          height = input$replicate * input$total_awake_height
        )
      })

      observeEvent(input$plottotal_sleep_phase, {
        output$total_sleep_phase <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_phase, aes(x = phase, y = total_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  #######if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$total_sleep_phase_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Total time sleeping (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$total_sleep_phase_width,
          height = input$replicate * input$total_sleep_phase_height
        )
      })

      observeEvent(input$plottotal_awake_phase, {
        output$total_awake_phase <- renderPlot(
          {
            req(input$meta)
            ggplot(bout_summary_phase_awake, aes(x = phase, y = total_bout_length / 60, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +  #######if boxplot needed
              # geom_jitter(aes(colour = genotype), alpha = .5, position = position_dodge(.9)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(
                fun = mean, geom = "point", aes(color = genotype, group = genotype),
                position = position_dodge(.9), size = 4, shape = 23
              ) +
              {
                if (input$total_awake_phase_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype, group = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5, position = position_dodge(.9)
                  )
                }
              } +
              scale_x_discrete(name = "Genotype") +
              scale_y_continuous(name = "Total time awake (m)") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              facet_grid(day ~ replicate, scales = "free_y") +
              My_Theme_2
          },
          res = 70,
          width = input$total_awake_phase_width,
          height = input$replicate * input$total_awake_phase_height
        )
      })

      observeEvent(input$plotact_index, {
        output$act_index <- renderPlot(
          {
            req(input$meta)
            activity_count_awake <- dt_curated[, .(
              activity_count_awake = sum(activity[asleep == FALSE])
            ), by = c("id", "day")]
            activity_count_awake <- dt_curated[meta = T][activity_count_awake]
            activity_count_awake <- activity_count_awake[bout_summary_awake, on = .(id=id,day=day)]
            activity_index <- as.data.frame(bout_summary_awake)
            activity_index$activity_index <- (activity_index$total_bout_length / 60) / activity_count_awake$activity_count_awake
            ggplot(activity_index, aes(x = genotype, y = activity_index, fill = genotype)) +
              # geom_boxplot(outlier.colour = "red") +    ######if boxplot needed
              # geom_jitter(aes(colour = genotype), size = 2, alpha = .5, position = position_jitter(height = .1, width = .1)) +
              geom_sina(aes(color = genotype), alpha = .6) +
              geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
              stat_summary(fun = mean, geom = "point", aes(color = genotype), size = 4, shape = 23) +
              {
                if (input$act_index_text == TRUE) {
                  stat_summary(aes(label = round((..y..), 2), color = genotype),
                    fun = mean, geom = "text",
                    size = 5, vjust = -0.5
                  )
                }
              } +
              scale_y_continuous(name = "Activity index") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme_2 +
              facet_grid(day ~ replicate, scales = "free_y")
          },
          res = 70,
          width = input$act_index_width,
          height = input$act_index_height
        )
      })


      ######################## for writing table#############################
      pro_chi_sq <- bout_summary
      pro_chi_sq_new <- bout_summary_phase
      pro_chi_sq$latency <- pro_chi_sq$latency / 60
      pro_chi_sq$first_bout_length <- pro_chi_sq$first_bout_length / 60
      pro_chi_sq$latency_to_longest_bout <- pro_chi_sq$latency_to_longest_bout / 60
      pro_chi_sq$length_longest_bout <- pro_chi_sq$length_longest_bout / 60
      pro_chi_sq$mean_bout_length <- pro_chi_sq$mean_bout_length / 60
      pro_chi_sq$total_bout_length <- pro_chi_sq$total_bout_length / 60
      pro_chi_sq <- as.matrix(pro_chi_sq)
      pro_chi_sq_new$mean_bout_length <- pro_chi_sq_new$mean_bout_length / 60
      pro_chi_sq_new$total_bout_length <- pro_chi_sq_new$total_bout_length / 60
      pro_chi_sq_new$latency <- pro_chi_sq_new$latency / 60
      pro_chi_sq_new <- as.matrix(pro_chi_sq_new)
      popplot <- ggetho(dt_curated, aes(y = asleep, colour = genotype), summary_time_window = mins(15)) +
        stat_pop_etho()
      popplot_wrap <- ggetho(dt_curated, aes(y = asleep, colour = genotype), summary_time_window = mins(15),
                             time_wrap = hours(24)) +
        stat_pop_etho()
      df_new <- ggplot_build(popplot)$plot$data   ##########works perfectly - individuals with each day profile
      df_new <- df_new[, -c("file_info")]
      # df_new <- as.data.table(df_new)
      df_new_wrap <- ggplot_build(popplot_wrap)$plot$data  ###individuals wrapped over days
      df_new_wrap_genotype <- as.data.table(df_new_wrap)
      df_new_wrap_genotype_summary_replicate <- (df_new_wrap_genotype[, .(
        sleep_fraction_genotype = mean(asleep)
      ), by = c("genotype", "t", "replicate")])   ##########average profile over genotype replicates separate
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
          paste("bout analysis data.csv", sep = "")
        },
        content = function(file) {
          write.csv(pro_chi_sq, file, row.names = FALSE)
        }
      )
      output$downloadData_chi_sq_new <- downloadHandler(
        filename = function() {
          paste("bout analysis data with phase details.csv", sep = "")
        },
        content = function(file) {
          write.csv(pro_chi_sq_new, file, row.names = FALSE)
        }
      )
      output$downloadData_ind_sleep_pro <- downloadHandler(
        filename = function() {
          paste("individual sleep profiles.csv", sep = "")
        },
        content = function(file) {
          write.csv(df_new, file, row.names = FALSE)
        }
      )
      output$downloadData_avg_sleep_pro <- downloadHandler(
        filename = function() {
          paste("average sleep profiles.csv", sep = "")
        },
        content = function(file) {
          write.csv(df_new_wrap_genotype_summary_replicate, file, row.names = FALSE)
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
