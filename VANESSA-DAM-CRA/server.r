library(shiny)
library(WaveletComp)
library(ggetho)
library(zeitgebr)
library(ggplot2)
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
    title = "Shiny Wrapper for Circadian Rhythms Analysis!",
    text = "This app requires a metadata file for your monitors, to make the metadata files, first visit the Data formatting tab. The metadata files will be created in the home folder of the app. Right now simultaneous analysis and visualization upto twelve genotypes are supported.",
    closeOnEsc = TRUE,
    closeOnClickOutside = FALSE,
    html = FALSE,
    type = "info",
    showConfirmButton = TRUE,
    showCancelButton = FALSE,
    confirmButtonText = "Understood!",
    confirmButtonCol = "#AEDEF4",
    timer = 0,
    imageUrl = "",
    animation = TRUE
  )
  session$onSessionEnded(stopApp)
  observe_helpers(withMathJax = TRUE)
  observe({
    userdata1 <- matrix(nrow = 32, ncol = 6)
    userdata1[, 1] <- input$monitorname1 ### change monitor name
    userdata1[, 2] <-
      as.character(input$startdatetime1) ### start date of recording time is ZT0
    userdata1[, 3] <- as.character(input$enddatetime1) ### stop date
    userdata1[, 5] <- input$genotype1 ### name of your genotype
    userdata1[, 4] <- seq(1, 32, by = 1)
    userdata1[, 6] <- input$replicate1

    userdata2 <- matrix(nrow = 32, ncol = 6)
    userdata2[, 1] <- input$monitorname2 ### change monitor name
    userdata2[, 2] <-
      as.character(input$startdatetime2) ### start date of recording time is ZT0
    userdata2[, 3] <- as.character(input$enddatetime2) ### stop date
    userdata2[, 5] <- input$genotype2 ### name of your genotype
    userdata2[, 4] <- seq(1, 32, by = 1)
    userdata2[, 6] <- input$replicate2

    userdata3 <- matrix(nrow = 32, ncol = 6)
    userdata3[, 1] <- input$monitorname3 ### change monitor name
    userdata3[, 2] <-
      as.character(input$startdatetime3) ### start date of recording time is ZT0
    userdata3[, 3] <- as.character(input$enddatetime3) ### stop date
    userdata3[, 5] <- input$genotype3 ### name of your genotype
    userdata3[, 4] <- seq(1, 32, by = 1)
    userdata3[, 6] <- input$replicate3

    userdata4 <- matrix(nrow = 32, ncol = 6)
    userdata4[, 1] <- input$monitorname4 ### change monitor name
    userdata4[, 2] <-
      as.character(input$startdatetime4) ### start date of recording time is ZT0
    userdata4[, 3] <- as.character(input$enddatetime4) ### stop date
    userdata4[, 5] <- input$genotype4 ### name of your genotype
    userdata4[, 4] <- seq(1, 32, by = 1)
    userdata4[, 6] <- input$replicate4

    userdata5 <- matrix(nrow = 32, ncol = 6)
    userdata5[, 1] <- input$monitorname5 ### change monitor name
    userdata5[, 2] <-
      as.character(input$startdatetime5) ### start date of recording time is ZT0
    userdata5[, 3] <- as.character(input$enddatetime5) ### stop date
    userdata5[, 5] <- input$genotype5 ### name of your genotype
    userdata5[, 4] <- seq(1, 32, by = 1)
    userdata5[, 6] <- input$replicate5

    userdata6 <- matrix(nrow = 32, ncol = 6)
    userdata6[, 1] <- input$monitorname6 ### change monitor name
    userdata6[, 2] <-
      as.character(input$startdatetime6) ### start date of recording time is ZT0
    userdata6[, 3] <- as.character(input$enddatetime6) ### stop date
    userdata6[, 5] <- input$genotype6 ### name of your genotype
    userdata6[, 4] <- seq(1, 32, by = 1)
    userdata6[, 6] <- input$replicate6

    userdata7 <- matrix(nrow = 32, ncol = 6)
    userdata7[, 1] <- input$monitorname7 ### change monitor name
    userdata7[, 2] <-
      as.character(input$startdatetime7) ### start date of recording time is ZT0
    userdata7[, 3] <- as.character(input$enddatetime7) ### stop date
    userdata7[, 5] <- input$genotype7 ### name of your genotype
    userdata7[, 4] <- seq(1, 32, by = 1)
    userdata7[, 6] <- input$replicate7

    userdata8 <- matrix(nrow = 32, ncol = 6)
    userdata8[, 1] <- input$monitorname8 ### change monitor name
    userdata8[, 2] <-
      as.character(input$startdatetime8) ### start date of recording time is ZT0
    userdata8[, 3] <- as.character(input$enddatetime8) ### stop date
    userdata8[, 5] <- input$genotype8 ### name of your genotype
    userdata8[, 4] <- seq(1, 32, by = 1)
    userdata8[, 6] <- input$replicate8

    userdata9 <- matrix(nrow = 32, ncol = 6)
    userdata9[, 1] <- input$monitorname9 ### change monitor name
    userdata9[, 2] <-
      as.character(input$startdatetime9) ### start date of recording time is ZT0
    userdata9[, 3] <- as.character(input$enddatetime9) ### stop date
    userdata9[, 5] <- input$genotype9 ### name of your genotype
    userdata9[, 4] <- seq(1, 32, by = 1)
    userdata9[, 6] <- input$replicate9

    userdata10 <- matrix(nrow = 32, ncol = 6)
    userdata10[, 1] <- input$monitorname10 ### change monitor name
    userdata10[, 2] <-
      as.character(input$startdatetime10) ### start date of recording time is ZT0
    userdata10[, 3] <- as.character(input$enddatetime10) ### stop date
    userdata10[, 5] <- input$genotype10 ### name of your genotype
    userdata10[, 4] <- seq(1, 32, by = 1)
    userdata10[, 6] <- input$replicate10

    userdata11 <- matrix(nrow = 32, ncol = 6)
    userdata11[, 1] <- input$monitorname11 ### change monitor name
    userdata11[, 2] <-
      as.character(input$startdatetime11) ### start date of recording time is ZT0
    userdata11[, 3] <- as.character(input$enddatetime11) ### stop date
    userdata11[, 5] <- input$genotype11 ### name of your genotype
    userdata11[, 4] <- seq(1, 32, by = 1)
    userdata11[, 6] <- input$replicate11

    userdata12 <- matrix(nrow = 32, ncol = 6)
    userdata12[, 1] <- input$monitorname12 ### change monitor name
    userdata12[, 2] <-
      as.character(input$startdatetime12) ### start date of recording time is ZT0
    userdata12[, 3] <- as.character(input$enddatetime12) ### stop date
    userdata12[, 5] <- input$genotype12 ### name of your genotype
    userdata12[, 4] <- seq(1, 32, by = 1)
    userdata12[, 6] <- input$replicate12

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

    if (input$genotype == 1) {
      toscale1 <- 1
    } else if (input$genotype == 2) {
      toscale1 <- 2
    } else {
      toscale1 <- 3
    }

    if (input$replicate == 1) {
      toscale2 <- 3
    } else if (input$replicate == 2) {
      toscale2 <- 6
    } else if (input$replicate == 3) {
      toscale2 <- 9
    } else {
      toscale2 <- 12
    }

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
      title = element_text(size = 8, face = "bold"),
      axis.title.x = element_text(size = 8, face = "bold"),
      axis.text.x = element_text(size = 7),
      axis.title.y = element_text(size = 8, face = "bold"),
      axis.text.y = element_text(size = 7),
      legend.text = element_text(size = 7),
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
      ),
    )

    My_Theme1 <- theme(
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
      ),
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
        dt <- load_dam(metadata_proc)
        dt[, moving := activity > 0]
        dt_curated <- curate_dead_animals(dt)
        lifespan_dt <- dt_curated[, .(lifespan = max(t)), by = id]
        valid_ids <- lifespan_dt[lifespan > days(input$remove), id]
        dt_curated <- dt_curated[id %in% valid_ids]
        dt_curated[, uid := 1:.N, meta = T]
        dt_curated[, .(id, uid), meta = T]
        dt_curated <- dt_curated[t %between% c(days(input$start), days(input$end))]
        beepr::beep(sound = 10)
      })

      observeEvent(input$do6, {
        withBusyIndicatorServer("do6", {
          output$alletho <- renderPlot(
            {
              req(input$meta)
              alletho <-
                ggetho(dt,
                  aes(z = activity),
                  summary_time_window = mins(input$min)
                ) +
                stat_ld_annotations(
                  height = 0.01,
                  l_duration = hours(input$light),
                  period = hours(input$ldperiod)
                ) +
                stat_tile_etho() +
                My_Theme +
                scale_fill_distiller(palette = "Blues")
              alletho
            },
            res = 100
          )
        })
      })

      output$allacto <- renderPlot(
        {
          req(input$meta)
          alletho <-
            ggetho(dt, aes(z = activity), summary_time_window = mins(input$min)) +
            stat_ld_annotations(
              height = 0.005,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            My_Theme
          alletho
        },
        res = 100
      )

      output$curatedetho <- renderPlot(
        {
          req(input$meta)
          ggetho(dt_curated,
            aes(z = activity),
            summary_time_window = mins(input$min)
          ) +
            stat_ld_annotations(
              height = 0.01,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_tile_etho() +
            My_Theme +
            scale_fill_distiller(palette = "Blues")
        },
        res = 100
      )

      output$curatedacto <- renderPlot(
        {
          req(input$meta)
          alletho <-
            ggetho(dt_curated,
              aes(z = activity),
              summary_time_window = mins(input$min)
            ) +
            stat_ld_annotations(
              height = 0.005,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            My_Theme
          alletho
        },
        res = 100
      )

      toscale <-
        nrow(as.matrix(as.character(unique(
          dt_curated$id
        ))))

      output$curatedactoraw <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(z = activity),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + uid + replicate, ncol = 4, scales = "free_y") +
            theme(strip.background = element_rect(fill = "red")) +
            theme(strip.text = element_text(colour = "white")) +
            My_Theme
        },
        res = 100,
        width = 1600,
        height = (toscale * 70)
      )



      output$curatedactoavg <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(z = moving),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + uid + replicate, ncol = 4, scales = "free_y") +
            My_Theme
        },
        res = 100,
        width = 1600,
        height = (toscale * 70)
      )

      output$rawpro <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = activity,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            facet_wrap(~ genotype + uid + replicate, ncol = 4, scales = "free_y") +
            My_Theme
        },
        res = 100,
        width = 1600,
        height = (toscale * 50)
      )

      output$avgpro <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = moving,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            facet_wrap(~ genotype + uid + replicate, ncol = 4, scales = "free_y") +
            My_Theme
        },
        res = 100,
        width = 1600,
        height = (toscale * 50)
      )

      output$avgdaywisepro1 <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = moving,
              colour = genotype
            ),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            facet_wrap(~replicate, ncol = 1, scales = "free_y") +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 500)
      )

      output$avgdaywisepro1_raw <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = activity,
              colour = genotype
            ),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            facet_wrap(~replicate, ncol = 1, scales = "free_y") +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 500)
      )

      output$rawpro1 <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = activity,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            facet_wrap(~replicate, ncol = 1, scales = "free_y") +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 500)
      )

      output$avgpro1 <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = moving,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            facet_wrap(~replicate, ncol = 1, scales = "free_y") +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 500)
      )

      output$rawpro1all <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = activity,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = 500
      )

      output$avgpro1all <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = moving,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min),
            time_offset = hours(6)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = 0.1,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho() +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme
        },
        res = 100,
        width = 1500,
        height = 500
      )


      output$avgprocircular <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = moving,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) +
            stat_ld_annotations(
              height = 1,
              alpha = .1,
              x_limits = c(0, hours(24)),
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho(geom = "bar", alpha = 0.4) +
            facet_wrap(~replicate, ncol = 1) +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme +
            coord_polar(clip = "off")
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 1000)
      )

      output$avgprocircular_raw <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(
              x = t,
              y = activity,
              colour = genotype
            ),
            time_wrap = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) +
            stat_ld_annotations(
              height = 3,
              alpha = .1,
              x_limits = c(0, hours(24)),
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_pop_etho(geom = "bar", alpha = 0.4) +
            facet_wrap(~replicate, ncol = 1) +
            scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
            My_Theme +
            coord_polar(clip = "off")
        },
        res = 100,
        width = 1500,
        height = (input$replicate * 1000)
      )

      output$indiv_raw <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(x = t, z = activity),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + replicate, ncol = 1, scales = "free_y") +
            My_Theme1
        },
        res = 300,
        width = 1000,
        height = (toscale2 * 600)
      )

      output$indiv_avg <- renderPlot(
        {
          req(input$meta)
          ggetho(
            dt_curated,
            aes(x = t, z = moving),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + replicate, ncol = 1, scales = "free_y") +
            My_Theme1
        },
        res = 300,
        width = 1000,
        height = (toscale2 * 600)
      )

      observeEvent(input$show, {
        l <- as.matrix(as.character(unique(dt_curated$id)))
        if (input$ind_act_met == 1) {
          output$indiv1 <- renderPlot({
            req(input$meta)
            ggetho(
              dt_curated[id == l[input$ind]],
              aes(x = t, z = activity),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it moving
              stat_ld_annotations(
                height = 1,
                alpha = 0.05,
                outline = NA,
                l_duration = hours(input$light),
                period = hours(input$ldperiod)
              ) +
              stat_bar_tile_etho() +
              facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
              My_Theme
          })
        } else {
          output$indiv1 <- renderPlot({
            req(input$meta)
            ggetho(
              dt_curated[id == l[input$ind]],
              aes(x = t, z = moving),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it moving
              stat_ld_annotations(
                height = 1,
                alpha = 0.05,
                outline = NA,
                l_duration = hours(input$light),
                period = hours(input$ldperiod)
              ) +
              stat_bar_tile_etho() +
              facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
              My_Theme
          })
        }
      })

      observeEvent(input$show3, {
        l <- as.matrix(as.character(unique(dt_curated$id)))
        output$indiv1_avg <- renderPlot({
          req(input$meta)
          ggetho(
            dt_curated[id == l[input$ind]],
            aes(x = t, z = moving),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
            My_Theme
        })
      })

      observeEvent(input$show1, {
        l <- as.matrix(as.character(unique(dt_curated$id)))
        output$indiv1_norm <- renderPlot({
          req(input$meta)
          ggetho(
            dt_curated[id == l[input$ind]],
            aes(x = t, z = moving),
            multiplot = 2,
            multiplot_period = hours(input$modtau),
            summary_time_window = mins(input$min)
          ) + ### can change to activity if raw needed or keep it moving
            stat_ld_annotations(
              height = 1,
              alpha = 0.05,
              outline = NA,
              l_duration = hours(input$light),
              period = hours(input$ldperiod)
            ) +
            stat_bar_tile_etho() +
            facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
            My_Theme
        })
      })




      ######################### for chi-square calculations##############################
      observeEvent(input$do3, {
        withBusyIndicatorServer("do3", {
          if (input$permethod == "Chi-square") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              FUN = chi_sq_periodogram
            )
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            # mf_chi_sq <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else if (input$permethod == "Autocorrelation") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              FUN = ac_periodogram
            )
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            # mf_ac <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else if (input$permethod == "Lomb-Scargle") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              FUN = ls_periodogram
            )
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            # mf_ls <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              FUN = cwt_periodogram
            )
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            # mf_cwt <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          }
        })
        ###################################################################################
        observeEvent(input$do4, {
          withBusyIndicatorServer("do4", {
            req(input$meta)
            per_xsq_dt_new <- per_xsq_dt_chi_sq[peak == 1, id]
            dt_curated <<- dt_curated[id %in% per_xsq_dt_new]
            per_xsq_dt_chi_sq <<-
              per_xsq_dt_chi_sq[id %in% per_xsq_dt_new]
          })
        })

        output$chisqperiodplotallwithpeaks <- renderPlot(
          {
            req(input$meta)
            req(input$permethod)
            ggperio(per_xsq_dt_chi_sq) +
              geom_line(aes(group = id, colour = genotype), size = 1) +
              geom_peak(peak_rank = 1:2, col = "blue") +
              geom_line(aes(y = signif_threshold), colour = "red") +
              facet_wrap(~ genotype + uid + replicate, ncol = 4, scales = "free_y") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme
          },
          res = 100,
          width = 1400,
          height = (toscale * 50)
        )

        ###################################

        pro_chi_sq <- per_xsq_dt_chi_sq
        pro_chi_sq$period <- pro_chi_sq$period / 3600
        pro_chi_sq <- subset(pro_chi_sq, peak > 0)
        pro_chi_sq <- as.matrix(pro_chi_sq)
        output$periodpower <- DT::renderDataTable(
          pro_chi_sq,
          filter = list(position = "top", clear = FALSE, plain = TRUE)
        )
        ###########################
        output$chisqperiodplotaverage <- renderPlot(
          {
            req(input$meta)
            ggperio(
              per_xsq_dt_chi_sq,
              aes(y = power - signif_threshold, colour = genotype)
            ) +
              stat_pop_etho() +
              facet_wrap(~replicate, ncol = 1, scales = "free_y") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme
          },
          res = 100,
          width = 1000,
          height = (input$replicate * 500)
        )


        observeEvent(input$show, {
          withBusyIndicatorServer("show", {
            l1 <- as.matrix(as.character(unique(per_xsq_dt_chi_sq$id)))
            output$chisqperiodplotallwithpeaks1 <- renderPlot({
              req(input$meta)
              ggperio(per_xsq_dt_chi_sq[id == l1[input$ind]]) +
                geom_line(aes(group = id, colour = genotype), size = 1) +
                geom_peak(peak_rank = 1:2, col = "blue") +
                geom_line(aes(y = signif_threshold), colour = "red") +
                facet_wrap(~ genotype + uid, scales = "free_y") +
                scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                My_Theme
            })
          })
        })

        output$chisqperiodplotbox <- renderPlot(
          {
            req(input$meta)
            summary_dt <- rejoin(per_xsq_dt_chi_sq[peak == 1])
            ggplot(summary_dt, aes(genotype, period, fill = genotype)) +
              geom_boxplot(outlier.colour = NA) +
              geom_jitter(aes(size = power - signif_threshold), alpha = .5) +
              facet_wrap(~replicate, ncol = 1) +
              scale_y_hours(name = "Period") +
              scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
              My_Theme
          },
          res = 100,
          width = 1000,
          height = (input$replicate * 500)
        )
        ###################################################################################

        #############
        output$downloadData_chi_sq <- downloadHandler(
          filename = function() {
            paste("periodpower.csv", sep = "")
          },
          content = function(file) {
            write.csv(pro_chi_sq, file, row.names = FALSE)
          }
        )
        ##################
      })
    })
    # })
    ################################### For CWT Spectogram####################################
    # observe({
    observeEvent(input$do1, {
      output$contents_CWT <- renderTable({
        req(input$raw)
        raw <- read.delim(input$raw$datapath,
          header = input$header,
          sep = input$sep
        )

        if (input$disp == "head") {
          return(head(raw))
        } else {
          return(raw)
        }
      })

      output$plot_CWT <- renderPlot(
        {
          req(input$raw)
          raw <- read.delim(input$raw$datapath,
            header = input$header,
            sep = input$sep
          )
          proc <- as.data.frame(raw[, -c(1:10)])
          nsim <- 10

          ind <- input$chn

          proc1 <-
            as.data.frame((proc[, ind])) ## change your individual number
          x <- as.data.frame(proc1)
          colnames(x) <- NULL
          rownames(x) <- NULL
          my.data <- data.frame(x = x)

          wt <-
            analyze.wavelet(
              my.data,
              "x",
              loess.span = 0,
              ## change loess.span if your data has trend and needs detrending
              dj = 1 / 500,
              dt = input$bin / 60,
              lowerPeriod = input$lp,
              upperPeriod = input$up,
              make.pval = T,
              method = "shuffle",
              n.sim = nsim,
              verbose = T
            )
          wt.image(
            wt,
            color.key = "quantile",
            n.levels = 250,
            ##### change color.key to "interval" depending on what you want to plot
            legend.params = list(lab = "wavelet power levels", mar = 4.7),
            siglvl = 0.05,
            label.time.axis = T,
            spec.time.axis = list(
              at = seq(1, length(as.matrix(x)), by = (length(
                as.matrix(x)
              ) / input$ndays)),
              labels = seq(0, (input$ndays -
                1), by = 1)
            ),
            timelab = paste("time elapsed (days)", "for individual number", ind),
            verbose = T,
            periodlab = "Period (in hours)",
            label.period.axis = T,
            col.ridge = "black",
            col.contour = "red",
            spec.period.axis = list(at = seq(input$lp, input$up, by = 2)),
            main = paste("CWT spectogram of Individual #", ind)
          )
          # dev.off()
        },
        res = 100
      )
    })
  })
})
