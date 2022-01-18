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
    title = "Visualization and ANalysis of timE SerieS dAta - Drosophila Activity Monitors (VANESSA-DAM) for circadian rhythm analysis!",
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
    size = "m",
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
  session$onSessionEnded(stopApp)
  observe_helpers(withMathJax = TRUE)
  observe({
    req(
      # input$modtau, input$ldperiod, input$min, input$light, input$ind_act_met
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

    toscale1 <- input$genotype

    toscale2 <- input$replicate * input$genotype

    
    output$downloadmetadata <- downloadHandler(
      filename = function() {
        paste("Metadata_user.csv", sep = "")
      },
      content = function(file) {
        write.csv(userdataforprint, file, append = F, quote = F, sep = ",", col.names = T, row.names = F)
      }
    )


    req(input$meta)
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
        req(input$meta)
        dt <- load_dam(metadata_proc)
        dt[, moving := activity > 0]
        dt_curated <- curate_dead_animals(dt)
        dt_curated[, day := ceiling(t / days(1))]
        dt_curated$day[dt_curated$day == 0] <- 1
        setbehavr(dt_curated, metadata_proc)
        lifespan_dt <- dt_curated[, .(lifespan = max(t)), by = id]
        valid_ids <- lifespan_dt[lifespan > days(input$remove), id]
        dt_curated <- dt_curated[id %in% valid_ids]
        dt_curated <- dt_curated[day %between% c(input$start, input$end)]
        setkey(dt_curated, day, id)
        dt_curated[, normact := (activity / sum(activity)) * 100, by = .(id, day)]
        setkey(dt_curated, id)
        setbehavr(dt_curated, metadata_proc)
        dt_curated[, uid := 1:.N, meta = T]
        dt_curated[, .(id, uid), meta = T]
        beepr::beep(sound = 10)
      })

      observeEvent(input$plotalletho, {
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
          res = 100,
          width = input$alletho_width,
          height = input$alletho_height
        )
      })

      observeEvent(input$plotallacto, {
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
          res = 100,
          width = input$allacto_width,
          height = input$allacto_height
        )
      })

      observeEvent(input$plotcuratedetho, {
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
          res = 100,
          width = input$curatedetho_width,
          height = input$curatedetho_height
        )
      })

      observeEvent(input$plotcuratedacto, {
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
          res = 100,
          width = input$curatedacto_width,
          height = input$curatedacto_height
        )
      })

      toscale <-
        nrow(as.matrix(as.character(unique(
          dt_curated$id
        ))))

      observeEvent(input$plotcuratedactoraw, {
        output$curatedactoraw <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(z = activity),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it normact
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
          width = input$curatedactoraw_width,
          height = (toscale * input$curatedactoraw_height)
        )
      })



      observeEvent(input$plotcuratedactoavg, {
        output$curatedactoavg <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(z = normact),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it normact
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
          width = input$curatedactoavg_width,
          height = (toscale * input$curatedactoavg_height)
        )
      })

      observeEvent(input$plotrawpro, {
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
          width = input$rawpro_width,
          height = (toscale * input$rawpro_height)
        )
      })

      observeEvent(input$plotavgpro, {
        output$avgpro <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(
                x = t,
                y = normact,
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
          width = input$avgpro_width,
          height = (toscale * input$avgpro_height)
        )
      })

      observeEvent(input$plotavgdaywisepro1, {
        output$avgdaywisepro1 <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(
                x = t,
                y = normact,
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
          width = input$avgdaywisepro1_width,
          height = (input$replicate * input$avgdaywisepro1_height)
        )
      })

      observeEvent(input$plotavgdaywisepro1_raw, {
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
          width = input$avgdaywisepro1_raw_width,
          height = (input$replicate * input$avgdaywisepro1_raw_height)
        )
      })

      observeEvent(input$plotrawpro1, {
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
          width = input$rawpro1_width,
          height = (input$replicate * input$rawpro1_height)
        )
      })

      observeEvent(input$plotavgpro1, {
        output$avgpro1 <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(
                x = t,
                y = normact,
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
          width = input$avgpro1_width,
          height = (input$replicate * input$avgpro1_height)
        )
      })

      observeEvent(input$plotrawpro1all, {
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
          width = input$rawpro1all_width,
          height = input$rawpro1all_height
        )
      })

      observeEvent(input$plotavgpro1all, {
        output$avgpro1all <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(
                x = t,
                y = normact,
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
          width = input$avgpro1all_width,
          height = input$avgpro1all_height
        )
      })


      observeEvent(input$plotavgprocircular, {
        output$avgprocircular <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(
                x = t,
                y = normact,
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
          width = input$avgprocircular_width,
          height = (input$replicate * input$avgprocircular_height)
        )
      })

      observeEvent(input$plotavgprocircular_raw, {
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
          width = input$avgprocircular_raw_width,
          height = (input$replicate * input$avgprocircular_raw_height)
        )
      })

      observeEvent(input$plotindiv_raw, {
        output$indiv_raw <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(x = t, z = activity),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it normact
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
          width = input$indiv_raw_width,
          height = (toscale2 * input$indiv_raw_height)
        )
      })

      observeEvent(input$plotindiv_avg, {
        output$indiv_avg <- renderPlot(
          {
            req(input$meta)
            ggetho(
              dt_curated,
              aes(x = t, z = normact),
              multiplot = 2,
              multiplot_period = hours(input$modtau),
              summary_time_window = mins(input$min)
            ) + ### can change to activity if raw needed or keep it normact
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
          width = input$indiv_avg_width,
          height = (toscale2 * input$indiv_avg_height)
        )
      })

      observeEvent(input$show | input$indiv1_height | input$indiv1_width, {
        l <- as.matrix(as.character(unique(dt_curated$id)))
        if (input$ind_act_met == 1) {
          output$indiv1 <- renderPlot(
            {
              req(input$meta)
              ggetho(
                dt_curated[id == l[input$ind]],
                aes(x = t, z = activity),
                multiplot = 2,
                multiplot_period = hours(input$modtau),
                summary_time_window = mins(input$min)
              ) + ### can change to activity if raw needed or keep it normact
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
            },
            res = 100,
            width = input$indiv1_width,
            height = input$indiv1_height
          )
        } else {
          output$indiv1 <- renderPlot(
            {
              req(input$meta)
              ggetho(
                dt_curated[id == l[input$ind]],
                aes(x = t, z = normact),
                multiplot = 2,
                multiplot_period = hours(input$modtau),
                summary_time_window = mins(input$min)
              ) + ### can change to activity if raw needed or keep it normact
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
            },
            res = 100,
            width = input$indiv1_width,
            height = input$indiv1_height
          )
        }
      })

      # observeEvent(input$show3, {
      #   l <- as.matrix(as.character(unique(dt_curated$id)))
      #   output$indiv1_avg <- renderPlot({
      #     req(input$meta)
      #     ggetho(
      #       dt_curated[id == l[input$ind]],
      #       aes(x = t, z = normact),
      #       multiplot = 2,
      #       multiplot_period = hours(input$modtau),
      #       summary_time_window = mins(input$min)
      #     ) + ### can change to activity if raw needed or keep it normact
      #       stat_ld_annotations(
      #         height = 1,
      #         alpha = 0.05,
      #         outline = NA,
      #         l_duration = hours(input$light),
      #         period = hours(input$ldperiod)
      #       ) +
      #       stat_bar_tile_etho() +
      #       facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
      #       My_Theme
      #   })
      # })
      #
      # observeEvent(input$show1, {
      #   l <- as.matrix(as.character(unique(dt_curated$id)))
      #   output$indiv1_norm <- renderPlot({
      #     req(input$meta)
      #     ggetho(
      #       dt_curated[id == l[input$ind]],
      #       aes(x = t, z = normact),
      #       multiplot = 2,
      #       multiplot_period = hours(input$modtau),
      #       summary_time_window = mins(input$min)
      #     ) + ### can change to activity if raw needed or keep it normact
      #       stat_ld_annotations(
      #         height = 1,
      #         alpha = 0.05,
      #         outline = NA,
      #         l_duration = hours(input$light),
      #         period = hours(input$ldperiod)
      #       ) +
      #       stat_bar_tile_etho() +
      #       facet_wrap(~ genotype + uid, ncol = 1, scales = "free_y") +
      #       My_Theme
      #   })
      # })




      ######################### for chi-square calculations##############################
      observeEvent(input$do3, {
        withBusyIndicatorServer("do3", {
          if (input$permethod == "Chi-square") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              alpha = input$alphasig,
              FUN = chi_sq_periodogram
            )
            per_xsq_dt_chi_sq <- rejoin(per_xsq_dt_chi_sq)
            per_xsq_dt_chi_sq <- per_xsq_dt_chi_sq[, c("file_info", "region_id", "experiment_id", "start_datetime", "stop_datetime") := NULL]
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            setbehavr(per_xsq_dt_chi_sq, metadata_proc)
            # mf_chi_sq <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else if (input$permethod == "Autocorrelation") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              alpha = input$alphasig,
              FUN = ac_periodogram
            )
            per_xsq_dt_chi_sq <- rejoin(per_xsq_dt_chi_sq)
            per_xsq_dt_chi_sq <- per_xsq_dt_chi_sq[, c("file_info", "region_id", "experiment_id", "start_datetime", "stop_datetime") := NULL]
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            setbehavr(per_xsq_dt_chi_sq, metadata_proc)
            # mf_ac <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else if (input$permethod == "Lomb-Scargle") {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              alpha = input$alphasig,
              FUN = ls_periodogram
            )
            per_xsq_dt_chi_sq <- rejoin(per_xsq_dt_chi_sq)
            per_xsq_dt_chi_sq <- per_xsq_dt_chi_sq[, c("file_info", "region_id", "experiment_id", "start_datetime", "stop_datetime") := NULL]
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            setbehavr(per_xsq_dt_chi_sq, metadata_proc)
            # mf_ls <- memoise(per_xsq_dt_chi_sq)
            beepr::beep(sound = 10)
          } else {
            per_xsq_dt_chi_sq <- periodogram(activity,
              dt_curated,
              period_range = c(hours(input$ll), hours(input$ul)),
              alpha = input$alphasig,
              FUN = cwt_periodogram
            )
            per_xsq_dt_chi_sq <- rejoin(per_xsq_dt_chi_sq)
            per_xsq_dt_chi_sq <- per_xsq_dt_chi_sq[, c("file_info", "region_id", "experiment_id", "start_datetime", "stop_datetime") := NULL]
            per_xsq_dt_chi_sq <- find_peaks(per_xsq_dt_chi_sq)
            setbehavr(per_xsq_dt_chi_sq, metadata_proc)
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

        observeEvent(input$plotchisqperiodplotallwithpeaks, {
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
            width = input$chisqperiodplotallwithpeaks_width,
            height = (toscale * input$chisqperiodplotallwithpeaks_height)
          )
        })

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
        observeEvent(input$plotchisqperiodplotaverage, {
          output$chisqperiodplotaverage <- renderPlot(
            {
              req(input$meta)
              ggperio(
                per_xsq_dt_chi_sq,
                aes(x = period, y = power - signif_threshold, colour = genotype)
              ) +
                stat_pop_etho() +
                facet_wrap(~replicate, ncol = 1, scales = "free_y") +
                scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                My_Theme
            },
            res = 100,
            width = input$chisqperiodplotaverage_width,
            height = (input$replicate * input$chisqperiodplotaverage_height)
          )
        })


        observeEvent(input$show | input$chisqperiodplotallwithpeaks1_height | input$chisqperiodplotallwithpeaks1_width, {
          withBusyIndicatorServer("show", {
            l1 <- as.matrix(as.character(unique(per_xsq_dt_chi_sq$id)))
            output$chisqperiodplotallwithpeaks1 <- renderPlot(
              {
                req(input$meta)
                ggperio(per_xsq_dt_chi_sq[id == l1[input$ind]]) +
                  geom_line(aes(group = id, colour = genotype), size = 1) +
                  geom_peak(peak_rank = 1:2, col = "blue") +
                  geom_line(aes(y = signif_threshold), colour = "red") +
                  facet_wrap(~ genotype + uid, scales = "free_y") +
                  scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                  scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                  My_Theme
              },
              res = 100,
              width = input$chisqperiodplotallwithpeaks1_width,
              height = input$chisqperiodplotallwithpeaks1_height
            )
          })
        })

        observeEvent(input$plotchisqperiodplotviolin, {
          output$chisqperiodplotviolin <- renderPlot(
            {
              req(input$meta)
              summary_dt <- rejoin(per_xsq_dt_chi_sq[peak == 1])
              ggplot(summary_dt, aes(genotype, period, fill = genotype)) +
                # geom_boxplot(outlier.colour = NA) +    ##########if box plots needed
                # geom_point(aes(size = power - signif_threshold, color = genotype, fill = genotype), alpha = .5, position = position_jitter(0.15)) +
                scale_size_continuous(range = c(1, 3)) +
                geom_violin(aes(color = genotype, fill = genotype), trim = TRUE, alpha = 0.5) +
                geom_sina(aes(color = genotype, size = power - signif_threshold, ), alpha = .6) +
                stat_summary(fun = mean, geom = "point", aes(color = genotype), size = 3, shape = 23) +
                {
                  if (input$chisqperiodplotviolin_text == TRUE) {
                    stat_summary(aes(label = round((..y.. / 3600), 2), color = genotype), fun = mean, geom = "text", size = 5, vjust = -0.5)
                  }
                } +
                facet_wrap(~replicate, ncol = 1) +
                scale_y_hours(name = "Period") +
                scale_color_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                scale_fill_manual(values = c(input$col1, input$col2, input$col3, input$col4, input$col5, input$col6, input$col7, input$col8, input$col9, input$col10, input$col11, input$col12)) +
                My_Theme
            },
            res = 100,
            width = input$chisqperiodplotviolin_width,
            height = (input$replicate * input$chisqperiodplotviolin_height)
          )
        })

        observeEvent(input$plotperioddistrib, {
          output$perioddistrib <- renderPlot(
            {
              req(input$meta)
              summary_dt <- rejoin(per_xsq_dt_chi_sq[peak == 1])
              ggplot(summary_dt, aes((period / 3600), genotype, color = genotype, fill = genotype)) +
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
                My_Theme
            },
            res = 100,
            width = input$perioddistrib_width,
            height = (input$replicate * input$perioddistrib_height)
          )
        })
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
  })
  ################################### For CWT Spectogram####################################
  # observe({
  observeEvent(input$do1, {
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

    output$plot_CWT <- renderPlot(
      {
        req(input$raw)
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
    output$plot_CWT_waveletpower <- renderPlot(
      {
        req(input$raw)
        maximum.level <- 1.001 * max(wt$Power.avg)
        wt.avg(wt,
          maximum.level = maximum.level, spec.period.axis = list(at = seq(input$lp, input$up, by = 2)),
          periodlab = "Period (in hours)"
        )
        # dev.off()
      },
      res = 100
    )
  })
  # })
  ############################# For timeseries smoothing#########################
  observeEvent(input$do_smooth, {
    req(input$raw_smooth)
    df <- read.delim(input$raw_smooth$datapath,
      header = input$header,
      sep = input$sep
    )
    start_date_time <- lubridate::ymd_hms(input$startdatetime_smooth)
    end_date_time <- lubridate::ymd_hms(input$enddatetime_smooth)
    result_date_time_start <- lubridate::ymd_hms(input$resultdatetime_smooth)
    result_date_time_end <- result_date_time_start + lubridate::days(1)
    # result_date_time_start_formatted <- format(round(result_date_time_start, units = "day"), '%Y-%m-%d %H:%M:%S')
    # result_date_time_end_formatted <- format(round(result_date_time_end, units = "day"), '%Y-%m-%d %H:%M:%S')
    bin <- input$bin_smooth ################# bin of actual data
    bin1 <- input$bin_req_smooth #################### bin of wanted data aggregation step
    n <- input$n_smooth ################ filter order or generic filter model
    W <- input$W_smooth ################## critical frequencies of the filter. W must be a scalar for low-pass and high-pass filters, and W must be a two-element vector c(low, high) specifying the lower and upper bands. For digital filters, W must be between 0 and 1 where 1 is the Nyquist frequency
    bf <- signal::butter(n = n, W = W, type = "low", plane = "z") # order 2, 10 Hz low-pass filter
    # ag <- data.frame(timestamp = seq(lubridate::ymd_hms(result_date_time_start_formatted), lubridate::ymd_hms(result_date_time_end_formatted), by = (bin1 * 60))) ########## change start time as your data
    ag <- data.frame(timestamp = seq(as.POSIXct(result_date_time_start), as.POSIXct(result_date_time_end), by = (bin1 * 60))) ########## change start time as your data
    all_nan <- function(x) any(!is.nan(x)) ############ define function to remove NaNs
    b <- input$b_smooth ################ kernel smoothing bandwith
    df$V2 <- lubridate::dmy(df$V2)
    df$V3 <- hms::as_hms(df$V3)
    lol <- paste(df$V2, df$V3, sep = " ")
    df_lol <- cbind(lol, df)
    df_lol$lol <- lubridate::ymd_hms(df_lol$lol)
    df_lol_new <- df_lol %>%
      dplyr::filter(between(df_lol$lol, start_date_time, end_date_time))
    ok1 <- as.numeric(rownames(df_lol_new))
    df_lol_new <- cbind(ok1, df_lol_new)
    df_lol_new1 <- df_lol_new[-nrow(df_lol_new), ]
    lol_new <- data.frame(timestamp = seq(as.POSIXct(start_date_time), as.POSIXct(end_date_time), by = (bin1 * 60))) ########## change start time as your data
    lol_new <- lol_new[-nrow(lol_new), ]
    df_lol_new <- subset(df_lol_new, select = -c(lol, V2, V3, V8))
    ok <- aggregate(df_lol_new, # the data frame
      by = list(cut(df_lol_new$ok1, seq(1, nrow(df_lol_new), (bin1 / bin)))), # the bins in minutes (change to any number)
      sum
    )
    df_lol_new <- ok
    df <- df_lol_new
    modulo_tau <- 24 ########### change modulo tau accordingly
    s_per_day <- (60 / bin1) * modulo_tau
    raw <- df[1:((floor(length(df[, 1]) / s_per_day)) * s_per_day), -c(1:9)]
    # raw <- raw[,-c(1,2,3:14)] ##############uncomment and type the channels you want to remove, ONLY if you want to remove channels
    raw_new <- cbind(lol_new, raw)
    if (input$smooth_method == "low pass butterworth filter") {
      for_profile_bf <- matrix(0, nrow = s_per_day, ncol = length(raw[1, ]))
      raw_smooth_new_bf <- matrix(0, nrow = nrow(raw_new), ncol = (length(raw_new[1, ]) - 1))
      for (i in 1:(length(raw_new[1, ])) - 1) {
        raw_smooth <- signal::filtfilt(bf, raw_new[, 1 + i])
        raw_smooth_new_bf[, i] <- raw_smooth
      }
      raw_new_smooth_bf <- cbind(raw_new, raw_smooth_new_bf)
      raw_new_smooth_bf <- raw_new_smooth_bf[, -c(2:33)]
      colnames(raw_new_smooth_bf) <- c("local time (arbitrary date)")
      raw_new_smooth1_bf <- raw_new_smooth_bf[, -1]
      for (i in 1:length(raw_new_smooth1_bf[1, ])) {
        a <- pracma::Reshape(raw_new_smooth1_bf[, i], s_per_day)
        mean_a <- as.matrix(rowMeans(a))
        for_profile_bf[, i] <- mean_a
      }
      pro_bf <<- for_profile_bf
      pro_bf <<- rbind(pro_bf, pro_bf[1, ])
      pro_bf <<- cbind(ag, pro_bf)
      pro_bf <<- pro_bf %>% select_if(all_nan)
      pro_sel_bf <- pro_bf[, colSums(pro_bf == 0) / nrow(pro_bf) < .9, drop = FALSE]

      pro_sel_rowmean_bf <- rowMeans(pro_sel_bf[, -1])
      pro_sel_rowmean_bf <- cbind(pro_sel_bf, pro_sel_rowmean_bf)
      pro_new_bf <- pro_sel_rowmean_bf[, c(1, ncol(pro_sel_rowmean_bf))]
      # plot(pro_new, type = "l", col = alpha("red", 0.5), lwd = 2)
      plot_mean <- ggplot(pro_new_bf, aes(x = pro_new_bf$timestamp, y = pro_new_bf$pro_sel_rowmean_bf)) +
        geom_line(color = "red", alpha = 0.8, size = 1) +
        scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "4 hour", expand = c(0, 0)) +
        xlab("Time") +
        ylab(paste0("Activity counts/", bin1, "minute", " (butterworth filter, low pass)")) +
        My_Theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      pro_sel_long_bf <- pro_sel_bf %>%
        tidyr::pivot_longer(
          2:ncol(pro_sel_bf)
        )
      pro_sel_long_sorted_bf <- pro_sel_long_bf[order(pro_sel_long_bf$name), ]

      plot_all <- ggplot(pro_sel_long_sorted_bf, aes(x = pro_sel_long_sorted_bf$timestamp, y = pro_sel_long_sorted_bf$value)) +
        geom_line(color = pro_sel_long_sorted_bf$name, alpha = 0.8, size = 1) +
        facet_wrap(~ pro_sel_long_sorted_bf$name, scales = "free_y") +
        scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "4 hour", expand = c(0, 0)) +
        xlab("Time") +
        ylab(paste0("Activity counts/", bin1, "minute", " (butterworth filter, low pass)")) +
        scale_color_gradientn(colours = rainbow(32)) +
        My_Theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(strip.background = element_rect(
          colour = "black",
          fill = NA
        ))
      output$smoothened_bf <- downloadHandler(
        filename = function() {
          paste("smoothened_bf.csv", sep = "")
        },
        content = function(file) {
          write.csv(raw_new_smooth_bf, file, row.names = FALSE)
        }
      )
    } else {
      for_profile_ks <- matrix(0, nrow = s_per_day, ncol = length(raw[1, ]))
      raw_smooth_new_ks <- matrix(0, nrow = nrow(raw_new), ncol = (length(raw_new[1, ]) - 1))
      for (i in 1:(length(raw_new[1, ])) - 1) {
        raw_smooth <- ksmooth(time(raw_new$lol_new), raw_new[, 1 + i], "normal", bandwidth = b)
        raw_smooth_new_ks[, i] <- raw_smooth$y
      }
      raw_new_smooth_ks <- cbind(raw_new, raw_smooth_new_ks)
      raw_new_smooth_ks <- raw_new_smooth_ks[, -c(2:33)]
      colnames(raw_new_smooth_ks) <- c("local time (arbitrary date)")
      raw_new_smooth1_ks <- raw_new_smooth_ks[, -1]
      for (i in 1:length(raw_new_smooth1_ks[1, ])) {
        a <- pracma::Reshape(raw_new_smooth1_ks[, i], s_per_day)
        mean_a <- as.matrix(rowMeans(a))
        for_profile_ks[, i] <- mean_a
      }
      pro_ks <<- for_profile_ks
      pro_ks <<- rbind(pro_ks, pro_ks[1, ])
      pro_ks <<- cbind(ag, pro_ks)
      pro_ks <<- pro_ks %>% select_if(all_nan)
      pro_sel_ks <- pro_ks[, colSums(pro_ks == 0) / nrow(pro_ks) < .9, drop = FALSE]

      pro_sel_rowmean_ks <- rowMeans(pro_sel_ks[, -1])
      pro_sel_rowmean_ks <- cbind(pro_sel_ks, pro_sel_rowmean_ks)
      pro_new_ks <- pro_sel_rowmean_ks[, c(1, ncol(pro_sel_rowmean_ks))]
      # plot(pro_new, type = "l", col = alpha("red", 0.5), lwd = 2)
      plot_mean <- ggplot(pro_new_ks, aes(x = pro_new_ks$timestamp, y = pro_new_ks$pro_sel_rowmean_ks)) +
        geom_line(color = "red", alpha = 0.8, size = 1) +
        scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "4 hour", expand = c(0, 0)) +
        xlab("Time") +
        ylab(paste0("Activity counts/", bin1, "minute", " (KS bandwidth = ", b, ")")) +
        My_Theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      pro_sel_long_ks <- pro_sel_ks %>%
        tidyr::pivot_longer(
          2:ncol(pro_sel_ks)
        )
      pro_sel_long_sorted_ks <- pro_sel_long_ks[order(pro_sel_long_ks$name), ]

      plot_all <- ggplot(pro_sel_long_sorted_ks, aes(x = pro_sel_long_sorted_ks$timestamp, y = pro_sel_long_sorted_ks$value)) +
        geom_line(color = pro_sel_long_sorted_ks$name, alpha = 0.8, size = 1) +
        facet_wrap(~ pro_sel_long_sorted_ks$name, scales = "free_y") +
        scale_x_datetime(date_labels = "%I:%M %p", date_breaks = "4 hour", expand = c(0, 0)) +
        xlab("Time") +
        ylab(paste0("Activity counts/", bin1, "minute", " (KS bandwidth = ", b, ")")) +
        scale_color_gradientn(colours = rainbow(32)) +
        My_Theme +
        theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
        theme(strip.background = element_rect(
          colour = "black",
          fill = NA
        ))
      output$smoothened_ks <- downloadHandler(
        filename = function() {
          paste("smoothtened_ks.csv", sep = "")
        },
        content = function(file) {
          write.csv(raw_new_smooth_ks, file, row.names = FALSE)
        }
      )
    }

    output$plot_smooth_average <- renderPlot(
      {
        req(input$raw_smooth)
        plot_mean
      },
      res = 100
    )
    output$plot_smooth_ind <- renderPlot(
      {
        req(input$raw_smooth)
        plot_all
      },
      res = 100
    )
  })
  # })
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
        replicate = input$replicate, remove = input$remove, start = input$start, end = input$end,
        permethod = input$permethod, ul = input$ul, ll = input$ll, alphasig = input$alphasig
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
  output$report_smooth <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = "report_smooth.html",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report_smoothing.Rmd")
      file.copy("report_smoothing.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(
        raw_smooth = input$raw_smooth, startdatetime_smooth = input$startdatetime_smooth,
        enddatetime_smooth = input$enddatetime_smooth, resultdatetime_smooth = input$resultdatetime_smooth,
        bin_smooth = input$bin_smooth, bin_req_smooth = input$bin_req_smooth, n_smooth = input$n_smooth,
        W_smooth = input$W_smooth, b_smooth = input$b_smooth
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
