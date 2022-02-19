# library(tidyverse)
# library(magrittr)
# library(ggthemes)
# library(epitools)
# library(viridis)
# library(viridisLite)
# library(kableExtra)
# library(DT)
# library(sf)
# library(tmap)
# library(tmaptools)
# library(spdep)
# library(INLA)
# library(cowplot)
# library(shiny)
# library(shinythemes)
# library(shinyscreenshot)
# library(shinycssloaders)
# library(bs4Dash)
# library(shinydashboardPlus)

options(shiny.maxRequestSize=20*1024^2)

server <- function(input, output, session) {

  #controlbar reactivity
  ## When users click the "analytics" menu, the control bar pops out
  observeEvent(input$sidebarMenu, {
    idx <- strsplit(input$sidebarMenu, "_")[[1]][2]
    if (idx %in% c(5,6,7)) {
      updateControlbar("controlbar")
    }
    })
  ## End

  #To include a template CSV for users
  RV <- reactiveValues(data = template_csv)

  values <- reactiveValues(
    upload_state = NULL
  )
  ## End

  #To trigger a weights matrix based on map they upload
  values_matrix <- reactiveValues(
    upload_state = NULL
  )
  ## End

  #Data user uploads
  data <-
    reactive({
      if(input$scotland_lip == "No"){
        req(input$file_case)

        ext <- tools::file_ext(input$file_case$name)

        switch(ext,
               csv = read.csv(input$file_case$datapath),
               validate("Invalid file; Please upload a .csv"))
      } else {
        read.csv(system.file('smallareamapp/extdata', 'scotlip_shiny_input.csv', package='smallareamapp')) %>%
          mutate(sir = round(as.numeric(sir),2),

                 lci = NA_real_,
                 uci = NA_real_)
      }
    })
  ## End

  #For users to upload a map
  map <-
    reactive({
      if(input$scotland_lip == "No"){
        # shpdf is a data.frame with the name, size, type and datapath
        # of the uploaded files
        req(input$file_map)
        shpdf <- input$file_map
        tempdirname <- dirname(shpdf$datapath[1])

        # Rename files
        for (i in 1:nrow(shpdf)) {
          file.rename(
            shpdf$datapath[i],
            paste0(tempdirname, "/", shpdf$name[i])
          )
        }
        map <- st_read(paste(tempdirname,
                             shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                             sep = "/"
        )) %>%
          st_make_valid()
        map
      } else {
        x <- system.file('smallareamapp/extdata/scotlip/map', 'scotlip_test.shp', package='smallareamapp')
        st_read(x) %>%
          st_make_valid()

    }
  })
  ## End

# Updating controlbar menu to match user data values

    #update sex filter
  observeEvent(data(), {
   updateSelectInput(session,
                     inputId = "sex_var",
                      choices = unique(data()$sex),
                      selected = unique(data()$sex)[1])
  })

  # update cancer type filer
  observeEvent(data(), {
    updateSelectInput(session, "cancer_var",
                      choices = unique(data()$cancer),
                      selected = unique(data()$cancer)[1])
  })

  ## End

# Update the data preview data table in the upload section

  #update preview table through the next few steps
  observeEvent(input$do, {
    values$upload_state <- "uploaded"

  })

  observeEvent(input$reset, {
    values$upload_state <- "reset"
  })

# The preview table on the file upload tab

  output$table_input <- DT::renderDataTable({
    if (is.null(values$upload_state)) {
      return(datatable(RV$data %>% mutate(
        sir = round(sir,2),
        lci = round(lci,2),
        uci = round(uci,2)) %>% head(20),
                       rownames = F,
                       options = list(
                         fixedColumns = TRUE,
                         autoWidth = T,
                         scrollX = TRUE,
                         ordering = TRUE),
                       class = "display"))
    } else if (values$upload_state == 'uploaded') {
      return(datatable(data() %>% head(20),
                       rownames = F,
                       options = list(
                         fixedColumns = TRUE,
                         autoWidth = T,
                         scrollX = TRUE,
                         ordering = TRUE),
                       class = "display"))
    } else if (values$upload_state == 'reset') {
      return(datatable(RV$data %>% mutate(
        sir = round(sir,2),
        lci = round(lci,2),
        uci = round(uci,2)) %>% head(20),
                       rownames = F,
                       options = list(
                         fixedColumns = TRUE,
                         autoWidth = T,
                         scrollX = TRUE,
                         ordering = TRUE),
                       class = "display"))
    }


  })
  ## End

# Data from CSV and Shapefiles need to link on area name, this bit allows users to specify what that variable is

  # update area name table choices
  observeEvent(data(), {
    updateSelectInput(session, "area_name_table",
                      choices = colnames(data()),
                      selected = colnames(data())[1])
  })

  # update area name map choices
  observeEvent(map(), {
    columns_map <- map() %>% st_drop_geometry() %>% colnames()
    updateSelectInput(session, "area_name_map",
                      choices = columns_map,
                      selected = columns_map[1])
  })
  ## End

# Preview map for data upload page
  output$preview_map <- renderTmap({

      map_df_sf <- map()

      tm_shape(map_df_sf) +
        tm_polygons(input$area_name_map,
                    id = input$area_name_map,
                    title = "Map preview",
                    border.col = "white",
                    lwd = 0.5,
                    palette = "-viridis",
                    legend.show = FALSE)
  })
  ## End

  #Create weights matrix
  ## Button to initiate
  observeEvent(input$weight_matrix, {
    values_matrix$upload_state <- "uploaded"

  })
  ## End

  ## Indicator of progress with weight matrix through textoutput

  output$wm_text <- renderText(
    if(is.null(values_matrix$upload_state)) {
      return("A weights matrix is required for spatial modelling.")} else {
        map_matrix <- map()

        set.ZeroPolicyOption(TRUE)
        get.ZeroPolicyOption()
      nb <- poly2nb(map_matrix)

      tmp_file <- paste0(tempfile(), ".adj")
      nb2INLA(file = tmp_file, nb)
      g <<- inla.read.graph(filename = tmp_file)
      return("You successfuly created a weights matrix, good job!")
    }
  )
  ## End


  #snapshots
  observeEvent(input$map1, {
    screenshot(
      scale = 1,
      filename = "plot_var_1",
      id="var_map"
    )
  })

  observeEvent(input$map2, {
    screenshot(
      scale = 1,
      filename = "plot_var_2",
      id="var_map2"
    )
  })

  ## End ##

  ### Data for Analytics ###

  #dataset function, load data and filter based on inputs
  datasetInput <- eventReactive(input$run, {

    mydatain <- data()
    mydatain %>%
      filter(cancer == input$cancer_var,
             sex == input$sex_var)
  })

  #Creating a reactive that analyzes data depending on whether data will be spatially modelled or not

#here is our inla result... use bindCache to run this once and improve app performance

  inla_rv <- eventReactive(input$run, {
      mytable <- datasetInput()
      if(input$spatial_choice == "No"){
        mytable
      }
      else{
        if(input$model_choice == "bym2"){
          chsadf_inla <-
            mytable %>%
            mutate(idareau = 1:(mytable %>% nrow),
                   idareav = 1:(mytable %>% nrow),
                   idarea = 1:(mytable %>% nrow))

          inla(formula,
               family = "poisson", data = chsadf_inla,
               E = exp, control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, cpo = T, config = T,
                                      return.marginals.predictor = T),
               control.inla = list(strategy = "laplace", npoints = 21)
          )
        } else{
          chsadf_inla <-
            mytable %>%
            mutate(idareau = 1:(mytable %>% nrow),
                   idareav = 1:(mytable %>% nrow),
                   idarea = 1:(mytable %>% nrow))

          inla(formula_bym,
               family = "poisson", data = chsadf_inla,
               E = exp, control.predictor = list(compute = TRUE),
               control.compute = list(dic = TRUE, cpo = T, config = T,
                                      return.marginals.predictor = T),
               control.inla = list(strategy = "laplace", npoints = 21)
          )
        }
      }

    })

test <-
  eventReactive(input$run, {
    mytable <- datasetInput()

    chsadf_inla <-
      mytable %>%
      mutate(idareau = 1:(mytable %>% nrow),
             idareav = 1:(mytable %>% nrow),
             idarea = 1:(mytable %>% nrow))

    #When spatial model is "No", users can still examine SIR and case values
    if(input$spatial_choice == "No"){
      # mytable %>%
      inla_rv() %>%
        mutate(
          # exp = round(exp,0),
          # sir = round(sir,2),
          # lci = round(lci,2),
          # uci = round(uci,2),
          cis = paste(round(lci,2),"-", round(uci,2))) %>%
        rename(SIR = sir)

    } else{
      #When Spatial model is "Yes", we can use inla to estimate risk
      #INLA
      if(input$model_choice == "bym2"){

        res <- inla_rv()

        exc <- sapply(res$marginals.fitted.values,
                      FUN = function(marg){1 - inla.pmarginal(q = input$threshold, marginal = marg)})

        chsadf_inla %>%
          mutate(
            # exp = round(exp,0),
            # sir = round(sir,2),
            # lci = round(lci,2),
            # uci = round(uci,2),
            cis = paste(round(lci,2),"-", round(uci,2)),
            RR = res$summary.fitted.values[, "mean"],
            LL = res$summary.fitted.values[, "0.025quant"],
            UL = res$summary.fitted.values[, "0.975quant"],
            cris = paste0(LL, "-", UL),
            change = round(abs((sir-RR)/sir*100),2)) %>%
          mutate(exc = exc) %>%
          rename(SIR = sir)

      } else{

        res <- inla_rv()

        exc <- sapply(res$marginals.fitted.values,
                      FUN = function(marg){1 - inla.pmarginal(q = input$threshold, marginal = marg)})

        chsadf_inla %>%
          mutate(
            # exp = round(exp,0),
            # sir = round(sir,2),
            # lci = round(lci,2),
            # uci = round(uci,2),
            cis = paste(round(lci,2),"-", round(uci,2)),
            RR = res$summary.fitted.values[, "mean"],
            LL = res$summary.fitted.values[, "0.025quant"],
            UL = res$summary.fitted.values[, "0.975quant"],
            cris = paste0(LL, "-", UL),
            change = round(abs((sir-RR)/sir*100),2)) %>%
          mutate(exc = exc) %>%
          rename(SIR = sir)

      }
    }

  })

  ## END

# Creating maps
map_test <- eventReactive(input$run,{

  if(input$spatial_choice == "No"){

    map_df <-
      test() %>%
      mutate(
        exp = round(exp,0),
        SIR = round(SIR,2),
        lci = round(lci,2),
        uci = round(uci,2)
        # cis = paste0(round(lci, 2), "-", round(uci,2)),
        )

    map_df_sf <-
      map() %>%
      left_join(., map_df, by = input$area_name_map)
  } else{

    map_df <-
      test() %>%
        mutate(
          exp = round(exp,0),
          SIR = round(SIR,2),
          cis = paste(round(lci,2),"-", round(uci,2)),
          RR = round(RR,2),
          LL = round(LL,2),
          UL = round(UL,2),
          cris = paste0(round(LL,2), "-", round(UL,2))) %>%
        mutate(exc = round(exc,2))

    map_df_sf <-
      map() %>%
      left_join(., map_df, by = input$area_name_map)

    }
})# %>%
  # bindCache(inla_rv(), map())
## End


## Calculating spatial structured effect
improved_res <- eventReactive(input$run, {
  # mytable <- datasetInput()
  #
  # chsadf_inla <-
  #   mytable %>%
  #   mutate(idareau = 1:(mytable %>% nrow),
  #          idareav = 1:(mytable %>% nrow),
  #          idarea = 1:(mytable %>% nrow))
  if(input$model_choice == "bym2"){

    res <- inla_rv()

  # res_improved <- inla.hyperpar(res)
    hyper <- inla.zmarginal(res$marginals.hyperpar[[2]])
    hyper$quant0.5}
  else{

    res <- inla_rv()

    unstructured_effect <-
      res$internal.marginals.hyperpar[[1]]
    unstructured_effect.var <- inla.tmarginal(function(x) 1/exp(x), unstructured_effect)
    x_v <- inla.zmarginal(unstructured_effect.var)[[1]]

    spatial_effect <-
      res$internal.marginals.hyperpar[[2]]
    spatial_effect.var <- inla.tmarginal(function(x) 1/exp(x), spatial_effect)
    x_u <- inla.zmarginal(spatial_effect.var)[[1]]

    x_u/(x_v+x_u)
  }
})

#95% HPD credible intervals
spatial_effect_hpd <- reactive({

  if(input$model_choice == "bym2"){

    res <- inla_rv()

    # res_improved <- inla.hyperpar(res)
    inla.hpdmarginal(0.95, res$marginals.hyperpar[[2]])}
  else{
    NULL
  }
}) %>%
  bindCache(datasetInput(), inla_rv(), input$model_choice)


## End

# Data table for Analytics tab
  output$table1 <- DT::renderDataTable(server = F, {
    dt <- test()
    if(input$spatial_choice == "No"){
      dt %>%
        select(input$area_name_map, cancer, sex, cases, exp, SIR,  cis, area_pop) %>%
        mutate(
          exp = round(exp,0),
          SIR = round(SIR,2)) %>%
        datatable(.,
                  rownames = F,
                  colnames = c(input$area_name_map, "cancer", "sex", "Observed", "Expected", "SIR", "95% CIs", "Population"),
                  extensions = 'Buttons',

                  options = list(
                    pageLength = 5,
                    fixedColumns = TRUE,
                    autoWidth = F,
                    ordering = TRUE,
                    dom = 'Bfrtip',
                    buttons = list(
                      list(extend = 'csv',   filename =  paste("sir", input$cancer_var, sep = "_")))
                  ),


                  class = "display"
        )} else {
          dt %>%
            select(input$area_name_map, "cases", "exp", "SIR", cis,  "RR", LL, UL, cris, exc, area_pop, change) %>%
            mutate(
              exp = round(exp,0),
              SIR = round(SIR,2),
              # cis = paste0(round(lci,2), "-", round(uci,2)),
              RR = round(RR,2)) %>%
            mutate(
              cris = paste0(round(LL,2), "-", round(UL,2)),
              exc = round(exc,2)
              # change = round(abs((sir-RR)/sir*100),2)
              ) %>%
            select(-LL, -UL) %>%
            datatable(.,
                      rownames = F,
                      colnames = c(input$area_name_map, "Observed", "Expected", "SIR",  "95% CIs",  "RR",  "95% CrIs", "Exc. Probability", "Population", "SIR Percent Change"),
                      extensions = 'Buttons',

                      options = list(
                        pageLength = 5,
                        fixedColumns = TRUE,
                        autoWidth = F,
                        ordering = TRUE,
                        dom = 'Bfrtip',
                        buttons = list(
                          list(extend = 'csv',   filename =  paste("sir", input$cancer_var, sep = "_")))
                      ),


                      class = "display")
                }

  })

  #Variable #1 map

  output$var_map <- renderTmap({

    map_df_sf <- map_test()

        a <- as.character(input$variable_var)
        b <- as.character(input$variable_var1)

if(input$spatial_choice == "No"){
if(input$map_style1 == "fixed"){

    if(input$variable_var == "SIR"){
      tm_shape(map_df_sf) +
        tm_polygons(col = a,
                    id = input$area_name_map,
                    title = Z,
                    border.col = "white",
                    lwd = 0.5,
                    style = input$map_style1,
                    breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                    palette = input$map_palette1,
                    popup.vars = c(
                      "Cases: " = "cases",
                      "Expected: " = "exp",
                      "SIR: " = "SIR",
                      "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )}

  # else if(input$variable_var == "RR"){
    else if(input$variable_var != "SIR"){
          tm_shape(map_df_sf) +
            tm_polygons(col = a,
                        id = input$area_name_map,
                        title = a,
                        border.col = "white",
                        lwd = 0.5,
                        style = input$map_style1,
                        breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                        palette = input$map_palette1,
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "SIR: " = "SIR",
                          "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1)
        }
}
  else{
  if(input$variable_var == "SIR"){
    tm_shape(map_df_sf) +
      tm_polygons(col = a,
                  id = input$area_name_map,
                  title = a,
                  border.col = "white",
                  lwd = 0.5,
                  style = input$map_style1,
                  n = input$bins1,
                  palette = input$map_palette1,
                  popup.vars = c(
                    "Cases: " = "cases",
                    "Expected: " = "exp",
                    "SIR: " = "SIR",
                    "95% CIs: " = "cis")) +
      tm_layout(
        frame = F,
        legend.title.size = 1.2,
        legend.text.size = 1
      )}
  # else if(input$variable_var == "RR"){
  else if(input$variable_var != "SIR"){
        tm_shape(map_df_sf) +
          tm_polygons(col = a,
                      id = input$area_name_map,
                      title = a,
                      border.col = "white",
                      lwd = 0.5,
                      style = input$map_style1,
                      n = input$bins1,
                      palette = input$map_palette1,
                      popup.vars = c(
                        "Cases: " = "cases",
                        "Expected: " = "exp",
                        "SIR: " = "SIR",
                        "95% CIs: " = "cis")) +
      tm_layout(
        frame = F,
        legend.title.size = 1.2,
        legend.text.size = 1)
      }
}
} else{
  if(input$border == "Yes"){
  if(input$map_style1 == "fixed"){
    if(input$variable_var1 == "SIR"){
      tm_shape(map_df_sf) +
        tm_polygons(col = b,
                    id = input$area_name_map,
                    title = b,
                    border.col = "white",
                    lwd = 0.5,
                    style = input$map_style1,
                    breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                    palette = input$map_palette1,
                    popup.vars = c(
                      "Cases: " = "cases",
                      "Expected: " = "exp",
                      "SIR: " = "SIR",
                      "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )  +
        tm_shape(map_df_sf %>% filter(exc >= input$pe)) +
        tm_borders(
          col = "black",
          lwd = 1.5
        )
      }
    else if(input$variable_var1 != "SIR"){
          tm_shape(map_df_sf) +
            tm_polygons(col = b,
                        id = input$area_name_map,
                        title = b,
                        border.col = "white",
                        lwd = 0.5,
                        style = input$map_style1,
                        breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                        palette = input$map_palette1,
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )  +
            tm_shape(map_df_sf %>% filter(exc >= input$pe)) +
            tm_borders(
              col = "black",
              lwd = 1.5
            )

        }
  }
    else{
    if(input$variable_var1 == "SIR"){
      tm_shape(map_df_sf) +
        tm_polygons(col = b,
                    id = input$area_name_map,
                    title = b,
                    border.col = "white",
                    lwd = 0.5,
                    style = input$map_style1,
                    n = input$bins1,
                    palette = input$map_palette1,
                    popup.vars = c(
                      "Cases: " = "cases",
                      "Expected: " = "exp",
                      "SIR: " = "SIR",
                      "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )  +
        tm_shape(map_df_sf %>% filter(exc >= input$pe)) +
        tm_borders(
          col = "black",
          lwd = 1.5
        )
      }
    else if(input$variable_var1 != "SIR"){
          tm_shape(map_df_sf) +
            tm_polygons(col = b,
                        id = input$area_name_map,
                        title = b,
                        border.col = "white",
                        lwd = 0.5,
                        style = input$map_style1,
                        n = input$bins1,
                        palette = input$map_palette1,
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc")) +
            tm_shape(map_df_sf %>% filter(exc >= input$pe)) +
            tm_borders(
              col = "black",
              lwd = 1.5
            )
        }
  }
  }
  else{
    if(input$map_style1 == "fixed"){
      if(input$variable_var1 == "SIR"){
        tm_shape(map_df_sf) +
          tm_polygons(col = b,
                      id = input$area_name_map,
                      title = b,
                      border.col = "white",
                      lwd = 0.5,
                      style = input$map_style1,
                      breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                      palette = input$map_palette1,
                      popup.vars = c(
                        "Cases: " = "cases",
                        "Expected: " = "exp",
                        "SIR: " = "SIR",
                        "95% CIs: " = "cis")) +
          tm_layout(
            frame = F,
            legend.title.size = 1.2,
            legend.text.size = 1
          )
      }
      else if(input$variable_var1 != "SIR"){
        tm_shape(map_df_sf) +
          tm_polygons(col = b,
                      id = input$area_name_map,
                      title = b,
                      border.col = "white",
                      lwd = 0.5,
                      style = input$map_style1,
                      breaks = c(seq(input$breaks_min1, input$breaks_max1, input$breaks_step1), Inf),
                      palette = input$map_palette1,
                      popup.vars = c(
                        "Cases: " = "cases",
                        "Expected: " = "exp",
                        "RR: " = "RR",
                        "95% CrIs: " = "cris",
                        "Exceedance Prob: " = "exc"))

      }
    } else{
      if(input$variable_var1 == "SIR"){
        tm_shape(map_df_sf) +
          tm_polygons(col = b,
                      id = input$area_name_map,
                      title = b,
                      border.col = "white",
                      lwd = 0.5,
                      style = input$map_style1,
                      n = input$bins1,
                      palette = input$map_palette1,
                      popup.vars = c(
                        "Cases: " = "cases",
                        "Expected: " = "exp",
                        "SIR: " = "SIR",
                        "95% CIs: " = "cis")) +
          tm_layout(
            frame = F,
            legend.title.size = 1.2,
            legend.text.size = 1
          )
      }
      else if(input$variable_var1 != "SIR"){
        tm_shape(map_df_sf) +
          tm_polygons(col = b,
                      id = input$area_name_map,
                      title = b,
                      border.col = "white",
                      lwd = 0.5,
                      style = input$map_style1,
                      n = input$bins1,
                      palette = input$map_palette1,
                      popup.vars = c(
                        "Cases: " = "cases",
                        "Expected: " = "exp",
                        "RR: " = "RR",
                        "95% CrIs: " = "cris",
                        "Exceedance Prob: " = "exc"))
      }
    }
  }
  }
  })

  #Calculating the spatial effect
  output$spatial_effect <- renderUI({

    #INLA
    if(input$spatial_choice == "Yes"){
    if(input$model_choice == "bym2"){
      text <- improved_res()
      text2 <- spatial_effect_hpd()

      return(HTML(paste0("The spatially structured effect was ", round(text,4), " with a 95% credible interval of (", round(text2[1], 4), "-",  round(text2[2], 4), ").", " Meaning that ", round(text*100,2), "% ", "(", round(text2[1]*100, 2), "% - ",  round(text2[2]*100, 2), "%) ", "of the variance in the modeled risk can be explained by a spatial effect.")))

    } else {

      text <- improved_res()
      return(HTML(paste0("The spatially structured effect was ", round(text,4), ". Meaning that ", round(text*100,2), "% of the variance in the modeled risk can be explained by a spatial effect.")))


    }} else{
      HTML(paste("To view this data table, please select", em("Yes"), "to the", em("Spatial Modelling"), "dropdown."))
    }
  })

  #calculating how many cases are in excess among elevated areas
  output$excess_table <- renderText({

    #INLA
    if(input$spatial_choice == "Yes"){
      chsadf_inla <-
      test() %>%
        mutate(prob = case_when(
          # exc >= 0.95 ~ "Equal or greater than 90%",
          exc >= 0.8 ~ "Equal or greater than 80%",
          T ~ ""
        ))

      tibble(
        Cohort = c("Total", "Regions with elevated risk"),
        `Area Units` = c(chsadf_inla %>% nrow, chsadf_inla %>% filter(exc >= 0.8) %>% nrow),
        Observed = c(round(sum(chsadf_inla$cases),0), chsadf_inla %>% filter(exc >= 0.8) %$% round(sum(cases),0)),
        Expected = c(round(sum(chsadf_inla$exp),0), chsadf_inla %>% filter(exc >= 0.8) %$% round(sum(exp),0))
      ) %>%
        mutate(Excess = Observed - Expected) %>%
        kable(., format = "html") %>%
        kable_styling()
    } else {
      HTML(paste("To view this data table, please select", em("Yes"), "to the", em("Spatial Modelling"), "dropdown."))
    }
  })

  #Model diagnostics: Observed vs. Fitted values
  output$pred_plot <- renderPlotly({
    if(input$spatial_choice == "No"){
    } else{
      mytable <- datasetInput()
      result <- inla_rv()

      post_pred <-
        tibble(obs = mytable$sir,
               pred = result$summary.fitted.values$mean,
               name = mytable[, input$area_name_map])

      linear_model_result <-
        lm(obs ~ pred, data = post_pred)

      rsquar <-
        summary(linear_model_result)$adj.r.squared

      #Plotly build
      post_pred %>%
        plot_ly(data = ., x = ~obs, y = ~pred, type = 'scatter', mode = 'markers',
                text = ~name,
                hovertemplate = paste0("<b>%{text}</b><br>",
                                    "<b>SIR</b>: %{x:.2f}<br>",
                                    "<b>RR</b>: %{y:.2f}<br>",
                                    "<extra></extra>"),
                marker = list(size = 6,
                              line = list(color = 'black',
                                          width = 1))
        ) %>%
        layout(title = 'Observed vs. Fitted values',
               yaxis = list(title = "<b>Fitted RR<b>"),
               xaxis = list(title = "<b>Observed SIR<b>"),
               margin = list(b=100),
               annotations = list(x = 1, y = -0.25, #position of text adjust as needed
                                  text = paste("Adujsted R-Squared =", round(rsquar,2)), showarrow = F,
                                  xref='paper', yref='paper',
                                  xanchor='right', yanchor='auto', xshift=0, yshift=0,
                                  font=list(size=15, color="black"))) %>%
        layout(
          shapes=list(type='line',
                      line = list(dash = "dash",
                                  opacity = 0.5),
                      x0=0,
                      x1=ceiling(max(post_pred %$% obs)),
                      y0=0,
                      y1=ceiling(max(post_pred %$% obs))
          )
        )
    }

  })

  #Model diagnostics: PIT plot
  output$pit_plot <- renderPlotly({
    if(input$spatial_choice == "No"){
    } else{

      mytable <- datasetInput()
      n <- nrow(mytable)
      uniquant <- (1:n)/(n+1)
    result <- inla_rv()
    names <- mytable[, input$area_name_map]
    pit <- result$cpo$pit

    test <-
      tibble(
        N = logit(uniquant),
        region_name = names,
        PIT = logit(sort(pit))
      )


    fig <- plot_ly(data = test, x = ~N, y = ~PIT,
                    # name = ~region_name,
                    text = ~region_name,
                    hovertemplate = paste0(
                      "<b>%{text}</b><br>",
                      "%{yaxis.title.text}: %{y:.2f}<br>",
                      "<extra></extra>"
                    ),
                   mode = 'markers',
                   type = 'scatter',
                   marker = list(size = 6,
                                 line = list(color = 'black',
                                             width = 1)))
    fig <- fig %>% layout(
                          yaxis = list(zeroline = FALSE, title = "PIT"),
                          xaxis = list(zeroline = FALSE, title = "Uniform quantiles"))


    a <- min(test$PIT)
    b  <- max(test$PIT)

    fig <- fig %>% layout(
      shapes=list(type='line',
                  line = list(colour = "black",
                              dash = "dash",
                              opacity = 0.5), x0=min(logit(uniquant)), x1=max(logit(uniquant)), y0=min(logit(uniquant)), y1=max(logit(uniquant)))
    )

    fig
    }

  })

  #spatial autocorrelation: Moran's Density Plot
  output$morans_plot <- renderPlot({
    if(input$spatial_choice == "No"){
    } else{
      mytable <- datasetInput()
      map_df_sf <-
        map() %>%
        left_join(., datasetInput(), by = input$area_name_map)

      nb <- poly2nb(map_df_sf)
      lw <- nb2listw(nb, style="W", zero.policy=TRUE)
      # Moran's I test MC simulations
      MC <-
        moran.mc(map_df_sf$sir, lw, nsim=599)

      df <-
        tibble(I = MC$res)

      MC_result <-
        if(MC$p.value < 0.05){
          "Spatial autocorrelation is present"} else{
            "Spatial autocorrelation is not present"}

      MC_result2 <-
        if(MC$p.value < 0.05 & MC$statistic > 0){
          " and SIRs are clustered."} else if(
            MC$p.value < 0.05 & MC$statistic < 0){
            " and SIRs are dispersed."}else{
              " and SIRs are distributed at random."}

      ggplot(df, aes(x = I)) +
        geom_density(fill = "grey") +
        geom_vline(xintercept = MC$statistic, col = "black", size = 2) +
        labs(
          title = "Simulated Moran's I",
          subtitle = paste0(MC_result, MC_result2),
          x = "Moran's I",
          y = "Density",
          caption = paste0(
            "Moran's I = ", round(MC$statistic,4),
            "\np-value = ", round(MC$p.value, 4)
        )) +
        theme_bw(base_size = 14) +
        theme(
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()
        )

    }

  })

  #Model summary save
  output$model_summary <- downloadHandler(
    filename <- function(){
      paste("smallareamapp_model_summary.RData")
    },

    content = function(file) {
      res <- inla_rv()
      save(res, file = file)
    }
  )

}
