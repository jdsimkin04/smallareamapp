# library(tidyverse)
# library(magrittr)
# library(ggthemes)
# library(readr)
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
    if (idx == 5) {
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
        ))
        map
      } else {
        x <- system.file('smallareamapp/extdata/scotlip/map', 'scotlip_test.shp', package='smallareamapp')
        st_read(x)

    }
  })
  ## End

# Updating controlbar menu to match user data values

    #update sex filter
  observeEvent(data(), {
   updateSelectInput(session, "sex_var",
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
      return(datatable(RV$data %>% head(20),
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
      return(datatable(RV$data %>% head(20),
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

  ## End

  ### Data for Analytics ###

  #dataset function, load data and filter based on inputs
  datasetInput <- reactive({

    mydatain <- data()
    mydatain %>%
      filter(cancer == input$cancer_var,
             sex == input$sex_var)

  })

  #Creating a reactive that analyzes data depending on whether data will be spatially modelled or not
test <- reactive({

    mytable <- datasetInput()
    #When spatial model is "No", users can still examine SIR and case values
    if(input$spatial_choice == "No"){
      mytable %>%
        mutate(
          exp = round(exp,0),
          sir = round(sir,2),
          lci = round(lci,2),
          uci = round(uci,2),
          cis = paste(round(lci,2),"-",round(uci,2))) %>%
        rename(SIR = sir)

    } else{
      #When Spatial model is "Yes", we can use inla to estimate risk
      #INLA
      if(input$model_choice == "bym2"){
        chsadf_inla <-
          mytable %>%
          mutate(idareau = 1:(mytable %>% nrow),
                 idareav = 1:(mytable %>% nrow),
                 idarea = 1:(mytable %>% nrow))

        res <- inla(formula,
                    family = "poisson", data = chsadf_inla,
                    E = exp, control.predictor = list(compute = TRUE),
                    control.compute = list(dic = TRUE)
        )

        exc <- sapply(res$marginals.fitted.values,
                      FUN = function(marg){1 - inla.pmarginal(q = input$threshold, marginal = marg)})

        chsadf_inla %>%
          mutate(
            exp = round(exp,0),
            sir = round(sir,2),
            lci = round(lci,2),
            uci = round(uci,2),
            cis = paste0(lci, "-", uci),
            RR = round(res$summary.fitted.values[, "mean"],2),
            LL = round(res$summary.fitted.values[, "0.025quant"],2),
            UL = round(res$summary.fitted.values[, "0.975quant"],2),
            cris = paste0(LL, "-", UL),
            change = round(abs((sir-RR)/sir*100),2)) %>%
          mutate(exc = round(exc,2)) %>%
          rename(SIR = sir)

      } else{
        chsadf_inla <-
          mytable %>%
          mutate(idareau = 1:(mytable %>% nrow),
                 idareav = 1:(mytable %>% nrow),
                 idarea = 1:(mytable %>% nrow))

        res <- inla(formula_bym,
                    family = "poisson", data = chsadf_inla,
                    E = exp, control.predictor = list(compute = TRUE),
                    control.compute = list(dic = TRUE)
        )

        exc <- sapply(res$marginals.fitted.values,
                      FUN = function(marg){1 - inla.pmarginal(q = input$threshold, marginal = marg)})

        chsadf_inla %>%
          mutate(
            exp = round(exp,0),
            sir = round(sir,2),
            lci = round(lci,2),
            uci = round(uci,2),
            cis = paste0(lci, "-", uci),
            RR = round(res$summary.fitted.values[, "mean"],2),
            LL = round(res$summary.fitted.values[, "0.025quant"],2),
            UL = round(res$summary.fitted.values[, "0.975quant"],2),
            cris = paste0(LL, "-", UL),
            change = round(abs((sir-RR)/sir*100),2)) %>%
          mutate(exc = round(exc,2)) %>%
          rename(SIR = sir)

      }
    }

  })
  ## END

# Creating maps
map_test <- reactive({


    map_df <-
      test()

    map_df_sf <-
      map() %>%
      left_join(., map_df, by = input$area_name_map)

})
## End


## Calculating spatial structured effect
improved_res <- reactive({
  mytable <- datasetInput()

  chsadf_inla <-
    mytable %>%
    mutate(idareau = 1:(mytable %>% nrow),
           idareav = 1:(mytable %>% nrow),
           idarea = 1:(mytable %>% nrow))
  if(input$model_choice == "bym2"){

  res <- inla(formula,
              family = "poisson", data = chsadf_inla,
              E = exp, control.predictor = list(compute = TRUE),
              control.compute = list(dic = TRUE)
  )

  res_improved <- inla.hyperpar(res, dz = 0.2, diff.logdens = 20)

  res_improved$summary.hyperpar$mean[2]} else{

    res <- inla(formula_bym,
                family = "poisson", data = chsadf_inla,
                E = exp, control.predictor = list(compute = TRUE),
                control.compute = list(dic = TRUE)
    )

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
## End

# Data table for Analytics tab
  output$table1 <- DT::renderDataTable({
    dt <- test()
    if(input$spatial_choice == "No"){
      dt %>%
        select(input$area_name_map, cancer, sex, cases, exp, SIR,  cis, area_pop) %>%
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
            select(input$area_name_map, "cases", "exp", "SIR", cis,  "RR", cris, exc, area_pop, change) %>%
            datatable(.,
                      rownames = F,
                      colnames = c(input$area_name_map, "Observed", "Expected", "SIR",  "95% CIs",  "RR",  "95% CrIs", "Exc. Probability", "Population", "SIR Percent Change"),
                      extensions = 'Buttons',

                      options = list(
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

    if(input$variable_var == "SIR"){
      tm_shape(map_df_sf) +
        tm_polygons(col = input$variable_var,
                    id = input$area_name_map,
                    title = input$variable_var,
                    border.col = "white",
                    lwd = 0.5,
                    style = "cont",
                    # style = "fixed",
                    n=5,
                    palette = "Reds",
                    popup.vars = c(
                      "Cases: " = "cases",
                      "Expected: " = "exp",
                      "SIR: " = "SIR",
                      "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )} else if(input$variable_var == "RR"){
          tm_shape(map_df_sf) +
            tm_polygons(col = input$variable_var,
                        id = input$area_name_map,
                        title = input$variable_var,
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc"))
        } else if(input$variable_var == "exc"){
          tm_shape(map_df_sf) +
            tm_polygons(col = "exc",
                        id = input$area_name_map,
                        title = "Exceedance Probability",
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc"))
        } else{
          tm_shape(map_df_sf) +
            tm_polygons(col = input$variable_var,
                        id = input$area_name_map,
                        title = input$variable_var,
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "Population: " = "area_pop"))
        }
  })

  output$var_map2 <- renderTmap({
    map_df_sf <- map_test()

    if(input$variable_var2 == "SIR"){
      tm_shape(map_df_sf) +
        tm_polygons(col = input$variable_var2,
                    id = input$area_name_map,
                    title = input$variable_var2,
                    border.col = "white",
                    lwd = 0.5,
                    style = "cont",
                    # style = "fixed",
                    n=5,
                    palette = "Reds",
                    popup.vars = c(
                      "Cases: " = "cases",
                      "Expected: " = "exp",
                      "SIR: " = "SIR",
                      "95% CIs: " = "cis")) +
        tm_layout(
          frame = F,
          legend.title.size = 1.2,
          legend.text.size = 1
        )} else if(input$variable_var2 == "RR"){
          tm_shape(map_df_sf) +
            tm_polygons(col = input$variable_var2,
                        id = input$area_name_map,
                        title = input$variable_var2,
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc"))
        } else if(input$variable_var2 == "exc"){
          tm_shape(map_df_sf) +
            tm_polygons(col = "exc",
                        id = input$area_name_map,
                        title = "Exceedance Probability",
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "RR: " = "RR",
                          "95% CrIs: " = "cris",
                          "Exceedance Prob: " = "exc"))
        } else{
          tm_shape(map_df_sf) +
            tm_polygons(col = input$variable_var2,
                        id = input$area_name_map,
                        title = input$variable_var2,
                        border.col = "white",
                        lwd = 0.5,
                        style = "cont",
                        # style = "fixed",
                        n=5,
                        palette = "Reds",
                        popup.vars = c(
                          "Cases: " = "cases",
                          "Expected: " = "exp",
                          "Population" = "area_pop"))
        }
  })

  output$spatial_effect <- renderUI({

    #INLA
    if(input$spatial_choice == "Yes"){
    if(input$model_choice == "bym2"){
      text <- improved_res()

      return(HTML(paste0("The spatially structured effect was ", round(text,4), ". Meaning that ", round(text*100,2), "% of the variance in the data can be explained by a spatial effect.")))

    } else {

      text <- improved_res()
      return(HTML(paste0("The spatially structured effect was ", round(text,4), ". Meaning that ", round(text*100,2), "% of the variance in the data can be explained by a spatial effect.")))


    }} else{
      HTML("To view this data table, please select", em("Yes"), "to the", em("Spatial Modelling"), "dropdown.")
    }
  })

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
      paste("To view this data table, please select", em("Yes"), "to the", em("Spatial Modelling"), "dropdown.")
    }
  })

}
