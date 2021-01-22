# Template csv to show on the file upload page of shiny
template_csv <- read.csv(system.file('smallareamapp/extdata', 'template_csv.csv', package='smallareamapp'))

# template_csv2 <- read.csv(system.file('smallareamapp/extdata', 'scotlip_shiny_input.csv', package='smallareamapp')) %>%
#   mutate(sir = round(as.numeric(sir),2),
#          lci = NA_real_,
#          uci = NA_real_)
set.ZeroPolicyOption(TRUE)
get.ZeroPolicyOption()

#prior settings
prior <- list(
  prec = list(
    prior = "pc.prec",
    param = c(0.2 / 0.31, 0.01),
    initial = 5),
  phi = list(
    prior = "pc",
    param = c(0.5, 2 / 3),
    initial = -3)
)

#formula settings

formula <- cases ~ 1 + f(idarea,
                         model = "bym2",
                         graph = g,
                         constr = T,
                         hyper = prior,
                         scale.model = T)

formula_bym <- cases ~ 1 + f(idarea, model="bym",graph=g, hyper =
                         list(prec.unstruct = list(prior="loggamma",param=c(1,0.01)),
                              prec.spatial = list(prior="loggamma",param=c(1,0.001))))

 #default priors
# formula_bym <- cases ~ 1 +
#   f(idareau, model = "besag", graph = g) +
#   f(idareav, model = "iid")

formula_bym <- cases ~ 1 +
  f(idarea, model = "bym", graph = g)


tmap_mode("view")
