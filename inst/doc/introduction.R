## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>",
  fig.height=5,
  fig.width=5
)

## ----echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE----------------------
#  devtools::install_github("RobinDenz1/CareDensity")

## ---- warning=FALSE, message=FALSE--------------------------------------------
library(CareDensity)
library(igraph)
library(data.table)
library(MatrixExtra)

set.seed(3431)

# some arbitrary patient-provider contact data
data <- data.frame(PatID=c("1", "1", "1", "2", "2", "3", "3", "4", "5"),
                   ArztID=c("A", "C", "D", "A", "D", "A", "D", "D", "C"))
data

## -----------------------------------------------------------------------------
# create graph
g <- graph_from_data_frame(data, directed=FALSE)

# add type
V(g)$type <- bipartite_mapping(g)$type

# change some things for better plots
V(g)$color <- ifelse(V(g)$type, "salmon", "lightblue")
V(g)$shape <- ifelse(V(g)$type, "square", "circle")
E(g)$color <- "lightgray"

plot(g, vertex.label.cex=0.8, vertex.label.color="black")

## -----------------------------------------------------------------------------
# project to provider mode
mat <- project_to_one_mode(g, mode="cols")

# make it an igraph object again
g_provider <- graph_from_adjacency_matrix(mat, mode="upper", weighted=TRUE, diag=FALSE)

# plot it
plot(g_provider, edge.label=E(g_provider)$weight, vertex.shape="square",
     edge.color="black", vertex.color="salmon",
     vertex.label.cex=0.8)

## ---- message=FALSE-----------------------------------------------------------
care_density(data)

## -----------------------------------------------------------------------------
d_type <- data.frame(ID=c("A", "C", "D"),
                     Type=c("GP", "GP", "Psychiatrist"))
d_type

## -----------------------------------------------------------------------------
fragmented_care_density(data, weights=NULL, type=d_type, by_connection=TRUE)

## -----------------------------------------------------------------------------
d_weights <- data.frame(from=c("GP", "GP", "Psychiatrist"),
                        to=c("GP", "Psychiatrist", "Psychiatrist"),
                        weight=c(1, 1, 1))
d_weights

## -----------------------------------------------------------------------------
fragmented_care_density(data, weights=d_weights, type=d_type)

## -----------------------------------------------------------------------------
d_weights <- data.frame(from=c("GP", "GP", "Psychiatrist"),
                        to=c("GP", "Psychiatrist", "Psychiatrist"),
                        weight=c(1.1, 0.5, 1.3))
d_weights

## -----------------------------------------------------------------------------
fragmented_care_density(data, weights=d_weights, type=d_type)

## -----------------------------------------------------------------------------
d_outcome <- data.frame(PatID=c("1", "2", "3", "4", "5"),
                        Y=c(0, 0, 1, 1, 0))
d_outcome

## -----------------------------------------------------------------------------
d_consum <- fragmented_care_density(data, weights=NULL, type=d_type, by_connection=TRUE)

## -----------------------------------------------------------------------------
d_consum$sum_weights <- fifelse(d_consum$sum_weights > 0, 1, 0)

## -----------------------------------------------------------------------------
d_consum <- dcast(as.data.table(d_consum), PatID ~ connection,
                  value.var="sum_weights", fill=0)
d_consum$`NA` <- NULL

## -----------------------------------------------------------------------------
d_outcome <- merge(d_consum, d_outcome, by="PatID")

## -----------------------------------------------------------------------------
mod <- glm(Y ~ `GP - GP` + `Psychiatrist - GP`, data=d_outcome, family="binomial")
summary(mod)

