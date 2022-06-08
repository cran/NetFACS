## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  fig.path = "man/figures/README-",
  out.width = "100%", 
  message = FALSE, 
  warning = FALSE
)

## ----setup, include = FALSE---------------------------------------------------
library(NetFACS)
library(ggplot2)
library(knitr)

## ----load.netfacs, echo = T, message=F, eval = F------------------------------
#  # install NetFACS from CRAN
#  install.packages("NetFACS")
#  
#  # read library
#  library(NetFACS)

## ----instal.dev, echo = T, message=F, eval = F--------------------------------
#  # install NetFACS from GitHub
#  devtools::install_github("NetFACS/NetFACS")

## ----read.data, echo = T------------------------------------------------------
data("letternet") # this is the Manifesto #
data("emotions_set") # this is the CK Database #

## ----cm.table, echo=FALSE-----------------------------------------------------
kable(head(letternet[[1]]), row.names = FALSE, caption = "Letter data for words in the Communist Manifesto")
kable(head(letternet[[2]]), row.names = FALSE, caption = "Additional information for Communist Manifesto")

## ----odd.photo, echo=TRUE-----------------------------------------------------
odd.photo <- data.frame(
  photo = c("photo1", "photo2", "photo3", "photo4", "photo5", "photo6"),
  AUs = c(
    "AU1/AU5/AU9",
    "AU1/AU2",
    "AU1/AU2/AU10",
    "AU1/AU2",
    "AU5/AU17/AU18",
    "AU6/AU12"
  )
)
kable(odd.photo, row.names = FALSE, caption = "Photo Data")

## ----odd.video, echo=TRUE-----------------------------------------------------
odd.video <- data.frame(
  video = c(rep("video 1", 3),
            rep("video 2", 2),
            rep("video 3", 3)),
  AUs = c("AU1", "AU5", "AU9",
          "AU1", "AU2",
          "AU1", "AU2", "AU10"),
  start.times = c(0.1, 0.2, 0.3,
                  0.1, 0.3,
                  0.1, 0.4, 0.4),
  durations = rep(0.3, times = 8)
)
kable(odd.video, row.names = FALSE, caption = "Video Data")

## ----prepare.net, echo=TRUE---------------------------------------------------
au.prepared <- prepare.netfacs(
  elements = odd.video$AUs,
  type = "video",
  video.id = odd.video$video,
  start.time = odd.video$start.times,
  duration = odd.video$durations,
  frame.duration = 0.05
)
kable(head(au.prepared$element.matrix),
      row.names = FALSE,
      caption = "Element Matrix of prepare.netfacs")
kable(head(au.prepared$video.info),
      row.names = FALSE,
      caption = "Video Info of prepare.netfacs")

## ----netfacs, cache=T---------------------------------------------------------
# here, we test whether any Action Units and combinations appear more frequently than expected under one condition than under another.
# load data
au.data <- emotions_set[[1]]
# this is the basic data frame, with each video represented by a row, and each Action Unit represented by a column

au.info <- emotions_set[[2]]
# this is the additional information about the emotions etc

# We remove AU 25 here, because it is not informative, and all AUs that have 2 or fewer occurrences, because not meaningful interpretation is available for them
au.data <- au.data[, setdiff(colnames(au.data), "25")]
au.data <- au.data[, colSums(au.data) > 2]

# create netfacs object for angry faces
angry.face <- netfacs(
  data = au.data, # this is the data matrix
  condition = au.info$emotion, # info about condition of each case
  test.condition = "anger", # condition we are interested in
  null.condition = NULL, # null condition (test against all other emotions)
  duration = NULL, # we could add duration information for videos
  ran.trials = 1000, # number of randomizations. The larger the better
  control = NULL, # control variables, e.g. gender, ethnicity etc.
  random.level = NULL, # Works like a random effect.
  combination.size = 4, # limit the analysis to make things faster,
  tail = "upper.tail", # should the p-value reflect two-tailed or right/left tailed testing?
  use_parallel = TRUE, # use parallel processing
  n_cores = NULL # number of cores for parallel processing
)

## ----netfacs.table, echo=FALSE------------------------------------------------
kable(
  head(angry.face$result[angry.face$result$count > 0,], 20),
  row.names = FALSE,
  align = "c",
  caption = "Top rows of the netfacs function results"
)

## ----extract.anger, cache=T---------------------------------------------------
# extract angry face information for the first level (single elements)
anger.aus <- netfacs_extract(
  netfacs.data = angry.face,
  combination.size = 1, # only looking at combinations with 1 element (here, Action Units)
  min.count = 1, # minimum number of times that the combination should occur
  min.prob = 0, # minimum observed probability of the combination
  min.specificity = 0, # minimum specificity of the combination
  significance = 0.01
) # significance level we are interested in

## ----first.level.table, echo=FALSE--------------------------------------------
kable(anger.aus[order(-1 * anger.aus$effect.size),],
      align = "c",
      row.names = FALSE,
      caption = "Result of netfacs_extract for single elements")

## ----element.plot, fig.width=6, fig.height=4, fig.align='center', message=F----
# create plot showing the importance of each AU for the angry faces

element.plot(netfacs.data = angry.face)

## ----distribution.plot, fig.width=6, fig.height=4, fig.align='center', message=F----
# create plot showing the distribution of the null probabilities and how the observed probability compares

distribution.plot(netfacs.data = angry.face)$"4"
distribution.plot(netfacs.data = angry.face)$"9"

## ----third.level.anger--------------------------------------------------------
# extract information for three-element-combinations in angry faces

anger.aus3 <- netfacs_extract(
  netfacs.data = angry.face,
  combination.size = 3, # only looking at combinations with 3 elements (here, Action Units)
  min.count = 5, # minimum number of times that the combination should occur
  min.prob = 0, # minimum observed probability of the combination
  min.specificity = 0, # minimum specificity of the combination
  significance = 0.01
) # significance level we are interested in

## ----third.level.table, echo=FALSE--------------------------------------------
kable(head(anger.aus3[order(-1 * anger.aus3$effect.size),]),
      align = "c",
      row.names = FALSE,
      caption = "Results of netfacs_extract function for combinations of three elements")

## ----element.specificity------------------------------------------------------

specificity <- element.specificity(netfacs.data = angry.face)

## ----element.specificity.table, echo=FALSE, align = "c"-----------------------
kable(
  specificity[1],
  align = "c",
  row.names = FALSE,
  digits = 2,
  booktabs = TRUE,
  caption = "Results of the specificity increase in combinations due to to inclusion of each element"
)

## ----conditional.probs--------------------------------------------------------
conditional.probs <- network.conditional(
  netfacs.data = angry.face,
  min.prob = 0,
  min.count = 5,
  ignore.element = NULL
)

## ----cond.probs.table, echo=FALSE---------------------------------------------
# show only a number of the conditional probabilities
kable(
  conditional.probs$conditional.probalities[c(1:6, 31:36),],
  row.names = FALSE,
  align = "c",
  caption = "Conditional probabilities for a subset of dyadic combinations"
)

## ----multi.facs---------------------------------------------------------------
multi.facs <- netfacs_multiple(
  data = au.data,
  condition = au.info$emotion,
  ran.trials = 1000,
  combination.size = 2,
  use_parallel = TRUE
)

## ----overlap, fig.height=8, fig.width=10, fig.align='center', message=F-------
overlap.net <- overlap.network(
  netfacs.list = multi.facs,
  min.prob = 0, # minimum probability of a connection to be included
  min.count = 3, # minimum count of co-occurrences for a connection to be included
  significance = 0.01, # significance level for combinations to be considered
  clusters = FALSE, # should the bipartite network be clustered
  plot.bubbles = TRUE,
)

plot(overlap.net$specificity)
plot(overlap.net$occurrence)

## ----conditional.plot, fig.height=8, fig.width=10, fig.align='center', message=F----
conditional.probs <- network.conditional(
  netfacs.data = angry.face,
  min.prob = 0.5,
  min.count = 5,
  ignore.element = NULL,
  plot.bubbles = TRUE
)

# plot conditional probabilities
conditional.probs$plot

## ----angry.net----------------------------------------------------------------
angry.net <- netfacs.network(
  netfacs.data = angry.face,
  link = "unweighted", # edges are linked for significant results only
  significance = 0.01,
  min.count = 3, # remove rare elements as they might be random variation
  min.prob = 0
)

## ----angry.plot, fig.width=8, fig.height=8, fig.align='center', message=F-----
network.plot(
  netfacs.graph = angry.net,
  title = "angry network",
  clusters = FALSE,
  plot.bubbles = TRUE,
  hide.unconnected = TRUE
)

## ----multi.net----------------------------------------------------------------
multi.net <- multiple.netfacs.network(
  netfacs.list = multi.facs,
  link = "weighted", # network contains edges where significantly connected
  significance = 0.01,
  min.count = 3, # again remove rare connections
  min.prob = 0
)

## ----multi.plot, fig.width=10, fig.height=8, fig.align='center', message=F----
multiple.network.plot(netfacs.graphs = multi.net)

## ----all.face, cache=F--------------------------------------------------------
all.face <-
  netfacs(
    data = au.data,
    condition = NULL,
    ran.trials = 1000,
    combination.size = 2,
    use_parallel = TRUE
  )
all.net <-
  netfacs.network(netfacs.data = all.face,
                  min.count = 3,
                  link = "unweighted")

## ----all.plot, fig.width=8, fig.height=8, fig.align='center', message=F-------
network.plot(
  netfacs.graph = all.net,
  title = "all network with clusters",
  clusters = TRUE,
  plot.bubbles = TRUE
)

## ---- network.summary---------------------------------------------------------
net.sum <- network.summary(angry.net)

## ----net.sum.table, echo=FALSE------------------------------------------------
# show only a number of the conditional probabilities
kable(
  net.sum,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network centrality measures for angry faces"
)

## ----network.summary.graph----------------------------------------------------
net.sum.graph <- network.summary.graph(angry.net)

## ----net.graph.table, echo=FALSE----------------------------------------------
kable(
  net.sum.graph,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network graph measures for anry faces"
)

## ----multinet.summary---------------------------------------------------------
xx <- lapply(multi.net, function(x) {
  network.summary.graph(x)
})
xx <- do.call(rbind, xx)
xx <- cbind(emotion = names(multi.net), xx)

## ----net.sum.all.table, echo=FALSE--------------------------------------------
kable(
  xx,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Network graph measures for all faces"
)

## ----event.size.angry---------------------------------------------------------
event.size.angry <- angry.face$event.size.information
size.plot <- event.size.plot(netfacs.data = angry.face)

## ----event.size angry.table, echo=FALSE---------------------------------------
kable(
  event.size.angry,
  align = "c",
  row.names = FALSE,
  digits = 2,
  caption = "Combination sizes of facial expressions in the angry condition"
)

## ----size.plot, fig.width=10, fig.height=8, fig.align='center', message=F, echo=F----
plot(size.plot)

## ----happy, echo=F------------------------------------------------------------
happy.face <-
  netfacs(
    data = au.data,
    condition = au.info$emotion,
    test.condition = "happy",
    ran.trials = 1000,
    use_parallel = TRUE
  )

## ----event.size.happy, echo = F-----------------------------------------------
kable(
  happy.face$event.size.information,
  align = "c",
  row.names = FALSE,
  digits = 2,
  caption = "Combination sizes of happy expressions in the angry condition"
)

## ----entropy------------------------------------------------------------------
xx <- lapply(multi.facs, function(x) {
  entropy.overall(x)
})
xx <- do.call(rbind, xx)
xx <- cbind(emotion = names(multi.facs), xx)

## ----entropy.results, echo = F------------------------------------------------
kable(
  xx,
  align = "c",
  row.names = FALSE,
  digits = 3,
  caption = "Ratios between expected and observed entropies in different emotions"
)

## ---- eval = F----------------------------------------------------------------
#  # create an edge table
#  anger.tab <- igraph::as_data_frame(multi.net$anger)
#  
#  # create an adjacency matrix
#  anger.adj.mat <- as.matrix(igraph::as_adjacency_matrix(multi.net$anger))
#  
#  # save as CSV file
#  # write.csv(anger.tab, "anger_net_tab.csv")
#  # write.csv(anger.adj.mat, "adj_net_mat.csv")

