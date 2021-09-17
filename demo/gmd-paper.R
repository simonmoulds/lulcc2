## July 2015, updated June 2018

## Demo script to run the code and produce the figures shown in
## the Geoscientific Model Development paper

library(RColorBrewer)  ## required for plotting

## load data
data(pie)

## observed maps
lu <- DiscreteLulcRasterStack(x=stack(pie[1:3]),
                               categories=c(1,2,3),
                               labels=c("Forest","Built","Other"),
                               t=c(0,6,14))

## show object
lu

## figure
p <- plot(lu,
          between=list(x=0,y=0),
          par.settings=list(axis.line=list(col="black"),
            strip.background=list(col="lightgrey")),
          par.strip.text=list(cex=0.6),
          scales=list(cex=0.6),
          col.regions=c("palegreen","midnightblue","indianred1"),
          colorkey=FALSE,
          layout=c(3,1),
          key=list(space="bottom",
            cex=0.6,
            rectangles=list(col=c("palegreen","midnightblue","indianred1"), size=3),
            text=list(labels=c("Forest","Built","Other"))))
p

## library(lattice)
## trellis.device(device="pdf", width=4.72, height=3, file="f03_pie.pdf", family="Courier")
## print(p)
## dev.off()

## cross tabulate change between two time points
crossTabulate(lu, times=c(0,14))

## explanatory variables
ix <- data.frame(var=c("ef_001","ef_002","ef_003"),
                 yr=c(0,0,0),
                 dynamic=c(F,F,F))
ef <- ExpVarRasterStack(x=stack(pie[4:6]), index=ix)
ef

## resample ef (not necessary in this example)
ef <- resample(ef, lu)

## fit statistical models
part <- partition(x=lu, size=0.1, spatial=TRUE, t=0)

train.data <- getPredictiveModelInputData(lu=lu,
                                          ef=ef,
                                          cells=part[["train"]],
                                          t=0)

test.data  <- getPredictiveModelInputData(lu=lu,
                                          ef=ef,
                                          cells=part[["test"]],
                                          t=0)

library(randomForest)
library(rpart)

built.form  <- as.formula("Built ~ ef_001 + ef_002 + ef_003")
built.rf    <- randomForest(built.form, data=train.data)
built.glm   <- glm(built.form, family=binomial, data=train.data)
## built.rpart <- rpart(built.form, method="class", data=train.data)

forest.form <- as.formula("Forest ~ ef_001 + ef_002")
forest.rf   <- randomForest(forest.form, data=train.data)
forest.glm  <- glm(forest.form, data=train.data)

other.form  <- as.formula("Other ~ ef_001 + ef_002")
other.rf    <- randomForest(other.form, data=train.data)
other.glm   <- glm(other.form, data=train.data)

rf.mods  <- new("PredictiveModelList",
                models=list(forest.rf, built.rf, other.rf),
                categories=lu@categories,
                labels=lu@labels)

glm.mods <- PredictiveModelList(models=list(forest.glm, built.glm, other.glm),
                                categories=lu@categories,
                                labels=lu@labels)

rf.pred  <- PredictionList(models=rf.mods, newdata=test.data)
rf.perf  <- PerformanceList(pred=rf.pred, measure="rch")

glm.pred <- PredictionList(models=glm.mods, newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

## create suitability maps
all.data <- as.data.frame(x=ef, cells=part[['all']])
probmaps <- predict(object=glm.mods, newdata=all.data, data.frame=TRUE)
points <- rasterToPoints(lu[[1]], spatial=TRUE)
probmaps <- SpatialPointsDataFrame(points, probmaps)
r <- rasterize(x=probmaps, y=lu[[1]], field=names(probmaps))

## figure 4
library(RColorBrewer)
p <- rasterVis::levelplot(r,
                          layout=c(3,1),
                          margin=FALSE,
                          par.strip.text=list(cex=0.6),
                          par.settings=list(axis.line=list(col="black"),
                            strip.background=list(col="lightgrey")),
                          between=list(x=0,y=0),
                          col.regions=colorRampPalette(brewer.pal(9, "YlGnBu")),
                          at=seq(0,1,length=100),
                          scales=list(cex=0.6),
                          colorkey=list(space="bottom",labels=list(cex=0.6),width=0.5))
p

## trellis.device(device="pdf", width=4.72, height=3, file="f04_suitability.pdf", family="Courier")
## print(p)
## dev.off()


## test ability of models to predict location of urban gain
## 1985 -> 1991
part <- rasterToPoints(lu[[1]], fun=function(x) x != 2, spatial=TRUE)
test.data <- getPredictiveModelInputData(lu=lu, ef=ef, cells=part, t=6)

glm.pred <- PredictionList(models=glm.mods[2], newdata=test.data)
glm.perf <- PerformanceList(pred=glm.pred, measure="rch")

## figure 6
p <- plot(list(glm=glm.perf),
          aspect="iso",
          xlab=list(label="False Alarms/(False Alarms + Correct Rejections)", cex=0.6),
          ylab=list(label="Hits/(Hits + Misses)", cex=0.6),
          scales=list(x=list(tck=0.6), y=list(tck=0.6), cex=0.6),
          key.args=list(cex=0.6, size=2.5),
          par.strip.text=list(cex=0.6),
          par.settings=list(strip.background=list(col="lightgrey")))
p

## trellis.device(device="pdf", width=3.27, height=3.27, file="f06_builtgain.pdf", family="Courier")
## print(p)
## dev.off()

## obtain demand scenario
dmd <- approxExtrapDemand(lu=lu, tout=0:14)

## plot demand scenario (figure not shown in paper)
matplot(dmd, type="l", ylab="Demand (no. of cells)", xlab="Time point", lty=1, col=c("Green","Red","Blue"))
legend("topleft", legend=lu@labels, col=c("Green","Red","Blue"), lty=1)

## neighbourhood values
w <- matrix(data=1, nrow=3, ncol=3)
nb <- NeighbRasterStack(x=lu[[1]], weights=w, categories=c(1,2,3))

## create CLUE-S model object
clues.model <- CluesModel(observed.lulc=lu, 
                          explanatory.variables=ef,
                          predictive.models=glm.mods,
                          time=0:14,
                          demand=dmd,
                          history=NULL,
                          mask=NULL,
                          neighbourhood=NULL,
                          transition.rules=matrix(data=1, nrow=3, ncol=3),
                          neighbourhood.rules=NULL,
                          elasticity=c(0.2,0.2,0.2),
                          iteration.factor=0.00001,
                          max.iteration=5000,
                          max.difference=5,
                          ave.difference=5)
clues.result <- allocate(clues.model)

## create Ordered model (Fuchs et al)
ordered.model <- OrderedModel(observed.lulc=lu, 
                              explanatory.variables=ef, 
                              predictive.models=glm.mods, 
                              time=0:14, 
                              demand=dmd,                              
                              transition.rules=matrix(data=1, nrow=3, ncol=3),                              
                              order=c(2,1,3)) 
ordered.result <- allocate(ordered.model, stochastic=TRUE)

## evaluate model result
## ordered
ordered.tabs <- ThreeMapComparison(x=lu[[1]],
                                   x1=lu[[3]],
                                   y1=ordered.result[[15]],
                                   factors=2^(1:8), 
                                   categories=lu@categories,
                                   labels=lu@labels) 

ordered.agr  <- AgreementBudget(x=ordered.tabs) 
ordered.fom  <- FigureOfMerit(x=ordered.tabs) 

plot(ordered.agr, from=1, to=2) 
plot(ordered.fom, from=1, to=2) 

## CLUE-S
clues.tabs <- ThreeMapComparison(x=lu[[1]],
                                 x1=lu[[3]],
                                 y1=clues.result[[15]],
                                 factors=2^(1:8),
                                 categories=lu@categories,
                                 labels=lu@labels)

## plot three dimensional tables in different ways (figures not shown in paper)
plot(clues.tabs)
plot(clues.tabs, category=1, factors=2^(1:8)[c(1,3,5,7)])

## calculate agreement budget and plot

## CLUE-S
clues.agr <- AgreementBudget(x=clues.tabs)
p1 <- plot(clues.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
## p1

## Ordered
p2 <- plot(ordered.agr,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,0.08),
           scales=list(y=list(at=c(seq(from=0,to=0.08,by=0.02)), cex=0.6, tck=0.6), x=list(cex=0.6, tck=0.6)),
           key=list(cex=0.6))
## p2

## figure 7
agr.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
agr.p

## trellis.device(device="pdf", width=4.72, height=5.72, file="f07_agreement.pdf")
## print(agr.p)
## dev.off()

## CLUE-S
clues.fom <- FigureOfMerit(x=clues.tabs)
p1 <- plot(clues.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p1

## Ordered
p2 <- plot(ordered.fom,
           from=1,
           to=2,
           par.strip.text=list(cex=0.6),
           par.settings=list(strip.background=list(col="lightgrey")),
           xlab=list(cex=0.6),
           ylab=list(cex=0.6),
           ylim=c(0,1),
           scales=list(y=list(at=(seq(from=0,to=1,by=0.2)), cex=0.6), x=list(cex=0.6)),
           key=NULL)
p2

fom.p <- c("CLUE-S"=p1, Ordered=p2, layout=c(1,2))
fom.p

## trellis.device(device="pdf", width=4.72, height=4.72, file="f08_figure_of_merit.pdf")
## print(fom.p)
## dev.off()
