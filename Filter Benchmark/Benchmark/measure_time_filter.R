timetrain.filter = makeMeasure(id = "timetrain.filter", minimize = TRUE, best = 0, worst = Inf,
  properties = c("classif", "classif.multi", "multilabel", "regr", "surv", "costsens", "cluster", "req.model"),
  name = "Time for filtering",
  fun = function(task, model, pred, feats, extra.args) {
    if ("FilterWrapper" %in% class(model$learner)) {
      time.all = model$time
      time.model = model$learner.model$next.model$time
      time.filter = max(0, time.all - time.model)
    } else {
      time.filter = 0
    }
    return(time.filter)
  }
)
