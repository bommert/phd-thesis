resampling = function(lrn, instance) {

  source("measure_time_filter.R")
  ms = list(mmce, timetrain, timetrain.filter)

  # inner resampling
  r = resample(learner = lrn, task = instance$task.train, resampling = instance$rin.train,
    measures = ms, models = FALSE)

  # outer evaluation
  feats.train = getTaskFeatureNames(instance$task.train)
  feats.test = getTaskFeatureNames(instance$task.test)
  feats.diff = setdiff(feats.test, feats.train)
  if (length(feats.diff) > 0) {
    task.test = subsetTask(instance$task.test, features = feats.train)
  } else {
    task.test = instance$task.test
  }

  mod = train(lrn, instance$task.train)
  pred = predict(mod, task.test)
  mt = performance(pred = pred, measures = ms, model = mod)

  if ("FilterWrapper" %in% class(mod$learner)) {
    feats = getFilteredFeatures(mod)
  } else {
    feats = feats.train
  }

  ret = list(
    measures.tune = r$measures.test,
    measures.test = mt,
    features.test = feats
  )

  return(ret)
}

