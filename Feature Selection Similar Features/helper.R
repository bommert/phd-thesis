help_fun = function(instance, lrn, ps, ctrl = instance$ctrl) {
  res = tuneParams(lrn, instance$task, instance$rin, par.set = ps, control = ctrl)
  lrn.opt = setHyperPars(lrn, par.vals = res$x)
  mod = train(lrn.opt, instance$task)
  feats = extractSelectedFeatures(mod)
  ret = getTaskFeatureNames(instance$task) %in% feats
  return(ret)
}
