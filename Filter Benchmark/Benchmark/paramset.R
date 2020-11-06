mm = makeModelMultiplexer(list("classif.glmnet", "classif.kknn", "classif.ksvm"))

ps = makeModelMultiplexerParamSet(mm,
  classif.glmnet = makeParamSet(
    makeDiscreteParam("alpha", values = 0L),
    makeNumericParam("s", lower = -15, upper = 15, trafo = function(x) 2^x)
  ),
  classif.kknn = makeParamSet(
    makeDiscreteParam("kernel", values = "rectangular"),
    makeIntegerParam("k", lower = 1L, upper = 20L)
  ),
  classif.ksvm = makeParamSet(
    makeNumericParam("sigma", lower = -15, upper = 15, trafo = function(x) 2^x),
    makeNumericParam("C", lower = -15, upper = 15, trafo = function(x) 2^x)
  )
)

ps = c(ps, makeParamSet(
  makeNumericParam("fw.perc", lower = 0, upper = 1)
))
