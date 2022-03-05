library("bayesplot")

prior_predictive_check <- function(feature.names, dataset.size = 120){

  sample.size = 3000

  sapply(feature.names, function(col, climateset, sample.size, dataset.size){


    data = climateset[,col]

    if( all( data >= 0) )
      data = log(data + 0.01)

    if( all( data < 0) )
      data = log(-data)

    #moy = mean(data)

    #std = sd(data)

    ####TODO: scale the variables at the beginning
    data = as.numeric(scale(data))

    x <- list(y = data,
              #yrep = matrix(rnorm(sample.size * dataset.size, mean = moy, sd = std),
              yrep = matrix(rnorm(sample.size * dataset.size),
              nrow = sample.size,
              ncol = dataset.size))

    g_plot = ppc_dens_overlay(xlibrary("bayesplot"))
    plot(g_plot)
  }, climateset, sample.size, dataset.size)
}
prior_predictive_check <- function(feature.names, dataset.size = 120){

  sample.size = 3000

  sapply(feature.names, function(col, climateset, sample.size, dataset.size){


    data = climateset[,col]

    if( all( data >= 0) )
      data = log(data + 0.01)

    if( all( data < 0) )
      data = log(-data)

    #moy = mean(data)

    #std = sd(data)



    ####TODO: scale the variables at the beginning
    data = as.numeric(scale(data))

    x <- list(y = data,
              #yrep = matrix(rnorm(sample.size * dataset.size, mean = moy, sd = std),
              yrep = matrix(rnorm(sample.size * dataset.size),
              nrow = sample.size,
              ncol = dataset.size))

    g_plot = ppc_dens_overlay(x$y, x$yrep[1:100,])

    plot(g_plot)

    #feature_density(feature.names)
  }, climateset, sample.size, dataset.size)

  return(invisible(NULL))
}
