.generate.observations <-
  function(design){

    eff.light <- c(-10, -2, 2, 3)
    eff.heat.v1 <- c(-2, 0, 1.5, 2)
    eff.heat.v2 <- c(-3, 0, 3.5, 1.5)
    
    light <- 1:4    
    heat <- 1:4
    
    light <- rep(light, times = 4)
    heat <- rep(heat, each = 4)
    
    ylight <- rep(eff.light, times = 4)
    yheat1 <- rep(eff.heat.v1, each = 4)
    yheat2 <- rep(eff.heat.v2, each = 4)
    y1 <- 52.5 + ylight + yheat1
    y2 <- 52 + ylight + yheat2
    
    
    mdf <- data.frame(ymean = c(y1, y2), light = c(light, light), 
                      heat = c(heat, heat), variety = rep(c("R", "F"), each = 16))
    # interaction.plot(light, as.factor(heat), y1)
    # interaction.plot(light, as.factor(heat), y2)
    # a1 <- aov(ymean ~ as.factor(light)*as.factor(heat) + as.factor(variety), 
    #           data = mdf)
    # summary(a1)

    sigma.season <- 3
    sigma.side <- 2
    sigma.greenhouse <- 2
    sigma <- 1.8
    
    eff.side <- rnorm(2, 0, sigma.side)
    eff.greenhouse <- rnorm(2, 0, sigma.greenhouse)
    eff.season <- rnorm(2, 0, sigma.season)

    dm <- design
    n <- dim(dm)[1]
    obs <- numeric(n)
    i = 1
    for (i in 1:n) {
      
      li = dm$light[i]
      hi = dm$heat[i]
      vi = dm$variety[i]
      
      sr <- with(mdf, 
                 which(light == li & heat == hi & variety == vi))
      (obs.mean <- mdf$ymean[sr])
    
      greenhouse <- switch(dm$greenhouse[i], A = 1, B = 2)
      side <- switch(dm$side[i], n = 1, s = 2)

      (obs[i] <- obs.mean + eff.greenhouse[greenhouse] +
          eff.side[side] + eff.season[dm$season[i]] + 
          rnorm(1, 0, sigma))
    }

    miss <- sample(1:n, size = 2)
    obs[miss] <- NA
    return(data.frame(dm, obs = obs))
  }

