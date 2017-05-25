source("~/Documents/masters/R/mastersims.R")
mod2005 <- sims(year = 2005, n = 10000)
mod2006 <- sims(year = 2006, n = 10000)
mod2007 <- sims(year = 2007, n = 10000)
mod2008 <- sims(year = 2008, n = 10000)
mod2009 <- sims(year = 2009, n = 10000)
mod2010 <- sims(year = 2010, n = 10000)
mod2011 <- sims(year = 2011, n = 10000)
mod2012 <- sims(year = 2012, n = 10000)
mod2013 <- sims(year = 2013, n = 10000)
mod2014 <- sims(year = 2014, n = 10000)
mod2015 <- sims(year = 2015, n = 10000)
mod2016 <- sims(year = 2016, n = 10000)
mod2017 <- sims(year = 2017, n = 10000)

# after running sims() for years 2005 - 2017 and with n = 10000
f <- function(x, year){
  x$year <- year
  return(x)
}
mod2005 <- f(x = mod2005, year = 2005)
mod2006 <- f(x = mod2006, year = 2006)
mod2007 <- f(x = mod2007, year = 2007)
mod2008 <- f(x = mod2008, year = 2008)
mod2009 <- f(x = mod2009, year = 2009)
mod2010 <- f(x = mod2010, year = 2010)
mod2011 <- f(x = mod2011, year = 2011)
mod2012 <- f(x = mod2012, year = 2012)
mod2013 <- f(x = mod2013, year = 2013)
mod2014 <- f(x = mod2014, year = 2014)
mod2015 <- f(x = mod2015, year = 2015)
mod2016 <- f(x = mod2016, year = 2016)
mod2017 <- f(x = mod2017, year = 2017)
# 2014 - 2017 have more cols
masters <- rbind(mod2005, mod2006, mod2007, mod2008, mod2009, 
                 mod2010, mod2011, mod2012, mod2013)
masters$vegasranks <- NA
masters <- rbind(masters, mod2014, mod2015, mod2016, mod2017)
write.csv(df, "~/Documents/masters/allyears.csv", row.names = FALSE)
rm(list = ls())