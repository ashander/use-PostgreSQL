\name{pgTab}
\alias{pgTab}
\title{Read a full table from PostgreSQL database}
\usage{
  pgTab(tab.name, con.drv = NULL)
}
\arguments{
  \item{tab.name}{a valid table}

  \item{con.drv}{a list containing a non-expired connection
  to and driver for PgSQL database}
}
\description{
  Read a full table from PostgreSQL database
}
\details{
  Runs SQL query against and returns result
}
\examples{
\dontrun{
## make a connection
c <- pgCon()  #running without argumetns requires pg.auth list set as global option
mytable.df<-pgQ("mytable", c)
pgDiscon(c)
}
}

