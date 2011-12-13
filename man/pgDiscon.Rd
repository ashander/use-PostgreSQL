\name{pgDiscon}
\alias{pgDiscon}
\title{Close all connections to PostgreSQL DB}
\usage{
  pgDiscon(con.drv = NULL)
}
\arguments{
  \item{con.drv}{a list containing a non-expired connection
  to and driver for PgSQL database}
}
\description{
  Close all connections to PostgreSQL DB
}
\details{
  closes databse connection using DBI-compliant connection
  via RPostgreSQL package
}
\examples{
\dontrun{
## make a connection
c <- pgCon() #running without argumetns requires pg.auth list set as global option
pgDiscon(c)
}
}

