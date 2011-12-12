\name{pgCon}
\alias{pgCon}
\title{Connect to a PostgreSQL database}
\usage{
  pgCon(auth.list = getOption("pg.auth", stop("Need to provide a list of authorization args with elements host, user, password, dbname, port")),
    drv.name = "PostgreSQL")
}
\arguments{
  \item{auth.list}{a list including authorization arguments
  including host, user, password, dbname, port}

  \item{drv.name}{database driver to use for DBI
  ('PostgreSQL' on mac/linux)}
}
\description{
  Connect to a PostgreSQL database
}
\details{
  constructs a databse connection using DBI-compliant
  connection via RPostgreSQL package returns list of
  connection and driver.
}
\examples{
\dontrun{
options(pg.auth =  alist(host="localhost", user="user", password="", dbname="", port="5432"))
## make a connection
c <- pgCon()
pgDiscon(c)
}
}

