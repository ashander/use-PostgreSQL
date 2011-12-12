\name{pgQry}
\alias{pgQry}
\title{Send/retrieve SQL queries from a PostgreSQL database}
\usage{
  pgQry(query, con.drv = NULL)
}
\arguments{
  \item{query}{a valid SQL query}

  \item{con.drv}{a list containing a non-expired connection
  to and driver for PgSQL database}
}
\description{
  Send/retrieve SQL queries from a PostgreSQL database
}
\details{
  Runs SQL query against and returns result
}
\examples{
\dontrun{
## make a connection
c <- pgCon()  #running without argumetns requires pg.auth list set as global option
mytable.df<-pgQ("select * from myschema.mytable", c)
pgDiscon(c)
}
}

