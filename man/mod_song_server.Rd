% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/modSong.R
\name{mod_song_server}
\alias{mod_song_server}
\title{Song Server}
\usage{
mod_song_server(id, td)
}
\arguments{
\item{id}{An ID string that corresponds with the ID used to call the
module's UI function.}
}
\value{
reactive url of the selected team song.
}
\description{
Hosts 2 songs, the team song can be turned off but the piki piki song cannot
Also home of the App startup informational modal.
}
\keyword{internal}
