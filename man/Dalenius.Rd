\name{Dalenius}
\alias{Dalenius}
\title{
Estratificación univariada de Dalenius & Hodges
}
\description{
Función que permite crear estratos con el mínimo de varianza dentro de cada grupo y el máximo entre los estratos.
}
\usage{
Dalenius(variable, L = 3, plot = T)
}
\arguments{
  \item{variable}{
Un vector de clase numérica. Es la variable que se desea estratificar.
}
\item{L}{
Número de estratos a crear (mínimo 3)
}
\item{plot}{
Valor logico que indica si se creará un histograma que muestre los estratos creados.
}
}
\details{
Por default, el metodo admite 3 estratos como mínimo. Los elementos dentro de cada estrato tienen varianza mínima en su interior y máxima entre estratos
}
\value{
\item{Clases}{Devuelve el objeto data.frame con las min(L*10,n) clases generadas para hacer el proceso de estratificación}
\item{Cortes}{Indica los puntos de cortes de cada uno de los estratos}
\item{Estratos}{Devuelve un objeto de la misma longitud que la variable introducida para estratificar. El objeto devuelto está estratificado con base en los cortes generados}
\item{Frecuencias}{Indica las frecuencias absolutas y relativas de elementos que contiene cada uno de los estratos generados}
}
\references{
http://gaia.inegi.org.mx/scince2/documentos/scince/fichaTecnica.pdf
}
\author{
Oscar Arturo Bringas López
}
\examples{
Dalenius(variable = rnorm(100000),L = 3, plot = T)
Dalenius(variable=rbinom(1000000,50000,.5),L = 7, plot = T)
}
