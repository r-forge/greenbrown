\name{plot.Sofia}
\alias{plot.Sofia}
\title{plot a Sofia object}
\description{Plots a \code{\link{Sofia}} object.}
\usage{\method{plot}{Sofia}(x, ylab = "y", mfrow = NULL, names = NULL, main = NULL, 
    plot.order = NULL, labels = paste0("(", letters, ")"), ...)}
\arguments{
  \item{x}{a 'Sofia' object}
  \item{ylab}{label for response variable}
  \item{mfrow}{number of rows and columns for the plot}
  \item{names}{names of the variables in the response functions}
  \item{main}{title of the plot}
  \item{plot.order}{Order for plotting of factors}
  \item{labels}{Labels for subplots. Set to NULL to avoid labels.}
  \item{\dots}{further arguments (not used)}
}
\details{No details.}

\references{No reference.	}
\author{Matthias Forkel <matthias.forkel@geo.tuwien.ac.at> [aut, cre]}



\seealso{\code{\link{Sofia}}}
\examples{

# get data
data(firedata)

# predictor variables
train <- firedata$train == 1 # use training data
xvars.df <- data.frame(
  NLDI = firedata$NLDI[train],
  CRU.WET.orig = firedata$CRU.WET.orig[train],
  Liu.VOD.annual = firedata$Liu.VOD.annual[train],
  GIMMS.FAPAR.pre = firedata$GIMMS.FAPAR.pre[train],
  CRU.DTR.orig = firedata$CRU.DTR.orig[train]
)

# observed data
obs <- firedata$GFED.BA.obs[train]
regid <- firedata$regid[train]

# Which x variable should depend on land cover?
per.group <- c(FALSE, TRUE, TRUE, TRUE, TRUE)

# land cover
area <- data.frame(
  Tree = firedata$CCI.LC.Tree[train],
  Shrub = firedata$CCI.LC.Shrub[train],
  HrbCrp = firedata$CCI.LC.HrbCrp[train]
)

# define parameters (from Forkel et al. 2016, Fig. 1)
sofiapar <- SofiaPar(colnames(xvars.df), colnames(area), per.group=per.group, 
                     par.act=c(1.9, 0, 780, 1, # for PopDens
                               0.3, 1.1, -5.3, 0, 0, 0, 8.9, 0.54, -23, -39, 13, -16, # for CRU.DTR
                               0.13, 3, 0.53, 0, 0, 0, 0.35, -0.44, 0.36, -1.2, -4.8, -45, # for CRU.WET
                               -0.7, 18, -1.5, 0, 0, 0, 22, 11, -17, -2.3, 0.64, 1,  # for GIMMS.FAPAR
                               1.9, 3, -0.36, 0, 0, 0, -21, 68, -38, 0.35, 0.31, 0.11) # for Liu.VOD
)

# run model
sf <- Sofia(xvars.df, area, per.group=per.group, sofiapar=sofiapar)
plot(sf) 


}
