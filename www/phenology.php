
<!-- This is the project specific website template -->
<!-- It can be changed as liked or replaced by other content -->

<?php

$domain=ereg_replace('[^\.]*\.(.*)$','\1',$_SERVER['HTTP_HOST']);
$group_name=ereg_replace('([^\.]*)\..*$','\1',$_SERVER['HTTP_HOST']);
$themeroot='r-forge.r-project.org/themes/rforge/';

echo '<?xml version="1.0" encoding="UTF-8"?>';
?>
<!DOCTYPE html
	PUBLIC "-//W3C//DTD XHTML 1.0 Transitional//EN"
	"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en   ">

  <head>
	<meta http-equiv="Content-Type" content="text/html; charset=UTF-8" />
	<title><?php echo $group_name; ?></title>
	<link href="http://<?php echo $themeroot; ?>styles/estilo1.css" rel="stylesheet" type="text/css" />
	<link rel="stylesheet" href="styles.css" type="text/css">
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>

<h1>greenbrown - land surface phenology and trend analysis</h1>

<!-- menu -->
<hr>
<h2>Contents</h2>
<ul>
	<li><a href="index.php">greenbrown: Introduction</a></li> 
	<li><a href="install.php">Installation and loading</a></li> 
	<li>Using greenbrown - applications and examples:</li>
	<ul>
		<li><a href="trends.php">Trends and breakpoints</a></li>
		<li><a href="phenology.php">Phenology analysis</a></li>
	</ul>
	<li><a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/">Developer summary page</a> </li>
</ul>
<hr>
<!-- end of menu -->

<h2>Using greenbrown: Phenology analysis</h2>

<p>Metrics of land surface phenology and greenness are often derived from vegetation index time series to map the spatial variability of vegetation phenology or to analyse temporal changes in vegetation. Phenology and greenness metrics (PGMs) are for example the start and end of growing season (SOS and EOS) or the greenness during the peak of the growing season (PEAK). A description of these metrics and a discussion on the caveats and challenges of phenology detection from remote sensing-derived vegetation index time series can be found in Forkel et al. (2015): </p>

<p><ul>Forkel M, Migliavacca M, Thonicke K, Reichstein M, Schaphoff S, Weber U, Carvalhais N (2015) <a href="http://onlinelibrary.wiley.com/doi/10.1111/gcb.12950/abstract">Co-dominant water control on global inter-annual variability and trends in land surface phenology and greenness.</a> Global Change Biology, accepted.</ul></p>

<p>In the greenbrown package, phenology detection is implemented as a step-wise procedure with prepocessing and analysis steps as described in Forkel et al. (2015). The steps are:</p>
<ol>
<li>Filling of permanent gaps in time series</li>
<li>Time series smoothing and interpolation</li>
<li>Detection of phenology events</li>
</ol>

<p>The corresponding functions to these steps can be used separately or the entire step-wise procedure can be applied just by using the main function <b><a href="man/Phenology.html">Phenology</a></b> for time series and <b><a href="man/PhenologyRaster.html">PhenologyRaster</a></b> for multi-temporal raster data. </p>

<p>The function <b><a href="man/Seasonality.html">Seasonality</a></b> is used during several steps to check if the time series has seasonality. Otherwise, the calculation of most phenology metrics like SOS and EOS is not useful.</p>

<h3>Main functions: Phenology and PhenologyRaster</h3>

<p>The main functions for phenology detection that cover these three steps are <b><a href="man/Phenology.html">Phenology</a></b> for time series and <b><a href="man/PhenologyRaster.html">PhenologyRaster</a></b> for multi-temporal raster data. These two functions encapsulate several other functions that are needed during the different steps and that are explained below.</p>

<blockquote>
> # load example NDVI time series<br>
> data(ndvi)<br>
> plot(ndvi)<br>
<br>
> # compute phenology metrics<br>
> spl.trs <- Phenology(ndvi, tsgf="TSGFspline", approach="White") <br>
> spl.trs<br>
--- Phenology ---------------------------- <br>
Calculate phenology metrics on time series <br>
------------------------------------------ <br>
<br>
Method <br>
 - smoothing and gap filling :  TSGFspline <br>
 - summary approach          :  White <br>
<br>
Mean and standard deviation of annual metrics: <br>
SOS    :  86 +- 11 <br>
EOS    :  280 +- 15 <br>
LOS    :  190 +- 21 <br>
MSP    :  0.27 +- 0.019 <br>
MAU    :  0.27 +- 0.019 <br>
RSP    :  NaN +- NA <br>
RAU    :  NaN +- NA <br>
POP    :  180 +- 10 <br>
POT    :  0.37 +- 1.3 <br>
MGS    :  0.3 +- 0.021 <br>
PEAK   :  0.32 +- 0.022 <br>
TROUGH :  0.23 +- 0.018<br>
<br>
> plot(spl.trs)
</blockquote>

<p><img src="fig_Phenology.png"><br>
Fig. 1: Annual time series of the the start of growing season (sos), end of growing season (eos) and position of peak day (pop).</p>

<p>The phenology analysis can be easily applied to raster datasets with <b><a href="man/PhenologyRaster.html">PhenologyRaster</a></b>.</p>

<blockquote>
> # load example NDVI time series<br>
> data(ndvimap)<br>
> phenmap <- PhenologyRaster(ndvimap, start=c(1982, 1), freq=12, tsgf="TSGFspline", approach="Deriv") <br>
> plot(phenmap, c(grep("SOS.1982", names(phenmap)), grep("EOS.1982", names(phenmap))))
</blockquote>

<p><img src="fig_PhenologyRaster.png"><br>
Fig. 2: Maps of start and end of the growing season in 1982 (in day of year).</p>


<h3>Step 1 - Filling of permanent gaps in time series: FillPermanentGap</h3>

<p>Permanent gaps in time series are missing values that occur regulary every year or in many years. Permanent gaps in satellite-derived vegetation index time series are for example the winter months in northern regions where reliable observation are not possible because of high sun zenit angles or snow cover. Such gaps can be identified using the function <b><a href="man/IsPermanentGap.html">IsPermanentGap</a></b> and these gaps can be filled with <b><a href="man/FillPermanentGaps.html">FillPermanentGaps</a></b>.</p>

<blockquote>
> data(ndvi)<br>
<br>
> # set NA values into winter months to simulate gaps<br>
> winter <- (1:length(ndvi))[cycle(ndvi) == 1 | cycle(ndvi) == 2 | cycle(ndvi) == 12]<br>
> gaps <- sample(winter, length(winter)*0.3)<br>
> ndvi2 <- ndvi<br>
> ndvi2[gaps] <- NA<br>
> plot(ndvi2)<br>
<br>
> # check and fill permanent gaps<br>
> IsPermanentGap(ndvi2)<br>
> fill <- FillPermanentGaps(ndvi2) # default fills with the minimum value<br>	
> plot(fill, col="red"); lines(ndvi)
</blockquote>


<h3>Step 2 - Temporal smoothing and gap filling: TsPP</h3>

<p>Several functions are implemented to smooth, to gap-fill and interpolate time series to daily time steps:<p>
<ul>
<li><b><a href="man/TSGFlinear.html">TSGFlinear</a></b>: linear interpolation and running median</li>
<li><b><a href="man/TSGFspline.html">TSGFspline</a></b>: spline smoothing and interpolation</li>
<li><b><a href="man/TSGFssa.html">TSGFssa</a></b>: singular spectrum analysis </li>
<li><b><a href="man/TSGFdoublelog.html">TSGFdoublelog</a></b>: fitting of a double-logistic function to the data of each year. Thereby two types of double-logistic function can be currently used: <b><a href="man/FitDoubleLogBeck.html">FitDoubleLogBeck</a></b> and <b><a href="man/FitDoubleLogElmore.html">FitDoubleLogElmore</a></b></li>
</ul>

<p>The functions of steps 1 and 2 are both encapsulated by the function <b><a href="man/TsPP.html">TsPP</a></b> (time series preprocessing).</p>

<blockquote>
> data(ndvi) <br>
<br>
> # introduce random gaps <br>
> gaps <- ndvi<br>
> gaps[runif(100, 1, length(ndvi))] <- NA<br>
<br>
> # smoothing and gap filling<br>
> tsgf <- TSGFspline(gaps)<br>
> plot(gaps, main="Temporal smoothing and gap-filling using splines")<br>
> lines(tsgf, col="red")<br>
</blockquote>

<p><img src="fig_TSGFspline.png"><br>
Fig. 3: Original time series with gaps (black) and smoothed and interpolated time series (red).</p>


<h3>Step 3 - Detection of phenology events: PhenoDeriv and PhenoTrs</h3>

<p>In the last step, phenology events like the start of growing season (SOS) and end of growing season (EOS) are estimated from the daily interpolated, gap-filled and smoothed time series. This can be done either by using thresholds (<b><a href="man/PhenoTrs.html">PhenoTrs</a></b>) or by extreme values of the derivative of the seasonal cycle (<b><a href="man/PhenoDeriv.html">PhenoDeriv</a></b>). <b><a href="man/PhenoTrs.html">PhenoTrs</a></b> implements thereby two approaches to estimate phenology events. <p> 

<blockquote>
> data(ndvi)<br>
<br>
> # time series pre-processing<br>
> x <- TsPP(ndvi, interpolate=TRUE)[1:365]<br>
<br>
> # calculate phenology metrics for first year<br>
> PhenoDeriv(x, plot=TRUE)
</blockquote>

<p><img src="fig_PhenoDeriv.png"><br>
Fig. 4: Seasonal cycle of a single year with estimated PGMs (phenology and greenness metrics).</p>


<br>
<hr>
<p><a href="index.php">greenbrown</a>, Matthias Forkel, mforkel [at] bgc-jena.mpg.de, 2015-04-15</p>

</body>
</html>
