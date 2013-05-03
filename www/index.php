
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
	<style type="text/css">
	b { font-family:Courier New; font-weight:normal; color:#0000DD; padding:4px }
	h1, h2, h3, p, li { font-family:Arial; font-weight:normal; }
	</style>
  </head>

<body>

<!-- R-Forge Logo -->
<table border="0" width="100%" cellspacing="0" cellpadding="0">
<tr><td>
<a href="http://r-forge.r-project.org/"><img src="http://<?php echo $themeroot; ?>/imagesrf/logo.png" border="0" alt="R-Forge Logo" /> </a> </td> </tr>
</table>


<!-- get project title  -->
<!-- own website starts here, the following may be changed as you like -->

<?php if ($handle=fopen('http://'.$domain.'/export/projtitl.php?group_name='.$group_name,'r')){
$contents = '';
while (!feof($handle)) {
	$contents .= fread($handle, 8192);
}
fclose($handle);
echo $contents; } ?>

<!-- end of project description -->

<p>'Greening' describes in the earth observation community positive trends in vegetation greenness. Vegetation greenness can be expressed as Normalized Difference Vegetation Index (NDVI) that is related to the coverage of green vegetation, photosynthetic activity and green biomass. Greening trends were observed worldwide based on satellite observations of the last 30 years (1980-2012). On the other hand, some regions have negative trends in vegetation greenness ('browning'). The package 'greenbrown' provides access to different methods to analyze such greening and browning trends and trend changes in raster datasets like from satellite observations. A description, comparison and application of these methods for trend and trend change analysis can be found in Forkel et al. (2013).</p>

<hr>

<h3>Installation and loading</h3>
<p>The package greenbrown was developed and tested under the R version 2.15.3. To install the package directly from Rforge you need the most recent R version 3.0.</p>
<p>To install the most recent version of greenbrown type directly within R: <br><b>install.packages("greenbrown", repos="http://R-Forge.R-project.org")</b></p>
<p>If you want to use greenbrown, you have to load the package at the beginning of each R session with: <br><b>library(greenbrown)</b></p> 

<hr>

<h3>Using the greenbrown package</h3>

<p>The main function to calculate trends and breakpoints on single time series is <b>Trend</b>. Type <b>?Trend</b> for the help page with a description of the parameters and examples. The function <b>Trend</b> offers a common access to different methods for trend analysis. Currently, the following methods are implemented in the greenbrown package:</p>
<ul>
<li><b>TrendAAT</b> computes trends on annual aggregated time series.</li> 
<li><b>TrendSTM</b> computes trends based on a season-trend model.</li>
<li><b>TrendSeasonalAdjusted</b> removes first the seasonal cycle from a time series and computes then the trend on the seasonal-adjusted time series.</li>
</ul>

<p>With the function <b>TrendRaster</b> these trend estimation methods can be applied on gridded spatial raster datasets like from earth observation data or climate model simulations. This function applies the function <b>Trend</b> on multilayer raster stacks (called RasterBricks in R). Type <b>?TrendRaster</b> for examples how to use trend analysis on raster data.</p>

<p>To read, write and process your raster datasets in R, please see the <a href="http://cran.r-project.org/web/packages/raster/" target="_blank">raster</a> package for more information. To read a multilayer raster dataset from a file, you will have to use the function <b>brick</b> (see <b>?brick</b> for help).</p> 

<p>To estimate statistical properties of time series in a raster dataset (like trend slope, seasonality, inter-annual and short-term variability), use the function <b>GetTsStatisticsRaster</b> (see <b>?GetTsStatisticsRaster</b> for help).</p> 

<p>The package provides further functions to simulate artifical (or surrogate) time series. Based on observed statistical properties of time series, artifical time series can be created in order to test different trend methods if they are able to re-detect the prescribed trend of the artifical time series. To create an artificial time series, use <b>SimTs</b>.</p> 

<p>An example how to use this package is given at lower section of this website.</p>

<hr>

<h3>References</h3>
<p>Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): <a href="http://www.mdpi.com/2072-4292/5/5/2113" target="_blank">Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology.</a> - Remote Sensing 5, 2113-2144.</p>

<hr>

<h3>Contact</h3>
<p>Matthias Forkel, mforkel [at] bgc-jena.mpg.de, Max Planck Institute for Biogeochemistry Jena, Germany </p>

<hr>

<h3>Example</h3>

<p>The following code provides an example how you can use the main functions of the greenbrown package. But please consult also the help pages of each function (e.g. type <b>?Trend</b> within R) for further informations on methods and parameters.</p>

<b>
# load the package<br>
library(greenbrown)<br>
<br>
<br>
# 1. calculate trends on single time series<br>
<br>
# load a time series of NDVI (normalized difference vegetation index)<br>
data(ndvi) # load the time series<br>
plot(ndvi) # plot the time series<br>
?ndvi # some information about the data<br>
<br>
# open the help page for the main function Trend<br>
?Trend<br>
<br>
# calculate trend (default method: trend calculated based on mean annual data)<br>
trd <- Trend(ndvi)<br>
trd<br>
plot(trd)<br>
<br>
# calculate trend (default method but detect breakpoints)<br>
trd <- Trend(ndvi, mosum.pval=1)<br>
trd<br>
plot(trd) # this line will produce figure 1:<br>
</b>

<p><img src="figure1.png"><br>
Fig. 1: Trend in mean annual NDVI from an example grid cell in central Alaska.</p>


<p>
<b>
# 2. calculate trends on raster data sets<br>
<br>
# load a multi-temporal raster dataset of Normalized Difference Vegetation Index<br>
data(ndvimap) # load the data<br>
ndvimap # some information about the data<br>
?ndvimap # some more information about the data<br>
plot(ndvimap, 8, col=brgr.colors(50))<br>
<br>
# Note: you can load your own raster data with the function 'brick' - see ?brick for help<br>
<br>
# see the help page for further information about how to apply Trend methods on raster data:<br>
?TrendRaster <br>
<br>
# calculate trend on the raster dataset using annual maximum NDVI<br>
AATmap <- TrendRaster(ndvimap, start=c(1982, 1), freq=12, method="AAT", breaks=2, funAnnual=max)<br>
plot(AATmap, col=brgr.colors(20), legend.width=2) # this line will produce figure 2:<br>
</b>

<p><img src="figure2.png"><br>
Fig. 2: Trends on annual maximum NDVI with detected breakpoints (BP, year of breakpoint), length of the time series segments (LengthSEG, in years), slope of the trend in each segment (SlopeSEG) and p-value of the trend in each segment (Pval).</p>

<hr>

<p> The <strong>greenbrown developper summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
