
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
	b { font-family:Courier; font-weight:normal; }
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

<h1>Currently, the full functionality of this package will be made availabe during March 2013.</h1> 

<p>'Greening' describes in the earth observation community positive trends in vegetation greenness. Vegetation greenness can be expressed as Normalized Difference Vegetation Index (NDVI) that is related to the coverage of green vegetation, photosynthetic activity and green biomass. Greening trends were observed worldwide based on satellite observations of the last 30 years (1980-2012). On the other hand, some regions have negative trends in vegetation greenness ('browning'). The package 'greenbrown' provides access to different methods to analyze such greening and browning trends and trend changes in raster datasets like from satellite observations. A description, comparison and application of these methods for trend and trend change analysis can be found in Forkel et al. (2013).</p>


<h3>Installation and loading</h3>
<p>The package greenbrown was developed and tested under the R version 2.15.3.</p>
<p>To install the most recent version of greenbrown type directly within R: <br><b>install.packages("greenbrown", repos="http://R-Forge.R-project.org")</b></p>
<p>If you want to use greenbrown, you have to load the package at the beginning of each R session with: <br><b>library(greenbrown)</b></p> 


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

<h3>References</h3>
<p>Forkel, M., N. Carvalhais, J. Verbesselt, M. Mahecha, C. Neigh and M. Reichstein (2013): Trend Change Detection in NDVI Time Series: Effects of Inter-Annual Variability and Methodology. - Remote Sensing, submitted.</p>


<h3>Contact</h3>
<p>Matthias Forkel, mforkel [at] bgc-jena.mpg.de </p>


<p> The <strong>greenbrown developper summary page</strong> you can find <a href="http://<?php echo $domain; ?>/projects/<?php echo $group_name; ?>/"><strong>here</strong></a>. </p>

</body>
</html>
