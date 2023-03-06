---
title: "Computing More Accurate Autosal Salinities by Accounting for Instrument Drift"
author: "Jeff Jackson"
format:
  html:
    keep-md: true
editor: visual
engine: knitr
---



## Autosal Salinity Background

On hydrographic research missions, often water samples are collected to analyze water samples for chemical components and biological production. One of the most common chemical components measured is salinity. Historically salinity was defined as practical salinity.

> *"The current definition for Practical Salinity states: a seawater of Practical Salinity 35 has a conductivity ratio of unity at 15 degrees Centigrade (and 1 atmosphere pressure) with a potassium chloride (KCl) solution containing a mass of 32.4356 grams of KCl per kilogram of solution."*\
> *This excerpt was taken from an Ocean Scientific International Ltd. (OSIL) document entitled [IAPSO Standard Seawater and the Practical Salinity Scale](https://osil.com/content/uploads/2020/11/IAPSO-Standard-Seawater-and-the-Practical-Salinity-Scale.pdf)*

Water samples collected for the determination of Practical Salinity have primarily been analyzed at sea (optimal method; but can be done back at the office) using a [Guildline Instruments](https://www.guildline.ca) [8400B Autosal Laboratory Salinometer](https://www.guildline.ca/oceanography/salinometers/salinity-measurement/8400b-Autosal-lab-salinometer). The salinometer measures the ratio of the conductivity of a water sample to an internal reference. The salinometer output corresponds to two times the conductivity ratio of the water sample to the reference. The least count of the two times conductivity ratio output is 0.00001, corresponding to a salinity precision of approximately 0.0002.

The internal reference is preset to a known external reference. The external reference used is a [IAPSO](https://iapso-ocean.org/) Standard Seawater provided by [OSIL](https://osil.com/salinity-measurement-standards/). Each batch of IAPSO Standard Seawater is created to have a practical salinity very close to 35 and has a batch-specific certified conductivity ratio (K15) relative to the conductivity of a standard KCl solution as described above. For example, mission HUD2004016 used Standard Seawater from Batch P141 with a certified K15 value 0.99993. @fig-iapso below shows a Standard Seawater from batch P165 sitting in front of a 8400B Autosal Lab salinometer.


::: {.cell layout-align="left"}
::: {.cell-output-display}
![IAPSO Standard Seawater Ampoule from Batch P165](Documentation/IAPSO_Seawater_Standard_P165.png){#fig-iapso fig-align='left' fig-alt='A picture of an IAPSO Standard Seawater Ampoule.' width=5.0in}
:::
:::


The World Ocean Circulation Experiment (WOCE) Hydrographic Programme One-time WHP Standards for Water Samples (Joyce, 1991) states that salinity precision better than 0.001 and accuracy of 0.002 are required for WOCE. Joyce (1991) reports that these requirements can be achieved on a routine basis with the Autosal Salinometer, if it is operated with great care and experience with regular monitoring with Standard Seawater and close laboratory temperature control.

::: callout-note
Please note that the [WOCE manual](https://cchdo.ucsd.edu/policy) has been superseded by the [GO-SHIP Repeat Hydrography Manual](https://www.go-ship.org/HydroMan.html).
:::

Salinometer standardization is accomplished by measuring the conductivity ratio of a Standard Seawater sample and adjusting a vernier potentiometer to make the salinometer output agree with the certified conductivity ratio of the Standard Seawater. Periodic standardizations of the salinometer are carried out to monitor changes in the internal reference of the instrument. Corrections can then be applied to the measured conductivity ratios before the calculation of salinity using the PSS78 equations. Formally, a multiplicative correction ratio should be applied, but the ratio is so close to unity that a first-order additive correction is sufficient. These additive corrections are known as drift corrections. Software algorithms taking account of the drift corrections provide the final bottle salinity values.

In a typical mission situation, water samples from several stations are analyzed during a single session. The salinometer operator begins by analyzing a Standard Seawater sample, then a set of water samples, and finally a second Standard Seawater sample. The beginning and ending standards and the intervening water samples constitute an analysis run. The 402 bottle salinity samples analyzed for mission HUD2004016 were divided among 10 such runs.

Although the salinometer can in principle be adjusted to reproduce the exact conductivity ratio of the Standard Seawater, it is our practice to minimize such adjustments to preserve system stability. Even when adjustments are made, it is not practical to adjust the salinometer to agree exactly with the certified K15 value. In general a non-zero drift correction will need to be applied to all samples.

For mission HUD2004016, the initial standard gave a two times conductivity value of 1.99990. This was 0.00004 greater than the doubled K15 value (1.99986) for the P141 Standard Seawater used. This corresponds to a salinity offset of approximately 0.0008. Following the initial standardization for mission HUD2004016, the salinometer operator adjusted the reference point of the salinometer on two occasions. Both of these adjustments gave the same two times conductivity ratio reading of 1.99983 for a difference of -0.00003 relative to the certified K15 value of the Standard water. The cumulative two times conductivity ratio drift during the mission relative to the initial value ranged from -0.00006 to 0.00010, corresponding to a range in salinity change of approximately 0.0028.







<br>

Table 1: First 10 rows of the file containing Autosal measured conductivity ratios including the HUD2010049 values.


::: {.cell}
::: {.cell-output-display}

`````{=html}
<table class="table table" style="font-size: 18px; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;"> SampleID </th>
   <th style="text-align:center;font-weight: bold;"> CRATx2 </th>
   <th style="text-align:center;font-weight: bold;"> Notes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;width: 10em; "> P151 </td>
   <td style="text-align:center;width: 10em; "> 1.99998 </td>
   <td style="text-align:center;width: 30em; "> K15 = 0.99997
K15*2 = 1.99994
Sal. = 34.999 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306701 </td>
   <td style="text-align:center;width: 10em; "> 1.96386 </td>
   <td style="text-align:center;width: 30em; "> HL_02 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306705 </td>
   <td style="text-align:center;width: 10em; "> 1.81857 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306710 </td>
   <td style="text-align:center;width: 10em; "> 1.78498 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306711 </td>
   <td style="text-align:center;width: 10em; "> 1.95463 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306715 </td>
   <td style="text-align:center;width: 10em; "> 1.76956 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 306720 </td>
   <td style="text-align:center;width: 10em; "> 1.76951 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 368451 </td>
   <td style="text-align:center;width: 10em; "> 1.83716 </td>
   <td style="text-align:center;width: 30em; "> SHEDIAC </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 368454 </td>
   <td style="text-align:center;width: 10em; "> 1.77478 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 368457 </td>
   <td style="text-align:center;width: 10em; "> 1.72929 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
</tbody>
</table>

`````

:::
:::


<br> <br>

Table 2: First 10 rows of HUD2010049 Autosal measured conductivity ratios.


::: {.cell}
::: {.cell-output-display}

`````{=html}
<table class="table table" style="font-size: 18px; width: auto !important; margin-left: auto; margin-right: auto; margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;"> SampleID </th>
   <th style="text-align:center;font-weight: bold;"> CRATx2 </th>
   <th style="text-align:center;font-weight: bold;"> Notes </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;width: 10em; "> 374251 </td>
   <td style="text-align:center;width: 10em; "> 1.96386 </td>
   <td style="text-align:center;width: 30em; "> Start of HUD2010049 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374255 </td>
   <td style="text-align:center;width: 10em; "> 1.78901 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374260 </td>
   <td style="text-align:center;width: 10em; "> 1.77816 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374275 </td>
   <td style="text-align:center;width: 10em; "> 1.99481 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374276 </td>
   <td style="text-align:center;width: 10em; "> 1.99565 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374277 </td>
   <td style="text-align:center;width: 10em; "> 1.99585 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374278 </td>
   <td style="text-align:center;width: 10em; "> 1.99633 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374279 </td>
   <td style="text-align:center;width: 10em; "> 1.99670 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374280 </td>
   <td style="text-align:center;width: 10em; "> 1.99680 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374281 </td>
   <td style="text-align:center;width: 10em; "> 1.99712 </td>
   <td style="text-align:center;width: 30em; ">  </td>
  </tr>
</tbody>
</table>

`````

:::
:::


<br> <br>

Table 3: First 10 rows of HUD2010049 computed salinities of unknown origin. <!-- @ref(tab:salt-table) -->


::: {.cell}
::: {.cell-output-display}

`````{=html}
<table class="table table" style="font-size: 18px; width: auto !important; margin-right: 0; margin-left: auto margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;"> SampleID </th>
   <th style="text-align:center;font-weight: bold;"> CalculatedSalinity </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;width: 10em; "> 374251 </td>
   <td style="text-align:center;width: 10em; "> 34.288 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374255 </td>
   <td style="text-align:center;width: 10em; "> 30.885 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374260 </td>
   <td style="text-align:center;width: 10em; "> 30.676 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374275 </td>
   <td style="text-align:center;width: 10em; "> 34.896 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374276 </td>
   <td style="text-align:center;width: 10em; "> 34.912 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374277 </td>
   <td style="text-align:center;width: 10em; "> 34.916 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374278 </td>
   <td style="text-align:center;width: 10em; "> 34.925 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374279 </td>
   <td style="text-align:center;width: 10em; "> 34.933 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374280 </td>
   <td style="text-align:center;width: 10em; "> 34.935 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374281 </td>
   <td style="text-align:center;width: 10em; "> 34.941 </td>
  </tr>
</tbody>
</table>

`````

:::
:::


In order to produce the best salts for a data set, it is important that instrument drift be taken into account by adjusting the salts for this drift.

The salinity computation follows the formula from UNESCO (see [Fofonoff and Millard, 1983](https://repository.oceanbestpractices.org/handle/11329/109)). The input conductivity from the Autosal must be the actual Autosal reading (i.e. twice the conductivity ratio). The sample conductivity is then adjusted with an offset and a drift term according to the following:

-   $C_{R}$ - Autosal sample conductivity ratio
-   $C_{S}$ - Autosal Standard Seawater conductivity ratio at the start of the run
-   $C_{E}$ - Autosal Standard Seawater conductivity ratio at the end of the run
-   $V$ - Standard Seawater conductivity (K15) from ampoule label
-   $S$ - current record position for sample conductivity ratio
-   $N$ - number of records in the sample conductivity run
-   $R_{t}$ - see UNESCO report
-   $r_{t}$ - see UNESCO report
-   $R_{p}$ - see UNESCO report

$$
C_{R}\ =\ C_{R}\ +\ (V\ -\ C_{S}) - \frac{(C_{E}\ - \ C_{S}) \times S}{N\ +\ 1}
$$

where: $$
  V\ -\ C_{S}
  $$ is the offset and\
$$
  \frac{(C_{E}\ -\ C_{S})\ *\ S}{N\ +\ 1}
  $$ is the drift.

$C_{R}$ is then adjusted to produce a true conductivity ratio relative to C at standard salinity, temperature, and pressure (i.e. s = 35, t = 15, p = 0).

$R_{t}$ is computed and used in the standard formula to compute salinity.

$$
C_{R}\ =\ C_{R}\ *\ \frac{ C_{35,t,0}}{C_{35,15,0}}
$$

$$
R_t\ =\ \frac{C_R}{(r_t \times R_p)}
$$ <br> <br>

The R function **compute_salts.R** was written based off an old MATLAB script. It computes the practical salinity values adjusted for instrument drift from analyzed Standard Seawater ampoule and water sample conductivity ratios.


::: {.cell file='compute_salts.R'}

```{.r .cell-code}
#' @title compute_salts.R
#' @author Jeff Jackson
#' 
#' @param ids An array of sample id numbers
#' @param autosal An array of water sample conductivity ratios obtained from a 
#' 8400B Autosal Lab salinomter.
#' @param batch A string containing the IAPSO Standard Seawater batch identifier
#' @returns A data frame containing the ids and drift corrected salinities 
#' (a.k.a. salts).
#'   
#' @note Report any bugs to DataServicesDonnees@dfo-mpo.gc.ca
#' 
#' @examples
#' compute_salts(sampleIDs, CRATx2, 'P151')
#' 
#' @details 
#' @source Ocean Data and Information Services, Bedford Institute of 
#' Oceanography, DFO, Canada.
#' 
#' Created: 03-MAR-2023
#' Last Updated: 03-MAR-2023
#' 
#' You may distribute under the terms of either the GNU General Public
#' License or the Apache v2 License, as specified in the README file.

library(gsw)
library(oce)
library(tibble)

compute_salts <- function(ids, autosal, batch) {
  
  output_df <- tibble(ids, autosal)
  
  names(output_df)[2] <- "drift_corrected_salts"

  batchIds <- which(ids == batch)
  nBatchIds <- length(batchIds)
  
  # Loop through the sections of conductivity ratios to compute salts that are 
  # adjusted by the drifts.
  for (i in 1:(nBatchIds - 1)) {

    # Get the Standard Seawater values that form the endpoints for the current 
    # section.
    b1 <- batchIds[i]
    b2 <- batchIds[i+1]
    
    standard1 <- autosal[b1]
    standard2 <- autosal[b2]
    
    total_drift <- standard2 - standard1
    
    n <- b2 - b1
    s <- 1:n
    
    # Compute the salts
    salts <- gsw_SP_salinometer(autosal[(b1+1):b2]/2, 24)
    
    # Calculate the drift over the section
    drift <- (total_drift / n) * s
    
    # Compute the drift corrected salts
    corrected_salts <- salts + drift
  
    output_df$drift_corrected_salts[(b1+1):b2] <- corrected_salts
    
  }
  
  # Remove the Standard Seawater rows from the output data frame
  output_df <- output_df[-batchIds,]
  
  return (output_df)
}
```
:::

::: {.cell}

:::


<!-- Modify the table properties for the corrected salts table -->



```{=html}
<style>
table {
  background-color: white !important;
  color: black !important;
}
</style>
```


Table 2: First 10 rows of HUD2010049 computed salinities. <!-- @ref(tab:salt-table) -->


::: {.cell}
::: {.cell-output-display}

`````{=html}
<table class="table table" style="font-size: 18px; width: auto !important; margin-right: 0; margin-left: auto margin-left: auto; margin-right: auto;">
 <thead>
  <tr>
   <th style="text-align:center;font-weight: bold;"> ids </th>
   <th style="text-align:center;font-weight: bold;"> drift_corrected_salts </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:center;width: 10em; "> 374251 </td>
   <td style="text-align:center;width: 10em; "> 34.28993 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374255 </td>
   <td style="text-align:center;width: 10em; "> 30.88720 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374260 </td>
   <td style="text-align:center;width: 10em; "> 30.67784 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374275 </td>
   <td style="text-align:center;width: 10em; "> 34.89790 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374276 </td>
   <td style="text-align:center;width: 10em; "> 34.91443 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374277 </td>
   <td style="text-align:center;width: 10em; "> 34.91836 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374278 </td>
   <td style="text-align:center;width: 10em; "> 34.92781 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374279 </td>
   <td style="text-align:center;width: 10em; "> 34.93509 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374280 </td>
   <td style="text-align:center;width: 10em; "> 34.93706 </td>
  </tr>
  <tr>
   <td style="text-align:center;width: 10em; "> 374281 </td>
   <td style="text-align:center;width: 10em; "> 34.94335 </td>
  </tr>
</tbody>
</table>

`````

:::
:::
