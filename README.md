# VHE_Metric


## Introduction to the Vulnerable Household Equivalent (VHE) Metric 


Research has so far taken the ‘adult female’ (non-pregnant, non-lactating) as the default comparator household member (Tang et al., 2021). This has a sound basis, given the often higher requirements relative to energy than men, and for the direct benefit that women’s well-being often confers on their children. Here we undertook a household-specific analysis by comparing the apparent dietary micronutrient supply of the whole household to the requirements of the most vulnerable member of that household (VHE) based on greatest critical nutrient density threshold. Results were compared to the prevalence of apparent inadequacy arising from the use of the non-pregnant, non-lactating female requirements (AFE). We utilized the 2019/20 Malawi's Fifth Integrated Household Survey (IHS5) to compare the perfomance of the new VHE metric with the existing AFE metric
This repo.  is a detailed code for creating the new VHE approach, while also calculating the existing AFE metric for comparison. 

Therefore, this study proposes a complementary novel analytical model for estimating individualized dietary intakes from household apparent consumption data called the ‘vulnerable household equivalent (VHE)’ which is sensitive to the demographic structure of populations and nutritional needs of vulnerable groups within households captured in the Household Consumption and Expenditure Survey (HCES). The repository is divided into four sections.

* `script` - R script for calculating the new VHE metric and the existing AFE metric using Malawi's 2019/20 IHS5 data<br>
* `data` - Complementary data relating to the scripts<br>
* `summaries` - summary statistics for comparing the perfomance of the VHE metric with the existing AFE metric<br>
* `supply` - Micronutrient supplies (i.e. vitamin A & zinc) for comparing the VHE metric with the existing AFE metric <br>

Below is a directory tree that outlines the structure of the repository:

```
├─ script/
│  ├─ vuln.calc.R

├─ data/
│  ├─ hh.spec.csv
│  ├─ vuln.energy.req.xlsx                       

├─ Supply/
│  ├─ va.F.Rmd
│  ├─ zn.F.Rmd 

├─ Summaries/
│  ├─ summary.explor.R
│  ├─ summary.vuln.Rmd 

```

## Data sources  
1. This repo. utilizes the already tidied IHS5 data, which can be accessed via the link  * https://doi.org/10.5281/zenodo.13822686 <br>
2. The Malawi's Fifth Integrated Household Survey (IHS5) data can be requested using the link provided below: <br>

* [Malawi Fifth Integrated Household Survey 2019-2020](https://microdata.worldbank.org/index.php/catalog/3818) <br>

Once logged in, please click download on the 'Data in Stata format' option. Once downloaded, please unzip the file, and move it to the 'data' folder of this repository; e.g. "GitHub\ihs5_cleaning_github\data\MWI_2019_IHS-V_v06_M_Stata".
