# Malaria_And_Climate_Change
This repo (DP13) is used to detect the relationship between malaria and climate change.  
  
## Summary:  

###Background:  

As a long-standing public health issue, malaria still severely hits many parts of this world, especially Africa. With greenhouse gas emissions, temperatures keep rising. We aim to quantify the impacts of global warming on the malaria infection rate in all epidemic regions and identify the most vulnerable areas.  
  
###Methods:  

We estimate the coefficients of relationships among variables by geographically weighted panel regression. Four scenarios based on diverse shared socio-economic pathways (SSPs) are employed, including SSP1-2.6, SSP2-4.6, SSP3-7.0, and SSP5-8.5. We estimate the difference between the predicted PfPR2-10 globally under different SSP scenarios during several periods.  
  
###Findings:  

The globally average difference of PfPR2-10 between SSP2-4.6 and SSP1-2.6 is 0.164% (95% uncertainty interal [UI] 0.160% - 0.168%) during 2021 – 2040, while the differences between SSP3-7.0 and SSP1-2.6 and between SSP5-8.5 and SSP1-2.6 are 0.104% (0.101% - 0.107%) and 0.041 (0.038% - 0.044%), respectively. The global average differences of PfPR2-10 of three scenario shifts during 2041 – 2060 and 2081 – 2100 are -0.181% (-0.185% - -0.177%), -0.482% (-0.493% - -0.472%), -0.730% (-0.762% - -0.730%), 1.287% (-1.315% - -1.260%), -3.036% (-3.105% - -2.967%), and -4.096% (-4.190% - -4.002%), respectively. Moreover, the increase in temperature adversely affects malaria the most in Africa during 2021 – 2040, where is most severely hit by malaria.  
  
###Interpretation:   
Global warming would increase the danger and risk of malaria in the most vulnerable regions in the near term, which aggravates the difficulty of eliminating malaria. GHG emissions reduction is a potential pathway to protect the people from malaria.  
  
## Author  
Chao Li, Shunsuke Managi  

## Result: The Grid-level Difference of PfPR2-10 between Different Scenarios in Different Periods  
![](06_Figure/S22_multiPanel.jpg)  

## Maunscript  
[Global malaria infection risk from global warming](07_Manuscript/Manuscript_DP13.pdf)  
[Supplementary Materials](07_Manuscript/Materials.pdf)  

## Data
### 01_dataset_used.RData  
**Note:** in this repo, we only upload the aggregated data set. In the seperated data sets, the size is over 100MB. To save the storage, we did not upload them. Therefore, in the section, we focus on the description of [01_dataset_used.RData](04_Data/01_dataset_used.RData).  
**[01_dataset_used.RData](04_Data/01_dataset_used.RData):**  
**PfPR:** Plasmodium falciparum parasite rate, range 0 ~ 1. The data is download from <https://malariaatlas.org/explorer/#/>.  
**NDVIMean:** NDVI value -100% ~ 100% from M*D13C2.  
**TempMean:** Annually average temperature C. The data is download from <https://disc.gsfc.nasa.gov/datasets/GLDAS_NOAH025_M_2.1/summary>.  
**AirPressureMean:** kPa. 
**HumidityMean:** unit is g/kg  
**PrecipitationMean:** g / (m2 * h)
**WindSpeedMean:** m/s
**PopulationDensity:** cap/km2. The data is download from <https://www.worldpop.org/>.  
**GDPperCap:** country-level USD/Cap.The data is download from  <https://data.worldbank.org/indicator/NY.GDP.PCAP.CD>.  
**TempSd:** Annually standard deviation temperature C.  
**TempSquare:** Annually average temperature square C2.  
  
## Code
### Python Code  
In this project, the python codes are mainly used to process the he5, nc4, or tiff data, since its high speed with GDAL.  
[01_DW_ChangeMalariaDataResolution_v1.py](02_PyCode/01_DW_ChangeMalariaDataResolution_v1.py): This script is to upscale the data, including **"PfPR2-10"**, from 0.0416666666666667 to 0.25.    
[02_DW_ClimateDataExtraction_v1.py](02_PyCode/02_DW_ClimateDataExtraction_v1.py): This script is to extract the data, including **"temperature"**, **"windSpeed"**, **"humidity"** and **"precipitation"**, from **GLDAS_NOAH025_M**.  
[03_DW_ChangePopulationDataResolution_v1.py](02_PyCode/03_DW_ChangePopulationDataResolution_v1.py): This script is to upscale the data, including **"population density"**, from lkm to 0.25 Deg.  
[04_DW_ChangeFutureTempDataResolution_v1.py](02_PyCode/04_DW_ChangeFutureTempDataResolution_v1.py): This script is to upscale the data, the temperature prediction, from 5 min to 0.25 degree. Data are from <https://www.worldclim.org/data/cmip6/cmip6_clim5m.html>. **Aborted, for the we directly use the data from IPCC**.   
  
### R Code  
**[01_DW_GetMalariaAndClimateData_v1.R](01_RCode/01_DW_GetMalariaAndClimateData_v1.R)**: This script is to wash the data to get the data sets for the analysis, including 01_malariaDataframe.RData, 02_NDVIRasterDataset.RData, 03_TempRasterDataset.RData, 04_AirPressureRasterDataset.RData, 05_HumidityRasterDataset.RData, 06_PrecipitationRasterDataset.RData, 07_WindSpeedRasterDataset.RData, 08_PopulationDataset.RData, and 09_incomeDataset.RData. Since most of these data sets are over 100MB, we do not upload them to GitHub.  
**[02_DW_MergeBasicDataset_v1.R](01_RCode/02_DW_MergeBasicDataset_v1.R)**: This script is to get the panel data set with all the control variables. The input data sets are listed in [01_DW_GetMalariaAndClimateData_v1.R](01_RCode/01_DW_GetMalariaAndClimateData_v1.R). The result of this script is the [01_dataset_used.RData](04_Data/01_dataset_used.RData), including "PfPR": Plasmodium falciparum parasite rate, range 0 ~ 1, "NDVIMean": NDVI value -100% ~ 100% from M*D13C2, "TempMean": Annually average temperature C, "AirPressureMean" kPa, "HumidityMean" unit is g/kg, "PrecipitationMean" g / (m2 h), "WindSpeedMean" m/s, "PopulationDensity" cap/km2, "GDPperCap" USD/Cap, "TempSd": Annually standard deviation temperature C, and "TempSquare": Annually average temperature square C2.  
**[03_AN_GWPRbasicAnalysis_v1.R](01_RCode/03_AN_GWPRbasicAnalysis_v1.R)**: This script conducts the analysis based on GWPR. Outputs are the results of GWPR based on the FEM. The GWPR, GWPR_FEM_CV_F_result_425.RData, is the result of GWPR based on FEM, using the **Fixed** distance bandwidth, which is 4.25 degree.  [GWPR_BW_setp_list_0.5_20_0.25.Rdata](05_Results/GWPR_BW_setp_list_0.5_20_0.25.Rdata) is the bandwidth selection process. Bandwidth is selected to be 4.25. 
**[04_AN_GWPRPrediction_v1.R](01_RCode/04_AN_GWPRPrediction_v1.R)**: This script is to predict the difference of PfPR2-10 between two scenarios at the grid level. The results including [prediction.2040.Rdata](05_Results/prediction.2040.Rdata), [prediction.2060.Rdata](05_Results/prediction.2060.Rdata), and [prediction.2100.Rdata](05_Results/prediction.2100.Rdata). All of these data set includes three variables, which are "predictPfPR.245.126", "predictPfPR.460.126", "predictPfPR.585.126". **Note**: here "460" is a typo, it should 370.  
**[05_AF_GWPRRevisedForCrossValidation_v1.R](01_RCode/05_AF_GWPRRevisedForCrossValidation_v1.R)**: This script revises the function in GWPR.light to complete 10-fold CV, used in 
**[06_AN_GWPR10FoldCrossValidation_v0.R](01_RCode/06_AN_GWPR10FoldCrossValidation_v0.R)**: This script performs 10-fold CV. The input data sets are [01_dataset_used.RData](04_Data/01_dataset_used.RData). The output is [femCrossValidation.Rdata](05_Results/femCrossValidation.Rdata).  
**[07_AF_GWPRBandwidthStepSelection_v1.R](01_RCode/07_AF_GWPRBandwidthStepSelection_v1.R)**: This script revises the function in GWPR.light to perform step bandwidth selection, used in [03_AN_GWPRbasicAnalysis_v1.R](01_RCode/03_AN_GWPRbasicAnalysis_v1.R).  
**[08_VI_SupplementaryMaterials_v1.R](01_RCode/08_VI_SupplementaryMaterials_v1.R)**: Visualization for supplementary material, Figure Sxx.  
**[09_VI_ManuscriptFigure_v1.R](01_RCode/09_VI_ManuscriptFigure_v1.R)**: Visualization for main text, Figure xx.  
**[10_VI_MultiPanelFigure_v1.R](01_RCode/10_VI_MultiPanelFigure_v1.R)**: Visualization for main text, MultiPanel figure.   
**[11_AN_SummaryNationalMeanSE_v1.R](01_RCode/11_AN_SummaryNationalMeanSE_v1.R)**: This script is to make the Table 1 in the main text.  
  
## Workflow  
**WF.py: (01, 02, 03) -> END**  
**WF.py.XX**: This step provides the all raster data from NASA or some places else.   
  
**WF.A: 01 -> 02 -> 03 -> 06 -> 04 -> (08, 09, 10, 11) -> END**  
**WF.A.01.02**: To get the data set,  [01_dataset_used.RData](04_Data/01_dataset_used.RData).  
**WF.A.02.03**: This step conducts the analysis using GWPR based on FEM with **Fixed** distance bandwidth.  
**WF.A.03.06**: This step conducts the 10-fold cross validation on the model of GWPR based on FEM.  
**WF.A.06.04**: This step predict the difference between two emission scenarios.  
**WF.A.04.08091011**: This step obtain the final results for the article.  
  
## Contact Us:
- Email: Prof. Shunsuke Managi <managi@doc.kyushu-u.ac.jp>  
- Email: Chao Li <chaoli0394@gmail.com>  
  
## Term of Use:
Authors/funders retain copyright (where applicable) of code on this Github repo. This GitHub repo and its contents herein, including data, link to data source, and analysis code that are intended solely for reproducing the results in the manuscript "Global malaria infection risk from global warming". The analyses rely upon publicly available data from multiple sources, that are often updated without advance notice. We hereby disclaim any and all representations and warranties with respect to the site, including accuracy, fitness for use, and merchantability. By using this site, its content, information, and software you agree to assume all risks associated with your use or transfer of information and/or software. You agree to hold the authors harmless from any claims relating to the use of this site.    
