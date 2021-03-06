# Global malaria infection risk from climate change
This repo (DP13) is used to detect the relationship between malaria and climate change.  
  
## Abstract:  

As a long-standing public health issue, malaria still severely hits many parts of this world, especially Africa. With greenhouse gas emissions, temperatures keep rising. Based on diverse shared socio-economic pathways (SSPs), the future temperatures are estimated. However, the impacts of climate change on the malaria infection rate in all epidemic regions are unknown. Here, we estimate the differences in the predicted malaria infection rate globally under different SSPs during several periods and malaria infection case changes (MICCs) due to those differences. Our results indicate that the globally MICCs from SSP1-2.6 to SSP2-4.5, to SSP3-7.0, and to SSP5-8.5 are 6.506 (95% uncertainty interval [UI] 6.150 – 6.861) million, 3.655 (3.416 – 3.894) million, and 2.823 (2.635 – 3.012) million during 2021 – 2040, respectively, which are 2.699%, 1.517%, and 1.171% increase, compared with 241 million infection cases in 2020. The temperature increase adversely affects malaria the most in Africa during 2021 – 2040. During 2081 – 2100, the MIICs of three scenario shifts are -79.109 (-83.626 - -74.591) million, -238.337 (-251.920 - -0.141) million, and -162.692 (-174.628 - -150.757) million, which are -32.825%, -98.895%, and -67.507% increase. Climate change would increase the danger and risk of malaria in the most vulnerable regions in the near term, which aggravates the difficulty of eliminating malaria. GHG emissions reduction is a potential pathway to protect people from malaria.  
  
## Author  
Chao Li, Shunsuke Managi  

## Result: The Grid-level Malaria Infection Case Changes of Three Scenario Shifts in Different Periods    
![](06_Figure/S41_ICC_multiPanel.jpg)  

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
**[05_AF_GWPRRevisedForCrossValidation_v1.R](01_RCode/05_AF_GWPRRevisedForCrossValidation_v1.R)**: This script revises the function in GWPR.light to complete 10-fold CV.  
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
