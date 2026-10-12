# MiltSim  
**Miltefosine Dose Optimization and Population Analysis**  

## Overview  
Miltsim is a Shiny application designed to optimize Miltefosine dosing and perform population-level pharmacokinetic and pharmacodynamic (PK/PD) analyses. The interactive interface allows users to:  

- Enter patient-specific data  
- Visualize dose-response relationships  
- Simulate various dosing regimens  
- Analyze population-level PK/PD data to identify trends and improve treatment strategies  

## Features  

- **Patient Data Input**: Enter key patient details such as age, weight, and height.  
- **Dose-Response Visualization**: Generate interactive plots to observe how different doses impact patient outcomes.  
- **Dosing Simulations**: Test and compare various dosing regimens to determine the most effective treatment for different patient groups.  
- **Population Analysis**: Examine aggregated PK/PD data to optimize dosing strategies at a broader level.  

## Getting Started  

Follow these steps to run the Shiny application:  

1. Install **R** and the **Shiny** package if not already installed.  
2. Clone this repository to your local machine:  

   ```sh  
   git clone https://github.com/QusaiASamara/Miltsimapp.git  
   cd Miltsim  
   ```  
3. Open the R project file in RStudio.  
4. Install required dependencies (see below).  
5. Launch the Shiny application using the following command in the R console:  

   ```r  
   shiny::runApp('path/to/shiny/app')  
   ```  

   *(Replace `'path/to/shiny/app'` with the actual directory path of the application.)*  

## Dependencies  

The required R packages are listed in the [`global.R`](https://github.com/QusaiASamara/Miltsim/blob/main/global.R) file. Install them before running the application:  

```r  
install.packages(c("shiny", "ggplot2", "dplyr", "tidyr", "shinyWidgets", "DT"))  
```  

*(Modify the package list as needed based on `global.R`.)*  

## Contributing  

We welcome contributions! To contribute:  

1. Fork this repository.  
2. Create a new branch for your changes.  
3. Submit a pull request with a clear description of your improvements.  

For any questions, feel free to open an issue.  
