# Medicaid Policy Impact Microsimulation Model

## Overview

This repository contains a microsimulation model that projects the health system and economic impacts of proposed Medicaid policy changes from 2025 to 2034. The model incorporates empirically-derived parameters from peer-reviewed literature and authoritative data sources to ensure scientific validity and reproducibility.

## Authors

- Sanjay Basu, MD, PhD - University of California San Francisco & Waymark Care
- Sadiq Patel, MSW, PhD - University of Pennsylvania & Waymark Care
- Seth A. Berkowitz, MD, MPH - University of North Carolina

## Repository Contents

- `medicaid_impact_model_updated_scenarios.R`: R script containing the microsimulation model with both base case (CBO) and higher impact (CBPP) scenarios
- `final_main_text_updated_scenarios.md`: Main text of the research paper with results and discussion
- `final_appendix_updated_scenarios.md`: Detailed appendix with methodological details, parameter sources, and supplementary tables
- `base_case/figures/`: Visualizations for the base case scenario 
- `higher_impact/figures/`: Visualizations for the higher impact scenario )

## Model Description

The microsimulation model projects the impacts of proposed Medicaid policy changes, including eligibility restrictions, work requirements, and reduced federal matching rates. It models two scenarios:

1. **Base Case Scenario**: Using Congressional Budget Office (CBO) projections of 10.3 million people losing Medicaid coverage by 2034
2. **Higher Impact Scenario**: Using Center on Budget and Policy Priorities (CBPP) projections of 14.4 million people losing Medicaid coverage by 2034

The model projects impacts across multiple domains:

- **Health Outcomes**: Excess mortality, preventable hospitalizations, delayed care, and medication non-adherence
- **Economic Impacts**: Job losses, GDP reduction, tax revenue reduction, and medical debt
- **Healthcare System Impacts**: Rural hospital closures, FQHC revenue reduction, and safety-net provider strain
- **Work Requirements Impact**: Coverage losses due to administrative burden and employment effects
- **Medical Debt Impact**: Financial hardship and care avoidance due to cost

## Key Parameters

All model parameters are empirically derived from peer-reviewed literature and authoritative data sources:

- **Health Outcome Parameters**: From Sommers et al. (NEJM 2012), AHRQ Statistical Brief #72, and the Oregon Health Insurance Experiment
- **Economic Impact Parameters**: From Bureau of Economic Analysis RIMS II multipliers and Commonwealth Fund estimates
- **Healthcare System Impact Parameters**: From Sheps Center for Health Services Research and HRSA Uniform Data System
- **Work Requirements Parameters**: From Sommers et al. (Health Affairs 2020)
- **Medical Debt Parameters**: From Sommers et al. (Health Affairs 2020)

## Running the Model

To run the model:

1. Ensure R is installed with the required packages: `ggplot2`, `scales`, and `gridExtra`
2. Set the scenario flag in the R script: `higher_impact_scenario <- TRUE` for higher impact scenario or `higher_impact_scenario <- FALSE` for base case
3. Run the R script: `Rscript medicaid_impact_model_updated_scenarios.R`
4. Results will be saved to `outputs_base_case/` or `outputs_higher_impact/` directories

## License

This project is licensed under the MIT License - see the LICENSE file for details.
