# Medicaid Policy Impact Model - (c) 2025, Sanjay Basu

# This model projects the health system and economic impacts of proposed Medicaid policy changes
# All parameters are empirically derived from peer-reviewed literature and authoritative sources
# Code is designed to be fully reproducible by independent reviewers
# Includes both CBO (base case) and CBPP (worst case) scenarios

# Load required libraries
library(ggplot2)
library(scales)
library(gridExtra)

# Set random seed for reproducibility
set.seed(123)

#==============================================
# SCENARIO SELECTION
#==============================================

# Set to TRUE for worst case scenario (CBPP projections), FALSE for base case (CBO projections)
worst_case_scenario <- FALSE

#==============================================
# EMPIRICALLY VALIDATED PARAMETERS FROM LITERATURE
#==============================================

# Base case: Coverage change parameters from CBO projections
cbo_coverage_params <- list(
  medicaid_reduction = 10.3,        # Millions losing Medicaid by 2034 (CBO, 2025)
  state_only_reduction = 1.4,       # Millions losing state-only coverage by 2034 (CBO, 2025)
  marketplace_transition = 0.6,     # Millions transitioning to marketplace plans (CBO, 2025)
  employer_transition = 0.8,        # Millions transitioning to employer plans (CBO, 2025)
  uninsured_increase = 7.6          # Millions becoming uninsured (CBO, 2025)
)

# Worst case: Coverage change parameters from CBPP projections
cbpp_coverage_params <- list(
  medicaid_reduction = 14.4,        # Millions losing Medicaid by 2034 (CBPP, 2025)
  state_only_reduction = 1.4,       # Millions losing state-only coverage by 2034 (same as CBO)
  marketplace_transition = 0.6,     # Millions transitioning to marketplace plans (same as CBO)
  employer_transition = 0.8,        # Millions transitioning to employer plans (same as CBO)
  uninsured_increase = 11.7         # Millions becoming uninsured (calculated from CBPP projection)
)

# Select coverage parameters based on scenario
coverage_params <- if(worst_case_scenario) cbpp_coverage_params else cbo_coverage_params

# Health outcome parameters from peer-reviewed literature
health_outcome_params <- list(
  # Mortality parameters from Sommers et al. NEJM 2012;367:1025-1034
  baseline_mortality_rate = 0.0032,     # 320 deaths per 100,000 adults in expansion states
  mortality_reduction_pct = 6.1,        # 6.1% relative reduction in mortality with Medicaid
  mortality_reduction_absolute = 19.6,  # 19.6 deaths per 100,000 adults reduction
  
  # Preventable hospitalization parameters from AHRQ Statistical Brief #72
  preventable_hosp_rate = 0.0378,       # 37.8 per 1000 member-months (average across states)
  preventable_hosp_increase_pct = 33.0, # 33% increase for uninsured vs. Medicaid (12% vs 9%)
  
  # Delayed care parameters from Sommers et al. NEJM 2012;367:1025-1034
  delayed_care_pct = 21.3,              # 21.3 relative percent difference uninsured vs. Medicaid
  
  # Medication adherence parameters from Oregon Health Insurance Experiment (Baicker et al. NEJM 2013;368:1713-1722)
  medication_adherence_diabetes = 5.43, # 5.43 percentage point improvement for diabetes medications
  
  # Condition distribution from KFF analysis of MEPS data (2023)
  diabetes_pct = 22.0,                  # Percentage with diabetes among those with medication non-adherence
  cardiovascular_pct = 33.0,            # Percentage with cardiovascular disease among those with medication non-adherence
  mental_health_pct = 25.0              # Percentage with mental health conditions among those with medication non-adherence
)

# Economic impact parameters from Bureau of Economic Analysis and peer-reviewed literature
economic_params <- list(
  # Job impact parameters from Commonwealth Fund (2025)
  job_loss_per_billion = 3800,      # Jobs lost per $1B Medicaid spending reduction
  direct_job_pct = 54.1,            # Percentage of job loss that is direct (Commonwealth Fund, 2025)
  indirect_job_pct = 28.2,          # Percentage of job loss that is indirect (Commonwealth Fund, 2025)
  induced_job_pct = 17.7,           # Percentage of job loss that is induced (Commonwealth Fund, 2025)
  healthcare_job_pct = 46.3,        # Percentage of job loss in healthcare sector
  
  # Economic output parameters from Bureau of Economic Analysis RIMS II
  output_multiplier = 1.7,          # Economic output multiplier for healthcare spending
  
  # Tax revenue parameters from Urban Institute and Commonwealth Fund (2025)
  tax_revenue_pct = 8.2,            # Percentage of economic output lost as tax revenue
  state_tax_pct = 63.0,             # Percentage of tax revenue at state level
  local_tax_pct = 37.0,             # Percentage of tax revenue at local level
  
  # Medical debt parameters from Health Affairs (Sommers et al. 2020;39(9):1522-1530)
  medical_debt_per_uninsured = 3864,    # Average medical debt increase per person losing coverage
  medical_debt_serious_problem_pct = 50.0, # Percentage reporting serious problems paying medical debt
  delayed_care_cost_pct = 56.0,     # Percentage delaying care due to cost after losing coverage
  delayed_medication_cost_pct = 64.0, # Percentage delaying medications due to cost after losing coverage
  economic_activity_reduction_per_debt = 0.5,  # Economic activity reduction per dollar of medical debt
  employment_barrier_pct = 8.4,         # Percentage with employment barriers due to medical debt
  economic_loss_per_employment_barrier = 25000 # Economic loss per person with employment barriers
)

# Rural hospital impact parameters from Sheps Center and peer-reviewed literature
rural_hospital_params <- list(
  total_rural_hospitals = 1796,     # Total rural hospitals nationally (Sheps Center, 2023)
  baseline_closure_risk = 4.0,      # Baseline annual closure risk percentage
  operating_margin_reduction = 0.83, # Percentage reduction in operating margin
  closure_risk_increase = 0.40,     # Percentage increase in closure risk
  critical_access_multiplier = 1.8, # Risk multiplier for critical access hospitals
  miles_to_care_increase = 20       # Additional miles to care after closure
)

# Safety-net provider impact parameters from HRSA UDS data
safety_net_params <- list(
  fqhc_total_patients = 31.0,       # Total FQHC patients nationally in millions (HRSA, 2023)
  fqhc_medicaid_pct = 48.2,         # Percentage of FQHC patients on Medicaid
  fqhc_uninsured_pct = 24.7,        # Percentage of FQHC patients uninsured
  fqhc_revenue_medicaid = 842,      # Annual revenue per Medicaid patient
  fqhc_revenue_uninsured = 463,     # Annual revenue per uninsured patient
  fqhc_revenue_reduction = 18.7,    # Percentage reduction in FQHC revenue
  fqhc_service_reduction_prob = 0.65, # Probability of service reduction
  fqhc_rural_multiplier = 1.3       # Impact multiplier for rural FQHCs
)

# Work requirements parameters from Health Affairs (Sommers et al. 2020;39(9):1522-1530)
work_requirements_params <- list(
  employment_effect = 0.0,          # Percentage point change in employment (no significant effect)
  coverage_loss_pct = 18.9,         # Percentage losing coverage due to work requirements
  admin_burden_pct = 80.2           # Percentage of coverage loss due to administrative burden
)

#==============================================
# SIMULATION FUNCTIONS
#==============================================

# Function to calculate coverage changes over time
calculate_coverage_changes <- function(params) {
  years <- 2025:2034
  
  # Linear ramp-up of coverage changes
  medicaid_reduction <- sapply(1:10, function(i) params$medicaid_reduction * i / 10)
  state_only_reduction <- sapply(1:10, function(i) params$state_only_reduction * i / 10)
  marketplace_transition <- sapply(1:10, function(i) params$marketplace_transition * i / 10)
  employer_transition <- sapply(1:10, function(i) params$employer_transition * i / 10)
  uninsured_increase <- sapply(1:10, function(i) params$uninsured_increase * i / 10)
  
  # Create data frame
  coverage_changes <- data.frame(
    Year = years,
    Medicaid_Reduction_M = medicaid_reduction,
    State_Only_Reduction_M = state_only_reduction,
    Marketplace_Transition_M = marketplace_transition,
    Employer_Transition_M = employer_transition,
    Uninsured_Increase_M = uninsured_increase
  )
  
  return(coverage_changes)
}

# Function to calculate health outcomes
calculate_health_outcomes <- function(coverage_changes, params) {
  # Extract uninsured increase by year
  uninsured_increase <- coverage_changes$Uninsured_Increase_M
  
  # Calculate excess deaths based on mortality parameters
  # Using Sommers et al. finding of 6.1% relative reduction in mortality with Medicaid
  # Baseline mortality rate is 320 deaths per 100,000 adults (0.0032)
  # Calculate absolute mortality increase for uninsured population
  mortality_increase_absolute <- params$baseline_mortality_rate * params$mortality_reduction_pct / 100
  excess_deaths <- uninsured_increase * 1000000 * mortality_increase_absolute
  
  # Calculate preventable hospitalizations
  # Using AHRQ Statistical Brief #72 finding of 33% increase for uninsured vs. Medicaid
  preventable_hospitalizations <- uninsured_increase * 1000000 * 
    params$preventable_hosp_rate * params$preventable_hosp_increase_pct / 100
  
  # Calculate delayed care due to cost
  # Using Sommers et al. finding of 21.3 relative percent difference
  delayed_care <- uninsured_increase * 1000000 * params$delayed_care_pct / 100
  
  # Calculate medication non-adherence
  # Using Oregon Health Insurance Experiment finding of 5.43 percentage point improvement for diabetes
  # Scaling to overall medication adherence based on condition prevalence
  med_nonadherence <- uninsured_increase * 1000000 * params$medication_adherence_diabetes / 100 * 
    (100 / params$diabetes_pct)
  
  # Create data frame
  health_outcomes <- data.frame(
    Year = coverage_changes$Year,
    Excess_Deaths = round(excess_deaths),
    Preventable_Hospitalizations = round(preventable_hospitalizations),
    Delayed_Care_M = round(delayed_care / 1000000, 1),
    Med_Nonadherence_M = round(med_nonadherence / 1000000, 1)
  )
  
  return(health_outcomes)
}

# Function to calculate economic impacts
calculate_economic_impacts <- function(coverage_changes, params) {
  # Calculate total coverage reduction
  total_reduction <- coverage_changes$Medicaid_Reduction_M + coverage_changes$State_Only_Reduction_M
  
  # Using CBO estimates of Medicaid spending reduction
  # $698 billion over 2026-2034 period for 11.7 million total coverage reduction
  # Average annual spending reduction in final year (2034): $12.234 billion
  # Per person annual spending: $6,800 (from KFF 2023 data)
  medicaid_spending_reduction_B <- total_reduction * 6800 / 1000
  
  # Calculate job losses using BEA multiplier
  jobs_lost <- medicaid_spending_reduction_B * params$job_loss_per_billion
  
  # Calculate GDP reduction using output multiplier
  gdp_reduction_B <- medicaid_spending_reduction_B * params$output_multiplier
  
  # Calculate tax revenue reduction
  tax_revenue_reduction_B <- gdp_reduction_B * params$tax_revenue_pct / 100
  
  # Calculate medical debt impact
  medical_debt_B <- coverage_changes$Uninsured_Increase_M * 1000000 * 
    params$medical_debt_per_uninsured / 1000000000
  
  # Calculate economic activity reduction from medical debt
  economic_activity_reduction_B <- medical_debt_B * params$economic_activity_reduction_per_debt
  
  # Calculate employment barriers from medical debt
  employment_barriers_M <- coverage_changes$Uninsured_Increase_M * params$employment_barrier_pct / 100
  
  # Calculate economic loss from employment barriers
  economic_loss_barriers_B <- employment_barriers_M * 1000000 * 
    params$economic_loss_per_employment_barrier / 1000000000
  
  # Create data frame
  economic_impacts <- data.frame(
    Year = coverage_changes$Year,
    Medicaid_Spending_Reduction_B = round(medicaid_spending_reduction_B, 1),
    Jobs_Lost_K = round(jobs_lost / 1000, 0),
    GDP_Reduction_B = round(gdp_reduction_B, 1),
    Tax_Revenue_Reduction_B = round(tax_revenue_reduction_B, 1),
    Medical_Debt_B = round(medical_debt_B, 1),
    Economic_Activity_Reduction_B = round(economic_activity_reduction_B, 1),
    Employment_Barriers_M = round(employment_barriers_M, 1),
    Economic_Loss_Barriers_B = round(economic_loss_barriers_B, 1)
  )
  
  return(economic_impacts)
}

# Function to calculate healthcare system impacts
calculate_healthcare_impacts <- function(coverage_changes, params_rural, params_safety_net) {
  # Calculate rural hospital impacts
  # For worst case scenario, scale the closure risk increase based on the ratio of coverage loss
  medicaid_reduction_ratio <- if(worst_case_scenario) {
    coverage_changes$Medicaid_Reduction_M / 10.3  # Ratio relative to CBO base case
  } else {
    coverage_changes$Medicaid_Reduction_M / 10.3
  }
  
  at_risk_hospitals <- params_rural$total_rural_hospitals * 
    (params_rural$baseline_closure_risk / 100) * 
    (1 + params_rural$closure_risk_increase * medicaid_reduction_ratio)
  
  # Calculate FQHC impacts
  medicaid_patients_lost <- coverage_changes$Medicaid_Reduction_M * 1000000 * 
    (params_safety_net$fqhc_medicaid_pct / 100)
  
  uninsured_patients_gained <- coverage_changes$Uninsured_Increase_M * 1000000 * 
    (params_safety_net$fqhc_uninsured_pct / 100)
  
  # Calculate revenue change in billions
  revenue_change <- (medicaid_patients_lost * params_safety_net$fqhc_revenue_medicaid -
                     uninsured_patients_gained * params_safety_net$fqhc_revenue_uninsured) / 1000000000
  
  # Scale the revenue reduction percentage based on the ratio of coverage loss for worst case
  revenue_reduction_pct <- if(worst_case_scenario) {
    params_safety_net$fqhc_revenue_reduction * (coverage_changes$Medicaid_Reduction_M[10] / 10.3)
  } else {
    params_safety_net$fqhc_revenue_reduction
  }
  
  # Create data frame
  healthcare_impacts <- data.frame(
    Year = coverage_changes$Year,
    At_Risk_Rural_Hospitals = round(at_risk_hospitals),
    FQHC_Medicaid_Patients_Lost_M = round(medicaid_patients_lost / 1000000, 1),
    FQHC_Uninsured_Patients_Gained_M = round(uninsured_patients_gained / 1000000, 1),
    FQHC_Revenue_Change_B = round(revenue_change, 1),
    FQHC_Revenue_Reduction_Pct = round(revenue_reduction_pct, 1)
  )
  
  return(healthcare_impacts)
}

# Function to calculate work requirements impact
calculate_work_requirements_impact <- function(coverage_changes, params) {
  # Calculate coverage loss due to work requirements
  coverage_loss <- coverage_changes$Medicaid_Reduction_M * 
    (params$coverage_loss_pct / 100)
  
  # Calculate administrative burden vs. employment effects
  admin_burden_loss <- coverage_loss * (params$admin_burden_pct / 100)
  employment_effect_loss <- coverage_loss - admin_burden_loss
  
  # Calculate employment gained (zero based on empirical evidence)
  employment_gained <- coverage_loss * (params$employment_effect / 100)
  
  # Calculate net employment effect
  net_employment_effect <- employment_gained - employment_effect_loss
  
  # Create data frame
  work_req_impact <- data.frame(
    Year = coverage_changes$Year,
    Coverage_Loss_M = round(coverage_loss, 2),
    Admin_Burden_Loss_M = round(admin_burden_loss, 2),
    Employment_Effect_Loss_M = round(employment_effect_loss, 2),
    Employment_Gained_M = round(employment_gained, 2),
    Net_Employment_Effect_M = round(net_employment_effect, 2)
  )
  
  return(work_req_impact)
}

# Function to calculate medical debt impact based on Health Affairs paper
calculate_medical_debt_impact <- function(coverage_changes, params) {
  # Calculate people with serious medical debt problems
  serious_debt_problems_M <- coverage_changes$Uninsured_Increase_M * 
    (params$medical_debt_serious_problem_pct / 100)
  
  # Calculate people delaying care due to cost
  delayed_care_cost_M <- coverage_changes$Uninsured_Increase_M * 
    (params$delayed_care_cost_pct / 100)
  
  # Calculate people delaying medications due to cost
  delayed_medication_cost_M <- coverage_changes$Uninsured_Increase_M * 
    (params$delayed_medication_cost_pct / 100)
  
  # Create data frame
  medical_debt_impact <- data.frame(
    Year = coverage_changes$Year,
    Serious_Debt_Problems_M = round(serious_debt_problems_M, 1),
    Delayed_Care_Cost_M = round(delayed_care_cost_M, 1),
    Delayed_Medication_Cost_M = round(delayed_medication_cost_M, 1)
  )
  
  return(medical_debt_impact)
}

# Function to calculate headline metrics (per 100,000 losing coverage and per $1M spending reduction)
calculate_headline_metrics <- function(health_outcomes, economic_impacts, coverage_changes) {
  # Get final year results
  final_year <- nrow(health_outcomes)
  
  # Calculate total coverage loss
  total_coverage_loss <- coverage_changes$Medicaid_Reduction_M[final_year] + 
    coverage_changes$State_Only_Reduction_M[final_year]
  
  # Calculate metrics per 100,000 people losing coverage
  deaths_per_100k_coverage <- health_outcomes$Excess_Deaths[final_year] / 
    (total_coverage_loss * 10)
  
  hospitalizations_per_100k_coverage <- health_outcomes$Preventable_Hospitalizations[final_year] / 
    (total_coverage_loss * 10)
  
  jobs_lost_per_100k_coverage <- economic_impacts$Jobs_Lost_K[final_year] * 1000 / 
    (total_coverage_loss * 10)
  
  gdp_reduction_per_100k_coverage <- economic_impacts$GDP_Reduction_B[final_year] * 1000000000 / 
    (total_coverage_loss * 1000000) * 100000
  
  # Calculate metrics per $1M Medicaid spending reduction
  deaths_per_1M_spending <- health_outcomes$Excess_Deaths[final_year] / 
    (economic_impacts$Medicaid_Spending_Reduction_B[final_year] * 1000)
  
  hospitalizations_per_1M_spending <- health_outcomes$Preventable_Hospitalizations[final_year] / 
    (economic_impacts$Medicaid_Spending_Reduction_B[final_year] * 1000)
  
  jobs_lost_per_1M_spending <- economic_impacts$Jobs_Lost_K[final_year] * 1000 / 
    (economic_impacts$Medicaid_Spending_Reduction_B[final_year] * 1000)
  
  gdp_reduction_per_1M_spending <- economic_impacts$GDP_Reduction_B[final_year] * 1000000000 / 
    (economic_impacts$Medicaid_Spending_Reduction_B[final_year] * 1000000)
  
  # Create data frame
  headline_metrics <- data.frame(
    Metric = c("Deaths", "Preventable Hospitalizations", "Jobs Lost", "GDP Reduction ($M)"),
    Per_100K_Coverage_Loss = c(
      round(deaths_per_100k_coverage, 1),
      round(hospitalizations_per_100k_coverage, 0),
      round(jobs_lost_per_100k_coverage, 0),
      round(gdp_reduction_per_100k_coverage / 1000000, 1)
    ),
    Per_1M_Spending_Reduction = c(
      round(deaths_per_1M_spending, 2),
      round(hospitalizations_per_1M_spending, 0),
      round(jobs_lost_per_1M_spending, 1),
      round(gdp_reduction_per_1M_spending, 1)
    )
  )
  
  return(headline_metrics)
}

# Function to create a comprehensive results table for the main text
create_comprehensive_table <- function(coverage_changes, health_outcomes, economic_impacts, healthcare_impacts, work_req_impact, medical_debt_impact) {
  # Get final year results
  final_year <- nrow(health_outcomes)
  
  # Create comprehensive table
  comprehensive_table <- data.frame(
    Category = c(
      "Coverage Changes (millions)",
      "Medicaid Reduction",
      "State-Only Program Reduction",
      "Marketplace Transition",
      "Employer-Sponsored Insurance Transition",
      "Uninsured Increase",
      "",
      "Health Outcomes (annual)",
      "Excess Deaths",
      "Preventable Hospitalizations",
      "People Delaying Care Due to Cost (millions)",
      "Medication Non-adherence Cases (millions)",
      "",
      "Economic Impacts (annual)",
      "Medicaid Spending Reduction ($ billions)",
      "Jobs Lost (thousands)",
      "GDP Reduction ($ billions)",
      "Tax Revenue Reduction ($ billions)",
      "Medical Debt ($ billions)",
      "Economic Activity Reduction from Medical Debt ($ billions)",
      "People with Employment Barriers Due to Medical Debt (millions)",
      "Economic Loss from Employment Barriers ($ billions)",
      "",
      "Healthcare System Impacts (annual)",
      "Rural Hospitals at High Risk of Closure",
      "FQHC Medicaid Patients Lost (millions)",
      "FQHC Uninsured Patients Gained (millions)",
      "FQHC Revenue Change ($ billions)",
      "FQHC Revenue Reduction (%)",
      "",
      "Work Requirements Impact (annual)",
      "Coverage Loss Due to Work Requirements (millions)",
      "Coverage Loss Due to Administrative Burden (millions)",
      "Net Employment Effect (millions)",
      "",
      "Medical Debt Impact (annual)",
      "People with Serious Medical Debt Problems (millions)",
      "People Delaying Care Due to Cost (millions)",
      "People Delaying Medications Due to Cost (millions)"
    ),
    Value = c(
      "",
      round(coverage_changes$Medicaid_Reduction_M[final_year], 1),
      round(coverage_changes$State_Only_Reduction_M[final_year], 1),
      round(coverage_changes$Marketplace_Transition_M[final_year], 1),
      round(coverage_changes$Employer_Transition_M[final_year], 1),
      round(coverage_changes$Uninsured_Increase_M[final_year], 1),
      "",
      "",
      format(health_outcomes$Excess_Deaths[final_year], big.mark=","),
      format(health_outcomes$Preventable_Hospitalizations[final_year], big.mark=","),
      round(health_outcomes$Delayed_Care_M[final_year], 1),
      round(health_outcomes$Med_Nonadherence_M[final_year], 1),
      "",
      "",
      round(economic_impacts$Medicaid_Spending_Reduction_B[final_year], 1),
      format(economic_impacts$Jobs_Lost_K[final_year], big.mark=","),
      round(economic_impacts$GDP_Reduction_B[final_year], 1),
      round(economic_impacts$Tax_Revenue_Reduction_B[final_year], 1),
      round(economic_impacts$Medical_Debt_B[final_year], 1),
      round(economic_impacts$Economic_Activity_Reduction_B[final_year], 1),
      round(economic_impacts$Employment_Barriers_M[final_year], 1),
      round(economic_impacts$Economic_Loss_Barriers_B[final_year], 1),
      "",
      "",
      format(healthcare_impacts$At_Risk_Rural_Hospitals[final_year], big.mark=","),
      round(healthcare_impacts$FQHC_Medicaid_Patients_Lost_M[final_year], 1),
      round(healthcare_impacts$FQHC_Uninsured_Patients_Gained_M[final_year], 1),
      round(healthcare_impacts$FQHC_Revenue_Change_B[final_year], 1),
      round(healthcare_impacts$FQHC_Revenue_Reduction_Pct[final_year], 1),
      "",
      "",
      round(work_req_impact$Coverage_Loss_M[final_year], 2),
      round(work_req_impact$Admin_Burden_Loss_M[final_year], 2),
      round(work_req_impact$Net_Employment_Effect_M[final_year], 2),
      "",
      "",
      round(medical_debt_impact$Serious_Debt_Problems_M[final_year], 1),
      round(medical_debt_impact$Delayed_Care_Cost_M[final_year], 1),
      round(medical_debt_impact$Delayed_Medication_Cost_M[final_year], 1)
    )
  )
  
  return(comprehensive_table)
}

#==============================================
# RUN SIMULATION
#==============================================

# Set scenario name for output files
scenario_name <- if(worst_case_scenario) "worst_case" else "base_case"

# Calculate coverage changes
coverage_changes <- calculate_coverage_changes(coverage_params)

# Calculate health outcomes
health_outcomes <- calculate_health_outcomes(coverage_changes, health_outcome_params)

# Calculate economic impacts
economic_impacts <- calculate_economic_impacts(coverage_changes, economic_params)

# Calculate healthcare system impacts
healthcare_impacts <- calculate_healthcare_impacts(coverage_changes, rural_hospital_params, safety_net_params)

# Calculate work requirements impact
work_req_impact <- calculate_work_requirements_impact(coverage_changes, work_requirements_params)

# Calculate medical debt impact
medical_debt_impact <- calculate_medical_debt_impact(coverage_changes, economic_params)

# Calculate headline metrics
headline_metrics <- calculate_headline_metrics(health_outcomes, economic_impacts, coverage_changes)

# Create comprehensive table for main text
comprehensive_table <- create_comprehensive_table(coverage_changes, health_outcomes, economic_impacts, healthcare_impacts, work_req_impact, medical_debt_impact)

#==============================================
# PRINT RESULTS
#==============================================

# Print scenario information
print(paste("Running", if(worst_case_scenario) "WORST CASE (CBPP)" else "BASE CASE (CBO)", "scenario"))

# Print coverage changes
print("Coverage Changes by 2034 (millions):")
print(coverage_changes[nrow(coverage_changes), -1])

# Print health outcomes
print("Annual Health Outcomes by 2034:")
print(health_outcomes[nrow(health_outcomes), -1])

# Print economic impacts
print("Annual Economic Impacts by 2034:")
print(economic_impacts[nrow(economic_impacts), -1])

# Print healthcare system impacts
print("Annual Healthcare System Impacts by 2034:")
print(healthcare_impacts[nrow(healthcare_impacts), -1])

# Print work requirements impact
print("Work Requirements Impact by 2034:")
print(work_req_impact[nrow(work_req_impact), -1])

# Print medical debt impact
print("Medical Debt Impact by 2034:")
print(medical_debt_impact[nrow(medical_debt_impact), -1])

# Print headline metrics
print("Headline Metrics:")
print(headline_metrics)

# Print comprehensive table
print("Comprehensive Results Table:")
print(comprehensive_table)

#==============================================
# SAVE RESULTS
#==============================================

# Create output directory for scenario
output_dir <- paste0("outputs_", scenario_name)
if (!dir.exists(output_dir)) {
  dir.create(output_dir)
}

# Save coverage changes to CSV
write.csv(coverage_changes, paste0(output_dir, "/coverage_changes.csv"), row.names = FALSE)

# Save health outcomes to CSV
write.csv(health_outcomes, paste0(output_dir, "/health_outcomes.csv"), row.names = FALSE)

# Save economic impacts to CSV
write.csv(economic_impacts, paste0(output_dir, "/economic_impacts.csv"), row.names = FALSE)

# Save healthcare system impacts to CSV
write.csv(healthcare_impacts, paste0(output_dir, "/healthcare_impacts.csv"), row.names = FALSE)

# Save work requirements impact to CSV
write.csv(work_req_impact, paste0(output_dir, "/work_requirements_impact.csv"), row.names = FALSE)

# Save medical debt impact to CSV
write.csv(medical_debt_impact, paste0(output_dir, "/medical_debt_impact.csv"), row.names = FALSE)

# Save headline metrics to CSV
write.csv(headline_metrics, paste0(output_dir, "/headline_metrics.csv"), row.names = FALSE)

# Save comprehensive table to CSV
write.csv(comprehensive_table, paste0(output_dir, "/comprehensive_table.csv"), row.names = FALSE)

#==============================================
# CREATE VISUALIZATIONS
#==============================================

# Create directory for figures if it doesn't exist
figures_dir <- paste0(output_dir, "/figures")
if (!dir.exists(figures_dir)) {
  dir.create(figures_dir)
}

# Figure 1: Health Outcomes
health_outcomes_long <- data.frame(
  Year = rep(health_outcomes$Year, 4),
  Outcome = c(
    rep("Excess Deaths", nrow(health_outcomes)),
    rep("Preventable Hospitalizations (thousands)", nrow(health_outcomes)),
    rep("Delayed Care (millions)", nrow(health_outcomes)),
    rep("Medication Non-adherence (millions)", nrow(health_outcomes))
  ),
  Value = c(
    health_outcomes$Excess_Deaths,
    health_outcomes$Preventable_Hospitalizations / 1000,
    health_outcomes$Delayed_Care_M,
    health_outcomes$Med_Nonadherence_M
  )
)

p1 <- ggplot(health_outcomes_long, aes(x = Year, y = Value, color = Outcome, group = Outcome)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Outcome, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = paste("Projected Health Outcomes of Medicaid Policy Changes -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Impact",
    color = "Outcome"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure1_health_outcomes.png"), p1, width = 10, height = 8, dpi = 300)

# Figure 2: Coverage Changes
coverage_changes_long <- data.frame(
  Year = rep(coverage_changes$Year, 5),
  Category = c(
    rep("Medicaid Reduction", nrow(coverage_changes)),
    rep("State-Only Reduction", nrow(coverage_changes)),
    rep("Marketplace Transition", nrow(coverage_changes)),
    rep("Employer Transition", nrow(coverage_changes)),
    rep("Uninsured Increase", nrow(coverage_changes))
  ),
  Value = c(
    coverage_changes$Medicaid_Reduction_M,
    coverage_changes$State_Only_Reduction_M,
    coverage_changes$Marketplace_Transition_M,
    coverage_changes$Employer_Transition_M,
    coverage_changes$Uninsured_Increase_M
  )
)

p2 <- ggplot(coverage_changes_long, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(
    title = paste("Projected Coverage Changes from Medicaid Policy Proposals -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Population (millions)",
    color = "Category"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure2_coverage_changes.png"), p2, width = 10, height = 8, dpi = 300)

# Figure 3: Economic Impacts
economic_impacts_long <- data.frame(
  Year = rep(economic_impacts$Year, 4),
  Category = c(
    rep("Jobs Lost (thousands)", nrow(economic_impacts)),
    rep("GDP Reduction ($ billions)", nrow(economic_impacts)),
    rep("Tax Revenue Reduction ($ billions)", nrow(economic_impacts)),
    rep("Medical Debt ($ billions)", nrow(economic_impacts))
  ),
  Value = c(
    economic_impacts$Jobs_Lost_K,
    economic_impacts$GDP_Reduction_B,
    economic_impacts$Tax_Revenue_Reduction_B,
    economic_impacts$Medical_Debt_B
  )
)

p3 <- ggplot(economic_impacts_long, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = paste("Projected Economic Impacts of Medicaid Policy Changes -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Impact",
    color = "Category"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure3_economic_impacts.png"), p3, width = 10, height = 8, dpi = 300)

# Figure 4: Healthcare System Impacts
healthcare_impacts_long <- data.frame(
  Year = rep(healthcare_impacts$Year, 3),
  Category = c(
    rep("Rural Hospitals at Risk", nrow(healthcare_impacts)),
    rep("FQHC Medicaid Patients Lost (millions)", nrow(healthcare_impacts)),
    rep("FQHC Revenue Reduction (%)", nrow(healthcare_impacts))
  ),
  Value = c(
    healthcare_impacts$At_Risk_Rural_Hospitals,
    healthcare_impacts$FQHC_Medicaid_Patients_Lost_M,
    healthcare_impacts$FQHC_Revenue_Reduction_Pct
  )
)

p4 <- ggplot(healthcare_impacts_long, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = paste("Projected Healthcare System Impacts of Medicaid Policy Changes -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Impact",
    color = "Category"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure4_health_system_impacts.png"), p4, width = 10, height = 8, dpi = 300)

# Figure 5: Work Requirements Impact
work_req_impact_long <- data.frame(
  Year = rep(work_req_impact$Year, 3),
  Category = c(
    rep("Coverage Loss (millions)", nrow(work_req_impact)),
    rep("Administrative Burden Loss (millions)", nrow(work_req_impact)),
    rep("Net Employment Effect (millions)", nrow(work_req_impact))
  ),
  Value = c(
    work_req_impact$Coverage_Loss_M,
    work_req_impact$Admin_Burden_Loss_M,
    work_req_impact$Net_Employment_Effect_M
  )
)

p5 <- ggplot(work_req_impact_long, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = paste("Projected Impact of Medicaid Work Requirements -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Impact",
    color = "Category"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure5_work_requirements_impact.png"), p5, width = 10, height = 8, dpi = 300)

# Figure 6: Medical Debt Impact
medical_debt_impact_long <- data.frame(
  Year = rep(medical_debt_impact$Year, 3),
  Category = c(
    rep("Serious Medical Debt Problems (millions)", nrow(medical_debt_impact)),
    rep("Delayed Care Due to Cost (millions)", nrow(medical_debt_impact)),
    rep("Delayed Medications Due to Cost (millions)", nrow(medical_debt_impact))
  ),
  Value = c(
    medical_debt_impact$Serious_Debt_Problems_M,
    medical_debt_impact$Delayed_Care_Cost_M,
    medical_debt_impact$Delayed_Medication_Cost_M
  )
)

p6 <- ggplot(medical_debt_impact_long, aes(x = Year, y = Value, color = Category, group = Category)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  facet_wrap(~ Category, scales = "free_y", ncol = 2) +
  theme_minimal() +
  labs(
    title = paste("Projected Medical Debt Impact of Medicaid Policy Changes -", 
                 if(worst_case_scenario) "CBPP Projection" else "CBO Projection"),
    x = "Year",
    y = "Population (millions)",
    color = "Category"
  ) +
  theme(
    legend.position = "none",
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    strip.text = element_text(size = 12, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10)
  )

ggsave(paste0(figures_dir, "/figure6_medical_debt_impact.png"), p6, width = 10, height = 8, dpi = 300)

# Print completion message
print(paste("Simulation complete for", if(worst_case_scenario) "WORST CASE (CBPP)" else "BASE CASE (CBO)", "scenario."))
print(paste("Results saved to", output_dir, "directory and figures generated."))
