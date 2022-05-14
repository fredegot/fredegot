cls
cd "C:\Users\frede\Desktop\thesis"
use "IBECdtaData - Copy.dta"
*encode firm, generate(compnum)
xtset compnum year
est clear

reg log_rev em roa log_gdp gdp_growth ofdipercent rndpercent i.year, cluster(firm)
vif

eststo:xtreg log_rev em log_gdp gdp_growth ofdipercent roa i.year, cluster(firm)

eststo:xtreg log_rev em  log_gdp gdp_growth ofdipercent roa rndpercent i.year, cluster(firm)

esttab, b(3) p(3) r2 p scalars(rho p chi2)

*regress log_rev em rndpercent log_gdp gdp_growth ofdipercent i.year, cluster(firm)
*vif

*esttab using "C:/Users/frede/Desktop/thesis/IBECtex2.tex", b(3) p(3)  r2 p scalars(rho p chi2) star(* 0.10 ** 0.05 *** 0.01) booktabs alignment(D{.}{.}{-1}) title(Are Wind Turbine EMNCs at a disadvantage? \label{reg1}) addnotes("Dependent variable: Log(Revenue)." "Data source: etc..")

*regress log_rev em log_rnd roa log_gdp gdp_growth ifdipercent ofdipercent i.year, cluster(firm)

*vif 

*eststo:
*esttab, b(3) p(3) p scalars(p) star(* 0.10 ** 0.05 *** 0.01)

*esttab using "C:/Users/frede/Desktop/Ting/CBS/Data/thesis/finalrandom.tex", b(3) p(3)  r2 p scalars(rho p chi2) star(* 0.10 ** 0.05 *** 0.01) booktabs alignment(D{.}{.}{-1}) title(POLS \label{reg1}) addnotes("Dependent variable: TEI." "Data source: etc..")

*xtpoisson  esg region gdpgrowth gdp_log size leverage i.year, robust
*eststo:reg tei region gdpgrowth gdp_log roa assets_log leverage i.year, cluster(company)
*eststo:reg esg region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg env region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg soc region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg gov region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
