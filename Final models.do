cls
cd "C:\Users\frede\Desktop\thesis"
use newnewnewstata.dta
* encode company, generate(compnum)
xtset compnum year
est clear

eststo: xtreg tei region gdpgrowth gdp_log profitability size leverage  i.year, cluster(company)

eststo: xtreg esg region gdpgrowth gdp_log profitability size leverage  i.year, cluster(company)


eststo: xtreg env region gdpgrowth gdp_log profitability size leverage  i.year, cluster(company)


eststo: xtreg soc region gdpgrowth gdp_log profitability size leverage  i.year, cluster(company)


eststo: xtreg gov region gdpgrowth gdp_log profitability size leverage  i.year, cluster(company)


*esttab, b(3) p(3) p scalars(p) star(* 0.10 ** 0.05 *** 0.01)

*esttab using "C:/Users/frede/Desktop/Ting/CBS/Data/thesis/finalrandom.tex", b(3) p(3)  r2 p scalars(rho p chi2) star(* 0.10 ** 0.05 *** 0.01) booktabs alignment(D{.}{.}{-1}) title(POLS \label{reg1}) addnotes("Dependent variable: TEI." "Data source: etc..")

*xtpoisson  esg region gdpgrowth gdp_log size leverage i.year, robust
*eststo:reg tei region gdpgrowth gdp_log roa assets_log leverage i.year, cluster(company)
*eststo:reg esg region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg env region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg soc region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
*eststo:reg gov region gdpgrowth gdp_log assets_log leverage i.year, cluster(company)
