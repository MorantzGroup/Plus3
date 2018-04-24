# Plus3
Code Used for Plus3 GOTV project.  

Developed by Daniel E. Ho, Alison Morantz, Cassandra Handan-Nader and Tom Rutter. 

This code accompanies the paper: 
"The Effectiveness of a Hyper-Localized
Get-Out-the-Vote Program: Evidence from the
2017 Virginia State Elections"


List of Files: 

- PairHouseholds.R
	- This is an R file that runs the Blossom V 
	  algorithm from Kolmogorov (2009) to pair each voter 
	  to another voter geographically close to them. 
	  
- runMatchingAlgorithm.R 
	- A function we used to assign volunteers to the group of 
	  eligible voters nearest to them. 
	  
- ProcessData.R 
	- Processes and merges the variouse datasets we created 
	  during the election before we run the analysis. 
	  

R packages required: 
"distances", "igraph", "geosphere"
