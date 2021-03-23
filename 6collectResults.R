# Collect all results and put it in a matrix
# original design is in the matrix Design
# Total number of rows of Design = TotalCells
#first load one workspace, for example, cell 1 of the Design matrix
#save the number of replications within that cell
setwd("C:/Users/Andres/Desktop/Leiden/Thesis/sim/Mine")
TotalCells <- 1 
Row <- 1
load(file.path("results",paste("MyResult", "Row", Row,".Rdata" , sep ="")))
K <- nrow(MyResult)
#Initialize a very large matrix with number of rows = K * TotalCells
TotalCells <- nrow(Design)
Results_sim <- matrix(NA, ncol = ncol(MyResult), nrow = K*TotalCells)
#Fill in this matrix with the results that you obtained for each cell of the design (= each of the workspaces).
#Thus loop over the rows of Design
for (i in 1:TotalCells){
  Row <- i
  load(file.path("results",paste("MyResult", "Row", Row,".Rdata" , sep ="")))
  Results_sim[(K*(i-1)+1):(i*K), ] <- MyResult
}

#repeate the Design matrix K times
Results_des <- Design[rep(1:nrow(Design),each=K),]
rownames(Results_des) <- 1:nrow(Results_des)

#rbind the vector 1:K TotalCelss times
Results_K <- do.call(what = rbind, args = replicate(TotalCells, matrix(c(1:K), ncol = 1), simplify = F))
#Create the final results matrix
ResultsSimAll <-as.data.frame(cbind(Results_des, K = Results_K, Results_sim))
#Give the columns the right name:
names(ResultsSimAll) <- c(names(Design), "K","Pearson", "Kendall", "Spearman", 
                          "HHG", "HSIC", "Hoeff", "dCor", "MI", "HHG/Pearson")
head(ResultsSimAll)

save(ResultsSimAll, file = "AllResultsSim.Rdata")
#Optionally save it as a .sav file
library(haven)
workingDirectory <- paste(getwd(), "/", sep = "")
filenameToSave <- paste(workingDirectory, "AllResultsSim", ".sav", sep = "")
write_sav(ResultsSimAll, filenameToSave)


#----------------------------------------No relation------------------------------------------------------#
setwd("C:/Users/Andres/Desktop/Leiden/Thesis/sim/Mine/results")
TotalCells <- 1 
Row <- 1
load(file.path("results_no_rel",paste("MyResult", "Row", Row,".Rdata" , sep ="")))
K <- nrow(MyResult)
#Initialize a very large matrix with number of rows = K * TotalCells
TotalCells_no <- nrow(Design_no)
Results_sim_no <- matrix(NA, ncol = ncol(MyResult), nrow = K*TotalCells_no)
#Fill in this matrix with the results that you obtained for each cell of the design (= each of the workspaces).
#Thus loop over the rows of Design
for (i in 1:TotalCells){
  Row <- i
  load(file.path("results_no_rel",paste("MyResult", "Row", Row,".Rdata" , sep ="")))
  Results_sim_no[(K*(i-1)+1):(i*K), ] <- MyResult
}

#repeate the Design matrix K times
Results_des_no <- Design_no[rep(1:nrow(Design_no),each=K),]
rownames(Results_des_no) <- 1:nrow(Results_des_no)

#rbind the vector 1:K TotalCelss times
Results_K_no <- do.call(what = rbind, args = replicate(TotalCells, matrix(c(1:K), ncol = 1), simplify = F))
#Create the final results matrix
ResultsSimAll_no <-as.data.frame(cbind(Results_des_no, K = Results_K_no, Results_sim_no))
#Give the columns the right name:
names(ResultsSimAll_no) <- c(names(Design_no), "K","Pearson", "Kendall", "Spearman", 
                             "HHG", "HSIC", "Hoeff", "dCor", "MI", "HHG/Pearson")
head(ResultsSimAll_no)

save(ResultsSimAll_no, file = "AllResultsSim_no.Rdata")
