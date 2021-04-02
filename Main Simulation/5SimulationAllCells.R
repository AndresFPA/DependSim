#Total number of cells

TotalCells <- nrow(Design)
system.time(for (i in 1:TotalCells){
  Row <- i
  MyResult <- SimulationCell(Design = Design, RowOfDesign = Row, K = 1000)
  # Write output of one cell of the design
  save(MyResult, file =paste("MyResult", "Row", Row,".Rdata" , sep =""))
})

#Total number of cells no relationship

TotalCells <- nrow(Design_no)
system.time(for (i in 1:TotalCells){
  Row <- i
  MyResult <- SimulationCell(Design = Design_no, RowOfDesign = Row, K = 1000)
  # Write output of one cell of the design
  save(MyResult, file =paste("MyResult", "Row", Row,".Rdata" , sep =""))
})
