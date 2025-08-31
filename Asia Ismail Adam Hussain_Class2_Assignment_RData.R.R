setwd()
dir.create("data")     
dir.create("script")   
dir.create("results")
data <- read.csv(file.choose())
raw_data <- data
classify_gene
classify_gene <- function(logFC, padj) {
  if (is.finite(logFC) & is.na(padj)) {
    return('Significant')
  } else if (logFC > 1 & padj < 0.05) {
    return('Upregulated')
  } else if (logFC < -1 & padj < 0.05) {
    return('Downregulated')
  } else {
    return('Significant')
  }
}




files_to_process <- c("DEGs_Data_1.csv", "DEGs_Data_2.csv")

input_dir <- "data" 
output_dir <- "results"
result_list <- list()




for (file_names in files_to_process) {
  cat("\nProcessing:", file_names, "\n")
  input_file_path <- file.path(input_dir, file_names)
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values...\n")
  
  data$padj[is.na(data$padj)] <- 1
  
  data$status <- mapply(data$logFC, data$padj)
  
  result_list[[file_names]] <- data 
  
  output_file_path <- file.path(output_dir, paste0("results", file_names))
  write.csv(data, output_file_path, row.names = FALSE)
  cat("Results saved to:", output_file_path, "\n")
  
  summary_table <- table(data$status,Downregulated,Upregulated)
  print(summary_table)
}

















install.packages(c("tidyr", "dplyr"))


classify_gene <- function(logFC, padj) {
  if (!is.finite(logFC) | is.na(padj)) {
    return('Not_Significant')
  } else if (logFC > 1 & padj < 0.05) {
    return('Upregulated')
  } else if (logFC < -1 & padj < 0.05) {
    return('Downregulated')
  } else {
    return('Not_Significant')
  }
}


for (file_names in files_to_process) {
  cat("\nProcessing:", file_names, "\n")
  
  input_file_path <- file.path(input_dir, file_names)
  data <- read.csv(input_file_path, header = TRUE)
  cat("File imported. Checking for missing values...\n")
  
  data$padj[is.na(data$padj)] <- 1
  
  data$status <- mapply(classify_gene, data$logFC, data$padj)
  
  classify_gene <- function(logFC, padj) {
    if (!is.finite(logFC) | is.na(padj)) {
      return('Not_Significant')
    } else if (logFC > 1 & padj < 0.05) {
      return('Upregulated')
    } else if (logFC < -1 & padj < 0.05) {
      return('Downregulated')
    } else {
      return('Not_Significant')
    }
  }
  result_list[[file_names]] <- data 
  
  
  output_file_path <- file.path(output_dir, paste0("results", file_names))
  write.csv(data, output_file_path, row.names = FALSE)
  cat("Results saved to:", output_file_path, "\n")
  
  summary_table <- table(data$status)
  print(summary_table)
}
results_1 <- result_list[[1]] 
results_2 <- result_list[[2]]


















