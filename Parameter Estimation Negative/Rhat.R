install.packages("coda")
library(coda)

load("/work/AlinaMariaNechita#3266/PE/output_data/hier_est_negative.RData")
ls()
ls(samples)


samples$BUGSoutput$summary

rhat_values <- samples$BUGSoutput$summary[, "Rhat"]
barplot(rhat_values, 
        names.arg = names(rhat_values), 
        las = 2, 
        col = "steelblue", 
        main = "Gelman-Rubin Diagnostic (Rhat)", 
        ylab = "Rhat Value",
        ylim = c(0.9, max(rhat_values, na.rm = TRUE) + 0.2))
abline(h = 1.1, col = "red", lty = 2)  # Add a red dashed line at Rhat = 1.1

# Extract Rhat values
rhat_values <- samples$BUGSoutput$summary[, "Rhat"]

# Convert to data frame
rhat_table <- data.frame(Parameter = names(rhat_values), Rhat = rhat_values)

# Print as a table
print(rhat_table)
write.csv(rhat_table, "rhat_table.csv", row.names = FALSE)

