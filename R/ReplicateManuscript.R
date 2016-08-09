ReplicateManuscript <-
function() {
	if (!requireNamespace("ExperimentHub")) {
		stop("The 'ExperimentHub' package must be installed before using this function")
	} else { 
		# Load the datasets and query genes
		message("* Loading the pre-processed datasets...")

		data(ExampleQueryGenes, envir = environment())
		QGs <- get("ExampleQueryGenes", envir = environment())

		attachNamespace('ExperimentHub')
		hub <- get('ExperimentHub', envir = environment())()
		x <- lapply(unique(unlist(strsplit(QGs$Dataset, ", "))), function(i) { hub[[i]] })
		names(x) <- unique(unlist(strsplit(QGs$Dataset, ", ")))


		Results <- lapply(seq_len(dim(QGs)[1]), function(i) {
			message("* Running CellMapper for ", QGs$CellType[i], "...")

			dataset <- x[strsplit(QGs$Dataset[i], ", ")[[1]]]
			if (length(dataset) == 1) { dataset <- dataset[[1]] }

			return(suppressWarnings(CM.search(dataset, query.genes = QGs$EntrezID[i], verbose = FALSE)))
		})
		names(Results) <- QGs$CellType

		return(Results)
	}
}
