## Initialize my intermediate and output variables in my usual
## eccentric way


newrow <- c()
normalrange <- data.frame()

for (st in 1:1) {
	newrow <- c(x$data[[st]]$meta$ll,x$data[[st]]$meta$name)

	for (y in 1:31) {
		newrow <- c(newrow, x$data[[st]]$data[[y]][[1]])
	}
normalrange <- rbind(normalrange, newrow)
}