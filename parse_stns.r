## set up column names
yr <- paste0("x",2005:2015)
mo <- c(paste0("0",1:9),10,11,12)
long <- c(paste0("0",1:9),10:31)
short <- c(paste0("0",1:9), 10:30)
feb <- c(paste0("0",1:9),10:28)
leap <- c(paste0("0",1:9),10:29)
ym <- c()
	days <- c(long, feb, long, short, long, short, long, long, short, long, short, long)
	leapdays <- c(long, feb, long, short, long, short, long, long, short, long, short, long)
	d <- c(days, days, days, leapdays, days, days, days, leapdays, days, days, days) 

for (y in 1:11) {
	for (m in 1:12) {
		ym <- c(ym, paste0(yr[y],mo[m]))
	}
}


alldays <- c()

for (g in 0:10) {
	for (m in 1:12) {
		if (m %in% c(1,3,5,7,8,10,12)) {

			for (d in 1:31) {
			nextday <- paste0(ym[g*12+m], long[d])
			alldays <- c(alldays,nextday)
		}
	}
		if (m %in% c(4,6,9,11)){
			for (d in 1:30) {
			nextday <- paste0(ym[g*12+m], long[d])
			alldays <- c(alldays,nextday)
		}
	}
		if (m == 2) {
			for (d in 1:28) {
			nextday <- paste0(ym[g*12+m], long[d])
			alldays <- c(alldays,nextday)
		}
	}

	}






}