

for (r in 1:length(fixed$whichlon)) {
	start <- as.numeric(gregexpr("/",fixed$Date.Discovered.x[r])[[1]]) + 1
	fixed$mo[r] <- substr(fixed$Date.Discovered.x[r], 1, start[1] - 2)
	fixed$da[r] <- substr(fixed$Date.Discovered.x[r], start[1], start[2] - 2)
	fixed$yr[r] <- paste0("20",substr(fixed$Date.Discovered.x[r], start[2], 13))
}


for (r in 1:length(fixed$mo)) {
	if (nchar(fixed$mo[r]) == 1) {fixed$mo[r] <- paste0("0",fixed$mo[r])}
}

for (r in 1:length(fixed$da)) {
	if (nchar(fixed$da[r]) == 1) {fixed$da[r] <- paste0("0",fixed$da[r])}
}


for (r in 1:length(fixed$mo)) {
	fixed$date[r] <- paste0("x",fixed$yr[r],fixed$mo[r], fixed$da[r])
}