library(chron);
library(ncdf4);

models <- c("access1-0_rcp85_r1i1p1","bcc-csm1-1-m_rcp85_r1i1p1","bcc-csm1-1_rcp85_r1i1p1","canesm2_rcp85_r1i1p1","ccsm4_rcp85_r1i1p1","cesm1-bgc_rcp85_r1i1p1","cesm1-cam5_rcp85_r1i1p1","cmcc-cm_rcp85_r1i1p1","cnrm-cm5_rcp85_r1i1p1","csiro-mk3-6-0_rcp85_r1i1p1","fgoals-g2_rcp85_r1i1p1","fio-esm_rcp85_r1i1p1","gfdl-cm3_rcp85_r1i1p1","gfdl-esm2g_rcp85_r1i1p1","gfdl-esm2m_rcp85_r1i1p1","giss-e2-r_rcp85_r1i1p1","hadgem2-ao_rcp85_r1i1p1","hadgem2-cc_rcp85_r1i1p1","hadgem2-es_rcp85_r1i1p1","inmcm4_rcp85_r1i1p1","ipsl-cm5a-mr_rcp85_r1i1p1","ipsl-cm5b-lr_rcp85_r1i1p1","miroc-esm-chem_rcp85_r1i1p1","miroc-esm_rcp85_r1i1p1","miroc5_rcp85_r1i1p1","mpi-esm-lr_rcp85_r1i1p1","mpi-esm-mr_rcp85_r1i1p1","mri-cgcm3_rcp85_r1i1p1","noresm1-m_rcp85_r1i1p1")


convert <- function (d,y) {
	day <- NA;
	if (y %% 4 == 0) leap <- TRUE else leap <- FALSE;
	if (leap == TRUE) {
		if (d <= 31) {month <- "01"; day <- d};
		if (d > 31  & d <= 60) {month <- "02"; day <- d - 31};
		if (d > 60 & d <= 91) {month <- "03"; day <- d - 60};
		if (d > 91 & d <= 121) {month <- "04"; day <- d - 91};
		if (d > 121 & d <= 152) {month <- "05"; day <- d - 121};
		if (d > 152 & d <= 182) {month <- "06"; day <- d - 152};	
		if (d > 182 & d <= 213) {month <- "07"; day <- d - 182};	
		if (d > 213 & d <= 244) {month <- "08"; day <- d - 213};	
		if (d > 244 & d <= 274) {month <- "09"; day <- d - 244};	
		if (d > 274 & d <= 305) {month <- "10"; day <- d - 274};	
		if (d > 305 & d <= 335) {month <- "11"; day <- d - 305};	
		if (d > 335 & d <= 366) {month <- "12"; day <- d - 335};	
		if (d < 1 | d > 366) {month <- NA; day <- NA};
	}
	else {
		if (d <= 31) {month <- "01"; day <- d};
		if (d > 31  & d <= 59) {month <- "02"; day <- d - 31};
		if (d > 59 & d <= 90) {month <- "03"; day <- d - 59};
		if (d > 90 & d <= 120) {month <- "04"; day <- d - 90};
		if (d > 120 & d <= 151) {month <- "05"; day <- d - 120};
		if (d > 151 & d <= 181) {month <- "06"; day <- d - 151};	
		if (d > 181 & d <= 212) {month <- "07"; day <- d - 181};	
		if (d > 212 & d <= 243) {month <- "08"; day <- d - 212};	
		if (d > 243 & d <= 274) {month <- "09"; day <- d - 243};	
		if (d > 273 & d <= 304) {month <- "10"; day <- d - 273};	
		if (d > 304 & d <= 334) {month <- "11"; day <- d - 304};	
		if (d > 334 & d <= 365) {month <- "12"; day <- d - 334};	
		if (d < 1 | d > 365) {month <- NA; day <- NA};
	}
	if (day < 10) {day <- paste0("0",as.character(day))} else day <- as.character(day);
	date <- paste0(month,day,as.character(y));
	return(date)
}


getslice <- function (arr, ts) {
	slice <- arr[,,ts];
	slice.vec <- as.vector(slice);
	return(slice.vec);
}

getyear <- function (arr, df, yr) {
	if (yr %% 4 == 0) days <- 366 else days <- 365;
	for (i in 1:days) {
		slice <- getslice(arr, i);
		slicename <- convert(i,as.numeric(yr));
		newcol.df <- as.data.frame(slice);
		colnames(newcol.df) <- c(slicename);
		df <- cbind(df, newcol.df);
	}
	return(df)
}

prepmodel <- function (fname, yr, mtype) {
	nc <- nc_open(fname);
	lon <- ncvar_get(nc, "longitude");
	lat <- ncvar_get(nc, "latitude");
	nc_close(nc);
	lonlat <- expand.grid(lon, lat);
	tmax.df <- data.frame(lonlat);
	colnames(tmax.df) <- c("LON","LAT");
	head(tmax.df);
	return(tmax.df);
}

processmodel <- function (model,mtype,year) {
	start <- year;
	end <- year + 19;
	for (y in start:end) {
		readf <- readnamer(model,y,mtype);
		writef <- writenamer(model,y,mtype);
		download.file(readf, writef, mode = "wb");
		if (y == start){
			print("Prepping data frame")
			df <- prepmodel(writef,year,mtype)
			};
		print(paste("processing ",y));
		nc <- nc_open(writef);
		tmax.array <- ncvar_get(nc, mtype);
		fillvalue <- 1.00000002004088e+20;
		nc_close(nc);
		tmax.array[tmax.array == fillvalue] <- NA;
		df <- getyear(tmax.array, df, y)
	}
	return(df)
}

proc <- function (model, mtype, startyr) {
	df <- processmodel(model, mtype, startyr);
	mid.time <- Sys.time();
	print("beginning to write data file");
	fname <- datanamer(model,mtype,startyr);
	save(df, file=fname);
	end.time <- Sys.time();
	print(paste("elapsed time for Rdata write:",end.time - mid.time));
}

readnamer <- function (model, yr, mtype) {
	rooturl <- "ftp://gdo-dcp.ucllnl.org/pub/dcp/archive/cmip5/hydro/BCSD_daily_forc_nc/";
	name <- paste0(rooturl,model,"/conus_c5.",model,".daily.",mtype,".",yr,".nc");
	return(name);
}

writenamer <- function(model, yr, mtype) {
	basename <- paste0("E:/Data/Research/James/rawdata/",model,"/")
	name <- paste0(basename,"conus_c5.",model,".daily.",mtype,".",yr,".nc");
}

datanamer <- function(model, yr, mtype) {
	basename <- paste0("E:/Data/Research/James/output/",model,"/")
	name <- paste0(basename,"conus_c5.",model,".daily.",mtype,".",yr,".Rdata");
}

extract <- function () {
	setwd("E:/Data/Research/James/");
	modelnames <- c("access1-0_rcp85_r1i1p1","bcc-csm1-1-m_rcp85_r1i1p1","bcc-csm1-1_rcp85_r1i1p1","canesm2_rcp85_r1i1p1","ccsm4_rcp85_r1i1p1","cesm1-bgc_rcp85_r1i1p1","cesm1-cam5_rcp85_r1i1p1","cmcc-cm_rcp85_r1i1p1","cnrm-cm5_rcp85_r1i1p1","csiro-mk3-6-0_rcp85_r1i1p1","fgoals-g2_rcp85_r1i1p1","fio-esm_rcp85_r1i1p1","gfdl-cm3_rcp85_r1i1p1","gfdl-esm2g_rcp85_r1i1p1","gfdl-esm2m_rcp85_r1i1p1","giss-e2-r_rcp85_r1i1p1","hadgem2-ao_rcp85_r1i1p1","hadgem2-cc_rcp85_r1i1p1","hadgem2-es_rcp85_r1i1p1","inmcm4_rcp85_r1i1p1","ipsl-cm5a-mr_rcp85_r1i1p1","ipsl-cm5b-lr_rcp85_r1i1p1","miroc-esm-chem_rcp85_r1i1p1","miroc-esm_rcp85_r1i1p1","miroc5_rcp85_r1i1p1","mpi-esm-lr_rcp85_r1i1p1","mpi-esm-mr_rcp85_r1i1p1","mri-cgcm3_rcp85_r1i1p1","noresm1-m_rcp85_r1i1p1")
	years <- c(1991,2021,2041);
	types <- c("tasmax", "tasmin", "pr");
	for (i in 1:29) {
		model <- modelnames[i];
		curdir <- (paste0("E:/Data/Research/James/rawdata/",model,"/"));
		dir.create(curdir);
		outdir <- paste0("E:/Data/Research/James/output/",model,"/");
		dir.create(outdir);
		print(paste("Start processing",model,Sys.time()));
		for (m in 1:1) {
			mtype <- types[m];
			print(paste("Start processing",model,mtype,Sys.time()))
			for (y in 1:1) {
				startyr <- years[y];
				start.time <- Sys.time();
				print(paste("*****Start processing",model,mtype,startyr,": ",start.time,"*****"));
				proc(model,mtype,startyr);
				end.time <- Sys.time();
				print(paste("*****End processing",model,mtype,startyr,": ",end.time,"*****"))
				print(paste("   --> Elapsed time for ",model,mtype,startyr,": ",end.time - start.time,"*****"))
			}
		}
	}	
}

