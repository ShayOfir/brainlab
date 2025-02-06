#Pipeline functions
# (c) Shay Ofir-Geva, 2020
#
# List of functions:
#
# Auxillary functions:
# rn(x) -> NIfTI        
#                                   Reads NIfTI obect from NIfTI file
#
# wn(x,nam) -> void     
#                                   Writes NIfTI object into NIfTI file
#
# flp(x) -> NIfTI       
#                                   Flips brain image right to left and vice versa
#
# analyze2nifti(full.fn, new.full.fn) 
#                                    NIfTI  Converts Analyze file into NIfTI file # nolint: line_length_linter.
#
#dependecies: fslr, oro.nifti, neurobase, stringr


#Define constants
prefix.cont <- "/shared/"
prefix <- prefix.cont
atlas.and.templates.folder <- paste0(prefix, "Templates_Atlases/")

#Inform the user that the pipeline was sourced successfully:
cat(sprintf("BrainLab pipeline was sourced successfully.\n"))

#Fundemental functions
rn <- function(x){
    #rn: NIfTI obect from NIfTI file
    #Args:
    #  x: NIfTI file
    #Returns:
    #  NIfTI object
    #
    return(oro.nifti::readNIfTI(x, reorient=FALSE))
}

wn <- function(x, nam){
    #wn: Writes NIfTI object into NIfTI file
    #Args:
    #  x: NIfTI object
    #  nam: NIfTI file
    #Returns:
    #  void
    #
    oro.nifti::writeNIfTI(x, nam, gzipped = TRUE)
}

flp <- function(x){
    #flp: Flips brain image right to left and vice versa
    #Args:
    #  x: NIfTI object
    #
    #Returns:
    #  NIfTI object
    #
    return(neurobase::flip_img(x,x = TRUE))
}

analyze2nifti <- function (full.fn, new.full.fn = NA){
    #analyze2nifti: Converts Analyze file into NIfTI file
    #Args:
    #  full.fn: Analyze file (with .hdr extension)
    #  new.full.fn: NIfTI file (optional)
    #Returns:
    #  NIfTI file
    #

    #If new.full.fn is not defined, define it
    if (is.na(new.full.fn)){
        fn <- basename(full.fn)
        path <- dirname(full.fn)
        new.fn <- substr(basename(fn),1,stringr::str_locate(basename(fn),"\\.hdr")-1)
        new.full.fn <- paste0(path,'/',new.fn)        
    }
    
    # Read the Analyze file and flip it
    x <- flp(fslr::fslroi(file = full.fn))
	
	#If depth dimension is too large (e.g. =9 or something), change it into 1.5mm
	if (oro.nifti::pixdim(x)[4] > 1.5)
	{	
		cat(sprintf('Wrong Z pixel dimension (%1.2f), changing into 1.5\n',pixdim(x)[4]))
		x <- modify.pixel.dim(x = x, index = 3, new.d = 1.5) #Change Z pixel dimension into 1.5   
	}
	
    wn(x, new.full.fn) 
    return(new.full.fn)
}

modify.pixel.dim <- function (x, index, new.d){
    #modify.pixel.dim: Modifies pixel dimension of NIfTI object
    #Args:
    #  x: NIfTI object
    #  index: index of the pixel dimension to be modified
    #  new.d: new pixel dimension
    #
    #Returns:
    #  NIfTI object

    require(oro.nifti)
	pd <- pixdim(x)
	pd[index+1] <- new.d
	pixdim(x) <- pd
	return(x)

}
