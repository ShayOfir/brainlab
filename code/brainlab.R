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

warp.transform.8bit <- function (folder,fn)
{
	#warp.transform.8bit: transforms a NIfTI brain image from 8-bit. 
    #   The convereted file be saved in the same directory with '.transform' postfix.
    #Args:
    #       folder : path to file
    #       fn : filename of brain image to transform
    #
    #Returns: void
    full.fn <- paste0(folder,'/',fn)
	x <- rn(full.fn);
	y <- transform.8bit(x)
    file_name <- decompose_filename(fn)[1]
	new.fn <- paste0(folder,'/','transform.',file_name)
	wn(y,new.fn)
}

transform.8bit <- function (x)
{
    #transform.8bit: Transforms NIfTI object from 8-bit NIfTI object
    #Args:
    #       x: NifTI object
    #
    #Returns:
    #       new.x : transformed NIfTI object
	sz = dim(x)
	LO = -1000
	dat <- x@.Data
	new.dat <- dat
	new.dat[dat>=50] = new.dat[dat>=50]*0.6
	for (slc in 1:sz[3]){
	cat(sprintf("Slice %d/%d\n",slc,sz[3]))
		for (r in 1:sz[2]){
			
			#first side
			k <- 1
			while (dat[k,r,slc] == 0 && k < sz[1]){
				new.dat[k,r,slc] <- LO
				k <- k + 1
			}
			k <- sz[1]
			while(dat[k,r,slc]==0 && k > 1){
				new.dat[k,r,slc] <-LO
				k <- k - 1
			}
		}
	}
	new.dat = new.dat + 1000
	new.dat[dat >= 250] = 2900
	new.x <- x
	new.x@.Data <- new.dat
	return(new.x)
	
}

decompose_filename <- function (fn){
    #decompose_filename: Decomposes filename into filename and extension
    #Args:
    #       fn: filename
    #
    #Returns:array of string [<filename>, <extension>]
    #
    base_name <- tools::file_path_sans_ext(fn)
    ext <- tools::file_ext(fn)
    if (grepl("\\.gz$", fn)) {
        ext <- paste0(tools::file_ext(base_name), ".", ext)
        base_name <- tools::file_path_sans_ext(base_name)
    }
    return(c(base_name, ext))
}

warp.deskull.brain <- function (pfolder, fname, ct.op = TRUE, fraq = 0.4)
{
    #warp.deskull.brain: Deskulls brain image
    #Args:
    #       pfolder: path to file
    #       x: NIfTI object
    #       ct.op: TRUE= CT, FALSE = MRI (T1)
    #       fraq: fraction of brain to keep
    #       more.opts: additional options for BET
    #
    #Returns:
    #       y: deskulled NIfTI object
    #
    full.fn <- paste0(pfolder,'/',fname)
    x <- rn(full.fn)
    file_name <- decompose_filename(fname)[1]
    ds.x <- deskull.brain(x, ct.op = ct.op, fraq = fraq)
    new.fn <- paste0(pfolder,'/ds.',file_name)
    wn(ds.x,new.fn)
    return(new.fn)
}   

deskull.brain <- function (x, ct.op = TRUE, fraq = 0.4, more.opts)
{
    #Deskull: Use BET to skull-strip brain.
    #Use:
    #
    #   ds.x <- Deskull(x, ct.op = TRUE)
    #
    # INPUT:
    #        x - NIfTI object, brain with skull
    #        ct.op - TRUE= CT, FALSE = MRI (T1)
    #
    # OUTPUT:
    #         ds.x - NIfTI object, deskulled brain 
    #
    # Recieves 'x' (NIfTI object) and output deskulled NIfTI object
    require(fslr)
    require(oro.nifti)
	

	
    if (ct.op){

        mx = max(x)
        mn = min(x)
        
        
        
        if (mx-mn < 1999){
            cat(sprintf('It seems that image is 8-bit, attempting transformation. If this does not work, try different transform.\n'))
            x <- transform.8bit(x)
            mx = max(x)
            mn = min(x)
        }
        if (mn < -1024){
            x[x < (-1024)] = -1024
            mn = min(x)
        }
        
        range = mx-mn
        cat(sprintf("intensity range: %d\n",round(range)))
        #constants for conversions
        kUDrak = 900 #Uninteresting Dark units
        kIMid = 200 #Interesting Mid units 
        kScaleRatio = 10 #increase dynamic range of interesting voxels by 3
        #convert image
        x = x - mn #translocate so min value is 0
        extra1 = x - kUDrak
        extra1[extra1 <= 0] = 0; #clip dark to 0
        extra9=extra1
        extra9[extra9 > kIMid] = kIMid #clip bright
        extra9 = extra9 * (kScaleRatio - 1) #boost mid range
        #transform image
        #browser()
        x = x + extra1
        x = x + extra9 #dark + bright + boostedMidRange
        x[x>3000] = 100 #Remove bone
        #save to file
        wn(x,"tmp")
        # run BET
        if (missing(fraq))
          {
            fraq = 0.4
          }
        if (missing(more.opts))
          {
            more.opts = " -v"
          }
        op.str <- paste0(" -f ",fraq," ",more.opts)
        fslbet(infile="tmp.nii.gz", outfile = 'ds.tmp.nii.gz', opts=op.str,retimg = FALSE)
        y <- rn('ds.tmp.nii.gz')
    }
    else
    {
      fw (x,"tmp")
      if (missing(fraq))
      {
        fraq = 0.5
      }
      if (missing(more.opts)){more.opts = " -v"}
      op.str <- paste0(" -f ",fraq," ",more.opts)
      y = fslbet("tmp.nii.gz",betcmd="bet",retimg = T,opts = op.str)
    }

    return(y)
}