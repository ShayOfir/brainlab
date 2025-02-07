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

warp.norm.brain <- function (patient.folder, p.fname, lesion.fname, file.format = 'nii', atlas.name = 'AAL', is.ct = TRUE, temp.op = 'atlas', heal.op = TRUE, smooth.op = 1.5, deskull=TRUE, fraq = 0.5){
    #load("PipelineData.RData")
    #pd <- pipeline.data
	browser()
    norm.brain(patient.folder, p.fname, lesion.fname, file.format, atlas.name, is.ct, temp.op, heal.op, smooth.op, deskull, fraq)
}

norm.brain <- function (patient.folder,
                         p.fname, 
                         lesion.fname, 
                         file.format = "nii", 
                         atlas.name="AAL",
                         is.ct = TRUE, 
                         temp.op = 'atlas',
                         heal.op = TRUE, 
                         smooth.op = 1.5, 
                         deskull=TRUE, 
                         fraq=0.5, 
                         realign = 'Rigid',
                         reg_method = 'SyN',
                         interim.output = TRUE){   
    #Normalize brain image and lesion to template.
    #Args:
    #  patient.folder: path to patient folder
    #  p.fname: brain image filename
    #  lesion.fname: lesion filename
    #  file.format: file format (nii or hdr)
    #  atlas.name: name of atlas
    #  is.ct: TRUE= CT, FALSE = MRI (T1)
    #  temp.op: template operation (atlas or template)
    #  heal.op: TRUE= heal lesion, FALSE = don't heal
    #  smooth.op: smoothing factor
    #  deskull: TRUE= deskull, FALSE = don't deskull
    #  fraq: fraction of brain to keep
    #  realign: pre-alignmemt method (Rigid, Affine of no-prealign)
    #  reg_method: registration method (SyN or Affine)
    #  interim.output: TRUE= save interim output, FALSE = don't save

    p.name <- basename(p.fname)
    x.fname <- paste0(patient.folder,p.fname)
    roi.fname <- paste0(patient.folder,lesion.fname)    
    results.path = paste0(patient.folder);
    atlas <- load.atlas(atlas.name,temp.op)
	temp <- atlas$template
    report.fname <- paste0(results.path,'/',p.name,".",atlas$name,".",temp.op,".csv")
    postfix <- paste0(p.name,".",atlas$name)

    #1. Load files and convert to ANALYZE if needed
    if (tolower(file.format)=="hdr"){ 
		cat(sprintf("Loading ANALYZE files: brain = %s, lesion = %s...\n",x.fname,roi.fname))
		x <- flp(fslr::fslroi (file = x.fname)) #This methods was the only to work. Equivalently, use "crop" tool from 'fsleyes' GUI
		roi <- flp(fslr::fslroi(file = roi.fname))
		#Save Temporary files
		wn(x,"x") 
		wn(roi,"lesion")
	

    }
    else
    {
        cat(sprintf("Loading NIfTI files: brain = %s, lesion = %s...\n",x.fname,roi.fname))
        x <- rn(x = x.fname)
        roi <- rn(x = roi.fname)
    }

    #2. Deskull x
    if (deskull){
        cat(sprintf("Skull stripping...\n"))
        ds.x <- deskull.brain(x, ct.op=is.ct, fraq=fraq, more.opts = "-v")  
		if (interim.output){
            wn(ds.x,paste0(results.path,"/ds.x.",postfix))
        }
    }
    else {
       ds.x <- x

    }
    wn(ds.x,"ds.x")

    #3. Register brain image and lesion to template
    #3.a. Realign brain image and lesion
	cat(sprintf('Template chosen: %s\n',temp.op))
	realign.str <- realign
    
    if (lower(realign)!='no-prealign'){
        
        cat(sprintf("Realigning brain and lesion using ANTs %s transformation...\n",realign.str))
        realign <- norm.ct(x = ds.x, template = temp, lesion = roi, out.file = "realigned.ds.x",method = realign.str)        
        if (interim.output){
            wn(realign$n.x,paste0(results.path,"/realign.ds.x.",postfix))
            wn(realign$n.lesion,paste0(results.path,"/realign.lesion.",postfix))
        }
        realign.ds.x <- realign$n.x
        #change the normalized lesion from "continous" to binary (aritfact of transformation):
        realign.lesion <- thres.lesion(realign$n.lesion,0.5)
    }
    else {
       realign <- list() 
       realign$n.x <- ds.x
       realign$n.lesion <- roi
       if (heal.op){            
            cat(sprintf("Warning: No pre-alignment was chosen, but healing is ON."))
            cat(sprintf("Make sure that brain image AC-PC line aligned to (0,0,0)!"))
            cat(sprintf("Healing of non-aligned images may result in distorted results. Choose either pre-alignment or turn off healing."))
       }
    }
	
    #3.b. Heal the lesion
    if (heal.op){
        cat(sprintf("Healing of lesion = ON...\n"))
        healed.realign.ds.x <- heal.CT(x = realign.ds.x, lesion = realign.lesion, smooth.factor = smooth.op)
		
		if (interim.output){
			wn(healed.realign.ds.x,paste0(results.path,"/heal.realign.ds.x.",postfix))
		}
    }
    else {
        cat(sprintf("Healing of lesion = OFF"))
        healed.realign.ds.x <- realign.ds.x #don't heal
    }
    #3.c. registration to template:
    cat(sprintf("Normalizaing brain and lesion using ANTs (method=%s)...\n",reg_method))
    nrm <- norm.ct(x = healed.realign.ds.x, template = temp, lesion = realign.lesion, out.file = "norm", method=reg_method)

    #4. Intersect with atlas
    #cat (sprintf("Calculation extent according to atlas: %s\n", atlas$name))
    #ROIs <- report.ROIs(atlas = atlas, n.lesion = nrm$n.lesion, q.thres = 0.9, out.file = report.fname)
    
    #5. Saving output files
    cat(sprintf("\nSaving NIFTI files...\n"))

    options.df <- data.frame("atlas" = atlas$name,
                            "temp.op" = temp.op,
                             "heal" = heal.op,
                             "smooth" = smooth.op, 
                             "deskull" = deskull, 
                             "fraq" = fraq, 
                             "pre-alignment" = realign,
                             "registration" = reg_method,
                             "interim.output" = interim.output)

    write.csv(options.df,paste0(results.path,"/parameters_",postfix,".csv"))	
    wn(temp,paste0(results.path,"/template.",postfix))
    wn(nrm$n.x,paste0(results.path,"/norm.x.",postfix))
    wn(nrm$n.lesion,paste0(results.path,"/norm.lesion.",postfix))   
    cat(sprintf("Done.\n"))
    
    

}