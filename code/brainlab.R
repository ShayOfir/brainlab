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
# continue docummentation here...
#-------------------------------------------------
# List of Features to implement in LSM part:
#-------------------------------------------------
# 1. VLSM:
#   1.1 Perform VLSM using LESYMAP methods: BMfast, regresfast, chisq, etc and output the results
#   1.2 Perform VLSM using custom univariate methods: mann-whitney (classical and permutations based)
#   1.3 Perform VLSM using custom multivariate methods:
#       1.3.1 two-way repeated measures (permutations-based) with post-hoc permutations-based t-test;
#       1.3.2. MANOVA (permutations-based)
# 2. Perform VLSM with conjunction analysis (more than two behaviors and overlap)
#dependecies: fslr, oro.nifti, neurobase, stringr

if (!exists("s_"))
  source('/shared/code/quick_source.R') #This file is used for quick sourcing of brainlab.R from any path

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
		cat(sprintf('Wrong Z pixel dimension (%1.2f), changing into 1.5\n',oro.nifti::pixdim(x)[4]))
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
      wn(x,"tmp")
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
                         interim.output = TRUE)
{   
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
    
    if (tolower(realign)!='none'){
        
        cat(sprintf("Realigning brain and lesion using ANTs %s transformation...\n",realign.str))
        realign <- norm.brain.and.lesion(x = ds.x, template = temp, lesion = roi, out.file = "realigned.ds.x",method = realign.str)        
        if (interim.output){
            wn(realign$n.x,paste0(results.path,"/realign.ds.x.",postfix))
            wn(realign$n.lesion,paste0(results.path,"/realign.lesion.",postfix))
        }
        realign.ds.x <- realign$n.x
        #change the normalized lesion from "continous" to binary (aritfact of transformation):
        realign.lesion <- thres.lesion(realign$n.lesion,0.5)
    }
    else {
       realign.ds.x <- ds.x
       realign.lesion <- roi
       if (heal.op){            
            cat(sprintf("Warning: No pre-alignment was chosen, but healing is ON.\n"))
            cat(sprintf("Make sure that brain image AC-PC line aligned to (0,0,0)!\n"))
            cat(sprintf("Healing of non-aligned images may result in distorted results. Choose either pre-alignment or turn off healing.\n"))
       }
    }
	
    #3.b. Heal the lesion
    if (heal.op){
        cat(sprintf("Healing of lesion = ON...\n"))
        healed.realign.ds.x <- heal.brain(x = realign.ds.x, lesion = realign.lesion, smooth.factor = smooth.op)
		
		if (interim.output){
			wn(healed.realign.ds.x,paste0(results.path,"/heal.realign.ds.x.",postfix))
		}
    }
    else {
        cat(sprintf("Healing of lesion = OFF\n"))
        healed.realign.ds.x <- realign.ds.x #don't heal
    }
    #3.c. registration to template:
    cat(sprintf("Normalizaing brain and lesion using ANTs (method=%s)...\n",reg_method))
    nrm <- norm.brain.and.lesion(x = healed.realign.ds.x, template = temp, lesion = realign.lesion, out.file = "norm", method=reg_method)

    #4. Intersect with atlas
    #cat (sprintf("Calculation extent according to atlas: %s\n", atlas$name))
    #ROIs <- report.ROIs(atlas = atlas, n.lesion = nrm$n.lesion, q.thres = 0.9, out.file = report.fname)
    
    #5. Saving output files
    cat(sprintf("\nSaving results image files...\n"))

    options.df <- data.frame(atlas = atlas$name,
                             temp.op   = temp.op,
                             heal = heal.op,
                             smooth = smooth.op, 
                             deskull = deskull, 
                             fraq = fraq, 
                             pre.alignment = realign,
                             registration = reg_method,
                             interim.output = interim.output)

    write.csv(options.df,paste0(results.path,"/parameters_",postfix,".csv"))	
    wn(temp,paste0(results.path,"/template.",postfix))
    wn(nrm$n.x,paste0(results.path,"/norm.x.",postfix))
    wn(nrm$n.lesion,paste0(results.path,"/norm.lesion.",postfix))   
    cat(sprintf("Done.\n"))
    
    

}

load.atlas <- function(atlas.name, temp.op="atlas"){
    #load.atlas: Loads atlas
    #Args:
    #  atlas.name: name of atlas
    #  temp.op: template operation (atlas or template)
    #
    #Returns:
    #  atlas: atlas object
    #

    require(oro.nifti)
    atlas <- list()
    fnames <- NULL
    path = atlas.and.templates.folder    
	fnames <- retrieve.atlas.files(atlas.name)
    
    if (!is.null(fnames)){
        atlas$name <- atlas.name
        
       
        atlas$LUT = read.csv(paste0(path,fnames$LUT.file),header = TRUE) 
		if (temp.op == "atlas")
        {
			atlas$template = rn(paste0(path,fnames$template.file))
			atlas$img <- readNIfTI (paste0(path,fnames$image.file),reorient = F)
		}
		else
		{
			atlas$template <- rn(paste0(path,"/scct_stripped.nii")) 
			atlas$img <- readNIfTI (paste0(path,fnames$image.file.for.scct),reorient = F)
		}
    }

    return (atlas)

    
}


retrieve.atlas.files <- function (atlas.name){
    #retrieve.atlas.files: Retrieves atlas files
    #Args:
    #  atlas.name: name of atlas
    #
    #  Returns:
    #  fnames: list of filenames
	require(oro.nifti)
	
	
	path <- atlas.and.templates.folder
	atlas.df <- read.csv (file = paste0(path,'atlas_list.csv'),header = T)
	query <- atlas.df[atlas.df$atlas.name == atlas.name,]
	if (plyr::empty(query)){
		cat(sprintf ("Atlas named %s was not found in atlas lists. Please update ./Templates_Atlases/atlas_list.csv.\n",atlas.name))
		return(list())	
	}
	else
	{
		return (query)
	}
}

atlas_list <- function()
{
    #atlas_list: Lists atlases
    #Args:
    #   void
    #
    #Returns:
    #   list of atlases
	cat(sprintf('\n'))
	path <- atlas.and.templates.folder
	atlas.df <- read.csv (file = paste0(path,'atlas_list.csv'),header = T)
	for (nm in atlas.df$atlas.name){
		cat(sprintf('%s \n',nm))
	}
	return(as.character(atlas.df$atlas.name))
}

norm.brain.and.lesion <- function(x, template, lesion, out.file, method)
{
    #norm.brain.and.lesion: Normalizes brain image and lesion to template
    #Args:
    #  x: brain image
    #  template: template image
    #  lesion: lesion image
    #  out.file: output file name (prefix, normalized brain image and lesion files)
    #  method: method of transformation (see extrantsr::ants_regwrite documentation for the options)
    #
    # Returns:
    # out: list of normalized brain image and lesion
    #

    require(oro.nifti)
    require(extrantsr)

    wn(x,"tmp.x")
    wn(template,"tmp.template")
    out <- list()
    
    if (!missing(lesion))
    {
        wn(lesion,"tmp.lesion")
        n.lesion.fname = paste0(out.file,"_lesion.nii.gz")
        ants_regwrite(filename="tmp.x.nii.gz", outfile=paste0(out.file,".nii.gz"), retimg = F,template.file="tmp.template.nii.gz", other.files="tmp.lesion.nii.gz", other.outfiles=n.lesion.fname, typeofTransform = method) 
        n.lesion <- rn(n.lesion.fname)
        out$n.lesion <- n.lesion
        out$n.x <- rn(out.file)
        return(out)
    }
    else {
       ants_regwrite(filename="tmp.x.nii.gz", outfile=paste0(out.file,".nii.gz"), retimg = F,template.file="tmp.template.nii.gz",  typeofTransform = method) 
       out$n.x <- rn(out.file)
       out$n.lesion <- NULL
    }
        
}


thres.lesion <- function (lesion, t = 0){
    #thres.lesion: Thresholds lesion
    #Args:
    #  lesion: lesion image
    #  t: threshold (float, 0 - 1)
    #
    #Returns:
    #  new.les: thresholded lesion image
    #
    require(oro.nifti)
    new.les <- lesion
    if (missing(t)){
        t = 0
    }
    new.les[new.les <= t] = 0
    new.les[new.les > t] = 1
    return(new.les)
    
}



heal.brain <- function (x, lesion, out.file, smooth.factor = 1.5)
{
    #heal.brain: Heals brain image
    #Args:
    #  x: brain image
    #  lesion: lesion image
    #  out.file: output file name
    #  smooth.factor: smoothing factor (float, default = 1.5)
    #
    #Returns:
    #  healed: healed brain image
    #

    require(oro.nifti)
    require(neurobase)
    require(extrantsr)
    thres = 0.001
    cat(sprintf("Healing lesion with symetrical brain-matter\n"))
    mask = lesion < thres
    mask.flip = flip_img(mask,x=T)
    x.masked = x * mask
    cat(sprintf("Smoothing...\n"))
    if (missing(smooth.factor))
        {
            smooth.factor = 3; #Default
        }
    if (smooth.factor > 0){
            x.sm = smooth_image(x, sigma = 3, verbose = TRUE)
    }
    else {
       x.sm = x #Don't smooth
    }
    
    patch.origin = (!mask.flip) * x.sm
    patch = flip_img(patch.origin, x= T)
    healed = x.masked + patch
        if (!missing(out.file)){
        writeNIfTI(healed,out.file,gzipped=T)
    }
    return(healed) 
    
}

overlay_lesions <- function (behavior_df, folder, fn_out, subject_column = 'pcode', interim = FALSE, side_column = 'Side', which_side = c('L','R'), threshold = 0.5){
    #overlay_lesions: Overlays brain images
    #Args:
    #  df: data frame
    #  folder: folder
    #  fn_out: output filename
    #  subject_column: subject column
    #  track_lesion: track lesion
    #  side_column: side column
    #  filter_side: filter side
    #
    #Returns:
    #  sum.x: sum of brain images
    #
    df = read.csv(behavior_df, header = TRUE)
    xx <- list()
    
    # Filter subjects by side:
    if (is.null(which_side)){
        which_side <- c('L','R')
    }
    if (!(is.null(side_column))){
        cohort <- df[df[[side_column]] %in% which_side,]
    }
    
    cat(sprintf('Lesions folder: %s.\n',folder))
    # Load images:
    images <- paste0(cohort[[subject_column]],'.nii.gz')

    for (img in images){
        cat(sprintf('Overlaying %s...\n',img))
        xx[[img]] <- rn(paste0(folder,"/",img))
        xx[[img]] <- thres.lesion(xx[[img]], threshold)
    }

    # Sum images:
    sum.x <- xx[[1]]
    for (k in 2:length(xx)){
		sum.x <- sum.x + xx[[k]]
		if (interim){
			wn(x = sum.x, nam = paste0(fn_out,df[[subject_column]][k])) #This command is used for tracking the overlay, so one can identify images with wrong side labels
		}
    }

    wn(x = sum.x, nam = fn_out)
	wn(x = sum.x / length(xx), nam = paste0(fn_out,'_avg'))

	return(sum.x)
    
    
}


report.ROIs <- function (atlas, n.lesion, thres = 0.5, q.thres = NA, out.file){
    # Generate report of extent of involvement of each region of the atlas by the lesion
    # The lesion must mbe normalized first to atlas.img
    require(oro.nifti)

	
    atlas.img <- atlas$img
    atlas.LUT <- atlas$LUT
    
    if (!is.na(q.thres))
        {
            if (q.thres >=0 && q.thres <= 1)
                {thres = as.numeric(quantile(c(n.lesion[n.lesion>0]) > 0,q.thres))}                
        }
    #cat(sprintf("Using threshold = %1.3f",thres))
	
    roi.df = data.frame(index = c(atlas.img[n.lesion > 0]),roi = c(n.lesion[n.lesion>0]))
    #label_sums = ddply(.data=roi.df, .variables =  .(index),.fun =  summarize, sum_roi = sum(roi), sum_roi_thresh =sum(roi.df$roi > thres))
    eval(parse(text=paste0(" label_sums = plyr::ddply(.data=roi.df, .variables =  .(index),.fun =  summarize, nVoxelsAbove0 = sum(roi>0), nVoxelsAboveThres =sum(roi 
           > ",as.character(thres),"))"))) 
    label_sums = merge (label_sums, atlas.LUT, by="index") 
    sums = label_sums
    label_sums$Label <- label_sums$index <- NULL 
    atlas.tab <- as.data.frame(table(c(atlas.img))) #Creates as table with total size (in voxels) for each region
    colnames(atlas.tab) <- c("index","size")    
    region_pct <- merge(sums, atlas.tab, by="index")
    rownames(region_pct) <- sums$Label
    region_pct$Label <- region_pct$index
    
    region_pct$Extent.NonZero <- region_pct$nVoxelsAbove0 * 100 / region_pct$size 
    region_pct$Extent.AboveThres <- region_pct$nVoxelsAboveThres * 100 / region_pct$size

    if (!missing(out.file)){
        write.csv(region_pct,out.file,row.names=TRUE)
    }
	#save(roi.df,file = "debug.RData")
    return(region_pct)

}



flip_lesions <- function(behavior_fn, folder, subject_column = 'pcode', side_column = 'Side', dest_folder, flip_to = 'L')
{
	#Flip lesions to one side to enable flip-analysis
	#This practice may be controversial, but sometimes we don't have a choice
	
	#1. Load data-frame of patients names and their sides

	cohort <- read.csv(behavior_fn , header = T)
	for (p in 1:nrow(cohort))
	{
			fn <- paste0(folder,'/' ,cohort[[subject_column]][p], '.nii.gz') #We assume that files are .nii.gz files
			if (file.exists(fn))
			{
				out.fn <- paste0(dest_folder, '/', cohort[[subject_column]][p])
				if (!file.exists(dest_folder)) dir.create(dest_folder)
				if (cohort[[side_column]][p] != flip_to) #flip is required
					{
						cat(sprintf('Patient %s: flipped (side = %s)\n',cohort[[subject_column]][p],cohort[[side_column]][p]))
						lesion <- rn(fn) #Load lesion file
						flip.lesion <- flp(lesion)					
						wn(flip.lesion, out.fn)
					}
					else
					{
						cat(sprintf('Patient %s: no need to flip (side = %s), copying only...\n',cohort[[subject_column]][p],cohort[[side_column]][p]))
						system(paste('cp -v ',fn,dest_folder))
						
					}
			}
			else
			{
				cat(sprintf('Lesion file for patient %s was not found!\n',cohort[[subject_column]][p]))
			}
	}
		
}

# ------- Lesion load computation -----------------------

get_image_data <- function(img) {
  if (is(img, "antsImage")) {
    as.array(img)
  } else if (is(img, "nifti")) {
    img@.Data
  } else {
    stop("img must be either an oro.nifti or antsImage object.")
  }
}








load.SMATTS <- function (SMATTS.path, RST_type = c('none','inclusive','exclusive'))
{
	if (length(RST_type) > 1)
  {
    RST_type <- 'none'
    cat(sprintf('Multiple RST_type options provided, using default: %s\n',RST_type))
  }

	SMATTS <- list()
	tracts <- c('M1','S1','PMd','PMv','SMA','preSMA')
	sides <- c('Left','Right')
	for (sid in sides){
		SMATTS[[sid]] <- list()
		for (trct in tracts){
			cat(sprintf('Loading %s-%s...\n',sid,trct))
			fn <- paste0(sid,'-',trct,'-S-MATT')
			SMATTS[[sid]][[trct]] <- rn(paste0(SMATTS.path,'/',fn))
		}
	}
  if (RST_type != 'none')
   {
     cat(sprintf('Loading RST with type = %s...\n',RST_type))
     postfix <- if (RST_type == 'inclusive') '' else 'x'
     for (sid in sides){
       fn <- paste0(sid,'-CRP-RST',postfix)
       SMATTS[[sid]][[paste0('CRST',postfix)]] <- rn(paste0(SMATTS.path,'/',fn))
     }
  }
	
	
	return(SMATTS)
}


SMATTS.to.atlas <- function(SMATTS, side = c('Left','Right')){
	base_atlas <- load.atlas('BNA1mm') # uses the spm152
  # please beware that my 1mm atlses are aligned with spm152 and not with the mni152_T1_1mm template.
  # I do not recall the reason for this, but lesions as well as many atlases were aligned to this template and
  # it really doesn't worth the effort to re-align everything to mni152_T1_1mm.
	def.temp <- base_atlas$template
	sides <- names(SMATTS)
	tracts <- names(SMATTS[[sides[1]]])
	short.sides <- c('L','R')
	atlases <- list()
	sid.index <- 1
  if (!is.null(side)) sides <- side

	for (sid in sides){
		for (trct in tracts){
      if (length(sides) > 1)
			  { 
          a.name <- paste0('SM.',sid,'.',trct) }
      else
        { 
          a.name <- paste0('SM.',trct)
      }
 
			atlases[[a.name]]$name <- a.name
			atlases[[a.name]]$template <- def.temp
      atlases[[a.name]]$img <- SMATTS[[sid]][[trct]]
      atlases[[a.name]]$LUT <- data.frame(index = 1, 
                                          Label = trct,
                                          Side = sid[1],
                                          Label_NoSide = trct)
		}
	}
	return(atlases)
}




lateralize_atlas <- function(atlas_name, side = c('L','R')){
  require(dplyr)

  atlas <- load.atlas(atlas_name)

  if (!(side %in% unique(atlas$LUT$Side)))
  {
    stop(sprintf('Invalid side %s. Valid sides for atlas %s are: %s', side, atlas_name, paste(unique(atlas$LUT$Side), collapse = ', ')))
  }

  # Create a one-sided mask and atlas
  indices <- (atlas$LUT %>% dplyr::filter(Side != side))$index

  img <- atlas$img

  # zero out the non-relevant side:
  img[(atlas$img %in% indices)] <- 0

  # save as an atlas object:
  lat_atlas <- list(name = paste0(atlas$name,'_',side),
              LUT = atlas$LUT %>% filter(Side == side),
              template = atlas$template,
              img = img)

  return(lat_atlas)
}

prepare_atlases_for_LL <- function(){

  atlases <- list()
  sides <- c('L','R')

  for (side in sides){
    atlases[[side]] <- list(
      all = lateralize_atlas("HCPA_CST_all", side),
      radial = lateralize_atlas("HCPA_CST_radial", side),
      grid = lateralize_atlas("HCPA_CST_grid", side)
    )
  }

  return(atlases)
}


voxel_volume_cc <- function(nifti_obj) {

  if (is(nifti_obj, "antsImage")) {
    pd <- antsGetSpacing(nifti_obj)[1:3]
    vox_mm3 <- prod(pd)  # ANTsR always uses mm
  } else {
    spatial_unit <- bitwAnd(oro.nifti::xyzt_units(nifti_obj), 7L)
    pd <- oro.nifti::pixdim(nifti_obj)[2:4]
    vox_mm3 <- prod(pd) * switch(as.character(spatial_unit),
      "1" = 1e9,
      "2" = 1,
      "3" = 1e-9,
      1
    )
  }
  vox_mm3 / 1000  # mm^3 -> cc
}

compute_slice_overlap <- function(atlas, lesion) {
  require(dplyr)
  ROI_data    <- atlas$img@.Data
  lesion_data <- get_image_data(lesion) > 0  # binarize once
  
  n_slices    <- dim(ROI_data)[3]
  roi_indices <- sort(unique(ROI_data[ROI_data > 0]))
  xy_voxels   <- prod(dim(ROI_data)[1:2])

  # collapse x-y into rows, slices into columns: (xy × slices)
  roi_mat    <- matrix(ROI_data,    nrow = xy_voxels)
  lesion_mat <- matrix(lesion_data, nrow = xy_voxels)

  results <- lapply(roi_indices, function(idx) {
    roi_mask   <- roi_mat == idx                      # xy × slices logical
    slice_area <- colSums(roi_mask)
    n_voxel    <- colSums(roi_mask & lesion_mat)
    data.frame(
      ROI_index   = idx,
      axial_slice = seq_len(n_slices),
      n_voxel     = n_voxel,
      pct_voxel   = ifelse(slice_area > 0, n_voxel / slice_area, NA_real_),
      slice_area  = slice_area
    )
  })

  bind_rows(results) %>%
    filter(slice_area > 0) %>%
    mutate(ROI_name = atlas$LUT$Label_NoSide[match(ROI_index, atlas$LUT$index)])
}

compute_weighted_overlap <- function(overlap_df) {
    overlap_df <- overlap_df %>%
    dplyr::group_by(ROI_name) %>%
    dplyr::mutate(max_area = max(slice_area)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(weighted_overlap = n_voxel * max_area / slice_area)

    return(overlap_df)
}

compute_LL_by_slice <- function(overlap_df) {
    lesion_load_by_slice <- overlap_df %>%
      dplyr::group_by(axial_slice) %>%
      dplyr::summarise(weighted_sum = sum(weighted_overlap),
                       raw_sum = sum(n_voxel), .groups = "drop") %>%
      dplyr::arrange(.data$axial_slice)

    lesion_load_auc <- pracma::trapz(lesion_load_by_slice$axial_slice,
                                     lesion_load_by_slice$weighted_sum)

    return(lesion_load_by_slice)
}


compute_LL_metrics_per_ROI <- function(atlas, lesion) {
  require(pracma)
  require(dplyr)

  overlap_df <- compute_slice_overlap(atlas, lesion) %>%
                compute_weighted_overlap() 
  
  vox_cc <- voxel_volume_cc(lesion)

  lesion_load_by_slice <- overlap_df %>% compute_LL_by_slice()

  lesion_load_auc <- pracma::trapz(lesion_load_by_slice$axial_slice,
                                     lesion_load_by_slice$weighted_sum)

  metrics <- data.frame(
      total_pct             = sum(overlap_df$n_voxel) / sum(overlap_df$slice_area) * 100,
      max_pct               = max(overlap_df$pct_voxel) * 100,
      total_weighted        = lesion_load_auc,
      max_weighted          = max(lesion_load_by_slice$weighted_sum),
      total_volume          = sum(overlap_df$n_voxel) * vox_cc,
      total_weighted_volume = lesion_load_auc * vox_cc,
      max_weighted_volume   = max(lesion_load_by_slice$weighted_sum) * vox_cc
    )
  
  
  return(metrics)
}


compute_CST_LL <- function(lesion, CST_atlases, thres = 0.5, side = 'L', 
                           template = c('all', 'radial', 'grid', 'SMATT'), verbose = TRUE) {
  require(dplyr)
  
  require(pracma)
  
  template <- match.arg(template,   choices = c('all', 'radial', 'grid'))
  
  if (sum(lesion > 0) != sum(lesion > thres)) {
    show_info(sprintf('Lesion is not binary, applying thresholding (threshold=%1.2f)', thres), verbose = verbose)
    lesion <- thres.lesion(lesion, thres)
  }

  atlas <- CST_atlases[[side]][[template]]
  if (is.null(atlas)) {
    stop(sprintf('Atlas for template "%s" and side "%s" not found.', template, side))
  }

  overlap_df <- compute_slice_overlap(atlas, lesion) %>%
                compute_weighted_overlap() 
  
  vox_cc <- voxel_volume_cc(lesion)

  if (template == 'all') {
    
      lesion_load_by_slice <- overlap_df %>% compute_LL_by_slice()

      lesion_load_auc <- pracma::trapz(lesion_load_by_slice$axial_slice,
                                     lesion_load_by_slice$weighted_sum)

    out <- list(
      total_pct             = sum(overlap_df$n_voxel) / sum(overlap_df$slice_area) * 100,
      max_pct               = max(overlap_df$pct_voxel) * 100,
      total_weighted        = lesion_load_auc,
      max_weighted          = max(lesion_load_by_slice$weighted_sum),
      total_volume          = sum(overlap_df$n_voxel) * vox_cc,
      total_weighted_volume = lesion_load_auc * vox_cc,
      max_weighted_volume   = max(lesion_load_by_slice$weighted_sum) * vox_cc
    )
    out$lesion_load_by_slice <- lesion_load_by_slice
    
  } else {
    
    roi_df <- overlap_df %>%
      dplyr::group_by(ROI_name) %>%
      dplyr::summarise(
        total_pct             = sum(n_voxel) / sum(slice_area) * 100,
        max_pct               = max(pct_voxel) * 100,
        total_weighted        = pracma::trapz(axial_slice, weighted_overlap),
        max_weighted          = max(weighted_overlap),
        total_volume          = sum(n_voxel) * vox_cc,
        total_weighted_volume = pracma::trapz(axial_slice, weighted_overlap) * vox_cc,
        max_weighted_volume   = max(weighted_overlap) * vox_cc
      )

    perc_subsections_injured <- sum(roi_df$total_pct > 5) / nrow(roi_df) * 100

    out <- list(
      roi_df = roi_df,
      PS     = perc_subsections_injured
    )
  }

  out$overlap_df <- overlap_df
  return(out)
}

compute_ROI_lesion_load <- function(atlas, lesion, thres = 0.5) {
  require(dplyr)

  if (sum(lesion > 0) != sum(lesion > thres)) {
    lesion <- thres.lesion(lesion, thres)
  }

  overlap_df <- compute_slice_overlap(atlas, lesion)

  roi_summary <- overlap_df %>%
    dplyr::group_by(ROI_index) %>%
    dplyr::summarise(
      lesion_voxels = sum(n_voxel),
      total_area    = sum(slice_area)#,
      #.groups = "drop"
    ) %>%
    dplyr::mutate(lesion_frac = lesion_voxels / total_area)
  
  atlas$LUT %>%
    dplyr::left_join(roi_summary, by = c("index" = "ROI_index")) %>%
    dplyr::mutate(
      lesion_voxels = tidyr::replace_na(lesion_voxels, 0L),
      lesion_frac   = tidyr::replace_na(lesion_frac,   0)
    )
}



compute_CST_LL_for_cohort <- function(lesions_list,  cohort, subject_column = 'pcode', side_column = 'Side', verbose = TRUE)
{
    require(dplyr)
    require(purrr)

    CST_atlases <- prepare_atlases_for_LL()
    template_options <- c('all', 'radial', 'grid')
    results <- list()
    for (i in seq_along(cohort[[subject_column]])){
        pcode <- cohort[[subject_column]][i]
        side <- cohort[[side_column]][i]
        cat(sprintf('Computing CST-LL for patient %d (%d/%d)...\n',pcode, i, length(lesions_list)))

        #lesion <- extrantsr::ants2oro(lesions_list[[paste0(pcode)]])
        lesion <- lesions_list[[paste0(pcode)]]

        if (!is.null(lesion)){
            patient_result <- list()
            for (j in seq_along(template_options)){
                template <- template_options[j]
                patient_result[[template]] <- compute_CST_LL(lesion = lesion,
                                                            CST_atlases = CST_atlases,
                                                            side = side,
                                                            template = template,
                                                            verbose = verbose)
              }
            
            
            # build the new row:
            new_row <- list(
                    # Classic (Wiliamson et al., 2026) metrics:
                    pcode = pcode,
                    total_pct = patient_result$all$total_pct,
                    max_pct = patient_result$all$max_pct,
                    total_weighted = patient_result$all$total_weighted_volume,
                    max_weighted = patient_result$all$max_weighted_volume,
                    radial_PS = patient_result$radial$PS,
                    grid_PS = patient_result$grid$PS
            )

            # Individual sectors metrics (radial):
            
            for (sectors_template in c('radial', 'grid')){
              sector_metrics <- patient_result[[sectors_template]]$roi_df %>%
                dplyr::mutate(sector_num = stringr::str_extract(ROI_name, "\\d+")) %>% # Extract the numeric part
                dplyr::select(sector_num, total_pct, max_pct, total_weighted_volume, max_weighted_volume) %>%
                tidyr::pivot_wider(
                    names_from = sector_num,
                    values_from = c(total_pct, max_pct, total_weighted_volume, max_weighted_volume),
                    names_glue = paste0(sectors_template, "{sector_num}_{.value}"))
                new_row <- c(new_row, as.list(sector_metrics))
            }
                
        results[[length(results) + 1]] <- new_row   
        }
        else {
            cat(sprintf('Lesion for patient %d not found, skipping...\n',pcode))
        }
    }
    # Combine all rows into a single data frame
    results <- dplyr::bind_rows(results)
    return(results)
}

compute_ROI_LL_for_cohort <- function(lesions_list,
                                      cohort,
                                      atlas_names,
                                      subject_column = 'pcode',
                                      side_column    = 'Side',
                                      metric         = c('lesion_frac', 'lesion_voxels'),
                                      verbose        = TRUE) {
  require(dplyr)

  # prepare atlases:
  cat('Lateralizing atlases...\n')
  atlases_list <- list()

  
  for (atlas_name in atlas_names){
    atlases_list[[atlas_name]] <- list()
    for (side in c('L','R')){
      cat(sprintf('Atlas: %s, side: %s\n', atlas_name, side))
      atlases_list[[atlas_name]][[side]] <- lateralize_atlas(atlas_name, side)
  }
}
 
  metric <- match.arg(metric)

  results <- list()

  for (i in seq_along(cohort[[subject_column]])) {
    pcode  <- cohort[[subject_column]][i]
    side   <- cohort[[side_column]][i]
    lesion <- lesions_list[[as.character(pcode)]]

    if (is.null(lesion)) {
      show_info(sprintf('Lesion for patient %s not found, skipping...', pcode), verbose)
      next
    }

    show_info(sprintf('Computing ROI lesion load for patient %s (%d/%d)...',
                      pcode, i, nrow(cohort)), verbose)

    new_row <- list(pcode = pcode, side = side)

    for (atlas_name in names(atlases_list)) {

      
      ll_df <- compute_ROI_lesion_load(atlases_list[[atlas_name]][[side]], lesion)

      # build named vector: {atlas_name}_{Label_NoSide} -> metric value
      vals        <- setNames(ll_df[[metric]], paste0(atlas_name, '.', ll_df$Label_NoSide))
      new_row     <- c(new_row, as.list(vals))
    }

    results[[length(results) + 1]] <- new_row
  }

  dplyr::bind_rows(results)
}

# ------- Lesion Symptom Mapping -------------------------------------------------

get_subj_threshold <- function(minSubjectPerVoxel){
    if (!is.numeric(minSubjectPerVoxel) & is.character(minSubjectPerVoxel)) 
	{#2 # input is percentage
        if (! grepl('%', minSubjectPerVoxel) ) stop('minSubjectPerVoxel set as character with no % symbol, switch to numeric for exact number of subjects.')
        thresholdPercent = as.numeric(gsub('%','', minSubjectPerVoxel)) / 100
    }#2
	else if (is.numeric(minSubjectPerVoxel))
	{ #2
		# user defined exact subject number
		thresholdPercent = minSubjectPerVoxel / length(lesions.list)
	}#2

    return(thresholdPercent)
}

show_info <- function (txt, verbose){
    if (verbose){
        cat(sprintf('%s\n',txt))
    }
}

load_lesion_if_exists <- function (folder, pcode, format = 'ants'){
    require(ANTsR)

    fname <- file.path(folder, as.character(pcode))
    fn.nii <- paste0(fname,'.nii')
	  fn.nii.gz <- paste0(fname,'.nii.gz')	
    fn <- NULL
    if (file.exists(fn.nii)){
        fn <- fn.nii
    } else if (file.exists(fn.nii.gz)){
        fn <- fn.nii.gz
    } 

    if (!is.null(fn)){
      if (format == 'ants'){
        x <- antsImageRead(fn)}
      else {
        x <- rn(fn)
      }
    }
    else {
        x <- NULL
    }

    return(x)
}

load_behavior <- function(behavior)
{
     # 1. check that behavior is a data frame.
    if (!is.data.frame(behavior)){
       # check whether behavior is a string and if yes, if it exists:
       if (is.character(behavior) && file.exists(behavior)){
           behavior <- read.csv(behavior, header = TRUE)
       } else {
           stop('Behavior must be either data frame or a path to a CSV file.')
       }
    }
    return(behavior)
}

load_lesions <- function(behavior, folder, subject_column = 'pcode', lesion_threshold = 0, mask = NULL, verbose = TRUE){

    require(oro.nifti)
    require(ANTsR)
    require(extrantsr)
    
    #2. Check if folder exists:
    if (!dir.exists(folder)){
        stop(sprintf('Folder %s does not exist.', folder))
    }

    #3. Check if lesion_threshold is numeric and in range [0,1]:
    if (!is.numeric(lesion_threshold) || lesion_threshold < 0 || lesion_threshold > 1){
        stop('Lesion threshold must be a numeric value in range [0,1].')
    }

    #4. If mask is not NULL, check if it is a filename and if it exists:
    if (!is.null(mask)){
        if (is.character(mask) && file.exists(mask)){
            mask <- antsImageRead(mask)
        } 
        # if not, check if it is a NIfTI object:
        else if (is(mask, "nifti")){
            mask <- oro2ants(mask) #convert to ANTs image
        }
        else if (!is(mask, "ansImage")){
            show_info('Mask must be either a NIfTI object or a path to a NIfTI file.',verbose)
            mask <- NULL
        }
    }

    #5. Check if subject_column exists in behavior:
    if (!(subject_column %in% colnames(behavior))){
        stop(sprintf('Column %s does not exist in behavior data frame.', subject_column))
        
    }

    
    lesions <- list()
    for (i in 1:nrow(behavior)){
        pcode <- behavior[[subject_column]][i]
        show_info(sprintf('Load %s... [%d/%d], Thresholding with %1.2f threshold...',pcode,i,nrow(behavior),lesion_threshold), verbose)
        x <- load_lesion_if_exists(folder, pcode)
        if (!is.null(x)){
            #threshold lesion:
            lesions[[paste0(pcode)]] <- thres.lesion(x, lesion_threshold) #Threshold lesion

            # apply binary mask:
            if (!is.null(mask)){
                lesions[[paste0(pcode)]][mask == 0] <- 0 #apply mask
            }
            
        }
        else {
            show_info(sprintf('Lesion file was not found for patient %s.',pcode), verbose)
        }
    }
    return (lesions)
}

compute_patchinfo <- function(lesions_list, minSubjectsPerVoxel = '10%' , verbose = TRUE)
{
    require(oro.nifti)
    require (ANTsR)
    require (LESYMAP)

    # Error checking:
    # 1. Check if lesions_list is a list, and that it containts at least one ANTsImage object:
    if (!is.list(lesions_list) || length(lesions_list) == 0 || !all(sapply(lesions_list, is, "antsImage"))){
        stop('lesions_list must be a list of ANTsImage objects.')
    }


    #convert minSubjectsPerVoxel into thresholdPercent:
    thresholdPercent <- get_subj_threshold(minSubjectsPerVoxel)

    # compute average map
    avgles = antsAverageImages(lesions_list, verbose=FALSE) #average lesion

    # Remove voxels with too few, or too many, subjects
    mask = thresholdImage(avgles, thresholdPercent, 1 - thresholdPercent)

    if (thresholdPercent == 0) mask[avgles==0] <- 0

    show_info(sprintf('%d voxels were found.',sum(mask)), verbose)

    # check mask is not empty
    if (max(mask) == 0) show_info('Mask is empty. No voxels to run analysis on.', verbose)

    # compute unique patches

    show_info('Computing unique patches...', verbose)

    patchinfo = getUniqueLesionPatches(lesions_list, mask=mask,
                                showInfo = verbose, returnPatchMatrix = TRUE)
    lesmat = patchinfo$patchmatrix
    voxindx = patchinfo$patchindx
    voxmask = patchinfo$patchimg.samples

    patchimg <- ants2oro(patchinfo$patchimg) #it is easier to modify oro.nifti thans ANTs images
    lesmat.df <- as.data.frame(lesmat)

    n <- dim(lesmat)[2] #no. of patches
    lesmat.df <- as.data.frame(lesmat)
    names(lesmat.df) <- paste0('X',1:n)
    
	return(list(lesion_df = lesmat.df,
                patchinfo = patchinfo,
                mask = mask,
                avgles = avgles))		 
	    
}

create.empty.map <- function (from.map)
{
	z <- from.map
	z[TRUE] <- 0
  #browser()
	datatype(z) <- 16 #float
	bitpix(z) <- 32 #32 bits per voxel
	return(z)
}

map_stat <- function(patchimg, stat, out_fn = NULL) {
  statmap <- create.empty.map(patchimg) #patchimg is the output of getUniqueLesionPatches in oro format
  n_patches <- length(stat)
  for (j in 1:n_patches){
    if (stat[j] != 0){
      cat(sprintf('Computing patch %d...\n',j))
      statmap[patchimg == j] <- stat[j]
    }
  }
  if (!is.null(out_fn)){
    wn(abs(statmap), out_fn)
  }
  return(statmap)
}


LESYMAP_methods <- c('BMfast', 'regresfast', 'chisq','chisqPerm')
brainlab_univ_methods <- c('MW','MWperm')
brainlab_multiv_methods <- c('RM','MANOVA')
numeric_fields <- c('pThreshold', 'nperm', 'v')

analysis_fields_defaults <- list(method = 'BMfast',
                                 multipleComparison = 'none',
                                 pThreshold = 0.05,
                                 minSubjPerVoxel = '10%',
                                 nperm = 1000,
                                 v = 1,
                                idv_factor = 'lesion')

check_analysis_record <- function(analysis_record)
{
  # check if the analysis record has all the required fields
  # browser()
  for (field in names(analysis_fields_defaults)){
    if (!(field %in% names(analysis_record))){
      analysis_record[[field]] <- analysis_fields_defaults[[field]]
    }
    else{
      # check numeric fields:
      if (field %in% numeric_fields && !is.numeric(analysis_record[[field]])){
        analysis_record[[field]] <- as.numeric(analysis_record[[field]])
      }
    }
  }
  
  return(analysis_record)
}

invoke_lesymap_method <- function(method, patchinfo,  behavior_data, multipleComparison, pThreshold, nperm, v, verbose) 
{   
  lesmat = patchinfo$patchmatrix
  voxindx = patchinfo$patchindx
  voxmask = patchinfo$patchimg.samples

  if (method == 'BMfast'){
      #browser()
      lsm <- lsm_BMfast(lesmat = lesmat,
         behavior = behavior_data,
         FWERperm = (multipleComparison == 'FWERperm'),
         pThreshold = pThreshold,
         nperm = nperm,
         v = v,
         showInfo = verbose)
   }
    else if (method == 'regresfast'){
      lsm <- lsm_regresfast(lesmat, behavior,
                           FWERperm = (multipleComparison=='FWERperm'), nperm=nperm, pThreshold=pThreshold,
                             cPerm=(multipleComparison=='clusterPerm'), voxindx=voxindx, samplemask=voxmask,
                           showInfo=verbose)
      

    } else if (method == 'chisq'){
      lsm <- lsm_chisq(lesmat, behavior, runPermutations = FALSE)
    } else if (method == 'chisqPerm'){
       lsm <- lsm_chisq(lesmat, behavior, runPermutations = TRUE, nperm=nperm)
    } 
}

perform_VLSM_analysis <- function(patchinfo, mask, behavior, analysis_record, output_dir, verbose)
{
  # instructions contain:
  # idv_levels: independent variable levels (apart from lesion) a list of strings which correspond to column names. NULL = only lesion 
  # (=classical univariate VLSM)
  # idv_factor: independent variable name: string - name of the independent variable (e.g. 'direction')
  # dv: dependent variables: list of strings - column names
  # MANOVA: perform MANOVA on the dependent variables
  # method: string - method to use for VLSM (e.g. 'BMfast')
  # multipleComparison: string - method for multiple comparison correction (e.g. 'FWERperm')
  # pThreshold: numeric - p-value threshold for significance
  # v - if multipleComparison is 'FWERperm', this is the the rank of the extreme voxel to report (v=1 is the most extreme voxel)
  require(oro.nifti)
  require(ANTsR)
  require(extrantsr)
  require(LESYMAP)
  require(dplyr)

  lsm <- NULL # initialize lsm variable
  lesmat = patchinfo$patchmatrix
  voxindx = patchinfo$patchindx
  voxmask = patchinfo$patchimg.samples

  method = analysis_record$method
  multipleComparison = analysis_record$multipleComparison
  pThreshold = analysis_record$pThreshold
  nperm = analysis_record$nperm
  v = analysis_record$v

  
  #analysis_record <- check_analysis_record(analysis_record)


  # extract behavior columns:
    subset_behaviors <- NULL
    # check if there are behaviors column names which are not in the behavior table:
    for (col in analysis_record$dv){
      if (!col %in% colnames(behavior)){
        show_info(paste0('Behavior column ', col, ' not found in behavior table.'), verbose)
      }
      else {
        subset_behaviors <- c(subset_behaviors, col)
      }
    }

    # if there are no behaviors columns, return an error:
    if (length(subset_behaviors) == 0){
      stop('The column names in `dv` do not match any columns in the behavior table.\n dv: ',
           paste(analysis_record$dv, collapse = ', '),
           '\n behavior columns: ',
           paste(colnames(behavior), collapse = ', '))

    }

    # subset the behavior table to only include the specified behaviors:
    behavior_data <- behavior %>% select(all_of(c('pcode',subset_behaviors)))

    # check if there are any rows in the behavior data:
    if (nrow(behavior_data) == 0){
      stop('No rows in the behavior data after subsetting. Check the behavior table and the specified behaviors.')
    }
  
  if (method %in% LESYMAP_methods)
    # run the univariate analysis using LESYMAP's functions, for each dependent variable seperately:
    for (behav in subset_behaviors) {
      # if the behavior is a factor, convert it to a numeric variable
      if (is.factor(behavior_data[[behav]])) {
        behavior_data[[behav]] <- as.numeric(behavior_data[[behav]])
      }
      # run the analysis

      show_info(paste('Running VLSM analysis for behavior:', behav), verbose)
      lsm <- invoke_lesymap_method(
        method = method, 
        patchinfo = patchinfo, 
        behavior_data = behavior_data[[behav]], 
        multipleComparison = multipleComparison, 
        pThreshold = pThreshold, 
        nperm = nperm, 
        v = v,
        verbose = verbose
      )

      destpath = file.path(output_dir, behav)

      # save the results
      prepare_results(
        lsm = lsm,
        patchinfo = patchinfo,
        mask = mask,
        analysis_record = analysis_record,
        output_dir = destpath, 
        verbose = verbose
      )
    }
  
  
  return(lsm)
    
}

check_create_dir <- function(output_dir, verbose = TRUE)
 {
  if (!dir.exists(output_dir)) {
    if (verbose) show_info(paste('Creating output directory:', output_dir), verbose)
    dir.create(output_dir, recursive = TRUE)
  }
  return(output_dir)
}

parse_analysis_record <- function(analysis_record)
{
  method = analysis_record$method
  multipleComparison = analysis_record$multipleComparison
  pThreshold = analysis_record$pThreshold
  v = analysis_record$v
  return(stringr::str_replace_all(paste(method,'FWER=',multipleComparison,'p=',sprintf('%1.1f%%',pThreshold*100),'v=',sprintf('%d',v), sep = '_')," ",""))
}

prepare_results <- function(lsm, patchinfo, multipleComparison, mask, analysis_record, output_dir, verbose = TRUE)
{
  multipleComparison = analysis_record$multipleComparison
  pThreshold = analysis_record$pThreshold
  v = analysis_record$v

  voxindx = patchinfo$patchindx
  voxmask = patchinfo$patchimg.samples
  
  # START POST PROCESSING THE OUTPUT
    statistic = lsm$statistic

    if ('zscore' %in% names(lsm)) {
      haszscore = TRUE
      zscore = lsm$zscore
    } else { haszscore=FALSE }

    if ('pvalue' %in% names(lsm)) {
      haspvalue = TRUE
      pvalue = lsm$pvalue
    } else { haspvalue=FALSE }


    # multiple comparison correction
    if (haspvalue) {
      if (multipleComparison %in% p.adjust.methods ) {
        if (multipleComparison != 'none') show_info(paste('Correcting p-values:',multipleComparison,'...'), verbose)
        pvalue.adj = p.adjust(pvalue, method = as.character(multipleComparison))
        statistic[pvalue.adj>=pThreshold] = 0
        if (haszscore) zscore[pvalue.adj>pThreshold] = 0
      } else {
        pvalue.adj = pvalue
      }
    }
  
   # a final check to make sure there are no
    # infinite, na, or nan values
    if (any(is.nan(statistic))) show_info('WARNING: NaN values detected in statistic.', verbose)
    if (any(is.na(statistic))) show_info('WARNING: NA values detected in statistic.', verbose)
    if (any(is.infinite(statistic))) show_info('WARNING: Infinite values detected in statistic.', verbose)
    if (haspvalue && any(is.nan(pvalue.adj))) show_info('WARNING: NaN values detected in pvalues.', verbose)
    if (haspvalue && any(is.na(pvalue.adj))) show_info('WARNING: NA values detected in pvalues.', verbose)
    if (haspvalue && any(is.infinite(pvalue.adj))) show_info('WARNING: Infinite values detected in pvalues.', verbose)
    if (haszscore && any(is.nan(zscore))) show_info('WARNING: NaN values detected in zscore.', verbose)
    if (haszscore && any(is.na(zscore))) show_info('WARNING: NA values detected in zscore.', verbose)
    if (haszscore && any(is.infinite(zscore))) show_info('WARNING: Infinite values detected in zscore.', verbose)

   # put results in images
    show_info('Preparing images...', verbose)
    vlsm.stat = makeImage(mask, voxval=statistic[voxindx])
  
    # optional outputs
    if (haspvalue) vlsm.pval = makeImage(mask, voxval=pvalue.adj[voxindx])
    if (haspvalue) vlsm.pval[mask==0] = 1
    if (haszscore) vlsm.zmap = makeImage(mask, voxval=zscore[voxindx])
    
    # return results
    output = list(stat.img=vlsm.stat)
    # optional
    if (exists('vlsm.zmap')) output$zmap.img=vlsm.zmap
    if (exists('vlsm.pval')) output$pval.img=vlsm.pval

    output$mask.img=mask
    otherindx = (! names(lsm) %in% c('statistic', 'pvalue', 'zscore'))
    output = c(output, lsm[otherindx])
    output$patchinfo = patchinfo
    
    # save results
    # check if the VLSM results path exists, if not create it
    output_dir = check_create_dir(output_dir, verbose)
    
    # build name of sub-folder for the results according the analysis record:
    analysis_name = parse_analysis_record(analysis_record)
    output_path = file.path(output_dir, analysis_name)
    output_path = check_create_dir(output_path, verbose)

    # save the results to the output directory
    show_info('Saving results...', verbose)
    save.lesymap(lsm = output, saveDir = output_path, verbose = verbose)

   return(output) 

}


custom_filter <- function(df, filter_list) {
  require(dplyr)
  result <- list()

  for (i in seq_along(filter_list)) {
    condition <- filter_list[[i]]

    # Safely parse and evaluate the expression from string
    expr <- rlang::parse_expr(condition)
  
    # Apply filtering using tidy evaluation
    filtered_df <- df %>% filter(!!expr)

    result[[i]] <- filtered_df
  }

  return(result)
}

sanitize_filename <- function(name) {
  # Define mappings for operators and symbolic characters
  operator_map <- c(
    ">"  = "[gt]",
    "<"  = "[lt]",
    ">=" = "[ge]",
    "<=" = "[le]",
    "==" = "=",
    "!=" = "[ne]",
    "&"  = "[and]",
    "|"  = "[or]",
    "%in%" = "[in]",
    "*"  = "[star]",
    "+"  = "[plus]",
    "/"  = "",
    "\\" = "",
    "'"  = "",
    "\"" = "" # Remove quotes
  )

  # Reserved filenames on Windows
  reserved_names <- c("CON", "PRN", "AUX", "NUL",
                      paste0("COM", 1:9),
                      paste0("LPT", 1:9))

  cleaned <- trimws(name)

  # Replace operators/symbols using map
  for (op in names(operator_map)) {
    cleaned <- gsub(op, operator_map[op], cleaned, fixed = TRUE)
  }

  # Replace any remaining invalid characters
  cleaned <- gsub("[<>:\"/\\\\|?*]", "_", cleaned)

  # Protect reserved filenames
  if (toupper(cleaned) %in% reserved_names) {
    cleaned <- paste0(cleaned, "_safe")
  }

  return(cleaned)
}


invoke.fsl.cluster <- function (zmap, z.threshold)
{
	#wrapper function getting ready-cluster table from fsl_cluster()
	require(fslr)
	clusters <- list()
	if (all(zmap == 0)) #Empy map
	{
		clusters$tab <- data.frame()
		clusters$img <- zmap
	}
	else
	{
	wn(zmap,'tmp.zmap')
	
	clust <- fsl_cluster('tmp.zmap',threshold = z.threshold) #fslr package
	tab <- read_cluster_table(clust$outfile) #flsr package
	
	if (dim(tab)[1] <= 15) {
		clusters$tab <- data.frame()
		clusters$img <- zmap
		}
		else {
			clusters$tab <- retrieve.clusters (tab, zmap)
			clusters$img <- rn(clust$oindex)
		}
	}
	return(clusters)
}


report.max.z <- function (atlas, region, cluster, option = 'max')
{
	#browser()
	cluster[atlas$img!=region] <- 0
	out <- list()
	
	if (option == 'min')
		out$Zmax <- min(cluster[cluster>0])
		else
		out$Zmax <- max(cluster)
	pos <- which(cluster == out$Zmax, arr.ind = T)
	
	out$xyz <- mni.coordinates(pos, cluster)
	#out$cluster.size <- sum(cluster == out$Zmax)
	
	return(c(out$Zmax[1], out$xyz[1,]))
	
}


report.clusters <- function (zmap, z.threshold, atlases, conjunction.map = F, out.fn = NULL, option = 'max')
{
	
	require (dplyr)
	if (all(zmap==0))
	{
	
	cat(sprintf('No significant voxels.\n'))
	out <- NULL
	}
	else{
	cat(sprintf('Significant voxels were found.\n'))
	#browser()
	clusters.img <- list() #will contain an image with values corresponding to cluster codes
	if (!(conjunction.map)){
		clusters<- invoke.fsl.cluster(zmap, z.threshold)
		clusters.tab <- clusters$tab
		clusters.img[[1]] <- clusters$img
		zmaps <- list()
		zmaps[[1]] <- zmap
		}
		else
		{
			#browser()
			mx <- max(zmap)
			#Find the codes that exist in zmap
			codes <- rep(0,mx)
			for (j in 1:mx){
				if (sum(zmap[zmap==j])>0)
					{
						codes[j] = 1
					}
				}
			#For each code, threshold the zmap such as zmap==code and then compute clusters
			zmaps <- list()
			clusters.tab <- data.frame()
			for (j in 1:mx){
				if (codes[j]){
					#code exists
					#cut out the cluster with j and binarize it
					zmaps[[j]] <- zmap
					zmaps[[j]][zmap != j] <- 0
					#zmaps[[j]][zmap == j] <- 1
					#browser()
					clusters <- invoke.fsl.cluster(zmaps[[j]], j)
					clusters.tab <- bind_rows(clusters.tab, clusters$tab)
					clusters.img[[j]] <- clusters$img
				}
			}
			
	
		}
		
		#Intersect with atlases
		#browser()	
		grand.tab <- list()
		
		color.index <- 0
		
		cat(sprintf('Generating clusters report by ROIs...\n'))
		for (clst in clusters.img){	
			if (is.null(clst))
				{
					next
				}
			color.index <- color.index + 1
			grand.tab[[color.index]] <- list()
			cluster.codes <- 1:max(clst)
			for (j in cluster.codes){	
				grand.tab[[color.index]][[j]] <- list()	
				#isolate cluster j
				a.clst <- clst
				a.clst[clst != j] <- 0
				a.clst[clst == j] <- 1				
				for (atlas in atlases){
					#cat(sprintf('Computing clusters for atlas %s\n',atlas$name))
					#browser()

					ROIs1 <- report.ROIs(atlas, a.clst) 
					subset.cols <- c("Label","nVoxelsAbove0","Extent.NonZero")
					subset.flag <- TRUE
					for (col.name in subset.cols){
						if (!(col.name %in% names(ROIs1))) subset.flag <- FALSE
					}
					if(subset.flag){
						ROIs2 <- ROIs1 %>% select("Label","nVoxelsAbove0","Extent.NonZero") %>% filter(Label > 0)
					}
					else {
								cat(sprintf(('No significant voxels..., skipping.\n')))
								next
					}

					
					ROIs <- cbind(ROI.name = row.names(ROIs2),ROIs2)
					if (dim(ROIs)[1] > 0){
						ROIs <- ROIs %>% mutate(atlas = atlas$name, cluster.index = j)
						clust.zmap <- zmap
						clust.zmap[!a.clst] <- 0
						clust.zmap[a.clst] <- 1
						Zmax <- matrix(nrow = dim(ROIs)[1], ncol = 5) #Zmax, X, Y, Z, Cluster-Size
						nxt <- 1						
						for (region in ROIs$Label){
								
								zz <- report.max.z(atlas, region, clust.zmap, option = option)
								zz <- c(zz,sum(a.clst))
								Zmax[nxt,] <- zz
								nxt <- nxt + 1

						}
						
						#browser()
						Zmax.df <- as.data.frame(Zmax)
						Zname <- paste0('Z',option)
						names(Zmax.df) <- c(Zname,'X','Y','Z','Cluster.Size')
						ROIs2 <- bind_cols(ROIs, Zmax.df)
						grand.tab[[color.index]][[j]][[atlas$name]] <- ROIs2
					}
				}				
			}
		}
		#browser()
		tab <- data.frame()
		for (tb1 in grand.tab){
			for (tb2 in tb1){
				for (tb3 in tb2){
					if (dim(tb3)[1] > 0){
						tab <- bind_rows(tab, tb3)
					}
				}
			}
		}
		#browser()
		if (length(atlases)==0)
		{
			out <- clusters.tab
		}
		else
		{
			out <- tab
		}
		
		if (!is.null(out.fn))
		{
			write.csv(out,file=out.fn)
		}
		
		}
		
		
	
	return (out)
}
	

perform_LSM_for_cohort <- function(behavior,
                        folder,
                        analysis_plan,
                        output_dir,
                        dv = NULL,
                        subject_column = 'pcode',
                        lesion_threshold = 0,
                        minSubjectsPerVoxel = '10%',
                        pre_mask = NULL,
                        verbose = TRUE)
{
  # GENERAL PROCESS:
  #    INPUT:
  #    OUTPUT:



  # 1. Load the lesions and threshold them
  #    INPUT: 
  #           folder: path to the lesion images
  #           behavior: table with patients codes and behavior information
  #           subject_column: column with patient codes in the behavior table
  #    OUTPUT:
  #           list of ANTs lesion images
  lesions <- load_lesions(behavior = behavior,
                          folder = folder,
                          subject_column = subject_column,
                          lesion_threshold = lesion_threshold,
                          mask = pre_mask,
                          verbose = verbose)
  


  #2.  Compute patch-info 
  patch_info_list <- compute_patchinfo(lesions, minSubjectsPerVoxel = minSubjectsPerVoxel, verbose = verbose)
  patchinfo <- patch_info_list$patchinfo
  mask <- patch_info_list$mask
    
  #3. Perform the analysis according to the analysis plan
  for (row_num in (1:nrow(analysis_plan))) {
    analysis_record <- check_analysis_record(as.list(analysis_plan[row_num, ]))
    show_info(paste('Performing analysis:', parse_analysis_record(analysis_record)), verbose)
    # check if dependent variables (dv) are provided, if yes, overide them in an analysis record:
    
    analysis_record$dv <- dv
        
    # Perform the VLSM analysis

    lsm <- perform_VLSM_analysis(
      patchinfo = patchinfo,
      mask = mask,
      behavior = behavior,
      analysis_record = analysis_record,
      output_dir = output_dir,
      verbose = verbose
    )

    # Prepare the results and save them
    output <- prepare_results(
      lsm = lsm,
      patchinfo = patchinfo,
      multipleComparison = analysis_record$multipleComparison,
      mask = mask,
      analysis_record = analysis_record,
      output_dir = folder,
      verbose = verbose
    )
      
    # Create clusters report
  #   cat(sprintf('Computing clusters for single behavior: %s... ',filt.behave[zmp.indx]))
	# report.clusters (zmap = rn(paste0(input.folder,'/',zmp)), conjunction.map = FALSE, z.threshold = zthreshold[zmp.indx], out.fn = paste0(output.folder,filt.behave[zmp.indx],'_clusters.csv'), atlases = atlas.list)

  }
  

}

perform_LSM <- function(behavior,
                        filter_cohort,
                        folder,
                        analysis_plan,
                        output_dir = 'VLSM_results',
                        dv = NULL,
                        subject_column = 'pcode',
                        lesion_threshold = 0,
                        minSubjectsPerVoxel = '10%',
                        pre_mask = NULL,
                        verbose = TRUE)
{
# cohort is a list of fields and conditions for filtering. 
# Every cell in the list corresponds to one cohort. 
# In case more complex conditions are required, use list recursively.
# For example:
# cohort = list(Side = "=='L'", Side = "=='R'") will create two cohorts filtered by Side
# cohort = list(list(FMA = ">= 10", FMA = "<= 45", improve = "==1")) will filter cohort by FMA >= 10, FMA <= 45 and improve == 1
# The condition string could be anything that dplyr::filter can understand.
  require(oro.nifti)
 
    
 # Check if analysis_plan is a data frame, if not check if it is a filename and if yes check it exists and if yes try to read it as csv:
  if (!is.data.frame(analysis_plan)) {
    if (is.character(analysis_plan) && file.exists(analysis_plan)) {
      analysis_plan <- read.csv(analysis_plan, header = TRUE)
    } else {
      stop('analysis_plan must be a data frame or a path to a CSV file.')
    }
  }
    
 # Load behavior:
  behavior <- load_behavior(behavior)
  
  # Check if filter_cohort is a list, if not raise error
  if (!is.list(filter_cohort)) {
    stop('filter_cohort must be a list of conditions for filtering the cohort.')
  }
    else{
      # decode and filter the behavior data frame
      cohorts<- custom_filter(behavior, filter_cohort)
  }
    
  for (i in seq_along(cohorts)) {
       cohort <- cohorts[[i]]
       cohort_name <- stringr::str_replace_all(filter_cohort[[i]]," ","") # remove spaces from the cohort name
       # Create output directory for the cohort:
       folder_name <- sanitize_filename(cohort_name) # sanitize the cohort name to make it a valid folder name      
       check_create_dir(output_dir)
       
       output_path <- file.path(output_dir, folder_name)
      
       check_create_dir(output_path, verbose = verbose)

       cat(sprintf('Performing LSM for cohort %s...\n', cohort_name))

       perform_LSM_for_cohort(behavior = cohort,
                          folder = folder,
                          analysis_plan = analysis_plan,
                          output_dir = output_path,
                          dv = dv,
                          subject_column = subject_column,
                          lesion_threshold = lesion_threshold,
                          minSubjectsPerVoxel = minSubjectsPerVoxel,
                          pre_mask = pre_mask,
                          verbose = verbose)
  }
}
 



mni.coordinates <- function (coor, x, distance = FALSE)
{
	L <- dim(coor)[1]
	scalar = FALSE
	if (is.null(L)) #scalar
	{
		L <- 1
		scalar = TRUE
	}
	Ncol = 3
	if (distance)
	{
		Ncol = 4
	}
	out.coor <- matrix(nrow= L, ncol = Ncol)
	for (j in 1:L){
		if (scalar){
			xyz <- coor
			}
			else{
				xyz <- coor[j,]
			}
	
		out.coor[j,1:3] = oro.nifti::translateCoordinate(i=xyz, nim = x)
		if (distance) {out.coor[j,4] = sqrt(sum(xyz^2))}
	}
	return(out.coor)
}
