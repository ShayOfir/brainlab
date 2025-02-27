function normalize_brain (patient_folder, x_fname, lesion_fname, file_format, atlas_name, is_ct, temp_op, heal_op, smooth_op, realign_op, reg_method, interim_output)
%warp.norm.brain <- function (patient.folder, p.fname, lesion.fname, file.format = 'nii', atlas.name = 'AAL', is.ct = TRUE, temp.op = "scct", heal.op = TRUE, smooth.op = 1.5){
pfolder = quote(win2lin([patient_folder,'\'],'full'));
xfname = quote(win2lin(x_fname));
lesfname = quote(win2lin(lesion_fname));
fileformat = quote(file_format);
atlasname = quote(atlas_name);
isct = num2str(is_ct);
tempop = quote(temp_op);
healop = num2str(heal_op);
smoothop = num2str(smooth_op);
realignop = quote(realign_op);
regmethod = quote(reg_method);
interimoutput = num2str(interim_output);
deskullop = num2str(0); %turns auto-deskull off, since this step requires eyeballing


% #Normalize brain image and lesion to template.
%     #Args:
%     #  patient.folder: path to patient folder
%     #  p.fname: brain image filename
%     #  lesion.fname: lesion filename
%     #  file.format: file format (nii or hdr)
%     #  atlas.name: name of atlas
%     #  is.ct: TRUE= CT, FALSE = MRI (T1)
%     #  temp.op: template operation (atlas or template)
%     #  heal.op: TRUE= heal lesion, FALSE = don't heal
%     #  smooth.op: smoothing factor
%     #  deskull: TRUE= deskull, FALSE = don't deskull
%     #  fraq: fraction of brain to keep
%     #  realign: pre-alignmemt method (Rigid, Affine of no-prealign)
%     #  reg_method: registration method (SyN or Affine)
%     #  interim.output: TRUE= save interim output, FALSE = don't save
Rpipe('norm.brain','kw',{...
     'patient.folder',pfolder,...
     'p.fname', xfname,...
     'lesion.fname', lesfname,...
     'file.format', fileformat,...
     'atlas.name', atlasname,...
     'is.ct',isct,...
     'temp.op',tempop,...
     'heal.op', healop,...
     'smooth.op', smoothop,...
     'deskull', deskullop,...
     'realign', realignop,...
     'reg_method', regmethod,...
     'interim.output',interimoutput...
     })
                        