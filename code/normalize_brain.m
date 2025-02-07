function normalize_brain (patient_folder, x_fname, lesion_fname, file_format, atlas_name, is_ct, temp_op, heal_op, smooth_op, fraq)
%warp.norm.brain <- function (patient.folder, p.fname, lesion.fname, file.format = 'nii', atlas.name = 'AAL', is.ct = TRUE, temp.op = "scct", heal.op = TRUE, smooth.op = 1.5){
pfolder = quote(win2lin([patient_folder,'\'],'full'));
xfname = quote(win2lin(x_fname));
lesfname = quote(win2lin(lesion_fname));
fileformat = quote(file_format);
%If there is more than one atlas-name, send their names in a R-styled
%vector of strings, e.g.: "c('AAL','JHU')"
% if iscell(atlas_name)
%     nA = length(atlas_name);
%     atlasname = 'c(';
%     for k=1:nA
%         atlasname = [atlasname,quoate(atlas_name{k})];
%         if k < nA
%             atlasname = [atlasname,','];
%         else
%             atlasname = [atlasname,')'];
%         end
%     end
% else
    atlasname = quote(atlas_name);
% end

isct = num2str(is_ct);
tempop = quote(temp_op);
healop = num2str(heal_op);
smoothop = num2str(smooth_op);
deskullop = num2str(1);
frq = num2str(fraq);
Rpipe('warp.norm.brain',pfolder,xfname,lesfname,fileformat,atlasname,isct,tempop,healop,smoothop,deskullop,frq);
end