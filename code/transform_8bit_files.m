function newfile = transform_8bit_files(patient_folder,x_fname)

pfolder = quote(win2lin([patient_folder,'\'],'full'));
xfname = quote(win2lin(x_fname));
Rpipe ('warp.transform.8bit',pfolder,xfname);
newfile = ['transform.' x_fname];
newfile_full = [patient_folder '\' newfile];
if ~isfile(newfile_full)   
    newfile = '';
end

end