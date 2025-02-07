function newfile = deskull (patient_folder, x_fname, is_ct, fraq)
%Deskull: matlab wrapper for deskull.brain()

%prepare arguments:
pfolder = quote(win2lin([patient_folder],'full'));
xfname = quote(win2lin(x_fname));
isct = num2str(is_ct);
frq = num2str(fraq);

%invoke container-based function:
Rpipe('warp.deskull.brain',pfolder,xfname,isct,frq);

%check if file was created, if yes - return it to the calling function (to
%be entered into the brain image editbox
newfile = ['ds.' x_fname];
newfile_full = [patient_folder, '\', newfile];
if ~isfile(newfile_full)
    newfile = '';
end

end