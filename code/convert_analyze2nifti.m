function convert_analyze2nifti (full_fn)
% Wrap function to convery ANALYZE format to NII.GZ format, with flipping
% X-Y to radiological convention (all MEDx HDR files are filpped).
%
%analyze2nifti <- function (full.fn, new.full.fn = NA)

fullfn = quote(win2lin(full_fn,'full'));

Rpipe('analyze2nifti',fullfn);

end