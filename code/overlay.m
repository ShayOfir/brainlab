function overlay (behavior_fn, folder, fn_out, subject_column, interim, side_column, which_side, threshold)
%wrap_overlay <- function (df, folder, fn_out, subject_column = 'pcode', interim = FALSE, side_column = 'Side', which_side = c('L','R'), threshold = 0.5){

args = struct();
args.behavior_df = quote(win2lin(behavior_fn,'full'));
args.folder = quote(win2lin([folder,'\'],'full'));
args.fn_out = quote(win2lin(fn_out,'full'));
args.subject_column = quote(subject_column);
args.interim = num2str(interim);
if strcmp(side_column,'None')
    args.side_column = 'NULL';
else
    args.side_column = quote(side_column);
end
if isempty(which_side) 
    args.which_side = 'NULL';
elseif iscell(which_side)
    args.which_side = quote(which_side{1});
else
    args.which_side = 'NULL';
end

args.threshold = num2str(threshold);

Rpipe('overlay_lesions','kw',{...
     'behavior_df',args.behavior_df,...
     'folder',args.folder,...
     'fn_out',args.fn_out,...
     'subject_column',args.subject_column,...
     'interim',args.interim,...
     'side_column',args.side_column,...
     'which_side',args.which_side,...
     'threshold',args.threshold,...
     })
                        
end