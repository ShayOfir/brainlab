

function flip_lesions (behavior_fn, folder, subject_column, side_column, dest_folder, flip_to)

args = struct();
%flip_lesions <- function(behavior_fn, folder, subject_column = 'pcode', side_column = 'Side', dest_folder, fip_to = 'L')
args.behavior_df = quote(win2lin(behavior_fn,'full'));
args.folder = quote(win2lin([folder,'\'],'full'));
args.subject_column = quote(subject_column);
if strcmp(side_column,'None')
    args.side_column = 'NULL';
else
    args.side_column = quote(side_column);
end
args.dest_folder = quote(win2lin([dest_folder,'\'],'full'));
args.flip_to = quote(flip_to);

Rpipe('flip_lesions','kw',{ ...
     'behavior_fn',args.behavior_df,...
     'folder',args.folder,...
     'subject_column',args.subject_column,...
     'side_column',args.side_column,...
     'dest_folder',args.dest_folder,...
     'flip_to',args.flip_to
    })


end