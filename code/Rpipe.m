function Rpipe (func,varargin) 
%Invoke R-pipeline via docker container
%USE:
%           Rpipe (func, ['argument1', 'argument2', ...])
%           Rpipe (func, 'kw', {'argument1-key', 'argument1-value',
%           'argument2-key', 'argument2-value',...]})
%
%Args:
%          func (str) : function name to invoke. For a list of supported
%          function see documentation.
%           
%          There are two option two provide arguments. Either positional way (i.e., order of argument appearance is the same as in the function definition in pipeline R-script)
%          or via key-value pairs. To use the last, put an argument named
%          'kw' and then a cell-vector of key and value pairs. 
%          The cell-vector must me of mod(length,2)==0 or the function will
%          not interpret it correctly.
%
%          IMPORTANT: all arguments, even numeric values must be turned
%          into strings!
%
    addcom('source("/shared/code/brainlab.R")',1);
    acom = build_com(func,varargin);
    addcom(acom,0);    
    Rscript('temp.R');
end

function acom = build_com(func, varargin)

acom = [func,'('];
vargin = varargin{1};
if ((length(vargin)==2) && (strcmpi(vargin{1},'kw')) && (iscell(vargin{2})) && (mod(length(vargin{2}),2)==0))
    %key-words option
    args = vargin{2};
    kw = true;
    for k=1:length(args)
        if kw
            acom = [acom, args{k}, ' = '];            
        else
            if k < length(args)
                acom = [acom, args{k},', '];
            else
                acom = [acom, args{k}];
            end
        end
        kw = ~kw;

    end
else 

    if strcmpi(vargin{1},'kw')
        disp('Rpipe Warning: It seems that keyword-value argument style was intended, but the arguments cell-array is not correct. Intepreting as positional arguments')
    end

    for k=1:length(vargin)
        acom = [acom,vargin{k}];
        if k < length(vargin)
            acom = [acom,','];
        end
    end
end

acom = [acom,')'];


end

function addcom (com, varargin)
    filename = 'temp.R';
    rewrite = 0;    
    if nargin >= 2
        rewrite = varargin{1};
    end
    if nargin >= 3
        filename = varargin{2};
    end
    if rewrite
        f = fopen(filename,'w');        
    else
        f = fopen(filename,'a');
    end
    fprintf(f,[com,'\n']);
    fclose(f);
end