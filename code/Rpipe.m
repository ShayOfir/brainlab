function Rpipe (func,varargin)        
    addcom('source("/shared/code/brainlab.R")',1);
    acom = build_com(func,varargin);
    addcom(acom,0);    
    Rscript('temp.R');
end

function acom = build_com(func, varargin)

acom = [func,'('];

for k=1:length(varargin{1})
    acom = [acom,varargin{1}{k}];
    if k < length(varargin{1})
        acom = [acom,','];
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