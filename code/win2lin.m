function y = win2lin (x,varargin)

if nargin > 1
    option = varargin{1};
else
    option = 'short';
end

if strcmpi(option,'full')
    xx = lower(x);
    pos = findstr(xx,'\brainlab');
    y = xx(pos:end);
    y = replace(y, '\brainlab', '\shared');
else
    y=x;
end
y = replace(y,'\','/');



end