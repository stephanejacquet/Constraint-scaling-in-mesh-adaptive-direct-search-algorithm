close all
clear all
clc
X = 100*lhspoint(6,50);


for i=1:50
    x = 0.1*round(10*X(i,:));
    i_str = num2str(i);
    if i<10 
      i_str = ['0' i_str];
    end
    fid=fopen(['./x0_LH_' i_str '.txt'],'w');
    s = num2str(x);
    s = strtrim(deblank(s));
    for kk=1:5
        s = strrep(s,'      ',' ');
        s = strrep(s,'  ',' ');
    end
    fwrite(fid,s);
    fclose(fid);
end



