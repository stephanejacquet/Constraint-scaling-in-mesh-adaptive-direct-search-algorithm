%            b     blue          .     point              -     solid
%            g     green         o     circle             :     dotted
%            r     red           x     x-mark             -.    dashdot 
%            c     cyan          +     plus               --    dashed   
%            m     magenta       *     star             (none)  no line
%            y     yellow        s     square
%            k     black         d     diamond
%            w     white         v     triangle (down)
%                                ^     triangle (up)
%                                <     triangle (left)
%                                >     triangle (right)
%                                p     pentagram
%                                h     hexagram
%         


clear all;

MM=50;
MMM=10;
cBlue = [28 96 243] ./ 255;
cRed = [239 11 3] ./ 255;
cGreen = [96 239 10] ./ 255;
cOrange = [241 126 32] ./ 255;
cViolet = [150 11 150] ./ 255;
cRose = [255 95 145] ./ 255;

cLightBlue = [72 242 253] ./ 255;
cLightRed = [231 100 148] ./ 255;
cLightGreen = [56 220 190] ./ 255;
cLightOrange = [245 206 149] ./ 255;
cLightViolet = [187 133 189] ./ 255;
%------------------------------------%


X=load('outputs/pp5.txt');

L=size(X,1)/2;
NBcol=size(X,2);

maxAxeX=X(L,1);

if ( NBcol<=1 )
    warning('Too few columns (<2)');
    return;
end

figure;  

hold on;


h = zeros(1, NBcol-1);


L1=X(round(1+linspace(1,L-1,10)),:);
h(1)=plot ( L1(:,1) , L1(:,2)/100 , 'o', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','b','MarkerSize',7 ); 
plot( X(1:MM,1) , X(1:MM,2)/100 , 'b--' ); 

if ( NBcol==1 )
    legend(h(1),'1');
end


if ( NBcol>=3 )
    L2=X(round(3+linspace(1,L-3,11)),:);
    h(2)=plot ( L2(:,1) , L2(:,3)/100 , '>', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor',cRose,'MarkerSize',7 ); 
    plot( X(1:MM,1) , X(1:MM,3)/100 , 'r--' );
    if ( NBcol==3 )
        %legend(h,'Distance à la cache','Direction de dernier succès');
        legend (h,'Distance to cache','Direction of last success'); 
        grid on;
    end
end

if ( NBcol>=4 )
    L3=X(round(5+linspace(1,L-5,12)),:);
    h(3)=plot ( L3(:,1) , L3(:,4) , 'o', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','m','MarkerSize',7 ); 
    plot( X(:,1) , X(:,4) , 'm--' );
    if ( NBcol==4 )
        legend(h,'1','2','3');
    end
end

if ( NBcol>=5 )
    L4=X(round(7+linspace(1,L-7,13)),:);
    h(4)=plot ( L4(:,1) , L4(:,5) , '^', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','y','MarkerSize',7 ); 
    plot( X(:,1) , X(:,5) , 'y--' );
    if ( NBcol==5 )
        legend(h,'1','2','3','4');
    end
end

if ( NBcol>=6 )
    L5=X(round(9+linspace(1,L-9,14)),:);
    h(5)=plot ( L5(:,1) , L5(:,6) , '<', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','g','MarkerSize',7 ); 
    plot( X(:,1) , X(:,6) , 'g--' );
    if ( NBcol==6 )
        legend(h,'1','2','3','4','5');
    end
end

if ( NBcol>=7 )
    L6=X(round(11+linspace(1,L-11,15)),:); 
    h(6)=plot ( L6(:,1) , L6(:,7) , '>', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',7 );
    plot( X(:,1) , X(:,7) , 'r--' );
    if ( NBcol==7 )
        legend(h,'1','2','3','4','5','6');
    end
end

if ( NBcol>=7 )
    warning('Too many columns (>=7)');
    return;
end
%------------------------------------%

 


title ( 'precision \tau=1e-5' );
    
axis ( [1 maxAxeX  0  1] );

%xlabel ( 'ratio de performance \alpha' );
%ylabel ( 'ratio de problèmes' );
xlabel ( 'performance ratio \alpha' );
ylabel ( 'ratio of solved problems' );
% print(gcf, '-dpdf', '-r300', 'dp3.pdf')

hold off;
