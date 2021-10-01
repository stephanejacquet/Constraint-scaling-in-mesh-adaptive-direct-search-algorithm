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
MM=400;
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

for p=1:3

if ( p==1 )
    X=load('outputs/dp1.txt');
end
if ( p==2 )
    X=load('outputs/dp2.txt');
end
if ( p==3 )
   X=load('outputs/dp3.txt');    
end 

L=size(X,1);
NBcol=size(X,2);

maxAxeX=X(L,1);

if ( NBcol<=1 )
    warning('Too few columns (<2)');
    return;
end

figure;  

hold on;


h = zeros(1, NBcol-1);


L1=X(round(1+linspace(1,L-1,11)),:);
h(1)=plot ( L1(1:MMM,1) , L1(1:MMM,2)/100 , 'o', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','b','MarkerSize',7 ); 
plot( X(1:MM,1) , X(1:MM,2)/100 , 'b--' ); 

if ( NBcol==1 )
    legend(h(1),'1');
end


if ( NBcol>=3 )
    L2=X(round(3+linspace(1,L-3,11)),:);
    h(2)=plot ( L2(1:MMM,1) , L2(1:MMM,3)/100 , '>', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor',cRose,'MarkerSize',7 ); 
    plot( X(1:MM,1) , X(1:MM,3)/100 , 'r--' );
    if ( NBcol==3 )
        %legend(h,'Distance à la cache','Direction dernier Succès');
        legend (h,'Distance to cache','Direction of last success'); 
        grid on;
    end
end

if ( NBcol>=4 )
    L3=X(round(5+linspace(1,L-5,12)),:);
    h(3)=plot ( L3(:,1) , L3(:,4) , 'o', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','m','MarkerSize',7 ); 
    plot( X(1:MM,1) , X(1:MM,4) , 'm--' );
    if ( NBcol==4 )
        legend(h,'1','2','3');
    end
end

if ( NBcol>=5 )
    L4=X(round(7+linspace(1,L-7,13)),:);
    h(4)=plot ( L4(:,1) , L4(:,5) , '^', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','y','MarkerSize',7 ); 
    plot( X(1:MM,1) , X(1:MM,5) , 'y--' );
    if ( NBcol==5 )
        %legend(h,'Mads 2n','RobustMads 2n \beta = 1','RobustMads 2n \beta = 0.1','RobustMads 2n \beta = 0.01');
        legend(h,'1','2','3','4');
    end
end

if ( NBcol>=6 )
    L5=X(round(9+linspace(1,L-9,14)),:);
    h(5)=plot ( L5(:,1) , L5(:,6) , '<', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','g','MarkerSize',7 ); 
    plot( X(1:MM,1) , X(1:MM,6) , 'g--' );
    if ( NBcol==6 )
        %legend(h,'Mads 2n','RobustMads 2n \beta = 2','RobustMads 2n \beta = 1','RobustMads 2n \beta = 0.1','RobustMads 2n \beta = 0.01');
        legend(h,'1','2','3','4','5');
    end
end

if ( NBcol>=7 )
    L6=X(round(11+linspace(1,L-11,15)),:); 
    h(6)=plot ( L6(:,1) , L6(:,7) , 's', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','r','MarkerSize',7 );
    plot( X(1:MM,1) , X(1:MM,7) , 'r--' );
    if ( NBcol==7 )
        legend(h,'1','2','3','4','5','6');
    end
end

if ( NBcol>=8 )
    L7=X(round(13+linspace(1,L-13,16)),:); 
    h(7)=plot ( L7(:,1) , L7(:,8) , 'p', 'LineWidth',1,'MarkerEdgeColor','k','MarkerFaceColor','k','MarkerSize',7 );
    plot( X(1:MM,1) , X(1:MM,8) , 'r--' );
    if ( NBcol==8 )
        legend(h,'1','2','3','4','5','6','7');
    end
end

if ( NBcol>=9 )
    warning('Too many columns (>=8)');
    return;
end
%------------------------------------%

 

%set(gca,'FontSize',18);
%set(gcf,'papersize', [18 18]);
%set(gcf, 'PaperPosition', [-0.7    -0.7   20 20 ]);

if ( p==1 )
    title ( {'precision \tau=1e-3'} );
end
if ( p==2 )
    title ( {'precision \tau=1e-5'} );
end
if ( p==3 )
    title ( {'precision \tau=1e-7'});
end
    
axis ( [1 maxAxeX  0  1] );

%xlabel ( 'Groups de (n+1) évaluations' );
%ylabel ( 'ratio de problèmes' );

xlabel ( 'groups of (n+1) evaluations' );
ylabel ( 'ratio of solved problems' );

set(findall(gcf,'type','text'),'FontSize',16,'fontWeight','normal');
set(findall(gcf,'type','axes'),'FontSize',16,'fontWeight','normal');

% print(gcf, '-dpdf', '-r300', 'dp3.pdf')

hold off;

end