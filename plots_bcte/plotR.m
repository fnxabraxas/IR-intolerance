
%stgv=[ 405 469 1437 1501 2186 2202 2219 2234 2442 2453 2458 2475 2490 2517 3477 3541];


%stgv=[ 405 469 ];
%stgv=[ 1437 1501 2453 2517 3477 3541];






Nbb=4;
stgv=[ 1437 1501 2453 2517 3477 3541];

for i=1:length(stgv)
 h=figure(i);
 set(h,'Position',[0,0,750,550]);
 clf reset;
    %break
 for ib=1:Nbb
    plotallR(i,stgv,ib);
    %break
 end

  plotLEG([1 1 1 0 1 0]);

  filename=['racism_bcte_' num2str(stgv(i))];

  filenamesvg=[filename '.svg'];
  plot2svg(filenamesvg)


  %%%filenameps=[filename '.ps'];
  
  %filenamepdf=[filename '.pdf'];
  %%%print ('-dpsc2',filenameps);
  %fileps2pdf=['ps2pdf -sPAPERSIZE=a4 ' filenameps ' ' filenamepdf];
  %system(fileps2pdf);
  %zippdf=['zip racism_pdf ' filenamepdf];
  %system(zippdf);

  %%%filenameps=[filename '.pdf'];
  %%%print ('-dpdf','-r600',filenameps);

  %%%filenameps=[filename '.jpg'];
  %%%print ('-djpeg',filenameps);

  %%%filenameps=[filename '.bmp'];
  %%%print ('-dbmp',filenameps);

end

break


stgv=[ 469 405 ];
Nbb=4;



for i=1:length(stgv)
 h=figure(i);
 set(h,'Position',[0,0,750,550]);
 clf reset;

  plotLEG([0 1 1 1 0 1]);

 for ib=1:Nbb
    plotallR_III(i,stgv,ib);
 end
 

  filename=['racism_bcte_' num2str(stgv(i))];

  filenamesvg=[filename '.svg'];
  plot2svg(filenamesvg)

  %%%filenameps=[filename '.ps'];
  %filenamepdf=[filename '.pdf'];
  %%%print ('-dpsc2',filenameps);
  %fileps2pdf=['ps2pdf -sPAPERSIZE=a4 ' filenameps ' ' filenamepdf];
  %system(fileps2pdf);
  %zippdf=['zip racism_pdf ' filenamepdf];
  %system(zippdf);

  %%%filenameps=[filename '.pdf'];
  %%%print ('-dpdf',filenameps);

  %%%filenameps=[filename '.jpg'];
  %%%print ('-djpeg',filenameps);

  %%%filenameps=[filename '.bmp'];
  %%%print ('-dbmp',filenameps);

end

break
