
%stgv=[ 405 469 1437 1501 2186 2202 2219 2234 2442 2453 2458 2475 2490 2517 3477 3541];



stgv=[ 1437  2453 ]%1437 1501 2453 2517 3477 3541];%[ 1437 1501 2453 2517 3477 3541];
Nbb=4;



for i=1:length(stgv)
 figure(i);
 clf reset;

  plotLEG_PD_2([0 2 2 1 0 0]);

 for ib=2:2
    plotallR_PD_2(i,stgv,ib);
 end
end
      %break

  filename=['racism_bcte_PD_no'];
  filenameps=[filename '.ps'];
  %filenamepdf=[filename '.pdf'];
  print ('-dpsc',filenameps);
  %fileps2pdf=['ps2pdf -sPAPERSIZE=a4 ' filenameps ' ' filenamepdf];
  %system(fileps2pdf);
  %zippdf=['zip racism_PD_pdf ' filenamepdf];
  %system(zippdf);

  filenameps=[filename '.pdf'];
  print ('-dpdf',filenameps);

  filenameps=[filename '.jpg'];
  print ('-djpeg',filenameps);

  filenameps=[filename '.bmp'];
  print ('-dbmp',filenameps);



for i=1:length(stgv)
 figure(i);
 clf reset;

  plotLEG_PD_1([2 1 0 1 0 1]);

 for ib=2:2
    plotallR_PD_1(i,stgv,ib);
 end
end


  filename=['racism_bcte_PD_yes'];
  filenameps=[filename '.ps'];
  %filenamepdf=[filename '.pdf'];
  print ('-dpsc2',filenameps);
  %fileps2pdf=['ps2pdf -sPAPERSIZE=a4 ' filenameps ' ' filenamepdf];
  %system(fileps2pdf);
  %zippdf=['zip racism_PD_pdf ' filenamepdf];
  %system(zippdf);

  filenameps=[filename '.pdf'];
  print ('-dpdf',filenameps);

  filenameps=[filename '.jpg'];
  print ('-djpeg',filenameps);

  filenameps=[filename '.bmp'];
  print ('-dbmp',filenameps);


