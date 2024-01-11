
function plotallR_PD_2(i,stgv,ib)

  dirs=strvcat( 'broteT_RR_PD', 'broteR_TR_PD', 'broteT_RT');
  dirlon=[12 12 9 12];
  %colordir=strvcat( 'b', 'y', 'r', 'r' );
  colordir=[ 0 0 0; 1 1 0 ; 0.8 0.03 0.03;      0.8 0.03 0.03; 1 1 0;   0 0.3 1; ];
  transpdir=[0.5  1 1 0.5];
  lindir= strvcat( 'none', 'none', 'none', 'k' );
  cambiaY=[0 0 1 ];
  figure(1);

  switch i
    case 1,
      subplot(2,6,[1,2.3]);
    case 2,
      subplot(2,6,[3.65,4.95]);
    case 3,
      subplot(2,6,[7,8.3]);
    case 4,
      subplot(2,6,[9.65,10.95]);
  end

  axis ([0.01 0.99 0.01 0.99]);
  ylabel('\epsilon_A');
  xlabel('y');
  hold all;

  [Ndir,tt]=size(dirs);

  %Ndir=1
  for idir=1:Ndir
    file=['../modif/' dirs(idir,1:dirlon(idir)) '_' num2str(stgv(i)) '.dat'];
    disp(file);
    Cfile=importdata(file,' ',1);
    [Ny,Nb]=size(Cfile.data);
    Ny=Ny-1;
    Nb=(Nb-1)/2;
    y=Cfile.data(2:Ny+1,1);
    epsA=Cfile.data(2:Ny+1,ib+1);
    epsAI=Cfile.data(2:Ny+1,ib+Nb+1);
    bhead=Cfile.data(1,ib+1);
    title (['b/c=' num2str(bhead)]);
    %plot([-1 2],[1-1/bhead 1-1/bhead],'b-.');
    if(cambiaY(idir)==1)
      y=1-y;
    end

    iarriba=0;
    iabajo=0;
    clear epsAarriba;
    clear epsAabajo;
    clear yarriba;
    clear yabajo;
    acabodibujar=0;
    for iy=1:Ny
      switch epsAI(iy)

      case 1
	if iy>1
	  if iabajo==0
	    epsAarriba(1)=1;
	    epsAabajo(1)=1;
	    yarriba(1)=y(iy-1);
	    yabajo(1)=y(iy-1);
	    iarriba=iarriba+1;
	    iabajo=iabajo+1;
	  end
	end
	iabajo=iabajo+1;
	yabajo(iabajo)=y(iy);
	epsAabajo(iabajo)=epsA(iy);
	if iy<Ny
	  if y(iy+1)~=y(iy) || epsA(iy+1)==99
	    if iy>1
	      if y(iy-1)~=y(iy) || epsA(iy-1)==99
		iarriba=iarriba+1;
		yarriba(iarriba)=y(iy);
		epsAarriba(iarriba)=1; 
	      end
	    else
		iarriba=iarriba+1;
		yarriba(iarriba)=y(iy);
		epsAarriba(iarriba)=1; 
	    end
	  end
	else
	    if iy>1
	      if y(iy-1)~=y(iy) || epsA(iy-1)==99
		iarriba=iarriba+1;
		yarriba(iarriba)=y(iy);
		epsAarriba(iarriba)=1; 
	      end
	    else
		iarriba=iarriba+1;
		yarriba(iarriba)=y(iy);
		epsAarriba(iarriba)=1; 
	    end
	end
      case -1
	if iy>1
	  if iarriba==0
	    epsAarriba(1)=0;
	    epsAabajo(1)=0;
	    yarriba(1)=y(iy-1);
	    yabajo(1)=y(iy-1);
	    iarriba=iarriba+1;
	    iabajo=iabajo+1;
	  end
	end
	iarriba=iarriba+1;
	yarriba(iarriba)=y(iy);
	epsAarriba(iarriba)=epsA(iy);    
	if iy<Ny
	  if y(iy+1)~=y(iy) || epsA(iy+1)==99
	    if iy>1
	      if y(iy-1)~=y(iy) || epsA(iy-1)==99
		iabajo=iabajo+1;
		yabajo(iabajo)=y(iy);
		epsAabajo(iabajo)=0; 
	      end
	    else
		iabajo=iabajo+1;
		yabajo(iabajo)=y(iy);
		epsAabajo(iabajo)=0; 
	    end
	  end
	else
	    if iy>1
	      if y(iy-1)~=y(iy) || epsA(iy-1)==99
		iabajo=iabajo+1;
		yabajo(iabajo)=y(iy);
		epsAabajo(iabajo)=0; 
	      else
		iabajo=iabajo+1;
		yabajo(iabajo)=y(iy);
		epsAabajo(iabajo)=0; 
	      end
	    end
	end      
      case 0
       if epsA(iy)~=99
%	if epsA(iy)==0 && acabodibujar==0
	if acabodibujar==0
	  iabajo=iabajo+1;
	  iarriba=iarriba+1;
	  
	  %disp([[yarriba(iarriba-1) y(iy)], [epsAarriba(iarriba-1) 2*epsAarriba(iarriba-1)-epsAarriba(iarriba-2)], [yabajo(iabajo-1) y(iy)], [epsAabajo(iabajo-1) 2*epsAabajo(iabajo-1)-epsAabajo(iabajo-2)]  ])

	  [xint,yint]=curveintersect([yarriba(iarriba-1) y(iy)], [epsAarriba(iarriba-1) 2*epsAarriba(iarriba-1)-epsAarriba(iarriba-2)], [yabajo(iabajo-1) y(iy)], [epsAabajo(iabajo-1) 2*epsAabajo(iabajo-1)-epsAabajo(iabajo-2)]);

	  if length(yint)>=1
	    yabajo(iabajo)=xint;
	    yarriba(iarriba)=xint;
	    epsAabajo(iabajo)=yint;
	    epsAarriba(iarriba)=yint;	  
	  else
	    yabajo(iabajo)=y(iy);
	    yarriba(iarriba)=y(iy);
	    epsAabajo(iabajo)=2*epsAabajo(iabajo-1)-epsAabajo(iabajo-2);
	    epsAarriba(iarriba)=2*epsAarriba(iarriba-1)-epsAarriba(iarriba-2);	    
	  end

	  %yabajo(iabajo)=y(iy);
	  %yarriba(iarriba)=yabajo(iabajo);
	  %epsAabajo(iabajo)=epsAabajo(iabajo-1)/(1+epsAabajo(iabajo-1)-epsAarriba(iarriba-1));
	  %epsAarriba(iarriba)=epsAabajo(iabajo);
	  acabodibujar=1;
	end
	if iarriba>0
	 if (epsAarriba-epsAabajo) >= 0
%	  if iy<Ny
%	    yabajo=[yabajo y(iy+1)]; 
	    
%	  end
	  jbfill(yabajo,epsAarriba,epsAabajo,colordir(idir,:),lindir(idir,:),0,transpdir(idir));
	 else
	  clear ydata;
	  for ii=1:length(yabajo)
	    ydata(ii)=1;
	    if epsAabajo(ii)==0 epsAabajo(ii)=1; break; end
	  end
	  jbfill(yabajo(1:ii),ydata(1:ii),epsAabajo(1:ii),colordir(idir,:),lindir(idir,:),0,transpdir(idir));
	  clear xdata;
	  for ii=1:length(yarriba)
	    ydata(ii)=0;
	    if epsAarriba(ii)==1 epsAarriba(ii)=0; break; end
	  end
	  jbfill(yarriba(1:ii),epsAarriba(1:ii),ydata(1:ii),colordir(idir,:),lindir(idir,:),0,transpdir(idir));
	 end
	 clear epsAarriba;
	 clear epsAabajo;
	 clear yarriba;
	 clear yabajo;
	 iarriba=0;
	 iabajo=0;
	end
       end
      end % CASE
      
      if iarriba>1 && iabajo>1
	if epsAarriba(iarriba) < epsAabajo(iabajo) && epsAarriba(iarriba-1) > epsAabajo(iabajo-1)
	  [xint,yint]=curveintersect([yabajo(iabajo-1) yabajo(iabajo)], [epsAarriba(iarriba-1) epsAarriba(iarriba)], [yabajo(iabajo-1) yabajo(iabajo)], [epsAabajo(iabajo-1) epsAabajo(iabajo)]);  
	  jbfill([yabajo(1:iabajo-1) xint],[epsAarriba(1:iarriba-1) 1],[epsAabajo(1:iabajo-1) 0],colordir(idir,:),lindir(idir,:),0,transpdir(idir));
	  xsig=yabajo(iabajo);
	  ysigarriba=epsAarriba(iarriba);
	  ysigabajo=epsAabajo(iabajo);
	  clear epsAarriba;
	  clear epsAabajo;
	  clear yarriba;
	  clear yabajo;
	  iarriba=2;
	  iabajo=2;
	  yarriba(1)=xint;
	  yabajo(1)=xint;
	  yarriba(2)=xsig;
	  yabajo(2)=xsig;
	  epsAarriba(1)=yint;
	  epsAabajo(1)=yint;
	  epsAarriba(2)=ysigarriba;
	  epsAabajo(2)=ysigabajo;
	end
      end

    end % iy




    if iarriba>0
     if iarriba>1
      if yarriba(iarriba)==yarriba(iarriba-1) yarriba=[yarriba(1:iarriba-1)]; epsAarriba=[epsAarriba(1:iarriba-1)]; iarriba=iarriba-1; end
     end
     if iabajo>1
      if yabajo(iabajo)==yabajo(iabajo-1) yabajo=[yabajo(1:iabajo-1)]; epsAabajo=[epsAabajo(1:iabajo-1)]; iabajo=iabajo-1; end
     end
	  
     if (epsAarriba-epsAabajo) >= 0
      jbfill(yabajo,epsAarriba,epsAabajo,colordir(idir,:),lindir(idir,:),0,transpdir(idir));
     else
	clear ydata;
	for ii=1:length(yabajo)
	  ydata(ii)=1;
	  if epsAabajo(ii)==0 epsAabajo(ii)=1; break; end
	end
	jbfill(yabajo(1:ii),ydata(1:ii),epsAabajo(1:ii),colordir(idir,:),lindir(idir,:),0,transpdir(idir));
	clear xdata;
	for ii=1:length(yarriba)
	  ydata(ii)=0;
	  if epsAarriba(ii)==1 epsAarriba(ii)=0; break; end
	end
	jbfill(yarriba(1:ii),epsAarriba(1:ii),ydata(1:ii),colordir(idir,:),lindir(idir,:),0,transpdir(idir));
     end
    end



  end



end
