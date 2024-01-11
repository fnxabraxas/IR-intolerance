
function plotLEG(dibujar)

  subplot(2,6,6);
  axis off;
  axis(axis);
  wr=0.3; hr=0.09; cx=0.5; y1=0.91; cy=0.5; y2=0.09; fuente=12; grosorflecha=2;
  colorizq=[0.5 0.5 0.5];
  colordcha=[0.8 0.8 0.8];
    %axis normal
    %axis equal
    %axis
    %return
  h=rectangle('Position',[cx y1 wr hr],'FaceColor',colorizq);
  h=rectangle('Position',[cx-wr y1 wr hr],'FaceColor',colordcha);
  h=rectangle('Position',[cx cy-hr/2 wr hr],'FaceColor',colorizq);
  h=rectangle('Position',[cx-wr cy-hr/2 wr hr],'FaceColor',colordcha);
  h=rectangle('Position',[cx y2-hr wr hr],'FaceColor',colorizq);
  h=rectangle('Position',[cx-wr y2-hr wr hr],'FaceColor',colordcha);
  h=text(cx-wr/2,y1+hr/2,'T\prime'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  h=text(cx+wr/2,y1+hr/2,'T'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  h=text(cx-wr/2,cy,'T\prime'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  h=text(cx+wr/2,cy,'I'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  h=text(cx-wr/2,y2-hr/2,'I'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  h=text(cx+wr/2,y2-hr/2,'I'); set(h,'FontName','Times','FontSize',fuente,'FontWeight','normal','HorizontalAlignment','center','VerticalAlignment','middle');
  if dibujar(1)~=0
    h=arrow('Start',[0.35 0.9],'Stop',[0.35 0.55],'Width',grosorflecha);
    set(h,'EdgeColor',[0 0.3 1],'FaceColor',[0 0.3 1])
  end
  if dibujar(2)~=0  
    h=arrow('Start',[0.65 0.55],'Stop',[0.65 0.9],'Width',grosorflecha);
    set(h,'EdgeColor',[0.8 0.03 0.03],'FaceColor',[0.8 0.03 0.03]);
  end
  if dibujar(3)~=0
    h=arrow('Start',[0.35 0.45],'Stop',[0.35 0.1],'Width',grosorflecha);
    set(h,'EdgeColor',[0.8 0.8 0.8],'FaceColor',[1 1 0])
  end
  if dibujar(4)~=0
    h=arrow('Start',[0.65 0.1],'Stop',[0.65 0.45],'Width',grosorflecha);
    set(h,'EdgeColor',[0.3 0.3 0.3],'FaceColor',[0.3 0.3 0.3]);
  end
  if dibujar(5)~=0
    h=arrow('Start',[0.1 0.9],'Stop',[0.1 0.1],'Width',grosorflecha);
    set(h,'EdgeColor',[0.2 0.6 0],'FaceColor',[0.2 0.6 0])
  end
  if dibujar(6)~=0
    h=arrow('Start',[0.9 0.1],'Stop',[0.9 0.9],'Width',grosorflecha);
    set(h,'EdgeColor',[0.4 0 0],'FaceColor',[0.4 0 0]);
  end
end