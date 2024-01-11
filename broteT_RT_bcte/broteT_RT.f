
		include "sub_common.f"

		program broteR

		implicit none
		include "common_brote.h"
		character*80 outp
		integer i,j,k,stg, iy,iepsA, Ny, errorf,ibb,maymen,
     +													NSOLCmax(Nymax),
     +		SENTIDO(Nymax,Nbmax,10), encontradouno,NSOLC(Nymax,Nbmax)
		double precision yini,yfin,wy,blim, CURVE(Nymax,Nbmax,10),
     +						calc_xH,calc_x2oG,wepsA,epsAini,epsAfin,
     +						payoff11, payoff21,difpay,difpay1,difpay2,
     +						payoff12,payoff22,epsA1,epsA2


		print*, 'Number of strategy: '
		read(*,*) stg
		print*,'y: initial, final, number: '
		read(*,*) yini, yfin, Ny
		print*,'epsA: initial, final, w: '
		read(*,*) epsAini, epsAfin, wepsA
		print*,'Output file'
		read(*,*) outp
									print*,epsAini, epsAfin, wepsA
		wy=(yfin-yini)/(Ny-1.d0)
		

		Nb=4
      bvect=(/1.1d0,1.5d0,2.d0,3.d0,0.d0,0.d0, 0.d0, 0.d0, 0.d0, 0.d0/)
c		Nb=1
c      bvect=(/1.5d0,2.d0,3.d0,0.d0,0.d0,0.d0, 0.d0, 0.d0, 0.d0, 0.d0/)

		NSOLC=1
		NSOLCmax=1

		call num2actionmoral(stg)
		print*,'Strategy:  ',stg


      open(90,file=outp,status='UNKNOWN')
      write(90,'(A1,A)') '#',' y vs eps   b(1st row)'
		write(90,'(A8,4x,A,$)') '0000',' '
		do i=1,Nb
			write(90,'(F10.4,$)') bvect(i)
		enddo
		write(90,'(A,$)')  '      '
		do i=1,Nb
			write(90,'(I4,$)') 0
		enddo
		write(90,'(A)') ' '

		SENTIDO=0
		CURVE=99
		do ibb=1,Nb
		 b=bvect(ibb)
		 			print*,'b= ',b
		 yy=yini
		 do iy=1,Ny
			encontradouno=0
		 	print*,'y= ',yy
			epsA=epsAini
  43		continue
				call process(payoff11,payoff21,payoff12,payoff22)
				difpay1=payoff11-payoff21
				epsA1=epsA
  41		continue
			epsA=epsA1+wepsA
				call process(payoff11,payoff21,payoff12,payoff22)
				difpay2=payoff11-payoff21
				epsA2=epsA
			if ((difpay1.lt.0.d0).and.(difpay2.lt.0.d0).or.
     +					((difpay1.gt.0.d0).and.(difpay2.gt.0.d0))) then
				if((epsA2.ge.epsAfin).and.(encontradouno.eq.0)) then
					if(difpay1.lt.0.d0) then
						CURVE(iy,ibb,1)=1.d0
						SENTIDO(iy,ibb,1)=-1.d0
					elseif(difpay1.gt.0.d0) then
						CURVE(iy,ibb,1)=0.d0
					endif
c					if ((difpay1.lt.0.d0).and.(difpay2.lt.0.d0)) then
c						if (difpay1.lt.difpay2) then
c							 CURVE(iy,ibb)=1.d0
c							 SENTIDO(iy,ibb)=-1
c						else
c							 CURVE(iy,ibb)=0.d0
c							 SENTIDO(iy,ibb)=1					
c						endif
c					else
c						if (difpay1.gt.difpay2) then
c							 CURVE(iy,ibb)=1.d0
c							 SENTIDO(iy,ibb)=1
c						else
c							 CURVE(iy,ibb)=0.d0
c							 SENTIDO(iy,ibb)=-1					
c						endif
c					endif
						print*, CURVE(iy,ibb,1), SENTIDO(iy,ibb,1)
				elseif(epsA2.le.epsAfin) then
					epsA1=epsA2
					goto 41
				endif
			elseif ((difpay1*difpay2).lt.0.d0) then

  42			continue
								!print*,epsA1,epsA2, difpay1,difpay2
				!epsA=epsA1-difpay1*(epsA1-epsA2)/(difpay1-difpay2)
				epsA=(epsA1+epsA2)/2.d0
				if((abs(epsA2-epsA1).lt.epsAtol).and.(epsA.le.epsAfin))then
					if(encontradouno.ge.1) then
							NSOLC(iy,ibb)=NSOLC(iy,ibb)+1
							NSOLCmax(iy)=NSOLC(iy,ibb)
					endif
					CURVE(iy,ibb,NSOLC(iy,ibb))=epsA
					if(difpay1.lt.difpay2) SENTIDO(iy,ibb,NSOLC(iy,ibb))=-1
					if(difpay2.lt.difpay1) SENTIDO(iy,ibb,NSOLC(iy,ibb))=1
								print*, CURVE(iy,ibb,NSOLC(iy,ibb)),
     +							SENTIDO(iy,ibb,NSOLC(iy,ibb)),NSOLC(iy,ibb)
					encontradouno=1
					if(epsA2.lt.epsAfin) then
						epsA=epsA2
						goto 43
					endif
				elseif(epsA.le.epsAfin) then
					call process(payoff11,payoff21,payoff12,payoff22)
					difpay=payoff11-payoff21
										
							!print*,epsA,b,difpay

					if(difpay.eq.0.d0) difpay=cero
					if((difpay*difpay1).gt.0.d0) then
						difpay1=difpay
						epsA1=epsA
						goto 42
					elseif((difpay*difpay2).gt.0.d0) then
						difpay2=difpay
						epsA2=epsA
						goto 42
					endif

				endif

			endif

			yy=yy+wy
		 enddo !y


		enddo !b


		yy=yini
		do iy=1,Ny
		 do i=1,NSOLCmax(iy)
			write(90,'(F8.4,4x,A,$)') yy,' '
			do ibb=1,Nb
				write(90,'(F10.4,$)') CURVE(iy,ibb,i)
			enddo
			write(90,'(A,$)') '      '
			do ibb=1,Nb
				write(90,'(I4,$)') SENTIDO(iy,ibb,i)
			enddo
			write(90,'(A)') ' '
		 enddo
		 yy=yy+wy
		enddo


		close(90)
		stop
		end



		subroutine process(payoff11,payoff21,payoff12,payoff22)		
		implicit none
		include "common_brote.h"
		double precision payoff11,payoff21,payoff12,payoff22
		integer errorf
		double precision den,pp
				x1AGo=pp(yy,1.d0-yy,0)				
				call IC
				call solve_x1
				x1AoG=x(1,1,1)+x(1,2,1)
				call calc_ppB(yy*x1AoG+(1.d0-yy)*x1BoG,1.d0,0.d0,x2oG,den)
				if(abs(den).lt.cero) stop 'den=0 en ppal'
				call solve_x2(errorf)
				x2Go=x(2,1,1)+x(2,1,2)
				call calcpay(payoff11,payoff21,payoff12,payoff22)
						!print*, epsA,payoff11-payoff21
		return
		end


c----------- Solves ---------------------------------------------------------------

		subroutine solve_x1

      implicit none
      include "common_brote.h"
		integer a1,a2, cont, is
		double precision difx2(3),xGGold(3),xGBold(3),
     +						 		xBGold(3),xBBold(3), suma(3),
     +							 x11new,x22new, x12new,x21new, Stt,Srt,Sh

						!print*
						!print*,x(1,1,1),x(1,1,2)
						!print*,x(1,2,1),x(1,2,2)
						!print*,'---',x1AGo
						!print*,x(3,1,1),x(3,1,2)
						!print*,x(3,2,1),x(3,2,2)
						!print*,'---',x1BoG
						!print*

		cont=0
		difx2=1.d0
		x1BoG=x(3,1,1)+x(3,2,1)
		x1AoG=x(1,1,1)+x(1,2,1)
		do while ((difx2(1)**0.5d0.gt.xtol).or.(difx2(3)**0.5d0.gt.xtol))
		  cont=cont+1

		  do is=1,3,2
		  	xGGold(is)=x(is,1,1)
		  	xGBold(is)=x(is,1,2)
		  	xBGold(is)=x(is,2,1)
		  	xBBold(is)=x(is,2,2)
		  enddo


		  x11new= dt*(yy*Stt(1,1,1,1)+(1.d0-yy)*Srt(1,1,x1AoG,x1BoG))
     +												 +(1.d0-dt)*x(1,1,1)
		  x22new= dt*(yy*Stt(0,0,1,1)+(1.d0-yy)*Srt(0,0,x1AoG,x1BoG))
     +												 +(1.d0-dt)*x(1,2,2)
		  x12new= dt*(yy*Stt(1,0,1,1)+(1.d0-yy)*Srt(1,0,x1AoG,x1BoG))
     +												 +(1.d0-dt)*x(1,1,2)
		  x21new= dt*(yy*Stt(0,1,1,1)+(1.d0-yy)*Srt(0,1,x1AoG,x1BoG))
     +												 +(1.d0-dt)*x(1,2,1)
		  x(1,1,1)=x11new
		  x(1,2,2)=x22new
		  x(1,1,2)=x12new
		  x(1,2,1)=x21new
		  !x(1,1,2)=x1AGo-x(1,1,1)
		  !x(1,2,1)=1.d0-x1AGo-x(1,2,2)
		  !x1AoG=x(1,1,1)+x(1,2,1)

		  x1BoG=(1.d0-yy)*Sh(x1BoG,x1BoG)+yy*Sh(x1BoG,x1AoG)

		  x(3,1,1)=0.d0
		  x(3,2,2)=1.d0-x1BoG
		  x(3,1,2)=0.d0
		  x(3,2,1)=x1BoG



		  do is=1,3,2
			do a1=1,2
			do a2=1,2
				if(x(is,a1,a2).lt.0.d0) x(is,a1,a2)=0.d0
			enddo
			enddo
			suma(is)=x(is,1,1)+x(is,2,1)+x(is,1,2)+x(is,2,2)
			if(suma(is).ne.1.d0) then
			do a1=1,2
			do a2=1,2
				x(is,a1,a2)=x(is,a1,a2)/suma(is)
			enddo
			enddo	
			endif
		  enddo

		  x1AoG=x(1,1,1)+x(1,2,1)
		  x1BoG=x(3,1,1)+x(3,2,1)

			!print*,x(1,1,1),x(1,1,2) 
			!print*,x(1,2,1),x(1,2,2)
			!print*,x(1,1,1)+x(1,2,2)+x(1,1,2)+x(1,2,1)
			!print*,x(3,1,1),x(3,1,2) 
			!print*,x(3,2,1),x(3,2,2)
			!print*,x(3,1,1)+x(3,2,2)+x(3,1,2)+x(3,2,1)
			!print*
			!if(cont.eq.50) stop

		 do is=1,3,2
		  difx2(is)=(x(is,1,1)-XGGold(is))**2.d0
     +		+(x(is,1,2)-XGBold(is))**2.d0
     +      +(x(is,2,1)-XBGold(is))**2.d0+(x(is,2,2)-XBBold(is))**2.d0
		 enddo

		enddo

		do a1=1,2
		do a2=1,2
			x(1,a1,a2)=(anint(x(1,a1,a2)*1.d0/cero))*cero
		enddo
		enddo
		x1BoG=(anint(x1BoG*1.d0/cero))*cero

		return 
		end




		subroutine solve_x2(errorf)

      implicit none
      include "common_brote.h"
		integer i,j,b1,b2,errorf
		double precision  AT(2,2),bmT(2),AR(2,2),bmR(2),A(2,2),bm(2),
     +						Ainv(2,2),xsol(2), Str

		call AbT(AT,bmT)

		AR=0.d0
		bmR=0.d0
		do i=1,0,-1
		 	bmR(2-i)= Str(i,i,x2oG,x1BoG)
		enddo


		do i=1,2
		do j=1,2
			A(i,j)=yy*AT(i,j)
		enddo
		A(i,i)=A(i,i)-1.d0
		bm(i)=yy*bmT(i)+(1.d0-yy)*bmR(i)
		enddo
		bm=-bm

		call FINDInv(A,Ainv,2,2,errorf)
		if (errorf.ne.0) then
			goto 51
		endif
		xsol=matmul(Ainv,bm)
		x(2,1,1)=xsol(1)
		x(2,2,2)=xsol(2)
		x(2,2,1)=x2oG-x(2,1,1)
		x(2,1,2)=1.d0-x2oG-x(2,2,2)

 51	continue
		do b1=1,2
		do b2=1,2
			x(2,b1,b2)=(anint(x(2,b1,b2)*1.d0/cero))*cero
			if(x(2,b1,b2).lt.(-1.d0*cero)) then
				print*,b1,b2,x(2,b1,b2)
				print*, 'menor que 0 ***********************'
				!stop
			endif
		enddo
		enddo
		return 
		end




		subroutine IC
		implicit none
      include "common_brote.h"
		integer i
		double precision xH, pp

		xH=pp(1.d0,0.d0,1)
		!print*,xH
		do i=1,3
			x(i,1,1)=xH
			x(i,2,2)=1.d0-xH
			x(i,1,2)=0.d0
			x(i,2,1)=0.d0
		enddo
		!do i=1,3
		!	x(i,1,1)=x1AGo
		!	x(i,2,2)=1.d0-x1AGo
		!	x(i,1,2)=0.d0
		!	x(i,2,1)=0.d0
		!enddo
		return
		end





c------------ payoff -----------------------------------------------------------------

		subroutine calcpay(payoff11,payoff21,payoff12,payoff22)

		implicit none
      include "common_brote.h"
		double precision  payoff11, payoff21,
     +						payoff12,payoff22
		double precision p1A1,p11A,p12,p21,p22, Cfun,num,den
		integer ia, ib

		p1A1=0.d0
		p11A=0.d0
		p12=0.d0
		p21=0.d0
		p22=0.d0
		do ia=1,0,-1
		do ib=1,0,-1
			p1A1=p1A1+ Cfun(ia,x1AGo)*Cfun(ib,x1AGo)*iaction(2-ia,2-ib)  		! 1 le da a 1
			p11A=p11A+ (yy*Cfun(ia,x1AGo)*Cfun(ib,x1AGo) 
     +   +(1.d0-yy)*Cfun(ia,x1BoG)*Cfun(ib,x1AoG)  )*iaction(2-ia,2-ib)
			!p22=p22+ Cfun(ia,x2oG)*Cfun(ib,x2oG)*iaction(2-ia,2-ib) 			! 2 le da a 2
			p21=p21+ Cfun(ia,x2oG)*( yy*Cfun(ib,x1AoG) 								! 2 le da a 1
     +              +(1.d0-yy)*Cfun(ib,x1BoG)	)*iaction(2-ia,2-ib)				 
			p12=p12+ ( yy*Cfun(ia,x1AGo)*Cfun(ib,x2Go)  								! 1 le da a 2
     +      +(1.d0-yy)*Cfun(ia,x1BoG)*Cfun(ib,x2oG) )*iaction(2-ia,2-ib) 
		enddo
		enddo
		p1A1=yy*p1A1

			payoff11=(b*p11A-c*p1A1)  *(1.d0-epsA)    ! de 1 (con 1)
			payoff21=(b*p12-c*p21)  *(1.d0-epsA)    ! de 2 (con 1)
			payoff12=(b*p21-c*p12)  *(1.d0-epsA)    ! de 1 (con 2)
			payoff22=-999999.d0   *(1.d0-epsA)	 ! de 2 (con 2)

		return
		end



		subroutine limb(blim,maymen)
c		maymen:  +1: b>   -1: b<	para que haya invasión	

		implicit none
      include "common_brote.h"
		double precision  blim
		integer maymen
		double precision p1A1,p11A,p12,p21,p22, Cfun,num,den
		integer ia, ib

		p1A1=0.d0
		p11A=0.d0
		p12=0.d0
		p21=0.d0
		p22=0.d0
		do ia=1,0,-1
		do ib=1,0,-1
			p1A1=p1A1+ Cfun(ia,x1AGo)*Cfun(ib,x1AGo)*iaction(2-ia,2-ib)  		! 1 le da a 1
			p11A=p11A+ (yy*Cfun(ia,x1AGo)*Cfun(ib,x1AGo) 
     +   +(1.d0-yy)*Cfun(ia,x1BoG)*Cfun(ib,x1AoG)  )*iaction(2-ia,2-ib)
			!p22=p22+ Cfun(ia,x2oG)*Cfun(ib,x2oG)*iaction(2-ia,2-ib) 			! 2 le da a 2
			p21=p21+ Cfun(ia,x2oG)*( yy*Cfun(ib,x1AoG) 								! 2 le da a 1
     +              +(1.d0-yy)*Cfun(ib,x1BoG)	)*iaction(2-ia,2-ib)				 
			p12=p12+ ( yy*Cfun(ia,x1AGo)*Cfun(ib,x2Go)  								! 1 le da a 2
     +      +(1.d0-yy)*Cfun(ia,x1BoG)*Cfun(ib,x2oG) )*iaction(2-ia,2-ib) 
		enddo
		enddo
		p1A1=yy*p1A1
		

		num=p1A1-p21
		den=p11A-p12
		blim=num/den
			print*,num,den
		if(den.gt.1.d-12) then
			maymen=-1
		elseif(den.lt.-1.d-12) then
			maymen=1
		else
			maymen=0
			blim=1.d0
		endif

		return
		end

