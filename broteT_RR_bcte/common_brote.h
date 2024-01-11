

		integer, parameter :: Nbmax=10, NepsAmax=10, Nymax=1000
		double precision, parameter :: c=1.d0, cero=1.d-15,
     +								dt=1d-1, xtol=1.d-12, epsAtol=1.d-4

		integer  iaction(2,2),imoral(2,2,2), Nb, NepsA
		double precision epsA, x1AGo, x1AoG,x1BGo,x1BoG, x2oG, x2Go,b,
     +			 yy, bvect(Nbmax), epsAvect(NepsAmax),
     +				x(3,2,2)

		common /ppal/  epsA,yy, bvect,b, epsAvect,Nb, NepsA
	   common /xx/	x1AGo,x1AoG,x1BGo,x1BoG,x2oG,x2Go,iaction,imoral,x
