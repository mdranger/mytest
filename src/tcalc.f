
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      real function tcalc(stemp, teff)

	real stemp, teff(3)

c...This function computes the effect of temperature on
c   decomposition.  It is an exponential function.  Older
c   versions of Century used a density function.
c   Created 10/95 - rm
c
c...Called From:	CYCLE
c
c...Variables
c	STEMP:		Soil Temperature
c	TEFF(1):	Intercept
c	TEFF(2):	Slope
c	TEFF(3):	Exponent

	tcalc = teff(1) + teff(2) * exp(teff(3) * stemp)
	return
	end

