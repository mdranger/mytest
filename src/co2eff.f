
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


      subroutine co2eff(time)
      real time

c...Compute the effect of atmospheric CO2 concentration.

      include 'const.inc'
      include 'param.inc'
      include 'parfx.inc'
      include 'plot1.inc'

c...Function declarations
      real      effect, line, ramp
      external  line, ramp

c...Local variables
	integer   iel, mnmx, system
	real      co2conc


c...Reset all effects to 1.0
	do 30 system = CRPSYS, FORSYS
	  co2cpr(system) = 1.0
	  co2ctr(system) = 1.0
	  co2crs(system) = 1.0
	  do 20 mnmx = IMIN, IMAX
	    do 10 iel = 1, nelem
	      co2cce(system,mnmx,iel) = 1.0
10	    continue
20	  continue
30	continue

c...If there is no co2 effect, return; 
c     otherwise, calculate a new co2 effect
	if (co2sys .lt. 0) then
	  return
	endif

c...Calculate a co2 concentration
	if (co2rmp .eq. 0) then
c...Steady
          co2conc = co2ppm(1)
	else
c...Ramping
	  co2conc = ramp(time,co2tm(1),co2ppm(1),co2tm(2),co2ppm(2))
	endif

c...Calculate effect on production
        do 40 system = CRPSYS, FORSYS
	  co2cpr(system) = effect(co2ipr(system),co2conc)
40      continue

c...Calculate effect on PET
        do 50 system = CRPSYS, FORSYS
	  co2ctr(system) = effect(co2itr(system),co2conc)
50	continue

c...Calculate effect on C/E
	do 80 iel = 1, nelem
	  do 70 mnmx = IMIN, IMAX
            do 60 system = CRPSYS, FORSYS
	      co2cce(system,mnmx,iel) =
     $                 effect(co2ice(system,mnmx,iel),co2conc)
60          continue
70        continue
80      continue

c...Calculate effect on root/shoot
c     Reference co2 concentration = 350.0 at 1.0
        do 90 system = CRPSYS, FORSYS
	  co2crs(system) = line(co2conc,350.0,1.0,700.0,co2irs(system))
90      continue

	return
	end




      real function effect(co2input, co2conc)

      real      co2input, co2conc

c...Reference co2 concentration = 350.0
      effect = 1 + (co2input-1) / (log10(2.0))*(log10(co2conc/350.0))

      return
      end
