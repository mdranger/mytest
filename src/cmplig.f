
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


        subroutine cmplig(cursys,fligni,wdlig,pltlig)

        include 'const.inc'
        include 'param.inc'
        include 'wth.inc'

        include 'zztim.inc'

        integer  cursys
        real     fligni(2,CPARTS), wdlig(FPARTS), pltlig(CPARTS)

!...Compute plant lignin; returns the fraction of residue which will
!     lignin.

!...Local variables
        integer  m
        real     arain

!...Cursys tells whether a crop, forest or savanna system is being simulated.
        if (cursys .eq. CRPSYS .or. cursys .eq. SAVSYS) then
!...crop or savanna system: lignin contend depends on annual rainfall
          arain = 0.
          do 10 m = 1, MONTHS
            arain = arain + prcurr(m)
10        continue
          if (arain .eq. 0.) then
            do 20 m = 1, MONTHS
               arain = arain + precip(m)
20            continue
          endif
        endif


        if (cursys .eq. CRPSYS) then
c...Crop/grass
          pltlig(ABOVE)=fligni(INTCPT,ABOVE)+fligni(SLOPE,ABOVE)*arain
          pltlig(BELOW)=fligni(INTCPT,BELOW)+fligni(SLOPE,BELOW)*arain
!          print *,"cal", arain, pltlig(BELOW), fligni(1,2), fligni(2,2)
        else if (cursys .eq. FORSYS) then
c...Forest system (leaves, fine roots = above ground and below ground)
          pltlig(ABOVE)=wdlig(LEAF)
          pltlig(BELOW)=wdlig(FROOT)

        else if (cursys .eq. SAVSYS) then
c...Savanna

          pltlig(ABOVE) = (wdlig(LEAF)+fligni(INTCPT,ABOVE) +
     $                    fligni(SLOPE,ABOVE) * arain) / 2.0
          pltlig(BELOW) = (wdlig(FROOT)+fligni(INTCPT,BELOW) +
     $                    fligni(SLOPE,BELOW) * arain) / 2.0
        endif

!...Check range for pltlig; the hard-coded values should be replaced with
!...    parameter values someday
        pltlig(ABOVE) = max(0.02, pltlig(ABOVE))
	pltlig(ABOVE) = min(0.5, pltlig(ABOVE))
	pltlig(BELOW) = max(0.02, pltlig(BELOW))
	pltlig(BELOW) = min(0.5, pltlig(BELOW))

        return
        end
