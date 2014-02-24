
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...IRRIGT.F

      real function irrigt(month)

      integer   month

c...Simulate irrigation
 
      include 'param.inc'
      include 'parcp.inc'
      include 'plot1.inc'
      include 'wth.inc'

c...Local variables
      integer layr
      real toth2o


c...Check tave so that irrigation water 
c     is not added as snow
      if (tave .le. 0.0) then
        irract = 0.0

c...Add amount given by user
      else
        if (auirri .eq. 0) then
          irract = irramt
c  Liu, test irrigation with half amount, will remove 0.5 later
c          irract = irramt * 0.5

c...Add amount automatically to field capacity
        else if (auirri .eq. 1) then
          if (avh2o(1)/(0.0001+awhc) .le. fawhc) then
            toth2o = prcurr(month)
            do 10 layr = 1, nlaypg
              toth2o = toth2o + asmos(layr)
10          continue
            irract = max(twhc - toth2o,0.0)
c  Liu, test irrigation with half amount, will remove 0.5 later
c            irract = max(twhc - toth2o,0.0) * 0.5
          else
            irract = 0.0
          endif

c...Add amount automatically to nominated amount
        else if (auirri .eq. 2) then
          if (avh2o(1)/(0.0001+awhc) .le. fawhc) then
            irract = irraut
c  Liu, test irrigation with half amount, will remove 0.5 later
c            irract = irraut * 0.5
          else
            irract = 0.0
          endif

c...Add amount automatically to field capacity plus PET
        else if (auirri .eq. 3) then
          if (avh2o(1)/(0.0001+awhc) .le. fawhc) then
            toth2o = prcurr(month)
            do 20 layr = 1, nlaypg
              toth2o = toth2o + asmos(layr)
20          continue
            irract = max(twhc + pet - toth2o,0.0)
c  Liu, test irrigation with half amount, will remove 0.5 later, actually no auirri = 3 exists
c            irract = max(twhc + pet - toth2o,0.0) * 0.5
          else
            irract = 0.0
          endif
        endif
      endif

      irrtot = irrtot + irract
      irrigt = irract

      return
      end
