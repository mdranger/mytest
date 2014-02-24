
c               Copyright 1993 Colorado State University
c                       All Rights Reserved

C...PEVAP.F
c...Modified by zp using LEO's new thornthwaite equation
c...for PET calculation 08/30/2006
!...08/01/2010, found if we have negative prec, the program
!...have access error, 

      real function pevap(mnum)
 
      integer   mnum

      include 'const.inc'
      include 'param.inc'
      include 'parfx.inc'
      include 'site.inc'
      real daylen
      external daylen
c...Local variables
      integer   i
      real      avgmth(MONTHS), e, elev, highest, lowest, monpet,
     $          ra, t, td, tm, tr, day_length, num_days, heat_index,
     $          a_index, sum_temp


      elev = 0.0
c      sitlat = 40.0
      heat_index = 0.1
c...Determine max and min temperatures

      do 10 i = 1, 12
        avgmth(i) = (maxtmp(i) + mintmp(i)) * 0.5
        sum_temp = sum_temp + avgmth(i)
10    continue
      highest = avgmth(1)
      lowest = avgmth(1)
      do 20 i = 2, 12
        highest = max(highest, avgmth(i))
        lowest = min(lowest, avgmth(i))
20    continue
      lowest = MAX(lowest, -10.)

c...Determine average temperature range
      ra = abs(highest - lowest)

c...Temperature range calculation
      tr = maxtmp(mnum) - MAX(-10., mintmp(mnum))

      t = tr/2.+mintmp(mnum)
      tm = t+0.006*elev
      td = 0.0023*elev+0.37*t+0.53*tr+.35*ra-10.9
      e = ((700.*tm/(100.-abs(sitlat)))+15.*td)/(80.-t)
c Modified by zp, using Thornthwaite Equation
c E = 1.6 * Daylength / 12 * Number of days
      day_length = daylen(mnum, sitlat)
      num_days = 30
c The log function cannot have negative value
      heat_index  = 0
      do 30 i = 1, 12
        if ( avgmth(mnum) .lt. 0 ) then
        sum_temp = -1 * (abs(avgmth(mnum)) * 0.2 + 0.0001)**1.514
        else
        sum_temp = (avgmth(mnum) * 0.2 + 0.0001)**1.514
        endif
        heat_index  = sum_temp + heat_index
30    continue
      
      a_index = 0.000000675 * heat_index * heat_index * heat_index - 
     $          0.0000771 * heat_index * heat_index + 0.0179 * 
     $          heat_index + 0.49

      heat_index = exp ( a_index * log (10 * avgmth(mnum) / heat_index))
      monpet = 1.6 * day_length / 12 * num_days / 30  * heat_index
            
c      monpet = (e*30.)/10.

      if (monpet .lt. 0.5) then
        monpet = 0.5
      endif

c...fwloss(4) is a modifier for PET loss.   vek may90
      pevap = monpet*fwloss(4)
!      print *, 'month', mnum, avgmth(mnum), heat_index, monpet, pevap
      return
      end
