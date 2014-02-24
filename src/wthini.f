c              Copyright 1993 Colorado State University
c                       All Rights Reserved

      subroutine wthini(precip,prcstd,prcskw,mintmp,maxtmp)

      real      precip(12), prcstd(12), prcskw(12), mintmp(12), 
     $          maxtmp(12)

c...Determine what weather data will be used.
 
      include 'chrvar.inc'
      include 'const.inc'
      include 'wth.inc'

c...Function declarations
      real      anorm, sknorm
      external  anorm, sknorm

c...Local variables
      integer   m
      character label*4, string*80

c...Weather options
c     'M' use the precipitation and temperature values which were
c         read from the site.100 file
c     'S' use the temperature values which were read from the site.100
c         file; select precipitation values from a skewed or 
c         from a normal distribution using values in precip as means
c         and values in prcstd as standard deviations and values in 
c         prcskw as skewed.
c     'F' Open an actual weather data file and read precipitation and
c         temperature data.
c     'C' Continue reading precipitation and temperature data
c    	  from the current weather file.
c


c...Initialize mintmp and maxtmp
      do 10 m = 1, MONTHS
        mintmp(m) = tmn2m(m)
        maxtmp(m) = tmx2m(m)
10    continue

      if (wthr .eq. 'M') then
c...Use values in precip
        do 20 m = 1, MONTHS
          prcnxt(m) = precip(m)
20      continue

      elseif (wthr .eq. 'S') then

c...If skewness value is 0.0, select precipitation values from
c     normal distribution
c   Else, use skewness value to generate precip values
        do 30 m = 1, MONTHS
          if (prcskw(m) .eq. 0.0) then
            prcnxt(m) = max(0.0, anorm(precip(m),prcstd(m)))
          else
            prcnxt(m) = max(0.0, 
     $                  sknorm(precip(m), prcstd(m), prcskw(m)))
          endif
30      continue

      elseif (wthr .eq. 'F') then
c...Read from weather data file
        write(*,*) wthnam
c     	open(unit=9,file=wthnam,status='OLD')
     	open(unit=9,file=wthnam,status='OLD')
	rewind 9
   
        read(9,40,end=50) label,prcnxt
!      print *, label, prcnxt
40      format(a4,6x,12f7.0)
        goto 60
c...Error reading from weather file
50        string = '   The weather data file could not be read: ' //
     $              wthnam
          call message(string)
          STOP

60      if (label .ne. wlabel(1)) then
          call faterr(label,wlabel(1))
        endif

c...Check for -99.99 values and generate skewed values for ones found
c...Read in 12 month data
        do 70 m = 1, MONTHS
          if (prcnxt(m) .eq. -99.99) then
            if (prcskw(m) .eq. 0.0) then
              prcnxt(m) = max(0.0, anorm(precip(m),prcstd(m)))
	    else
              prcnxt(m) = max(0.0, 
     $                  sknorm(precip(m), prcstd(m), prcskw(m)))
	    endif
          endif
70      continue

      elseif (wthr .eq. 'C') then
c... Continue with current weather file
      endif

      return
      end
