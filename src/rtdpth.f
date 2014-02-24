c		Shuguang Liu
c		EROS Data Center, 1/00

	subroutine rtdpth()
c...	subroutine rtdpth(depth)
	
c......calculate the rooting depth
	
	include 'prof.inc'
	include 'const.inc'
	include 'parfx.inc'
	
	integer  j
	real rooting
	
	rooting = 0.0
	rlayers = 0
	
c....calculate the layers in the rooting depth
         do 10 j = 1, MAXLYR
           if (adep(j) .gt. 0.0) then
             rooting = rooting + adep(j)
             if (rooting .ge. rootdep) then
                rlayers = rlayers + 1
                goto 20
             endif

              rlayers = rlayers + 1
            endif
10       continue

c... the rooting depth
20           if (rooting .gt. rootdep) then
		aroot = rootdep
	     else
	        aroot = rooting
	     endif
	     
	rooting = 0.0
	slayers = 0
	
c....calculate the layers in the rooting depth (survival)
         do 30 j = 1, MAXLYR
           if (adep(j) .gt. 0.0) then
           rooting = rooting + adep(j)
           if (rooting .ge. survdep) then
              slayers = slayers + 1
              goto 40
           endif
              slayers = slayers + 1
           endif
30       continue

c...the root surviving depth
40           if (rooting .gt. survdep) then
		sroot = survdep
	     else
	        sroot = rooting
	     endif

c	print *, "rootdep, survdep, rlayers, slayers"
c	print *, rootdep, survdep, rlayers, slayers
	return
		
	end
