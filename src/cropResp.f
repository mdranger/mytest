!     
! File:   cropResp.f
! Author: zli
! Return the crop autotrophic respiration
! Created on December 5, 2008, 6:55 PM
!
      real function cropResp(gppin) 

!...calculate autotrophic respiration of crop.
!...added by Liu, Nov 2008
!....reference:  Simulating energy and carbon fluxes over winter wheat using
!.....coupled land surface and terrestrial ecosystem models
!.....Vivek K. Arora in Ag For Met 2003
!...Following are from Leo's opion
! THE LUE WOULD BE HIGHER FOR C4 PLANTS (CORN, ETC.).  
! WENPING HAS LOOKED AT THIS.  I THINK IT'S ABOUT 20% HIGHER.  
! ASK WENPING TO MAKE SURE.  
! YOU CAN USE THE C4 FLAG IN CROP.100 TO ADJUST EC-LUE COEFFICIENT.
! YOU CAN TRY TO SIMULATE AUTOTROPHIC RESP USING THE ALGORITHMS FOR TREE LEAVES.  
! IF IT DOES NOT MAKE SENSE, LOOK AT DNDC ALGORITHMS.  SHOULD BE STRAIGHTFORWARD.
! DON'T CONSIDER N AT THE MOMENT.

! input parameters, GPP of crop/grass
! 
      real gppin

      include 'parfs.inc'
      include 'plot1.inc'
      include 'plot3.inc'
      include 'wth.inc'
      include 'const.inc'
      !include 'potent.inc'
      include 'timvar.inc'
      include 'zztim.inc'


!...Local variables
      integer i
      real beta, alpha, q10air, q10soil, net_growth, sapw,
     $ crop_mr(CPARTS), total_mr, total_gr, nppcal
     
!...initialization
      total_mr = 0.0
      total_gr = 0.0
      net_growth = 0.0
      
!...maintenance resp coefficient (kgC/kg N per day) to month by (30)     
      beta = 0.128 * 30.0
      q10air = (3.22 - 0.046*meant)**((meant-20.0)/10.0)
      q10soil = (3.22 - 0.046*stemp)**((stemp-20.0)/10.0)
        
!....maintenence respiration 
      crop_mr(ABOVE) = beta * aglive(N) * q10air 
      crop_mr(BELOW) = beta * bglive(N) * q10soil 
      	do 10 i = 1, CPARTS
             total_mr = total_mr +  crop_mr(i) 
10     continue

!....growth respiration 
!....only exist if there is some growth happening
     
      alpha = 0.35
      net_growth = gppin - total_mr
        
         if (net_growth .gt. 0) then 
             total_gr = alpha * net_growth
         else
             total_gr = 0.0
         endif

!  Save the crop autotrophic respiration
      cropResp = total_mr + total_gr
      return
      end
