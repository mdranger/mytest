!     
! File:   AutoResp.f90
! Author: zli
!
! Created on December 5, 2008, 6:55 PM
!
!
      real function autoResp(fcmax) 

!...calculate autotrophic respiration of trees and return the total
!...Auto trophic respiration
!...added by Liu, Nov 2008
!....reference:  Simulating energy and carbon fluxes over winter wheat using
!.....coupled land surface and terrestrial ecosystem models
!.....Vivek K. Arora in Ag For Met 2003
      real  fcmax
      include 'parfs.inc'
      include 'plot1.inc'
      include 'plot3.inc'
      include 'wth.inc'
      include 'const.inc'
      !include 'potent.inc'
      include 'timvar.inc'
      include 'zztim.inc'


c...Local variables
      integer i
      real beta, alpha, q10air, q10soil, net_growth, sapw,
     $ tree_mr(5), total_mr, total_gr 
     
c...initialization
      total_mr = 0.0
      total_gr = 0.0
      net_growth = 0.0
      
c...maintenance resp coefficient (kgC/kg N per day) to month by (30)     
      beta = 0.128 * 30.0
      q10air = (3.22 - 0.046*meant)**((meant-20.0)/10.0)
      q10soil = (3.22 - 0.046*stemp)**((stemp-20.0)/10.0)
      
      

c...For large wood, calculate the percentage which is live (sapwood)
c...only sapwood has maintenance respiration
        sapw = sapk / (sapk + rlwodc)
        
!....maintenence respiration 
      tree_mr(LEAF) = beta * rleave(N) * q10air 
      tree_mr(FROOT) = beta * froote(N) * q10soil 
      tree_mr(FBRCH) = beta * fbrche(N) * q10air 
      tree_mr(LWOOD) = beta * rlwode(N) * sapw * q10air 
      tree_mr(CROOT) = beta * croote(N) * q10soil 
      	do 10 i = 1, FPARTS
             total_mr = total_mr +  tree_mr(i) 
10     continue

!....growth respiration 
!....only exist if there is some growth happening
     
      alpha = 0.35
      net_growth = fcmax - total_mr
        
         if (net_growth .gt. 0) then 
             total_gr = alpha * net_growth
         else
             total_gr = 0.0
         endif

      autoResp = total_mr + total_gr

      return
      end
