
c               Copyright 1993 Colorado State University
c                       All Rights Reserved


C...LACALC.F

      subroutine lacalc(lai, rleavc, rlwodc, btolai, maxlai, klai)

	real      lai, rleavc, rlwodc, btolai, maxlai, klai

c...CALLED FROM:  potfor

c...Calculate true LAI using leaf biomass and a biomass-to-LAI
c     conversion parameter which is the slope of a regression
c     line derived from LAI vs Foliar Mass for Slash Pine.

c...Calculate theoretical LAI as a function of large wood mass.
c     There is no strong consensus on the true nature of the relationship
c     between LAI and stemwood mass.  Version 3.0 used a negative exponential
c     relationship between leaf mass and large wood mass, which tended to
c     break down in very large forests.  Many sutdies have cited as "general"
c      an increase of LAI up to a maximum, then a decrease to a plateau value
c     (e.g. Switzer et al. 1968, Gholz and Fisher 1982).  However, this 
c     response is not general, and seems to mostly be a feature of young
c     pine plantations.  Northern hardwoods have shown a monotonic increase 
c     to a plateau  (e.g. Switzer et al. 1968).  Pacific Northwest conifers
c     have shown a steady increase in LAI with no plateau evident (e.g. 
c     Gholz 1982).  In this version, we use a simple saturation fucntion in
c     which LAI increases linearly against large wood mass initially, then
c     approaches a plateau value.  The plateau value can be set very large to
c     give a response of steadily increasing LAI with stemwood. 

c     References:
c             1)  Switzer, G.L., L.E. Nelson and W.H. Smith 1968.
c                 The mineral cycle in forest stands.  'Forest
c                 Fertilization:  Theory and Practice'.  pp 1-9
c                 Tenn. Valley Auth., Muscle Shoals, AL.
c
c             2)  Gholz, H.L., and F.R. Fisher 1982.  Organic matter
c                 production and distribution in slash pine (Pinus
c                 elliotii) plantations.  Ecology 63(6):  1827-1839.
c
c             3)  Gholz, H.L.  1982.  Environmental limits on aboveground
c                 net primary production and biomass in vegetation zones of
c                 the Pacific Northwest.  Ecology 63:469-481.
c


c...Local variables
        real rlai, tlai


        rlai = (rleavc * 2.5) * btolai

        tlai = maxlai * rlwodc / (klai + rlwodc)

c...Choose the LAI reducer on production.  I don not really understand
c     why we take the average in the first case, but it will probably
c     change...

        if (rlai .lt. tlai) then
c           lai = (rlai + tlai) / 2.0
c.....changed by Liu (11/4/2003)
           lai = rlai 
        else
           lai = tlai
        endif

        return
        end

