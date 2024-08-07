#==========================================================================================#
#==========================================================================================#
#     This list contains the list of all commonly used units written in mathematical       #
# format.                                                                                  #
#------------------------------------------------------------------------------------------#
untab <<- list( cg             = "c*g"
              , cm             = "c*m"
              , cmohaoyr       = "c*m^phantom(1)*h*a^{-1}*y*r^{-1}"
              , cmoyr          = "c*m^phantom(1)*y*r^{-1}"
              , cm2            = "c*m^2"
              , cm2m           = "c*m^2*m"
              , cm2om2         = "c*m^{2}*m^{-2}"
              , cm2om2odec     = "c*m^{2}*m^{-2}*d*e*c^{-1}"
              , cm2opl         = "c*m^{2}*p*l*a*n*t^{-1}"
              , cm2om2oyr      = "c*m^{2}*m^{-2}*y*r^{-1}"
              , day            = "d*a*y"
              , deg            = "degree"
              , degC           = "degree*C"
              , degCodec       = "degree*C^phantom(1)*d*e*c^{-1}"
              , degE           = "degree*E"
              , degN           = "degree*N"
              , degS           = "degree*S"
              , degW           = "degree*W"
              , empty          = "phantom(1)-phantom(1)"
              , gcoday         = "g*C^phantom(1)*d*a*y^{-1}"
              , gcokg          = "g*C^phantom(1)*k*g^{-1}"
              , gcokgw         = "g*C^phantom(1)*k*g*H[2]*O^{-1}"
              , gcokgcbio      = "g*C^phantom(1)*k*g[C[b*i*o]]^{-1}"
              , gcom2oday      = "g*C^phantom(1)*m^{-2}*d*a*y^{-1}"
              , gcom2odayodec  = "g*C^phantom(1)*m^{-2}*d*a*y^{-1}*d*e*c^{-1}"
              , gcom2loday     = "g*C^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}"
              , gcom2lodayodec = "g*C^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}*d*e*c^{-1}"
              , gdm            = "g^phantom(1)*d*m"
              , Gjom2          = "G*J^phantom(1)*m^{-2}"
              , gmpaos         = "g^phantom(1)*M*P*a^phantom(1)*s^{-1}"
              , gmt            = "G*M*T"
              , gnokg          = "g*N^phantom(1)*k*g^{-1}"
              , gom2oday       = "g^phantom(1)*m^{-2}*d*a*y^{-1}"
              , gom3           = "g^phantom(1)*m^{-3}"
              , gocm3          = "g^phantom(1)*c*m^{-3}"
              , gocm3odec      = "g^phantom(1)*c*m^{-3}*d*e*c^{-1}"
              , gokg           = "g^phantom(1)*k*g^{-1}"
              , gpokg          = "g*P^phantom(1)*k*g^{-1}"
              , gwokg          = "g*H[2]*O^phantom(1)*k*g^{-1}"
              , gwokgodec      = "g*H[2]*O^phantom(1)*k*g^{-1}*d*e*c^{-1}"
              , gwom2oday      = "g*H[2]*O^phantom(1)*m^{-2}*d*a*y^{-1}"
              , ha             = "h*a"
              , hpa            = "h*P*a"
              , hpaodec        = "h*P*a^phantom(1)*d*e*c^{-1}"
              , hr             = "h*o*u*r"
              , jokgom2        = "J^phantom(1)*k*g^{-1}*m^{-2}"
              , jokgopl        = "J^phantom(1)*k*g^{-1}*p*l^{-1}"
              , jom2           = "J^phantom(1)*m^{-2}"
              , K              = "K"
              , Kodec          = "K^phantom(1)*d*e*c^{-1}"
              , k              = "K"
              , kg             = "k*g"
              , kgc            = "k*g*C"
              , kgcokgc        = "k*g*C^phantom(1)*k*g*C^{-1}"
              , kgcokgn        = "k*g*C^phantom(1)*k*g*N^{-1}"
              , kgcokgp        = "k*g*C^phantom(1)*k*g*P^{-1}"
              , kgcom2         = "k*g*C^phantom(1)*m^{-2}"
              , kgcom2odec     = "k*g*C^phantom(1)*m^{-2}*d*e*c^{-1}"
              , kgcom2l        = "k*g*C^phantom(1)*m[l*e*a*f]^{-2}"
              , kgcom2loyr     = "k*g*C^phantom(1)*m[l*e*a*f]^{-2}*y*r^{-1}"
              , kgcom2loyrodec = "k*g*C^phantom(1)*m[l*e*a*f]^{-2}*y*r^{-1}*dec^{-1}"
              , kgcom2oday     = "k*g*C^phantom(1)*m^{-2}*d*a*y^{-1}"
              , kgcom2oyr      = "k*g*C^phantom(1)*m^{-2}*y*r^{-1}"
              , kgcom2oyrodec  = "k*g*C^phantom(1)*m^{-2}*y*r^{-1}*d*e*c^{-1}"
              , kgcom3         = "k*g*C^phantom(1)*m^{-3}"
              , kgcopl         = "k*g*C^phantom(1)*p*l*a*n*t^{-1}"
              , kgcoployr      = "k*g*C^phantom(1)*p*l*a*n*t^{-1}*y*r^{-1}"
              , kgcoyr         = "k*g*C^phantom(1)*y*r^{-1}"
              , kgokg          = "k*g^phantom(1)*k*g^{-1}"
              , kgom2          = "k*g^phantom(1)*m^{-2}"
              , kgom2loday     = "k*g^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}"
              , kgom3          = "k*g^phantom(1)*m^{-3}"
              , kgomos2        = "k*g^phantom(1)*m^{-1}*s^{-2}"
              , kgnokg         = "k*g*N^phantom(1)*k*g^{-1}"
              , kgnokgp        = "k*g*N^phantom(1)*k*g*P^{-1}"
              , kgnom2         = "k*g*N^phantom(1)*m^{-2}"
              , kgpokg         = "k*g*P^phantom(1)*k*g^{-1}"
              , kgwokg         = "k*g*H[2]*O^phantom(1)*k*g^{-1}"
              , kgpom2         = "k*g*P^phantom(1)*m^{-2}"
              , kgwom2         = "k*g*H[2]*O^phantom(1)*m^{-2}"
              , kgwom2l        = "k*g*H[2]*O^phantom(1)*m[l*e*a*f]^{-2}"
              , kgwom2oday     = "k*g*H[2]*O^phantom(1)*m^{-2}*d*a*y^{-1}"
              , kgwom2odec     = "k*g*H[2]*O^phantom(1)*m^{-2}*d*e*c^{-1}"
              , kgwom2ohr      = "k*g*H[2]*O^phantom(1)*m^{-2}*h*r^{-1}"
              , kgwom2os       = "k*g*H[2]*O^phantom(1)*m^{-2}*s^{-1}"
              , kgwom2loday    = "k*g*H[2]*O^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}"
              , kgwom3oday     = "k*g*H[2]*O^phantom(1)*m^{-3}*d*a*y^{-1}"
              , kgwom3ompa     = "k*g*H[2]*O^phantom(1)*m^{-3}*M*P*a^{-1}"
              , kgwomosompa    = "k*g*H[2]*O^phantom(1)*m^{-1}*s^{-1}*M*P*a^{-1}"
              , kgwoploday     = "k*g*H[2]*O^phantom(1)*p*l*a*n*t^{-1}*d*a*y^{-1}"
              , km             = "k*m"
              , Kmos           = "K^phantom(1)*m^phantom(1)*s^{-1}"
              , knom           = "k*N^phantom(1)*m^{-1}"
              , kpa            = "k*P*a"
              , kwom           = "k*W^phantom(1)*m^{-1}"
              , m              = "m"
              , m2             = "m^{2}"
              , m2ocm2         = "m^{2}*c*m^{-2}"
              , mg             = "m*g"
              , mgcom2oday     = "m*g*C^phantom(1)*m^{-2}*d*a*y^{-1}"
              , mgcom2loday    = "m*g*C^phantom(1)*m[l*e*a*f]^{-2}*d*a*y^{-1}"
              , min            = "m*i*n"
              , mm             = "m*m"
              , mmodec         = "m*m^phantom(1)*d*e*c^{-1}"
              , mmoyrodec      = "m*m^phantom(1)*y*r^{-1}*d*e*c^{-1}"
              , mm2okgw        = "m*m^{2}*k*g*H[2]*O^{-1}"
              , mmoday         = "m*m^phantom(1)*d*a*y^{-1}"
              , mmodayodec     = "m*m^phantom(1)*d*a*y^{-1}*d*e*c^{-1}"
              , mmohr          = "m*m^phantom(1)*h*r^{-1}"
              , mmolom2os      = "m*m*o*l^phantom(1)*m^{-2}*s^{-1}"
              , mmolom2osompa  = "m*m*o*l^phantom(1)*m^{-2}*s^{-1}*M*P*a^{-1}"
              , mmologos       = "m*m*o*l^phantom(1)*g^{-1}*s^{-1}"
              , mmolokgos      = "m*m*o*l^phantom(1)*k*g^{-1}*s^{-1}"
              , mmolcokg       = "k*g^phantom(1)*m*m*o*l*C^{-1}"
              , mmomo          = "m*m^phantom(1)*m*o*n*t*h^{-1}"
              , mnom2          =" M*N^phantom(1)*m^{-2}"
              , mog            = "m^phantom(1)*g^{-1}"
              , mokg           = "m^phantom(1)*k*g^{-1}"
              , molom2         = "m*o*l^phantom(1)*m^{-2}"
              , molom2osompa   = "m*o*l^phantom(1)*m^{-2}*s^{-1}*M*P*a^{-1}"
              , molom2l        = "m*o*l^phantom(1)*m[l*e*a*f]^{-2}"
              , molom2lompa    = "m*o*l^phantom(1)*m[l*e*a*f]^{-2}*M*P*a^{-1}"
              , molcom2        = "m*o*l*C^phantom(1)*m^{-2}"
              , molosompa      = "m*o*l^phantom(1)*s^{-1}*M*P*a^{-1}"
              , month          = "m*o*n*t*h"
              , monthodec      = "m*o*n*t*h^phantom(1)*d*e*c^{-1}"
              , mos            = "m^phantom(1)*s^{-1}"
              , momin          = "m^phantom(1)*m*i*n^{-1}"
              , mosodec        = "m^phantom(1)*s^{-1}*d*e*c^{-1}"
              , m2og           = "m^{2}*g^{-1}"
              , m2oha          = "m^{2}*h*a^{-1}"
              , m2ohaoyr       = "m^{2}*h*a^{-1}*y*r^{-1}"
              , m2okg          = "m^{2}*k*g^{-1}"
              , m2om2          = "m^{2}*m^{-2}"
              , m2om3          = "m^{2}*m^{-3}"
              , m2opl          = "m^{2}*p*l^{-1}"
              , m2lokg         = "m[l*e*a*f]^{2}*k*g^{-1}"
              , m2lokgc        = "m[l*e*a*f]^{2}*k*g*C^{-1}"
              , m2lokgcodec    = "m[l*e*a*f]^{2}*k*g*C^{-1}*d*e*c^{-1}"
              , m2lom2         = "m[l*e*a*f]^{2}*m^{-2}"
              , m2lom2odec     = "m[l*e*a*f]^{2}*m^{-2}*d*e*c^{-1}"
              , m2lopl         = "m[l*e*a*f]^{2}*p*l^{-1}"
              , m2pom2         = "m[l*e*a*f+w*o*o*d]^{2}*m^{-2}"
              , m2popl         = "m[l*e*a*f+w*o*o*d]^{2}*p*l^{-1}"
              , m2wom2         = "m[w*o*o*d]^{2}*m^{-2}"
              , m2wopl         = "m[w*o*o*d]^{2}*p*l^{-1}"
              , m3oha          = "m^{3}*h*a^{-1}"
              , m3om2          = "m^{3}*m^{-2}"
              , m3wom3         = "m[W]^{3}*m^{-3}"
              , Mgcoha         = "M*g*C^phantom(1)*h*a^{-1}"
              , Mgoha          = "M*g^phantom(1)*h*a^{-1}"
              , Mgwom2         = "M*g*H[2]*O^phantom(1)*m^{-2}"
              , Mjom2          = "M*J^phantom(1)*m^{-2}"
              , mmoyr          = "m*m^phantom(1)*y*r^{-1}"
              , mpa            = "M*P*a"
              , mpaodec        = "M*P*a^phantom(1)*d*e*c^{-1}"
              , mwom2ok        = "m*W^phantom(1)*m^{-2}*K^{-1}"
              , nmo.090        = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 90*m*m^phantom(1)*m*o^{-1}"
              , nmo.100        = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 100*m*m^phantom(1)*m*o^{-1}"
              , nmo.110        = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 110*m*m^phantom(1)*m*o^{-1}"
              , nmo.120        = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(R) < 120*m*m^phantom(1)*m*o^{-1}"
              , nmo.wdef       = "m*o*n*t*h*s*phantom(1)*\"|\"*phantom(1)*bar(W*D) > 10*m*m^phantom(1)*m*o^{-1}"
              , nmolomol       = "n*m*o*l^phantom(1)*m*o*l^{-1}"
              , nomm           = "N^phantom(1)*m*m^{-1}"
              , nomm2          = "N^phantom(1)*m*m^{-2}"
              , oneoday        = "d*a*y^{-1}"
              , oneom          = "m^{-1}"
              , oneompa        = "M*P*a^{-1}"
              , oneoha         = "h*a^{-1}"
              , oneok          = "K^{-1}"
              , oneokm2        = "k*m^{-2}"
              , oneokm2oday    = "k*m^{-2}*d*a*y^{-1}"
              , oneokm2omo     = "k*m^{-2}*m*o*n*t*h^{-1}"
              , oneom2         = "m^{-2}"
              , oneoyr         = "y*r^{-1}"
              , oneodec        = "d*e*c^{-1}"
              , pa             = "P*a"
              , pc             = "'%'"
              , pcoday         = "'%'^phantom(1)*d*a*y^{-1}"
              , pcodec         = "'%'^phantom(1)*d*e*c^{-1}"
              , pcbio          = "'%'[b*i*o]"
              , pcagboyr       = "'%'[A*G*B]^phantom(1)*y*r^{-1}"
              , pcagboyrodec   = "'%'[A*G*B]^phantom(1)*y*r^{-1}*d*e*c^{-1}"
              , pcagbo50yr     = "'%'[A*G*B]^phantom(1)*group(\"(\",50*y*r,\")\")^{-1}"
              , pcareaoyr      = "'%'[a*r*e*a]^phantom(1)*y*r^{-1}"
              , pcbaoyr        = "'%'[B*A]^phantom(1)*y*r^{-1}"
              , pcbiooyr       = "'%'[b*i*o]^phantom(1)*y*r^{-1}"
              , pcbiooyrodec   = "'%'[b*i*o]^phantom(1)*y*r^{-1}*d*e*c^{-1}"
              , pcdbhoyr       = "'%'[D*B*H]^phantom(1)*y*r^{-1}"
              , pcdbhoyrodec   = "'%'[D*B*H]^phantom(1)*y*r^{-1}*d*e*c^{-1}"
              , pcetoyr        = "'%'[E*T]^phantom(1)*y*r^{-1}"
              , pceto50yr      = "'%'[E*T]^phantom(1)*group(\"(\",50*y*r,\")\")^{-1}"
              , pcoyr          = "'%'^phantom(1)*y*r^{-1}"
              , pcoyrodec      = "'%'^phantom(1)*y*r^{-1}*d*e*c^{-1}"
              , pcpopoyr       = "'%'[p*o*p]^phantom(1)*y*r^{-1}"
              , pcsat          = "'%'[S*a*t]"
              , pgc            = "P*g*C"
              , pgcoyr         = "P*g*C^phantom(1)*y*r^{-1}"
              , plom2          = "p*l*a*n*t^phantom(1)*m^{-2}"
              , plom2odec      = "p*l*a*n*t^phantom(1)*m^{-2}*d*e*c^{-1}"
              , ploha          = "p*l*a*n*t^phantom(1)*h*a^{-1}"
              , plohaoyr       = "p*l*a*n*t^phantom(1)*h*a^{-1}*y*r^{-1}"
              , plohaodec      = "p*l*a*n*t^phantom(1)*h*a^{-1}*d*e*c^{-1}"
              , s              = "s"
              , t1e3           = "phantom(1)%*%10^{3}"
              , t1e6           = "phantom(1)%*%10^{6}"
              , t1e9           = "phantom(1)%*%10^{9}"
              , thkm2oyr       = "10^{3*phantom(1)}*k*m^{2}*y*r^{-1}"
              , umolcom2os     = "mu*m*o*l*C^phantom(1)*m^{-2}*s^{-1}"
              , umologcos      = "mu*m*o*l^phantom(1)*g*C^{-1}*s^{-1}"
              , umologos       = "mu*m*o*l^phantom(1)*g^{-1}*s^{-1}"
              , umolokgcos     = "mu*m*o*l^phantom(1)*k*g*C^{-1}*s^{-1}"
              , umolokgos      = "mu*m*o*l^phantom(1)*k*g^{-1}*s^{-1}"
              , umolom2        = "mu*m*o*l^phantom(1)*m^{-2}"
              , umolom2os      = "mu*m*o*l^phantom(1)*m^{-2}*s^{-1}"
              , molom2osompa   = "m^{2}*s^phantom(1)*M*P*a^phantom(1)*m*o*l^{-1}"
              , molom2lompa    = "m[l*e*a*f]^{2}*M*P*a^phantom(1)*m*o*l^{-1}"
              , molosompa      = "s^phantom(1)*M*P*a^phantom(1)*m*o*l^{-1}"
              , umolom2l       = "mu*m*o*l^phantom(1)*m[l*e*a*f]^{-2}"
              , umolom2los     = "mu*m*o*l^phantom(1)*m[l*e*a*f]^{-2}*s^{-1}"
              , umolcom2       = "mu*m*o*l*C^phantom(1)*m^{-2}"
              , umolcomol      = "mu*m*o*l*C^phantom(1)*m*o*l^{-1}"
              , umolomol       = "mu*m*o*l^phantom(1)*m*o*l^{-1}"
              , umolomolodec   = "mu*m*o*l^phantom(1)*m*o*l^{-1}*d*e*c^{-1}"
              , usd            = "U*S*D"
              , usdoyr         = "U*S*D^phantom(1)*y*r^{-1}"
              , utc            = "U*T*C"
              , wom2           = "W^phantom(1)*m^{-2}"
              , wom2l          = "W^phantom(1)*m[l*e*a*f]^{-2}"
              , wom2oyr        = "W^phantom(1)*m^{-2}*y*r^{-1}"
              , wom2odec       = "W^phantom(1)*m^{-2}*d*e*c^{-1}"
              , wopl           = "W^phantom(1)*p*l*a*n*t^{-1}"
              , yr             = "y*r"
              , zscore         = "Z[s*c*o*r*e]"
              , zscoreodec     = "Z[s*c*o*r*e]^phantom(1)*d*e*c^{-1}"
              )#end list
#==========================================================================================#
#==========================================================================================#




#==========================================================================================#
#==========================================================================================#
#     This list contains the inverse of all commonly used units written in mathematical    #
# format.  The inverse is useful for probability distribution functions (units of the      #
# density function are the inverse of the actual units of the quantity, so the integral    #
# becomes dimensionless.                                                                   #
#------------------------------------------------------------------------------------------#
i.untab <<- list( cg             = "c*g^{-1}"
                , cm             = "c*m^{-1}"
                , cmohaoyr       = "h*a^phantom(1)*y*r^phantom(1)*c*m^{-1}"
                , cmoyr          = "y*r^phantom(1)*c*m^{-1}"
                , cm2            = "c*m^{-2}"
                , cm2m           = "c*m^{-2}*m^{-1}"
                , cm2om2         = "m^{2}*c*m^{-2}"
                , cm2om2odec     = "d*e*c^phantom(1)*m^{2}*c*m^{-2}"
                , cm2opl         = "p*l*a*n*t^phantom(1)*c*m^{-2}"
                , cm2om2oyr      = "m^{2}*y*r^phantom(1)*c*m^{-2}"
                , day            = "d*a*y^{-1}"
                , deg            = "degree^{-1}"
                , degC           = "degree*C^{-1}"
                , degCodec       = "d*e*c^phantom(1)*degree*C^{-1}"
                , degE           = "degree*E^{-1}"
                , degN           = "degree*N^{-1}"
                , degS           = "degree*S^{-1}"
                , degW           = "degree*W^{-1}"
                , empty          = "phantom(1)-phantom(1)"
                , gcoday         = "d*a*y^phantom(1)*g*C^{-1}"
                , gcokg          = "k*g^phantom(1)*g*C^{-1}"
                , gcom2oday      = "m^{2}*d*a*y^phantom(1)*g*C^{-1}"
                , gcom2odayodec  = "m^{2}*d*a*y^phantom(1)*g*C^{-1}"
                , gcom2loday     = "m[l*e*a*f]^{2}*d*a*y^phantom(1)*d*e*c^phantom(1)*g*C^{-1}"
                , gcom2lodayodec = "m[l*e*a*f]^{2}*d*a*y^phantom(1)*d*e*c^phantom(1)*g*C^{-1}"
                , gcokgw         = "k*g*H[2]*O^phantom(1)*g*C^{-1}"
                , gcokgcbio      = "k*g[C[b*i*o]]^phantom(1)*g*C^{-1}"
                , gdm            = "g^{-1}*d*m^{-1}"
                , Gjom2          = "m^{2}*G*J^{-1}"
                , gmpaos         = "s^phantom(1)*g^{-1}*M*P*a^{-1}"
                , gmt            = "G*M*T^{-1}"
                , gnokg          = "k*g^phantom(1)*g*N^{-1}"
                , gom2oday       = "m^{2}*d*a*y^phantom(1)*g^{-1}"
                , gom3           = "m^{3}*g^{-1}"
                , gocm3          = "c*m^{3}*g^{-1}"
                , gocm3odec      = "d*e*c^phantom(1)*c*m^{3}*g^{-1}"
                , gokg           = "k*g^phantom(1)*g^{-1}"
                , gpokg          = "k*g^phantom(1)*g*P^{-1}"
                , gwokg          = "k*g^phantom(1)*g*H[2]*O^{-1}"
                , gwokgodec      = "k*g^phantom(1)*d*e*c^phantom(1)*g*H[2]*O^{-1}"
                , gwom2oday      = "m^{2}*d*a*y^phantom(1)*g*H[2]*O^{-1}"
                , ha             = "h*a^{-1}"
                , hpa            = "h*P*a^{-1}"
                , hpaodec        = "d*e*c^phantom(1)*h*P*a^{-1}"
                , hr             = "h*o*u*r^{-1}"
                , jokgom2        = "k*g^phantom(1)*m^{-2}*J^{-1}"
                , jokgopl        = "k*g^phantom(1)*pl^phantom(1)*J^{-1}"
                , jom2           = "m^{2}*J^{-1}"
                , K              = "K^{-1}"
                , Kodec          = "d*e*c^phantom(1)*K^{-1}"
                , k              = "K^{-1}"
                , kg             = "k*g^{-1}"
                , kgc            = "k*g*C^{-1}"
                , kgcokgc        = "k*g*C^phantom(1)*k*g*C^{-1}"
                , kgcokgn        = "k*g*N^phantom(1)*k*g*C^{-1}"
                , kgcokgp        = "k*g*P^phantom(1)*k*g*C^{-1}"
                , kgcom2         = "m^{2}*k*g*C^{-1}"
                , kgcom2odec     = "d*e*c^phantom(1)*m^{2}*k*g*C^{-1}"
                , kgcom2l        = "m[l*e*a*f]^{2}*k*g*C^{-1}"
                , kgcom2loyr     = "m[l*e*a*f]^{2}*y*r^phantom(1)*k*g*C^{-1}"
                , kgcom2loyrodec = "m[l*e*a*f]^{2}*y*r^phantom(1)*d*e*c^phantom(1)*k*g*C^{-1}"
                , kgcom2oday     = "m^{2}*d*a*y^phantom(1)*k*g*C^{-1}"
                , kgcom2oyr      = "m^{2}*y*r^phantom(1)*k*g*C^{-1}"
                , kgcom2oyrodec  = "m^{2}*y*r^phantom(1)*d*e*c^phantom(1)*k*g*C^{-1}"
                , kgcom3         = "m^{3}*k*g*C^{-1}"
                , kgcopl         = "p*l*a*n*t^phantom(1)*k*g*C^{-1}"
                , kgcoployr      = "p*l*a*n*t^phantom(1)*y*r^phantom(1)*k*g*C^{-1}"
                , kgcoyr         = "y*r^phantom(1)*k*g*C^{-1}"
                , kgokg          = "k*g^phantom(1)*k*g^{-1}"
                , kgom2          = "m^{2}*k*g^{-1}"
                , kgom3          = "m^{3}*k*g^{-1}"
                , kgomos2        = "m^phantom(1)*s^{2}*k*g^{-1}"
                , kgnokg         = "k*g^phantom(1)*k*g*N^{-1}"
                , kgnokgp        = "k*g*P^phantom(1)*k*g*N^{-1}"
                , kgnom2         = "m^{2}*k*g*N^{-1}"
                , kgpokg         = "k*g^phantom(1)*k*g*P^{-1}"
                , kgwokg         = "k*g^phantom(1)*k*g*H[2]*O^{-1}"
                , kgpom2         = "m^{2}*k*g*P^{-1}"
                , kgwom2         = "m^{2}*k*g*H[2]*O^{-1}"
                , kgwom2l        = "m[l*e*a*f]^{2}*k*g*H[2]*O^{-1}"
                , kgwom2oday     = "m^{2}*d*a*y^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom2odec     = "m^{2}*d*e*c^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom2ohr      = "m^{2}*h*r^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom2os       = "m^{2}*s^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom2loday    = "m[l*e*a*f]^{2}*d*a*y^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom3oday     = "m^{-3}*d*a*y^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwom3ompa     = "m^{-3}*M*P*a^phantom(1)*k*g*H[2]*O^{-1}"
                , kgwomosompa    = "m^phantom(1)*s^phantom(1)*M*P*a^phantom(1)*kg^{-1}"
                , kgwoploday     = "p*l*a*n*t^phantom(1)*d*a*y^phantom(1)*k*g*H[2]*O^{-1}"
                , km             = "k*m^{-1}"
                , Kmos           = "s^phantom(1)*K^{-1}*m^{-1}"
                , knom           = "m^phantom(1)*k*N^{-1}"
                , kpa            = "k*P*a^{-1}"
                , kwom           = "m^phantom(1)*k*W^{-1}"
                , m              = "m^{-1}"
                , m2             = "m^{-2}"
                , m2ocm2         = "c*m^{2}*m^{-2}"
                , mg             = "m*g^{-1}"
                , mgcom2oday     = "m^{2}*d*a*y^phantom(1)*m*g*C^{-1}"
                , mgcom2loday    = "m[l*e*a*f]^{2}*d*a*y^phantom(1)*m*g*C^{-1}"
                , min            = "m*i*n^{-1}"
                , mm             = "m*m^{-1}"
                , mmodec         = "d*e*c^phantom(1)*m*m^^{-1}"
                , mmoyrodec      = "y*r^phantom(1)*d*e*c^phantom(1)*m*m^^{-1}"
                , mm2okgw        = "k*g*H[2]*O^phantom(1)*m*m^{-2}"
                , mmoday         = "d*a*y^phantom(1)*m*m^{-1}"
                , mmodayodec     = "d*a*y^phantom(1)*d*e*c^phantom(1)*m*m^{-1}"
                , mmohr          = "h*r^phantom(1)*m*m^{-1}"
                , mmologos       = "g^phantom(1)*s^phantom(1)*m*m*o*l^{-1}"
                , mmolokgos      = "k*g^phantom(1)*s^phantom(1)*m*m*o*l^{-1}"
                , mmolom2os      = "m^{2}*s^phantom(1)*m*m*o*l^{-1}"
                , mmolom2osompa  = "m^{2}*s^phantom(1)*M*P*a^phantom(1)*m*m*o*l^{-1}"
                , mmolcokg       = "m*m*o*l*C^phantom(1)*k*g^{-1}"
                , mmomo          = "m*o*n*t*h^phantom(1)*^m*m{-1}"
                , mnom2          =" m^{2}*M*N^{-1}"
                , mog            = "g^phantom(1)*m^{-1}"
                , mokg           = "k*g^phantom(1)*m^{-1}"
                , molom2         = "m^{2}*m*o*l^{-1}"
                , molom2l        = "m[l*e*a*f]^{2}*m*o*l^{-1}"
                , molcom2        = "m^{2}*m*o*l*C^{-1}"
                , month          = "m*o*n*t*h^{-1}"
                , monthodec      = "d*e*c^phantom(1)*m*o*n*t*h^{-1}"
                , mos            = "s^phantom(1)*m^{-1}"
                , momin          = "m*i*n^phantom(1)*m^{-1}"
                , mosodec        = "s^phantom(1)*d*e*c^phantom(1)*m^{-1}"
                , m2og           = "g^phantom(1)*m^{-2}"
                , m2oha          = "h*a^phantom(1)*m^{-2}"
                , m2ohaoyr       = "h*a^phantom(1)*m^{2}*y*r^phantom(1)*h*a^{-1}"
                , m2okg          = "k*g^phantom(1)*m^{-2}"
                , m2om2          = "m^{2}*m^{-2}"
                , m2om3          = "m^{3}*m^{-2}"
                , m2opl          = "p*l^phantom(1)*m^{-2}"
                , m2lokg         = "k*g^phantom(1)*m[l*e*a*f]^{-2}"
                , m2lokgc        = "k*g*C^phantom(1)*m[l*e*a*f]^{-2}"
                , m2lokgcodec    = "d*e*c^phantom(1)*k*g*C^phantom(1)*m[l*e*a*f]^{-2}"
                , m2lom2         = "m^{2}*m[l*e*a*f]^{-2}"
                , m2lom2odec     = "m^{2}*d*e*c^phantom(1)*m[l*e*a*f]^{-2}"
                , m2lopl         = "p*l^phantom(1)*m[l*e*a*f]^{-2}"
                , m2pom2         = "m^{2}*m[l*e*a*f+w*o*o*d]^{-2}"
                , m2popl         = "p*l^phantom(1)*m[l*e*a*f+w*o*o*d]^{-2}"
                , m2wom2         = "m^{2}*m[w*o*o*d]^{-2}"
                , m2wopl         = "p*l^phantom(1)*m[w*o*o*d]^{-2}"
                , m3oha          = "h*a^phantom(1)*m^{-3}"
                , m3om2          = "m^{2}*m^{-3}"
                , m3wom3         = "m^{3}*m[W]^{-3}"
                , Mgcoha         = "h*a^phantom(1)*M*g*C^{-1}"
                , Mgoha          = "h*a^phantom(1)*M*g^{-1}"
                , Mgwom2         = "m^{2}*M*g*H[2]*O^{-1}"
                , Mjom2          = "m^{2}*M*J^{-1}"
                , mmoyr          = "y*r^phantom(1)*m*m^{-1}"
                , mpa            = "M*P*a^{-1}"
                , mpaodec        = "d*e*c^phantom(1)*M*P*a^{-1}"
                , mwom2ok        = "m^{2}*K^phantom(1)*m*W^{-1}"
                , nmo.090        = "m*o*n*t*h*s*{-1}*\"|\"*phantom(1)*bar(R) < 90*m*m^phantom(1)*m*o^{-1}"
                , nmo.100        = "m*o*n*t*h*s*{-1}*\"|\"*phantom(1)*bar(R) < 100*m*m^phantom(1)*m*o^{-1}"
                , nmo.110        = "m*o*n*t*h*s*{-1}*\"|\"*phantom(1)*bar(R) < 110*m*m^phantom(1)*m*o^{-1}"
                , nmo.120        = "m*o*n*t*h*s*{-1}*\"|\"*phantom(1)*bar(R) < 120*m*m^phantom(1)*m*o^{-1}"
                , nmo.wdef       = "m*o*n*t*h*s*{-1}*\"|\"*phantom(1)*bar(W*D) > 10*m*m^phantom(1)*m*o^{-1}"
                , nmolomol       = "m*o*l^phantom(1)*n*m*o*l^{-1}"
                , nomm           = "m*m^phantom(1)*N^{-1}"
                , nomm2          = "m*m^{2}*N^{-1}"
                , oneoday        = "d*a*y"
                , oneom          = "m"
                , oneompa        = "M*P*a"
                , oneoha         = "h*a"
                , oneok          = "K"
                , oneokm2        = "k*m^{2}"
                , oneokm2oday    = "k*m^{2}*d*a*y"
                , oneokm2omo     = "k*m^{2}*m*o*n*t*h"
                , oneom2         = "m^{2}"
                , oneoyr         = "y*r"
                , oneodec        = "d*e*c"
                , pa             = "P*a^{-1}"
                , pc             = "'%'^{-1}"
                , pcoday         = "d*a*y^phantom(1)*'%'^{-1}"
                , pcodec         = "d*e*c^phantom(1)*'%'^{-1}"
                , pcbio          = "'%'[b*i*o]^{-1}"
                , pcagboyr       = "y*r^phantom(1)*'%'[A*G*B]^{-1}"
                , pcagboyrodec   = "d*e*c^phantom(1)*y*r^phantom(1)*'%'[A*G*B]^{-1}"
                , pcagbo50yr     = "group(\"(\",50*y*r,\")\")^phantom(1)*'%'[A*G*B]^{-1}"
                , pcareaoyr      = "y*r^phantom(1)*'%'[a*r*e*a]^{-1}"
                , pcbaoyr        = "y*r^phantom(1)*'%'[B*A]^{-1}"
                , pcbiooyr       = "y*r^phantom(1)*'%'[b*i*o]^{-1}"
                , pcbiooyrodec   = "d*e*c^phantom(1)*y*r^phantom(1)*'%'[b*i*o]^{-1}"
                , pcdbhoyr       = "y*r^phantom(1)*'%'[D*B*H]^{-1}"
                , pcdbhoyrodec   = "d*e*c^phantom(1)*y*r^phantom(1)*'%'[D*B*H]^{-1}"
                , pcetoyr        = "y*r^phantom(1)*'%'[E*T]^{-1}"
                , pceto50yr      = "group(\"(\",50*y*r,\")\")^phantom(1)*'%'[E*T]^{-1}"
                , pcoyr          = "y*r^phantom(1)*'%'^{-1}"
                , pcoyrodec      = "y*r^phantom(1)*d*e*c^phantom(1)*'%'^{-1}"
                , pcpopoyr       = "y*r^phantom(1)*'%'[p*o*p]^{-1}"
                , pcsat          = "'%'[S*a*t]^{-1}"
                , pgc            = "P*g*C^{-1}"
                , pgcoyr         = "y*r^phantom(1)*P*g*C^{-1}"
                , plom2          = "m^{2}*p*l*a*n*t^{-1}"
                , plom2odec      = "d*e*c^phantom(1)*m^{2}*p*l*a*n*t^{-1}"
                , ploha          = "h*a^phantom(1)*p*l*a*n*t^{-1}"
                , plohaoyr       = "h*a^phantom(1)*y*r^phantom(1)*p*l*a*n*t^{-1}"
                , plohaodec      = "d*e*c^phantom(1)*h*a^phantom(1)*p*l*a*n*t^{-1}"
                , s              = "s^{-1}"
                , t1e3           = "phantom(1)%*%10^{-3}"
                , t1e6           = "phantom(1)%*%10^{-6}"
                , t1e9           = "phantom(1)%*%10^{-9}"
                , thkm2oyr       = "y*r^phantom(1)*10^{-3*phantom(1)}*k*m^{-2}"
                , umolcom2os     = "m^{2}*s^phantom(1)*mu*m*o*l*C^{-1}"
                , umologcos      = "g*C^phantom(1)*s^phantom(1)*mu*m*o*l^{-1}"
                , umologos       = "g^phantom(1)*s^phantom(1)*mu*m*o*l^{-1}"
                , umolokgcos     = "k*g*C^phantom(1)*s^phantom(1)*mu*m*o*l^{-1}"
                , umolokgos      = "k*g^phantom(1)*s^phantom(1)*mu*m*o*l^{-1}"
                , umolom2        = "m^{2}*mu*m*o*l^{-1}"
                , umolom2os      = "m^{2}*s^phantom(1)*mu*m*o*l^{-1}"
                , umolom2l       = "m[l*e*a*f]^{2}*mu*m*o*l^{-1}"
                , umolom2los     = "m[l*e*a*f]^{2}*s^phantom(1)*mu*m*o*l^{-1}"
                , umolcom2       = "m^{2}*mu*m*o*l*C^{-1}"
                , umolcomol      = "m*o*l^phantom(1)*mu*m*o*l*C^{-1}"
                , umolomol       = "m*o*l^phantom(1)*mu*m*o*l^{-1}"
                , umolomolodec   = "d*e*c^phantom(1)*m*o*l^phantom(1)*mu*m*o*l^{-1}"
                , usd            = "U*S*D^{-1}"
                , usdoyr         = "y*r^phantom(1)*U*S*D^{-1}"
                , utc            = "U*T*C^{-1}"
                , wom2           = "m^{2}*W^{-1}"
                , wom2l          = "m[l*e*a*f]^{2}*W^phantom(1)"
                , wom2oyr        = "m^{2}*y*r^phantom(1)*W^{-1}"
                , wom2odec       = "m^{2}*d*e*c^phantom(1)*W^{-1}"
                , wopl           = "p*l*a*n*t^phantom(1)*W^{-1}"
                , yr             = "y*r^{-1}"
                , zscore         = "Z[s*c*o*r*e]^{-1}"
                , zscoreodec     = "d*e*c^phantom(1)*Z[s*c*o*r*e]^{-1}"
                )#end list
#==========================================================================================#
#==========================================================================================#





#==========================================================================================#
#==========================================================================================#
#      Simplify units for some simpler cases in which the units cancel themselves out.     #
# This is not a comprehensive list and may need to be expanded.                            #
#------------------------------------------------------------------------------------------#
simplify_units <<- function(x){

   #---~---
   #   Make sure x is a scalar. In case it isn't, recursively call the function.
   #---~---
   if ( length(x) %eq% 1L){
      #---~---
      #   List potential pairs.
      #---~---
      del_hPa  = (  (  grepl(pattern="\\*h\\*P\\*a\\^phantom\\(1\\)"           ,x=x)
                    || grepl(pattern="^h\\*P\\*a\\^phantom\\(1\\)"             ,x=x) )
                 && (  grepl(pattern="\\*h\\*P\\*a\\^\\{-1\\}"                 ,x=x)
                    || grepl(pattern="^h\\*P\\*a\\^\\{-1\\}"                   ,x=x) ) )
      del_mm   = (  (  grepl(pattern="\\*m\\*m\\^phantom\\(1\\)"               ,x=x)
                    || grepl(pattern="^m\\*m\\^phantom\\(1\\)"                 ,x=x) )
                 && (  grepl(pattern="\\*m\\*m\\^\\{-1\\}"                     ,x=x)
                    || grepl(pattern="^m\\*m\\^\\{-1\\}"                       ,x=x) ) )
      del_m2   = (  (  grepl(pattern="\\*m\\^\\{2\\}"                          ,x=x)
                    || grepl(pattern="^m\\^\\{2\\}"                            ,x=x) )
                 && (  grepl(pattern="\\*m\\^\\{-2\\}"                         ,x=x)
                    || grepl(pattern="^m\\^\\{-2\\}"                           ,x=x) ) )
      del_m3   = (  (  grepl(pattern="\\*m\\^\\{3\\}"                          ,x=x)
                    || grepl(pattern="^m\\^\\{3\\}"                            ,x=x) )
                 && (  grepl(pattern="\\*m\\^\\{-3\\}"                         ,x=x)
                    || grepl(pattern="^m\\^\\{-3\\}"                           ,x=x) ) )
      del_day  = (  (  grepl(pattern="\\*d\\*a\\*y\\^phantom\\(1\\)"           ,x=x)
                    || grepl(pattern="^d\\*a\\*y\\^phantom\\(1\\)"             ,x=x) )
                 && (  grepl(pattern="\\*d\\*a\\*y\\^\\{-1\\}"                 ,x=x)
                    || grepl(pattern="^d*a*y\\^\\{-1\\}"                       ,x=x) ) )
      del_gC   = (  (  grepl(pattern="\\*g\\*C\\^phantom\\(1\\)"               ,x=x)
                    || grepl(pattern="^g\\*C\\^phantom\\(1\\)"                 ,x=x) )
                 && (  grepl(pattern="\\*g\\*C\\^\\{-1\\}"                     ,x=x)
                    || grepl(pattern="^g\\*C\\^\\{-1\\}"                       ,x=x) ) )
      del_kgC  = (  (  grepl(pattern="\\*k\\*g\\*C\\^phantom\\(1\\)"           ,x=x)
                    || grepl(pattern="^k\\*g\\*C\\^phantom\\(1\\)"             ,x=x) )
                 && (  grepl(pattern="\\*k\\*g\\*C\\^\\{-1\\}"                 ,x=x)
                    || grepl(pattern="^k\\*g\\*C\\^\\{-1\\}"                   ,x=x) ) )
      del_gW   = (  (  grepl(pattern="\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"    ,x=x)
                    || grepl(pattern="\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"    ,x=x) )
                 && (  grepl(pattern="\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"          ,x=x)
                    || grepl(pattern="\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"          ,x=x) ) )
      del_kgW  = (  (  grepl(pattern="\\*k\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)",x=x)
                    || grepl(pattern="^k\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"  ,x=x) )
                 && (  grepl(pattern="\\*k\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"      ,x=x)
                    || grepl(pattern="^k\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"        ,x=x) ) )
      #---~---


      #---~---
      #   List all patterns slated for deletion.
      #---~---
      del_list = character(0L)
      if (del_hPa){
         del_list =
            c( del_list, "\\*h\\*P\\*a\\^phantom\\(\\)", "^h\\*P\\*a\\^phantom\\(\\)"
                       , "\\*h\\*P\\*a\\^\\{-1\\}"     , "^h\\*P\\*a\\^\\{-1\\}"      )
      }#end if (del_hPa)
      if (del_mm){
         del_list =
            c( del_list, "\\*m\\*m\\^phantom\\(1\\)","^m\\*m\\^phantom\\(1\\)"
                       , "\\*m\\*m\\^\\{-1\\}"      ,"^m\\*m\\^\\{-1\\}"       )
      }#end if (del_mm)
      if (del_m2){
         del_list = c( del_list, "\\*m\\^\\{2\\}" , "^m\\^\\{2\\}"
                               , "\\*m\\^\\{-2\\}", "^m\\^\\{-2\\}" )
      }#end if (del_m2)
      if (del_m3){
         del_list = c( del_list, "\\*m\\^\\{3\\}" , "^m\\^\\{3\\}"
                               , "\\*m\\^\\{-3\\}", "^m\\^\\{-3\\}" )
      }#end if (del_m3)
      if (del_day){
         del_list =
            c( del_list, "\\*d\\*a\\*y\\^phantom\\(1\\)","^d\\*a\\*y\\^phantom\\(1\\)"
                       , "\\*d\\*a\\*y\\^\\{-1\\}"      ,"^d*a*y\\^\\{-1\\}"           )
      }#end if (del_day)
      if (del_gC){
         del_list =
            c( del_list, "\\*g\\*C\\^phantom\\(1\\)", "^g\\*C\\^phantom\\(1\\)"
                       , "\\*g\\*C\\^\\{-1\\}"      , "^g\\*C\\^\\{-1\\}"       )
      }#end if (del_gC)
      if (del_kgC){
         del_list =
            c( del_list, "\\*k\\*g\\*C\\^phantom\\(1\\)", "^k\\*g\\*C\\^phantom\\(1\\)"
                       , "\\*k\\*g\\*C\\^\\{-1\\}"      , "^k\\*g\\*C\\^\\{-1\\}"       )
      }#end if (del_kgC)
      if (del_gW){
         del_list =
            c( del_list, "\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"
                       , "\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"
                       , "\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"
                       , "\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"       )
      }#end if (del_gW)
      if (del_kgW){
         del_list =
            c( del_list, "\\*k\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"
                       , "^k\\*g\\*H\\[2\\]\\*O\\^phantom\\(1\\)"
                       , "\\*k\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"
                       , "^k\\*g\\*H\\[2\\]\\*O\\^\\{-1\\}"         )
      }#end if (del_kgW)
      #---~---


      #---~---
      #   Delete redundant units
      #---~---
      ans = x
      for (d in seq_along(del_list)) ans = gsub(pattern=del_list[d],replacement="",x=ans)
      #---~---


      #---~---
      #   Remove multiplication signs at the beginning or end of the units
      #---~---
      while(grepl(pattern="^\\*",x=ans)) ans = gsub(pattern="^\\*",replacement="",x=ans)
      while(grepl(pattern="\\*$",x=ans)) ans = gsub(pattern="\\*$",replacement="",x=ans)
      #---~---
   }else{
      #---~---
      #   Recursively call the function if the input is a vector, list, or data frame.
      #---~---
      if (tibble::is_tibble(x)){
         ans        = lapply(X=x,FUN=simplify_units)
         ans        = tibble::as_tibble(x)
         names(ans) = names(x)
      }else if (data.table::is.data.table(x)){
         ans        = lapply(X=x,FUN=simplify_units)
         ans        = data.table::as.data.table(x)
         names(ans) = names(x)
      }else if (is.data.frame(x)){
         ans        = lapply(X=x,FUN=simplify_units)
         ans        = as.data.frame(x)
         names(ans) = names(x)
      }else if (is.array(x)){
         ans           = apply(X=x,MARGIN=seq_along(dim(x)),FUN=simplify_units)
         dimnames(ans) = dimnames(x)
      }else if (is.vector(x)){
         ans        = sapply(X=x,FUN=simplify_units,simplify=TRUE)
         names(ans) = names(x)
      }else if (is.list(x)){
         ans        = lapply(X=x,FUN=simplify_units)
         names(ans) = names(x)
      }else{
         stop("Unexpected data type (",typeof(x),") for \"x\".")
      }#end if (tibble::is_tibble(x))
      #---~---
   }#end if (length(x) %eq% 1L)
   #---~---


   #---~---
   #   Return results.
   #---~---
   return(ans)
   #---~---

}#end function simplify_units
#==========================================================================================#
#==========================================================================================#
