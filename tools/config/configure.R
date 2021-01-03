# Prepare your package for installation here.
# Use 'define()' to define configuration variables.
# Use 'configure_file()' to substitute configuration values.
print("CONFIGURE!")

CPLEX_INCLUDE_PATH <- "/home/bobby/opt/ibm/ILOG/CPLEX_Studio_Community201/cplex/include"
CONCERT_INCLUDE_PATH <- "/home/bobby/opt/ibm/ILOG/CPLEX_Studio_Community201/concert/include"
CPLEX_LINK_PATH <- "/home/bobby/opt/ibm/ILOG/CPLEX_Studio_Community201/cplex/lib/x86-64_linux/static_pic"
CONCERT_LINK_PATH <- "/home/bobby/opt/ibm/ILOG/CPLEX_Studio_Community201/concert/lib/x86-64_linux/static_pic"

define(
  CXX_STD = "CXX11",
  #"PKG_CFLAGS" = "-m64 -fPIC -fno-strict-aliasing -fexceptions -DNDEBUG",
  "PKG_CXXFLAGS" = sprintf("$(SHLIB_OPENMP_CXXFLAGS) -I%s -I%s", CPLEX_INCLUDE_PATH, CONCERT_INCLUDE_PATH),
  "PKG_LIBS" = sprintf("$(SHLIB_OPENMP_CXXFLAGS) $(LAPACK_LIBS) $(BLAS_LIBS) $(FLIBS) -L%s -L %s -lconcert -lilocplex -lcplex -lm -lpthread", CPLEX_LINK_PATH, CONCERT_LINK_PATH)
  )


configure_file("src/Makevars.in")
#configure_file("src/Makevars.win.in")
