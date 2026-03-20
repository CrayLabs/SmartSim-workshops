project(heat_steady)

cmake_minimum_required(VERSION 3.13)
enable_language(Fortran)
SET(CMAKE_FC_COMPILER gfortran)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_C_STANDARD 99)

if(NOT DEFINED SMARTREDIS_INSTALL_PATH)
  set(SMARTREDIS_INSTALL_PATH "/lus/scratch/arigazzi/smartsim-dev/SmartRedis/install/")
endif()

# Specify all pre-processor and library dependencies
find_library(SMARTREDIS_LIBRARY smartredis PATHS ${SMARTREDIS_INSTALL_PATH}/lib NO_DEFAULT_PATH REQUIRED)
find_library(SMARTREDIS_FORTRAN_LIBRARY smartredis-fortran PATHS ${SMARTREDIS_INSTALL_PATH}/lib NO_DEFAULT_PATH REQUIRED)
find_library(HIREDIS hiredis PATHS ${SMARTREDIS_INSTALL_PATH}/lib NO_DEFAULT_PATH REQUIRED)
find_library(REDISPP redis++ PATHS ${SMARTREDIS_INSTALL_PATH}/lib NO_DEFAULT_PATH REQUIRED)
set(SMARTREDIS_LIBRARIES
	${SMARTREDIS_LIBRARY}
	${SMARTREDIS_FORTRAN_LIBRARY}
	${HIREDIS}
	${REDISPP}
)

include_directories(SYSTEM
    /usr/local/include
    ${SMARTREDIS_INSTALL_PATH}/include
)

# Build executables
add_executable(heat_steady
    f90getopt.F90
    fd2d_heat_steady_test.F90
    fd2d_heat_steady.F90
)
target_link_libraries(heat_steady
    ${SMARTREDIS_LIBRARIES}
)