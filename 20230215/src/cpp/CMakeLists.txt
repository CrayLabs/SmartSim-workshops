project(heat_steady)

cmake_minimum_required(VERSION 3.13)
set(CMAKE_CXX_STANDARD 17)

if(NOT DEFINED SMARTREDIS_INSTALL_PATH)
  set(SMARTREDIS_INSTALL_PATH "/lus/scratch/arigazzi/smartsim-dev/SmartRedis/install/")
endif()

find_library(SMARTREDIS_LIBRARY smartredis
             PATHS ${SMARTREDIS_INSTALL_PATH}/lib
             NO_DEFAULT_PATH REQUIRED
)

include_directories(SYSTEM
    /usr/local/include
    ${SMARTREDIS_INSTALL_PATH}/include
)

# Build executables

add_executable(heat_steady
    fd2d_heat_steady_test.cpp
    fd2d_heat_steady.cpp
)
target_link_libraries(heat_steady
    ${SMARTREDIS_LIBRARY}
)