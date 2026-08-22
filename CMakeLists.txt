cmake_minimum_required(VERSION 3.23)


# please edit the compiler path for your own

set(CMAKE_Fortran_COMPILER /opt/homebrew/bin/gfortran)

# Set the minimum deployment version of macOS to the current system version
set(CMAKE_OSX_DEPLOYMENT_TARGET "26.0" CACHE STRING "Minimum OS X deployment version")

project(iHydroSlide3D_v1.1 Fortran)
enable_language(Fortran)


# SET(CMAKE_BUILD_TYPE "Debug")
SET(CMAKE_BUILD_TYPE "Release")


# detect the operating system
IF (CMAKE_SYSTEM_NAME MATCHES "Linux")
    MESSAGE(STATUS "current platform: Linux ")

ELSEIF (CMAKE_SYSTEM_NAME MATCHES "Windows")
    MESSAGE(STATUS "current platform: Windows")


ELSEIF (CMAKE_SYSTEM_NAME MATCHES "Darwin")
    MESSAGE(STATUS "current platform: Mac OS X")

ELSE ()
    MESSAGE(STATUS "other platform: ${CMAKE_SYSTEM_NAME}")
ENDIF (CMAKE_SYSTEM_NAME MATCHES "Linux")

message("------------ fortran compiler --------------")


set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR})


# save the library
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

find_package(OpenMP REQUIRED)

if (OPENMP_FOUND)
    
    
    set(CMAKE_Fortran_FLAGS "${CMAKE_Fortran_FLAGS} ${OpenMP_Fortran_FLAGS}")

    
endif()


# please provide specific path of HDF5 installation
# SET(CMAKE_PREFIX_PATH ${CMAKE_PREFIX_PATH} "/opt/homebrew/Cellar/hdf5/1.14.6/")

find_package(HDF5 MODULE REQUIRED COMPONENTS Fortran HL)
SET(HDF5_USE_STATIC_LIBRARIES ON)


find_package(LibAEC REQUIRED) 
find_package(ZLIB REQUIRED)



message("fortran compiler: ${CMAKE_Fortran_COMPILER_ID}")
message("fortran compiler version: ${CMAKE_Fortran_COMPILER_VERSION}")
message("fortran compiler path: ${CMAKE_Fortran_COMPILER}")

# 输出 OpenMP 库路径
message("------------ openMP info --------------")
message("openMP found: ${OPENMP_FOUND}")
message("OpenMP Fortran flags: ${OpenMP_Fortran_FLAGS}")
message("OpenMP library: ${OpenMP_Fortran_LIBRARIES}")


# write the HDF5 finding result
message("------------ HDF5 info ----------------")
message("HDF5 found: ${HDF5_FOUND}")
message("HDF5 version: ${HDF5_VERSION}")
message("HDF5 include dirs: ${HDF5_INCLUDE_DIRS}")
message("HDF5 libraries: ${HDF5_LIBRARIES}")
message("HDF5_HL_LIBRARIES: ${HDF5_HL_LIBRARIES}")
message("--------------------------")


include_directories("${HDF5_INCLUDE_DIRS}")

# Recursively collect all Fortran source files (.f90)
# CONFIGURE_DEPENDS ensures CMake re-runs configuration when files are added/removed
# Restricting to *.f90 avoids accidentally including generated files like .mod
file(GLOB_RECURSE SRCS CONFIGURE_DEPENDS
    ${PROJECT_SOURCE_DIR}/src/*.f90
)
# Define the executable target built from the collected sources
add_executable(iHydroSlide3D ${SRCS})

# Set a unified output directory for Fortran module files (.mod)
# Placing them under build/mod prevents pollution of the source tree
# and avoids conflicts from stale module files
set_target_properties(iHydroSlide3D PROPERTIES
    Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/mod
)

# Add the module output directory to the include search path
# Required so the compiler can locate .mod files when resolving "use" statements
target_include_directories(iHydroSlide3D PRIVATE
    ${CMAKE_BINARY_DIR}/mod
)


# Link required libraries
# - HDF5_LIBRARIES: HDF5 dependency (typically from find_package)
# - ZLIB::ZLIB: compression library (often required by HDF5)
# - OpenMP::OpenMP_Fortran: enables OpenMP support for Fortran
# PRIVATE means these dependencies apply only to this target
target_link_libraries(iHydroSlide3D
    PRIVATE
        ${HDF5_LIBRARIES}
        ZLIB::ZLIB
        OpenMP::OpenMP_Fortran
)