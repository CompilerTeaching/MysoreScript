cmake_minimum_required(VERSION 2.8)

project(mysorescript)

set(mysorescript_CXX_SRCS
	# Just compile the Pegmatite .cc files into the program, don't bother
	# building a separate library.
	Pegmatite/ast.cc
	Pegmatite/parser.cc
	compiler.cc
	interpreter.cc
	main.cc
	runtime.cc
)
set(LLVM_LIBS
	all
)

# Define the mysorescript program that we will build
add_executable(mysorescript ${mysorescript_CXX_SRCS})
# We're using pegmatite in the RTTI mode
add_definitions(-DUSE_RTTI=1)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")

if(CMAKE_CXX_COMPILER_ID MATCHES "Clang")
	set(CLANG_FLAGS "-Wdeprecated -Wno-zero-length-array -Wno-unknown-attributes")
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${CLANG_FLAGS}")
else()
	set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall")
endif()



# Find the Boehm GC stuff
include(FindPkgConfig)
pkg_check_modules(GC REQUIRED bdw-gc)
link_directories(${GC_LIBRARY_DIRS})
find_library(LIBGC gc-threaded PATHS "${GC_LIBRARY_DIRS}")
find_library(LIBGC gc-threaded PATHS "${GC_LIBRARY_DIRS}")
if (${LIBGC} STREQUAL "LIBGC-NOTFOUND")
	find_library(LIBGC gc PATHS GC_LIBRARY_DIRS)
endif ()
if (${LIBGC} STREQUAL "LIBGC-NOTFOUND")
	message(ERROR "Unable to find Boehm GC library")
endif ()

message(STATUS "Using Boehm GC library: ${LIBGC}")
set(CMAKE_C_FLAGS "${CMAKE_C_FLAGS} ${GC_CFLAGS}")
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${GC_CFLAGS}")
include_directories(GC_INCLUDE_DIRS)


find_program(LLVM_CONFIG NAMES llvm-config DOC "Path to llvm-config utility")
if (${LLVM_CONFIG} STREQUAL "LLVM_CONFIG-NOTFOUND")
	message(SEND_ERROR "llvm-config not found, please manually set path with -DLLVM_CONFIG")
endif()
message(STATUS "Using llvm-config: ${LLVM_CONFIG}")

# Define LLVM version macros so that we can support multiple versions in the source.
exec_program(${LLVM_CONFIG}
	ARGS --version
	OUTPUT_VARIABLE LLVM_VER)
exec_program(${LLVM_CONFIG}
	ARGS --cxxflags
	OUTPUT_VARIABLE LLVM_CXXFLAGS)
exec_program(${LLVM_CONFIG}
	ARGS --libs ${LLVM_LIBS}
	OUTPUT_VARIABLE LLVM_LIBS_FLAGS)
exec_program(${LLVM_CONFIG}
	ARGS --ldflags
	OUTPUT_VARIABLE LLVM_LDFLAGS)
exec_program(${LLVM_CONFIG}
	ARGS --system-libs
	OUTPUT_VARIABLE LLVM_SYSTEMLIBS)
exec_program(${LLVM_CONFIG}
	ARGS --libdir
	OUTPUT_VARIABLE LLVM_LIBDIR)
string(REGEX REPLACE "([0-9]*).([0-9]*).*" "-DLLVM_MAJOR=\\1 -DLLVM_MINOR=\\2"  LLVM_VERSION "${LLVM_VER}")


set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} ${LLVM_CXXFLAGS} ${LLVM_VERSION}")
target_link_libraries(mysorescript ${LLVM_LIBS_FLAGS})
# llvm-config only gained a --system-libs flag in 3.5
if (LLVM_VER VERSION_GREATER 3.4)
	target_link_libraries(mysorescript ${LLVM_SYSTEMLIBS})
endif()
set(CMAKE_EXE_LINKER_FLAGS "${LLVM_LDFLAGS} ${LIBGC} ${CMAKE_EXE_LINKER_FLAGS} -Wl,-rpath,${LLVM_LIBDIR}")

# Make sure that we use the LLVM path as an rpath so that we can dynamically
# link to LLVM.  Don't let CMake specify its own rpath
set(CMAKE_SKIP_RPATH true)

# Make sure that LLVM is able to find functions in the main executable
SET_TARGET_PROPERTIES(mysorescript PROPERTIES
       ENABLE_EXPORTS TRUE)


option(BUILD_DOCUMENTATION "Use Doxygen to create the HTML based API documentation" OFF)
if(BUILD_DOCUMENTATION)
	FIND_PACKAGE(Doxygen)
	if (NOT DOXYGEN_FOUND)
		message(FATAL_ERROR
			"Doxygen is needed to build the documentation. Please install it correctly")
	endif()
	#-- Configure the Template Doxyfile for our specific project
	configure_file(Doxyfile.in
	               ${PROJECT_BINARY_DIR}/Doxyfile @ONLY IMMEDIATE)
	#-- Add a custom target to run Doxygen when ever the project is built
	add_custom_target (Docs ALL
	                   COMMAND ${DOXYGEN_EXECUTABLE} ${PROJECT_BINARY_DIR}/Doxyfile
	                   SOURCES ${PROJECT_BINARY_DIR}/Doxyfile)
endif()

option(ENABLE_TESTS "Enable the tests")

if(ENABLE_TESTS)
	enable_testing()
	add_subdirectory("Tests")
endif()
