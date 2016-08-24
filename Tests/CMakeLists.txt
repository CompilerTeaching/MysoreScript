message(STATUS "Adding tests")
file(GLOB TESTS "*.ms")

foreach(TEST ${TESTS})
	get_filename_component(TEST_NAME ${TEST} NAME_WE)
	message(STATUS "Adding test ${TEST_NAME}")
	add_test(${TEST_NAME} "${CMAKE_CURRENT_SOURCE_DIR}/runtest.sh" "${CMAKE_BINARY_DIR}/mysorescript" ${TEST} "${LLVM_BINDIR}/FileCheck")
	add_test("${TEST_NAME}_jit" "${CMAKE_CURRENT_SOURCE_DIR}/runtest.sh" "${CMAKE_BINARY_DIR}/mysorescript" ${TEST} "${LLVM_BINDIR}/FileCheck" "-c")
endforeach()

