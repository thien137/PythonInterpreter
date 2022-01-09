cmake_minimum_required(VERSION 3.16.3)

project("PYTHONINTERPRETER")

add_executable(${PROJECT_NAME} "main.cpp")

install(TARGETS "${PROJECT_NAME}" DESTINATION bin)
install(FILES "main.cpp" DESTINATION src)

add_subdirectory(src)

target_include_directories(${PROJECT_NAME} PUBLIC src)

target_link_directories(${PROJECT_NAME} PUBLIC src)

target_link_libraries(${PROJECT_NAME} src)