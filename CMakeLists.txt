cmake_minimum_required(VERSION 3.20)
project(OpenGLCourseWork)

# Locate the packages installed via vcpkg
find_package(glfw3 CONFIG REQUIRED)
find_package(glad CONFIG REQUIRED)
find_package(glm CONFIG REQUIRED)

add_executable(app main.cpp )

# Link the libraries
target_link_libraries(app PRIVATE glfw glad::glad glm::glm)