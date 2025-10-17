cmake_minimum_required(VERSION 3.16)
project(FirstPersonEngine LANGUAGES CXX)

option(ENABLE_OPENXR "Enable experimental OpenXR VR hooks" OFF)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)

add_executable(first_person_engine
    src/main.cpp
    src/Application.cpp
    src/Camera.cpp
    src/Input.cpp
    src/Level.cpp
    src/Renderer.cpp
    src/Shader.cpp
    src/Texture.cpp
    src/VRHooks.cpp
    src/Json.cpp
    src/glad.c
)

target_include_directories(first_person_engine PRIVATE
    ${CMAKE_CURRENT_SOURCE_DIR}/include
    ${CMAKE_CURRENT_SOURCE_DIR}/include/third_party
)

find_package(glfw3 CONFIG QUIET)
if (glfw3_FOUND)
    target_link_libraries(first_person_engine PRIVATE glfw)
else()
    find_package(PkgConfig QUIET)
    if (PkgConfig_FOUND)
        pkg_check_modules(GLFW QUIET glfw3)
        if (GLFW_FOUND)
            target_include_directories(first_person_engine PRIVATE ${GLFW_INCLUDE_DIRS})
            target_link_libraries(first_person_engine PRIVATE ${GLFW_LIBRARIES})
            target_compile_options(first_person_engine PRIVATE ${GLFW_CFLAGS_OTHER})
        else()
            message(FATAL_ERROR "glfw3 not found. Install GLFW or pass -Dglfw3_DIR to CMake.")
        endif()
    else()
        message(FATAL_ERROR "PkgConfig not available and glfw3 CONFIG package missing. Install GLFW or vcpkg/Conan package.")
    endif()
endif()

if (WIN32)
    target_link_libraries(first_person_engine PRIVATE opengl32)
elseif (APPLE)
    find_library(COCOA_LIBRARY Cocoa REQUIRED)
    find_library(IOKIT_LIBRARY IOKit REQUIRED)
    find_library(COREVIDEO_LIBRARY CoreVideo REQUIRED)
    target_link_libraries(first_person_engine PRIVATE ${COCOA_LIBRARY} ${IOKIT_LIBRARY} ${COREVIDEO_LIBRARY})
else()
    find_package(OpenGL REQUIRED)
    target_link_libraries(first_person_engine PRIVATE OpenGL::GL dl pthread)
endif()

if (ENABLE_OPENXR)
    target_compile_definitions(first_person_engine PRIVATE ENABLE_OPENXR)
    find_package(OpenXR QUIET)
    if (OpenXR_FOUND)
        target_link_libraries(first_person_engine PRIVATE OpenXR::openxr_loader)
    else()
        message(WARNING "ENABLE_OPENXR set but OpenXR loader not found; VR hooks will compile as stubs.")
    endif()
endif()

file(COPY assets DESTINATION ${CMAKE_CURRENT_BINARY_DIR})
file(COPY config DESTINATION ${CMAKE_CURRENT_BINARY_DIR})

