#include "Application.h"

#include "Level.h"

#include <GLFW/glfw3.h>
#include <glad/glad.h>

#include <algorithm>
#include <array>
#include <cmath>
#include <iostream>
#include <sstream>
#include <string>

namespace {
void framebufferCallback(GLFWwindow *window, int width, int height) {
    glViewport(0, 0, width, height);
    auto *app = static_cast<Application *>(glfwGetWindowUserPointer(window));
    if (app) {
        app->updateWindowTitle(0.0f);
    }
}

void cursorCallback(GLFWwindow *window, double xpos, double ypos) {
    auto *app = static_cast<Application *>(glfwGetWindowUserPointer(window));
    if (app) {
        app->handleMouseMove(xpos, ypos);
    }
}
}

bool Application::initialise(const std::string &assetRoot, std::string &log) {
    assetRootDirectory = assetRoot;
    if (!glfwInit()) {
        log = "Failed to initialise GLFW";
        return false;
    }

    glfwWindowHint(GLFW_CONTEXT_VERSION_MAJOR, 3);
    glfwWindowHint(GLFW_CONTEXT_VERSION_MINOR, 3);
    glfwWindowHint(GLFW_OPENGL_PROFILE, GLFW_OPENGL_CORE_PROFILE);

    window = glfwCreateWindow(config.width, config.height, "First Person Engine", nullptr, nullptr);
    if (!window) {
        log = "Failed to create GLFW window";
        glfwTerminate();
        return false;
    }

    glfwMakeContextCurrent(window);
    if (config.vsync) {
        glfwSwapInterval(1);
    } else {
        glfwSwapInterval(0);
    }

    if (!gladLoadGLLoader(reinterpret_cast<GLADloadproc>(glfwGetProcAddress))) {
        log = "Failed to initialise GLAD";
        return false;
    }

    glEnable(GL_DEPTH_TEST);

    glfwSetWindowUserPointer(window, this);
    glfwSetFramebufferSizeCallback(window, framebufferCallback);
    glfwSetInputMode(window, GLFW_CURSOR, GLFW_CURSOR_DISABLED);
    glfwSetCursorPosCallback(window, cursorCallback);

    std::string levelPath = resolveAssetPath(assetRootDirectory, "assets/levels/default.json");
    if (!level.loadFromFile(levelPath, log)) {
        return false;
    }

    std::string controlsPath = resolveAssetPath(assetRootDirectory, "config/controls.json");
    std::string bindingLog;
    input.loadBindings(controlsPath, bindingLog);
    if (!bindingLog.empty()) {
        std::cout << bindingLog << std::endl;
    }

    if (!renderer.initialise(assetRootDirectory, log)) {
        return false;
    }
    renderer.buildLevelGeometry(level);

    player.position = level.spawnPosition();
    camera.setPosition(player.position);
    camera.setYawPitch(3.1415926f * 0.5f, 0.0f);

    vrInfo = queryVRRuntime();

    return true;
}

void Application::run() {
    float lastTime = static_cast<float>(glfwGetTime());
    while (!glfwWindowShouldClose(window)) {
        float current = static_cast<float>(glfwGetTime());
        float delta = current - lastTime;
        lastTime = current;

        processInput(delta);
        update(delta);

        Mat4 view = camera.viewMatrix();
        int width = 0;
        int height = 0;
        glfwGetFramebufferSize(window, &width, &height);
        float aspect = height > 0 ? static_cast<float>(width) / static_cast<float>(height) : 1.0f;
        Mat4 projection = camera.projectionMatrix(aspect);
        renderer.renderScene(level, view, projection, camera.getPosition(), current);

        glfwSwapBuffers(window);
        glfwPollEvents();
        updateWindowTitle(delta);
    }
}

void Application::shutdown() {
    renderer.shutdown();
    if (window) {
        glfwDestroyWindow(window);
        window = nullptr;
    }
    glfwTerminate();
}

void Application::update(float deltaSeconds) {
    player.velocity.y += config.gravity * deltaSeconds;
    Vec3 previous = player.position;
    player.position += player.velocity * deltaSeconds;
    resolveCollisions(previous, player.position, player.velocity);
    camera.setPosition(player.position);
}

void Application::processInput(float deltaSeconds) {
    (void)deltaSeconds;
    const auto &bindings = input.bindings();

    if (glfwGetKey(window, GLFW_KEY_ESCAPE) == GLFW_PRESS) {
        glfwSetWindowShouldClose(window, GLFW_TRUE);
    }

    Vec3 move{0.0f, 0.0f, 0.0f};
    Vec3 forward = camera.forward();
    forward.y = 0.0f;
    forward = normalize(forward);
    Vec3 right = camera.right();
    right.y = 0.0f;
    right = normalize(right);

    if (glfwGetKey(window, bindings.moveForward) == GLFW_PRESS) {
        move += forward;
    }
    if (glfwGetKey(window, bindings.moveBackward) == GLFW_PRESS) {
        move -= forward;
    }
    if (glfwGetKey(window, bindings.moveLeft) == GLFW_PRESS) {
        move -= right;
    }
    if (glfwGetKey(window, bindings.moveRight) == GLFW_PRESS) {
        move += right;
    }

    float speed = config.movementSpeed;
    if (glfwGetKey(window, bindings.sprint) == GLFW_PRESS) {
        speed *= config.sprintMultiplier;
    }

    if (dot(move, move) > 0.0001f) {
        move = normalize(move) * speed;
        player.velocity.x = move.x;
        player.velocity.z = move.z;
    } else {
        player.velocity.x = 0.0f;
        player.velocity.z = 0.0f;
    }

    if (player.grounded && glfwGetKey(window, bindings.jump) == GLFW_PRESS) {
        player.velocity.y = config.jumpStrength;
        player.grounded = false;
    }
}

void Application::resolveCollisions(const Vec3 &previousPosition, Vec3 &position, Vec3 &velocity) {
    const float groundHeight = 1.0f;
    const float radius = 0.35f;

    // Floor collision
    if (position.y <= groundHeight) {
        position.y = groundHeight;
        velocity.y = 0.0f;
        player.grounded = true;
    } else {
        player.grounded = false;
    }

    const float cellSize = level.cellSize();
    auto collides = [&](const Vec3 &pos) {
        std::array<Vec2, 4> offsets = {Vec2{radius, radius}, Vec2{-radius, radius}, Vec2{radius, -radius}, Vec2{-radius, -radius}};
        for (const auto &offset : offsets) {
            float sampleX = pos.x + offset.x;
            float sampleZ = pos.z + offset.y;
            int cellX = static_cast<int>(std::floor(sampleX / cellSize));
            int cellZ = static_cast<int>(std::floor(sampleZ / cellSize));
            if (level.isWall(cellX, cellZ)) {
                return true;
            }
        }
        return false;
    };

    Vec3 candidate = position;
    candidate.z = previousPosition.z;
    if (collides(candidate)) {
        position.x = previousPosition.x;
        velocity.x = 0.0f;
    }

    candidate = position;
    candidate.x = previousPosition.x;
    if (collides(candidate)) {
        position.z = previousPosition.z;
        velocity.z = 0.0f;
    }

    if (position.y > level.maxHeight() - 0.2f) {
        position.y = level.maxHeight() - 0.2f;
        velocity.y = std::min(0.0f, velocity.y);
    }
}

void Application::updateWindowTitle(float frameTime) {
    if (!window) {
        return;
    }
    std::ostringstream ss;
    float fps = frameTime > 0.0001f ? 1.0f / frameTime : 0.0f;
    ss << "First Person Engine | " << static_cast<int>(fps) << " FPS";
    if (vrInfo.enabled) {
        ss << " | VR: " << vrInfo.runtime;
    }
    glfwSetWindowTitle(window, ss.str().c_str());
}

void Application::handleMouseMove(double xpos, double ypos) {
    if (firstMouse) {
        lastCursorX = xpos;
        lastCursorY = ypos;
        firstMouse = false;
        return;
    }

    double offsetX = xpos - lastCursorX;
    double offsetY = lastCursorY - ypos;
    lastCursorX = xpos;
    lastCursorY = ypos;

    camera.rotate(static_cast<float>(offsetX) * config.mouseSensitivity, static_cast<float>(offsetY) * config.mouseSensitivity);
}

