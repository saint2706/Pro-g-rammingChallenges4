#pragma once

#include "Camera.h"
#include "Input.h"
#include "Level.h"
#include "Renderer.h"
#include "VRHooks.h"

#include <string>

struct GLFWwindow;

struct ApplicationConfig {
    int width{1280};
    int height{720};
    bool vsync{true};
    float mouseSensitivity{0.0025f};
    float movementSpeed{5.0f};
    float sprintMultiplier{1.8f};
    float jumpStrength{5.0f};
    float gravity{-12.0f};
};

class Application {
  public:
    bool initialise(const std::string &assetRoot, std::string &log);
    void run();
    void shutdown();

  private:
    void update(float deltaSeconds);
    void processInput(float deltaSeconds);
    void resolveCollisions(const Vec3 &previousPosition, Vec3 &position, Vec3 &velocity);

    void updateWindowTitle(float frameTime);
    void handleMouseMove(double xpos, double ypos);

    struct Player {
        Vec3 position{0.0f, 1.0f, 0.0f};
        Vec3 velocity{0.0f, 0.0f, 0.0f};
        bool grounded{false};
    } player;

    ApplicationConfig config;
    InputManager input;
    Level level;
    Renderer renderer;
    Camera camera;
    VRHookInfo vrInfo;

    GLFWwindow *window{nullptr};
    std::string assetRootDirectory;
    double lastCursorX{0.0};
    double lastCursorY{0.0};
    bool firstMouse{true};
};

