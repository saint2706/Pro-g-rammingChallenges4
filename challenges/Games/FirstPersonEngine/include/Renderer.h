#pragma once

#include "Math.h"
#include "Shader.h"
#include "Texture.h"

#include <string>
#include <vector>

class Level;

class Renderer {
  public:
    bool initialise(const std::string &assetRoot, std::string &log);
    void shutdown();

    void buildLevelGeometry(const Level &level);
    void renderScene(const Level &level, const Mat4 &view, const Mat4 &projection, const Vec3 &cameraPosition, float timeSeconds);

  private:
    struct Vertex {
        Vec3 position;
        Vec3 normal;
        Vec2 uv;
    };

    unsigned int vao{0};
    unsigned int vbo{0};
    unsigned int ebo{0};
    size_t indexCount{0};

    Shader shader;
    Texture wallTexture;
    Texture floorTexture;
    Texture ceilingTexture;

    std::string shaderDirectory;
};

