#include "Renderer.h"

#include "Level.h"

#include <glad/glad.h>

#include <algorithm>
#include <cstddef>
#include <cmath>
#include <vector>

namespace {
struct GeometryBuilder {
    std::vector<Renderer::Vertex> vertices;
    std::vector<unsigned int> indices;

    void appendQuad(const Vec3 &a, const Vec3 &b, const Vec3 &c, const Vec3 &d, const Vec3 &normal, float uScale = 1.0f, float vScale = 1.0f) {
        unsigned int startIndex = static_cast<unsigned int>(vertices.size());
        vertices.push_back({a, normal, Vec2{0.0f, 0.0f}});
        vertices.push_back({b, normal, Vec2{uScale, 0.0f}});
        vertices.push_back({c, normal, Vec2{uScale, vScale}});
        vertices.push_back({d, normal, Vec2{0.0f, vScale}});

        indices.push_back(startIndex + 0);
        indices.push_back(startIndex + 1);
        indices.push_back(startIndex + 2);
        indices.push_back(startIndex + 2);
        indices.push_back(startIndex + 3);
        indices.push_back(startIndex + 0);
    }
};

Vec3 cellPosition(int x, int z, float cellSize) {
    return Vec3{x * cellSize, 0.0f, z * cellSize};
}

unsigned int adjustColor(unsigned int rgba, float delta) {
    auto clampByte = [](int value) { return std::clamp(value, 0, 255); };
    int r = static_cast<int>((rgba >> 24) & 0xFF);
    int g = static_cast<int>((rgba >> 16) & 0xFF);
    int b = static_cast<int>((rgba >> 8) & 0xFF);
    int a = static_cast<int>(rgba & 0xFF);
    int target = delta >= 0.0f ? 255 : 0;
    float amount = std::clamp(std::abs(delta), 0.0f, 1.0f);
    r = clampByte(static_cast<int>(r + (target - r) * amount));
    g = clampByte(static_cast<int>(g + (target - g) * amount));
    b = clampByte(static_cast<int>(b + (target - b) * amount));
    return static_cast<unsigned int>((r << 24) | (g << 16) | (b << 8) | a);
}
}

bool Renderer::initialise(const std::string &assetRoot, std::string &log) {
    shaderDirectory = assetRoot;
    std::string vertexShader = resolveAssetPath(shaderDirectory, "assets/shaders/standard.vert");
    std::string fragmentShader = resolveAssetPath(shaderDirectory, "assets/shaders/standard.frag");
    if (!shader.loadFromFiles(vertexShader, fragmentShader, log)) {
        return false;
    }

    glGenVertexArrays(1, &vao);
    glGenBuffers(1, &vbo);
    glGenBuffers(1, &ebo);

    glBindVertexArray(vao);
    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);

    glEnableVertexAttribArray(0);
    glVertexAttribPointer(0, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), reinterpret_cast<void *>(offsetof(Vertex, position)));
    glEnableVertexAttribArray(1);
    glVertexAttribPointer(1, 3, GL_FLOAT, GL_FALSE, sizeof(Vertex), reinterpret_cast<void *>(offsetof(Vertex, normal)));
    glEnableVertexAttribArray(2);
    glVertexAttribPointer(2, 2, GL_FLOAT, GL_FALSE, sizeof(Vertex), reinterpret_cast<void *>(offsetof(Vertex, uv)));

    glBindVertexArray(0);

    return true;
}

void Renderer::shutdown() {
    if (ebo != 0) {
        glDeleteBuffers(1, &ebo);
        ebo = 0;
    }
    if (vbo != 0) {
        glDeleteBuffers(1, &vbo);
        vbo = 0;
    }
    if (vao != 0) {
        glDeleteVertexArrays(1, &vao);
        vao = 0;
    }
}

void Renderer::buildLevelGeometry(const Level &level) {
    GeometryBuilder builder;
    const auto &layout = level.layout();
    const float cellSize = level.cellSize();
    const float height = level.maxHeight();

    LevelTextures textures = level.textures();
    wallTexture.createCheckerboard(32, textures.wallColor, adjustColor(textures.wallColor, -0.25f));
    floorTexture.createCheckerboard(32, textures.floorColor, adjustColor(textures.floorColor, 0.2f));
    ceilingTexture.createCheckerboard(32, textures.ceilingColor, adjustColor(textures.ceilingColor, -0.15f));

    size_t rows = layout.size();
    size_t cols = layout.empty() ? 0 : layout.front().size();

    // Floor
    Vec3 origin = cellPosition(0, 0, cellSize);
    Vec3 max = cellPosition(static_cast<int>(cols), static_cast<int>(rows), cellSize);
    builder.appendQuad(Vec3{origin.x, 0.0f, origin.z}, Vec3{max.x, 0.0f, origin.z}, Vec3{max.x, 0.0f, max.z}, Vec3{origin.x, 0.0f, max.z}, Vec3{0.0f, 1.0f, 0.0f}, static_cast<float>(cols), static_cast<float>(rows));

    // Ceiling
    builder.appendQuad(Vec3{origin.x, height, max.z}, Vec3{max.x, height, max.z}, Vec3{max.x, height, origin.z}, Vec3{origin.x, height, origin.z}, Vec3{0.0f, -1.0f, 0.0f}, static_cast<float>(cols), static_cast<float>(rows));

    for (size_t z = 0; z < rows; ++z) {
        for (size_t x = 0; x < cols; ++x) {
            if (!level.isWall(static_cast<int>(x), static_cast<int>(z))) {
                continue;
            }
            Vec3 base = cellPosition(static_cast<int>(x), static_cast<int>(z), cellSize);
            Vec3 minCorner = base;
            Vec3 maxCorner = Vec3{base.x + cellSize, height, base.z + cellSize};

            // Front face (towards -Z)
            builder.appendQuad(Vec3{minCorner.x, 0.0f, minCorner.z}, Vec3{maxCorner.x, 0.0f, minCorner.z}, Vec3{maxCorner.x, height, minCorner.z}, Vec3{minCorner.x, height, minCorner.z}, Vec3{0.0f, 0.0f, -1.0f});
            // Back face (towards +Z)
            builder.appendQuad(Vec3{maxCorner.x, 0.0f, maxCorner.z}, Vec3{minCorner.x, 0.0f, maxCorner.z}, Vec3{minCorner.x, height, maxCorner.z}, Vec3{maxCorner.x, height, maxCorner.z}, Vec3{0.0f, 0.0f, 1.0f});
            // Left face (-X)
            builder.appendQuad(Vec3{minCorner.x, 0.0f, maxCorner.z}, Vec3{minCorner.x, 0.0f, minCorner.z}, Vec3{minCorner.x, height, minCorner.z}, Vec3{minCorner.x, height, maxCorner.z}, Vec3{-1.0f, 0.0f, 0.0f});
            // Right face (+X)
            builder.appendQuad(Vec3{maxCorner.x, 0.0f, minCorner.z}, Vec3{maxCorner.x, 0.0f, maxCorner.z}, Vec3{maxCorner.x, height, maxCorner.z}, Vec3{maxCorner.x, height, minCorner.z}, Vec3{1.0f, 0.0f, 0.0f});
        }
    }

    glBindBuffer(GL_ARRAY_BUFFER, vbo);
    glBufferData(GL_ARRAY_BUFFER, static_cast<long>(builder.vertices.size() * sizeof(Vertex)), builder.vertices.data(), GL_STATIC_DRAW);
    glBindBuffer(GL_ELEMENT_ARRAY_BUFFER, ebo);
    glBufferData(GL_ELEMENT_ARRAY_BUFFER, static_cast<long>(builder.indices.size() * sizeof(unsigned int)), builder.indices.data(), GL_STATIC_DRAW);
    indexCount = builder.indices.size();
}

void Renderer::renderScene(const Level &level, const Mat4 &view, const Mat4 &projection, const Vec3 &cameraPosition, float timeSeconds) {
    (void)level;
    glClearColor(0.12f, 0.14f, 0.18f, 1.0f);
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    shader.use();
    shader.setMat4("uView", view.data());
    shader.setMat4("uProjection", projection.data());
    shader.setVec3("uCameraPos", cameraPosition.x, cameraPosition.y, cameraPosition.z);
    shader.setFloat("uTime", timeSeconds);

    wallTexture.bind(0);
    floorTexture.bind(1);
    ceilingTexture.bind(2);
    shader.setInt("uWall", 0);
    shader.setInt("uFloor", 1);
    shader.setInt("uCeiling", 2);

    glBindVertexArray(vao);
    glDrawElements(GL_TRIANGLES, static_cast<int>(indexCount), GL_UNSIGNED_INT, nullptr);
    glBindVertexArray(0);
}

