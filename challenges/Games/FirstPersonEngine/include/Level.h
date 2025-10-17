#pragma once

#include "Math.h"

#include <optional>
#include <string>
#include <vector>

struct LevelTextures {
    unsigned int wallColor{0xff888888};
    unsigned int floorColor{0xff404040};
    unsigned int ceilingColor{0xff707070};
};

class Level {
  public:
    bool loadFromFile(const std::string &path, std::string &log);

    float cellSize() const { return cellSizeValue; }
    const std::vector<std::string> &layout() const { return gridLayout; }
    const LevelTextures &textures() const { return textureConfig; }
    Vec3 spawnPosition() const { return spawn; }

    bool isWall(int x, int z) const;
    float maxHeight() const { return maxHeightValue; }

  private:
    float cellSizeValue{2.0f};
    float maxHeightValue{3.0f};
    std::vector<std::string> gridLayout;
    Vec3 spawn{1.5f, 1.0f, 1.5f};
    LevelTextures textureConfig;
};

std::optional<std::string> readTextFile(const std::string &path);
std::string resolveAssetPath(const std::string &rootHint, const std::string &relativePath);

