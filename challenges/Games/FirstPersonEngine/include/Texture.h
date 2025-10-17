#pragma once

#include <string>

class Texture {
  public:
    Texture() = default;
    ~Texture();

    void createCheckerboard(int size, unsigned int colorA, unsigned int colorB);
    void bind(int unit) const;

  private:
    unsigned int textureId{0};
};

unsigned int hexToRGBA(const std::string &hex);

