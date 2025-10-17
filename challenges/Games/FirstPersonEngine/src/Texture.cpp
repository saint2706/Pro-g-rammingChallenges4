#include "Texture.h"

#include <glad/glad.h>

#include <array>
#include <cctype>
#include <stdexcept>
#include <string>
#include <vector>

Texture::~Texture() {
    if (textureId != 0) {
        glDeleteTextures(1, &textureId);
        textureId = 0;
    }
}

namespace {
std::array<unsigned char, 4> unpackColor(unsigned int rgba) {
    return {static_cast<unsigned char>((rgba >> 24) & 0xFF),
            static_cast<unsigned char>((rgba >> 16) & 0xFF),
            static_cast<unsigned char>((rgba >> 8) & 0xFF),
            static_cast<unsigned char>(rgba & 0xFF)};
}
}

void Texture::createCheckerboard(int size, unsigned int colorA, unsigned int colorB) {
    if (size <= 0) {
        size = 8;
    }
    if (textureId == 0) {
        glGenTextures(1, &textureId);
    }
    glBindTexture(GL_TEXTURE_2D, textureId);

    std::vector<unsigned char> pixels(size * size * 4, 0);
    auto a = unpackColor(colorA);
    auto b = unpackColor(colorB);
    for (int y = 0; y < size; ++y) {
        for (int x = 0; x < size; ++x) {
            bool useA = ((x / 2 + y / 2) % 2) == 0;
            const auto &color = useA ? a : b;
            size_t index = static_cast<size_t>(y * size + x) * 4;
            pixels[index + 0] = color[0];
            pixels[index + 1] = color[1];
            pixels[index + 2] = color[2];
            pixels[index + 3] = color[3];
        }
    }

    glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, size, size, 0, GL_RGBA, GL_UNSIGNED_BYTE, pixels.data());
    glGenerateMipmap(GL_TEXTURE_2D);

    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_REPEAT);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR_MIPMAP_LINEAR);
    glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);

    glBindTexture(GL_TEXTURE_2D, 0);
}

void Texture::bind(int unit) const {
    glActiveTexture(GL_TEXTURE0 + unit);
    glBindTexture(GL_TEXTURE_2D, textureId);
}

unsigned int hexToRGBA(const std::string &hex) {
    if (hex.empty()) {
        throw std::runtime_error("Empty hex string");
    }

    std::string value = hex;
    if (value.front() == '#') {
        value.erase(value.begin());
    }
    if (value.size() != 6 && value.size() != 8) {
        throw std::runtime_error("Expected RRGGBB or RRGGBBAA hex value");
    }

    auto hexToComponent = [](char c) -> int {
        if (c >= '0' && c <= '9') {
            return c - '0';
        }
        char lower = static_cast<char>(std::tolower(static_cast<unsigned char>(c)));
        if (lower >= 'a' && lower <= 'f') {
            return lower - 'a' + 10;
        }
        throw std::runtime_error("Invalid hex digit");
    };

    auto readComponent = [&](size_t index) {
        return (hexToComponent(value[index]) << 4) | hexToComponent(value[index + 1]);
    };

    unsigned int r = static_cast<unsigned int>(readComponent(0));
    unsigned int g = static_cast<unsigned int>(readComponent(2));
    unsigned int b = static_cast<unsigned int>(readComponent(4));
    unsigned int a = value.size() == 8 ? static_cast<unsigned int>(readComponent(6)) : 255u;

    return (r << 24) | (g << 16) | (b << 8) | a;
}

