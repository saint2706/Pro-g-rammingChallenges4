#include "Level.h"

#include "Json.h"
#include "Texture.h"

#include <filesystem>
#include <fstream>
#include <sstream>
#include <stdexcept>

namespace {
bool parseSpawn(const JsonValue &value, Vec3 &out, std::string &log) {
    if (!value.isArray()) {
        log = "Level 'spawn' field must be an array";
        return false;
    }
    const auto &entries = value.asArray();
    if (entries.size() != 3) {
        log = "Level 'spawn' array must contain exactly three numbers";
        return false;
    }
    Vec3 result;
    for (size_t i = 0; i < 3; ++i) {
        auto number = entries[i].asNumber();
        if (!number) {
            log = "Level 'spawn' entries must be numeric";
            return false;
        }
        if (i == 0) {
            result.x = static_cast<float>(*number);
        } else if (i == 1) {
            result.y = static_cast<float>(*number);
        } else {
            result.z = static_cast<float>(*number);
        }
    }
    out = result;
    return true;
}

bool parseLayout(const JsonValue &value, std::vector<std::string> &rows, std::string &log) {
    if (!value.isArray()) {
        log = "Level 'layout' field must be an array of strings";
        return false;
    }
    const auto &array = value.asArray();
    if (array.empty()) {
        log = "Level layout must contain at least one row";
        return false;
    }
    rows.clear();
    rows.reserve(array.size());
    for (const auto &entry : array) {
        auto str = entry.asString();
        if (!str) {
            log = "Level layout rows must be strings";
            return false;
        }
        rows.push_back(*str);
    }
    return true;
}

bool parseColorField(const JsonValue &object, const std::string &key, unsigned int &out, std::string &log) {
    if (const JsonValue *value = object.find(key)) {
        auto colorString = value->asString();
        if (!colorString) {
            log = "Level color fields must be strings";
            return false;
        }
        try {
            out = hexToRGBA(*colorString);
        } catch (const std::exception &ex) {
            log = std::string("Invalid color value for '") + key + "': " + ex.what();
            return false;
        }
    }
    return true;
}
} // namespace

bool Level::loadFromFile(const std::string &path, std::string &log) {
    auto content = readTextFile(path);
    if (!content) {
        log = "Failed to read level file: " + path;
        return false;
    }

    std::string errorMessage;
    auto parsed = parseJson(*content, errorMessage);
    if (!parsed) {
        log = "Failed to parse level JSON: " + errorMessage;
        return false;
    }
    if (!parsed->isObject()) {
        log = "Level JSON root must be an object";
        return false;
    }

    const JsonValue &root = *parsed;

    if (const JsonValue *cell = root.find("cell_size")) {
        if (auto number = cell->asNumber()) {
            cellSizeValue = static_cast<float>(*number);
        } else {
            log = "Level 'cell_size' must be numeric";
            return false;
        }
    }

    if (const JsonValue *height = root.find("max_height")) {
        if (auto number = height->asNumber()) {
            maxHeightValue = static_cast<float>(*number);
        } else {
            log = "Level 'max_height' must be numeric";
            return false;
        }
    }

    if (const JsonValue *spawnValue = root.find("spawn")) {
        if (!parseSpawn(*spawnValue, spawn, log)) {
            return false;
        }
    }

    const JsonValue *layoutValue = root.find("layout");
    if (!layoutValue) {
        log = "Level JSON must contain a 'layout' array";
        return false;
    }
    if (!parseLayout(*layoutValue, gridLayout, log)) {
        return false;
    }

    if (!parseColorField(root, "wall", textureConfig.wallColor, log)) {
        return false;
    }
    if (!parseColorField(root, "floor", textureConfig.floorColor, log)) {
        return false;
    }
    if (!parseColorField(root, "ceiling", textureConfig.ceilingColor, log)) {
        return false;
    }

    return true;
}

bool Level::isWall(int x, int z) const {
    if (gridLayout.empty()) {
        return true;
    }
    if (z < 0 || static_cast<size_t>(z) >= gridLayout.size()) {
        return true;
    }
    const auto &row = gridLayout[static_cast<size_t>(z)];
    if (x < 0 || static_cast<size_t>(x) >= row.size()) {
        return true;
    }
    char tile = row[static_cast<size_t>(x)];
    return tile != '0' && tile != '.';
}

std::optional<std::string> readTextFile(const std::string &path) {
    std::ifstream file(path);
    if (!file) {
        return std::nullopt;
    }
    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

std::string resolveAssetPath(const std::string &rootHint, const std::string &relativePath) {
    namespace fs = std::filesystem;
    fs::path relative(relativePath);
    std::vector<fs::path> candidates;
    if (!rootHint.empty()) {
        candidates.emplace_back(fs::path(rootHint) / relative);
    }
    candidates.emplace_back(fs::current_path() / relative);

    for (const auto &candidate : candidates) {
        if (fs::exists(candidate)) {
            return candidate.string();
        }
    }
    return relative.string();
}

