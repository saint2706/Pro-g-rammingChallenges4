#include "Input.h"

#include "Json.h"
#include "Level.h"

#include <GLFW/glfw3.h>

#include <algorithm>
#include <cctype>
#include <map>
#include <string>

namespace {
std::map<std::string, int> buildKeyMap() {
    std::map<std::string, int> mapping;
    for (char c = 'A'; c <= 'Z'; ++c) {
        mapping[std::string(1, c)] = GLFW_KEY_A + (c - 'A');
    }
    for (char c = '0'; c <= '9'; ++c) {
        mapping[std::string(1, c)] = GLFW_KEY_0 + (c - '0');
    }
    mapping["SPACE"] = GLFW_KEY_SPACE;
    mapping["LEFT_SHIFT"] = GLFW_KEY_LEFT_SHIFT;
    mapping["RIGHT_SHIFT"] = GLFW_KEY_RIGHT_SHIFT;
    mapping["LEFT_CONTROL"] = GLFW_KEY_LEFT_CONTROL;
    mapping["RIGHT_CONTROL"] = GLFW_KEY_RIGHT_CONTROL;
    mapping["LEFT_ALT"] = GLFW_KEY_LEFT_ALT;
    mapping["RIGHT_ALT"] = GLFW_KEY_RIGHT_ALT;
    mapping["UP"] = GLFW_KEY_UP;
    mapping["DOWN"] = GLFW_KEY_DOWN;
    mapping["LEFT"] = GLFW_KEY_LEFT;
    mapping["RIGHT"] = GLFW_KEY_RIGHT;
    return mapping;
}

int parseKey(const std::string &value) {
    static std::map<std::string, int> mapping = buildKeyMap();
    std::string upper;
    upper.reserve(value.size());
    for (unsigned char c : value) {
        if (std::isspace(c)) {
            continue;
        }
        upper.push_back(static_cast<char>(std::toupper(c)));
    }
    auto it = mapping.find(upper);
    if (it != mapping.end()) {
        return it->second;
    }
    if (upper.size() == 1 && std::isprint(static_cast<unsigned char>(upper[0]))) {
        char c = upper[0];
        if (c >= 'A' && c <= 'Z') {
            return GLFW_KEY_A + (c - 'A');
        }
        if (c >= '0' && c <= '9') {
            return GLFW_KEY_0 + (c - '0');
        }
    }
    return GLFW_KEY_UNKNOWN;
}

void applyBinding(const JsonValue &root, const char *action, int &key, std::string &log) {
    if (const JsonValue *value = root.find(action)) {
        auto keyName = value->asString();
        if (!keyName) {
            log += std::string("Binding for '") + action + "' must be a string.\n";
            return;
        }
        int parsedKey = parseKey(*keyName);
        if (parsedKey == GLFW_KEY_UNKNOWN) {
            log += std::string("Unrecognised key name for '") + action + "': " + *keyName + "\n";
            return;
        }
        key = parsedKey;
    }
}
} // namespace

bool InputManager::loadBindings(const std::string &path, std::string &log) {
    auto content = readTextFile(path);
    if (!content) {
        log = "Using default key bindings (config not found at " + path + ")";
        return true;
    }

    std::string errorMessage;
    auto parsed = parseJson(*content, errorMessage);
    if (!parsed) {
        log = "Failed to parse key bindings JSON: " + errorMessage;
        return false;
    }
    if (!parsed->isObject()) {
        log = "Key bindings file must contain a JSON object";
        return false;
    }

    log.clear();
    const JsonValue &root = *parsed;
    applyBinding(root, "move_forward", keyBindings.moveForward, log);
    applyBinding(root, "move_backward", keyBindings.moveBackward, log);
    applyBinding(root, "move_left", keyBindings.moveLeft, log);
    applyBinding(root, "move_right", keyBindings.moveRight, log);
    applyBinding(root, "jump", keyBindings.jump, log);
    applyBinding(root, "sprint", keyBindings.sprint, log);
    return true;
}

int keyFromName(const std::string &name) {
    return parseKey(name);
}

std::string actionToKeyName(const std::string &action) {
    return action;
}

