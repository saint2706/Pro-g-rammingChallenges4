#pragma once

#include <map>
#include <string>

struct KeyBindings {
    int moveForward{87};   // W
    int moveBackward{83};  // S
    int moveLeft{65};      // A
    int moveRight{68};     // D
    int jump{32};          // Space
    int sprint{340};       // Left shift
};

class InputManager {
  public:
    bool loadBindings(const std::string &path, std::string &log);
    const KeyBindings &bindings() const { return keyBindings; }

  private:
    KeyBindings keyBindings;
};

int keyFromName(const std::string &name);
std::string actionToKeyName(const std::string &action);

