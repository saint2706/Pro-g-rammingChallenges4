#pragma once

#include <string>

struct VRHookInfo {
    bool enabled{false};
    std::string runtime{"None"};
};

VRHookInfo queryVRRuntime();

