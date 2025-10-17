#include "VRHooks.h"

VRHookInfo queryVRRuntime() {
    VRHookInfo info;
#ifdef ENABLE_OPENXR
    info.enabled = true;
    info.runtime = "OpenXR (stubbed integration)";
#else
    info.enabled = false;
    info.runtime = "Disabled";
#endif
    return info;
}

