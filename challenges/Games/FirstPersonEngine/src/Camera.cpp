#include "Camera.h"

#include <algorithm>
#include <cmath>

void Camera::setYawPitch(float yawRadians, float pitchRadians) {
    yaw = yawRadians;
    pitch = std::clamp(pitchRadians, -1.5f, 1.5f);
}

void Camera::rotate(float deltaYaw, float deltaPitch) {
    yaw += deltaYaw;
    pitch = std::clamp(pitch + deltaPitch, -1.5f, 1.5f);
}

Mat4 Camera::viewMatrix() const {
    Vec3 direction{
        std::cos(pitch) * std::cos(yaw),
        std::sin(pitch),
        std::cos(pitch) * std::sin(yaw)};
    Vec3 target = position + direction;
    return Mat4::lookAt(position, target, Vec3{0.0f, 1.0f, 0.0f});
}

Mat4 Camera::projectionMatrix(float aspectRatio) const {
    return Mat4::perspective(45.0f * 3.1415926f / 180.0f, aspectRatio, 0.1f, 100.0f);
}

Vec3 Camera::forward() const {
    return normalize(Vec3{std::cos(pitch) * std::cos(yaw), std::sin(pitch), std::cos(pitch) * std::sin(yaw)});
}

Vec3 Camera::right() const {
    Vec3 f = forward();
    return normalize(cross(f, Vec3{0.0f, 1.0f, 0.0f}));
}

