#pragma once

#include "Math.h"

class Camera {
  public:
    void setPosition(const Vec3 &pos) { position = pos; }
    const Vec3 &getPosition() const { return position; }

    void setYawPitch(float yawRadians, float pitchRadians);
    void rotate(float deltaYaw, float deltaPitch);

    Mat4 viewMatrix() const;
    Mat4 projectionMatrix(float aspectRatio) const;

    Vec3 forward() const;
    Vec3 right() const;

  private:
    Vec3 position{0.0f, 1.0f, 0.0f};
    float yaw{0.0f};
    float pitch{0.0f};
};

