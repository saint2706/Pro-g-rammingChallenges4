#pragma once

#include <array>
#include <cmath>

struct Vec2 {
    float x{0.0f};
    float y{0.0f};

    Vec2() = default;
    Vec2(float x_, float y_) : x(x_), y(y_) {}
};

struct Vec3 {
    float x{0.0f};
    float y{0.0f};
    float z{0.0f};

    Vec3() = default;
    Vec3(float x_, float y_, float z_) : x(x_), y(y_), z(z_) {}

    Vec3 operator+(const Vec3 &rhs) const { return Vec3{x + rhs.x, y + rhs.y, z + rhs.z}; }
    Vec3 operator-(const Vec3 &rhs) const { return Vec3{x - rhs.x, y - rhs.y, z - rhs.z}; }
    Vec3 operator*(float scalar) const { return Vec3{x * scalar, y * scalar, z * scalar}; }
    Vec3 &operator+=(const Vec3 &rhs) {
        x += rhs.x;
        y += rhs.y;
        z += rhs.z;
        return *this;
    }
    Vec3 &operator-=(const Vec3 &rhs) {
        x -= rhs.x;
        y -= rhs.y;
        z -= rhs.z;
        return *this;
    }
    Vec3 &operator*=(float scalar) {
        x *= scalar;
        y *= scalar;
        z *= scalar;
        return *this;
    }
};

inline Vec3 cross(const Vec3 &a, const Vec3 &b) {
    return Vec3{a.y * b.z - a.z * b.y, a.z * b.x - a.x * b.z, a.x * b.y - a.y * b.x};
}

inline float dot(const Vec3 &a, const Vec3 &b) {
    return a.x * b.x + a.y * b.y + a.z * b.z;
}

inline Vec3 normalize(const Vec3 &v) {
    float length = std::sqrt(dot(v, v));
    if (length <= 0.0f) {
        return Vec3{0.0f, 0.0f, 0.0f};
    }
    return Vec3{v.x / length, v.y / length, v.z / length};
}

struct Mat4 {
    std::array<float, 16> elements{};

    Mat4() {
        for (auto &value : elements) {
            value = 0.0f;
        }
    }

    static Mat4 identity() {
        Mat4 result;
        result.elements[0] = 1.0f;
        result.elements[5] = 1.0f;
        result.elements[10] = 1.0f;
        result.elements[15] = 1.0f;
        return result;
    }

    static Mat4 translation(const Vec3 &offset) {
        Mat4 result = identity();
        result.elements[12] = offset.x;
        result.elements[13] = offset.y;
        result.elements[14] = offset.z;
        return result;
    }

    static Mat4 scale(float value) {
        Mat4 result = identity();
        result.elements[0] = value;
        result.elements[5] = value;
        result.elements[10] = value;
        return result;
    }

    static Mat4 rotationY(float angleRadians) {
        Mat4 result = identity();
        float c = std::cos(angleRadians);
        float s = std::sin(angleRadians);
        result.elements[0] = c;
        result.elements[2] = s;
        result.elements[8] = -s;
        result.elements[10] = c;
        return result;
    }

    static Mat4 perspective(float fovyRadians, float aspect, float nearPlane, float farPlane) {
        Mat4 result;
        float f = 1.0f / std::tan(fovyRadians / 2.0f);
        result.elements[0] = f / aspect;
        result.elements[5] = f;
        result.elements[10] = (farPlane + nearPlane) / (nearPlane - farPlane);
        result.elements[11] = -1.0f;
        result.elements[14] = (2.0f * farPlane * nearPlane) / (nearPlane - farPlane);
        return result;
    }

    static Mat4 lookAt(const Vec3 &eye, const Vec3 &center, const Vec3 &up) {
        Vec3 f = normalize(center - eye);
        Vec3 s = normalize(cross(f, up));
        Vec3 u = cross(s, f);

        Mat4 result = identity();
        result.elements[0] = s.x;
        result.elements[4] = s.y;
        result.elements[8] = s.z;

        result.elements[1] = u.x;
        result.elements[5] = u.y;
        result.elements[9] = u.z;

        result.elements[2] = -f.x;
        result.elements[6] = -f.y;
        result.elements[10] = -f.z;

        result.elements[12] = -dot(s, eye);
        result.elements[13] = -dot(u, eye);
        result.elements[14] = dot(f, eye);
        return result;
    }

    Mat4 operator*(const Mat4 &rhs) const {
        Mat4 result;
        for (int row = 0; row < 4; ++row) {
            for (int col = 0; col < 4; ++col) {
                float sum = 0.0f;
                for (int k = 0; k < 4; ++k) {
                    sum += elements[k * 4 + row] * rhs.elements[col * 4 + k];
                }
                result.elements[col * 4 + row] = sum;
            }
        }
        return result;
    }

    const float *data() const { return elements.data(); }
};

inline Mat4 composeTransform(const Vec3 &position, float rotationYRadians, float scaleValue) {
    Mat4 translation = Mat4::translation(position);
    Mat4 rotation = Mat4::rotationY(rotationYRadians);
    Mat4 scale = Mat4::scale(scaleValue);
    return translation * rotation * scale;
}

