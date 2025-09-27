#pragma once

#include <string>

class Shader {
  public:
    Shader() = default;
    ~Shader();

    bool loadFromFiles(const std::string &vertexPath, const std::string &fragmentPath, std::string &log);
    void use() const;
    unsigned int id() const { return programId; }

    void setMat4(const std::string &name, const float *data) const;
    void setVec3(const std::string &name, float x, float y, float z) const;
    void setFloat(const std::string &name, float value) const;
    void setInt(const std::string &name, int value) const;

  private:
    unsigned int programId{0};
};

