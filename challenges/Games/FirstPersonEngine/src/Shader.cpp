#include "Shader.h"

#include <glad/glad.h>

#include <fstream>
#include <sstream>
#include <string>

namespace {
std::string readFile(const std::string &path) {
    std::ifstream file(path);
    if (!file) {
        return {};
    }
    std::ostringstream ss;
    ss << file.rdbuf();
    return ss.str();
}

unsigned int compileShader(unsigned int type, const std::string &source, std::string &log) {
    unsigned int shader = glCreateShader(type);
    const char *src = source.c_str();
    glShaderSource(shader, 1, &src, nullptr);
    glCompileShader(shader);

    int success = 0;
    glGetShaderiv(shader, GL_COMPILE_STATUS, &success);
    if (!success) {
        int length = 0;
        glGetShaderiv(shader, GL_INFO_LOG_LENGTH, &length);
        std::string info(length, '\0');
        glGetShaderInfoLog(shader, length, nullptr, info.data());
        log = info;
        glDeleteShader(shader);
        return 0;
    }
    return shader;
}
} // namespace

Shader::~Shader() {
    if (programId != 0) {
        glDeleteProgram(programId);
        programId = 0;
    }
}

bool Shader::loadFromFiles(const std::string &vertexPath, const std::string &fragmentPath, std::string &log) {
    std::string vertexSource = readFile(vertexPath);
    if (vertexSource.empty()) {
        log = "Failed to read vertex shader: " + vertexPath;
        return false;
    }
    std::string fragmentSource = readFile(fragmentPath);
    if (fragmentSource.empty()) {
        log = "Failed to read fragment shader: " + fragmentPath;
        return false;
    }

    unsigned int vertex = compileShader(GL_VERTEX_SHADER, vertexSource, log);
    if (!vertex) {
        return false;
    }
    unsigned int fragment = compileShader(GL_FRAGMENT_SHADER, fragmentSource, log);
    if (!fragment) {
        glDeleteShader(vertex);
        return false;
    }

    unsigned int program = glCreateProgram();
    glAttachShader(program, vertex);
    glAttachShader(program, fragment);
    glLinkProgram(program);

    int success = 0;
    glGetProgramiv(program, GL_LINK_STATUS, &success);
    if (!success) {
        int length = 0;
        glGetProgramiv(program, GL_INFO_LOG_LENGTH, &length);
        std::string info(length, '\0');
        glGetProgramInfoLog(program, length, nullptr, info.data());
        log = info;
        glDeleteShader(vertex);
        glDeleteShader(fragment);
        glDeleteProgram(program);
        return false;
    }

    glDetachShader(program, vertex);
    glDetachShader(program, fragment);
    glDeleteShader(vertex);
    glDeleteShader(fragment);

    programId = program;
    return true;
}

void Shader::use() const {
    glUseProgram(programId);
}

void Shader::setMat4(const std::string &name, const float *data) const {
    int location = glGetUniformLocation(programId, name.c_str());
    if (location >= 0) {
        glUniformMatrix4fv(location, 1, GL_FALSE, data);
    }
}

void Shader::setVec3(const std::string &name, float x, float y, float z) const {
    int location = glGetUniformLocation(programId, name.c_str());
    if (location >= 0) {
        glUniform3f(location, x, y, z);
    }
}

void Shader::setFloat(const std::string &name, float value) const {
    int location = glGetUniformLocation(programId, name.c_str());
    if (location >= 0) {
        glUniform1f(location, value);
    }
}

void Shader::setInt(const std::string &name, int value) const {
    int location = glGetUniformLocation(programId, name.c_str());
    if (location >= 0) {
        glUniform1i(location, value);
    }
}

