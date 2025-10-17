#version 330 core

layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNormal;
layout (location = 2) in vec2 aTexCoord;

out VS_OUT {
    vec3 WorldPos;
    vec3 Normal;
    vec2 TexCoord;
} vs_out;

uniform mat4 uView;
uniform mat4 uProjection;

void main()
{
    vs_out.WorldPos = aPos;
    vs_out.Normal = aNormal;
    vs_out.TexCoord = aTexCoord;
    gl_Position = uProjection * uView * vec4(aPos, 1.0);
}
