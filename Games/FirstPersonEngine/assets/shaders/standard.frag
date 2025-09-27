#version 330 core

out vec4 FragColor;

in VS_OUT {
    vec3 WorldPos;
    vec3 Normal;
    vec2 TexCoord;
} fs_in;

uniform sampler2D uWall;
uniform sampler2D uFloor;
uniform sampler2D uCeiling;
uniform vec3 uCameraPos;
uniform float uTime;

void main()
{
    vec3 normal = normalize(fs_in.Normal);
    vec3 lightDir = normalize(vec3(0.4, 1.0, 0.3));
    float diffuse = max(dot(normal, lightDir), 0.2);

    vec3 baseColor;
    if (normal.y > 0.5) {
        baseColor = texture(uFloor, fs_in.TexCoord * 2.0).rgb;
    } else if (normal.y < -0.5) {
        baseColor = texture(uCeiling, fs_in.TexCoord * 2.0).rgb;
    } else {
        baseColor = texture(uWall, fs_in.TexCoord * 2.0).rgb;
    }

    vec3 viewDir = normalize(uCameraPos - fs_in.WorldPos);
    vec3 halfwayDir = normalize(lightDir + viewDir);
    float specular = pow(max(dot(normal, halfwayDir), 0.0), 16.0);

    vec3 finalColor = baseColor * diffuse + vec3(0.15) * specular;
    FragColor = vec4(finalColor, 1.0);
}
