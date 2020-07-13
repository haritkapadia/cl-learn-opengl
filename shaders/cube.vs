#version 330 core
layout (location = 0) in vec3 aPos;
layout (location = 1) in vec3 aNorm;
layout (location = 2) in vec2 aTex;

out vec2 texCoord;
out vec3 normal;
out vec3 fragPos;

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{

    gl_Position = projection * view * model * vec4(aPos, 1.0);
    texCoord = vec2(aTex.x, 1.0 - aTex.y);
    normal = mat3(transpose(inverse(model))) * aNorm;
    fragPos = vec3(model * vec4(aPos, 1.0));
}