#version 330 core
out vec4 FragColor;

in vec2 texCoord;
in vec3 normal;
in vec3 fragPos;

struct Material {
    vec3 ambient;
    sampler2D diffuse;
    sampler2D specular;
    float shininess;
};

struct Light {
    vec3 ambient;
    vec3 diffuse;
    vec3 specular;
};

struct DirectionLight {
    vec3 direction;
    Light light;
};

struct PointLight {
    vec3 position;
    float constant;
    float linear;
    float quadratic;
    Light light;
};

struct SpotLight {
    vec3 position;
    vec3 direction;
    float innerCone;
    float outerCone;
    float constant;
    float linear;
    float quadratic;
    Light light;
};

#define SPOT_COUNT 3
#define POINT_COUNT 4
#define DIRECTION_COUNT 1
uniform PointLight[POINT_COUNT] points;
uniform SpotLight[SPOT_COUNT] spots;
uniform DirectionLight[DIRECTION_COUNT] directions;
uniform Material material;
uniform vec3 viewPos;

vec3 directionLight(DirectionLight light, vec3 norm, vec3 viewDir) {
    vec3 lightDir = normalize(-light.direction);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    vec3 ambient = light.light.ambient * vec3(texture(material.diffuse, texCoord));
    vec3 diffuse = light.light.diffuse * diff * vec3(texture(material.diffuse, texCoord));
    vec3 specular = light.light.specular * spec * vec3(texture(material.specular, texCoord));
    return ambient + diffuse + specular;
}

vec3 pointLight(PointLight light, vec3 norm, vec3 fragPos, vec3 viewDir) {
    vec3 lightDir = normalize(light.position - fragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    float distance = length(light.position - fragPos);
    float attenuation = 1.0 / (light.quadratic * distance * distance + light.linear * distance + light.constant);
    vec3 ambient = light.light.ambient * vec3(texture(material.diffuse, texCoord));
    vec3 diffuse = light.light.diffuse * diff * vec3(texture(material.diffuse, texCoord));
    vec3 specular = light.light.specular * spec * vec3(texture(material.specular, texCoord));
    return attenuation * (ambient + diffuse + specular);
}

vec3 spotLight(SpotLight light, vec3 norm, vec3 fragPos, vec3 viewDir) {
    vec3 lightDir = normalize(light.position - fragPos);
    float diff = max(dot(norm, lightDir), 0.0);
    vec3 reflectDir = reflect(-lightDir, norm);
    float spec = pow(max(dot(viewDir, reflectDir), 0.0), material.shininess);
    float distance = length(light.position - fragPos);
    float attenuation = 1.0 / (light.quadratic * distance * distance + light.linear * distance + light.constant);
    float lightAngle = dot(lightDir, normalize(-light.direction));
    float intensity = clamp((lightAngle - light.outerCone) / (light.innerCone - light.outerCone), 0.0, 1.0);
    vec3 ambient = light.light.ambient * vec3(texture(material.diffuse, texCoord));
    vec3 diffuse = light.light.diffuse * diff * vec3(texture(material.diffuse, texCoord));
    vec3 specular = light.light.specular * spec * vec3(texture(material.specular, texCoord));
    return attenuation * (ambient + intensity * (diffuse + specular));
}

void main()
{
    vec3 norm = normalize(normal);
    vec3 viewDir = normalize(viewPos - fragPos);
    vec3 result = vec3(0.0);
    for(int i = 0; i < DIRECTION_COUNT; i++)
        result += directionLight(directions[i], norm, viewDir);
    for(int i = 0; i < POINT_COUNT; i++)
        result += pointLight(points[i], norm, fragPos, viewDir);
    for(int i = 0; i < SPOT_COUNT; i++)
        result += spotLight(spots[i], norm, fragPos, viewDir);
    FragColor = vec4(result, 1.0);
}