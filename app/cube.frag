#version 330 core

uniform vec3 uCamera;
uniform vec4 uDiffuse;

in      vec3 vPosition;
in      vec3 vNormal;

out     vec4 fragColor;

const   vec3 lightColor = vec3(1);
const   float ambient = 0.2;

void main() {
    // Get surface color
    vec4 surfaceColor = uDiffuse;

    // Do lighting
    vec3 lightPosition = uCamera;
    
    //normal in world coordinates (from frag shader)
    vec3 normal = normalize(vNormal);

    //location of this fragment in world coordinates (from frag shader)
    vec3 surfacePos = vPosition;

    vec3 surfaceToLight = normalize(lightPosition - surfacePos);

    // Calculate final color of the pixel, based on:
    // 1. The angle of incidence: diffuseCoefficient
    // 2. The color/intensities of the light: lightColor
    // 3. The diffuse color: surfaceColor

    float diffuseCoefficient = max(ambient, dot(normal, surfaceToLight));
    vec3 diffuseLit = diffuseCoefficient * surfaceColor.rgb * lightColor;

    fragColor = vec4(diffuseLit, uDiffuse.a);
}
