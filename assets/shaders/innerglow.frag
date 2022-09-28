#version 300 es

precision mediump float;

uniform sampler2D SRC_CHANNEL;

vec4 CHANNEL_0;
vec4 COLOR;
vec2 UV;
vec2 SIZE;

vec4 DST;
vec4 SRC;

//<indigo-fragment>
layout (std140) uniform InnerGlowData {
  vec4 GLOW_COLOR;
  // TODO: amount
  // TODO: width
  // TODO: height
};

float sdfCalc(vec2 p, vec2 b){
  vec2 d = abs(p) - b;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

void fragment(){

  float amount = clamp(0.25, 0.0, 1.0);
  vec2 aspect = vec2(1.0, 1.0 + (500.0 / 800.0));

  float sdf = sdfCalc((UV * 2.0) - 1.0, vec2(1.0) - (vec2(amount, amount) * aspect));

  COLOR = SRC + vec4(vec3(sdf), 1.0);

}
//</indigo-fragment>
