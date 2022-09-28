#version 300 es

precision mediump float;

vec4 COLOR;
vec2 UV;

vec4 SRC;

//<indigo-fragment>

// The field order looks random, but its important for data packing.
layout (std140) uniform InnerGlowData {
  vec4 GLOW_COLOR;
  vec2 SCREEN_SIZE;
  float INTENSITY;
};

// Calculates the distance of the current fragment to the edge of a box.
float sdfCalc(vec2 p, vec2 b){
  vec2 d = abs(p) - b;
  return length(max(d, 0.0)) + min(max(d.x, d.y), 0.0);
}

void fragment(){

  // Make sure the intensity is a sensible value and invert.
  float intensity = 1.0 - clamp(INTENSITY, 0.05, 0.95);

  // Work out the aspect ratio to make the glow even. Assumes width > height.
  float aspect = SCREEN_SIZE.x / SCREEN_SIZE.y;

  // Shift the UV to be around the origin.
  vec2 p = ((UV * 2.0) - 1.0) * aspect;

  // Distance to the boundary from the origin
  vec2 b = vec2(aspect);

  // Work out the magic sdf distance value.
  float sdf = abs(sdfCalc(p, b));

  // Massage it into a nice shape.
  sdf = 1.0 - sdf;
  sdf = pow(sdf, 10.0 * intensity);
  sdf = smoothstep(0.0, 1.0, sdf);

  // Create the glow colour, remembering to account for premultiplied alpha.
  float alpha = sdf * GLOW_COLOR.a;
  vec4 glow = vec4(GLOW_COLOR.rgb * alpha, alpha);

  // Blend the normal colours of the layer with the glow.
  COLOR = mix(SRC, glow, glow.a);

}
//</indigo-fragment>
