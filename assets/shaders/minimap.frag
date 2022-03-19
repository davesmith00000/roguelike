#version 300 es

precision mediump float;

vec2 CHANNEL_0_SIZE;
vec2 TEXTURE_SIZE;
vec4 COLOR;
vec2 UV;

//<indigo-fragment>
#define MAX_TILE_COUNT 1000

layout (std140) uniform MiniMapData {
  vec2 GRID_DIMENSIONS;
  vec2 PLAYER_POSITION;
};

layout (std140) uniform MiniMapBlockData {
  vec4[MAX_TILE_COUNT] BLOCKS;
};

int lookUp(int index, vec4[MAX_TILE_COUNT] data) {
  vec4 v = data[index / 4];
  return int(v[index % 4]);
}

void fragment(){

  vec2 ONE_TEXEL = CHANNEL_0_SIZE / TEXTURE_SIZE;

  // Which grid square am I in on the map? e.g. 3x3, coords (1,1)
  vec2 gridSquare = UV * GRID_DIMENSIONS;

  // Which sequential box is that? e.g. 4 of 9
  int index = int(floor(gridSquare.y) * GRID_DIMENSIONS.x + floor(gridSquare.x));

  // Check for wall, 0 none, 1 present
  int block = lookUp(index, BLOCKS);

  // 0 = none, 1 = wall, 2 = player, 3 = hostile
  switch(block) {
    case 0:
      COLOR = vec4(0.0, 0.0, 0.0, 1.0);
      break;

    case 1:
      COLOR = vec4(0.5, 0.5, 0.5, 1.0);
      break;

    default:
      COLOR = vec4(0.0, 0.0, 0.0, 1.0);
      break;
  }

  if(abs(gridSquare.x - PLAYER_POSITION.x) < 1.0 && abs(gridSquare.y - PLAYER_POSITION.y) < 1.0) {
    COLOR = vec4(0.0, 1.0, 0.0, 1.0);
  }

}
//</indigo-fragment>
