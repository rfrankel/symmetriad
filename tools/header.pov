#include "colors.inc"

camera {
    location <4, 2, -10>
    look_at 0
    angle 36
}

plane { <0, 1, 0>, -10
    pigment {
      checker color White, color Grey
    }
  }

light_source { <10, 10, -20> White }

