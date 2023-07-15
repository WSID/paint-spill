# paint-spill

This library is for accelerated vector graphics with vulkan.

## Rendering Stages (Planned: May change)

1.  High-level draw instructions, and bunch of patterned shapes.

    This is what user would treat.

2.  For patterns, Set of pipelines and uniforms are used to implement this.

3.  For shapes, they will be polygon, and then triangulated.

4.  Render the triangles with pipelines.