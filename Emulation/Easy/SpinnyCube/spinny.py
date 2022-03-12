from vpython import scene, box, rate

scene.title = "Spinny Cube"

scene.range = 2
scene.autocenter = True

print("Drag with RMB to rotate view")
print("Scroll with wheel to zoom")

deg45 = 0.785398163387

cube = box()
cube.rotate(angle=deg45, axis=(1, 0, 0))
cube.rotate(angle=deg45, axis=(0, 0, 1))

while True:
    rate(50)
    cube.rotate(angle=0.005, axis=(0, 1, 0))
