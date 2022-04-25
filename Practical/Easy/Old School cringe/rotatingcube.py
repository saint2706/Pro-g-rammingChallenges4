from pylab import *
import numpy as np
import glob
from PIL import Image
import shutil


def quaternion_to_matrix(myx):

    xb, xc, xd = myx

    xnormsq = xb * xb + xc * xc + xd * xd

    if xnormsq < 1:

        b, c, d = xb, xc, xd
        a = np.sqrt(1 - xnormsq)
    else:

        b, c, d = -xb / xnormsq, -xc / xnormsq, -xd / xnormsq
        a = -np.sqrt(1 - 1.0 / xnormsq)

    assert a >= -1
    assert a <= 1

    return np.array(
        [
            [
                (a * a + b * b - c * c - d * d),
                (2 * b * c - 2 * a * d),
                (2 * b * d + 2 * a * c),
            ],
            [
                (2 * b * c + 2 * a * d),
                (a * a - b * b + c * c - d * d),
                (2 * c * d - 2 * a * b),
            ],
            [
                (2 * b * d - 2 * a * c),
                (2 * c * d + 2 * a * b),
                (a * a - b * b - c * c + d * d),
            ],
        ]
    ).T / (a * a + b * b + c * c + d * d)


def crazy_rotation(index, n_index):
    return quaternion_to_matrix(
        0.5 * sin(pi * 2 * index * n_index ** -1.0 * array([1, 2, 3]))
    )


def project(d):
    return vvs[:, :2] / (vvs[:, [2, 2]] - d)


vs = reshape(mgrid[-1:2:2, -1:2:2, -1:2:2].T, (8, 3))

ed = [
    (j, k)
    for j in range(8)
    for k in range(j, 8)
    if sum(abs(vs[j] - vs[k])) == 2
]


D = -5

figure(1, figsize=(6.4, 4.8))

ax = subplot(1, 1, 1)

suptitle("Cube animation")

Nind = 1080

for ind in range(Nind):
    print(ind)
    ax.clear()

    rotM = crazy_rotation(ind, Nind)

    vvs = dot(vs, rotM)

    pt = project(D)

    for j, k in ed:
        ax.plot(pt[[j, k], 0], pt[[j, k], 1], "g-", lw=3)

    ax.plot(pt[:, 0], pt[:, 1], "bo")

    ax.axis("equal")
    ax.axis([-0.5, 0.5, -0.5, 0.5])

    savefig(
        r"Practical\Easy\Old School cringe\rotatingcubepngs\anim%03d.png" % ind,
        dpi=100,
    )

fp_in = r"Practical\Easy\Old School cringe\rotatingcubepngs\anim*.png"
fp_out = r"Practical\Easy\Old School cringe\image.gif"

img, *imgs = [Image.open(f) for f in sorted(glob.glob(fp_in))]
img.save(
    fp=fp_out,
    format="GIF",
    append_images=imgs,
    save_all=True,
    duration=8.3,
    loop=0,
)

shutil.rmtree(r"Practical\Easy\Old School cringe\rotatingcubepngs")
