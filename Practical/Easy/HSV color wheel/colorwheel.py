import colour
import matplotlib.pyplot as plt
import numpy as np

COLOUR_STYLE = colour.plotting.colour_style()
COLOUR_STYLE.update(
    {
        "figure.figsize": (11, 11),
        "legend.framealpha": colour.plotting.COLOUR_STYLE_CONSTANTS.opacity.low,
    }
)
plt.style.use(COLOUR_STYLE)
plt.style.use("dark_background")
colour.utilities.describe_environment()
colour.utilities.filter_warnings(*[True] * 4)


def colour_wheel(samples=1024, clip_circle=True, method="Colour"):
    xx, yy = np.meshgrid(np.linspace(-1, 1, samples), np.linspace(-1, 1, samples))

    S = np.sqrt(xx**2 + yy**2)
    H = (np.arctan2(xx, yy) + np.pi) / (np.pi * 2)

    HSV = colour.utilities.tstack([H, S, np.ones(H.shape)])
    RGB = colour.HSV_to_RGB(HSV)

    if clip_circle:
        RGB[S > 1] = 0
        A = np.where(S > 1, 0, 1)
    else:
        A = np.ones(S.shape)

    if method.lower() == "matplotlib":
        RGB = colour.utilities.orient(RGB, "90 CW")
    elif method.lower() == "nuke":
        RGB = colour.utilities.orient(RGB, "Flip")
        RGB = colour.utilities.orient(RGB, "90 CW")

    R, G, B = colour.utilities.tsplit(RGB)

    return colour.utilities.tstack([R, G, B, A])


COLOUR_WHEEL = colour_wheel(method="Nuke")
colour.plotting.plot_image(COLOUR_WHEEL)
