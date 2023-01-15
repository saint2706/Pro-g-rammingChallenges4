import random
import matplotlib.pyplot as plt

# generate array of 10 random integers between 1 to 100
x = [random.randint(1, 100) for i in range(10)]
y = [random.randint(1, 100) for i in range(10)]

# Least Squares Linear Regression
xy = [x[i] * y[i] for i in range(len(x))]
x_sqr = [x[i] * x[i] for i in range(len(x))]
n = len(x)

# calculate slope
m = (n * sum(xy) - sum(x) * sum(y)) / (n * sum(x_sqr) - sum(x) * sum(x))

# calculate intercept
b = (sum(y) - m * sum(x)) / n

# plotting
plt.xticks(range(0, 101, 10))
plt.yticks(range(0, 101, 10))
plt.scatter(x, y, color="blue", s=25)
ax = plt.gca()
ax.set_xlabel("x")
ax.set_ylabel("y")
plt.plot([min(x), max(x)], [m * min(x) + b, m * max(x) + b], color="red")

# show label with coordinate on scatter points
for i in range(len(x)):
    plt.annotate(f"({x[i]}, {y[i]})", (x[i], y[i]))

# plot dotted lines between scatter points and regression line
for i in range(len(x)):
    plt.plot([x[i], x[i]], [y[i], m * x[i] + b], color="green", linestyle="--")

# plot legend
plt.legend(["Scatter Points", "Regression Line", "Error Lines"])

# Save plot as plot.png
path = r"Algorithmic\Medium\Least Squares Fitting\plot.png"
plt.savefig(path)
