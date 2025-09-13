close all;
clear all;
clc;

% Generate random array of 10 integers from 0 to 100
x = randi([0 100], 1, 10);
y = randi([0 100], 1, 10);

% Generate array xy and xSquare
xy = x .* y;
xSquare = x .^ 2;

% calculate slope m
m = (10 * sum(xy) - (sum(x) * sum(y))) / (10 * sum(xSquare) - (sum(x) ^ 2));
% calculate intercept c
c = (sum(y) - m * sum(x)) / 10;

% Scatter plot the x and y arrays
scatter(x, y);
hold on;
% plot the line using slope and intercept on the same plot
plot(x, m * x + c);
% save the plot to plot.png
saveas(gcf, 'plot.png');
