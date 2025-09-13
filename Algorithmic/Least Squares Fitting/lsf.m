close all;  % Close existing figures
clear;      % Clear workspace variables (avoid clear all which also clears functions from memory)
clc;        % Clear command window

% Generate random arrays (row vectors) of 10 integers from 0 to 100.
% In a real scenario these would be measured samples.
x = randi([0 100], 1, 10);
y = randi([0 100], 1, 10);

% Element-wise products needed for closed-form OLS formulas
xy = x .* y;      % x_i * y_i terms
xSquare = x .^ 2; % x_i^2 terms

% Closed-form slope (m) and intercept (c) for simple linear regression
N = numel(x); % number of samples (10)
m = (N * sum(xy) - sum(x) * sum(y)) / (N * sum(xSquare) - (sum(x) ^ 2));
c = (sum(y) - m * sum(x)) / N;

% Scatter plot of original points
scatter(x, y, 36, 'filled');
hold on;
% Plot regression line y = m x + c using original x sampling
plot(x, m * x + c, 'r-', 'LineWidth', 1.5);
grid on;
title(sprintf('Least Squares Fit: y = %.2f x + %.2f', m, c));
xlabel('x'); ylabel('y');
% Save the figure (PNG)
saveas(gcf, 'plot.png');
fprintf('Saved plot to plot.png\n');
