close all;
clear all;
clc;

%% Parameters of the system
%% This is the parameters we need to estimate
% Center
C = [3, 2];
% Radius
R = 1;

% Number of points
N = 1000;
% Noise
k = 0.050;

%% Create noisy circle
alpha = 2 * pi * rand(1, N);
noise = k * 2 * randn(1, N) - 1;
Points = C + [R * noise .* cos(alpha); R * noise .* sin(alpha)]';

%% Draw the noisy circle
plot (Points(:, 1), Points(:, 2), 'b.');
axis square equal;
grid on;

%% Prepare matrices
A = [Points(:, 1) Points(:, 2) ones(N, 1)];
B = [Points(:, 1) .* Points(:, 1) + Points(:, 2) .* Points(:, 2)];

%% Least square approximation
X = pinv(A) * B;

%% Calculate circle parameter
xc = X(1) / 2
yc = X(2) / 2
r = sqrt(4 * X(3) + X(1) * X(1) + X(2) * X(2)) / 2

%% Draw the fitted circle
Circle = [xc + r * cos([0:pi / 50:2 * pi]); yc + r * sin([0:pi / 50:2 * pi])]';
hold on;
plot (Circle(:, 1), Circle(:, 2), 'r', 'LineWidth', 2);
legend ('Points', 'Least-square circle');
