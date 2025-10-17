close all;  % Close figures
clear;      % Clear variables
clc;        % Clear console

%% Circle parameters (ground truth) we attempt to re-estimate from noisy data
C = [3, 2];   % Center (x_c, y_c)
R = 1;        % True radius

N = 1000;     % Number of noisy samples
k = 0.050;    % Noise scale factor

%% Create noisy circle
alpha = 2 * pi * rand(1, N);                % Random angles
noise = k * 2 * randn(1, N) - 1;            % Radial perturbation
Points = C + [R * noise .* cos(alpha); ...  % X component
			  R * noise .* sin(alpha)]';    % Y component

%% Draw the noisy circle
plot (Points(:, 1), Points(:, 2), 'b.'); % Scatter noisy points
axis square equal;
grid on;

%% Prepare matrices
A = [Points(:, 1) Points(:, 2) ones(N, 1)];                        % Design matrix
B = [Points(:, 1) .* Points(:, 1) + Points(:, 2) .* Points(:, 2)];  % Squared radii

%% Least square approximation
X = pinv(A) * B; % Pseudo-inverse solution to overdetermined system

%% Calculate circle parameter
xc = X(1) / 2;  % Estimated center x
yc = X(2) / 2;  % Estimated center y
r = sqrt(4 * X(3) + X(1) * X(1) + X(2) * X(2)) / 2; % Estimated radius
fprintf('Estimated circle: center=(%.3f, %.3f), r=%.3f\n', xc, yc, r);

%% Draw the fitted circle
theta = 0:pi / 50:2 * pi;  % Angle grid for smooth circle
Circle = [xc + r * cos(theta); yc + r * sin(theta)]';
hold on;
plot (Circle(:, 1), Circle(:, 2), 'r', 'LineWidth', 2);
legend ('Noisy Points', 'Least-Squares Circle');
title('Least Squares Circle Fitting');
