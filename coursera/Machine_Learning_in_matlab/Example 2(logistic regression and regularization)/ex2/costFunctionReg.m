function [J, grad] = costFunctionReg(theta, X, y, lambda)
%COSTFUNCTIONREG Compute cost and gradient for logistic regression with regularization
%   J = COSTFUNCTIONREG(theta, X, y, lambda) computes the cost of using
%   theta as the parameter for regularized logistic regression and the
%   gradient of the cost w.r.t. to the parameters. 

% Initialize some useful values
m = length(y); % number of training examples

% You need to return the following variables correctly 
J = 0;
grad = zeros(size(theta));

% ====================== YOUR CODE HERE ======================
% Instructions: Compute the cost of a particular choice of theta.
%               You should set J to the cost.
%               Compute the partial derivatives and set grad to the partial
%               derivatives of the cost w.r.t. each parameter in theta

sig1 = sigmoid(X * theta);
thetaReg = theta(2:size(theta));
s =(sum(thetaReg .^ 2)*lambda)/(2*m);
p = -1 *  sum((log(sig1).*y + log(1-sig1).*(1-y) ) );
p= sum(p);
J = (p / m)+ s;


% h = sigmoid(X*theta);
% thetaReg = theta(2:size(theta));
% % evaluate cost function
% J = -1*(sum(y.*log(h)+(1-y).*(log(1-h)))/m) + (sum(thetaReg.^2)*lambda)/(2*m);
%  

theta = [0; thetaReg];
grad = grad + (X'*(sig1-y))/m + theta*lambda/m;




% =============================================================

end
