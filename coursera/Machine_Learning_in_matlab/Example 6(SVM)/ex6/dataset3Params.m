function [C, sigma] = dataset3Params(X, y, Xval, yval)
%EX6PARAMS returns your choice of C and sigma for Part 3 of the exercise
%where you select the optimal (C, sigma) learning parameters to use for SVM
%with RBF kernel
%   [C, sigma] = EX6PARAMS(X, y, Xval, yval) returns your choice of C and 
%   sigma. You should complete this function to return the optimal C and 
%   sigma based on a cross-validation set.
%

% You need to return the following variables correctly.
% C = 1;
% sigma = 0.3;

% ====================== YOUR CODE HERE ======================
% Instructions: Fill in this function to return the optimal C and sigma
%               learning parameters found using the cross validation set.
%               You can use svmPredict to predict the labels on the cross
%               validation set. For example, 
%                   predictions = svmPredict(model, Xval);
%               will return the predictions on the cross validation set.
%
%  Note: You can compute the prediction error using 
%        mean(double(predictions ~= yval))
%

c=[.01 .03 .1 .3 1 3 10 30];
sig=[.01 .03 .1 .3 1 3 10 30];

for i=1:length(c),
    for j=1:length(sig),
        c1=c(i);
        sig1=sig(j);
model= svmTrain(X, y, c1, @(x1, x2) gaussianKernel(x1, x2, sig1)); 
predictions = svmPredict(model, Xval);
error(i,j) = mean(double(predictions ~= yval));

    end;
end;

pval=min(error);
pval=min(pval,[],2);
[c1,sig1]=find(error==pval);
C=c(c1);
sigma=sig(sig1);



% =========================================================================

end



