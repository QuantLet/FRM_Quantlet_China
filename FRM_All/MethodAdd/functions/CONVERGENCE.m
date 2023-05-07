function [PostG, DAG, R1, R2] = CONVERGENCE(DG, LogL, ny, Nx, lag)

LogL2 = LogL(2:end,:);
nsave = size(LogL2,1);
LogG = sum(LogL2, 2);

MEP_mean = DG./(nsave);        

[R1, NEFF1] = PSRF(LogG);
NEFF2 = 0;
if lag ~= 0;
    [R2, NEFF2] = MPSRF(LogL2); 
end
NEFF3 = Effective_Sample_Size(LogG);
neff = max([NEFF1,NEFF2,NEFF3]);

MEP_var = MEP_mean.*(1 - MEP_mean);
tval_upper = 1.65;
MEP_se   = sqrt(MEP_var./(neff));
PostG = reshape( MEP_mean, ny, []);
Q_a = MEP_mean - tval_upper*MEP_se;
Qa = reshape( Q_a, ny, []);

DAG = zeros(ny,Nx);     
DAG(Qa > 0.5) = 1; 

function [R, neff] = PSRF(X)

[N,M]=size(X);

onechain=0;
if M == 1;
    K = fix(N/3);
    Y = zeros(K,2);
    for i=1:2
        if i==1;
            Y(:,i) = X(1:K,:);    
        else
            Y(:,i) = X(end-K+1:end,:);
        end
    end
    Z = Y;
    onechain=1;
else
    Z = X;
end
[N,M]=size(Z);
% MM = M - 1;

W = 0;
for n = 1:M
    x = Z(1:N,n) - repmat(mean(Z(1:N,n)),N,1);
    W = W + sum(x.*x);
end
W = W / ((N-1) * M);
    
% Calculate variances B (in fact B/n) of the means.
Bpn = 0;
MX = mean(Z);
m = mean(MX);
for n=1:M
    x = mean(Z(1:N,n)) - m;
    Bpn = Bpn + x.*x;
end
Bpn = Bpn / (M-1);
    
% Calculate reduction factors
S = (N-1)/N * W + Bpn;
R = (M+1)/M * S ./ W - (N-1)/M/N;
V = R .* W;
R = real(sqrt(R));  
B = Bpn*N;

neff = fix(min(M*N*V./B,M*N));
if onechain && (nargout>1)
  neff=neff*3/2;
end

function NEFF = Effective_Sample_Size(Y)
% Estimate autocorrelation function of time series

[N, nvar] = size(Y);
ESS = zeros(1,nvar);
for i = 1:nvar
    x = Y(:,i);
    ct = autocorr(x);
    t =1 + 2*max(cumsum(ct));
    ESS(i) = N/t;
end
NEFF = real(fix(sum(ESS)));

function [MPSRF, neff] = MPSRF(varargin)
%   Method is from:
%      Brooks, S.P. and Gelman, A. (1998) General methods for
%      monitoring convergence of iterative simulations. Journal of
%      Computational and Graphical Statistics. 7, 434-455. Note that
%      this function returns square-root definiton of R (see Gelman
%      et al (2003), Bayesian Data Analsyis p. 297).

onechain=0;
if nargin==1
  X = varargin{1};
  if size(X,3)==1
    n = floor(size(X,1)/3);
    x = zeros([n size(X,2) 2]);
    x(:,:,1) = X(1:n,:);
    x(:,:,2) = X((end-n+1):end,:);
    X = x;
    onechain=1;
  end
elseif nargin==0
  error('Cannot calculate PSRF of scalar');
else
  X = zeros([size(varargin{1}) nargin]);
  for i=1:nargin
    X(:,:,i) = varargin{i};
  end
end

[N,D,M]=size(X);
% L = fix(N/2)+1;
L=N;
MPSRF = zeros(1,length(L:N));
for i=L:N
% Calculate mean W of the covariances
W = zeros(D);
for n=1:M
  x = X(1:i,:,n) - repmat(nanmean(X(1:i,:,n)),i,1);
  W = W + x'*x;
end
W = W / ((i-1) * M);

% Calculate covariance B (in fact B/n) of the means.
Bpn = zeros(D);
m = nanmean(reshape(nanmean(X),D,M),2)';
for n=1:M
  x = nanmean(X(1:i,:,n)) - m;
  Bpn = Bpn + x'*x;
end
Bpn = Bpn / (M-1);

% Calculate reduction factor R
E = sort(abs(eig(W \ Bpn)));
R = (i-1)/i + E(end) * (M+1)/M;
V = (i-1) / i * W + (1 + 1/M) * Bpn;
R = sqrt(R);  

MPSRF(i-(L-1)) = R;
end

B = Bpn*N;
neff = fix(nanmean(min(diag(M*N*V./B),M*N)));
if onechain && (nargout>1)
  neff=neff*3/2;
end