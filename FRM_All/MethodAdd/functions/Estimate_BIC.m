function bic = Estimate_BIC(data, min_lag, max_lag, ny)

D = data;
[T,nx] = size(D);
if nargin == 3;  ny = nx; end
D = (D - ones(T,1)*nanmean(D))./(ones(T,1)*nanstd(D));
D = D';
lags = min_lag : max_lag;
Bic = zeros(1,numel(lags));
for l = 1:numel(lags)
    p = lags(l);
    X1 = zeros(nx,T-p,p);
    for j=1:nx
        for i=1:p
            X1(j,:,p-i+1) = D(j,i:T-p+i-1);
        end
    end
    X = zeros(nx*p,T-p);
    for i=1:p
        s1 = (i-1)*nx+1;
        s2 = s1:s1+nx-1;
        X(s2',:) = squeeze(X1(:,:,i));
    end
    X = X';
    Y  = D(1:ny,1+p:end)'; 
    
    Beta = X\Y;
    Yhat = X*Beta;
    U = Y - Yhat; 
    Sigma = cov(U);
    p_density = mvnpdf(Yhat,Y,Sigma);
    p_density(p_density==0) = 1;
    LPS = sum(log(p_density));
    Bic(i) = -2*LPS + log(T)*nx*ny*p;
end
[~,bic] = min(Bic);