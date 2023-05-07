function [Sigma_pr, Sigma_pst, nu_pr, nu_pst, Kn, nvar]=PROC_DATA(D,...
    ny, sdz, lag)

if nargin == 3; lag = 0; end

[T,nx] = size(D);
if sdz == 1;
    D = (D - ones(T,1)*nanmean(D))./(ones(T,1)*nanstd(D));
else
    D = (D - ones(T,1)*nanmean(D));
end
DD = D';

Y  = DD(1:ny,1 + lag:end);  
if lag ~= 0;
    X1 = zeros(nx,T-lag,lag);
    X = zeros(nx*lag,T-lag);
    for j=1:nx
        for i=1:lag
            X1(j,:,lag-i+1) = DD(j,i:T-lag+i-1);
        end
    end
    for i=1:lag
        s1 = (i-1)*nx+1;                s2 = s1:s1+nx-1;
        X(s2',:) = squeeze(X1(:,:,i));
    end
    nexp = size(X,1);
else
    X = Y;
    nexp = size(X,1) - 1;   % number of explanatory variables
end

nvar = nexp + 1;
T_p = T - lag;
nu_pr = nvar + 1;
nu_pst = nu_pr + T_p;

if lag ~= 0;
    Sigma_pr = cell(1,ny);
    Sigma_pst = cell(1,ny);
    for i = 1:ny
        D = [X; Y(i,:)]'; 
        %S_hat = D'*D;
        S_hat = T_p.*nancov(D);
        Sigma_pr{i} = eye(size(S_hat,2));
        Sigma_pst{i} = (S_hat + nu_pr.*Sigma_pr{i})./(nu_pst);
    end
else
    D = X'; 
    S_hat = D'*D;
    Sigma_pr = eye(size(S_hat,2));
    Sigma_pst = (S_hat + nu_pr.*Sigma_pr)./(nu_pst);
end

Kn   = zeros(nvar,1);
for j = 1:nvar
    aux =  -0.5*(j*T_p)*log(pi);
    num = 0;         den = 0;     
    for i = 1:j
        num = num + gammaln((nu_pst + 1 - i)/2);
        den = den + gammaln((nu_pr + 1 - i)/2);
    end
    Kn(j) = aux + num - den ;
end