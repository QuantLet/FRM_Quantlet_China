function [X, B_p, B_0] = SIMVAR(N, T, lag)

T0 = 5e2;  n = 5;    k = round(N/n);
T = T + T0;

W = [-0.8;0.5;-0.5;0.6;0.8;-0.9;0.6;0.7;-0.6;0.5;-0.5;0.7;0.6;0.5;...
-0.6;-0.4;-0.4;0.4;-0.4; 0.5];

B0 = zeros(5);  B0(1,3) = W(1);  
B0(4,2) = W(2);   B0(4,5) = W(3);  
B1 = zeros(5);  B1(1:3,1) = W(6:8);  B1(5,2) = W(9); B1(2,3)=W(10);
B1(3,3)=W(11);   B1(4,4) = W(12);  B1(5,5) = W(13);  B1(4,3) = W(20);
B2 = zeros(5);  B2(4,1) = W(14);  B2(2,1) = W(15);  B2(3,5) = W(16);
B2(5,3) = W(17);  
B3 = zeros(5);  B3(3,4) = W(18);  B3(5,2) = W(19); 

U  = gennormrnd(0,1,N,T);
B_0 = kron(eye(k),B0);
B_1 = kron(eye(k),B1);
B_2 = kron(eye(k),B2);
B_3 = kron(eye(k),B3);
X  = gennormrnd(0,1,N,T);

if lag == 0;
    for t=lag+1:T
        X(:,t) = B_0*X(:,t) + U(:,t);
    end    
    X = X(:,T0+1:end)';
    B_p = 0.*B_1;    

elseif lag == 1;
    for t=lag+1:T
        X(:,t) = B_0*X(:,t) + B_1*X(:,t-1) + U(:,t);
    end    
    X = X(:,T0+1:end)';
    B_p = B_1;

elseif lag == 2;
    for t=lag+1:T
        X(:,t) = B_0*X(:,t) + B_1*X(:,t-1) + B_2*X(:,t-2) + U(:,t);
    end  
    X = X(:,T0+1:end)';
    B_p = [B_1,B_2];
    
elseif lag == 3;
    for t=lag+1:T
        X(:,t) = B_0*X(:,t) + B_1*X(:,t-1) + B_2*X(:,t-2) + ...
            B_3*X(:,t-3) + U(:,t);
    end  
    X = X(:,T0+1:end)';
    B_p = [B_1,B_2,B_3];
end

function r = gennormrnd(mu,sigma,m,n)
r = randn(m,n) .* sigma + mu;