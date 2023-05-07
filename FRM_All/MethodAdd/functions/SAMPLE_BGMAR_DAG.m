function M = SAMPLE_BGMAR_DAG(data, nsimu, sdz, lag, ny)

%====== PRELIMINARIES;
percent_burn = 0.5;
nburn = fix(percent_burn*nsimu);        
nsave = nsimu - nburn;

D = data;
[~,nx] = size(D); 
if nargin == 4;
    ny = nx;  
end

%====== PROCESS DATA;
[Sigma_pr, Sigma_pst, nu_pr, nu_pst, Kn, nvar] = PROC_DATA(D, ny,sdz,lag);

fprintf('\n#######')
fprintf(' BG-MAR SIMULATION IN PROGRESS ')
fprintf('#########\n')

% ========= INITIALIZATION;
LogL  = zeros(nsave+1,ny);

logL  = zeros(1,ny);
for yi = 1:ny
   logL(1,yi) = LOG_SCORE(yi, [], Sigma_pr, Sigma_pst, nu_pr, nu_pst,...
       Kn, nvar, lag);
end
    
LogL(1,:)  = logL;
Nx = nx*lag;
Dag = zeros(ny,Nx);
DG  = Dag(:);
scn = 5e2;

%====================== Start Sampling ============================
tic;  
ct = 1;
for t = 1:nsimu
    for yi = 1:ny
        j = randperm(Nx,1);
        parents = Dag(yi,:);
        new_parents = parents;
        new_parents(1,j) = 1 - new_parents(1,j);
        
        xj = find(new_parents);    
        nlogL = LOG_SCORE(yi, xj, Sigma_pr, Sigma_pst, nu_pr, nu_pst,...
            Kn, nvar, lag); 
        
        R = exp(nlogL - logL(yi));
        u = rand; 
        if u < min(1,R) % accept the move:
            Dag(yi,:) = new_parents;
            logL(yi) = nlogL;  
        end
    end
    if t > nburn;
        ct = ct + 1;        
        DG = DG + Dag(:);       LogL(ct,:) = logL;
    end
    if ((scn*floor(t/scn))==t);
        fprintf('  %4.0f out of %4.0f iterations, ' , t , nsimu );
        toc;  
    end
end
Time = toc;
% ====== CONVERGENCE DIAGNOSTICS ====
fprintf('\n#######')
fprintf(' CONVERGENCE DIAGNOSTICS ')
fprintf('#########\n')

[PostG, DAG, R1, R2] = CONVERGENCE(DG, LogL, ny, Nx, lag);

M.DAG   = DAG;
M.PostG = PostG;
M.PSRF   = R1;
M.MPSRF = R2;
M.Time    = Time;