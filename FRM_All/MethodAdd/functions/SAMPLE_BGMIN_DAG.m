function M = SAMPLE_BGMIN_DAG(data, nsimu, sdz, ny)

%====== PRELIMINARIES;
percent_burn = 0.5;
nburn = fix(percent_burn*nsimu);        
nsave = nsimu - nburn;

D = data;
[~,nx] = size(D); 
if nargin == 3;
    ny = nx;  
end
lag = 0;
D = detrend(D);
%====== PROCESS DATA;
[Sigma_pr, Sigma_pst, nu_pr, nu_pst, Kn, nvar] = PROC_DATA(D, ny, sdz);

fprintf('\n#######')
fprintf(' BG-MIN SIMULATION IN PROGRESS ')
fprintf('#########\n')

% ========= INITIALIZATION;
Vx = cell(1,1);       
for j = 1:ny
    nbrs = 1:ny;     nbrs(j)=[];
    Vx{j,1} =  nbrs;
end

LogL  = zeros(nsave+1,ny);

logL  = zeros(1,ny);
for yi = 1:ny
   logL(1,yi) = LOG_SCORE(yi, [], Sigma_pr, Sigma_pst, nu_pr, nu_pst,...
       Kn, nvar, lag);
end
    
LogL(1,:)  = logL;
Nx = ny-1;
Dag = zeros(ny);
DG  = Dag(:);
scn = 5e2;

%====================== Start Sampling ============================
tic;  
ct = 1;
for t = 1:nsimu
    py = randperm(ny);
    for i = 1:ny
        yi = py(i);
        cand_pa = Vx{yi,1};   % candidate parents
        idj = randperm(Nx,1);
        xi = cand_pa(idj);
        
        Dag_n = Dag;
        e_yx = Dag_n(yi,xi);
        e_xy = Dag_n(xi,yi);
        
        % first step verification
        if e_yx == 0 && e_xy == 1;
            Dag_n(yi,xi) = 1;  Dag_n(xi,yi) = 0;     % Reverse
            rev = 1;
        else
            Dag_n(yi,xi) = 1 - Dag_n(yi,xi);         % Add or delete
            rev = 0;
        end        
        
        % second step verification
        if graphisdag(sparse(Dag_n)) == 1;
            nlogL = logL;
            xj = find(Dag_n(yi,:));
            nlogL(yi) = LOG_SCORE(yi, xj, Sigma_pr, Sigma_pst, nu_pr,...
                nu_pst, Kn, nvar, lag);
            if rev == 1;
                yj = find(Dag_n(xi,:));
                nlogL(xi) = LOG_SCORE(xi, yj, Sigma_pr, Sigma_pst, ...
                    nu_pr, nu_pst, Kn, nvar, lag);
            end
            
            R = exp(sum(nlogL) - sum(logL));
            u = rand; 
            if u < min(1,R) % accept the move:
                Dag = Dag_n;
                logL = nlogL;
            end
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

[PostG, DAG, R1] = CONVERGENCE(DG, LogL, ny, ny, lag);

M.DAG   = DAG;
M.PostG = PostG;
M.PSRF   = R1;
M.Time    = Time;