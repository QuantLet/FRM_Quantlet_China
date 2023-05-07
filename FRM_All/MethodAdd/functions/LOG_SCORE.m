function LS = LOG_SCORE(yi, xj, Sigma_pr, Sigma_pst, nu_pr, nu_pst, Kn,...
    nvar, lag)

if lag == 0;
    Sig_pr = Sigma_pr;
    Sig_pst = Sigma_pst;
    jnt = [xj yi];
else
    Sig_pr = Sigma_pr{yi};
    Sig_pst = Sigma_pst{yi};
    jnt = [xj nvar];  % joint of xj and yi
end

n_num = length(jnt);       
n_den = n_num - 1;

Log_num = Kn(n_num) + 0.5*nu_pr*log(det(Sig_pr(jnt,jnt))) - 0.5*...
    nu_pst*log(det(Sig_pst(jnt,jnt)));
if n_den ~= 0;
    Log_den = Kn(n_den) + 0.5*nu_pr*log(det(Sig_pr(xj,xj))) - 0.5*...
        nu_pst*log(det(Sig_pst(xj,xj)));
else
    Log_den = 0;
end

LS = real(Log_num) - real(Log_den);