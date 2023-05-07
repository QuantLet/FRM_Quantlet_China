
%% Clean up
clc
clear

%% Libraries
addpath('MFEToolbox/multivariate')
addpath('MFEToolbox/univariate')
addpath('MFEToolbox/distributions')
addpath('MFEToolbox/utility')

%% Parameters
kratio        =  0.08; %capital ratio required
cevent        = log(1-0.2); %market return shock
horizon       = 120; %number of days horizon
s1            = 100; %number of simulations

%% Options (for optimization)
options_std   = optimset('Display','off','Algorithm','active-set','MaxIter',1);
options_first = optimset('Display','off','Algorithm','active-set');

options = options_first;

%% Load data
data_table = readtable('input_data.csv');

dates = datenum(data_table.date);
data = table2array(data_table(:,2:end));

%% Count number of firms in dataset
nf = (size(data,2)-1)/4;

%% Define sample for which SRISKv2 is computed
smpl = [];
for y=2010:2019
    for m=1:12
        smpl = [ smpl;  datenum( sprintf('%d%0.2d%d',y,m,eomday(y,m)) , 'yyyymmdd' ) ];
    end
end

%% Compute SRISKv2 and SRISK
sriskv2 = zeros(size(smpl,1),nf);

for s=1:length(smpl)
    
    fprintf('%s\n',datestr(smpl(s),'yyyy-mm-dd'));
    
    t = find(dates <= smpl(s), 1, 'last');
    
    ym = data(1:t,1)/100; %market returns. Divide them by 100 if returns are introduced in percentage points
    
    yi = zeros(t,nf);
    mvt = zeros(t,nf);
    dbt = zeros(t,nf);
    sigma_i = zeros(t,nf);
    rho = zeros(t,nf);
    param = zeros(nf,11);
    eps_i = zeros(t,nf);
    
    for i = 1:nf
        yi(:,i) = data(1:t,2+(i-1)*4)/ 100; %return of firm i
        mvt(:,i) = data(1:t,3+(i-1)*4); %market value of equity of firm i
        dbt(:,i) = data(1:t,5+(i-1)*4)-data(1:t,4+(i-1)*4); %book value of debt of firm i
        
        y = [ym yi(:,i)];
        
        try
            
            if isempty(options.MaxIter)
                [ param(i,:) , ~ , Ht ] = dcc(y,[],1,0,1,1,1,1,[],[],[],[],options); %Estimates of GARCH-DCC parameters (param) and covariance matrix (Ht)
            else
                try
                    [ param(i,:) , ~ , Ht ] = dcc(y,[],1,0,1,1,1,1,[],[],[],param,options);
                catch
                    fprintf('S')
                    options = optimset(options,'MaxIter',[]);
                    [ param(i,:) , ~ , Ht ] = dcc(y,[],1,0,1,1,1,1,[],[],[],[],options);
                end
            end
            
            if i == 1
                sigma_m = squeeze( sqrt(Ht(1,1,:)) ); %daily market return volatility estimate
            end
            
            sigma_i(:,i) = squeeze( sqrt(Ht(2,2,:)) ); %daily firm i return volatility estimate
            rho(:,i) = squeeze( Ht(1,2,:) ./ sqrt( Ht(1,1,:) .* Ht(2,2,:) ) ); %daily correlation of firm i return with market return
            
        catch
            fprintf('X')
            if i == 1
                sigma_m = std(y(end-30:end,1))*ones(t,1);
            end
            sigma_i(:,i) = std(y(end-30:end,2))*ones(t,1);
            rho(:,1)     = corr(y(end-30:end,1),y(end-30:end,2))*ones(t,1);
        end
        
        if i == 1
            eps_m = ym ./ sigma_m; %Estimate of market return innovation
        end
        
        eps_i(:,i) = (yi(:,i) ./ sigma_i(:,i) - rho(:,i).*eps_m)./(1-rho(:,i).^2).^0.5; %Estimate of firm i return innovation
        
    end
    
    eps = [eps_m eps_i];
    
    Ret_m_sim = zeros(s1,1);
    Ret_i_sim = zeros(s1,nf);
    shortfall_sriskv2_i = zeros(s1,nf);
    shortfall_srisk_i = zeros(s1,nf);
    
    for i = 1:s1
        
        sigma_m_sim = sigma_m(end,1); %starting value of market return volatility
        sigma_i_sim = sigma_i(end,:); %starting value of firm i return volatility
        rho_sim = rho(end,:); %starting value of correlation of firm i return with market return
        
        sim_ret_m = zeros(horizon,1);
        sim_ret_i = zeros(horizon,nf);
        
        for j = 1:horizon
            ret_inov = eps(ceil(rand()*size(eps,1)),:); %random set of market return and firms innovations selected for a simulated day
            
            sim_ret_m(j,1) = ret_inov(1,1)*sigma_m_sim; %simulated market return
            sigma_m_sim = (param(1,1)+param(1,2)*sim_ret_m(j,1)^2+param(1,3)*(min(sim_ret_m(j,1),0))^2+param(1,4)*sigma_m_sim^2)^0.5; %next value of market return volatility
            
            for k = 1:nf
                sim_ret_i(j,k)=sigma_i_sim(1,k)*(((1-rho_sim(1,k)^2)^0.5)*ret_inov(1,1+k)+rho_sim(1,k)*ret_inov(1,1)); %simulated firm i return
                std_ret_i = ((1-rho_sim(1,k)^2)^0.5)*ret_inov(1,1+k)+rho_sim(1,k)*ret_inov(1,1); %normalized firm i return
                sigma_i_sim(1,k) = (param(k,5)+param(k,6)*sim_ret_i(j,k)^2+param(k,7)*(min(sim_ret_i(j,k),0))^2+param(k,8)*sigma_i_sim(1,k)^2)^0.5; %next value of firm i return volatility
                rho_sim(1,k) = ((1-param(k,10)-param(k,11))*param(k,9)+param(k,10)*ret_inov(1,1)*std_ret_i+param(k,11)*rho_sim(1,k))/(((1-param(k,10)*(1-ret_inov(1,1)^2))^0.5)*((1-param(k,10)*(1-std_ret_i^2))^0.5)); %next value of correlation of market return with firm i return
            end
            
        end
        
        Ret_m_sim(i,1) = exp(sum(sim_ret_m))-1; %cumulative market return over simulated horizon
        
        for k = 1:nf
            Ret_i_sim(i,k) = exp(sum(sim_ret_i(:,k)))-1; %cumulative firm i return over simulated horizon
            shortfall_sriskv2_i(i,k) = max(0,kratio*dbt(end,k)-(1-kratio)*mvt(end,k)*(1+Ret_i_sim(i,k))); %Capital shortfall at the end of the simulated horizon
        end
        
    end
    
    for k = 1:nf
        sriskv2(s,k) = mean(shortfall_sriskv2_i(Ret_m_sim<cevent,k)); %SRISKv2 for firm i
    end
    
end
fprintf('Done!\n');

output_data = table(datestr(smpl),sriskv2);
writetable(output_data,'output_data.csv');

