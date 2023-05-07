% This code implements the Financial Application in Section 6 in
% Ahelegbey D. F., Billio, M., Casarin, R. (2014). 
% "Bayesian Graphical Models for Structural Vector Autoregressive 
% Processes",  Journal of Applied Econometrics, forthcoming
%========================================================================

close all; clc; clear;

% Add path of functions, data and figures
addpath('functions');    addpath('data');    addpath('figures');
sRoot = '\Users\ruting\Documents\macbook\PcBack\FRM_Quantlet\FRM_All\MethodAdd';

date_end_source = '20230306';

%-----------------------------LOAD DATA-----------------------------------
% Load monthly data of Supersectors, Euro-Stoxx600 (1/31/2001 - 8/30/2013)
% DATA = importdata('Stoxx600_Data.txt');   
DATA = importdata(strcat('Return_',date_end_source,'.csv')); 

% Vars = DATA.textdata(1, 3:end);
% Vars = cellfun(@(x) x(3:9),Vars,'UniformOutput',false);
Vars = 1:50;
Vars =(arrayfun(@(x) num2str(x),Vars,'UniformOutput',false))';
%-------------------------LOAD VARIABLES----------------------------------
% Load List of variables
% filename = 'Stoxx_Variables.txt';     
% fileID = fopen(filename,'r');
% Vars = textscan(fileID, '%s%s%s', 'Delimiter', '\t');
% fclose(fileID);
% ShortID = Vars{1};       % Short ID of variables
% 
% 
% clear filename fileID ans

Data = DATA.data(:, 2:end);     

%----------------------------PRELIMINARIES--------------------------------
nsimu  = 4e4;       % number of Simulation
sdz = 1;            % 1. Standardize dataset  0. Demean dataset
lag = 1;            % lag order

nStart = 200;
Output = zeros(size(Data,1), 2);
Output(:, 1) = DATA.data(:, 1);     

for i = nStart : size(Data,1)
    Temp = Data(i - nStart + 1 : i, :);
    sumcol = sum(Temp,1);
    MAR = SAMPLE_BGMAR_DAG(Temp(:,sumcol ~= 0), nsimu, sdz, lag);
    Output(i, 2) = sum(MAR.DAG, 'all')/ (sum(sumcol ~= 0))^2 * 100;
end
Output(1:nStart-1, :) = [];
Output2 = array2table(Output, 'VariableNames',{'Date','DAG'});

sDir_Excel = ['BGVAR_', date_end_source,'.xlsx'];
writetable(Output2,sDir_Excel,'Sheet',1)

save(['BGVAR_', date_end_source,'.mat'],'Output2') 
%-------------------SAMPLING MAR STRUCTURE -------------------------------
 