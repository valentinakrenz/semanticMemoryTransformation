%% MEMORY REINSTATEMENT ANALYSIS
% CORRELATES ACTIVIATION PATTERNS OF EACH ITEM AT ENCODING WITH
% ACTIVATION PATTERNS OF OLD ITEMS AT MEMORY TESTING

% CODE FOR THE MANUSCRIPT "UNRAVELING THE SEMANTIC NATURE OF MEMORY OVER TIME" 
% by Valentina Krenz, Arjen Alink, Tobias Sommer, Benno Roozendaal, Lars Schwabe
% submitted 2022

%% requirements: 
% NIfTI_tools: https://www.mathworks.com/matlabcentral/fileexchange/8797-tools-for-nifti-and-analyze-image
% Statistics and Machine Learning Toolbox: https://de.mathworks.com/products/statistics.html

%% path settings 
% addpath to your Nifti tools folder
addpath NiftiTools/
% path to parentfolder with your timages
betaDir='/data/timages/'; 
% naming convention of subfolders with subject data
SJfolders=dir([betaDir 'MemTrans*']);
% number of subjects
nSjs=numel(SJfolders);
% path to subsubfoder to beta or t images of each subject
SubFolder='/GLM/';
% number of beta or t images
nrBetaMaps=420; 
% path to your ROIs
ROIdir='/data/ROIs/HC_longaxis/';
% get ROI names
RoiNames=dir([ROIdir  '*.nii']);

%% RSM indices per stimulus type
% 1:30 encoding1 negative
% 31:60 encoding1 neutral

% 61:90 encoding2 negative
% 91:120 encoding2 neutral

% 121:150 encoding3 negative
% 151:180 encoding3 neutral

% 181:210 old negative
% 211:240 old neutral

% 241:270 perc negative
% 271:300 perc neutral

% 301:330 sem negative
% 331:360 sem neutral

% 361:390 unrelared negative
% 391:420 unrelated neutral

%% define condition indices
Contrasts=struct();

Contrasts.ERSNeg=NaN(nrBetaMaps);%neg
Contrasts.ERSNeu=NaN(nrBetaMaps);%neut

Contrasts.ESSNeg=NaN(nrBetaMaps);%neg
Contrasts.ESSNeu=NaN(nrBetaMaps);%neut

Contrasts.EPSNeg=NaN(nrBetaMaps);%neg
Contrasts.EPSNeu=NaN(nrBetaMaps);%neut

ImageInds=NaN(420);
ImageInds(1:180,:)=repmat(repmat([1:30 1:30]',3,1),1,420);

for EncIm=1:30

    % get encoding activity of one item over all encoding runs
    ImInds_Neg=[EncIm EncIm+60 EncIm+120];
    ImInds_Neu=[EncIm+30 EncIm+90 EncIm+150];
    
%   % change if you want run 1
%     ImInds_Neg=EncIm; % run 1 neg
%     ImInds_Neu=EncIm+30; % run 1 neut
    
%   % change if you want only run 2
%     ImInds_Neg=EncIm+60; % run 2 neg
%     ImInds_Neu=EncIm+90; % run 2 neut

%   % change if you want run 3
%     ImInds_Neg=EncIm+120; % run 3 neg
%     ImInds_Neu=EncIm+150; % run 3 neut

    Contrasts.ERSNeg(ImInds_Neg,EncIm+180)=1;%Encoding-Retrieval-Similarity neg
    Contrasts.ERSNeu(ImInds_Neu,EncIm+210)=1;%Encoding-Retrieval-Similarity neut

    Contrasts.EPSNeg(ImInds_Neg,EncIm+240)=1;%Reinstatement by perceptually related lures neg
    Contrasts.EPSNeu(ImInds_Neg,EncIm+270)=1;%Reinstatement by perceptually related lures neut
    
    Contrasts.ESSNeg(ImInds_Neg,EncIm+300)=1;%Reinstatement by semantically related lures neg 
    Contrasts.ESSNeu(ImInds_Neg,EncIm+330)=1;%Reinstatement by semantically related lures neut
end

%% prepare struct to store RSM for each ROI
% get number of ROIs
nRois=numel(RoiNames);
% initiate ROInames cell
ROInames=cell(nRois,1);
% initiate struct for RSM data per subject ROI and beta image
ROIdata=struct();
for r=1:nRois
    ROI=load_nii([ROIdir RoiNames(r).name]);
    ROIdata.([RoiNames(r).name(1:end-4) '_Inds'])=find(ROI.img);
    ROIdata.([RoiNames(r).name(1:end-4) '_RSMs'])=zeros(numel(SJfolders),nrBetaMaps,nrBetaMaps,'single');
    ROInames{r}=RoiNames(r).name(1:end-4);
end

%% read in beta images and compute RSMs for each ROI
for sj=1:nSjs   
    sj
    tic
    betaPath=[betaDir,SJfolders(sj).name];
    betaFiles=dir([betaPath, 'spmT*.nii']); %take only t images %change this if you want to analyze beta image
    for b=1:numel(betaFiles)
        betaDat=load_nii([betaPath,betaFiles(b).name]);
        if b==1
            BetaMaps=zeros([numel(betaFiles),size(betaDat.img)],'single');
        end
        BetaMaps(b,:,:,:)= betaDat.img;
    end     
    for r=1:nRois
        RoiBetas=BetaMaps(:,ROIdata.([RoiNames(r).name(1:end-4) '_Inds']));
        RoiBetas(:,isnan(mean(RoiBetas,1)))=[]; %remove NaN voxels 
        ROIdata.([RoiNames(r).name(1:end-4) '_RSMs'])(sj,:,:)=corr(squeeze(RoiBetas)');   
    end
    toc
end

%% prepare output table
AllContrastNames=fields(Contrasts);
Means=zeros(numel(AllContrastNames),1);
OPinTable=table();

%% fill output table with Fisher z-transformed data of each sj, ROI and stimulus
for roi=1:nRois
    roiRSMs=ROIdata.([ROInames{roi} '_RSMs']);
    for ctr=1:numel(AllContrastNames)      
        for stim=1:30
            OPinTable.([ AllContrastNames{ctr} '_' num2str(stim) 'x' ROInames{roi} '_allEncRuns'])=atanh(mean(roiRSMs(:,Contrasts.(AllContrastNames{ctr})==1&ImageInds==stim),2));    
        end
    end
end

%% export results
writetable(OPinTable,'RSA_MemoryReinstatement_mainROIs.csv')


