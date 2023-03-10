%CODE USED TO COMPUTE THE ENCODING-RETRIEVAL-SIMILARITY AS REPORTED IN THE MANUSCRIPT 
%"UNRAVELING THE SEMANTIC NATURE OF MEMORY OVER TIME" 
%by Valentina Krenz, Arjen Alink, Tobias Sommer, Benno Roozendaal, Lars Schwabe
%submitted 2022

%script drafted by Arjen Alink, adapted by Valentina Krenz

%CORRELATES ACTIVIATION PATTERNS OF EACH ITEM AT ENCODING WITH
%ACTIVATION PATTERNS 


%% order of timages per condition

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


%% PREPARE AND READ IN DATA 
addpath NiftiTools/

betaDir= 'YOURPATH\tImages\'; %change to yourpath
ROIdir= 'YOURPATH\code\ROIs\mainROIs\';  %change to yourpath

%SubFolder='/oneGLMforRSA/';
SJfolders=dir([betaDir 'sj*']);
nSjs=numel(SJfolders);

nrBetaMaps=420; 
RoiNames=dir([ROIdir  '*.nii']);
nRois=numel(RoiNames);

ROIdata=struct();
ROInames=cell(nRois,1);

%% RSM indices per stimulus type

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

for r=1:nRois
    ROI=load_nii([ROIdir RoiNames(r).name]);
    ROIdata.([RoiNames(r).name(1:end-4) '_Inds'])=find(ROI.img);
    ROIdata.([RoiNames(r).name(1:end-4) '_RSMs'])=zeros(numel(SJfolders),nrBetaMaps,nrBetaMaps,'single');
    ROInames{r}=RoiNames(r).name(1:end-4);
end

for sj=1:nSjs   
    sj
    tic
    betaPath=[betaDir,SJfolders(sj).name];
    betaFiles=dir([betaPath, 'spm*.nii']);
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

AllContrastNames=fields(Contrasts);
Means=zeros(numel(AllContrastNames),1);
OPinTable=table();
        
for roi=1:nRois
    roiRSMs=ROIdata.([ROInames{roi} '_RSMs']);
    for ctr=1:numel(AllContrastNames)      
        for stim=1:30
            OPinTable.([ AllContrastNames{ctr} '_' num2str(stim) 'x' ROInames{roi} '_allEncRuns'])=atanh(mean(roiRSMs(:,Contrasts.(AllContrastNames{ctr})==1&ImageInds==stim),2));    
        end
    end
end

writetable(OPinTable,'RSA_MemoryReinstatement_mainROIs.csv')


