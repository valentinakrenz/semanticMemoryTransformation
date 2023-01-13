%% AA: Code that, retrieves the mean pattern correlation for each "cell type"
%, which at the moment is 0, 0.5 and 0.8. These are stored in
%MeanCorrsPerCellType

%MODEL-BASED RSA FOR THE PAPER "UNRAVELING THE SEMANTIC NATURE OF MEMORY
%TRANSFORMATION OVER TIME"
%submitted 2022

%script drafted by Arjen Alink, adapted by Valentina Krenz

%COMPARES ACTIVIATION PATTERNS OF EACH ITEM AT RECOGNITION TESTING WITH
%THREE CONCEPTUAL MODELS

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

%% parameters

dataFolder='YOURPATH\tImages\'; % change to your path
nSjs=52; %number of subjects
addpath NiftiTools\
load('YOURPATH\data\neuro\runDat.mat') %change to your path  %data which item was in which run

SJfolders=dir([dataFolder 'sj*']);
ROIfolder='YOURPATH\code\ROIs\mainROIs\'; % change to your path
RoiImNames=dir([ROIfolder '*.nii']);

%% DEFINE CONDITIONN INDICES
NegInds=[1:30,[1:30]+60, [1:30]+120,[1:30]+180];
NeutInds=[31:60,[31:60]+60, [31:60]+120,[31:60]+180];
OldInds=1:60;
PercInds=61:120;
SemInds=121:180;
NewInds=181:240;

%% DEFINE MODEL RDMS

% NEGATIVE ITEMS

% model 1: old items are distinct to all lures
OldAreSim_RestDiff_Neg=NaN(240);
OldAreSim_RestDiff_Neg(intersect(OldInds,NegInds),intersect(OldInds,NegInds))=.8;
OldAreSim_RestDiff_Neg(intersect(OldInds,NegInds),intersect(SemInds,NegInds))=0;
OldAreSim_RestDiff_Neg(intersect(OldInds,NegInds),intersect(PercInds,NegInds))=0;
OldAreSim_RestDiff_Neg(intersect(OldInds,NegInds),intersect(NewInds,NegInds))=0;
OldAreSim_RestDiff_Neg(intersect(SemInds,NegInds),intersect(OldInds,NegInds))=0;
OldAreSim_RestDiff_Neg(intersect(PercInds,NegInds),intersect(OldInds,NegInds))=0;
OldAreSim_RestDiff_Neg(intersect(NewInds,NegInds),intersect(OldInds,NegInds))=0;
% remove the diagonal
OldAreSim_RestDiff_Neg(eye(size(OldAreSim_RestDiff_Neg))==1)=NaN;    
figure;imagesc(OldAreSim_RestDiff_Neg);

% model 2: old items and semantically related items are similar
OldAreSimToSem_Neg=NaN(240);
OldAreSimToSem_Neg(intersect(OldInds,NegInds),intersect(OldInds,NegInds))=.8; %old-old --> similar
OldAreSimToSem_Neg(intersect(OldInds,NegInds),intersect(SemInds,NegInds))= .5;
OldAreSimToSem_Neg(intersect(SemInds,NegInds),intersect(OldInds,NegInds))= .5;
OldAreSimToSem_Neg(intersect(OldInds,NegInds),intersect(PercInds,NegInds))= 0;
OldAreSimToSem_Neg(intersect(OldInds,NegInds),intersect(NewInds,NegInds))= 0;
OldAreSimToSem_Neg(intersect(PercInds,NegInds),intersect(OldInds,NegInds))= 0;
OldAreSimToSem_Neg(intersect(NewInds,NegInds),intersect(OldInds,NegInds))= 0;
% remove the diagonal
OldAreSimToSem_Neg(eye(size(OldAreSimToSem_Neg))==1)=NaN;
figure;imagesc(OldAreSimToSem_Neg);

% model 3: old items and perceptually related items are similar
OldAreSimToPerc_Neg=NaN(240);
OldAreSimToPerc_Neg(intersect(OldInds,NegInds),intersect(OldInds,NegInds))=.8; %old-old --> similar
OldAreSimToPerc_Neg(intersect(OldInds,NegInds),intersect(PercInds,NegInds))=.5; %old-sem --> comparable
OldAreSimToPerc_Neg(intersect(PercInds,NegInds),intersect(OldInds,NegInds))=.5; %sem-old --> comparable
OldAreSimToPerc_Neg(intersect(OldInds,NegInds),intersect(SemInds,NegInds))= 0;
OldAreSimToPerc_Neg(intersect(OldInds,NegInds),intersect(NewInds,NegInds))= 0;
OldAreSimToPerc_Neg(intersect(SemInds,NegInds),intersect(OldInds,NegInds))= 0;
OldAreSimToPerc_Neg(intersect(NewInds,NegInds),intersect(OldInds,NegInds))= 0;
% remove the diagonal
OldAreSimToPerc_Neg(eye(size(OldAreSimToPerc_Neg))==1)=NaN;
figure;imagesc(OldAreSimToPerc_Neg);


% NEUTRAL ITEMS

% model 1: old items are distinct to all lures
OldAreSim_RestDiff_Neut=NaN(240);
OldAreSim_RestDiff_Neut(intersect(OldInds,NeutInds),intersect(OldInds,NeutInds))=.8;
OldAreSim_RestDiff_Neut(intersect(OldInds,NegInds),intersect(SemInds,NegInds))=0;
OldAreSim_RestDiff_Neut(intersect(OldInds,NegInds),intersect(PercInds,NegInds))=0;
OldAreSim_RestDiff_Neut(intersect(OldInds,NegInds),intersect(NewInds,NegInds))=0;
OldAreSim_RestDiff_Neut(intersect(SemInds,NegInds),intersect(OldInds,NegInds))=0;
OldAreSim_RestDiff_Neut(intersect(PercInds,NegInds),intersect(OldInds,NegInds))=0;
OldAreSim_RestDiff_Neut(intersect(NewInds,NegInds),intersect(OldInds,NegInds))=0;
% remove the diagonal
OldAreSim_RestDiff_Neut(eye(size(OldAreSim_RestDiff_Neut))==1)=NaN;
figure;imagesc(OldAreSim_RestDiff_Neut);

% model 2: old items and semantically related items are similar
OldAreSimToSem_Neut=NaN(240);
OldAreSimToSem_Neut(intersect(OldInds,NeutInds),intersect(OldInds,NeutInds))=.8; %fill old-old for neut --> similar
OldAreSimToSem_Neut(intersect(OldInds,NeutInds),intersect(SemInds,NeutInds))=.5; %fill old-sem for neut --> comparable
OldAreSimToSem_Neut(intersect(SemInds,NeutInds),intersect(OldInds,NeutInds))=.5; %fill sem-old for neut --> comparable
OldAreSimToSem_Neut(intersect(OldInds,NeutInds),intersect(PercInds,NeutInds))= 0;
OldAreSimToSem_Neut(intersect(OldInds,NeutInds),intersect(NewInds,NeutInds))= 0;
OldAreSimToSem_Neut(intersect(PercInds,NeutInds),intersect(OldInds,NeutInds))= 0;
OldAreSimToSem_Neut(intersect(NewInds,NeutInds),intersect(OldInds,NeutInds))= 0;
% remove the diagonal
OldAreSimToSem_Neut(eye(size(OldAreSimToSem_Neut))==1)=NaN;
figure;imagesc(OldAreSimToSem_Neut);

% model 3: old items and perceptually related items are similar
OldAreSimToPerc_Neut=NaN(240);%fill all items with NaNs
OldAreSimToPerc_Neut(intersect(OldInds,NeutInds),intersect(OldInds,NeutInds))=.8; %old-old for --> similar
OldAreSimToPerc_Neut(intersect(OldInds,NeutInds),intersect(PercInds,NeutInds))=.5; %old-per for with --> comparable
OldAreSimToPerc_Neut(intersect(PercInds,NeutInds),intersect(OldInds,NeutInds))=.5; %per-old for with --> comparable
OldAreSimToPerc_Neut(intersect(OldInds,NeutInds),intersect(SemInds,NeutInds))= 0;
OldAreSimToPerc_Neut(intersect(OldInds,NeutInds),intersect(NewInds,NeutInds))= 0;
OldAreSimToPerc_Neut(intersect(SemInds,NeutInds),intersect(OldInds,NeutInds))= 0;
OldAreSimToPerc_Neut(intersect(NewInds,NeutInds),intersect(OldInds,NeutInds))= 0;
% remove the diagonal
OldAreSimToPerc_Neut(eye(size(OldAreSimToPerc_Neut))==1)=NaN;
figure;imagesc(OldAreSimToPerc_Neut);

%% save all contrasts in a cell mat, and save the names
AllModelNames={'oldAreSimRestDiffxneg','oldAreSimRestDiffxneu','OldAreSimToPercxneg','OldAreSimToPercxneu','OldAreSimToSemxneg','OldAreSimToSemxneu'};
AllModels={OldAreSim_RestDiff_Neg,OldAreSim_RestDiff_Neut,OldAreSimToPerc_Neg,OldAreSimToPerc_Neut,OldAreSimToSem_Neg,OldAreSimToSem_Neut};

figure;
for i=1:numel(AllModelNames)
    subplot(2,4,i);
    imagesc(AllModels{i});
    title(AllModelNames{i});
end

OPcollumNames={};
count=1;
for r=1:numel(RoiImNames)
    for i=1:numel(AllModelNames)
        OPcollumNames{count}=[RoiImNames(r).name(1:end-4) 'x' AllModelNames{i}];
        count=count+1;
    end
end

%% PREPARE RESULTS TABLE
OPsimVals=zeros(nSjs,numel( OPcollumNames));
DataTable=array2table(OPsimVals);
DataTable.Properties.VariableNames=OPcollumNames;

for sj= 1:nSjs
    
    
    %% compute runEffRDM
    SjNumber=str2num(SJfolders(sj).name(8:10));
    SjTrialTimingDat=runDat.(['vp' num2str(SjNumber)]);
    RunEffRSM=zeros(420);
    for tr1=1:240
        for tr2=1:240
            trialRunCode=(SjTrialTimingDat(tr1,2)*10)+...
                SjTrialTimingDat(tr2,2);
            RunEffRSM(tr1+180,tr2+180)=trialRunCode;
        end
    end
    RunEffRSM(eye(420)==1)=0;
    AllRetrRunCombos=unique(RunEffRSM);
    AllRetrRunCombos=AllRetrRunCombos(AllRetrRunCombos>0);
    RunEffRSM=RunEffRSM(181:end,181:end);
    %%
    
    sj
    tic
    betaPath=[dataFolder,SJfolders(sj).name];
    betaFiles=dir([betaPath, '*.nii']);
    for b=181:420
        betaDat=load_nii([betaPath,betaFiles(b).name]);
        if b==181
            BetaMaps=zeros([240,size(betaDat.img)],'single');
        end
        BetaMaps(b-180,:,:,:)= betaDat.img;
    end
    %%
    SelDat=BetaMaps;
    lowDiagSel=tril(ones(240),-1)==1;
    for roi=1:numel(RoiImNames)
        roi
        roiMap=load_nii([ROIfolder, RoiImNames(roi).name]);
        roiInds=find(roiMap.img(:)==1);
        
        ROI_Patts=SelDat(:,roiInds);
        
        RSM=corr(ROI_Patts','rows','complete');
        
        %% removing retrieval run effect
        for runcomp=1:numel(AllRetrRunCombos)
            RSM(RunEffRSM==AllRetrRunCombos(runcomp))=RSM(RunEffRSM==AllRetrRunCombos(runcomp))-...
                mean(RSM(RunEffRSM==AllRetrRunCombos(runcomp)));
        end
        
        for mdl=1:numel(AllModels)
                             
            DataTable{sj, mdl+((roi-1)*numel(AllModels))}=atanh(corr(RSM( lowDiagSel),AllModels{mdl}( lowDiagSel),'rows','complete','type','spearman'));
        end
        
    end
    
end
writetable(DataTable,'modelComparisonRSA_mainROIs.csv');



