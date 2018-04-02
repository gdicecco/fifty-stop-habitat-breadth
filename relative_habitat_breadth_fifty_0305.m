% FILENAME: relative_habitat_breadth_fifty_0305.m
% AUTHOR: ALLEN HURLBERT
% CREATED: 27.VII.05
% MODIFIED: 03.VIII.06
% PURPOSE: INPUT DATA FILE WITH 54 COLUMNS--STATE, ROUTE ID, YEAR, SPECIES CODE, AND SPECIES ABUNDANCE
%          AGGREGATED OVER FIVE GROUPS OF TEN POINT COUNT STOPS.
%          FOR EACH ROUTE, CALCULATE MORISITA'S INDEX OF AGGREGATION, Im, FOR EACH SPECIES.
%          RANK SPECIES BY Im VALUES WITHIN EACH ROUTE, AND STORE STANDARDIZED RANK (Im RANK/# SPP).
%          FOR EACH SPECIES, CALCULATE MEAN STANDARDIZED Im RANK OVER ALL THE ROUTES ON WHICH IT OCCURS.

%          THIS FILE DIFFERS FROM 'relative_habitat_breadth_ten.m' IN THAT
%          IT USES 3 YRS (2003,2004,2005) WORTH OF DATA. MEAN VALUES ARE
%          CALCULATED FOR EACH ROUTE ACROSS YEARS, AND THEN A SPECIES-
%          LEVEL MEAN IS CALCULATED BASED ON ROUTE MEANS.

data = readtable('2010_fiftystopdata.csv');
stateroute = data{:,3}*1000 + data{:,4};
data = [stateroute data(:,3:54)];
Ntot = sum(data(:,4:53),2);
mean_stopN = mean(data(:,4:53),2);
var_stopN = std(data(:,4:53),0,2).^2;
part1 = Ntot./(Ntot-1);
part2 = mean_stopN.^-1;
part3 = (var_stopN./mean_stopN)+mean_stopN-1;
Im = part1.*part2.*part3;
Im = 10000*Im;              %for some reason Matlab was seeing miniscule differences in values
Im = round(Im);             %that should have been identical, so I just rounded everything to 
Im = Im/10000;              %the nearest ten thousandth

%create new dataset with stateroute, YEAR, AOU species code, and Im values; 
%then rank Im values within each route-year
newdata = [data(:,1:3) Im];

unique_routes = unique(newdata(:,1));
num_routes = size(unique_routes,1);
unique_spp = unique(newdata(:,3));
num_spp = size(unique_spp,1);

for i=1:num_routes;
    for k=2003:2005;
    rte_rows = find(newdata(:,1)==unique_routes(i) & newdata(:,2)==k);
    if(length(rte_rows)>0);
    [Im_sorted, Im_index] = sort(newdata(rte_rows,4));
    [x, Im_rank] = sort(Im_index);
    
    newdata(rte_rows,5) = Im_rank;
    
    %when Im values (and thus their ranks) are identical, replace with the mean rank of the tied places
    unique_Ims = unique(Im_sorted);
    for j=1:length(unique_Ims);
        [r,c] = find(newdata(rte_rows,4)==unique_Ims(j));
        newdata(rte_rows(r),6) = mean(newdata(rte_rows(r),5));
    end
    valid_Ims = size(newdata(newdata(rte_rows,4)~=Inf),1); %number of species for which Im is defined
    newdata(rte_rows,7) = newdata(rte_rows,6)/valid_Ims;
        
    %when Im is undefined (Inf), assign a flagged NaN
    newdata(newdata(:,4)==Inf,7) = NaN;
    i
    end
    end
end

%converting data to short form matrix
Im_matrix = nan(num_spp,num_routes,3); %creates emptry matrix filled with NaNs
%convert 'newdata' to a species x route x year matrix
for i=1:length(newdata);
    Im_matrix(find(unique_spp==newdata(i,3)),find(unique_routes==newdata(i,1)),newdata(i,2)-2002) = newdata(i,7);
end
%'calculating means'
Im_matrix2 = nanmean(Im_matrix,3); %takes the mean, excluding NaN values, across years
Spp_Ims = [unique_spp nanmean(Im_matrix2,2)]; %takes the mean of the means, excluding NaNs, across sites

%dlmwrite('H:\Manuscripts\RangeOccupancy\Data\new_localniche.csv',Spp_Ims,',')




%%%%%%%OLD BULKY CODE FOR GETTING THE SPECIES MEANS%%%%%%%%%

%calculate mean Im for each species-route combo when a species was observed
%on the same route more than one year
%preoutput = [];
%for i=1:num_routes;
%    for j=1:num_spp;
%    [r,c] = find(newdata(:,1)==unique_routes(i) & newdata(:,3)==unique_spp(j) & newdata(:,7)~=-9999);
%    if(length(r>0));
%       preoutput = [preoutput; unique_routes(i) unique_spp(j) mean(newdata(r,7)];
%    end
%    end    
%end

%calculate mean Im for each species over routes on which it occurs, store in OUTPUT
%OUTPUT = zeros(num_spp,3);

%output species code, mean standardized Im rank, and number of routes on which it was calculated
%for i=1:num_spp;
%    [y,z] = find(preoutput(:,2)==unique_spp(i));
%    OUTPUT(i,1) = unique_spp(i);
%    OUTPUT(i,2) = mean(preoutput(y,3));
%    OUTPUT(i,3) = size(y,1);
%end