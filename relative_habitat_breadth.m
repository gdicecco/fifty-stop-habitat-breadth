% FILENAME: relative_habitat_breadth.m
% AUTHOR: ALLEN HURLBERT
% CREATED: 27.VII.05
% MODIFIED: 
% PURPOSE: INPUT DATA FILE WITH 54 COLUMNS--STATE, ROUTE ID, YEAR, SPECIES CODE, AND SPECIES ABUNDANCE
%          AT EACH OF FIFTY POINT COUNT STOPS.
%          FOR EACH ROUTE, CALCULATE MORISITA'S INDEX OF AGGREGATION, Im, FOR EACH SPECIES.
%          RANK SPECIES BY Im VALUES WITHIN EACH ROUTE, AND STORE STANDARDIZED RANK (Im RANK/# SPP).
%          FOR EACH SPECIES, CALCULATE MEAN STANDARDIZED Im RANK OVER ALL THE ROUTES ON WHICH IT OCCURS.

data = dlmread('fiftystopdata04.csv',',');
stateroute = data(:,1)*1000 + data(:,2);
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

%create new dataset with stateroute, AOU species code, and Im values; then rank Im values within each route
newdata = [data(:,1) data(:,3) Im];

unique_routes = unique(newdata(:,1));
num_routes = size(unique_routes,1);

for i=1:num_routes;
    rte_rows = find(newdata(:,1)==unique_routes(i));
    
    [Im_sorted, Im_index] = sort(newdata(rte_rows,3));
    [x, Im_rank] = sort(Im_index);
    
    newdata(rte_rows,4) = Im_rank;
    
    %when Im values (and thus their ranks) are identical, replace with the mean rank of the tied places
    unique_Ims = unique(Im_sorted);
    for j=1:length(unique_Ims);
        [r,c] = find(newdata(rte_rows,3)==unique_Ims(j));
        newdata(rte_rows(r),5) = mean(newdata(rte_rows(r),4));
               
        %newdata(newdata(rte_rows,3)==unique_Ims(j),5) = mean(newdata(newdata(rte_rows,3)==unique_Ims(j),4));
    end
    valid_Ims = size(newdata(newdata(rte_rows,3)~=Inf),1); %number of species for which Im is defined
    newdata(rte_rows,6) = newdata(rte_rows,5)/valid_Ims;
        
    %when Im is undefined (Inf), assign a flagged rank of -9999
    newdata(newdata(:,3)==Inf,6) = -9999;
    i
end


%calculate mean Im for each species over routes on which it occurs, store in OUTPUT
unique_spp = unique(newdata(:,2));
num_spp = size(unique_spp,1);

OUTPUT = zeros(num_spp,3);

%output species code, mean standardized Im rank, and number of routes on which it was calculated
for i=1:num_spp;
    [y,z] = find(newdata(:,2)==unique_spp(i));
    OUTPUT(i,1) = unique_spp(i);
    OUTPUT(i,2) = mean(newdata(y(newdata(y,6)~=-9999),6));
    OUTPUT(i,3) = size(y(newdata(y,6)~=-9999),1);
end