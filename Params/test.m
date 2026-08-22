% This code is written for TWI calculation:
% data need:
% 1. Standard FAC file
% 2. Slope file with degree

clear;clc;


% read the asc data

ncols = 598; 
nrows = 650;

FormatString=repmat('%f',1,ncols);
NoData = -9999;
cellSize = 0.000833;
XLLCorner 	=    108.335815;
YLLCorner   =    32.654663;


fid_slope = fopen('..\Basics\Mask.asc','r');
MASK = cell2mat(textscan(fid_slope,FormatString,nrows,'HeaderLines',6));
Count = sum(MASK(:)>0);
Area = Count * 0.09 * 0.09; % km^2


% geotiffwrite('IM2.tif', IM, R);
