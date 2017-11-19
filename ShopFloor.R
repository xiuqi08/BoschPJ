library(data.table)
library(dtplyr)
library(dplyr)

DATA_DIR <- '../xiuqi/Downloads/Data/Bosch'
TRAIN_NUM <- paste(DATA_DIR,'train_numeric.csv',sep='/')
TRAIN_DATE <- paste(DATA_DIR,'train_date.csv',sep='/')

ID_COL <- 'Id'
TARGET_COL <- 'Response'
NROWS <- 1000

dtNum <- fread(TRAIN_NUM, select = c(ID_COL,TARGET_COL), nrows=NROWS)
dtDate <- fread(TRAIN_DATE, nrows=NROWS)

# Add the start time of each station as columns
for(station in paste0('S',0:51)){
  # Cols is the first index of date column belongs to station
  cols <- min(which(grepl(station, colnames(dtDate))))
  if(!cols==Inf){
    # dCol stores start time of the station
    dCol <- dtDate[,cols,with=FALSE]
    # Add start time columns for each station
    dtDate[,paste0(station) := dCol]
  }
}

# Select columns of station start times
dtStations = dtDate[,!grepl("L",colnames(dtDate)),with=F]
# Melt table to ID, Station, StartTime format
dtStationsM = melt(dtStations, id.vars = c(ID_COL))

# Joint with numeric to have response
dtStationsM %>%
  left_join(dtNum, by = ID_COL) -> dtStationsM
# Remove rows with na in value
dtStationsM %>%
  filter(!is.na(value)) -> dtStationsMFiltered
# Sort by ascending time
dtStationsMFiltered %>%
  arrange(value) -> dtStationsMFiltered

#View(dtStationsMFiltered)

require(GGally)
library(network)
library(sna)
library(ggplot2)

options(repr.plot.width=5,repr.plot.height=15)
# Obtain the subsequent station for each row
dtStationsMFiltered %>%
  group_by(Id) %>%
  mutate(nextStation = lead(variable)) -> edgelistsComplete
#View(edgelistsComplete)

# Find the first station of each id
edgelistsComplete %>%
  group_by(Id) %>%
  filter(!(variable %in% nextStation)) %>%
  ungroup() %>%
  select(variable,Response) -> startingPoints

colnames(startingPoints)= c('nextStation',TARGET_COL)
startingPoints$variable = "S"
#View(startingPoints)
edgelistsComplete %>%
  select(variable,nextStation,Response) -> paths
#View(paths)

# Fill the nextStation of lastStation with Response
paths[is.na(nextStation)]$nextStation <- paste("Result",paths[is.na(nextStation)]$Response)
# Row bind startingPoints( From (S) to first station) and Paths (path from first station)
paths <- rbind(startingPoints,paths)
# Drop column response since it has been recorded in nextStation of last station
paths <- select(paths,-Response)
paths$nextStation <- as.character(paths$nextStation)
paths$variable <- as.character(paths$variable)

# Rename columns for plotiing
colnames(paths) <- c("Target","Source")
paths <- paths[,c("Source","Target")]
#View(paths)
write.csv(paths,'/Users/xiuqi/Downloads/Data/Bosch/paths.csv',quote = F,row.names = F)

############################## Not Tested ##########################
# Create network from edgelist
# network receives a dataframe containing 2 columns: from and to
net <- network(as.data.frame(na.omit(paths)),directed = T)
# create a station-line mapping lookup
LineStations <- NULL
for(station in unique(paths$Source)){
  if(station!="S"){
    x <- paste0("_",station,"_")
    # y is the name of the first columns start with station x
    y <- head(colnames(dtDate)[which(grepl(x,colnames(dtDate)))],1)
    # split y to get line name
    y <- strsplit(y,'_')[[1]][1]
    LineStations = rbind(LineStations,data.frame(Node=station,Line=y))
  }
}
# Node  Line
# S31    L3
LineStations <- rbind(LineStations, data.frame(Node=c('Result 1',"Result 0","S"),Line=c("Outcome","Outcome","START")))

# Merge station-line mapping into graph for coloring
# x: Return name of all nodes in network
x = data.frame(Node = network.vertex.names(net))
# Merge with LineStations to get line of all nodes
x = merge(x, LineStations, by = "Node", sort = FALSE)$Line
# ????????
net %v% "line" = as.character(x)
print(net)

nodeCoordinates=data.frame(label=c("S","S0","S1","S2","S3","S4","S5","S6",
                                   "S7","S8","S9","S10","S11","S12","S13",
                                   "S14","S15","S16","S17","S18","S19",
                                   "S20","S21","S22","S23","S24","S25",
                                   "S26","S27","S28","S29","S30","S31",
                                   "S32","S33","S34","S35","S36","S37",
                                   "S38","S39","S40","S41","S43",
                                   "S44","S45","S47","S48","S49",
                                   "S50","S51","Result 0","Result 1"),
                           x=c(0,
                               1,2,3,3,4,4,5,5,6,7,7,7,
                               1,2,3,3,4,4,5,5,6,7,7,7,
                               6,6,7,7,7,
                               8,9,10,10,10,11,11,12,13,14,
                               8,9,10,11,11,12,13,14,15,15,16,
                               17,17),
                           y=c(5,
                               9,9,10,8,10,8,10,8,7,10,9,8,
                               5,5,6,4,6,4,6,4,5,6,5,4,
                               2,0,2,1,0,
                               7,7,8,7,6,8,6,7,7,7,
                               3,3,3,4,2,3,3,3,4,2,3,
                               7,3))

network = ggnet2(net)

netCoordinates = select(network$data,label)
netCoordinates = left_join(netCoordinates,nodeCoordinates,by = "label")
netCoordinates = as.matrix(select(netCoordinates,x,y))



network = ggnet2(net,
                 alpha = 0.75, size = "indegree",
                 label = T, size.cut = 4,
                 color = "line",palette = "Set1",
                 mode = netCoordinates,
                 edge.alpha = 0.5, edge.size = 1,
                 legend.position = "bottom")
print(network)