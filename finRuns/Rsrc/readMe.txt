1. Navigete to the folder:
/scratch/project_2000994/PREBASruns/finRuns/Rsrc

2. set the forest center for which you want to do the runs
forCent.r (line 1)

3. run the batch files (this will run the model):
1_runModel_IDi.sh	i from 1 to 10 
example:
sbatch 1_runModel_ID1.sh

4. create raster from model outputs:
sbatch 2_createRast.sh

5. check if there are NAs in the NEP raster and find the segID.
francesco is developing the code

structure of code:
1. read raster GPP and NEP
2. check if there are NAs in NEP where you have values for GPP
3. find the corresponding segID and rerun the model (francesco).