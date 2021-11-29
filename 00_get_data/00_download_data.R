library(osfr)

project=osf_retrieve_node("https://osf.io/vmbk6/")
files=osf_ls_files(project)
#make sure to have the folder described in the path: "data/raw_data/02_raw_data_csv"
osf_download(files,path="data/raw_data/02_raw_data_csv") 

