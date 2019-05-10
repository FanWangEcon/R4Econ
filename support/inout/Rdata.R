# Data in and out
setwd(paste0(root,'/_RData/'))
save.image(file=paste0('PLANNER_', str.file.name, '.RData'))
load('PLANNER_m0t24_g9c13.RData')


st.rdata <- paste0(root,'/_RData/estiprodprofile/')
st.rdata.name.prep.a <- 'all_logged_'
st.rdata.file.name <- paste0(st.rdata.name.prep.a, str.file.name, '.RData')

setwd(st.rdata)
load(st.rdata.file.name)

st.rdata.name.prep.b.no0 <- 'no0_logged_'
st.rdata.file.name.new <- paste0(st.rdata.name.prep.a, str.file.name, '.RData')
setwd(st.rdata)
save.image(file=st.rdata.file.name.new)
