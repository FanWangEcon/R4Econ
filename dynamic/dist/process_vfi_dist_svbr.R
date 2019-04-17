# Required Packages
# library(tidyverse)
# library(AER)
# library(R.matlab)

# ls.vfds <- ff_dyna_combine_vfds(root = 'C:/Users/fan/ThaiForInfLuuRobFan/',
#                                 folder = 'matlab/inf_az/_mat/svbr/',
#                                 subfolder = 'gda_medium1',
#                                 st.file.prefix.vf = 'vf_az_p_gb_sa',
#                                 st.file.prefix.ds = 'ds_az_p_gb_sa',
#                                 ar.it.pm.subset = c(110,112, 116,118,
#                                                     150,152, 156,158),
#                                 ar.it.inner.counter = 1:1:9,
#                                 bl.txt.print = FALSE)

ff_dyna_combine_vfds <- function(root = 'C:/Users/fan/ThaiForInfLuuRobFan/',
                                 folder = 'matlab/inf_az/_mat/svbr/',
                                 subfolder = 'gda_medium1',
                                 st.file.prefix.vf = 'vf_az_p_gb_sa',
                                 st.file.prefix.ds = 'ds_az_p_gb_sa',
                                 ar.it.pm.subset = c(110,112, 116,118, 150,152, 156,158),
                                 ar.it.inner.counter = 1:1:9,
                                 bl.txt.print = FALSE){

    # options(warn=-1) # Suppress Warning Messages
    # Load in Matlab Mat File from running vf_az

    # root <- 'C:/Users/fan/ThaiForInfLuuRobFan/'
    # folder <- 'matlab/inf_az/_mat/svbr/'
    # subfolder <- 'mat_grid_a_large1'
    # subfolder <- 'gda_medium1'
    # subfolder <- 'gda_small1'
    # st.file.prefix.vf <- 'vf_az_p_gb_sa'
    # st.file.prefix.ds <- 'ds_az_p_gb_sa'

    # ar.it.pm.subset <- c(110,112, 116,118,
    #                      150,152, 156,158)

    # ar.it.inner.counter <- 1:1:9

    # bl.txt.print <- TRUE

    st.caption <- paste0(subfolder, '\n',
                         paste0('ar.it.inner.counter:', paste0(ar.it.inner.counter, collapse='-'), ';'), '\n',
                         paste0('ar.it.pm.subset:', paste0(ar.it.pm.subset, collapse='-'), ';'))

    list.ar.it <- list(ar.it.inner.counter, ar.it.pm.subset)
    mt.fl.expanded <- do.call(expand.grid, list.ar.it)

    df.slds <- tibble()
    for (i in 1:dim(mt.fl.expanded)[1]) {

        # Counters
        it.inner.counter <- mt.fl.expanded[i, 1]
        it.pm.subset <- mt.fl.expanded[i, 2]

        # Folder Names and Load .mat
        curfolder <- paste0(folder, subfolder, '/pm', it.pm.subset, '/')
        st.vf.matfile <- paste0(st.file.prefix.vf, it.pm.subset, '_c', it.inner.counter,'.mat')
        vf.mat.out <- readMat(paste0(root, curfolder, st.vf.matfile))
        st.ds.matfile <- paste0(st.file.prefix.ds, it.pm.subset, '_c', it.inner.counter,'.mat')
        ds.mat.out <- readMat(paste0(root, curfolder, st.ds.matfile))

        # From Distribution File
        mt.dist <- ds.mat.out$D0

        # From VFI file
        mt.pol <- vf.mat.out$mt.pol
        mt.val <- vf.mat.out$mt.val
        mt.con <- vf.mat.out$mt.cons
        mt.inc <- vf.mat.out$mt.incm

        # State Vectors
        ar.a <- vf.mat.out$ar.a
        ar.z <- vf.mat.out$ar.z

        # LParameters
        it.z.n <- vf.mat.out$it.z.n
        it.a.n <- vf.mat.out$it.a.n
        fl.crra <- vf.mat.out$fl.crra
        fl.rho <- vf.mat.out$fl.rho
        fl.sig <- vf.mat.out$fl.sig

        # Combine to Dataframe
        ar.st.vars <- c('a', 'z')
        list.ar.fl <- list(ar.a, paste0('z=', round(ar.z, 3)))
        list.ts.valpolmat <- tibble(val=as.numeric(mt.val),
                                    aprime=as.numeric(mt.pol),
                                    con=as.numeric(mt.con),
                                    inc=as.numeric(mt.inc),
                                    prob=as.numeric(mt.dist),
                                    it.pm.subset = it.pm.subset,
                                    it.inner.counter = it.inner.counter,
                                    it.z.n = it.z.n,
                                    it.a.n = it.a.n,
                                    fl.crra = fl.crra,
                                    fl.rho = fl.rho,
                                    fl.sig = fl.sig)

        df.slds.cur <- ff_dyna_sup_expand_grids(ar.st.vars, list.ar.fl, list.ts.valpolmat)
        if(bl.txt.print) {
            print(dim(df.slds.cur))
        }

        # Stack Dataframes together
        df.slds <- bind_rows(df.slds, df.slds.cur)
        if(bl.txt.print) {
            print(dim(df.slds))
        }
    }

    # Generate Additional Variables
    df.slds <- df.slds %>% mutate(wealth = inc + a,
                                  bl.borr = if_else(aprime < 0, 1, 0))

    # Generate Categorical Variables
    df.slds <- df.slds %>% mutate(it.z.n_str = sprintf("%02d", it.z.n),
                                  it.a.n_str = sprintf("%04d", it.a.n)) %>%
                      mutate(st_pm_z_n_n_z = paste0('crra=', fl.crra,
                                                    ',rho=', fl.rho,
                                                    ',sig=', fl.sig,
                                                    '\nan=', it.a.n_str,
                                                    ',zn=', it.z.n_str),
                             st_it_pm = paste0('pm=', it.pm.subset,
                                                    ',i=', it.inner.counter))
    return(list(df=df.slds))

}
