#' Splitter Function 
#'
#' @param split_data 
#' @param valid_wanted 
#' @param train_split 
#' @param seed 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
train_splitter = function(split_data, valid_wanted = FALSE, train_split, seed, ...){
  
  base::set.seed(seed)
  resls = list()
  
  split_data = split_data
  split_data_EAX = split_data[which(split_data$best_solver == "EAXrestart"), ]
  split_data_LKH = split_data[which(split_data$best_solver == "LKHrestart"), ]
  
  if(valid_wanted == FALSE){
    # train test only
    train_split = train_split 
    
    #=================================================================
    # RUE
    #=================================================================
    # RUE.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "rue.500"), "ID"]) %>% unique(.)
    N_sample_other_EAX = (((split_data_EAX[which(split_data_EAX$generator %in% c("rue.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split) %>% ceiling()
    
    samples_EAX = sample(ID_sample_EAX, N_sample_other_EAX, replace = F)
    rue.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_EAX), ] 
    rue.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$generator == "rue.500" & 
                                                     split_data_EAX$ID %!in% samples_EAX), ] 
    # RUE.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "rue.500"), "ID"]) %>% unique(.)
    N_sample_other_LKH = (((split_data_LKH[which(split_data_LKH$generator %in% c("rue.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split)  %>% ceiling()
    samples_LKH = sample(ID_sample_LKH, N_sample_other_LKH, replace = F)
    rue.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_LKH), ] 
    rue.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$generator == "rue.500" & 
                                                     split_data_LKH$ID %!in% samples_LKH), ] 
    
    #=================================================================
    # netgen.500
    #=================================================================
    # netgen.500.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "netgen.500"), "ID"]) %>% unique(.)
    N_sample_other_EAX = (((split_data_EAX[which(split_data_EAX$generator %in% c("netgen.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_EAX = sample(ID_sample_EAX, N_sample_other_EAX, replace = F)
    netgen.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_EAX), ] 
    netgen.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$generator == "netgen.500" &
                                                        split_data_EAX$ID %!in% samples_EAX), ] 
    # netgen.500.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "netgen.500"), "ID"]) %>% unique(.)
    N_sample_other_LKH = (((split_data_LKH[which(split_data_LKH$generator %in% c("netgen.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split)  %>% ceiling()
    samples_LKH = sample(ID_sample_LKH, N_sample_other_LKH, replace = F)
    netgen.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_LKH), ] 
    netgen.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$generator == "netgen.500" & 
                                                        split_data_LKH$ID %!in% samples_LKH), ] 
    
    #=================================================================
    # netgen.morphed.500
    #=================================================================
    # netgen.morphed.500.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "netgen_morphed.500"), "ID"]) %>% unique(.)
    N_sample_other_EAX = (((split_data_EAX[which(split_data_EAX$generator %in% c("netgen_morphed.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_EAX = sample(ID_sample_EAX, N_sample_other_EAX, replace = F)
    netgen.morphed.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_EAX), ] 
    netgen.morphed.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$generator == "netgen_morphed.500" & 
                                                                split_data_EAX$ID %!in% samples_EAX), ] 
    # netgen.morphed.500.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "netgen_morphed.500"), "ID"]) %>% unique(.)
    N_sample_other_LKH = (((split_data_LKH[which(split_data_LKH$generator %in% c("netgen_morphed.500")), "ID"]) %>% 
                             unique(.) %>% length(.)) * train_split)  %>% ceiling()
    samples_LKH = sample(ID_sample_LKH, N_sample_other_LKH, replace = F)
    netgen.morphed.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_LKH), ] 
    netgen.morphed.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$generator == "netgen_morphed.500" & 
                                                                split_data_LKH$ID %!in% samples_LKH), ] 
    
    #=================================================================
    # evol500.eaxsoph
    #=================================================================
    # evol500.eaxsoph.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "evol500.eaxsoph"), "ID"]) %>% unique(.)
    N_sample_soph_EAX = (((split_data_EAX[which(split_data_EAX$generator %in% c("evol500.eaxsoph")), "ID"]) %>% 
                            unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_EAX = sample(ID_sample_EAX, N_sample_soph_EAX, replace = F)
    evol500.eaxsoph.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_EAX), ] 
    evol500.eaxsoph.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$generator == "evol500.eaxsoph" & 
                                                                 split_data_EAX$ID %!in% samples_EAX), ] 
    # evol500.eaxsoph.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "evol500.eaxsoph"), "ID"]) %>% unique(.)
    N_sample_soph_LKH = (((split_data_LKH[which(split_data_LKH$generator %in% c("evol500.eaxsoph")), "ID"]) %>% 
                            unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_LKH = sample(ID_sample_LKH, N_sample_soph_LKH, replace = F)
    evol500.eaxsoph.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_LKH), ] 
    evol500.eaxsoph.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$generator == "evol500.eaxsoph" & 
                                                                 split_data_LKH$ID %!in% samples_LKH), ] 
    
    #=================================================================
    # evol500.lkhsoph  // PROBLEM there are almost no lkh soph instancce where EAX+restart is the better solver 
    #=================================================================
    # evol500.lkhsoph.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "evol500.lkhsoph"), "ID"]) %>% unique(.)
    N_sample_soph_EAX = (((split_data_EAX[which(split_data_EAX$generator %in% c("evol500.lkhsoph")), "ID"]) %>% 
                            unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_EAX = sample(ID_sample_EAX, N_sample_soph_EAX, replace = F) # with replacement 
    evol500.lkhsoph.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_EAX), ] 
    evol500.lkhsoph.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$generator == "evol500.lkhsoph" & 
                                                                 split_data_EAX$ID %!in% samples_EAX), ] 
    # evol500.lkhsoph.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "evol500.lkhsoph"), "ID"]) %>% unique(.)
    N_sample_soph_LKH = (((split_data_LKH[which(split_data_LKH$generator %in% c("evol500.lkhsoph")), "ID"]) %>% 
                            unique(.) %>% length(.)) * train_split) %>% ceiling()
    samples_LKH = sample(ID_sample_LKH, N_sample_soph_LKH, replace = F) # with replacement 
    evol500.lkhsoph.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_LKH), ] 
    evol500.lkhsoph.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$generator == "evol500.lkhsoph" & 
                                                                 split_data_LKH$ID %!in% samples_LKH), ] 
    #=================================================================
    # bind results
    #=================================================================
    
    TRAIN_train.500 = do.call("rbind", list(rue.500_sample_TRAIN_EAX,
                                            rue.500_sample_TRAIN_LKH,
                                            netgen.500_sample_TRAIN_EAX,
                                            netgen.500_sample_TRAIN_LKH,
                                            netgen.morphed.500_sample_TRAIN_EAX,
                                            netgen.morphed.500_sample_TRAIN_LKH,
                                            evol500.eaxsoph.500_sample_TRAIN_EAX,
                                            evol500.eaxsoph.500_sample_TRAIN_LKH,
                                            evol500.lkhsoph.500_sample_TRAIN_EAX,
                                            evol500.lkhsoph.500_sample_TRAIN_LKH))
    
    TEST_train.500 = do.call("rbind", list(rue.500_sample_TEST_EAX, 
                                           rue.500_sample_TEST_LKH,
                                           netgen.500_sample_TEST_EAX, 
                                           netgen.500_sample_TEST_LKH, 
                                           netgen.morphed.500_sample_TEST_LKH, 
                                           netgen.morphed.500_sample_TEST_EAX,
                                           evol500.eaxsoph.500_sample_TEST_EAX, 
                                           evol500.eaxsoph.500_sample_TEST_LKH, 
                                           evol500.lkhsoph.500_sample_TEST_EAX, 
                                           evol500.lkhsoph.500_sample_TEST_LKH))
    
    resls = list.append(resls,
                        TRAIN_train.500 = TRAIN_train.500,
                        TEST_train.500 = TEST_train.500
    )
    # garbage collect
    rm(ID_sample_EAX, ID_sample_LKH, N_sample_other_LKH, N_sample_other_EAX,
       N_sample_soph_EAX, N_sample_soph_LKH, samples_LKH, samples_EAX, 
       evol500.eaxsoph.500_sample_TEST_EAX, evol500.eaxsoph.500_sample_TEST_LKH,
       evol500.eaxsoph.500_sample_TRAIN_EAX, evol500.eaxsoph.500_sample_TRAIN_LKH,
       evol500.lkhsoph.500_sample_TEST_EAX, evol500.lkhsoph.500_sample_TEST_LKH, 
       evol500.lkhsoph.500_sample_TRAIN_EAX, evol500.lkhsoph.500_sample_TRAIN_LKH,
       netgen.500_sample_TEST_EAX, netgen.500_sample_TEST_LKH, 
       netgen.500_sample_TRAIN_EAX, netgen.500_sample_TRAIN_LKH,
       netgen.morphed.500_sample_TEST_EAX, netgen.morphed.500_sample_TEST_LKH, 
       netgen.morphed.500_sample_TRAIN_EAX, netgen.morphed.500_sample_TRAIN_LKH,
       rue.500_sample_TEST_EAX, rue.500_sample_TEST_LKH, 
       rue.500_sample_TRAIN_EAX, rue.500_sample_TRAIN_LKH,
       train_split, split_data, split_data_EAX, split_data_LKH)
    
  } else {
    train_split = train_split
    test_split = (1 - train_split) / 2
    valid_split = test_split
    
    # train valid and test
    # actual sampling from each generator w.r.t. solver
    #=================================================================
    # RUE
    #=================================================================
    # RUE.EAX
    # ID to sample from
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "rue.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_EAX[which(split_data_EAX$generator %in% c("rue.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_EAX) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_EAX) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_EAX, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_EAX, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_EAX, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    rue.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_train), ] 
    rue.500_sample_VALID_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_valid), ] 
    rue.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_test), ] 
    
    # RUE.LKH
    # ID to sample from
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "rue.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_LKH[which(split_data_LKH$generator %in% c("rue.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_LKH) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_LKH) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_LKH, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_LKH, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_LKH, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    rue.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_train), ] 
    rue.500_sample_VALID_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_valid), ] 
    rue.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_test), ] 
    
    #=================================================================
    # netgen.500
    #=================================================================
    # netgen.500.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "netgen.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_EAX[which(split_data_EAX$generator %in% c("netgen.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_EAX) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_EAX) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_EAX, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_EAX, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_EAX, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    netgen.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_train), ] 
    netgen.500_sample_VALID_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_valid), ] 
    netgen.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_test), ] 
    
    # netgen.500.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "netgen.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_LKH[which(split_data_LKH$generator %in% c("netgen.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_LKH) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_LKH) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_LKH, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_LKH, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_LKH, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    netgen.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_train), ] 
    netgen.500_sample_VALID_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_valid), ] 
    netgen.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_test), ] 
    
    #=================================================================
    # netgen.morphed.500
    #=================================================================
    # netgen.morphed.500.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "netgen_morphed.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_EAX[which(split_data_EAX$generator %in% c("netgen_morphed.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_EAX) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_EAX) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_EAX, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_EAX, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_EAX, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    netgen.morphed.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_train), ] 
    netgen.morphed.500_sample_VALID_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_valid), ] 
    netgen.morphed.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_test), ] 
    
    # netgen.morphed.500.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "netgen_morphed.500"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_LKH[which(split_data_LKH$generator %in% c("netgen_morphed.500")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_LKH) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_LKH) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_LKH, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_LKH, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_LKH, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    netgen.morphed.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_train), ] 
    netgen.morphed.500_sample_VALID_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_valid), ] 
    netgen.morphed.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_test), ] 
    
    #=================================================================
    # evol500.eaxsoph
    #=================================================================
    # evol500.eaxsoph.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "evol500.eaxsoph"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_EAX[which(split_data_EAX$generator %in% c("evol500.eaxsoph")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_EAX) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_EAX) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_EAX, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_EAX, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_EAX, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    evol500.eaxsoph.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_train), ] 
    evol500.eaxsoph.500_sample_VALID_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_valid), ] 
    evol500.eaxsoph.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_test), ] 
    
    # evol500.eaxsoph.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "evol500.eaxsoph"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_LKH[which(split_data_LKH$generator %in% c("evol500.eaxsoph")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_LKH) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_LKH) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_LKH, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_LKH, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_LKH, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    evol500.eaxsoph.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_train), ] 
    evol500.eaxsoph.500_sample_VALID_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_valid), ] 
    evol500.eaxsoph.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_test), ] 
    
    #=================================================================
    # evol500.lkhsoph  // PROBLEM there are almost no lkh soph instance where EAX+restart is the better solver 
    #=================================================================
    # evol500.lkhsoph.EAX
    ID_sample_EAX = (split_data_EAX[which(split_data_EAX$generator == "evol500.lkhsoph"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_EAX[which(split_data_EAX$generator %in% c("evol500.lkhsoph")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_EAX) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_EAX) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_EAX, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_EAX, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_EAX, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    evol500.lkhsoph.500_sample_TRAIN_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_train), ] 
    evol500.lkhsoph.500_sample_VALID_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_valid), ] 
    evol500.lkhsoph.500_sample_TEST_EAX = split_data_EAX[which(split_data_EAX$ID %in% samples_test), ] 
    
    # evol500.lkhsoph.LKH
    ID_sample_LKH = (split_data_LKH[which(split_data_LKH$generator == "evol500.lkhsoph"), "ID"]) %>% unique(.)
    
    N_train = (((split_data_LKH[which(split_data_LKH$generator %in% c("evol500.lkhsoph")), "ID"]) %>% 
                  unique(.) %>% length(.)) * train_split) %>% ceiling()
    N_valid = ((length(ID_sample_LKH) - N_train)/2) %>% floor(.) # less for valid
    N_test = ((length(ID_sample_LKH) - N_train)/2) %>% ceiling(.) # more for test
    
    samples_train = sample(ID_sample_LKH, N_train, replace = F)
    samples_valid = sample(setdiff(ID_sample_LKH, samples_train), N_valid, replace = F)
    samples_test =  sample(setdiff(ID_sample_LKH, samples_train) %>% setdiff(., samples_valid) , N_test, replace = F)
    
    evol500.lkhsoph.500_sample_TRAIN_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_train), ] 
    evol500.lkhsoph.500_sample_VALID_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_valid), ] 
    evol500.lkhsoph.500_sample_TEST_LKH = split_data_LKH[which(split_data_LKH$ID %in% samples_test), ] 
    
    #=================================================================
    # bind results
    #=================================================================
    
    TRAIN_train.500 = do.call("rbind", list(rue.500_sample_TRAIN_EAX,
                                            rue.500_sample_TRAIN_LKH,
                                            netgen.500_sample_TRAIN_EAX,
                                            netgen.500_sample_TRAIN_LKH,
                                            netgen.morphed.500_sample_TRAIN_EAX,
                                            netgen.morphed.500_sample_TRAIN_LKH,
                                            evol500.eaxsoph.500_sample_TRAIN_EAX,
                                            evol500.eaxsoph.500_sample_TRAIN_LKH,
                                            evol500.lkhsoph.500_sample_TRAIN_EAX,
                                            evol500.lkhsoph.500_sample_TRAIN_LKH))
    
    VALID_train.500 = do.call("rbind", list(rue.500_sample_VALID_EAX, 
                                            rue.500_sample_VALID_LKH,
                                            netgen.500_sample_VALID_EAX, 
                                            netgen.500_sample_VALID_LKH, 
                                            netgen.morphed.500_sample_VALID_LKH, 
                                            netgen.morphed.500_sample_VALID_EAX,
                                            evol500.eaxsoph.500_sample_VALID_EAX, 
                                            evol500.eaxsoph.500_sample_VALID_LKH, 
                                            evol500.lkhsoph.500_sample_VALID_EAX, 
                                            evol500.lkhsoph.500_sample_VALID_LKH))
    
    TEST_train.500 = do.call("rbind", list(rue.500_sample_TEST_EAX, 
                                           rue.500_sample_TEST_LKH,
                                           netgen.500_sample_TEST_EAX, 
                                           netgen.500_sample_TEST_LKH, 
                                           netgen.morphed.500_sample_TEST_LKH, 
                                           netgen.morphed.500_sample_TEST_EAX,
                                           evol500.eaxsoph.500_sample_TEST_EAX, 
                                           evol500.eaxsoph.500_sample_TEST_LKH, 
                                           evol500.lkhsoph.500_sample_TEST_EAX, 
                                           evol500.lkhsoph.500_sample_TEST_LKH))
    
    resls = list.append(resls,
                        TRAIN_train.500 = TRAIN_train.500,
                        VALID_train.500 = VALID_train.500, 
                        TEST_train.500 = TEST_train.500
    )
    
  }
  rm(ID_sample_EAX, ID_sample_LKH, N_train, N_test, N_valid, samples_train, samples_test, samples_valid, 
     evol500.eaxsoph.500_sample_TEST_EAX, evol500.eaxsoph.500_sample_TEST_LKH, 
     evol500.eaxsoph.500_sample_VALID_EAX,
     evol500.eaxsoph.500_sample_TRAIN_EAX, evol500.eaxsoph.500_sample_TRAIN_LKH, evol500.eaxsoph.500_sample_VALID_LKH,
     evol500.lkhsoph.500_sample_TEST_EAX, evol500.lkhsoph.500_sample_TEST_LKH, evol500.lkhsoph.500_sample_VALID_LKH, 
     evol500.lkhsoph.500_sample_TRAIN_EAX, evol500.lkhsoph.500_sample_VALID_EAX, evol500.lkhsoph.500_sample_TRAIN_LKH,
     netgen.500_sample_TEST_EAX, netgen.500_sample_TEST_LKH, netgen.500_sample_VALID_LKH, 
     netgen.500_sample_TRAIN_EAX, netgen.500_sample_TRAIN_LKH, netgen.500_sample_VALID_EAX,
     netgen.morphed.500_sample_TEST_EAX, netgen.morphed.500_sample_TEST_LKH, netgen.morphed.500_sample_VALID_LKH, 
     netgen.morphed.500_sample_TRAIN_EAX, netgen.morphed.500_sample_TRAIN_LKH,netgen.morphed.500_sample_VALID_EAX,
     rue.500_sample_TEST_EAX, rue.500_sample_TEST_LKH, rue.500_sample_VALID_LKH, 
     rue.500_sample_TRAIN_EAX, rue.500_sample_TRAIN_LKH, rue.500_sample_VALID_EAX,
     train_split, split_data, split_data_EAX, split_data_LKH)
  
  return(resls)
}