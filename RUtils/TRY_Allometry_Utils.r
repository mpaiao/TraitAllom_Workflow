#==========================================================================================
#   Additional TRY utilities for fitting allometric models
#
#   Author: Marcos Longo 
#   Email:  m l o n g o a@t l b l d.o.t g o v
#
#   Date: 17-Jan-2023
#
#------------------------------------------------------------------------------------------



#---~---
#     This function fits multiple allometric models based on data, and retrieves the one
# with the most parsimonious yet informative model, using either homoscedastic or 
# heteroscedastic distribution of errors.
#
# Input variables:
# + DataTRY            -- tibble with the data to be used for model fitting.
# + try_trait          -- tibble with the TRY trait look-up table (variables and ID).
# + ModelTRY           -- tibble with list of models that we will seek to fit. It must have
#                         the following elements:
#                         - Model  -- a list of models to try to fit ("character").
#                                     Acceptable options are:
#                                     ~ OneLinear   : y = a0 * x
#                                     ~ OneLogLinear: y = a0 * x^a1
#                                     ~ TwoLinear   : y = a0 * x    * w
#                                     ~ TwoMixLinear: y = a0 * x^a1 * w
#                                     ~ TwoLogLinear: y = a0 * x^a1 * w^a2
#                                     ~ MartinezCano: y = a0 * x^a1 / ( a2 + x^a1 )
#                                     ~ Weibull     : y = a0 * (1 - exp (-a1 * x^a2 ))
#                         - xName  -- Name of the first predictor variable, typically the
#                                     one associated with size (DBH, Height, DBH^2*H). 
#                                     It must match a column name in "DataTRY".
#                         - wName  -- Name of the second predictor variable. Used only when
#                                     "Model" requires two predictors. This is typically the
#                                     variable used for scaling (e.g., wood density, LMA).
#                                     When required, it must match a column name in 
#                                     "DataTRY"
#                         - yName  -- Name of the response variable. It must match a column
#                                     name in "DataTRY".
# + CategAllom         -- tibble with information on categorical traits
#                         - TraitID    -- TRY trait ID associated with the categorical trait.
#                                         In addition, two special values are possible:
#                                         ~ NA_integer_  -- special flag for all data
#                                         ~ 0            -- special ID for clusters.
#                         - Name       -- Names of the categories, consistent with the 
#                                         possible values in each categorical trait. For the
#                                         special case of "all data", use "ALL".
#                         - TraitClass -- Name of the column associated with the trait. It
#                                         must match a column in DataTRY. For the special
#                                         case of all data, use "All".
#                         - DescClass  -- Slightly longer description of the column 
#                                         associated with the categorical trait (the trait
#                                         in general, not the specific classes).
# + UseFixedModel      -- If TRUE, we decide which model to use based on the global data.
#                         Otherwise, each Cluster/PFT will be fitted independently.
# + InfoCrit           -- Which information criterion approach to use? Options are 
#                         AIC (Akaike Information Criterion) and BIC (Bayesian Information
#                         Criterion). In both cases, we used the modified values to account
#                         for sample size.
# + InfoExtraOffset    -- Extra offset for selecting models with more parameters. We only
#                         take a model with more parameters if there is strong evidence that
#                         it actually improves the model.
# + CntAllomMin        -- Minimum number of points for fitting allometric models.
# + CntAllomMax        -- Maximum number of points to consider for fitting. This is used to
#                         limit the model fitting to a more manageable number of 
#                         observations when allometric data are exceedingly large. If no
#                         cap is sought, set CntAllomMax = +Inf. This setting is 
#                         independent on the data binning sampling defined by UseSizeBins.
# + AllomConfInt       -- Confidence interval for allometric model fittings.
# + AllomMaxItGain     -- Maximum number of iterations for calling the main optimiser for
#                         seeking a better maximum using the previous optimised set of 
#                         parameters. This sometimes helps improving the model, but it
#                         typically stabilises after a handful of iterations.
# + AllomMaxItOptim    -- Maximum number of iterations before giving up optimising the model.
#                         This is actually what controls the search for the optimal point,
#                         and should be relatively high as sometimes the initial guess is 
#                         too far from the actual optimal.
# + AllomTolGain       -- Relative tolerance for successive calls of the main optimiser.
#                         Values of the order of 1e-5 are typically sufficient.
# + AllomTolOptim      -- Relative tolerance for model fitting. This controls the error 
#                         tolerance of the optimiser itself and should be stricter (1.e-7
#                         or less).
# + AllomTolBestMult   -- Multiplication factor to relax tolerance when seeking the global 
#                         best model, for computational efficiency. Typically 100-1000.
# + AllomMaxItBestFrac -- Multiplication factor to relax tolerance for model fitting when
#                        running the bootstrapping for parameter uncertainty. Typically
#                        100-1000.
# + AllomTolMax        -- Maximum (i.e., most relaxed) tolerance acceptable. This can be
#                         used to avoid too relaxed tolerance when finding the best model
#                         or bootstraping.
# + AllomMaxItMin      -- Minimum number of steps per call of the optimiser. This can be
#                         used to go a bit quicker through the multi-model comparison.
# + AllomCntPred       -- Number of points spanning the range of predictor for fitted curve.
# + AllomQuantPred     -- Span predictor evenly across quantiles (TRUE) or linearly (FALSE)
# + AllomCntBoot       -- Number of times for bootstrapping for generating confidence bands.
# + CntBootFail        -- For each bootstrap iteration, maximum number of attempts before 
#                         giving up.
# + UseSizeBins        -- Use binning across size classes to have a more balanced 
#                         distribution across the predictor range? Note that in this case
#                         we cannot use AIC or BIC to define the best model, and thus we
#                         simply use adjusted R2.
# + xSample            -- Which variable to use for binning? This must be a valid column
#                         in DataTRY
# + xLogSmp            -- Should the binning be applied in the linear scale or log scale?
# + MinSmpPerBin       -- Minimum number of observations for each bin. Binning will not 
#                         occur unless there are at least twice as many valid points (and
#                         possibly even more points in case of very imbalanced data sets).
# + MaxCntBins         -- Maximum number of bins to be considered.
# + Verbose            -- Print information?
#---~---
Allom_Fit <<- function( DataTRY
                      , try_trait
                      , ModelTRY
                      , CategAllom
                      , UseFixedModel      = TRUE
                      , InfoCrit           = c("AIC","BIC","mR2Adj")
                      , InfoExtraOffset    = 10.
                      , CntAllomMin        = 30L
                      , CntAllomMax        = 10000L
                      , AllomConfInt       = 0.95
                      , AllomMaxItGain     = 5L
                      , AllomMaxItOptim    = 5000L
                      , AllomTolGain       = 1.e-6
                      , AllomTolOptim      = 1.e-8
                      , AllomTolBestMult   = 10.
                      , AllomMaxItBestFrac = 1.
                      , AllomTolMax        = 1.e-5
                      , AllomMaxItMin      = 500L
                      , AllomCntPred       = 101L
                      , AllomQuantPred     = TRUE
                      , AllomCntBoot       = 1000L
                      , UseSizeBins        = FALSE
                      , xSample            = ModelTRY$xName[1L]
                      , xLogSmp            = FALSE
                      , MinSmpPerBin       = 500L
                      , MaxCntBins         = 20L
                      , Verbose            = FALSE
                      ){


   #---~---
   #   Define information criterion to be used.
   #---~---
   InfoCrit        = match.arg(InfoCrit)
   if (InfoCrit %in% "mR2Adj"){
      XICFmt          = "%.3f"
      InfoExtraOffset = 0.
   }else{
      XICFmt          = "%.2f"
   }#end if (InfoCrit %in% "mR2Adj")
   #---~---


   #---~---
   #   Define function that retrieves the information criterion
   #---~---
   XIC = function(object,InfoCrit){
      ans = switch( InfoCrit
                  , AIC    = object$AIC
                  , BIC    = object$BIC
                  , mR2Adj = - object$wgt.r.squared
                  )#end switch
      return(ans)
   }#end function
   #---~---



   #---~---
   #   First, we trim the input data to entries in which all variables considered for the
   # model fitting are valid. This may delete rows that have valid entries for the simplest
   # models being considered, however, this is needed if we want to compare different 
   # models using information criteria.
   #---~---
   if (Verbose) cat0(" + Keep only rows with all data needed.")
   AllomVars = with(ModelTRY,unique(c(xName,wName,yName)))
   AllomVars = AllomVars[! is.na(AllomVars)]
   DataTRY   = DataTRY %>%
      filter_at( all_of(AllomVars), all_vars(! is.na(.)))
   #---~---


   #---~---
   #   Define ancillary tolerance values.
   #---~---
   AllomTolGainBest    = min(AllomTolMax, AllomTolBestMult * AllomTolGain )
   AllomTolOptimBest   = min(AllomTolMax, AllomTolBestMult * AllomTolOptim)
   AllomMaxItGainBest  = 1L
   AllomMaxItOptimBest = max(AllomMaxItMin, ceiling(AllomMaxItOptim * AllomMaxItBestFrac))
   #---~---


   #---~---
   #   Define useful quantities
   #   CntModel         -- Number of models to try.
   #   CntData          -- Number of training data samples.
   #   CntCategAllom    -- Number of category levels
   #   CntPredCateg     -- Number of predictions times number of categories 
   #   CntBootCateg     -- Number of bootstrap realisations times number of categories
   #   CntPredBoot      -- Number of predictions times number of bootstrap realisations
   #   CntBootPredCateg -- Number of bootstrap times predictions times categories
   #---~---
   if (Verbose) cat0(" + Define ancillary variables.")
   CntModel         = nrow(ModelTRY)
   CntData          = nrow(DataTRY )
   CntCategAllom    = nrow(CategAllom)
   CntPredCateg     = AllomCntPred * CntCategAllom
   CntBootCateg     = AllomCntBoot * CntCategAllom
   CntPredBoot      = AllomCntPred * AllomCntBoot
   CntBootPredCateg = AllomCntBoot * AllomCntPred * CntCategAllom
   #---~---




   #---~---
   #   Define some useful vectors for generating predictors for plots.
   #   - IndexX: Vector with the indices for variable X
   #   - AllomBootID: Vector with the bootstrap realisation index.
   #   - AllomBootPredID: Vector with the bootstrap ID for each predictor.
   #   - PredQuant: Vector with the quantiles that may be used for generating predictions.
   #---~---
   IndexX          = sequence(AllomCntPred)
   AllomBootID     = sequence(AllomCntBoot)
   AllomBootPredID = rep( x= AllomBootID, each=AllomCntPred)
   PredQuant       = seq(from=0,to=1,length.out=AllomCntPred)
   #---~---



   #---~---
   #---~---
   #   Initialise objects for allometry plots:
   #---~---
   if (Verbose) cat0(" + Initialise output structures.")
   #---~---
   # SummAllom is the tibble with the coefficients and a few goodness-of-fit metrics.
   #---~---
   SummAllom = tibble( Class        = CategAllom$Name
                     , TraitClass   = CategAllom$TraitClass
                     , DescClass    = CategAllom$DescClass
                     , N            = rep( x = 0L           , times = CntCategAllom )
                     , Function     = rep( x = NA_character_, times = CntCategAllom )
                     , Formula      = rep( x = NA_character_, times = CntCategAllom )
                     , Scedastic    = rep( x = NA_character_, times = CntCategAllom )
                     , xName        = rep( x = NA_character_, times = CntCategAllom )
                     , wName        = rep( x = NA_character_, times = CntCategAllom )
                     , yName        = rep( x = NA_character_, times = CntCategAllom )
                     , a0           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a0        = rep( x = NA_real_     , times = CntCategAllom )
                     , a1           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a1        = rep( x = NA_real_     , times = CntCategAllom )
                     , a2           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_a2        = rep( x = NA_real_     , times = CntCategAllom )
                     , s0           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_s0        = rep( x = NA_real_     , times = CntCategAllom )
                     , s1           = rep( x = NA_real_     , times = CntCategAllom )
                     , SE_s1        = rep( x = NA_real_     , times = CntCategAllom )
                     , LogLik       = rep( x = NA_real_     , times = CntCategAllom )
                     , Bias         = rep( x = NA_real_     , times = CntCategAllom )
                     , MAE          = rep( x = NA_real_     , times = CntCategAllom )
                     , RMSE         = rep( x = NA_real_     , times = CntCategAllom )
                     , wR2Adjust    = rep( x = NA_real_     , times = CntCategAllom )
                     , oR2Adjust    = rep( x = NA_real_     , times = CntCategAllom )
                     , AIC          = rep( x = NA_real_     , times = CntCategAllom )
                     , BIC          = rep( x = NA_real_     , times = CntCategAllom )
                     )#end tibble
   #---~---
   #   PredAllom is the tibble with the model predictions along the span of the predictors.
   #---~---
   PredAllom = tibble( Class      = rep( x = CategAllom$Name      , each  = AllomCntPred )
                     , TraitClass = rep( x = CategAllom$TraitClass, each  = AllomCntPred )
                     , DescClass  = rep( x = CategAllom$DescClass , each  = AllomCntPred )
                     , xID        = rep( x = IndexX               , times = CntCategAllom)
                     , xName      = rep( x = NA_character_        , times = CntPredCateg )
                     , wName      = rep( x = NA_character_        , times = CntPredCateg )
                     , yName      = rep( x = NA_character_        , times = CntPredCateg )
                     , x          = rep( x = NA_real_             , times = CntPredCateg )
                     , w          = rep( x = NA_real_             , times = CntPredCateg )
                     , y          = rep( x = NA_real_             , times = CntPredCateg )
                     , sigma      = rep( x = NA_real_             , times = CntPredCateg )
                     , yLwr       = rep( x = NA_real_             , times = CntPredCateg )
                     , yUpr       = rep( x = NA_real_             , times = CntPredCateg )
                     )#end tibble
   #---~---
   #   SummBoot is the tibble with the coefficient estimates from the bootstrapping.
   #---~---
   SummBoot = tibble( Class       = rep( x = CategAllom$Name      , each  = AllomCntBoot  )
                    , TraitClass  = rep( x = CategAllom$TraitClass, each  = AllomCntBoot  )
                    , DescClass   = rep( x = CategAllom$DescClass , each  = AllomCntBoot  )
                    , N           = rep( x = 0L                   , times = CntBootCateg  )
                    , BootID      = rep( x = AllomBootID          , times = CntCategAllom )
                    , Function    = rep( x = NA_character_        , times = CntBootCateg  )
                    , Formula     = rep( x = NA_character_        , times = CntBootCateg  )
                    , Scedastic   = rep( x = NA_character_        , times = CntBootCateg  )
                    , xName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , wName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , yName       = rep( x = NA_character_        , times = CntBootCateg  )
                    , a0          = rep( x = NA_real_             , times = CntBootCateg  )
                    , a1          = rep( x = NA_real_             , times = CntBootCateg  )
                    , a2          = rep( x = NA_real_             , times = CntBootCateg  )
                    , s0          = rep( x = NA_real_             , times = CntBootCateg  )
                    , s1          = rep( x = NA_real_             , times = CntBootCateg  )
                    , Bias        = rep( x = NA_real_             , times = CntBootCateg  )
                    , MAE         = rep( x = NA_real_             , times = CntBootCateg  )
                    , RMSE        = rep( x = NA_real_             , times = CntBootCateg  )
                    , wR2Adjust   = rep( x = NA_real_             , times = CntBootCateg  )
                    , oR2Adjust   = rep( x = NA_real_             , times = CntBootCateg  )
                    , xBias       = rep( x = NA_real_             , times = CntBootCateg  )
                    , xSigRes     = rep( x = NA_real_             , times = CntBootCateg  )
                    , xMAE        = rep( x = NA_real_             , times = CntBootCateg  )
                    )#end tibble
   #---~---
   #   PredBoot is the tibble with the model predictions along the span of the predictors
   # for each bootstrap realisation.
   #---~---
   PredBoot = tibble( Class      = rep( x = CategAllom$Name      , each  = CntPredBoot   )
                    , TraitClass = rep( x = CategAllom$TraitClass, each  = CntPredBoot   )
                    , DescClass  = rep( x = CategAllom$DescClass , each  = CntPredBoot   )
                    , BootID     = rep( x = AllomBootPredID   , times = CntCategAllom    )
                    , xID        = rep( x = IndexX            , times = CntBootCateg     )
                    , xName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , wName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , yName      = rep( x = NA_character_     , times = CntBootPredCateg )
                    , x          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , w          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , y          = rep( x = NA_real_          , times = CntBootPredCateg )
                    , sigma      = rep( x = NA_real_          , times = CntBootPredCateg )
                    )#end tibble
   #---~---



   #---~---
   #   Set variables to help deciding whether to fit the global and specific models only 
   # when needed.
   #---~---
   FitModel   = CntData %ge% CntAllomMin
   LoopGlobal = sequence(CntModel*as.integer(FitModel)*as.integer(UseFixedModel))
   #---~---



   #---~---
   #   If we are to use sampling, create a sample of data points that is evenly
   # distributed across size classes. Otherwise, the data for training is the original
   # data set unless it has so many points that it exceeds the maximum.
   #---~---
   TrainTRY = Allom_SetTrain( OrigTRY      = DataTRY
                            , CntAllomMax  = CntAllomMax
                            , UseSizeBins  = UseSizeBins
                            , xName        = xSample
                            , LogSmp       = xLogSmp
                            , MinSmpPerBin = MinSmpPerBin
                            , MaxCntBins   = MaxCntBins
                            )#end Allom_SetTrain
   #---~---



   #---~---
   #   If sought, find the best global model.
   #---~---
   gBest       = NA_integer_
   gCntParam   = +Inf
   gXIC        = +Inf
   gDesc       = "None. All models failed."
   gExpect     = NULL
   gFit        = NULL
   gSigma      = NULL
   gStartLSQ   = NA_character_
   gStartSigma = NA_character_
   if (Verbose && ( length(LoopGlobal) > 0)) cat0(" + Fit global model.")
   for (m in LoopGlobal){
      #---~---
      #   Retrieve settings for model fitting.
      #---~---
      mModel  = ModelTRY$Model [m]
      xName   = ModelTRY$xName [m]
      wName   = ModelTRY$wName [m]
      yName   = ModelTRY$yName [m]
      mLogSmp = ModelTRY$LogSmp[m]
      #---~---




      #---~---
      #   Set predictors, number of parameters and first guesses based on the functional 
      # form of the model.
      #---~---
      mSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                   , Model   = mModel
                                   , xName   = xName
                                   , yName   = yName
                                   , wName   = wName
                                   )#end Allom_PrepOptim
      mPredictor  = mSettings$Predictor
      mCntParam   = mSettings$CntParam
      mExpect     = mSettings$Expect
      mSigma      = mSettings$Sigma
      mStartLSQ   = mSettings$StartLSQ
      mStartSigma = mSettings$StartSigma
      #---~---




      #---~---
      #   Fit homoscedastic model
      #---~---
      mFitHomo   = 
         try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                    , sig.formula = NULL
                                    , data        = TrainTRY
                                    , lsq.first   = mStartLSQ
                                    , tol.gain    = AllomTolGainBest
                                    , tol.optim   = AllomTolOptimBest
                                    , maxit       = AllomMaxItGainBest
                                    , maxit.optim = AllomMaxItOptimBest
                                    , verbose     = FALSE
                                    )#end optim.lsq.htscd
            , silent = TRUE
            )#end try
      #---~---



      #---~---
      #   Retrieve information criterion and update guesses in case fitting was successful.
      #---~---
      if ("try-error" %in% is(mFitHomo)){
         #---~---
         #   Model could not be fitted. Discard model, but still try the heteroscedastic.
         #---~---
         mXICHomo   = +Inf
         if (Verbose){
            cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  Failed fitting.")
         }#end if (Verbose)
         #---~---
      }else{
         #---~---
         #   Use the fitted homoscedastic model parameters as first guess for 
         # the heteroscedastic model.
         #---~---
         mXICHomo         = XIC(mFitHomo,InfoCrit)
         mStartLSQ        = as.list(coef(mFitHomo))
         names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
         if (Verbose){
            cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
         }#end if (Verbose)
         #---~---
      }#end if ("try-error" %in% is(mFitHomo))
      #---~---


      #---~---
      #   Select model if this is the best model so far.
      #---~---
      mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
      if ( ( mXICHomo + mXICOff ) < gXIC ){
         gBest       = m
         gCntParam   = mCntParam
         gXIC        = mXICHomo
         gDesc       = paste0(mModel,". (Homoscedastic)")
         gExpect     = mExpect
         gSigma      = NULL
         gFit        = mFitHomo
         gSigma      = NULL
         gStartLSQ   = mStartLSQ
         gStartSigma = mStartSigma
      }#end if (mXICHomo < zXIC)
      #---~---


      #---~---
      #   Fit the heteroscedastic model. Add one parameter as we fit uncertainty too.
      #---~---
      mCntParam  = mCntParam + 1L
      mFitHete   = 
         try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                    , sig.formula = mSigma
                                    , data        = TrainTRY
                                    , lsq.first   = mStartLSQ
                                    , sig.first   = mStartSigma
                                    , tol.gain    = AllomTolGainBest
                                    , tol.optim   = AllomTolOptimBest
                                    , maxit       = AllomMaxItGainBest
                                    , maxit.optim = AllomMaxItOptimBest
                                    , verbose     = FALSE
                                    )#end optim.lsq.htscd
            , silent = TRUE
            )#end try
      #---~---


      #---~---
      #   Retrieve information criterion and update guesses in case fitting was successful.
      #---~---
      if ("try-error" %in% is(mFitHete)){
         #---~---
         #   Model could not be fitted. Discard model.
         #---~---
         mXICHete = + Inf
         if (Verbose){
            cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  Failed fitting.")
         }#end if (Verbose)
         #---~---
      }else{
         mXICHete         = XIC(mFitHete,InfoCrit)
         mStartLSQ        = as.list(coef(mFitHete))
         mStartLSQ        = mStartLSQ[grepl(pattern="^lsq\\.",x=names(mStartLSQ))]
         names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
         mStartSigma      = list( s1 = coef(mFitHete)["sig.s1"] )
         if (Verbose){
            cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                               ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHete),".")
         }#end if (Verbose)
      }#end if ("try-error" %in% is(mFitHete))
      #---~---


      #---~---
      #   Select model if this is the best model so far.
      #---~---
      mXICOff = InfoExtraOffset * as.numeric( mCntParam > gCntParam )
      if ( ( mXICHete + mXICOff ) < gXIC ){
         gBest       = m
         gCntParam   = mCntParam
         gXIC        = mXICHete
         gDesc       = paste0(mModel,". (Heteroscedastic)")
         gHete       = TRUE
         gExpect     = mExpect
         gSigma      = mSigma
         gFit        = mFitHete
         gStartLSQ   = mStartLSQ
         gStartSigma = mStartSigma
      }#end if (mXICHete < zXIC)
      #---~---
   }#end for (m in LoopGlobal)
   #---~---



   #---~---
   #   If we sought to fit a global model, we only loop through categories if we were 
   # successful finding a model.
   #---~---
   if (UseFixedModel){
      LoopCateg  = sequence(CntCategAllom*as.integer(FitModel)*is.finite(gXIC))
   }else{
      LoopCateg  = sequence(CntCategAllom*as.integer(FitModel))
   }#end if (UseFixedModel)
   #---~---



   #---~---
   #   Loop through all the categories and models.
   #---~---
   if (Verbose && (length(LoopCateg) > 0)) cat0(" + Fit allometric models by categories:")
   for (z in LoopCateg){
      #---~---
      #   Select category (or everything)
      #---~---
      zName      = SummAllom$TraitClass[z]
      zCateg     = SummAllom$Class     [z]
      zDescClass = SummAllom$DescClass [z]
      if (zName %in% "All"){
         zSel  = rep(TRUE,times=CntData)
      }else{
         zSel  = DataTRY[[zName]] %in% zCateg
      }#end if (zName %in% "All")
      #---~---


      #---~---
      #   Subset data
      #---~---
      SubsetTRY = DataTRY[zSel,,drop=FALSE]
      CntSubset = nrow(SubsetTRY)
      #---~---



      #---~---
      #   If we are to use sampling, create a sample of data points that is evenly
      # distributed across size classes. Otherwise, the data for training is the original
      # data set unless it has so many points that it exceeds the maximum.
      #---~---
      TrainTRY = Allom_SetTrain( OrigTRY      = SubsetTRY
                               , CntAllomMax  = CntAllomMax
                               , UseSizeBins  = UseSizeBins
                               , xName        = xSample
                               , LogSmp       = xLogSmp
                               , MinSmpPerBin = MinSmpPerBin
                               , MaxCntBins   = MaxCntBins
                               )#end Allom_SetTrain
      #---~---


      #---~---
      #   Decide whether or not to test multiple models for this category. If using the 
      # same functional form for all classes, we skip the test and use the best global
      # model.
      #---~---
      FitModel = CntSubset %ge% CntAllomMin
      if (UseFixedModel && FitModel){
         LoopModel   = integer(0L)
         zBest       = gBest
         zCntParam   = gCntParam
         zXIC        = gXIC
         zDesc       = gDesc
         zExpect     = gExpect
         zFit        = gFit
         zSigma      = gSigma
         zStartLSQ   = gStartLSQ
         zStartSigma = gStartSigma
      }else if (FitModel){
         LoopModel   = LoopGlobal
         zBest       = NA_integer_
         zCntParam   = +Inf
         zXIC        = +Inf
         zDesc       = "None. All models failed."
         zExpect     = NULL
         zFit        = NULL
         zSigma      = NULL
         zStartLSQ   = NA_character_
         zStartSigma = NA_character_
      }else{
         LoopModel   = integer(0L)
         zBest       = NA_integer_
         zCntParam   = +Inf
         zXIC        = +Inf
         zDesc       = "None. All models failed."
         zExpect     = NULL
         zFit        = NULL
         zSigma      = NULL
         zStartLSQ   = NA_character_
         zStartSigma = NA_character_
      }#end if (UseFixedModel && FitModel)
      #---~---


      #---~---
      #   Initialise best model with nothing.
      #---~---
      for (m in LoopModel){
         #---~---
         #   Retrieve settings for model fitting.
         #---~---
         mModel  = ModelTRY$Model [m]
         xName   = ModelTRY$xName [m]
         wName   = ModelTRY$wName [m]
         yName   = ModelTRY$yName [m]
         mLogSmp = ModelTRY$LogSmp[m]
         #---~---



         #---~---
         #   First, we find some information about predictors and predictand that may be
         # useful for defining first guesses.
         #---~---
         qLwrX = quantile(x=TrainTRY[[xName]],probs=0.025,names=FALSE,na.rm=TRUE)
         qMidX = quantile(x=TrainTRY[[xName]],probs=0.500,names=FALSE,na.rm=TRUE)
         qUprX = quantile(x=TrainTRY[[xName]],probs=0.975,names=FALSE,na.rm=TRUE)
         qLwrY = quantile(x=TrainTRY[[yName]],probs=0.025,names=FALSE,na.rm=TRUE)
         qMidY = quantile(x=TrainTRY[[yName]],probs=0.500,names=FALSE,na.rm=TRUE)
         qUprY = quantile(x=TrainTRY[[yName]],probs=0.975,names=FALSE,na.rm=TRUE)
         if (is.na(wName)){
            qMidW = NA_real_
         }else{
            qMidW = quantile(x=TrainTRY[[wName]],probs=0.500,names=FALSE,na.rm=TRUE)
         }#end if (is.na(wName))
         #---~---




         #---~---
         #   Set predictors, number of parameters and first guesses based on the functional 
         # form of the model.
         #---~---
         mSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                      , Model   = mModel
                                      , xName   = xName
                                      , yName   = yName
                                      , wName   = wName
                                      )#end Allom_PrepOptim
         mPredictor  = mSettings$Predictor
         mCntParam   = mSettings$CntParam
         mExpect     = mSettings$Expect
         mSigma      = mSettings$Sigma
         mStartLSQ   = mSettings$StartLSQ
         mStartSigma = mSettings$StartSigma
         #---~---



         #---~---
         #   Fit homoscedastic model
         #---~---
         mFitHomo   = 
            try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                       , sig.formula = NULL
                                       , data        = TrainTRY
                                       , lsq.first   = mStartLSQ
                                       , tol.gain    = AllomTolGainBest
                                       , tol.optim   = AllomTolOptimBest
                                       , maxit       = AllomMaxItGainBest
                                       , maxit.optim = AllomMaxItOptimBest
                                       , verbose     = FALSE
                                       )#end optim.lsq.htscd
               , silent = TRUE
               )#end try
         if ("try-error" %in% is(mFitHomo)){
            #---~---
            #   Model could not be fitted. Discard model and try luck with heteroscedastic.
            #---~---
            mXICHomo  = +Inf
            if (Verbose){
               cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  Failed fitting.")
            }#end if (Verbose)
            #---~---
         }else{
            #---~---
            #   Use the fitted homoscedastic model parameters as first guess for 
            # the heteroscedastic model.
            #---~---
            mXICHomo         = XIC(mFitHomo,InfoCrit)
            mStartLSQ        = as.list(coef(mFitHomo))
            names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
            if (Verbose){
               cat0("   - ",mModel,",  homoscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
            }#end if (Verbose)
            #---~---
         }#end if ("try-error" %in% is(mFitHomo))
         #---~---


         #---~---
         #   Select model if this is the best model so far.
         #---~---
         mXICOff = InfoExtraOffset * as.numeric( mCntParam > zCntParam )
         if ( ( mXICHomo + mXICOff ) < zXIC ){
            zBest       = m
            zCntParam   = mCntParam
            zXIC        = mXICHomo
            zDesc       = paste0(mModel,". (Homoscedastic)")
            zExpect     = mExpect
            zFit        = mFitHomo
            zSigma      = NULL
            zStartLSQ   = mStartLSQ
            zStartSigma = mStartSigma
         }#end if (mXICHomo < zXIC)
         #---~---


         #---~---
         #   Fit the heteroscedastic model. Add one parameter as we fit uncertainties too.
         #---~---
         mCntParam  = mCntParam + 1L
         mFitHete   = 
            try( expr = optim.lsq.htscd( lsq.formula = mExpect
                                       , sig.formula = mSigma
                                       , data        = TrainTRY
                                       , lsq.first   = mStartLSQ
                                       , sig.first   = mStartSigma
                                       , tol.gain    = AllomTolGainBest
                                       , tol.optim   = AllomTolOptimBest
                                       , maxit       = AllomMaxItGainBest
                                       , maxit.optim = AllomMaxItOptimBest
                                       , verbose     = FALSE
                                       )#end optim.lsq.htscd
               , silent = TRUE
               )#end try
         #---~---


         #---~---
         #   Retrieve information criterion and update guesses in case fitting was 
         # successful.
         #---~---
         if ("try-error" %in% is(mFitHete)){
            #---~---
            #   Model could not be fitted. Discard model.
            #---~---
            mXICHete = + Inf
            if (Verbose){
               cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHomo),".")
            }#end if (Verbose)
            #---~---
         }else{
            mXICHete         = XIC(mFitHete,InfoCrit)
            mStartLSQ        = as.list(coef(mFitHete))
            mStartLSQ        = mStartLSQ[grepl(pattern="^lsq\\.",x=names(mStartLSQ))]
            names(mStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(mStartLSQ))
            mStartSigma      = list( s1 = coef(mFitHete)["sig.s1"] )
            if (Verbose){
               cat0("   - ",mModel,",  heteroscedastic "," (X = ",xName,"; W = ",wName,")."
                                  ,"  ",InfoCrit," = ",sprintf(XICFmt,mXICHete),".")
            }#end if (Verbose)
         }#end if ("try-error" %in% is(mFitHete))
         #---~---


         #---~---
         #   Select model if this is the best model so far.
         #---~---
         mXICOff = InfoExtraOffset * as.numeric( mCntParam > zCntParam )
         if ( ( mXICHete + mXICOff ) < zXIC ){
            zBest       = m
            zCntParam   = mCntParam
            zXIC        = mXICHete
            zDesc       = paste0(mModel,". (Heteroscedastic)")
            zHete       = TRUE
            zExpect     = mExpect
            zFit        = mFitHete
            zSigma      = mSigma
            zStartLSQ   = mStartLSQ
            zStartSigma = mStartSigma
         }#end if (mXICHete < zXIC)
         #---~---
      }#end for (m in LoopModel)
      #---~---



      #---~---
      #   Select model if this is the best model so far. If all model fittings failed,
      # cycle to the next class/category.
      #---~---
      if (is.na(zBest)){
         warning("   - Failed fitting models for: ",zDescClass,", category: ",zCateg,".")
         next
      }else{
         zModel  = ModelTRY$Model [zBest]
         xName   = ModelTRY$xName [zBest]
         wName   = ModelTRY$wName [zBest]
         yName   = ModelTRY$yName [zBest]
         zLogSmp = ModelTRY$LogSmp[zBest]
         if (Verbose){
            cat0("   - Selected model: ",zDesc,"; (X = ",xName,"; W = ",wName,")."
                                              ," Find coefficients and uncertainties.")
         }#end if (Verbose){
      }#end if (is.na(zBest))
      #---~---



      #---~---
      #   Set predictors, number of parameters and first guesses based on the functional 
      # form of the model.
      #---~---
      zSettings   = Allom_PrepOptim( OrigTRY = TrainTRY
                                   , Model   = zModel
                                   , xName   = xName
                                   , yName   = yName
                                   , wName   = wName
                                   )#end Allom_PrepOptim
      zPredictor  = zSettings$Predictor
      zCntParam   = zSettings$CntParam
      zStartLSQ   = zSettings$StartLSQ
      zStartSigma = if(is.null(zSigma)){NULL}else{zSettings$StartSigma}
      #---~---



      #---~---
      #   Fit the model with stricter tolerance and also generate bootstrapped estimates.
      #---~---
      if (is.null(zSigma)){
         zFit = try( expr   = optim.lsq.htscd( lsq.formula = zExpect
                                             , sig.formula = NULL
                                             , data        = TrainTRY
                                             , lsq.first   = zStartLSQ
                                             , err.method  = "bootstrap"
                                             , tol.gain    = AllomTolGain
                                             , tol.optim   = AllomTolOptim
                                             , maxit       = AllomMaxItGain
                                             , maxit.optim = AllomMaxItOptim
                                             , n.boot      = AllomCntBoot
                                             , verbose     = TRUE
                                             )#end optim.lsq.htscd
                   , silent = TRUE
                   )#end try
      }else{
         #---~---
         #   Fit the homoscedastic model first to bring coefficients closer to the answer, 
         # then fit the heteroscedastic model.
         #---~---
         zFitHomo = try( expr   = optim.lsq.htscd( lsq.formula = zExpect
                                                 , sig.formula = NULL
                                                 , data        = TrainTRY
                                                 , lsq.first   = zStartLSQ
                                                 , tol.gain    = AllomTolGainBest
                                                 , tol.optim   = AllomTolOptimBest
                                                 , maxit       = AllomMaxItGainBest
                                                 , maxit.optim = AllomMaxItOptimBest
                                                 , verbose     = FALSE
                                                 )#end optim.lsq.htscd
                       , silent = TRUE
                       )#end try
         zStartLSQ        = as.list(coef(zFitHomo))
         names(zStartLSQ) = gsub(pattern="^lsq\\.",replacement="",x=names(zStartLSQ))
         #---~---



         #---~---
         #   Fit the heteroscedastic model.
         #---~---
         zFit = try( expr   = optim.lsq.htscd( lsq.formula = zExpect
                                             , sig.formula = zSigma
                                             , data        = TrainTRY
                                             , lsq.first   = zStartLSQ
                                             , sig.first   = zStartSigma
                                             , err.method  = "bootstrap"
                                             , tol.gain    = AllomTolGain
                                             , tol.optim   = AllomTolOptim
                                             , maxit       = AllomMaxItGain
                                             , maxit.optim = AllomMaxItOptim
                                             , n.boot      = AllomCntBoot
                                             , verbose     = TRUE
                                             )#end optim.lsq.htscd
                   , silent = TRUE
                   )#end try
         #---~---
      }#end if (is.null(zSigma))
      #---~---


      if (Verbose) cat0("   - Best model (",zDescClass,", category ",zCateg,"): ",zDesc,".")
      zModel     = ModelTRY$Model [zBest]
      zFormula   = as.character(zExpect)
      zScedastic = if(is.null(zSigma)){"Homoscedastic"}else{"Heteroscedastic"}
      xName      = ModelTRY$xName [zBest]
      wName      = ModelTRY$wName [zBest]
      yName      = ModelTRY$yName [zBest]
      zLogSmp    = ModelTRY$LogSmp[zBest]
      #---~---




      #---~---
      #---~---
      #   Find the range of the "x" predictor and the average of the "w" predictor (if 
      # needed) for the for fitted model.
      #---~---
      #---~---
      #   Decide approach for x variable.
      #---~---
      if (AllomQuantPred){
         #---~---
         #   Predictors based on equally spaced quantiles (we do not use the subsets).
         #---~---
         PredX   = quantile(x=DataTRY[[xName]],probs=PredQuant,names=FALSE,na.rm=TRUE)
         #---~---
      }else{
         #---~---
         #   Predictors based on equally spaced points across range.
         #---~---
         minX  = min(DataTRY[[xName]],na.rm=TRUE)
         maxX  = max(DataTRY[[xName]],na,rm=TRUE)
         if (zLogSmp){
            PredX = exp(seq(from=log(minX),to=log(maxX),length.out=AllomCntPred))
         }else{
            PredX = seq(from=minX,to=maxX,length.out=AllomCntPred)
         }#end if (zLogSmp)
         #---~---
      }#end if if (AllomQuantPred)
      #---~---
      #   For w variable, we fix it at the mean (or set it to NA when not used).
      #---~---
      if (is.na(wName)){
         PredW = rep(x=NA_real_,times=AllomCntPred)
      }else{
         PredW = rep(x=mean(x=DataTRY[[wName]],na.rm=TRUE),times=AllomCntPred)
      }#end if (is.na(wName))
      #---~---



      #---~---
      #   Retrieve parameters, add dummy parameters for the ones that were not used.
      #---~---
      if (Verbose) cat0("     ~ Retrieve parameters and save goodness-of-fit metrics.")
      zParam        = coef(zFit)
      names(zParam) = gsub(pattern="^lsq\\." ,replacement=""  ,x=names(zParam))
      names(zParam) = gsub(pattern="^sig\\." ,replacement=""  ,x=names(zParam))
      if (! "a1" %in% names(zParam)) zParam["a1"] = NA_real_
      if (! "a2" %in% names(zParam)) zParam["a2"] = NA_real_
      if (! "s0" %in% names(zParam)) zParam["s0"] = zFit$sigma0
      if (! "s1" %in% names(zParam)) zParam["s1"] = 0.
      zParam        = zParam[c("a0","a1","a2","s0","s1")]
      #---~---


      #---~---
      #   Set initial guesses for the bootstrap fitting.
      #---~---
      zStartLSQ   = zParam[(! is.na(zParam)) & (! names(zParam) %in% c("s0","s1") )]
      zStartSigma = if(is.null(zSigma)){NULL}else{zParam["s1"]}
      #---~---



      #---~---
      #   Find the predicted height based on the original data and find goodness-of-fit 
      # metrics.
      #---~---
      yOrig = TrainTRY[[yName]]
      yPred = Allom_Pred( x     = TrainTRY[[xName]]
                        , w     = TrainTRY[[wName]]
                        , param = zParam
                        , fun   = zModel
                        , ans   = "mu"
                        )#end try TRY_AllomPred
      ySigma = Allom_Pred( x     = TrainTRY[[xName]]
                         , w     = TrainTRY[[wName]]
                         , param = zParam
                         , fun   = zModel
                         , ans   = "sigma"
                         )#end try TRY_AllomPred
      #---~---


      #---~---
      #   Copy information to the summary.
      #---~---
      saIdx = with(SummAllom, which(( Class %in% zCateg) & ( TraitClass %in% zName )))
      SummAllom$N        [saIdx] = CntSubset
      SummAllom$Function [saIdx] = zModel
      SummAllom$Formula  [saIdx] = zFormula
      SummAllom$Scedastic[saIdx] = zScedastic
      SummAllom$xName    [saIdx] = xName
      SummAllom$wName    [saIdx] = wName
      SummAllom$yName    [saIdx] = yName
      SummAllom$a0       [saIdx] = zParam["a0"]
      SummAllom$a1       [saIdx] = zParam["a1"]
      SummAllom$a2       [saIdx] = zParam["a2"]
      SummAllom$s0       [saIdx] = zParam["s0"]
      SummAllom$s1       [saIdx] = zParam["s1"]
      SummAllom$LogLik   [saIdx] = logLik(zFit)
      SummAllom$Bias     [saIdx] = Allom_Bias (yOrig=yOrig,yPred=yPred)
      SummAllom$MAE      [saIdx] = Allom_MAE  (yOrig=yOrig,yPred=yPred)
      SummAllom$RMSE     [saIdx] = Allom_RMSE (yOrig=yOrig,yPred=yPred,nParam=zCntParam)
      SummAllom$wR2Adjust[saIdx] = Allom_R2Adj( yOrig  = yOrig
                                              , yPred  = yPred
                                              , ySigma = ySigma
                                              , nParam = zCntParam
                                              )#end Allom_R2Adj
      SummAllom$oR2Adjust[saIdx] = Allom_R2Adj( yOrig  = yOrig
                                              , yPred  = yPred
                                              , nParam = zCntParam
                                              )#end Allom_R2Adj
      SummAllom$AIC      [saIdx] = AIC(zFit)
      SummAllom$BIC      [saIdx] = BIC(zFit)
      #---~---


      #---~---
      #   Predict model based on selected function.
      #---~---
      spIdx   = with( PredAllom
                    , which( ( Class %in% zCateg ) & ( TraitClass %in% zName ) )
                    )#end with
      PredAllom$xName [spIdx] = xName
      PredAllom$wName [spIdx] = wName
      PredAllom$yName [spIdx] = yName
      PredAllom$x     [spIdx] = PredX
      PredAllom$w     [spIdx] = PredW
      PredAllom$y     [spIdx] = Allom_Pred( x     = PredX
                                          , w     = PredW
                                          , param = zParam
                                          , fun   = zModel
                                          , ans   = "mu"
                                          )#end Allom_Pred
      PredAllom$sigma [spIdx] = Allom_Pred( x     = PredX
                                          , w     = PredW
                                          , param = zParam
                                          , fun   = zModel
                                          , ans   = "sigma"
                                          )#end Allom_Pred
      #---~---



      #---~---
      #   Iterate over the bootstrap to obtain the parameter distribution.
      #---~---
      BootLoop = sequence(AllomCntBoot*is.finite(zBest))
      if (Verbose && ( length(BootLoop) > 0L )){
         cat0("     ~ Calculate predictions using bootstrapped model: ")
         ProgBar = txtProgressBar(max=AllomCntBoot,char=".",style=3L)
      }#end if (Verbose && ( length(BootLoop) > 0L ))
      for (b in BootLoop){
         #---~---
         #   Report progress.
         #---~---
         if (Verbose) dummy = setTxtProgressBar(pb=ProgBar,value=b)
         #---~---


         #---~---
         #   Retrieve parameters, add dummy parameters for the ones that were not used.
         #---~---
         bParam        = zFit$coeff.boot[b,]
         names(bParam) = gsub(pattern="^lsq\\." ,replacement=""  ,x=names(bParam))
         names(bParam) = gsub(pattern="^sig\\." ,replacement=""  ,x=names(bParam))
         if (! "a1" %in% names(bParam)) bParam["a1"] = NA_real_
         if (! "a2" %in% names(bParam)) bParam["a2"] = NA_real_
         if (! "s0" %in% names(bParam)) bParam["s0"] = zFit$sigma0.boot[b]
         if (! "s1" %in% names(bParam)) bParam["s1"] = 0.
         bParam        = bParam[c("a0","a1","a2","s0","s1")]
         #---~---


         #---~---
         #   Copy information to the summary.
         #---~---
         bsIdx = with( SummBoot, which( ( Class  %in% zCateg ) & ( TraitClass %in% zName )
                                      & ( BootID %in% b      )
                                      )#end which
                     )#end with
         SummBoot$N        [bsIdx] = CntSubset
         SummBoot$Function [bsIdx] = zModel
         SummBoot$Formula  [bsIdx] = zFormula
         SummBoot$Scedastic[bsIdx] = zScedastic
         SummBoot$xName    [bsIdx] = xName
         SummBoot$wName    [bsIdx] = wName
         SummBoot$yName    [bsIdx] = yName
         SummBoot$a0       [bsIdx] = bParam["a0"]
         SummBoot$a1       [bsIdx] = bParam["a1"]
         SummBoot$a2       [bsIdx] = bParam["a2"]
         SummBoot$s0       [bsIdx] = bParam["s0"]
         SummBoot$s1       [bsIdx] = bParam["s1"]
         SummBoot$LogLik   [bsIdx] = zFit$support.boot                 [b]
         SummBoot$Bias     [bsIdx] = zFit$boot.train$goodness$bias     [b]
         SummBoot$MAE      [bsIdx] = zFit$boot.train$goodness$mae      [b]
         SummBoot$RMSE     [bsIdx] = zFit$boot.train$goodness$rmse     [b]
         SummBoot$wR2Adjust[bsIdx] = zFit$boot.train$wgt.r.squared     [b]
         SummBoot$oR2Adjust[bsIdx] = zFit$boot.train$r.squared         [b]
         SummBoot$xBias    [bsIdx] = zFit$cross.val$bias               [b]
         SummBoot$xSigRes  [bsIdx] = zFit$cross.val$sigma              [b]
         SummBoot$xMAE     [bsIdx] = zFit$cross.val$mae                [b]
         #---~---


         #---~---
         #   Predict model based on current function
         #---~---
         bpIdx = with( PredBoot, which( ( Class  %in% zCateg ) & ( TraitClass %in% zName )
                                      & ( BootID %in% b      )
                                      )#end which
                     )#end with
         PredBoot$xName [bpIdx] = xName
         PredBoot$wName [bpIdx] = wName
         PredBoot$yName [bpIdx] = yName
         PredBoot$x     [bpIdx] = PredX
         PredBoot$w     [bpIdx] = PredW
         PredBoot$y     [bpIdx] = Allom_Pred( x     = PredX
                                            , w     = PredW
                                            , param = bParam
                                            , fun   = zModel
                                            , ans   = "mu"
                                            )#end Allom_Pred
         PredBoot$sigma [bpIdx] = Allom_Pred( x     = PredX
                                            , w     = PredW
                                            , param = bParam
                                            , fun   = zModel
                                            , ans   = "sigma"
                                            )#end Allom_Pred
         #---~---
      }#end for (b in sequence(AllomCntBoot))
      if (Verbose) dummy = close(ProgBar)
      #---~---

      # Find confidence interval for parameters
      StatSumm = SummBoot %>%
         filter( (Class %in% zCateg) & (TraitClass %in% zName) ) %>%
         summarise( SE_a0 = sd(x=a0,na.rm=TRUE)
                  , SE_a1 = sd(x=a1,na.rm=TRUE)
                  , SE_a2 = sd(x=a2,na.rm=TRUE)
                  , SE_s0 = sd(x=s0,na.rm=TRUE)
                  , SE_s1 = sd(x=s1,na.rm=TRUE) )

      # Find confidence interval for y predictions
      StatPred = PredBoot %>%
         filter( (Class %in% zCateg) & (TraitClass %in% zName) ) %>%
         group_by( xID ) %>%
         summarise( yLwr = quantile(x=y,probs=AllomConfLwr,names=FALSE,na.rm=TRUE)
                  , yUpr = quantile(x=y,probs=AllomConfUpr,names=FALSE,na.rm=TRUE) ) %>%
         ungroup()

      #---~---
      #   Copy data back to the main structure
      #---~---
      SummAllom$SE_a0[saIdx] = StatSumm$SE_a0
      SummAllom$SE_a1[saIdx] = StatSumm$SE_a1
      SummAllom$SE_a2[saIdx] = StatSumm$SE_a2
      SummAllom$SE_s0[saIdx] = StatSumm$SE_s0
      SummAllom$SE_s1[saIdx] = StatSumm$SE_s1
      PredAllom$yLwr [spIdx] = StatPred$yLwr
      PredAllom$yUpr [spIdx] = StatPred$yUpr
      #---~---
   }#end for (z in LoopCateg)
   #---~---

   #---~---
   #   Build list with the elements
   #---~---
   Answer = list( SummAllom = SummAllom, PredAllom = PredAllom
                , SummBoot  = SummBoot , PredBoot  = PredBoot  )
   return(Answer)
   #---~---
}#end function Allom_Fit
#---~---



#---~---
#   Generic function that predicts height from DBH (or the standard deviation of residuals)
#
#   Input variables:
#   x     -- First predictor, usually some measurement of tree size (e.g., dbh, height, 
#             dbh^2*height).
#   w     -- Second predictor, usually some mass-related scale (e.g., wood density, LMA).
#   param -- Set of parameters.  It should be a named vector with the following names
#            + a0 -- Scaling parameter
#            + a1 -- Typically a power parameter, but it depends on the function. Not used
#                    depending on the function.
#            + a2 -- Typically a power parameter, but it depends on the function. Not used
#                    depending on the function.
#            + s0 -- Scale factor for local sigma (when model is heteroscedastic).
#            + s1 -- Power factor for local sigma (when model is heteroscedastic).
#   fun   -- Which functional form to use. Currently the following options are acceptable:
#            + OneLinear   : y = a0 * x
#            + OneLogLinear: y = a0 * x^a1
#            + TwoLinear   : y = a0 * x    * w
#            + TwoMixLinear: y = a0 * x^a1 * w
#            + TwoLogLinear: y = a0 * x^a1 * w^a2
#            + MartinezCano: y = a0 * x^a1 / ( a2 + x^a1 )
#            + Weibull     : y = a0 * (1 - exp (-a1 * x^a2 ))
#   ans   -- What to return. "mu" returns the predicted value, "sigma" returns the local
#            standard deviation of the residuals.
#---~---
Allom_Pred <<- function( x
                       , w     = NULL
                       , param
                       , fun   = c( "OneLinear"   , "OneLogLinear", "TwoLinear"
                                  , "TwoMixLinear", "TwoLogLinear", "MartinezCano"
                                  , "Weibull"     )
                       , ans   = c("mu","sigma")
                       ){

   #---~---
   #   Match the arguments
   #---~---
   fun = match.arg(fun)
   ans = match.arg(ans)
   #---~---



   #---~---
   #   Find the expected value
   #---~---
   if (fun %in% "OneLinear"){
      mu = param["a0"] * x
   }else if (fun %in% "OneLogLinear"){
      mu = param["a0"] * x^param["a1"]
   }else if (fun %in% "TwoLinear"){
      mu = param["a0"] * x * w
   }else if (fun %in% "TwoMixLinear"){
      mu = param["a0"] * x^param["a1"] * w
   }else if (fun %in% "TwoLogLinear"){
      mu = param["a0"] * x^param["a1"] * w^param["a2"]
   }else if (fun %in% "MartinezCano"){
      mu = param["a0"] * x^param["a1"] / ( param["a2"] + x^param["a1"] )
   }else if (fun %in% "Weibull"){
      mu = param["a0"] * ( 1. - exp( - param["a1"] * x^param["a2"] ) )
   }#end if
   #---~---


   #---~---
   #   Decide what to return
   #---~---
   if (ans %in% "mu"){
      #---~---
      #   Expected value
      #---~---
      return(mu)
      #---~---
   }else{
      #---~---
      #   Find the local standard deviation of the parameters
      #---~---
      sigma = param["s0"] * mu^param["s1"]
      return(sigma)
      #---~---
   }#end if (ans %in% "mu")
   #---~---
}#end Allom_Pred
#---~---



#---~---
#     This function computes the adjusted R2 for allometric models. This is intended to be
# used for full-model assessment only, as it inherently assumes that the model is unbiased.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# ySigma - vector with the predicted values of standard deviation of the residuals.
# nParam - number of model parameters
#---~---
Allom_R2Adj <<- function(yOrig,yPred,ySigma=NULL,nParam){

   #---~---
   #   Assign a default sigma if it is missing.
   #---~---
   if (is.null(ySigma)) ySigma = 0*yOrig + sd(yOrig-yPred,na.rm=TRUE)
   #---~---


   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep = is.finite(yOrig) & is.finite(yPred) & is.finite(ySigma)
   yOrig = yOrig[yKeep]
   yPred = yPred[yKeep]
   #---~---


   #---~---
   #   Find the weights and transform the data.
   #---~---
   yWeight  = ( sd(yOrig-yPred) / ySigma )^2
   ysOrig   = sqrt(yWeight) * yOrig
   ysPred   = sqrt(yWeight) * yPred
   #---~---


   #---~---
   #   Find the average of the transformed data.
   #---~---
   ysMean = mean(ysOrig)
   nData  = length(ysOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom
   #---~---
   SSRes = sum((ysOrig-ysPred)^2)
   SSTot = sum((ysOrig-ysMean)^2)
   DFRes = nData - nParam
   DFTot = nData - 1
   #---~---


   #---~---
   #   Find the adjusted R2 and report it.
   #---~---
   r2adj = 1 - SSRes * DFTot / ( SSTot * DFRes )
   return(r2adj)
   #---~---
}#end Allom_R2Adj
#---~---



#---~---
#     This function computes the root mean square error of prediction 
# for allometric models. This is intended to be used for full-model assessment and not
# cross-validation, as it accounts for number of degrees of freedom.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
# nParam - number of model parameters
#---~---
Allom_RMSE <<- function(yOrig,yPred,nParam){
   
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep = is.finite(yOrig) & is.finite(yPred)
   yOrig = yOrig[yKeep]
   yPred = yPred[yKeep]
   yMean = mean(yOrig)
   nData = length(yOrig)
   #---~---


   #---~---
   #   Find sum of squares and degrees of freedom, then compute the RMSE of predictions
   #---~---
   SSRes = sum((yOrig-yPred)^2)
   DFRes = nData - nParam
   ans   = sqrt(SSRes/DFRes)
   return(ans)
   #---~---
}#end Allom_RMSE
#---~---



#---~---
#     This function computes the mean absolute error. This can be used for both the full
# model assessment and cross-validation.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_MAE <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(abs(yResid))
   return(ans)
   #---~---
}#end Allom_MAE
#---~---



#---~---
#     This function computes the mean bias (negative average of the residuals). This is 
# intended to be used mostly for cross-validation.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_Bias <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find mean bias
   #---~---
   ans   = mean(-yResid)
   return(ans)
   #---~---
}#end Allom_Bias
#---~---



#---~---
#     This function computes the standard deviation of residuals. This is not the same as 
# the RMSE because it does not account for bias and the number of parameters. This is 
# intended to be used mostly for cross-validation.
#
# Input variables:
# yOrig  - vector with original values of variable y
# yPred  - vector with predicted values of variable y
#---~---
Allom_SigRes <<- function(yOrig,yPred){
   #---~---
   #   Keep only the entries in which both yOrig and yPred are valid
   #---~---
   yKeep  = is.finite(yOrig) & is.finite(yPred)
   yOrig  = yOrig[yKeep]
   yPred  = yPred[yKeep]
   yResid = yOrig - yPred
   #---~---


   #---~---
   #   Find standard deviation of residuals
   #---~---
   ans   = sd(yResid)
   return(ans)
   #---~---
}#end Allom_SigRes
#---~---





#---~---
#   This function sets the training data such that it does not exceed the maximum number 
# of training points. Depending on the options, it will also resample the data to ensure 
# the sampling effort is balanced across the sample size.
#
# Inputs
#
# + OrigTRY      -- Original data frame to be used as training data.
# + CntAllomMax  -- Maximum number of points to consider for fitting. This is used to
#                   limit the model fitting to a more manageable number of 
#                   observations when allometric data are exceedingly large. If no
#                   cap is sought, set CntAllomMax = +Inf. This setting is 
#                   independent on the data binning sampling defined by UseSizeBins.
# + UseSizeBins  -- Use binning across size classes to have a more balanced 
#                   distribution across the predictor range? Note that in this case
#                   we cannot use AIC or BIC to define the best model, and thus we
#                   simply use adjusted R2.
# + xName        -- Name of the column with size data.
# + LogSmp       -- Apply log transformation when defining the bin boundaries?
# + MinSmpPerBin -- Minimum number of observations for each bin. Binning will not 
#                   occur unless there are at least twice as many valid points (and
#                   possibly even more points in case of very imbalanced data sets).
# + MaxCntBins   -- Maximum number of bins to be considered.
#---~---
Allom_SetTrain <<- function( OrigTRY
                           , CntAllomMax  = 10000L
                           , UseSizeBins  = FALSE
                           , xName
                           , LogSmp       = FALSE
                           , MinSmpPerBin = 500L
                           , MaxCntBins   = 20L
                           ){

   #---~---
   #   Find the number of valid data points and a few ancillary variables.
   #---~---
   CntOrig   = nrow(OrigTRY)            # Original sample size
   OrigIndex = sequence(CntOrig)        # Vector indices of the original sample
   CntUpper  = min(CntOrig,CntAllomMax) # Upper limit for the sampling effort
   #---~---


   #---~---
   #   If we are to use sampling, create a sample of data points that is evenly
   # distributed across size classes.
   #---~---
   if (UseSizeBins){
      #---~---
      #   Find the bounds for the binning; we use quantiles to avoid outliers stretching
      # the bins.
      #---~---
      qLwrX = quantile(x=OrigTRY[[xName]],probs=0.01,names=FALSE,na.rm=TRUE)
      qUprX = quantile(x=OrigTRY[[xName]],probs=0.99,names=FALSE,na.rm=TRUE)
      #---~---



      #---~---
      #   First guess for number of bins.
      #---~---
      InitCntBins = max(1L, min(MaxCntBins, floor(CntOrig / MinSmpPerBin)))
      #---~---



      #---~---
      #   Iterate until all bins have at least the minimum number of samples. In case
      # the data does not have enough points for binning, we give up on re-sampling.
      #---~---
      CntBins = InitCntBins + 1L
      iterate = TRUE
      while (iterate){
         CntBins     = CntBins - 1L
         CntBrks     = CntBins + 1L


         #---~---
         #   If CntBins is 1, we give up on binning data, as we don't have enough data 
         # points.
         #---~---
         if (CntBins %eq% 1L){
            #---~---
            #   Stop iterating.
            #---~---
            iterate = FALSE
            #---~---

            #---~---
            #   No binning, but we still sample in case the number of data points 
            # exceeds the maximum.
            #---~---
            xIndex   = Allom_Sample(x=OrigIndex,size=CntUpper)
            TrainTRY = OrigTRY[xIndex,,drop=FALSE]
            #---~---
         }else{
            #---~---
            #   Find the number of bins. We don't split in quantiles, but in equal width
            # bins because the idea is to balance the range of samples.
            #---~---
            if (LogSmp){
               qBrksX          = exp(seq(from=log(qLwrX),to=log(qUprX),length.out=CntBrks))
               qBrksX[     1L] = -Inf
               qBrksX[CntBrks] = +Inf
            }else{
               qBrksX          = seq(from=qLwrX,to=qUprX,length.out=CntBrks)
               qBrksX[     1L] = -Inf
               qBrksX[CntBrks] = +Inf
            }#end if (mLogBins)
            #---~---


            #---~---
            #   Split variable X into bins.
            #---~---
            xCut      = cut(OrigTRY[[xName]],breaks=qBrksX,labels=FALSE)
            TallyCut  = table(xCut)
            CntSample = floor(CntUpper / CntBins)
            #---~---


            #---~---
            #   Make sure the sample size is fine. If not, iterate.
            #---~---
            UseBins = all(TallyCut %ge% MinSmpPerBin)
            iterate = ! UseBins
            if (UseBins){
               xIndex   = c( unlist( mapply( FUN      = Allom_Sample
                                           , x        = split(x=seq_along(xCut),f=xCut)
                                           , MoreArgs = list(size=CntSample)
                                           , SIMPLIFY = FALSE
                                           )#end mapply
                                   )#end unlist
                           )#end c
               names(xIndex) = NULL
               TrainTRY = OrigTRY[xIndex,,drop=FALSE]
            }#end if (UseBins)
            #---~---
         }#end if (CntBins %eq% 1L)
         #---~---
      }#end while (iterate)
      #---~---
   }else{
      #---~---
      #   No binning, but we still sample in case the number of data points exceeds the 
      # maximum.
      #---~---
      xIndex   = Allom_Sample(x=OrigIndex,size=CntUpper)
      TrainTRY = OrigTRY[xIndex,,drop=FALSE]
      #---~---
   }#end if (UseSizeBins)
   #---~---


   #---~---
   #   Return training data
   #---~---
   return(TrainTRY)
   #---~---
}#end function Allom_SetTrain
#---~---





#---~---
#   This function sets the first guesses and a few additional parameters for the parameter
# optimisation.
#
# Inputs
#
# + OrigTRY      -- Original data frame to be used as training data.
# + Model        -- Which model to fit? Options are
#                   - OneLinear (y = a0 * x )
#                   - OneLogLinear (y = a0 * x^a1                    )
#                   - TwoLinear    (y = a0 * x    * w                )
#                   - TwoMixLinear (y = a0 * x^a1 * w                )
#                   - TwoLogLinear (y = a0 * x^a1 * w^2              )
#                   - MartinezCano (y = a0 * x^a1 / (a2 + x^a1)      )
#                   - Weibull      (y = a0 * (1 - exp( -a1 * x^a2) ) )
# + xName        -- Name of the column with main predictor (x).
# + yName        -- Name of the column with predictand (y)
# + wName        -- Name of the column with scaling data (only needed for the models
#                   above that have a "w" term).

# + CntAllomMax  -- Maximum number of points to consider for fitting. This is used to
#                   limit the model fitting to a more manageable number of 
#                   observations when allometric data are exceedingly large. If no
#                   cap is sought, set CntAllomMax = +Inf. This setting is 
#                   independent on the data binning sampling defined by UseSizeBins.
# + UseSizeBins  -- Use binning across size classes to have a more balanced 
#                   distribution across the predictor range? Note that in this case
#                   we cannot use AIC or BIC to define the best model, and thus we
#                   simply use adjusted R2.
# + LogSmp       -- Apply log transformation when defining the bin boundaries?
# + MinSmpPerBin -- Minimum number of observations for each bin. Binning will not 
#                   occur unless there are at least twice as many valid points (and
#                   possibly even more points in case of very imbalanced data sets).
# + MaxCntBins   -- Maximum number of bins to be considered.
#---~---
Allom_PrepOptim <<- function( OrigTRY
                            , Model = c("OneLinear","OneLogLinear","TwoLinear"
                                       ,"TwoMixLinear","TwoLogLinear","MartinezCano"
                                       ,"Weibull")
                            , xName
                            , yName
                            , wName
                            ){

   #---~---
   #   Make sure Model is a valid model
   #---~---
   Model = match.arg(Model)
   #---~---


   #---~---
   #   First, we find some information about predictors and predictand that may be
   # useful for defining first guesses.
   #---~---
   qLwrX = quantile(x=OrigTRY[[xName]],probs=0.025,names=FALSE,na.rm=TRUE)
   qMidX = quantile(x=OrigTRY[[xName]],probs=0.500,names=FALSE,na.rm=TRUE)
   qUprX = quantile(x=OrigTRY[[xName]],probs=0.975,names=FALSE,na.rm=TRUE)
   qLwrY = quantile(x=OrigTRY[[yName]],probs=0.025,names=FALSE,na.rm=TRUE)
   qMidY = quantile(x=OrigTRY[[yName]],probs=0.500,names=FALSE,na.rm=TRUE)
   qUprY = quantile(x=OrigTRY[[yName]],probs=0.975,names=FALSE,na.rm=TRUE)
   #--- W is not always available.
   if (is.na(wName)){
      qMidW = NA_real_
   }else{
      qMidW = quantile(x=OrigTRY[[wName]],probs=0.500,names=FALSE,na.rm=TRUE)
   }#end if (is.na(wName))
   #---~---


   #---~---
   #   Set predictors, number of parameters and first guesses based on the functional 
   # form of the model.
   #---~---
   Predictor = 
     switch( EXPR         = Model
           , OneLinear    = paste0("a0 * ",xName                              )
           , OneLogLinear = paste0("a0 * ",xName,"^a1"                        )
           , TwoLinear    = paste0("a0 * ",xName," * ",wName                  )
           , TwoMixLinear = paste0("a0 * ",xName,"^a1 * ",wName               )
           , TwoLogLinear = paste0("a0 * ",xName,"^a1 * ",wName,"^a2"         )
           , MartinezCano = paste0("a0 * ",xName,"^a1 / ( a2 + ",xName,"^a1 )")
           , Weibull      = paste0("a0 * (1 - exp( -a1 * ",xName,"^a2 ) )"    )
           , stop(paste0(" Invalid Model: \"",Model,"\".")                   )
           )#end Predictor
   CntParam = 
     switch( EXPR         = Model
           , OneLinear    = 1L
           , OneLogLinear = 2L
           , TwoLinear    = 1L
           , TwoMixLinear = 2L
           , TwoLogLinear = 3L
           , MartinezCano = 3L
           , Weibull      = 3L
           )#end Predictor
   First_a0  =
     switch( EXPR         = Model
           , OneLinear    = ( qUprY - qLwrY ) / ( qUprX - qLwrX)
           , OneLogLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX)
           , TwoLinear    = ( qUprY - qLwrY ) / ( qUprX - qLwrX) / qMidW
           , TwoMixLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX) / qMidW
           , TwoLogLinear = ( qUprY - qLwrY ) / ( qUprX - qLwrX) / qMidW
           , MartinezCano = qUprY
           , Weibull      = qUprY
           )#end First_a0
   First_a1  =
     switch( EXPR         = Model
           , OneLinear    = NULL
           , OneLogLinear = 1.
           , TwoLinear    = NULL
           , TwoMixLinear = 1.
           , TwoLogLinear = 1.
           , MartinezCano = 1.
           , Weibull      = 1./qUprX
           )#end First_a1
   First_a2  =
     switch( EXPR         = Model
           , OneLinear    = NULL
           , OneLogLinear = NULL
           , TwoLinear    = NULL
           , TwoMixLinear = NULL
           , TwoLogLinear = 1.
           , MartinezCano = qMidX
           , Weibull      = 1.
           )#end First_a2
   #---~---


   #---~---
   #   Define equations for expected model and expected variance.
   #---~---
   Expect     = paste0(yName," ~ I(",Predictor,")")
   Sigma      = paste0(" ~ I(yhat^s1)")
   StartLSQ   = as.list(c( a0 = First_a0, a1 = First_a1, a2 = First_a2 ))
   StartSigma = list(s1=0.)
   #---~---


   #---~---
   #   Build a list with all the information
   #---~---
   Answer = list( Predictor  = Predictor
                , CntParam   = CntParam
                , Expect     = Expect
                , Sigma      = Sigma
                , StartLSQ   = StartLSQ
                , StartSigma = StartSigma
                )#end list
   #---~---

   #---~---
   #   Return training data
   #---~---
   return(Answer)
   #---~---
}#end function Allom_PrepOptim
#---~---




#---~---
#     This function is a wrapper for sampling that decides whether to run with or without
# replacement depending on the requested size.
#
# Input variables:
# x    - Vector to be sampled
# size - Size of the vector with samples.
#---~---
Allom_Sample <<- function(x,size){
   #---~---
   #   Make sure that x has more than one element (so sample will not assume x is the 
   # number of integer elements to be sampled).
   #---~---
   if (length(x) == 1) x=c(x,x)
   ans       = sort(sample(x=x,size=size,replace=length(x) < size))
   return(ans)
   #---~---
}#end Allom_Sample
#---~---
