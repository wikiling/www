{-# LANGUAGE CPP #-}

module A where

import GHC
import GHC.Utils.Outputable
import GHC.Driver.Session ( defaultFatalMessager, defaultFlushOut )
import GHC.Paths ( libdir )
import GHC.Plugins ( showSDoc )
--GHC.Paths is available via cabal install ghc-paths
 
-- import DynFlags
targetFile = "B.hs"

run :: GhcMonad m => ModSummary -> String -> m ()
run modSum expr = do
#if __GLASGOW_HASKELL__ < 704
  setContext [ms_mod modSum] []
#else
#if __GLASGOW_HASKELL__ < 706
  setContext [IIModule $ ms_mod modSum]
#else
  setContext [IIModule $ moduleName  $ ms_mod modSum]       
#endif 
#endif
  rr<- runStmt expr RunToCompletion
  case rr of
    RunOk ns->
      do
        let q=(qualName &&& qualModule) defaultUserStyle
        mapM_ (\n -> do
                mty <- lookupName n
                case mty of
                  Just (AnId aid) -> do
                      df <- getSessionDynFlags
                      t <- gtry $ obtainTermFromId maxBound True aid
                      evalDoc <- case t of
                          Right term -> showTerm term
                          Left  exn  -> return (text "*** Exception:" <+>
                                                  text (show (exn :: SomeException)))
                      liftIO $ putStrLn $ showSDocForUser df q evalDoc
                      return ()
                  _ -> return ()
                ) ns
    RunException e -> liftIO $ print e
    _ -> return ()

main :: IO ()
main = do
   res <- example
#if __GLASGOW_HASKELL__ > 704
   str <- runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      return $ showSDoc dflags $ ppr res
   putStrLn str
#else
   putStrLn $ showSDoc ( ppr res )
#endif
 
example = 
#if __GLASGOW_HASKELL__ > 704
  defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
#else
  defaultErrorHandler defaultLogAction $ do
#endif
  runGhc (Just libdir) $ do
    dflags <- getSessionDynFlags
    target <- guessTarget targetFile Nothing
    setTargets [target]
    load LoadAllTargets
    modSum <- getModSummary $ mkModuleName "B"
    p <- parseModule modSum
    t <- typecheckModule p
    d <- desugarModule t
    l <- loadModule d
    n <- getNamesInScope
    c <- return $ coreModule d

    -- g <- getModuleGraph
    -- mapM showModule g     
    return $ (parsedSource d,"/n-----/n",  typecheckedSource d)
  