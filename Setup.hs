import Distribution.Simple
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.PreProcess
import System.FilePath
import System.Process

main = defaultMainWithHooks hooks

hooks = simpleUserHooks { runTests = runTests' }

runTests' _ _ _ lbi = do
  defaultMainWithHooksArgs hooks ["build"]
  let rts = if (withProfExe lbi) || (withProfLib lbi)
            then "+RTS -p -hc"
            else ""
  system $ buildDir lbi </> "polygod" </> "polygod --test --maximum-generated-tests=1000 --threads=2 " ++ rts
  return ()


