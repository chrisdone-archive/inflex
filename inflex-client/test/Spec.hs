{-# LANGUAGE OverloadedStrings #-}

-- |

module Main where

import System.Process.Typed
import Test.Hspec

main =
  hspec
    (describe
       "Compile"
       (do it
             "psc-package build: success"
             (shouldReturn
                (runProcess_
                   (proc "stack" ["exec", "--", "psc-package", "build"]))
                ())
           it
             "purs bundle: success"
             (shouldReturn
                (runProcess_
                   (proc
                      "stack"
                      [ "exec"
                      , "--"
                      , "purs"
                      , "bundle"
                      , "output/**/*.js"
                      , "-m"
                      , "Spec"
                      , "--main"
                      , "Spec"
                      , "-o"
                      , "app.js"
                      ]))
                ())
           it
             "node run: success"
             (shouldReturn
                (readProcessStdout_ (proc "node" ["app.js","{\"a\":1}"]))
                "{\"a\":1}\n\
                \(Right (MyRecord { a: 1 }))\n")))
