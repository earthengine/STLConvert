module Main where

import Data.Word(Word16)
import Data.Binary.Get(Get,runGet,getWord16le,getByteString,getWord32le)
import Data.Binary.IEEE754(getFloat32le)
import Data.ByteString.Lazy(readFile)
import Control.Monad(replicateM)
import Prelude hiding (readFile)
import System.Environment(getArgs)


data Vector3 = Vector3 { x :: Float, y :: Float, z :: Float }

instance Show Vector3 where
  show v = show (x v) ++ " " ++ show (y v) ++ " " ++ show (z v)          

data Triangle = Triangle { normal :: Vector3, 
                           vertex1 :: Vector3, 
                           vertex2 :: Vector3, 
                           vertex3 :: Vector3,
                           attr :: Word16}

instance Show Triangle where
  show t =
    "facet normal " ++ show (normal t) ++ "\n\
    \\touter loop\n\
    \\t\tvertex " ++ show (vertex1 t) ++ "\n\
    \\t\tvertex " ++ show (vertex2 t) ++ "\n\
    \\t\tvertex " ++ show (vertex3 t) ++ "\n\
    \\tendloop\n\
    \endfacet\n"
 
getVector3 :: Get Vector3
getVector3 = do
  w1 <- getFloat32le
  w2 <- getFloat32le
  w3 <- getFloat32le
  return $ Vector3 w1 w2 w3

getTriangle :: Get Triangle
getTriangle = do
  n <- getVector3
  v1 <- getVector3
  v2 <- getVector3
  v3 <- getVector3
  a <- getWord16le
  return $ Triangle n v1 v2 v3 a

getStlBinary :: Get [Triangle]
getStlBinary = do
  _ <- getByteString 80
  cnt <- getWord32le
  replicateM (fromIntegral cnt) getTriangle
  
ioStlBin2Asc :: FilePath -> IO ()
ioStlBin2Asc file = do
  contents <- readFile $ file
  let stl = runGet getStlBinary contents  
  putStr "solid\n\n"
  mapM_ print stl
  
checkArgs :: [String] -> IO ()
checkArgs [file] = ioStlBin2Asc file
checkArgs _ = usage

usage :: IO ()
usage = putStrLn "Usage: StlBin2Asc file"

main :: IO ()
main = do
  args <-getArgs
  checkArgs args  
