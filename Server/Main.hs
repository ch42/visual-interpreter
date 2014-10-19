{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where
	
import Server.Core.Handler
import Happstack.Lite	



--- Start the server by calling this function
main :: IO ()
main = serve (Just config) setup


--- Configure the server here
config :: ServerConfig
config = ServerConfig { 
						port = 8080, 
						ramQuota  = 1 * 10^6, 
						diskQuota = 20 * 10^6, 
						tmpDir    = "/tmp/"
             		  }	


--- URL mapping
setup :: ServerPart Response
setup = msum 
	[
		dir "visual"				$ clientHandler,
		dir "parser"				$ parserHandler,
		dir "keywords"			$ keywordsHandler,
		dir "alpha-conversion"	$ alphaConversionHandler,
		dir "beta-reduction" 	$ betaReductionHandler	
	]







		
		
	
					
		
	

	