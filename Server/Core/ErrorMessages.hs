module Server.Core.ErrorMessages where

import Happstack.Lite	
	
type ErrorMessage = String

badRequest :: ErrorMessage -> ServerPart Response
badRequest content = setResponseCode 400 >> return (toResponse content)

invalidExprErr :: ErrorMessage
invalidExprErr = "Error: Your expression is syntactically incorrect and could therefore not be parsed."

noExprErr :: ErrorMessage
noExprErr = "Error: You did not send an expression to the server."	
	
reductionFailedErr :: ErrorMessage
reductionFailedErr = "β-Reduction failed. You have to apply a necessary α-conversion first."	