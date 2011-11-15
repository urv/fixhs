module Data.FIX.FIX43 ( fix43 ) where
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT ( new, insert )
import Data.FIX.Message
import Data.FIX.Parser
import Data.Functor ( (<$>) )
import Test.QuickCheck ( arbitrary )


tAccount :: FIXTag
tAccount = FIXTag 
   { tName = "Account"
   , tnum = 1
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAdvId :: FIXTag
tAdvId = FIXTag 
   { tName = "AdvId"
   , tnum = 2
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAdvRefID :: FIXTag
tAdvRefID = FIXTag 
   { tName = "AdvRefID"
   , tnum = 3
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAdvSide :: FIXTag
tAdvSide = FIXTag 
   { tName = "AdvSide"
   , tnum = 4
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag 
   { tName = "AdvTransType"
   , tnum = 5
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAvgPx :: FIXTag
tAvgPx = FIXTag 
   { tName = "AvgPx"
   , tnum = 6
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag 
   { tName = "BeginSeqNo"
   , tnum = 7
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tClOrdID :: FIXTag
tClOrdID = FIXTag 
   { tName = "ClOrdID"
   , tnum = 11
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCommission :: FIXTag
tCommission = FIXTag 
   { tName = "Commission"
   , tnum = 12
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCommType :: FIXTag
tCommType = FIXTag 
   { tName = "CommType"
   , tnum = 13
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tCumQty :: FIXTag
tCumQty = FIXTag 
   { tName = "CumQty"
   , tnum = 14
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCurrency :: FIXTag
tCurrency = FIXTag 
   { tName = "Currency"
   , tnum = 15
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag 
   { tName = "EndSeqNo"
   , tnum = 16
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tExecID :: FIXTag
tExecID = FIXTag 
   { tName = "ExecID"
   , tnum = 17
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tExecInst :: FIXTag
tExecInst = FIXTag 
   { tName = "ExecInst"
   , tnum = 18
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tExecRefID :: FIXTag
tExecRefID = FIXTag 
   { tName = "ExecRefID"
   , tnum = 19
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tExecTransType :: FIXTag
tExecTransType = FIXTag 
   { tName = "ExecTransType"
   , tnum = 20
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tHandlInst :: FIXTag
tHandlInst = FIXTag 
   { tName = "HandlInst"
   , tnum = 21
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSecurityIDSource :: FIXTag
tSecurityIDSource = FIXTag 
   { tName = "SecurityIDSource"
   , tnum = 22
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIOIid :: FIXTag
tIOIid = FIXTag 
   { tName = "IOIid"
   , tnum = 23
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIOIOthSvc :: FIXTag
tIOIOthSvc = FIXTag 
   { tName = "IOIOthSvc"
   , tnum = 24
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag 
   { tName = "IOIQltyInd"
   , tnum = 25
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag 
   { tName = "IOIRefID"
   , tnum = 26
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIOIQty :: FIXTag
tIOIQty = FIXTag 
   { tName = "IOIQty"
   , tnum = 27
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIOITransType :: FIXTag
tIOITransType = FIXTag 
   { tName = "IOITransType"
   , tnum = 28
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag 
   { tName = "LastCapacity"
   , tnum = 29
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLastMkt :: FIXTag
tLastMkt = FIXTag 
   { tName = "LastMkt"
   , tnum = 30
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLastPx :: FIXTag
tLastPx = FIXTag 
   { tName = "LastPx"
   , tnum = 31
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLastQty :: FIXTag
tLastQty = FIXTag 
   { tName = "LastQty"
   , tnum = 32
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLinesOfText :: FIXTag
tLinesOfText = FIXTag 
   { tName = "LinesOfText"
   , tnum = 33
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag 
   { tName = "MsgSeqNum"
   , tnum = 34
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag 
   { tName = "NewSeqNo"
   , tnum = 36
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOrderID :: FIXTag
tOrderID = FIXTag 
   { tName = "OrderID"
   , tnum = 37
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOrderQty :: FIXTag
tOrderQty = FIXTag 
   { tName = "OrderQty"
   , tnum = 38
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag 
   { tName = "OrdStatus"
   , tnum = 39
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tOrdType :: FIXTag
tOrdType = FIXTag 
   { tName = "OrdType"
   , tnum = 40
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag 
   { tName = "OrigClOrdID"
   , tnum = 41
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOrigTime :: FIXTag
tOrigTime = FIXTag 
   { tName = "OrigTime"
   , tnum = 42
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag 
   { tName = "PossDupFlag"
   , tnum = 43
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tPrice :: FIXTag
tPrice = FIXTag 
   { tName = "Price"
   , tnum = 44
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag 
   { tName = "RefSeqNum"
   , tnum = 45
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag 
   { tName = "RelatdSym"
   , tnum = 46
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRule80A :: FIXTag
tRule80A = FIXTag 
   { tName = "Rule80A"
   , tnum = 47
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSecurityID :: FIXTag
tSecurityID = FIXTag 
   { tName = "SecurityID"
   , tnum = 48
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSenderCompID :: FIXTag
tSenderCompID = FIXTag 
   { tName = "SenderCompID"
   , tnum = 49
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSenderSubID :: FIXTag
tSenderSubID = FIXTag 
   { tName = "SenderSubID"
   , tnum = 50
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSendingDate :: FIXTag
tSendingDate = FIXTag 
   { tName = "SendingDate"
   , tnum = 51
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tSendingTime :: FIXTag
tSendingTime = FIXTag 
   { tName = "SendingTime"
   , tnum = 52
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tQuantity :: FIXTag
tQuantity = FIXTag 
   { tName = "Quantity"
   , tnum = 53
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSide :: FIXTag
tSide = FIXTag 
   { tName = "Side"
   , tnum = 54
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSymbol :: FIXTag
tSymbol = FIXTag 
   { tName = "Symbol"
   , tnum = 55
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag 
   { tName = "TargetCompID"
   , tnum = 56
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag 
   { tName = "TargetSubID"
   , tnum = 57
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tText :: FIXTag
tText = FIXTag 
   { tName = "Text"
   , tnum = 58
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag 
   { tName = "TimeInForce"
   , tnum = 59
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tTransactTime :: FIXTag
tTransactTime = FIXTag 
   { tName = "TransactTime"
   , tnum = 60
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tUrgency :: FIXTag
tUrgency = FIXTag 
   { tName = "Urgency"
   , tnum = 61
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag 
   { tName = "ValidUntilTime"
   , tnum = 62
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tSettlmntTyp :: FIXTag
tSettlmntTyp = FIXTag 
   { tName = "SettlmntTyp"
   , tnum = 63
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tFutSettDate :: FIXTag
tFutSettDate = FIXTag 
   { tName = "FutSettDate"
   , tnum = 64
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tSymbolSfx :: FIXTag
tSymbolSfx = FIXTag 
   { tName = "SymbolSfx"
   , tnum = 65
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tListID :: FIXTag
tListID = FIXTag 
   { tName = "ListID"
   , tnum = 66
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tListSeqNo :: FIXTag
tListSeqNo = FIXTag 
   { tName = "ListSeqNo"
   , tnum = 67
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTotNoOrders :: FIXTag
tTotNoOrders = FIXTag 
   { tName = "TotNoOrders"
   , tnum = 68
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tListExecInst :: FIXTag
tListExecInst = FIXTag 
   { tName = "ListExecInst"
   , tnum = 69
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocID :: FIXTag
tAllocID = FIXTag 
   { tName = "AllocID"
   , tnum = 70
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocTransType :: FIXTag
tAllocTransType = FIXTag 
   { tName = "AllocTransType"
   , tnum = 71
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag 
   { tName = "RefAllocID"
   , tnum = 72
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoOrders :: FIXTag
tNoOrders = FIXTag 
   { tName = "NoOrders"
   , tnum = 73
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAvgPrxPrecision :: FIXTag
tAvgPrxPrecision = FIXTag 
   { tName = "AvgPrxPrecision"
   , tnum = 74
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradeDate :: FIXTag
tTradeDate = FIXTag 
   { tName = "TradeDate"
   , tnum = 75
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tExecBroker :: FIXTag
tExecBroker = FIXTag 
   { tName = "ExecBroker"
   , tnum = 76
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tPositionEffect :: FIXTag
tPositionEffect = FIXTag 
   { tName = "PositionEffect"
   , tnum = 77
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag 
   { tName = "NoAllocs"
   , tnum = 78
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag 
   { tName = "AllocAccount"
   , tnum = 79
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocQty :: FIXTag
tAllocQty = FIXTag 
   { tName = "AllocQty"
   , tnum = 80
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tProcessCode :: FIXTag
tProcessCode = FIXTag 
   { tName = "ProcessCode"
   , tnum = 81
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNoRpts :: FIXTag
tNoRpts = FIXTag 
   { tName = "NoRpts"
   , tnum = 82
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRptSeq :: FIXTag
tRptSeq = FIXTag 
   { tName = "RptSeq"
   , tnum = 83
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCxlQty :: FIXTag
tCxlQty = FIXTag 
   { tName = "CxlQty"
   , tnum = 84
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoDlvyInst :: FIXTag
tNoDlvyInst = FIXTag 
   { tName = "NoDlvyInst"
   , tnum = 85
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tDlvyInst :: FIXTag
tDlvyInst = FIXTag 
   { tName = "DlvyInst"
   , tnum = 86
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocStatus :: FIXTag
tAllocStatus = FIXTag 
   { tName = "AllocStatus"
   , tnum = 87
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAllocRejCode :: FIXTag
tAllocRejCode = FIXTag 
   { tName = "AllocRejCode"
   , tnum = 88
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSignature :: FIXTag
tSignature = FIXTag 
   { tName = "Signature"
   , tnum = 89
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tSecureDataLen :: FIXTag
tSecureDataLen = FIXTag 
   { tName = "SecureDataLen"
   , tnum = 90
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecureData :: FIXTag
tSecureData = FIXTag 
   { tName = "SecureData"
   , tnum = 91
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tBrokerOfCredit :: FIXTag
tBrokerOfCredit = FIXTag 
   { tName = "BrokerOfCredit"
   , tnum = 92
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSignatureLength :: FIXTag
tSignatureLength = FIXTag 
   { tName = "SignatureLength"
   , tnum = 93
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEmailType :: FIXTag
tEmailType = FIXTag 
   { tName = "EmailType"
   , tnum = 94
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag 
   { tName = "RawDataLength"
   , tnum = 95
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRawData :: FIXTag
tRawData = FIXTag 
   { tName = "RawData"
   , tnum = 96
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tPossResend :: FIXTag
tPossResend = FIXTag 
   { tName = "PossResend"
   , tnum = 97
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tEncryptMethod :: FIXTag
tEncryptMethod = FIXTag 
   { tName = "EncryptMethod"
   , tnum = 98
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tStopPx :: FIXTag
tStopPx = FIXTag 
   { tName = "StopPx"
   , tnum = 99
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tExDestination :: FIXTag
tExDestination = FIXTag 
   { tName = "ExDestination"
   , tnum = 100
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCxlRejReason :: FIXTag
tCxlRejReason = FIXTag 
   { tName = "CxlRejReason"
   , tnum = 102
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOrdRejReason :: FIXTag
tOrdRejReason = FIXTag 
   { tName = "OrdRejReason"
   , tnum = 103
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tIOIQualifier :: FIXTag
tIOIQualifier = FIXTag 
   { tName = "IOIQualifier"
   , tnum = 104
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tWaveNo :: FIXTag
tWaveNo = FIXTag 
   { tName = "WaveNo"
   , tnum = 105
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIssuer :: FIXTag
tIssuer = FIXTag 
   { tName = "Issuer"
   , tnum = 106
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecurityDesc :: FIXTag
tSecurityDesc = FIXTag 
   { tName = "SecurityDesc"
   , tnum = 107
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tHeartBtInt :: FIXTag
tHeartBtInt = FIXTag 
   { tName = "HeartBtInt"
   , tnum = 108
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tClientID :: FIXTag
tClientID = FIXTag 
   { tName = "ClientID"
   , tnum = 109
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMinQty :: FIXTag
tMinQty = FIXTag 
   { tName = "MinQty"
   , tnum = 110
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag 
   { tName = "MaxFloor"
   , tnum = 111
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tTestReqID :: FIXTag
tTestReqID = FIXTag 
   { tName = "TestReqID"
   , tnum = 112
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tReportToExch :: FIXTag
tReportToExch = FIXTag 
   { tName = "ReportToExch"
   , tnum = 113
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag 
   { tName = "LocateReqd"
   , tnum = 114
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tOnBehalfOfCompID :: FIXTag
tOnBehalfOfCompID = FIXTag 
   { tName = "OnBehalfOfCompID"
   , tnum = 115
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOnBehalfOfSubID :: FIXTag
tOnBehalfOfSubID = FIXTag 
   { tName = "OnBehalfOfSubID"
   , tnum = 116
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tQuoteID :: FIXTag
tQuoteID = FIXTag 
   { tName = "QuoteID"
   , tnum = 117
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNetMoney :: FIXTag
tNetMoney = FIXTag 
   { tName = "NetMoney"
   , tnum = 118
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlCurrAmt :: FIXTag
tSettlCurrAmt = FIXTag 
   { tName = "SettlCurrAmt"
   , tnum = 119
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlCurrency :: FIXTag
tSettlCurrency = FIXTag 
   { tName = "SettlCurrency"
   , tnum = 120
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tForexReq :: FIXTag
tForexReq = FIXTag 
   { tName = "ForexReq"
   , tnum = 121
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag 
   { tName = "OrigSendingTime"
   , tnum = 122
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag 
   { tName = "GapFillFlag"
   , tnum = 123
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tNoExecs :: FIXTag
tNoExecs = FIXTag 
   { tName = "NoExecs"
   , tnum = 124
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCxlType :: FIXTag
tCxlType = FIXTag 
   { tName = "CxlType"
   , tnum = 125
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tExpireTime :: FIXTag
tExpireTime = FIXTag 
   { tName = "ExpireTime"
   , tnum = 126
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tDKReason :: FIXTag
tDKReason = FIXTag 
   { tName = "DKReason"
   , tnum = 127
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag 
   { tName = "DeliverToCompID"
   , tnum = 128
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag 
   { tName = "DeliverToSubID"
   , tnum = 129
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag 
   { tName = "IOINaturalFlag"
   , tnum = 130
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag 
   { tName = "QuoteReqID"
   , tnum = 131
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tBidPx :: FIXTag
tBidPx = FIXTag 
   { tName = "BidPx"
   , tnum = 132
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferPx :: FIXTag
tOfferPx = FIXTag 
   { tName = "OfferPx"
   , tnum = 133
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBidSize :: FIXTag
tBidSize = FIXTag 
   { tName = "BidSize"
   , tnum = 134
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferSize :: FIXTag
tOfferSize = FIXTag 
   { tName = "OfferSize"
   , tnum = 135
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag 
   { tName = "NoMiscFees"
   , tnum = 136
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag 
   { tName = "MiscFeeAmt"
   , tnum = 137
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag 
   { tName = "MiscFeeCurr"
   , tnum = 138
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag 
   { tName = "MiscFeeType"
   , tnum = 139
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag 
   { tName = "PrevClosePx"
   , tnum = 140
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag 
   { tName = "ResetSeqNumFlag"
   , tnum = 141
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag 
   { tName = "SenderLocationID"
   , tnum = 142
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag 
   { tName = "TargetLocationID"
   , tnum = 143
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag 
   { tName = "OnBehalfOfLocationID"
   , tnum = 144
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag 
   { tName = "DeliverToLocationID"
   , tnum = 145
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag 
   { tName = "NoRelatedSym"
   , tnum = 146
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSubject :: FIXTag
tSubject = FIXTag 
   { tName = "Subject"
   , tnum = 147
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tHeadline :: FIXTag
tHeadline = FIXTag 
   { tName = "Headline"
   , tnum = 148
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tURLLink :: FIXTag
tURLLink = FIXTag 
   { tName = "URLLink"
   , tnum = 149
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tExecType :: FIXTag
tExecType = FIXTag 
   { tName = "ExecType"
   , tnum = 150
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag 
   { tName = "LeavesQty"
   , tnum = 151
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag 
   { tName = "CashOrderQty"
   , tnum = 152
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag 
   { tName = "AllocAvgPx"
   , tnum = 153
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag 
   { tName = "AllocNetMoney"
   , tnum = 154
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag 
   { tName = "SettlCurrFxRate"
   , tnum = 155
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag 
   { tName = "SettlCurrFxRateCalc"
   , tnum = 156
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag 
   { tName = "NumDaysInterest"
   , tnum = 157
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag 
   { tName = "AccruedInterestRate"
   , tnum = 158
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag 
   { tName = "AccruedInterestAmt"
   , tnum = 159
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag 
   { tName = "SettlInstMode"
   , tnum = 160
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tAllocText :: FIXTag
tAllocText = FIXTag 
   { tName = "AllocText"
   , tnum = 161
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag 
   { tName = "SettlInstID"
   , tnum = 162
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag 
   { tName = "SettlInstTransType"
   , tnum = 163
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag 
   { tName = "EmailThreadID"
   , tnum = 164
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag 
   { tName = "SettlInstSource"
   , tnum = 165
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSettlLocation :: FIXTag
tSettlLocation = FIXTag 
   { tName = "SettlLocation"
   , tnum = 166
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecurityType :: FIXTag
tSecurityType = FIXTag 
   { tName = "SecurityType"
   , tnum = 167
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEffectiveTime :: FIXTag
tEffectiveTime = FIXTag 
   { tName = "EffectiveTime"
   , tnum = 168
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tStandInstDbType :: FIXTag
tStandInstDbType = FIXTag 
   { tName = "StandInstDbType"
   , tnum = 169
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tStandInstDbName :: FIXTag
tStandInstDbName = FIXTag 
   { tName = "StandInstDbName"
   , tnum = 170
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tStandInstDbID :: FIXTag
tStandInstDbID = FIXTag 
   { tName = "StandInstDbID"
   , tnum = 171
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlDeliveryType :: FIXTag
tSettlDeliveryType = FIXTag 
   { tName = "SettlDeliveryType"
   , tnum = 172
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSettlDepositoryCode :: FIXTag
tSettlDepositoryCode = FIXTag 
   { tName = "SettlDepositoryCode"
   , tnum = 173
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlBrkrCode :: FIXTag
tSettlBrkrCode = FIXTag 
   { tName = "SettlBrkrCode"
   , tnum = 174
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlInstCode :: FIXTag
tSettlInstCode = FIXTag 
   { tName = "SettlInstCode"
   , tnum = 175
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentName :: FIXTag
tSecuritySettlAgentName = FIXTag 
   { tName = "SecuritySettlAgentName"
   , tnum = 176
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentCode :: FIXTag
tSecuritySettlAgentCode = FIXTag 
   { tName = "SecuritySettlAgentCode"
   , tnum = 177
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentAcctNum :: FIXTag
tSecuritySettlAgentAcctNum = FIXTag 
   { tName = "SecuritySettlAgentAcctNum"
   , tnum = 178
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentAcctName :: FIXTag
tSecuritySettlAgentAcctName = FIXTag 
   { tName = "SecuritySettlAgentAcctName"
   , tnum = 179
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentContactName :: FIXTag
tSecuritySettlAgentContactName = FIXTag 
   { tName = "SecuritySettlAgentContactName"
   , tnum = 180
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecuritySettlAgentContactPhone :: FIXTag
tSecuritySettlAgentContactPhone = FIXTag 
   { tName = "SecuritySettlAgentContactPhone"
   , tnum = 181
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentName :: FIXTag
tCashSettlAgentName = FIXTag 
   { tName = "CashSettlAgentName"
   , tnum = 182
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentCode :: FIXTag
tCashSettlAgentCode = FIXTag 
   { tName = "CashSettlAgentCode"
   , tnum = 183
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentAcctNum :: FIXTag
tCashSettlAgentAcctNum = FIXTag 
   { tName = "CashSettlAgentAcctNum"
   , tnum = 184
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentAcctName :: FIXTag
tCashSettlAgentAcctName = FIXTag 
   { tName = "CashSettlAgentAcctName"
   , tnum = 185
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentContactName :: FIXTag
tCashSettlAgentContactName = FIXTag 
   { tName = "CashSettlAgentContactName"
   , tnum = 186
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashSettlAgentContactPhone :: FIXTag
tCashSettlAgentContactPhone = FIXTag 
   { tName = "CashSettlAgentContactPhone"
   , tnum = 187
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tBidSpotRate :: FIXTag
tBidSpotRate = FIXTag 
   { tName = "BidSpotRate"
   , tnum = 188
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBidForwardPoints :: FIXTag
tBidForwardPoints = FIXTag 
   { tName = "BidForwardPoints"
   , tnum = 189
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferSpotRate :: FIXTag
tOfferSpotRate = FIXTag 
   { tName = "OfferSpotRate"
   , tnum = 190
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferForwardPoints :: FIXTag
tOfferForwardPoints = FIXTag 
   { tName = "OfferForwardPoints"
   , tnum = 191
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOrderQty2 :: FIXTag
tOrderQty2 = FIXTag 
   { tName = "OrderQty2"
   , tnum = 192
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tFutSettDate2 :: FIXTag
tFutSettDate2 = FIXTag 
   { tName = "FutSettDate2"
   , tnum = 193
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tLastSpotRate :: FIXTag
tLastSpotRate = FIXTag 
   { tName = "LastSpotRate"
   , tnum = 194
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLastForwardPoints :: FIXTag
tLastForwardPoints = FIXTag 
   { tName = "LastForwardPoints"
   , tnum = 195
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tAllocLinkID :: FIXTag
tAllocLinkID = FIXTag 
   { tName = "AllocLinkID"
   , tnum = 196
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocLinkType :: FIXTag
tAllocLinkType = FIXTag 
   { tName = "AllocLinkType"
   , tnum = 197
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecondaryOrderID :: FIXTag
tSecondaryOrderID = FIXTag 
   { tName = "SecondaryOrderID"
   , tnum = 198
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoIOIQualifiers :: FIXTag
tNoIOIQualifiers = FIXTag 
   { tName = "NoIOIQualifiers"
   , tnum = 199
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMaturityMonthYear :: FIXTag
tMaturityMonthYear = FIXTag 
   { tName = "MaturityMonthYear"
   , tnum = 200
   , tparser = toFIXMonthYear
   , arbitraryValue = FIXMonthYear <$> arbitrary }

tPutOrCall :: FIXTag
tPutOrCall = FIXTag 
   { tName = "PutOrCall"
   , tnum = 201
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tStrikePrice :: FIXTag
tStrikePrice = FIXTag 
   { tName = "StrikePrice"
   , tnum = 202
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCoveredOrUncovered :: FIXTag
tCoveredOrUncovered = FIXTag 
   { tName = "CoveredOrUncovered"
   , tnum = 203
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCustomerOrFirm :: FIXTag
tCustomerOrFirm = FIXTag 
   { tName = "CustomerOrFirm"
   , tnum = 204
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMaturityDay :: FIXTag
tMaturityDay = FIXTag 
   { tName = "MaturityDay"
   , tnum = 205
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOptAttribute :: FIXTag
tOptAttribute = FIXTag 
   { tName = "OptAttribute"
   , tnum = 206
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag 
   { tName = "SecurityExchange"
   , tnum = 207
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag 
   { tName = "NotifyBrokerOfCredit"
   , tnum = 208
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag 
   { tName = "AllocHandlInst"
   , tnum = 209
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMaxShow :: FIXTag
tMaxShow = FIXTag 
   { tName = "MaxShow"
   , tnum = 210
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tPegDifference :: FIXTag
tPegDifference = FIXTag 
   { tName = "PegDifference"
   , tnum = 211
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tXmlDataLen :: FIXTag
tXmlDataLen = FIXTag 
   { tName = "XmlDataLen"
   , tnum = 212
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tXmlData :: FIXTag
tXmlData = FIXTag 
   { tName = "XmlData"
   , tnum = 213
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tSettlInstRefID :: FIXTag
tSettlInstRefID = FIXTag 
   { tName = "SettlInstRefID"
   , tnum = 214
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoRoutingIDs :: FIXTag
tNoRoutingIDs = FIXTag 
   { tName = "NoRoutingIDs"
   , tnum = 215
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRoutingType :: FIXTag
tRoutingType = FIXTag 
   { tName = "RoutingType"
   , tnum = 216
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRoutingID :: FIXTag
tRoutingID = FIXTag 
   { tName = "RoutingID"
   , tnum = 217
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSpread :: FIXTag
tSpread = FIXTag 
   { tName = "Spread"
   , tnum = 218
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBenchmark :: FIXTag
tBenchmark = FIXTag 
   { tName = "Benchmark"
   , tnum = 219
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tBenchmarkCurveCurrency :: FIXTag
tBenchmarkCurveCurrency = FIXTag 
   { tName = "BenchmarkCurveCurrency"
   , tnum = 220
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tBenchmarkCurveName :: FIXTag
tBenchmarkCurveName = FIXTag 
   { tName = "BenchmarkCurveName"
   , tnum = 221
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tBenchmarkCurvePoint :: FIXTag
tBenchmarkCurvePoint = FIXTag 
   { tName = "BenchmarkCurvePoint"
   , tnum = 222
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCouponRate :: FIXTag
tCouponRate = FIXTag 
   { tName = "CouponRate"
   , tnum = 223
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCouponPaymentDate :: FIXTag
tCouponPaymentDate = FIXTag 
   { tName = "CouponPaymentDate"
   , tnum = 224
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tIssueDate :: FIXTag
tIssueDate = FIXTag 
   { tName = "IssueDate"
   , tnum = 225
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tRepurchaseTerm :: FIXTag
tRepurchaseTerm = FIXTag 
   { tName = "RepurchaseTerm"
   , tnum = 226
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRepurchaseRate :: FIXTag
tRepurchaseRate = FIXTag 
   { tName = "RepurchaseRate"
   , tnum = 227
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tFactor :: FIXTag
tFactor = FIXTag 
   { tName = "Factor"
   , tnum = 228
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tTradeOriginationDate :: FIXTag
tTradeOriginationDate = FIXTag 
   { tName = "TradeOriginationDate"
   , tnum = 229
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tExDate :: FIXTag
tExDate = FIXTag 
   { tName = "ExDate"
   , tnum = 230
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tContractMultiplier :: FIXTag
tContractMultiplier = FIXTag 
   { tName = "ContractMultiplier"
   , tnum = 231
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoStipulations :: FIXTag
tNoStipulations = FIXTag 
   { tName = "NoStipulations"
   , tnum = 232
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tStipulationType :: FIXTag
tStipulationType = FIXTag 
   { tName = "StipulationType"
   , tnum = 233
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tStipulationValue :: FIXTag
tStipulationValue = FIXTag 
   { tName = "StipulationValue"
   , tnum = 234
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tYieldType :: FIXTag
tYieldType = FIXTag 
   { tName = "YieldType"
   , tnum = 235
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tYield :: FIXTag
tYield = FIXTag 
   { tName = "Yield"
   , tnum = 236
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tTotalTakedown :: FIXTag
tTotalTakedown = FIXTag 
   { tName = "TotalTakedown"
   , tnum = 237
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tConcession :: FIXTag
tConcession = FIXTag 
   { tName = "Concession"
   , tnum = 238
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tRepoCollateralSecurityType :: FIXTag
tRepoCollateralSecurityType = FIXTag 
   { tName = "RepoCollateralSecurityType"
   , tnum = 239
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRedemptionDate :: FIXTag
tRedemptionDate = FIXTag 
   { tName = "RedemptionDate"
   , tnum = 240
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tUnderlyingCouponPaymentDate :: FIXTag
tUnderlyingCouponPaymentDate = FIXTag 
   { tName = "UnderlyingCouponPaymentDate"
   , tnum = 241
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tUnderlyingIssueDate :: FIXTag
tUnderlyingIssueDate = FIXTag 
   { tName = "UnderlyingIssueDate"
   , tnum = 242
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tUnderlyingRepoCollateralSecurityType :: FIXTag
tUnderlyingRepoCollateralSecurityType = FIXTag 
   { tName = "UnderlyingRepoCollateralSecurityType"
   , tnum = 243
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingRepurchaseTerm :: FIXTag
tUnderlyingRepurchaseTerm = FIXTag 
   { tName = "UnderlyingRepurchaseTerm"
   , tnum = 244
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingRepurchaseRate :: FIXTag
tUnderlyingRepurchaseRate = FIXTag 
   { tName = "UnderlyingRepurchaseRate"
   , tnum = 245
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tUnderlyingFactor :: FIXTag
tUnderlyingFactor = FIXTag 
   { tName = "UnderlyingFactor"
   , tnum = 246
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tUnderlyingRedemptionDate :: FIXTag
tUnderlyingRedemptionDate = FIXTag 
   { tName = "UnderlyingRedemptionDate"
   , tnum = 247
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tLegCouponPaymentDate :: FIXTag
tLegCouponPaymentDate = FIXTag 
   { tName = "LegCouponPaymentDate"
   , tnum = 248
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tLegIssueDate :: FIXTag
tLegIssueDate = FIXTag 
   { tName = "LegIssueDate"
   , tnum = 249
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tLegRepoCollateralSecurityType :: FIXTag
tLegRepoCollateralSecurityType = FIXTag 
   { tName = "LegRepoCollateralSecurityType"
   , tnum = 250
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegRepurchaseTerm :: FIXTag
tLegRepurchaseTerm = FIXTag 
   { tName = "LegRepurchaseTerm"
   , tnum = 251
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegRepurchaseRate :: FIXTag
tLegRepurchaseRate = FIXTag 
   { tName = "LegRepurchaseRate"
   , tnum = 252
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegFactor :: FIXTag
tLegFactor = FIXTag 
   { tName = "LegFactor"
   , tnum = 253
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegRedemptionDate :: FIXTag
tLegRedemptionDate = FIXTag 
   { tName = "LegRedemptionDate"
   , tnum = 254
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tCreditRating :: FIXTag
tCreditRating = FIXTag 
   { tName = "CreditRating"
   , tnum = 255
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingCreditRating :: FIXTag
tUnderlyingCreditRating = FIXTag 
   { tName = "UnderlyingCreditRating"
   , tnum = 256
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegCreditRating :: FIXTag
tLegCreditRating = FIXTag 
   { tName = "LegCreditRating"
   , tnum = 257
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradedFlatSwitch :: FIXTag
tTradedFlatSwitch = FIXTag 
   { tName = "TradedFlatSwitch"
   , tnum = 258
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tBasisFeatureDate :: FIXTag
tBasisFeatureDate = FIXTag 
   { tName = "BasisFeatureDate"
   , tnum = 259
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tBasisFeaturePrice :: FIXTag
tBasisFeaturePrice = FIXTag 
   { tName = "BasisFeaturePrice"
   , tnum = 260
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMDReqID :: FIXTag
tMDReqID = FIXTag 
   { tName = "MDReqID"
   , tnum = 262
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSubscriptionRequestType :: FIXTag
tSubscriptionRequestType = FIXTag 
   { tName = "SubscriptionRequestType"
   , tnum = 263
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMarketDepth :: FIXTag
tMarketDepth = FIXTag 
   { tName = "MarketDepth"
   , tnum = 264
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMDUpdateType :: FIXTag
tMDUpdateType = FIXTag 
   { tName = "MDUpdateType"
   , tnum = 265
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAggregatedBook :: FIXTag
tAggregatedBook = FIXTag 
   { tName = "AggregatedBook"
   , tnum = 266
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tNoMDEntryTypes :: FIXTag
tNoMDEntryTypes = FIXTag 
   { tName = "NoMDEntryTypes"
   , tnum = 267
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoMDEntries :: FIXTag
tNoMDEntries = FIXTag 
   { tName = "NoMDEntries"
   , tnum = 268
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMDEntryType :: FIXTag
tMDEntryType = FIXTag 
   { tName = "MDEntryType"
   , tnum = 269
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMDEntryPx :: FIXTag
tMDEntryPx = FIXTag 
   { tName = "MDEntryPx"
   , tnum = 270
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMDEntrySize :: FIXTag
tMDEntrySize = FIXTag 
   { tName = "MDEntrySize"
   , tnum = 271
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMDEntryDate :: FIXTag
tMDEntryDate = FIXTag 
   { tName = "MDEntryDate"
   , tnum = 272
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tMDEntryTime :: FIXTag
tMDEntryTime = FIXTag 
   { tName = "MDEntryTime"
   , tnum = 273
   , tparser = toFIXTimeOnly
   , arbitraryValue = FIXTimeOnly <$> arbitrary }

tTickDirection :: FIXTag
tTickDirection = FIXTag 
   { tName = "TickDirection"
   , tnum = 274
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMDMkt :: FIXTag
tMDMkt = FIXTag 
   { tName = "MDMkt"
   , tnum = 275
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tQuoteCondition :: FIXTag
tQuoteCondition = FIXTag 
   { tName = "QuoteCondition"
   , tnum = 276
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tTradeCondition :: FIXTag
tTradeCondition = FIXTag 
   { tName = "TradeCondition"
   , tnum = 277
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tMDEntryID :: FIXTag
tMDEntryID = FIXTag 
   { tName = "MDEntryID"
   , tnum = 278
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMDUpdateAction :: FIXTag
tMDUpdateAction = FIXTag 
   { tName = "MDUpdateAction"
   , tnum = 279
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMDEntryRefID :: FIXTag
tMDEntryRefID = FIXTag 
   { tName = "MDEntryRefID"
   , tnum = 280
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMDReqRejReason :: FIXTag
tMDReqRejReason = FIXTag 
   { tName = "MDReqRejReason"
   , tnum = 281
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMDEntryOriginator :: FIXTag
tMDEntryOriginator = FIXTag 
   { tName = "MDEntryOriginator"
   , tnum = 282
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLocationID :: FIXTag
tLocationID = FIXTag 
   { tName = "LocationID"
   , tnum = 283
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDeskID :: FIXTag
tDeskID = FIXTag 
   { tName = "DeskID"
   , tnum = 284
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDeleteReason :: FIXTag
tDeleteReason = FIXTag 
   { tName = "DeleteReason"
   , tnum = 285
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tOpenCloseSettleFlag :: FIXTag
tOpenCloseSettleFlag = FIXTag 
   { tName = "OpenCloseSettleFlag"
   , tnum = 286
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tSellerDays :: FIXTag
tSellerDays = FIXTag 
   { tName = "SellerDays"
   , tnum = 287
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMDEntryBuyer :: FIXTag
tMDEntryBuyer = FIXTag 
   { tName = "MDEntryBuyer"
   , tnum = 288
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMDEntrySeller :: FIXTag
tMDEntrySeller = FIXTag 
   { tName = "MDEntrySeller"
   , tnum = 289
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMDEntryPositionNo :: FIXTag
tMDEntryPositionNo = FIXTag 
   { tName = "MDEntryPositionNo"
   , tnum = 290
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tFinancialStatus :: FIXTag
tFinancialStatus = FIXTag 
   { tName = "FinancialStatus"
   , tnum = 291
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tCorporateAction :: FIXTag
tCorporateAction = FIXTag 
   { tName = "CorporateAction"
   , tnum = 292
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tDefBidSize :: FIXTag
tDefBidSize = FIXTag 
   { tName = "DefBidSize"
   , tnum = 293
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tDefOfferSize :: FIXTag
tDefOfferSize = FIXTag 
   { tName = "DefOfferSize"
   , tnum = 294
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoQuoteEntries :: FIXTag
tNoQuoteEntries = FIXTag 
   { tName = "NoQuoteEntries"
   , tnum = 295
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoQuoteSets :: FIXTag
tNoQuoteSets = FIXTag 
   { tName = "NoQuoteSets"
   , tnum = 296
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tQuoteStatus :: FIXTag
tQuoteStatus = FIXTag 
   { tName = "QuoteStatus"
   , tnum = 297
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tQuoteCancelType :: FIXTag
tQuoteCancelType = FIXTag 
   { tName = "QuoteCancelType"
   , tnum = 298
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tQuoteEntryID :: FIXTag
tQuoteEntryID = FIXTag 
   { tName = "QuoteEntryID"
   , tnum = 299
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tQuoteRejectReason :: FIXTag
tQuoteRejectReason = FIXTag 
   { tName = "QuoteRejectReason"
   , tnum = 300
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tQuoteResponseLevel :: FIXTag
tQuoteResponseLevel = FIXTag 
   { tName = "QuoteResponseLevel"
   , tnum = 301
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tQuoteSetID :: FIXTag
tQuoteSetID = FIXTag 
   { tName = "QuoteSetID"
   , tnum = 302
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tQuoteRequestType :: FIXTag
tQuoteRequestType = FIXTag 
   { tName = "QuoteRequestType"
   , tnum = 303
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTotQuoteEntries :: FIXTag
tTotQuoteEntries = FIXTag 
   { tName = "TotQuoteEntries"
   , tnum = 304
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingSecurityIDSource :: FIXTag
tUnderlyingSecurityIDSource = FIXTag 
   { tName = "UnderlyingSecurityIDSource"
   , tnum = 305
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingIssuer :: FIXTag
tUnderlyingIssuer = FIXTag 
   { tName = "UnderlyingIssuer"
   , tnum = 306
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSecurityDesc :: FIXTag
tUnderlyingSecurityDesc = FIXTag 
   { tName = "UnderlyingSecurityDesc"
   , tnum = 307
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSecurityExchange :: FIXTag
tUnderlyingSecurityExchange = FIXTag 
   { tName = "UnderlyingSecurityExchange"
   , tnum = 308
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSecurityID :: FIXTag
tUnderlyingSecurityID = FIXTag 
   { tName = "UnderlyingSecurityID"
   , tnum = 309
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSecurityType :: FIXTag
tUnderlyingSecurityType = FIXTag 
   { tName = "UnderlyingSecurityType"
   , tnum = 310
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSymbol :: FIXTag
tUnderlyingSymbol = FIXTag 
   { tName = "UnderlyingSymbol"
   , tnum = 311
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSymbolSfx :: FIXTag
tUnderlyingSymbolSfx = FIXTag 
   { tName = "UnderlyingSymbolSfx"
   , tnum = 312
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingMaturityMonthYear :: FIXTag
tUnderlyingMaturityMonthYear = FIXTag 
   { tName = "UnderlyingMaturityMonthYear"
   , tnum = 313
   , tparser = toFIXMonthYear
   , arbitraryValue = FIXMonthYear <$> arbitrary }

tUnderlyingMaturityDay :: FIXTag
tUnderlyingMaturityDay = FIXTag 
   { tName = "UnderlyingMaturityDay"
   , tnum = 314
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingPutOrCall :: FIXTag
tUnderlyingPutOrCall = FIXTag 
   { tName = "UnderlyingPutOrCall"
   , tnum = 315
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingStrikePrice :: FIXTag
tUnderlyingStrikePrice = FIXTag 
   { tName = "UnderlyingStrikePrice"
   , tnum = 316
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tUnderlyingOptAttribute :: FIXTag
tUnderlyingOptAttribute = FIXTag 
   { tName = "UnderlyingOptAttribute"
   , tnum = 317
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tUnderlyingCurrency :: FIXTag
tUnderlyingCurrency = FIXTag 
   { tName = "UnderlyingCurrency"
   , tnum = 318
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRatioQty :: FIXTag
tRatioQty = FIXTag 
   { tName = "RatioQty"
   , tnum = 319
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSecurityReqID :: FIXTag
tSecurityReqID = FIXTag 
   { tName = "SecurityReqID"
   , tnum = 320
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecurityRequestType :: FIXTag
tSecurityRequestType = FIXTag 
   { tName = "SecurityRequestType"
   , tnum = 321
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecurityResponseID :: FIXTag
tSecurityResponseID = FIXTag 
   { tName = "SecurityResponseID"
   , tnum = 322
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecurityResponseType :: FIXTag
tSecurityResponseType = FIXTag 
   { tName = "SecurityResponseType"
   , tnum = 323
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecurityStatusReqID :: FIXTag
tSecurityStatusReqID = FIXTag 
   { tName = "SecurityStatusReqID"
   , tnum = 324
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnsolicitedIndicator :: FIXTag
tUnsolicitedIndicator = FIXTag 
   { tName = "UnsolicitedIndicator"
   , tnum = 325
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tSecurityTradingStatus :: FIXTag
tSecurityTradingStatus = FIXTag 
   { tName = "SecurityTradingStatus"
   , tnum = 326
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tHaltReasonChar :: FIXTag
tHaltReasonChar = FIXTag 
   { tName = "HaltReasonChar"
   , tnum = 327
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tInViewOfCommon :: FIXTag
tInViewOfCommon = FIXTag 
   { tName = "InViewOfCommon"
   , tnum = 328
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tDueToRelated :: FIXTag
tDueToRelated = FIXTag 
   { tName = "DueToRelated"
   , tnum = 329
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tBuyVolume :: FIXTag
tBuyVolume = FIXTag 
   { tName = "BuyVolume"
   , tnum = 330
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSellVolume :: FIXTag
tSellVolume = FIXTag 
   { tName = "SellVolume"
   , tnum = 331
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tHighPx :: FIXTag
tHighPx = FIXTag 
   { tName = "HighPx"
   , tnum = 332
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLowPx :: FIXTag
tLowPx = FIXTag 
   { tName = "LowPx"
   , tnum = 333
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tAdjustment :: FIXTag
tAdjustment = FIXTag 
   { tName = "Adjustment"
   , tnum = 334
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradSesReqID :: FIXTag
tTradSesReqID = FIXTag 
   { tName = "TradSesReqID"
   , tnum = 335
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradingSessionID :: FIXTag
tTradingSessionID = FIXTag 
   { tName = "TradingSessionID"
   , tnum = 336
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tContraTrader :: FIXTag
tContraTrader = FIXTag 
   { tName = "ContraTrader"
   , tnum = 337
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradSesMethod :: FIXTag
tTradSesMethod = FIXTag 
   { tName = "TradSesMethod"
   , tnum = 338
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradSesMode :: FIXTag
tTradSesMode = FIXTag 
   { tName = "TradSesMode"
   , tnum = 339
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradSesStatus :: FIXTag
tTradSesStatus = FIXTag 
   { tName = "TradSesStatus"
   , tnum = 340
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradSesStartTime :: FIXTag
tTradSesStartTime = FIXTag 
   { tName = "TradSesStartTime"
   , tnum = 341
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tTradSesOpenTime :: FIXTag
tTradSesOpenTime = FIXTag 
   { tName = "TradSesOpenTime"
   , tnum = 342
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tTradSesPreCloseTime :: FIXTag
tTradSesPreCloseTime = FIXTag 
   { tName = "TradSesPreCloseTime"
   , tnum = 343
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tTradSesCloseTime :: FIXTag
tTradSesCloseTime = FIXTag 
   { tName = "TradSesCloseTime"
   , tnum = 344
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tTradSesEndTime :: FIXTag
tTradSesEndTime = FIXTag 
   { tName = "TradSesEndTime"
   , tnum = 345
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tNumberOfOrders :: FIXTag
tNumberOfOrders = FIXTag 
   { tName = "NumberOfOrders"
   , tnum = 346
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMessageEncoding :: FIXTag
tMessageEncoding = FIXTag 
   { tName = "MessageEncoding"
   , tnum = 347
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEncodedIssuerLen :: FIXTag
tEncodedIssuerLen = FIXTag 
   { tName = "EncodedIssuerLen"
   , tnum = 348
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedIssuer :: FIXTag
tEncodedIssuer = FIXTag 
   { tName = "EncodedIssuer"
   , tnum = 349
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedSecurityDescLen :: FIXTag
tEncodedSecurityDescLen = FIXTag 
   { tName = "EncodedSecurityDescLen"
   , tnum = 350
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedSecurityDesc :: FIXTag
tEncodedSecurityDesc = FIXTag 
   { tName = "EncodedSecurityDesc"
   , tnum = 351
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedListExecInstLen :: FIXTag
tEncodedListExecInstLen = FIXTag 
   { tName = "EncodedListExecInstLen"
   , tnum = 352
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedListExecInst :: FIXTag
tEncodedListExecInst = FIXTag 
   { tName = "EncodedListExecInst"
   , tnum = 353
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedTextLen :: FIXTag
tEncodedTextLen = FIXTag 
   { tName = "EncodedTextLen"
   , tnum = 354
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedText :: FIXTag
tEncodedText = FIXTag 
   { tName = "EncodedText"
   , tnum = 355
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedSubjectLen :: FIXTag
tEncodedSubjectLen = FIXTag 
   { tName = "EncodedSubjectLen"
   , tnum = 356
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedSubject :: FIXTag
tEncodedSubject = FIXTag 
   { tName = "EncodedSubject"
   , tnum = 357
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedHeadlineLen :: FIXTag
tEncodedHeadlineLen = FIXTag 
   { tName = "EncodedHeadlineLen"
   , tnum = 358
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedHeadline :: FIXTag
tEncodedHeadline = FIXTag 
   { tName = "EncodedHeadline"
   , tnum = 359
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedAllocTextLen :: FIXTag
tEncodedAllocTextLen = FIXTag 
   { tName = "EncodedAllocTextLen"
   , tnum = 360
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedAllocText :: FIXTag
tEncodedAllocText = FIXTag 
   { tName = "EncodedAllocText"
   , tnum = 361
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedUnderlyingIssuerLen :: FIXTag
tEncodedUnderlyingIssuerLen = FIXTag 
   { tName = "EncodedUnderlyingIssuerLen"
   , tnum = 362
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedUnderlyingIssuer :: FIXTag
tEncodedUnderlyingIssuer = FIXTag 
   { tName = "EncodedUnderlyingIssuer"
   , tnum = 363
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tEncodedUnderlyingSecurityDescLen :: FIXTag
tEncodedUnderlyingSecurityDescLen = FIXTag 
   { tName = "EncodedUnderlyingSecurityDescLen"
   , tnum = 364
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedUnderlyingSecurityDesc :: FIXTag
tEncodedUnderlyingSecurityDesc = FIXTag 
   { tName = "EncodedUnderlyingSecurityDesc"
   , tnum = 365
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tAllocPrice :: FIXTag
tAllocPrice = FIXTag 
   { tName = "AllocPrice"
   , tnum = 366
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tQuoteSetValidUntilTime :: FIXTag
tQuoteSetValidUntilTime = FIXTag 
   { tName = "QuoteSetValidUntilTime"
   , tnum = 367
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tQuoteEntryRejectReason :: FIXTag
tQuoteEntryRejectReason = FIXTag 
   { tName = "QuoteEntryRejectReason"
   , tnum = 368
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLastMsgSeqNumProcessed :: FIXTag
tLastMsgSeqNumProcessed = FIXTag 
   { tName = "LastMsgSeqNumProcessed"
   , tnum = 369
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOnBehalfOfSendingTime :: FIXTag
tOnBehalfOfSendingTime = FIXTag 
   { tName = "OnBehalfOfSendingTime"
   , tnum = 370
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tRefTagID :: FIXTag
tRefTagID = FIXTag 
   { tName = "RefTagID"
   , tnum = 371
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRefMsgType :: FIXTag
tRefMsgType = FIXTag 
   { tName = "RefMsgType"
   , tnum = 372
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSessionRejectReason :: FIXTag
tSessionRejectReason = FIXTag 
   { tName = "SessionRejectReason"
   , tnum = 373
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBidRequestTransType :: FIXTag
tBidRequestTransType = FIXTag 
   { tName = "BidRequestTransType"
   , tnum = 374
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tContraBroker :: FIXTag
tContraBroker = FIXTag 
   { tName = "ContraBroker"
   , tnum = 375
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tComplianceID :: FIXTag
tComplianceID = FIXTag 
   { tName = "ComplianceID"
   , tnum = 376
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSolicitedFlag :: FIXTag
tSolicitedFlag = FIXTag 
   { tName = "SolicitedFlag"
   , tnum = 377
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tExecRestatementReason :: FIXTag
tExecRestatementReason = FIXTag 
   { tName = "ExecRestatementReason"
   , tnum = 378
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBusinessRejectRefID :: FIXTag
tBusinessRejectRefID = FIXTag 
   { tName = "BusinessRejectRefID"
   , tnum = 379
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tBusinessRejectReason :: FIXTag
tBusinessRejectReason = FIXTag 
   { tName = "BusinessRejectReason"
   , tnum = 380
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tGrossTradeAmt :: FIXTag
tGrossTradeAmt = FIXTag 
   { tName = "GrossTradeAmt"
   , tnum = 381
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoContraBrokers :: FIXTag
tNoContraBrokers = FIXTag 
   { tName = "NoContraBrokers"
   , tnum = 382
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMaxMessageSize :: FIXTag
tMaxMessageSize = FIXTag 
   { tName = "MaxMessageSize"
   , tnum = 383
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoMsgTypes :: FIXTag
tNoMsgTypes = FIXTag 
   { tName = "NoMsgTypes"
   , tnum = 384
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMsgDirection :: FIXTag
tMsgDirection = FIXTag 
   { tName = "MsgDirection"
   , tnum = 385
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNoTradingSessions :: FIXTag
tNoTradingSessions = FIXTag 
   { tName = "NoTradingSessions"
   , tnum = 386
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTotalVolumeTraded :: FIXTag
tTotalVolumeTraded = FIXTag 
   { tName = "TotalVolumeTraded"
   , tnum = 387
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tDiscretionInst :: FIXTag
tDiscretionInst = FIXTag 
   { tName = "DiscretionInst"
   , tnum = 388
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tDiscretionOffset :: FIXTag
tDiscretionOffset = FIXTag 
   { tName = "DiscretionOffset"
   , tnum = 389
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBidID :: FIXTag
tBidID = FIXTag 
   { tName = "BidID"
   , tnum = 390
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tClientBidID :: FIXTag
tClientBidID = FIXTag 
   { tName = "ClientBidID"
   , tnum = 391
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tListName :: FIXTag
tListName = FIXTag 
   { tName = "ListName"
   , tnum = 392
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTotalNumSecurities :: FIXTag
tTotalNumSecurities = FIXTag 
   { tName = "TotalNumSecurities"
   , tnum = 393
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBidType :: FIXTag
tBidType = FIXTag 
   { tName = "BidType"
   , tnum = 394
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNumTickets :: FIXTag
tNumTickets = FIXTag 
   { tName = "NumTickets"
   , tnum = 395
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSideValue1 :: FIXTag
tSideValue1 = FIXTag 
   { tName = "SideValue1"
   , tnum = 396
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSideValue2 :: FIXTag
tSideValue2 = FIXTag 
   { tName = "SideValue2"
   , tnum = 397
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tNoBidDescriptors :: FIXTag
tNoBidDescriptors = FIXTag 
   { tName = "NoBidDescriptors"
   , tnum = 398
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBidDescriptorType :: FIXTag
tBidDescriptorType = FIXTag 
   { tName = "BidDescriptorType"
   , tnum = 399
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBidDescriptor :: FIXTag
tBidDescriptor = FIXTag 
   { tName = "BidDescriptor"
   , tnum = 400
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSideValueInd :: FIXTag
tSideValueInd = FIXTag 
   { tName = "SideValueInd"
   , tnum = 401
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLiquidityPctLow :: FIXTag
tLiquidityPctLow = FIXTag 
   { tName = "LiquidityPctLow"
   , tnum = 402
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLiquidityPctHigh :: FIXTag
tLiquidityPctHigh = FIXTag 
   { tName = "LiquidityPctHigh"
   , tnum = 403
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLiquidityValue :: FIXTag
tLiquidityValue = FIXTag 
   { tName = "LiquidityValue"
   , tnum = 404
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tEFPTrackingError :: FIXTag
tEFPTrackingError = FIXTag 
   { tName = "EFPTrackingError"
   , tnum = 405
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tFairValue :: FIXTag
tFairValue = FIXTag 
   { tName = "FairValue"
   , tnum = 406
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOutsideIndexPct :: FIXTag
tOutsideIndexPct = FIXTag 
   { tName = "OutsideIndexPct"
   , tnum = 407
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tValueOfFutures :: FIXTag
tValueOfFutures = FIXTag 
   { tName = "ValueOfFutures"
   , tnum = 408
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLiquidityIndType :: FIXTag
tLiquidityIndType = FIXTag 
   { tName = "LiquidityIndType"
   , tnum = 409
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tWtAverageLiquidity :: FIXTag
tWtAverageLiquidity = FIXTag 
   { tName = "WtAverageLiquidity"
   , tnum = 410
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tExchangeForPhysical :: FIXTag
tExchangeForPhysical = FIXTag 
   { tName = "ExchangeForPhysical"
   , tnum = 411
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tOutMainCntryUIndex :: FIXTag
tOutMainCntryUIndex = FIXTag 
   { tName = "OutMainCntryUIndex"
   , tnum = 412
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCrossPercent :: FIXTag
tCrossPercent = FIXTag 
   { tName = "CrossPercent"
   , tnum = 413
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tProgRptReqs :: FIXTag
tProgRptReqs = FIXTag 
   { tName = "ProgRptReqs"
   , tnum = 414
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tProgPeriodInterval :: FIXTag
tProgPeriodInterval = FIXTag 
   { tName = "ProgPeriodInterval"
   , tnum = 415
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tIncTaxInd :: FIXTag
tIncTaxInd = FIXTag 
   { tName = "IncTaxInd"
   , tnum = 416
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNumBidders :: FIXTag
tNumBidders = FIXTag 
   { tName = "NumBidders"
   , tnum = 417
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradeType :: FIXTag
tTradeType = FIXTag 
   { tName = "TradeType"
   , tnum = 418
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tBasisPxType :: FIXTag
tBasisPxType = FIXTag 
   { tName = "BasisPxType"
   , tnum = 419
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNoBidComponents :: FIXTag
tNoBidComponents = FIXTag 
   { tName = "NoBidComponents"
   , tnum = 420
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCountry :: FIXTag
tCountry = FIXTag 
   { tName = "Country"
   , tnum = 421
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTotNoStrikes :: FIXTag
tTotNoStrikes = FIXTag 
   { tName = "TotNoStrikes"
   , tnum = 422
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tPriceType :: FIXTag
tPriceType = FIXTag 
   { tName = "PriceType"
   , tnum = 423
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tDayOrderQty :: FIXTag
tDayOrderQty = FIXTag 
   { tName = "DayOrderQty"
   , tnum = 424
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tDayCumQty :: FIXTag
tDayCumQty = FIXTag 
   { tName = "DayCumQty"
   , tnum = 425
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tDayAvgPx :: FIXTag
tDayAvgPx = FIXTag 
   { tName = "DayAvgPx"
   , tnum = 426
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tGTBookingInst :: FIXTag
tGTBookingInst = FIXTag 
   { tName = "GTBookingInst"
   , tnum = 427
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoStrikes :: FIXTag
tNoStrikes = FIXTag 
   { tName = "NoStrikes"
   , tnum = 428
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tListStatusType :: FIXTag
tListStatusType = FIXTag 
   { tName = "ListStatusType"
   , tnum = 429
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNetGrossInd :: FIXTag
tNetGrossInd = FIXTag 
   { tName = "NetGrossInd"
   , tnum = 430
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tListOrderStatus :: FIXTag
tListOrderStatus = FIXTag 
   { tName = "ListOrderStatus"
   , tnum = 431
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tExpireDate :: FIXTag
tExpireDate = FIXTag 
   { tName = "ExpireDate"
   , tnum = 432
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tListExecInstType :: FIXTag
tListExecInstType = FIXTag 
   { tName = "ListExecInstType"
   , tnum = 433
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tCxlRejResponseTo :: FIXTag
tCxlRejResponseTo = FIXTag 
   { tName = "CxlRejResponseTo"
   , tnum = 434
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tUnderlyingCouponRate :: FIXTag
tUnderlyingCouponRate = FIXTag 
   { tName = "UnderlyingCouponRate"
   , tnum = 435
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tUnderlyingContractMultiplier :: FIXTag
tUnderlyingContractMultiplier = FIXTag 
   { tName = "UnderlyingContractMultiplier"
   , tnum = 436
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tContraTradeQty :: FIXTag
tContraTradeQty = FIXTag 
   { tName = "ContraTradeQty"
   , tnum = 437
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tContraTradeTime :: FIXTag
tContraTradeTime = FIXTag 
   { tName = "ContraTradeTime"
   , tnum = 438
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tClearingFirm :: FIXTag
tClearingFirm = FIXTag 
   { tName = "ClearingFirm"
   , tnum = 439
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tClearingAccount :: FIXTag
tClearingAccount = FIXTag 
   { tName = "ClearingAccount"
   , tnum = 440
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLiquidityNumSecurities :: FIXTag
tLiquidityNumSecurities = FIXTag 
   { tName = "LiquidityNumSecurities"
   , tnum = 441
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMultiLegReportingType :: FIXTag
tMultiLegReportingType = FIXTag 
   { tName = "MultiLegReportingType"
   , tnum = 442
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tStrikeTime :: FIXTag
tStrikeTime = FIXTag 
   { tName = "StrikeTime"
   , tnum = 443
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tListStatusText :: FIXTag
tListStatusText = FIXTag 
   { tName = "ListStatusText"
   , tnum = 444
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEncodedListStatusTextLen :: FIXTag
tEncodedListStatusTextLen = FIXTag 
   { tName = "EncodedListStatusTextLen"
   , tnum = 445
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedListStatusText :: FIXTag
tEncodedListStatusText = FIXTag 
   { tName = "EncodedListStatusText"
   , tnum = 446
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tPartyIDSource :: FIXTag
tPartyIDSource = FIXTag 
   { tName = "PartyIDSource"
   , tnum = 447
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tPartyID :: FIXTag
tPartyID = FIXTag 
   { tName = "PartyID"
   , tnum = 448
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTotalVolumeTradedDate :: FIXTag
tTotalVolumeTradedDate = FIXTag 
   { tName = "TotalVolumeTradedDate"
   , tnum = 449
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tTotalVolumeTradedTime :: FIXTag
tTotalVolumeTradedTime = FIXTag 
   { tName = "TotalVolumeTradedTime"
   , tnum = 450
   , tparser = toFIXTimeOnly
   , arbitraryValue = FIXTimeOnly <$> arbitrary }

tNetChgPrevDay :: FIXTag
tNetChgPrevDay = FIXTag 
   { tName = "NetChgPrevDay"
   , tnum = 451
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tPartyRole :: FIXTag
tPartyRole = FIXTag 
   { tName = "PartyRole"
   , tnum = 452
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoPartyIDs :: FIXTag
tNoPartyIDs = FIXTag 
   { tName = "NoPartyIDs"
   , tnum = 453
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoSecurityAltID :: FIXTag
tNoSecurityAltID = FIXTag 
   { tName = "NoSecurityAltID"
   , tnum = 454
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecurityAltID :: FIXTag
tSecurityAltID = FIXTag 
   { tName = "SecurityAltID"
   , tnum = 455
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecurityAltIDSource :: FIXTag
tSecurityAltIDSource = FIXTag 
   { tName = "SecurityAltIDSource"
   , tnum = 456
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoUnderlyingSecurityAltID :: FIXTag
tNoUnderlyingSecurityAltID = FIXTag 
   { tName = "NoUnderlyingSecurityAltID"
   , tnum = 457
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingSecurityAltID :: FIXTag
tUnderlyingSecurityAltID = FIXTag 
   { tName = "UnderlyingSecurityAltID"
   , tnum = 458
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingSecurityAltIDSource :: FIXTag
tUnderlyingSecurityAltIDSource = FIXTag 
   { tName = "UnderlyingSecurityAltIDSource"
   , tnum = 459
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tProduct :: FIXTag
tProduct = FIXTag 
   { tName = "Product"
   , tnum = 460
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCFICode :: FIXTag
tCFICode = FIXTag 
   { tName = "CFICode"
   , tnum = 461
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingProduct :: FIXTag
tUnderlyingProduct = FIXTag 
   { tName = "UnderlyingProduct"
   , tnum = 462
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUnderlyingCFICode :: FIXTag
tUnderlyingCFICode = FIXTag 
   { tName = "UnderlyingCFICode"
   , tnum = 463
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTestMessageIndicator :: FIXTag
tTestMessageIndicator = FIXTag 
   { tName = "TestMessageIndicator"
   , tnum = 464
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tQuantityType :: FIXTag
tQuantityType = FIXTag 
   { tName = "QuantityType"
   , tnum = 465
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tBookingRefID :: FIXTag
tBookingRefID = FIXTag 
   { tName = "BookingRefID"
   , tnum = 466
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tIndividualAllocID :: FIXTag
tIndividualAllocID = FIXTag 
   { tName = "IndividualAllocID"
   , tnum = 467
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRoundingDirection :: FIXTag
tRoundingDirection = FIXTag 
   { tName = "RoundingDirection"
   , tnum = 468
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tRoundingModulus :: FIXTag
tRoundingModulus = FIXTag 
   { tName = "RoundingModulus"
   , tnum = 469
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tCountryOfIssue :: FIXTag
tCountryOfIssue = FIXTag 
   { tName = "CountryOfIssue"
   , tnum = 470
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tStateOrProvinceOfIssue :: FIXTag
tStateOrProvinceOfIssue = FIXTag 
   { tName = "StateOrProvinceOfIssue"
   , tnum = 471
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLocaleOfIssue :: FIXTag
tLocaleOfIssue = FIXTag 
   { tName = "LocaleOfIssue"
   , tnum = 472
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoRegistDtls :: FIXTag
tNoRegistDtls = FIXTag 
   { tName = "NoRegistDtls"
   , tnum = 473
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMailingDtls :: FIXTag
tMailingDtls = FIXTag 
   { tName = "MailingDtls"
   , tnum = 474
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tInvestorCountryOfResidence :: FIXTag
tInvestorCountryOfResidence = FIXTag 
   { tName = "InvestorCountryOfResidence"
   , tnum = 475
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tPaymentRef :: FIXTag
tPaymentRef = FIXTag 
   { tName = "PaymentRef"
   , tnum = 476
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDistribPaymentMethod :: FIXTag
tDistribPaymentMethod = FIXTag 
   { tName = "DistribPaymentMethod"
   , tnum = 477
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCashDistribCurr :: FIXTag
tCashDistribCurr = FIXTag 
   { tName = "CashDistribCurr"
   , tnum = 478
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCommCurrency :: FIXTag
tCommCurrency = FIXTag 
   { tName = "CommCurrency"
   , tnum = 479
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCancellationRights :: FIXTag
tCancellationRights = FIXTag 
   { tName = "CancellationRights"
   , tnum = 480
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMoneyLaunderingStatus :: FIXTag
tMoneyLaunderingStatus = FIXTag 
   { tName = "MoneyLaunderingStatus"
   , tnum = 481
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMailingInst :: FIXTag
tMailingInst = FIXTag 
   { tName = "MailingInst"
   , tnum = 482
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTransBkdTime :: FIXTag
tTransBkdTime = FIXTag 
   { tName = "TransBkdTime"
   , tnum = 483
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tExecPriceType :: FIXTag
tExecPriceType = FIXTag 
   { tName = "ExecPriceType"
   , tnum = 484
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tExecPriceAdjustment :: FIXTag
tExecPriceAdjustment = FIXTag 
   { tName = "ExecPriceAdjustment"
   , tnum = 485
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tDateOfBirth :: FIXTag
tDateOfBirth = FIXTag 
   { tName = "DateOfBirth"
   , tnum = 486
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tTradeReportTransType :: FIXTag
tTradeReportTransType = FIXTag 
   { tName = "TradeReportTransType"
   , tnum = 487
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tCardHolderName :: FIXTag
tCardHolderName = FIXTag 
   { tName = "CardHolderName"
   , tnum = 488
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCardNumber :: FIXTag
tCardNumber = FIXTag 
   { tName = "CardNumber"
   , tnum = 489
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCardExpDate :: FIXTag
tCardExpDate = FIXTag 
   { tName = "CardExpDate"
   , tnum = 490
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tCardIssNo :: FIXTag
tCardIssNo = FIXTag 
   { tName = "CardIssNo"
   , tnum = 491
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tPaymentMethod :: FIXTag
tPaymentMethod = FIXTag 
   { tName = "PaymentMethod"
   , tnum = 492
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRegistAcctType :: FIXTag
tRegistAcctType = FIXTag 
   { tName = "RegistAcctType"
   , tnum = 493
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDesignation :: FIXTag
tDesignation = FIXTag 
   { tName = "Designation"
   , tnum = 494
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTaxAdvantageType :: FIXTag
tTaxAdvantageType = FIXTag 
   { tName = "TaxAdvantageType"
   , tnum = 495
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRegistRejReasonText :: FIXTag
tRegistRejReasonText = FIXTag 
   { tName = "RegistRejReasonText"
   , tnum = 496
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tFundRenewWaiv :: FIXTag
tFundRenewWaiv = FIXTag 
   { tName = "FundRenewWaiv"
   , tnum = 497
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tCashDistribAgentName :: FIXTag
tCashDistribAgentName = FIXTag 
   { tName = "CashDistribAgentName"
   , tnum = 498
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashDistribAgentCode :: FIXTag
tCashDistribAgentCode = FIXTag 
   { tName = "CashDistribAgentCode"
   , tnum = 499
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashDistribAgentAcctNumber :: FIXTag
tCashDistribAgentAcctNumber = FIXTag 
   { tName = "CashDistribAgentAcctNumber"
   , tnum = 500
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashDistribPayRef :: FIXTag
tCashDistribPayRef = FIXTag 
   { tName = "CashDistribPayRef"
   , tnum = 501
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashDistribAgentAcctName :: FIXTag
tCashDistribAgentAcctName = FIXTag 
   { tName = "CashDistribAgentAcctName"
   , tnum = 502
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCardStartDate :: FIXTag
tCardStartDate = FIXTag 
   { tName = "CardStartDate"
   , tnum = 503
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tPaymentDate :: FIXTag
tPaymentDate = FIXTag 
   { tName = "PaymentDate"
   , tnum = 504
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tPaymentRemitterID :: FIXTag
tPaymentRemitterID = FIXTag 
   { tName = "PaymentRemitterID"
   , tnum = 505
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRegistStatus :: FIXTag
tRegistStatus = FIXTag 
   { tName = "RegistStatus"
   , tnum = 506
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tRegistRejReasonCode :: FIXTag
tRegistRejReasonCode = FIXTag 
   { tName = "RegistRejReasonCode"
   , tnum = 507
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRegistRefID :: FIXTag
tRegistRefID = FIXTag 
   { tName = "RegistRefID"
   , tnum = 508
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRegistDetls :: FIXTag
tRegistDetls = FIXTag 
   { tName = "RegistDetls"
   , tnum = 509
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoDistribInsts :: FIXTag
tNoDistribInsts = FIXTag 
   { tName = "NoDistribInsts"
   , tnum = 510
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRegistEmail :: FIXTag
tRegistEmail = FIXTag 
   { tName = "RegistEmail"
   , tnum = 511
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tDistribPercentage :: FIXTag
tDistribPercentage = FIXTag 
   { tName = "DistribPercentage"
   , tnum = 512
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tRegistID :: FIXTag
tRegistID = FIXTag 
   { tName = "RegistID"
   , tnum = 513
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tRegistTransType :: FIXTag
tRegistTransType = FIXTag 
   { tName = "RegistTransType"
   , tnum = 514
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tExecValuationPoint :: FIXTag
tExecValuationPoint = FIXTag 
   { tName = "ExecValuationPoint"
   , tnum = 515
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tOrderPercent :: FIXTag
tOrderPercent = FIXTag 
   { tName = "OrderPercent"
   , tnum = 516
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOwnershipType :: FIXTag
tOwnershipType = FIXTag 
   { tName = "OwnershipType"
   , tnum = 517
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNoContAmts :: FIXTag
tNoContAmts = FIXTag 
   { tName = "NoContAmts"
   , tnum = 518
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tContAmtType :: FIXTag
tContAmtType = FIXTag 
   { tName = "ContAmtType"
   , tnum = 519
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tContAmtValue :: FIXTag
tContAmtValue = FIXTag 
   { tName = "ContAmtValue"
   , tnum = 520
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tContAmtCurr :: FIXTag
tContAmtCurr = FIXTag 
   { tName = "ContAmtCurr"
   , tnum = 521
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOwnerType :: FIXTag
tOwnerType = FIXTag 
   { tName = "OwnerType"
   , tnum = 522
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tPartySubID :: FIXTag
tPartySubID = FIXTag 
   { tName = "PartySubID"
   , tnum = 523
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNestedPartyID :: FIXTag
tNestedPartyID = FIXTag 
   { tName = "NestedPartyID"
   , tnum = 524
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNestedPartyIDSource :: FIXTag
tNestedPartyIDSource = FIXTag 
   { tName = "NestedPartyIDSource"
   , tnum = 525
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tSecondaryClOrdID :: FIXTag
tSecondaryClOrdID = FIXTag 
   { tName = "SecondaryClOrdID"
   , tnum = 526
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSecondaryExecID :: FIXTag
tSecondaryExecID = FIXTag 
   { tName = "SecondaryExecID"
   , tnum = 527
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOrderCapacity :: FIXTag
tOrderCapacity = FIXTag 
   { tName = "OrderCapacity"
   , tnum = 528
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tOrderRestrictions :: FIXTag
tOrderRestrictions = FIXTag 
   { tName = "OrderRestrictions"
   , tnum = 529
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tMassCancelRequestType :: FIXTag
tMassCancelRequestType = FIXTag 
   { tName = "MassCancelRequestType"
   , tnum = 530
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMassCancelResponse :: FIXTag
tMassCancelResponse = FIXTag 
   { tName = "MassCancelResponse"
   , tnum = 531
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMassCancelRejectReason :: FIXTag
tMassCancelRejectReason = FIXTag 
   { tName = "MassCancelRejectReason"
   , tnum = 532
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tTotalAffectedOrders :: FIXTag
tTotalAffectedOrders = FIXTag 
   { tName = "TotalAffectedOrders"
   , tnum = 533
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoAffectedOrders :: FIXTag
tNoAffectedOrders = FIXTag 
   { tName = "NoAffectedOrders"
   , tnum = 534
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAffectedOrderID :: FIXTag
tAffectedOrderID = FIXTag 
   { tName = "AffectedOrderID"
   , tnum = 535
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAffectedSecondaryOrderID :: FIXTag
tAffectedSecondaryOrderID = FIXTag 
   { tName = "AffectedSecondaryOrderID"
   , tnum = 536
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tQuoteType :: FIXTag
tQuoteType = FIXTag 
   { tName = "QuoteType"
   , tnum = 537
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNestedPartyRole :: FIXTag
tNestedPartyRole = FIXTag 
   { tName = "NestedPartyRole"
   , tnum = 538
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoNestedPartyIDs :: FIXTag
tNoNestedPartyIDs = FIXTag 
   { tName = "NoNestedPartyIDs"
   , tnum = 539
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTotalAccruedInterestAmt :: FIXTag
tTotalAccruedInterestAmt = FIXTag 
   { tName = "TotalAccruedInterestAmt"
   , tnum = 540
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMaturityDate :: FIXTag
tMaturityDate = FIXTag 
   { tName = "MaturityDate"
   , tnum = 541
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tUnderlyingMaturityDate :: FIXTag
tUnderlyingMaturityDate = FIXTag 
   { tName = "UnderlyingMaturityDate"
   , tnum = 542
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tInstrRegistry :: FIXTag
tInstrRegistry = FIXTag 
   { tName = "InstrRegistry"
   , tnum = 543
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCashMargin :: FIXTag
tCashMargin = FIXTag 
   { tName = "CashMargin"
   , tnum = 544
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tNestedPartySubID :: FIXTag
tNestedPartySubID = FIXTag 
   { tName = "NestedPartySubID"
   , tnum = 545
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tScope :: FIXTag
tScope = FIXTag 
   { tName = "Scope"
   , tnum = 546
   , tparser = toFIXMultipleValueString
   , arbitraryValue = FIXMultipleValueString <$> arbitrary }

tMDImplicitDelete :: FIXTag
tMDImplicitDelete = FIXTag 
   { tName = "MDImplicitDelete"
   , tnum = 547
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tCrossID :: FIXTag
tCrossID = FIXTag 
   { tName = "CrossID"
   , tnum = 548
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tCrossType :: FIXTag
tCrossType = FIXTag 
   { tName = "CrossType"
   , tnum = 549
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCrossPrioritization :: FIXTag
tCrossPrioritization = FIXTag 
   { tName = "CrossPrioritization"
   , tnum = 550
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOrigCrossID :: FIXTag
tOrigCrossID = FIXTag 
   { tName = "OrigCrossID"
   , tnum = 551
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoSides :: FIXTag
tNoSides = FIXTag 
   { tName = "NoSides"
   , tnum = 552
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tUsername :: FIXTag
tUsername = FIXTag 
   { tName = "Username"
   , tnum = 553
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tPassword :: FIXTag
tPassword = FIXTag 
   { tName = "Password"
   , tnum = 554
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoLegs :: FIXTag
tNoLegs = FIXTag 
   { tName = "NoLegs"
   , tnum = 555
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegCurrency :: FIXTag
tLegCurrency = FIXTag 
   { tName = "LegCurrency"
   , tnum = 556
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTotalNumSecurityTypes :: FIXTag
tTotalNumSecurityTypes = FIXTag 
   { tName = "TotalNumSecurityTypes"
   , tnum = 557
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoSecurityTypes :: FIXTag
tNoSecurityTypes = FIXTag 
   { tName = "NoSecurityTypes"
   , tnum = 558
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecurityListRequestType :: FIXTag
tSecurityListRequestType = FIXTag 
   { tName = "SecurityListRequestType"
   , tnum = 559
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSecurityRequestResult :: FIXTag
tSecurityRequestResult = FIXTag 
   { tName = "SecurityRequestResult"
   , tnum = 560
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tRoundLot :: FIXTag
tRoundLot = FIXTag 
   { tName = "RoundLot"
   , tnum = 561
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMinTradeVol :: FIXTag
tMinTradeVol = FIXTag 
   { tName = "MinTradeVol"
   , tnum = 562
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMultiLegRptTypeReq :: FIXTag
tMultiLegRptTypeReq = FIXTag 
   { tName = "MultiLegRptTypeReq"
   , tnum = 563
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegPositionEffect :: FIXTag
tLegPositionEffect = FIXTag 
   { tName = "LegPositionEffect"
   , tnum = 564
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLegCoveredOrUncovered :: FIXTag
tLegCoveredOrUncovered = FIXTag 
   { tName = "LegCoveredOrUncovered"
   , tnum = 565
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegPrice :: FIXTag
tLegPrice = FIXTag 
   { tName = "LegPrice"
   , tnum = 566
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tTradSesStatusRejReason :: FIXTag
tTradSesStatusRejReason = FIXTag 
   { tName = "TradSesStatusRejReason"
   , tnum = 567
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradeRequestID :: FIXTag
tTradeRequestID = FIXTag 
   { tName = "TradeRequestID"
   , tnum = 568
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradeRequestType :: FIXTag
tTradeRequestType = FIXTag 
   { tName = "TradeRequestType"
   , tnum = 569
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tPreviouslyReported :: FIXTag
tPreviouslyReported = FIXTag 
   { tName = "PreviouslyReported"
   , tnum = 570
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tTradeReportID :: FIXTag
tTradeReportID = FIXTag 
   { tName = "TradeReportID"
   , tnum = 571
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradeReportRefID :: FIXTag
tTradeReportRefID = FIXTag 
   { tName = "TradeReportRefID"
   , tnum = 572
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMatchStatus :: FIXTag
tMatchStatus = FIXTag 
   { tName = "MatchStatus"
   , tnum = 573
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tMatchType :: FIXTag
tMatchType = FIXTag 
   { tName = "MatchType"
   , tnum = 574
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tOddLot :: FIXTag
tOddLot = FIXTag 
   { tName = "OddLot"
   , tnum = 575
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tNoClearingInstructions :: FIXTag
tNoClearingInstructions = FIXTag 
   { tName = "NoClearingInstructions"
   , tnum = 576
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tClearingInstruction :: FIXTag
tClearingInstruction = FIXTag 
   { tName = "ClearingInstruction"
   , tnum = 577
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tTradeInputSource :: FIXTag
tTradeInputSource = FIXTag 
   { tName = "TradeInputSource"
   , tnum = 578
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tTradeInputDevice :: FIXTag
tTradeInputDevice = FIXTag 
   { tName = "TradeInputDevice"
   , tnum = 579
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoDates :: FIXTag
tNoDates = FIXTag 
   { tName = "NoDates"
   , tnum = 580
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tAccountType :: FIXTag
tAccountType = FIXTag 
   { tName = "AccountType"
   , tnum = 581
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tCustOrderCapacity :: FIXTag
tCustOrderCapacity = FIXTag 
   { tName = "CustOrderCapacity"
   , tnum = 582
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tClOrdLinkID :: FIXTag
tClOrdLinkID = FIXTag 
   { tName = "ClOrdLinkID"
   , tnum = 583
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMassStatusReqID :: FIXTag
tMassStatusReqID = FIXTag 
   { tName = "MassStatusReqID"
   , tnum = 584
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMassStatusReqType :: FIXTag
tMassStatusReqType = FIXTag 
   { tName = "MassStatusReqType"
   , tnum = 585
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tOrigOrdModTime :: FIXTag
tOrigOrdModTime = FIXTag 
   { tName = "OrigOrdModTime"
   , tnum = 586
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tLegSettlmntTyp :: FIXTag
tLegSettlmntTyp = FIXTag 
   { tName = "LegSettlmntTyp"
   , tnum = 587
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLegFutSettDate :: FIXTag
tLegFutSettDate = FIXTag 
   { tName = "LegFutSettDate"
   , tnum = 588
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tDayBookingInst :: FIXTag
tDayBookingInst = FIXTag 
   { tName = "DayBookingInst"
   , tnum = 589
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tBookingUnit :: FIXTag
tBookingUnit = FIXTag 
   { tName = "BookingUnit"
   , tnum = 590
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tPreallocMethod :: FIXTag
tPreallocMethod = FIXTag 
   { tName = "PreallocMethod"
   , tnum = 591
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tUnderlyingCountryOfIssue :: FIXTag
tUnderlyingCountryOfIssue = FIXTag 
   { tName = "UnderlyingCountryOfIssue"
   , tnum = 592
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingStateOrProvinceOfIssue :: FIXTag
tUnderlyingStateOrProvinceOfIssue = FIXTag 
   { tName = "UnderlyingStateOrProvinceOfIssue"
   , tnum = 593
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingLocaleOfIssue :: FIXTag
tUnderlyingLocaleOfIssue = FIXTag 
   { tName = "UnderlyingLocaleOfIssue"
   , tnum = 594
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tUnderlyingInstrRegistry :: FIXTag
tUnderlyingInstrRegistry = FIXTag 
   { tName = "UnderlyingInstrRegistry"
   , tnum = 595
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegCountryOfIssue :: FIXTag
tLegCountryOfIssue = FIXTag 
   { tName = "LegCountryOfIssue"
   , tnum = 596
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegStateOrProvinceOfIssue :: FIXTag
tLegStateOrProvinceOfIssue = FIXTag 
   { tName = "LegStateOrProvinceOfIssue"
   , tnum = 597
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegLocaleOfIssue :: FIXTag
tLegLocaleOfIssue = FIXTag 
   { tName = "LegLocaleOfIssue"
   , tnum = 598
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegInstrRegistry :: FIXTag
tLegInstrRegistry = FIXTag 
   { tName = "LegInstrRegistry"
   , tnum = 599
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSymbol :: FIXTag
tLegSymbol = FIXTag 
   { tName = "LegSymbol"
   , tnum = 600
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSymbolSfx :: FIXTag
tLegSymbolSfx = FIXTag 
   { tName = "LegSymbolSfx"
   , tnum = 601
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSecurityID :: FIXTag
tLegSecurityID = FIXTag 
   { tName = "LegSecurityID"
   , tnum = 602
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSecurityIDSource :: FIXTag
tLegSecurityIDSource = FIXTag 
   { tName = "LegSecurityIDSource"
   , tnum = 603
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tNoLegSecurityAltID :: FIXTag
tNoLegSecurityAltID = FIXTag 
   { tName = "NoLegSecurityAltID"
   , tnum = 604
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSecurityAltID :: FIXTag
tLegSecurityAltID = FIXTag 
   { tName = "LegSecurityAltID"
   , tnum = 605
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSecurityAltIDSource :: FIXTag
tLegSecurityAltIDSource = FIXTag 
   { tName = "LegSecurityAltIDSource"
   , tnum = 606
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegProduct :: FIXTag
tLegProduct = FIXTag 
   { tName = "LegProduct"
   , tnum = 607
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegCFICode :: FIXTag
tLegCFICode = FIXTag 
   { tName = "LegCFICode"
   , tnum = 608
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegSecurityType :: FIXTag
tLegSecurityType = FIXTag 
   { tName = "LegSecurityType"
   , tnum = 609
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegMaturityMonthYear :: FIXTag
tLegMaturityMonthYear = FIXTag 
   { tName = "LegMaturityMonthYear"
   , tnum = 610
   , tparser = toFIXMonthYear
   , arbitraryValue = FIXMonthYear <$> arbitrary }

tLegMaturityDate :: FIXTag
tLegMaturityDate = FIXTag 
   { tName = "LegMaturityDate"
   , tnum = 611
   , tparser = toFIXDateOnly
   , arbitraryValue = FIXDateOnly <$> arbitrary }

tLegStrikePrice :: FIXTag
tLegStrikePrice = FIXTag 
   { tName = "LegStrikePrice"
   , tnum = 612
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegOptAttribute :: FIXTag
tLegOptAttribute = FIXTag 
   { tName = "LegOptAttribute"
   , tnum = 613
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tLegContractMultiplier :: FIXTag
tLegContractMultiplier = FIXTag 
   { tName = "LegContractMultiplier"
   , tnum = 614
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegCouponRate :: FIXTag
tLegCouponRate = FIXTag 
   { tName = "LegCouponRate"
   , tnum = 615
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegSecurityExchange :: FIXTag
tLegSecurityExchange = FIXTag 
   { tName = "LegSecurityExchange"
   , tnum = 616
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegIssuer :: FIXTag
tLegIssuer = FIXTag 
   { tName = "LegIssuer"
   , tnum = 617
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEncodedLegIssuerLen :: FIXTag
tEncodedLegIssuerLen = FIXTag 
   { tName = "EncodedLegIssuerLen"
   , tnum = 618
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedLegIssuer :: FIXTag
tEncodedLegIssuer = FIXTag 
   { tName = "EncodedLegIssuer"
   , tnum = 619
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tLegSecurityDesc :: FIXTag
tLegSecurityDesc = FIXTag 
   { tName = "LegSecurityDesc"
   , tnum = 620
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tEncodedLegSecurityDescLen :: FIXTag
tEncodedLegSecurityDescLen = FIXTag 
   { tName = "EncodedLegSecurityDescLen"
   , tnum = 621
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tEncodedLegSecurityDesc :: FIXTag
tEncodedLegSecurityDesc = FIXTag 
   { tName = "EncodedLegSecurityDesc"
   , tnum = 622
   , tparser = toFIXData
   , arbitraryValue = FIXData <$> arbitrary }

tLegRatioQty :: FIXTag
tLegRatioQty = FIXTag 
   { tName = "LegRatioQty"
   , tnum = 623
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLegSide :: FIXTag
tLegSide = FIXTag 
   { tName = "LegSide"
   , tnum = 624
   , tparser = toFIXChar
   , arbitraryValue = FIXChar <$> arbitrary }

tTradingSessionSubID :: FIXTag
tTradingSessionSubID = FIXTag 
   { tName = "TradingSessionSubID"
   , tnum = 625
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tAllocType :: FIXTag
tAllocType = FIXTag 
   { tName = "AllocType"
   , tnum = 626
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tNoHops :: FIXTag
tNoHops = FIXTag 
   { tName = "NoHops"
   , tnum = 627
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tHopCompID :: FIXTag
tHopCompID = FIXTag 
   { tName = "HopCompID"
   , tnum = 628
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tHopSendingTime :: FIXTag
tHopSendingTime = FIXTag 
   { tName = "HopSendingTime"
   , tnum = 629
   , tparser = toFIXTimestamp
   , arbitraryValue = FIXTimestamp <$> arbitrary }

tHopRefID :: FIXTag
tHopRefID = FIXTag 
   { tName = "HopRefID"
   , tnum = 630
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tMidPx :: FIXTag
tMidPx = FIXTag 
   { tName = "MidPx"
   , tnum = 631
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBidYield :: FIXTag
tBidYield = FIXTag 
   { tName = "BidYield"
   , tnum = 632
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMidYield :: FIXTag
tMidYield = FIXTag 
   { tName = "MidYield"
   , tnum = 633
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferYield :: FIXTag
tOfferYield = FIXTag 
   { tName = "OfferYield"
   , tnum = 634
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tClearingFeeIndicator :: FIXTag
tClearingFeeIndicator = FIXTag 
   { tName = "ClearingFeeIndicator"
   , tnum = 635
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tWorkingIndicator :: FIXTag
tWorkingIndicator = FIXTag 
   { tName = "WorkingIndicator"
   , tnum = 636
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tLegLastPx :: FIXTag
tLegLastPx = FIXTag 
   { tName = "LegLastPx"
   , tnum = 637
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tPriorityIndicator :: FIXTag
tPriorityIndicator = FIXTag 
   { tName = "PriorityIndicator"
   , tnum = 638
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tPriceImprovement :: FIXTag
tPriceImprovement = FIXTag 
   { tName = "PriceImprovement"
   , tnum = 639
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tPrice2 :: FIXTag
tPrice2 = FIXTag 
   { tName = "Price2"
   , tnum = 640
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tLastForwardPoints2 :: FIXTag
tLastForwardPoints2 = FIXTag 
   { tName = "LastForwardPoints2"
   , tnum = 641
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tBidForwardPoints2 :: FIXTag
tBidForwardPoints2 = FIXTag 
   { tName = "BidForwardPoints2"
   , tnum = 642
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tOfferForwardPoints2 :: FIXTag
tOfferForwardPoints2 = FIXTag 
   { tName = "OfferForwardPoints2"
   , tnum = 643
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tRFQReqID :: FIXTag
tRFQReqID = FIXTag 
   { tName = "RFQReqID"
   , tnum = 644
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tMktBidPx :: FIXTag
tMktBidPx = FIXTag 
   { tName = "MktBidPx"
   , tnum = 645
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMktOfferPx :: FIXTag
tMktOfferPx = FIXTag 
   { tName = "MktOfferPx"
   , tnum = 646
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMinBidSize :: FIXTag
tMinBidSize = FIXTag 
   { tName = "MinBidSize"
   , tnum = 647
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tMinOfferSize :: FIXTag
tMinOfferSize = FIXTag 
   { tName = "MinOfferSize"
   , tnum = 648
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tQuoteStatusReqID :: FIXTag
tQuoteStatusReqID = FIXTag 
   { tName = "QuoteStatusReqID"
   , tnum = 649
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tLegalConfirm :: FIXTag
tLegalConfirm = FIXTag 
   { tName = "LegalConfirm"
   , tnum = 650
   , tparser = toFIXBool
   , arbitraryValue = FIXBool <$> arbitrary }

tUnderlyingLastPx :: FIXTag
tUnderlyingLastPx = FIXTag 
   { tName = "UnderlyingLastPx"
   , tnum = 651
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tUnderlyingLastQty :: FIXTag
tUnderlyingLastQty = FIXTag 
   { tName = "UnderlyingLastQty"
   , tnum = 652
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSecDefStatus :: FIXTag
tSecDefStatus = FIXTag 
   { tName = "SecDefStatus"
   , tnum = 653
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tLegRefID :: FIXTag
tLegRefID = FIXTag 
   { tName = "LegRefID"
   , tnum = 654
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tContraLegRefID :: FIXTag
tContraLegRefID = FIXTag 
   { tName = "ContraLegRefID"
   , tnum = 655
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

tSettlCurrBidFxRate :: FIXTag
tSettlCurrBidFxRate = FIXTag 
   { tName = "SettlCurrBidFxRate"
   , tnum = 656
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tSettlCurrOfferFxRate :: FIXTag
tSettlCurrOfferFxRate = FIXTag 
   { tName = "SettlCurrOfferFxRate"
   , tnum = 657
   , tparser = toFIXDouble
   , arbitraryValue = FIXDouble <$> arbitrary }

tQuoteRequestRejectReason :: FIXTag
tQuoteRequestRejectReason = FIXTag 
   { tName = "QuoteRequestRejectReason"
   , tnum = 658
   , tparser = toFIXInt
   , arbitraryValue = FIXInt <$> arbitrary }

tSideComplianceID :: FIXTag
tSideComplianceID = FIXTag 
   { tName = "SideComplianceID"
   , tnum = 659
   , tparser = toFIXString
   , arbitraryValue = FIXString <$> arbitrary }

headerFIX43 :: FIXTags
headerFIX43 = 
   LT.insert (tnum tSenderCompID) tSenderCompID $
   LT.insert (tnum tTargetCompID) tTargetCompID $
   LT.insert (tnum tOnBehalfOfCompID) tOnBehalfOfCompID $
   LT.insert (tnum tDeliverToCompID) tDeliverToCompID $
   LT.insert (tnum tSecureDataLen) tSecureDataLen $
   LT.insert (tnum tSecureData) tSecureData $
   LT.insert (tnum tMsgSeqNum) tMsgSeqNum $
   LT.insert (tnum tSenderSubID) tSenderSubID $
   LT.insert (tnum tSenderLocationID) tSenderLocationID $
   LT.insert (tnum tTargetSubID) tTargetSubID $
   LT.insert (tnum tTargetLocationID) tTargetLocationID $
   LT.insert (tnum tOnBehalfOfSubID) tOnBehalfOfSubID $
   LT.insert (tnum tOnBehalfOfLocationID) tOnBehalfOfLocationID $
   LT.insert (tnum tDeliverToSubID) tDeliverToSubID $
   LT.insert (tnum tDeliverToLocationID) tDeliverToLocationID $
   LT.insert (tnum tPossDupFlag) tPossDupFlag $
   LT.insert (tnum tPossResend) tPossResend $
   LT.insert (tnum tSendingTime) tSendingTime $
   LT.insert (tnum tOrigSendingTime) tOrigSendingTime $
   LT.insert (tnum tXmlDataLen) tXmlDataLen $
   LT.insert (tnum tXmlData) tXmlData $
   LT.insert (tnum tMessageEncoding) tMessageEncoding $
   LT.insert (tnum tLastMsgSeqNumProcessed) tLastMsgSeqNumProcessed $
   LT.insert (tnum tOnBehalfOfSendingTime) tOnBehalfOfSendingTime $
   LT.insert (tnum tNoHops) gNoHops''    LT.new
   where
      gNoHops'' = FIXTag
         { tName = "NoHops"
         , tnum = tnum tNoHops
         , tparser = gNoHopsP''
         , arbitraryValue = arbibtraryFIXGroup gNoHopsSpec'' }

      gNoHopsP'' = groupP gNoHopsSpec''
      gNoHopsSpec'' = FGSpec
         { gsLength = tNoHops
         , gsSeperator = tHopCompID
         , gsBody = gNoHopsBody'' }
         where
         gNoHopsBody'' = 
            LT.insert (tnum tHopSendingTime) tHopSendingTime $
            LT.insert (tnum tHopRefID) tHopRefID             LT.new



trailerFIX43 :: FIXTags
trailerFIX43 = 
   LT.insert (tnum tSignatureLength) tSignatureLength $
   LT.insert (tnum tSignature) tSignature    LT.new


mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { msName = "Heartbeat"
   , msType = C.pack "0"
   , msHeader = headerFIX43
   , msBody = mHeartbeatBody
   , msTrailer = trailerFIX43 }
   where
   mHeartbeatBody = 
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { msName = "TestRequest"
   , msType = C.pack "1"
   , msHeader = headerFIX43
   , msBody = mTestRequestBody
   , msTrailer = trailerFIX43 }
   where
   mTestRequestBody = 
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { msName = "ResendRequest"
   , msType = C.pack "2"
   , msHeader = headerFIX43
   , msBody = mResendRequestBody
   , msTrailer = trailerFIX43 }
   where
   mResendRequestBody = 
      LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
      LT.insert (tnum tEndSeqNo) tEndSeqNo       LT.new


mReject :: FIXMessageSpec
mReject = FMSpec
   { msName = "Reject"
   , msType = C.pack "3"
   , msHeader = headerFIX43
   , msBody = mRejectBody
   , msTrailer = trailerFIX43 }
   where
   mRejectBody = 
      LT.insert (tnum tRefSeqNum) tRefSeqNum $
      LT.insert (tnum tRefTagID) tRefTagID $
      LT.insert (tnum tRefMsgType) tRefMsgType $
      LT.insert (tnum tSessionRejectReason) tSessionRejectReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec
   { msName = "SequenceReset"
   , msType = C.pack "4"
   , msHeader = headerFIX43
   , msBody = mSequenceResetBody
   , msTrailer = trailerFIX43 }
   where
   mSequenceResetBody = 
      LT.insert (tnum tGapFillFlag) tGapFillFlag $
      LT.insert (tnum tNewSeqNo) tNewSeqNo       LT.new


mLogout :: FIXMessageSpec
mLogout = FMSpec
   { msName = "Logout"
   , msType = C.pack "5"
   , msHeader = headerFIX43
   , msBody = mLogoutBody
   , msTrailer = trailerFIX43 }
   where
   mLogoutBody = 
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mIOI :: FIXMessageSpec
mIOI = FMSpec
   { msName = "IOI"
   , msType = C.pack "6"
   , msHeader = headerFIX43
   , msBody = mIOIBody
   , msTrailer = trailerFIX43 }
   where
   mIOIBody = 
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tIOITransType) tIOITransType $
      LT.insert (tnum tIOIRefID) tIOIRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tIOIQty) tIOIQty $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tValidUntilTime) tValidUntilTime $
      LT.insert (tnum tIOIQltyInd) tIOIQltyInd $
      LT.insert (tnum tIOINaturalFlag) tIOINaturalFlag $
      LT.insert (tnum tNoIOIQualifiers) gNoIOIQualifiers''' $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tBenchmark) tBenchmark       LT.new
      where
         gNoIOIQualifiers''' = FIXTag
            { tName = "NoIOIQualifiers"
            , tnum = tnum tNoIOIQualifiers
            , tparser = gNoIOIQualifiersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoIOIQualifiersSpec''' }

         gNoIOIQualifiersP''' = groupP gNoIOIQualifiersSpec'''
         gNoIOIQualifiersSpec''' = FGSpec
            { gsLength = tNoIOIQualifiers
            , gsSeperator = tIOIQualifier
            , gsBody = gNoIOIQualifiersBody''' }
            where
            gNoIOIQualifiersBody''' = 
               LT.new

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeperator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' = 
               LT.insert (tnum tRoutingID) tRoutingID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { msName = "Advertisement"
   , msType = C.pack "7"
   , msHeader = headerFIX43
   , msBody = mAdvertisementBody
   , msTrailer = trailerFIX43 }
   where
   mAdvertisementBody = 
      LT.insert (tnum tAdvId) tAdvId $
      LT.insert (tnum tAdvTransType) tAdvTransType $
      LT.insert (tnum tAdvRefID) tAdvRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tAdvSide) tAdvSide $
      LT.insert (tnum tQuantity) tQuantity $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { msName = "ExecutionReport"
   , msType = C.pack "8"
   , msHeader = headerFIX43
   , msBody = mExecutionReportBody
   , msTrailer = trailerFIX43 }
   where
   mExecutionReportBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tNoContraBrokers) gNoContraBrokers''' $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tCrossID) tCrossID $
      LT.insert (tnum tOrigCrossID) tOrigCrossID $
      LT.insert (tnum tCrossType) tCrossType $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tExecRefID) tExecRefID $
      LT.insert (tnum tExecType) tExecType $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tWorkingIndicator) tWorkingIndicator $
      LT.insert (tnum tOrdRejReason) tOrdRejReason $
      LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDayBookingInst) tDayBookingInst $
      LT.insert (tnum tBookingUnit) tBookingUnit $
      LT.insert (tnum tPreallocMethod) tPreallocMethod $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tCashMargin) tCashMargin $
      LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tNoStipulations) gNoStipulations''' $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tOrderCapacity) tOrderCapacity $
      LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tLastQty) tLastQty $
      LT.insert (tnum tUnderlyingLastQty) tUnderlyingLastQty $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tUnderlyingLastPx) tUnderlyingLastPx $
      LT.insert (tnum tLastSpotRate) tLastSpotRate $
      LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tLastCapacity) tLastCapacity $
      LT.insert (tnum tLeavesQty) tLeavesQty $
      LT.insert (tnum tCumQty) tCumQty $
      LT.insert (tnum tAvgPx) tAvgPx $
      LT.insert (tnum tDayOrderQty) tDayOrderQty $
      LT.insert (tnum tDayCumQty) tDayCumQty $
      LT.insert (tnum tDayAvgPx) tDayAvgPx $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tReportToExch) tReportToExch $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCommCurrency) tCommCurrency $
      LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tYieldType) tYieldType $
      LT.insert (tnum tYield) tYield $
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
      LT.insert (tnum tExDate) tExDate $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
      LT.insert (tnum tTradedFlatSwitch) tTradedFlatSwitch $
      LT.insert (tnum tBasisFeatureDate) tBasisFeatureDate $
      LT.insert (tnum tBasisFeaturePrice) tBasisFeaturePrice $
      LT.insert (tnum tConcession) tConcession $
      LT.insert (tnum tTotalTakedown) tTotalTakedown $
      LT.insert (tnum tNetMoney) tNetMoney $
      LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
      LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tLastForwardPoints2) tLastForwardPoints2 $
      LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tTransBkdTime) tTransBkdTime $
      LT.insert (tnum tExecValuationPoint) tExecValuationPoint $
      LT.insert (tnum tExecPriceType) tExecPriceType $
      LT.insert (tnum tExecPriceAdjustment) tExecPriceAdjustment $
      LT.insert (tnum tPriorityIndicator) tPriorityIndicator $
      LT.insert (tnum tPriceImprovement) tPriceImprovement $
      LT.insert (tnum tNoContAmts) gNoContAmts''' $
      LT.insert (tnum tNoLegs) gNoLegs'''       LT.new
      where
         gNoContAmts''' = FIXTag
            { tName = "NoContAmts"
            , tnum = tnum tNoContAmts
            , tparser = gNoContAmtsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoContAmtsSpec''' }

         gNoContAmtsP''' = groupP gNoContAmtsSpec'''
         gNoContAmtsSpec''' = FGSpec
            { gsLength = tNoContAmts
            , gsSeperator = tContAmtType
            , gsBody = gNoContAmtsBody''' }
            where
            gNoContAmtsBody''' = 
               LT.insert (tnum tContAmtValue) tContAmtValue $
               LT.insert (tnum tContAmtCurr) tContAmtCurr                LT.new

         gNoContraBrokers''' = FIXTag
            { tName = "NoContraBrokers"
            , tnum = tnum tNoContraBrokers
            , tparser = gNoContraBrokersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoContraBrokersSpec''' }

         gNoContraBrokersP''' = groupP gNoContraBrokersSpec'''
         gNoContraBrokersSpec''' = FGSpec
            { gsLength = tNoContraBrokers
            , gsSeperator = tContraBroker
            , gsBody = gNoContraBrokersBody''' }
            where
            gNoContraBrokersBody''' = 
               LT.insert (tnum tContraTrader) tContraTrader $
               LT.insert (tnum tContraTradeQty) tContraTradeQty $
               LT.insert (tnum tContraTradeTime) tContraTradeTime $
               LT.insert (tnum tContraLegRefID) tContraLegRefID                LT.new

         gNoLegs''' = FIXTag
            { tName = "NoLegs"
            , tnum = tnum tNoLegs
            , tparser = gNoLegsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec''' }

         gNoLegsP''' = groupP gNoLegsSpec'''
         gNoLegsSpec''' = FGSpec
            { gsLength = tNoLegs
            , gsSeperator = tLegSymbol
            , gsBody = gNoLegsBody''' }
            where
            gNoLegsBody''' = 
               LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
               LT.insert (tnum tLegSecurityID) tLegSecurityID $
               LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
               LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID'''''' $
               LT.insert (tnum tLegProduct) tLegProduct $
               LT.insert (tnum tLegCFICode) tLegCFICode $
               LT.insert (tnum tLegSecurityType) tLegSecurityType $
               LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
               LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
               LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
               LT.insert (tnum tLegIssueDate) tLegIssueDate $
               LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
               LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
               LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
               LT.insert (tnum tLegFactor) tLegFactor $
               LT.insert (tnum tLegCreditRating) tLegCreditRating $
               LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
               LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
               LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
               LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
               LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
               LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
               LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
               LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
               LT.insert (tnum tLegCouponRate) tLegCouponRate $
               LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
               LT.insert (tnum tLegIssuer) tLegIssuer $
               LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
               LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
               LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
               LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
               LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
               LT.insert (tnum tLegRatioQty) tLegRatioQty $
               LT.insert (tnum tLegSide) tLegSide $
               LT.insert (tnum tLegPositionEffect) tLegPositionEffect $
               LT.insert (tnum tLegCoveredOrUncovered) tLegCoveredOrUncovered $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tLegRefID) tLegRefID $
               LT.insert (tnum tLegPrice) tLegPrice $
               LT.insert (tnum tLegSettlmntTyp) tLegSettlmntTyp $
               LT.insert (tnum tLegFutSettDate) tLegFutSettDate $
               LT.insert (tnum tLegLastPx) tLegLastPx                LT.new
               where
                  gNoLegSecurityAltID'''''' = FIXTag
                     { tName = "NoLegSecurityAltID"
                     , tnum = tnum tNoLegSecurityAltID
                     , tparser = gNoLegSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec'''''' }

                  gNoLegSecurityAltIDP'''''' = groupP gNoLegSecurityAltIDSpec''''''
                  gNoLegSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoLegSecurityAltID
                     , gsSeperator = tLegSecurityAltID
                     , gsBody = gNoLegSecurityAltIDBody'''''' }
                     where
                     gNoLegSecurityAltIDBody'''''' = 
                        LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                         LT.new

                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoStipulations''' = FIXTag
            { tName = "NoStipulations"
            , tnum = tnum tNoStipulations
            , tparser = gNoStipulationsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec''' }

         gNoStipulationsP''' = groupP gNoStipulationsSpec'''
         gNoStipulationsSpec''' = FGSpec
            { gsLength = tNoStipulations
            , gsSeperator = tStipulationType
            , gsBody = gNoStipulationsBody''' }
            where
            gNoStipulationsBody''' = 
               LT.insert (tnum tStipulationValue) tStipulationValue                LT.new



mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { msName = "OrderCancelReject"
   , msType = C.pack "9"
   , msHeader = headerFIX43
   , msBody = mOrderCancelRejectBody
   , msTrailer = trailerFIX43 }
   where
   mOrderCancelRejectBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tWorkingIndicator) tWorkingIndicator $
      LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tCxlRejResponseTo) tCxlRejResponseTo $
      LT.insert (tnum tCxlRejReason) tCxlRejReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mLogon :: FIXMessageSpec
mLogon = FMSpec
   { msName = "Logon"
   , msType = C.pack "A"
   , msHeader = headerFIX43
   , msBody = mLogonBody
   , msTrailer = trailerFIX43 }
   where
   mLogonBody = 
      LT.insert (tnum tEncryptMethod) tEncryptMethod $
      LT.insert (tnum tHeartBtInt) tHeartBtInt $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData $
      LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag $
      LT.insert (tnum tMaxMessageSize) tMaxMessageSize $
      LT.insert (tnum tNoMsgTypes) gNoMsgTypes''' $
      LT.insert (tnum tTestMessageIndicator) tTestMessageIndicator $
      LT.insert (tnum tUsername) tUsername $
      LT.insert (tnum tPassword) tPassword       LT.new
      where
         gNoMsgTypes''' = FIXTag
            { tName = "NoMsgTypes"
            , tnum = tnum tNoMsgTypes
            , tparser = gNoMsgTypesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoMsgTypesSpec''' }

         gNoMsgTypesP''' = groupP gNoMsgTypesSpec'''
         gNoMsgTypesSpec''' = FGSpec
            { gsLength = tNoMsgTypes
            , gsSeperator = tRefMsgType
            , gsBody = gNoMsgTypesBody''' }
            where
            gNoMsgTypesBody''' = 
               LT.insert (tnum tMsgDirection) tMsgDirection                LT.new



mNews :: FIXMessageSpec
mNews = FMSpec
   { msName = "News"
   , msType = C.pack "B"
   , msHeader = headerFIX43
   , msBody = mNewsBody
   , msTrailer = trailerFIX43 }
   where
   mNewsBody = 
      LT.insert (tnum tOrigTime) tOrigTime $
      LT.insert (tnum tUrgency) tUrgency $
      LT.insert (tnum tHeadline) tHeadline $
      LT.insert (tnum tEncodedHeadlineLen) tEncodedHeadlineLen $
      LT.insert (tnum tEncodedHeadline) tEncodedHeadline $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tLinesOfText) gLinesOfText''' $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData       LT.new
      where
         gLinesOfText''' = FIXTag
            { tName = "LinesOfText"
            , tnum = tnum tLinesOfText
            , tparser = gLinesOfTextP'''
            , arbitraryValue = arbibtraryFIXGroup gLinesOfTextSpec''' }

         gLinesOfTextP''' = groupP gLinesOfTextSpec'''
         gLinesOfTextSpec''' = FGSpec
            { gsLength = tLinesOfText
            , gsSeperator = tText
            , gsBody = gLinesOfTextBody''' }
            where
            gLinesOfTextBody''' = 
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new


         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeperator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' = 
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mEmail :: FIXMessageSpec
mEmail = FMSpec
   { msName = "Email"
   , msType = C.pack "C"
   , msHeader = headerFIX43
   , msBody = mEmailBody
   , msTrailer = trailerFIX43 }
   where
   mEmailBody = 
      LT.insert (tnum tEmailThreadID) tEmailThreadID $
      LT.insert (tnum tEmailType) tEmailType $
      LT.insert (tnum tOrigTime) tOrigTime $
      LT.insert (tnum tSubject) tSubject $
      LT.insert (tnum tEncodedSubjectLen) tEncodedSubjectLen $
      LT.insert (tnum tEncodedSubject) tEncodedSubject $
      LT.insert (tnum tNoRoutingIDs) gNoRoutingIDs''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tLinesOfText) gLinesOfText''' $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData       LT.new
      where
         gLinesOfText''' = FIXTag
            { tName = "LinesOfText"
            , tnum = tnum tLinesOfText
            , tparser = gLinesOfTextP'''
            , arbitraryValue = arbibtraryFIXGroup gLinesOfTextSpec''' }

         gLinesOfTextP''' = groupP gLinesOfTextSpec'''
         gLinesOfTextSpec''' = FGSpec
            { gsLength = tLinesOfText
            , gsSeperator = tText
            , gsBody = gLinesOfTextBody''' }
            where
            gLinesOfTextBody''' = 
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new


         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRoutingIDsSpec''' }

         gNoRoutingIDsP''' = groupP gNoRoutingIDsSpec'''
         gNoRoutingIDsSpec''' = FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeperator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' = 
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec
   { msName = "NewOrderSingle"
   , msType = C.pack "D"
   , msHeader = headerFIX43
   , msBody = mNewOrderSingleBody
   , msTrailer = trailerFIX43 }
   where
   mNewOrderSingleBody = 
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDayBookingInst) tDayBookingInst $
      LT.insert (tnum tBookingUnit) tBookingUnit $
      LT.insert (tnum tPreallocMethod) tPreallocMethod $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tCashMargin) tCashMargin $
      LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tNoStipulations) gNoStipulations''' $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tYieldType) tYieldType $
      LT.insert (tnum tYield) tYield $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCommCurrency) tCommCurrency $
      LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
      LT.insert (tnum tOrderCapacity) tOrderCapacity $
      LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tPrice2) tPrice2 $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tAllocQty) tAllocQty                LT.new
               where
                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoStipulations''' = FIXTag
            { tName = "NoStipulations"
            , tnum = tnum tNoStipulations
            , tparser = gNoStipulationsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec''' }

         gNoStipulationsP''' = groupP gNoStipulationsSpec'''
         gNoStipulationsSpec''' = FGSpec
            { gsLength = tNoStipulations
            , gsSeperator = tStipulationType
            , gsBody = gNoStipulationsBody''' }
            where
            gNoStipulationsBody''' = 
               LT.insert (tnum tStipulationValue) tStipulationValue                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { msName = "NewOrderList"
   , msType = C.pack "E"
   , msHeader = headerFIX43
   , msBody = mNewOrderListBody
   , msTrailer = trailerFIX43 }
   where
   mNewOrderListBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tProgRptReqs) tProgRptReqs $
      LT.insert (tnum tBidType) tBidType $
      LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tListExecInstType) tListExecInstType $
      LT.insert (tnum tListExecInst) tListExecInst $
      LT.insert (tnum tEncodedListExecInstLen) tEncodedListExecInstLen $
      LT.insert (tnum tEncodedListExecInst) tEncodedListExecInst $
      LT.insert (tnum tTotNoOrders) tTotNoOrders $
      LT.insert (tnum tNoOrders) gNoOrders'''       LT.new
      where
         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tListSeqNo) tListSeqNo $
               LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
               LT.insert (tnum tSettlInstMode) tSettlInstMode $
               LT.insert (tnum tNoPartyIDs) gNoPartyIDs'''''' $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tAccountType) tAccountType $
               LT.insert (tnum tDayBookingInst) tDayBookingInst $
               LT.insert (tnum tBookingUnit) tBookingUnit $
               LT.insert (tnum tPreallocMethod) tPreallocMethod $
               LT.insert (tnum tNoAllocs) gNoAllocs'''''' $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tCashMargin) tCashMargin $
               LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
               LT.insert (tnum tHandlInst) tHandlInst $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tMaxFloor) tMaxFloor $
               LT.insert (tnum tExDestination) tExDestination $
               LT.insert (tnum tNoTradingSessions) gNoTradingSessions'''''' $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tSymbol) tSymbol $
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tSideValueInd) tSideValueInd $
               LT.insert (tnum tLocateReqd) tLocateReqd $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tNoStipulations) gNoStipulations'''''' $
               LT.insert (tnum tQuantityType) tQuantityType $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrderPercent) tOrderPercent $
               LT.insert (tnum tRoundingDirection) tRoundingDirection $
               LT.insert (tnum tRoundingModulus) tRoundingModulus $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tPriceType) tPriceType $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tStopPx) tStopPx $
               LT.insert (tnum tSpread) tSpread $
               LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
               LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
               LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
               LT.insert (tnum tYieldType) tYieldType $
               LT.insert (tnum tYield) tYield $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tComplianceID) tComplianceID $
               LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
               LT.insert (tnum tIOIid) tIOIid $
               LT.insert (tnum tQuoteID) tQuoteID $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tEffectiveTime) tEffectiveTime $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tGTBookingInst) tGTBookingInst $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tOrderCapacity) tOrderCapacity $
               LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
               LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
               LT.insert (tnum tRule80A) tRule80A $
               LT.insert (tnum tForexReq) tForexReq $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tPrice2) tPrice2 $
               LT.insert (tnum tPositionEffect) tPositionEffect $
               LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
               LT.insert (tnum tMaxShow) tMaxShow $
               LT.insert (tnum tPegDifference) tPegDifference $
               LT.insert (tnum tDiscretionInst) tDiscretionInst $
               LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
               LT.insert (tnum tDesignation) tDesignation $
               LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
               LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
               LT.insert (tnum tNetMoney) tNetMoney                LT.new
               where
                  gNoAllocs'''''' = FIXTag
                     { tName = "NoAllocs"
                     , tnum = tnum tNoAllocs
                     , tparser = gNoAllocsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec'''''' }

                  gNoAllocsP'''''' = groupP gNoAllocsSpec''''''
                  gNoAllocsSpec'''''' = FGSpec
                     { gsLength = tNoAllocs
                     , gsSeperator = tAllocAccount
                     , gsBody = gNoAllocsBody'''''' }
                     where
                     gNoAllocsBody'''''' = 
                        LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
                        LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs''''''''' $
                        LT.insert (tnum tAllocQty) tAllocQty                         LT.new
                        where
                           gNoNestedPartyIDs''''''''' = FIXTag
                              { tName = "NoNestedPartyIDs"
                              , tnum = tnum tNoNestedPartyIDs
                              , tparser = gNoNestedPartyIDsP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec''''''''' }

                           gNoNestedPartyIDsP''''''''' = groupP gNoNestedPartyIDsSpec'''''''''
                           gNoNestedPartyIDsSpec''''''''' = FGSpec
                              { gsLength = tNoNestedPartyIDs
                              , gsSeperator = tNestedPartyID
                              , gsBody = gNoNestedPartyIDsBody''''''''' }
                              where
                              gNoNestedPartyIDsBody''''''''' = 
                                 LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                                 LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                                 LT.insert (tnum tNestedPartySubID) tNestedPartySubID                                  LT.new


                  gNoPartyIDs'''''' = FIXTag
                     { tName = "NoPartyIDs"
                     , tnum = tnum tNoPartyIDs
                     , tparser = gNoPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec'''''' }

                  gNoPartyIDsP'''''' = groupP gNoPartyIDsSpec''''''
                  gNoPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoPartyIDs
                     , gsSeperator = tPartyID
                     , gsBody = gNoPartyIDsBody'''''' }
                     where
                     gNoPartyIDsBody'''''' = 
                        LT.insert (tnum tPartyIDSource) tPartyIDSource $
                        LT.insert (tnum tPartyRole) tPartyRole $
                        LT.insert (tnum tPartySubID) tPartySubID                         LT.new

                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new

                  gNoStipulations'''''' = FIXTag
                     { tName = "NoStipulations"
                     , tnum = tnum tNoStipulations
                     , tparser = gNoStipulationsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec'''''' }

                  gNoStipulationsP'''''' = groupP gNoStipulationsSpec''''''
                  gNoStipulationsSpec'''''' = FGSpec
                     { gsLength = tNoStipulations
                     , gsSeperator = tStipulationType
                     , gsBody = gNoStipulationsBody'''''' }
                     where
                     gNoStipulationsBody'''''' = 
                        LT.insert (tnum tStipulationValue) tStipulationValue                         LT.new

                  gNoTradingSessions'''''' = FIXTag
                     { tName = "NoTradingSessions"
                     , tnum = tnum tNoTradingSessions
                     , tparser = gNoTradingSessionsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec'''''' }

                  gNoTradingSessionsP'''''' = groupP gNoTradingSessionsSpec''''''
                  gNoTradingSessionsSpec'''''' = FGSpec
                     { gsLength = tNoTradingSessions
                     , gsSeperator = tTradingSessionID
                     , gsBody = gNoTradingSessionsBody'''''' }
                     where
                     gNoTradingSessionsBody'''''' = 
                        LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                         LT.new




mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { msName = "OrderCancelRequest"
   , msType = C.pack "F"
   , msHeader = headerFIX43
   , msBody = mOrderCancelRequestBody
   , msTrailer = trailerFIX43 }
   where
   mOrderCancelRequestBody = 
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { msName = "OrderCancelReplaceRequest"
   , msType = C.pack "G"
   , msHeader = headerFIX43
   , msBody = mOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX43 }
   where
   mOrderCancelReplaceRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDayBookingInst) tDayBookingInst $
      LT.insert (tnum tBookingUnit) tBookingUnit $
      LT.insert (tnum tPreallocMethod) tPreallocMethod $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tCashMargin) tCashMargin $
      LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tYieldType) tYieldType $
      LT.insert (tnum tYield) tYield $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCommCurrency) tCommCurrency $
      LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
      LT.insert (tnum tOrderCapacity) tOrderCapacity $
      LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tPrice2) tPrice2 $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tAllocQty) tAllocQty                LT.new
               where
                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { msName = "OrderStatusRequest"
   , msType = C.pack "H"
   , msHeader = headerFIX43
   , msBody = mOrderStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mOrderStatusRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mAllocation :: FIXMessageSpec
mAllocation = FMSpec
   { msName = "Allocation"
   , msType = C.pack "J"
   , msHeader = headerFIX43
   , msBody = mAllocationBody
   , msTrailer = trailerFIX43 }
   where
   mAllocationBody = 
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tAllocTransType) tAllocTransType $
      LT.insert (tnum tAllocType) tAllocType $
      LT.insert (tnum tRefAllocID) tRefAllocID $
      LT.insert (tnum tAllocLinkID) tAllocLinkID $
      LT.insert (tnum tAllocLinkType) tAllocLinkType $
      LT.insert (tnum tBookingRefID) tBookingRefID $
      LT.insert (tnum tNoOrders) gNoOrders''' $
      LT.insert (tnum tNoExecs) gNoExecs''' $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tQuantity) tQuantity $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tAvgPx) tAvgPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tAvgPrxPrecision) tAvgPrxPrecision $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tConcession) tConcession $
      LT.insert (tnum tTotalTakedown) tTotalTakedown $
      LT.insert (tnum tNetMoney) tNetMoney $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tTotalAccruedInterestAmt) tTotalAccruedInterestAmt $
      LT.insert (tnum tLegalConfirm) tLegalConfirm $
      LT.insert (tnum tNoAllocs) gNoAllocs'''       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tAllocPrice) tAllocPrice $
               LT.insert (tnum tAllocQty) tAllocQty $
               LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tNotifyBrokerOfCredit) tNotifyBrokerOfCredit $
               LT.insert (tnum tAllocHandlInst) tAllocHandlInst $
               LT.insert (tnum tAllocText) tAllocText $
               LT.insert (tnum tEncodedAllocTextLen) tEncodedAllocTextLen $
               LT.insert (tnum tEncodedAllocText) tEncodedAllocText $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tAllocAvgPx) tAllocAvgPx $
               LT.insert (tnum tAllocNetMoney) tAllocNetMoney $
               LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
               LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
               LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
               LT.insert (tnum tSettlInstMode) tSettlInstMode $
               LT.insert (tnum tNoMiscFees) gNoMiscFees''''''                LT.new
               where
                  gNoMiscFees'''''' = FIXTag
                     { tName = "NoMiscFees"
                     , tnum = tnum tNoMiscFees
                     , tparser = gNoMiscFeesP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoMiscFeesSpec'''''' }

                  gNoMiscFeesP'''''' = groupP gNoMiscFeesSpec''''''
                  gNoMiscFeesSpec'''''' = FGSpec
                     { gsLength = tNoMiscFees
                     , gsSeperator = tMiscFeeAmt
                     , gsBody = gNoMiscFeesBody'''''' }
                     where
                     gNoMiscFeesBody'''''' = 
                        LT.insert (tnum tMiscFeeCurr) tMiscFeeCurr $
                        LT.insert (tnum tMiscFeeType) tMiscFeeType                         LT.new

                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoExecs''' = FIXTag
            { tName = "NoExecs"
            , tnum = tnum tNoExecs
            , tparser = gNoExecsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoExecsSpec''' }

         gNoExecsP''' = groupP gNoExecsSpec'''
         gNoExecsSpec''' = FGSpec
            { gsLength = tNoExecs
            , gsSeperator = tLastQty
            , gsBody = gNoExecsBody''' }
            where
            gNoExecsBody''' = 
               LT.insert (tnum tExecID) tExecID $
               LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
               LT.insert (tnum tLastPx) tLastPx $
               LT.insert (tnum tLastCapacity) tLastCapacity                LT.new

         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tListID) tListID                LT.new

         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { msName = "ListCancelRequest"
   , msType = C.pack "K"
   , msHeader = headerFIX43
   , msBody = mListCancelRequestBody
   , msTrailer = trailerFIX43 }
   where
   mListCancelRequestBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { msName = "ListExecute"
   , msType = C.pack "L"
   , msHeader = headerFIX43
   , msBody = mListExecuteBody
   , msTrailer = trailerFIX43 }
   where
   mListExecuteBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec
   { msName = "ListStatusRequest"
   , msType = C.pack "M"
   , msHeader = headerFIX43
   , msBody = mListStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mListStatusRequestBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListStatus :: FIXMessageSpec
mListStatus = FMSpec
   { msName = "ListStatus"
   , msType = C.pack "N"
   , msHeader = headerFIX43
   , msBody = mListStatusBody
   , msTrailer = trailerFIX43 }
   where
   mListStatusBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tListStatusType) tListStatusType $
      LT.insert (tnum tNoRpts) tNoRpts $
      LT.insert (tnum tListOrderStatus) tListOrderStatus $
      LT.insert (tnum tRptSeq) tRptSeq $
      LT.insert (tnum tListStatusText) tListStatusText $
      LT.insert (tnum tEncodedListStatusTextLen) tEncodedListStatusTextLen $
      LT.insert (tnum tEncodedListStatusText) tEncodedListStatusText $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tTotNoOrders) tTotNoOrders $
      LT.insert (tnum tNoOrders) gNoOrders'''       LT.new
      where
         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoOrdersSpec''' }

         gNoOrdersP''' = groupP gNoOrdersSpec'''
         gNoOrdersSpec''' = FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tCumQty) tCumQty $
               LT.insert (tnum tOrdStatus) tOrdStatus $
               LT.insert (tnum tWorkingIndicator) tWorkingIndicator $
               LT.insert (tnum tLeavesQty) tLeavesQty $
               LT.insert (tnum tCxlQty) tCxlQty $
               LT.insert (tnum tAvgPx) tAvgPx $
               LT.insert (tnum tOrdRejReason) tOrdRejReason $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mAllocationAck :: FIXMessageSpec
mAllocationAck = FMSpec
   { msName = "AllocationAck"
   , msType = C.pack "P"
   , msHeader = headerFIX43
   , msBody = mAllocationAckBody
   , msTrailer = trailerFIX43 }
   where
   mAllocationAckBody = 
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tAllocStatus) tAllocStatus $
      LT.insert (tnum tAllocRejCode) tAllocRejCode $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tLegalConfirm) tLegalConfirm       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new



mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { msName = "DontKnowTrade"
   , msType = C.pack "Q"
   , msHeader = headerFIX43
   , msBody = mDontKnowTradeBody
   , msTrailer = trailerFIX43 }
   where
   mDontKnowTradeBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tDKReason) tDKReason $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tLastQty) tLastQty $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { msName = "QuoteRequest"
   , msType = C.pack "R"
   , msHeader = headerFIX43
   , msBody = mQuoteRequestBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteRequestBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tRFQReqID) tRFQReqID $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tQuoteRequestType) tQuoteRequestType $
               LT.insert (tnum tQuoteType) tQuoteType $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tNoStipulations) gNoStipulations'''''' $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tQuantityType) tQuantityType $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tSpread) tSpread $
               LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
               LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
               LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
               LT.insert (tnum tPriceType) tPriceType $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tPrice2) tPrice2 $
               LT.insert (tnum tYieldType) tYieldType $
               LT.insert (tnum tYield) tYield                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new

                  gNoStipulations'''''' = FIXTag
                     { tName = "NoStipulations"
                     , tnum = tnum tNoStipulations
                     , tparser = gNoStipulationsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec'''''' }

                  gNoStipulationsP'''''' = groupP gNoStipulationsSpec''''''
                  gNoStipulationsSpec'''''' = FGSpec
                     { gsLength = tNoStipulations
                     , gsSeperator = tStipulationType
                     , gsBody = gNoStipulationsBody'''''' }
                     where
                     gNoStipulationsBody'''''' = 
                        LT.insert (tnum tStipulationValue) tStipulationValue                         LT.new




mQuote :: FIXMessageSpec
mQuote = FMSpec
   { msName = "Quote"
   , msType = C.pack "S"
   , msHeader = headerFIX43
   , msBody = mQuoteBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteType) tQuoteType $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tBidPx) tBidPx $
      LT.insert (tnum tOfferPx) tOfferPx $
      LT.insert (tnum tMktBidPx) tMktBidPx $
      LT.insert (tnum tMktOfferPx) tMktOfferPx $
      LT.insert (tnum tMinBidSize) tMinBidSize $
      LT.insert (tnum tBidSize) tBidSize $
      LT.insert (tnum tMinOfferSize) tMinOfferSize $
      LT.insert (tnum tOfferSize) tOfferSize $
      LT.insert (tnum tValidUntilTime) tValidUntilTime $
      LT.insert (tnum tBidSpotRate) tBidSpotRate $
      LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
      LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
      LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
      LT.insert (tnum tMidPx) tMidPx $
      LT.insert (tnum tBidYield) tBidYield $
      LT.insert (tnum tMidYield) tMidYield $
      LT.insert (tnum tOfferYield) tOfferYield $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
      LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSettlCurrBidFxRate) tSettlCurrBidFxRate $
      LT.insert (tnum tSettlCurrOfferFxRate) tSettlCurrOfferFxRate $
      LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { msName = "SettlementInstructions"
   , msType = C.pack "T"
   , msHeader = headerFIX43
   , msBody = mSettlementInstructionsBody
   , msTrailer = trailerFIX43 }
   where
   mSettlementInstructionsBody = 
      LT.insert (tnum tSettlInstID) tSettlInstID $
      LT.insert (tnum tSettlInstTransType) tSettlInstTransType $
      LT.insert (tnum tSettlInstRefID) tSettlInstRefID $
      LT.insert (tnum tSettlInstMode) tSettlInstMode $
      LT.insert (tnum tSettlInstSource) tSettlInstSource $
      LT.insert (tnum tAllocAccount) tAllocAccount $
      LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tStandInstDbType) tStandInstDbType $
      LT.insert (tnum tStandInstDbName) tStandInstDbName $
      LT.insert (tnum tStandInstDbID) tStandInstDbID $
      LT.insert (tnum tSettlDeliveryType) tSettlDeliveryType $
      LT.insert (tnum tSettlDepositoryCode) tSettlDepositoryCode $
      LT.insert (tnum tSettlBrkrCode) tSettlBrkrCode $
      LT.insert (tnum tSettlInstCode) tSettlInstCode $
      LT.insert (tnum tSecuritySettlAgentName) tSecuritySettlAgentName $
      LT.insert (tnum tSecuritySettlAgentCode) tSecuritySettlAgentCode $
      LT.insert (tnum tSecuritySettlAgentAcctNum) tSecuritySettlAgentAcctNum $
      LT.insert (tnum tSecuritySettlAgentAcctName) tSecuritySettlAgentAcctName $
      LT.insert (tnum tSecuritySettlAgentContactName) tSecuritySettlAgentContactName $
      LT.insert (tnum tSecuritySettlAgentContactPhone) tSecuritySettlAgentContactPhone $
      LT.insert (tnum tCashSettlAgentName) tCashSettlAgentName $
      LT.insert (tnum tCashSettlAgentCode) tCashSettlAgentCode $
      LT.insert (tnum tCashSettlAgentAcctNum) tCashSettlAgentAcctNum $
      LT.insert (tnum tCashSettlAgentAcctName) tCashSettlAgentAcctName $
      LT.insert (tnum tCashSettlAgentContactName) tCashSettlAgentContactName $
      LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone $
      LT.insert (tnum tPaymentMethod) tPaymentMethod $
      LT.insert (tnum tPaymentRef) tPaymentRef $
      LT.insert (tnum tCardHolderName) tCardHolderName $
      LT.insert (tnum tCardNumber) tCardNumber $
      LT.insert (tnum tCardStartDate) tCardStartDate $
      LT.insert (tnum tCardExpDate) tCardExpDate $
      LT.insert (tnum tCardIssNo) tCardIssNo $
      LT.insert (tnum tPaymentDate) tPaymentDate $
      LT.insert (tnum tPaymentRemitterID) tPaymentRemitterID       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new



mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec
   { msName = "MarketDataRequest"
   , msType = C.pack "V"
   , msHeader = headerFIX43
   , msBody = mMarketDataRequestBody
   , msTrailer = trailerFIX43 }
   where
   mMarketDataRequestBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tMarketDepth) tMarketDepth $
      LT.insert (tnum tMDUpdateType) tMDUpdateType $
      LT.insert (tnum tAggregatedBook) tAggregatedBook $
      LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
      LT.insert (tnum tScope) tScope $
      LT.insert (tnum tMDImplicitDelete) tMDImplicitDelete $
      LT.insert (tnum tNoMDEntryTypes) gNoMDEntryTypes''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions'''       LT.new
      where
         gNoMDEntryTypes''' = FIXTag
            { tName = "NoMDEntryTypes"
            , tnum = tnum tNoMDEntryTypes
            , tparser = gNoMDEntryTypesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoMDEntryTypesSpec''' }

         gNoMDEntryTypesP''' = groupP gNoMDEntryTypesSpec'''
         gNoMDEntryTypesSpec''' = FGSpec
            { gsLength = tNoMDEntryTypes
            , gsSeperator = tMDEntryType
            , gsBody = gNoMDEntryTypesBody''' }
            where
            gNoMDEntryTypesBody''' = 
               LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new


         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec
   { msName = "MarketDataSnapshotFullRefresh"
   , msType = C.pack "W"
   , msHeader = headerFIX43
   , msBody = mMarketDataSnapshotFullRefreshBody
   , msTrailer = trailerFIX43 }
   where
   mMarketDataSnapshotFullRefreshBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tFinancialStatus) tFinancialStatus $
      LT.insert (tnum tCorporateAction) tCorporateAction $
      LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
      LT.insert (tnum tTotalVolumeTradedDate) tTotalVolumeTradedDate $
      LT.insert (tnum tTotalVolumeTradedTime) tTotalVolumeTradedTime $
      LT.insert (tnum tNetChgPrevDay) tNetChgPrevDay $
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoMDEntriesSpec''' }

         gNoMDEntriesP''' = groupP gNoMDEntriesSpec'''
         gNoMDEntriesSpec''' = FGSpec
            { gsLength = tNoMDEntries
            , gsSeperator = tMDEntryType
            , gsBody = gNoMDEntriesBody''' }
            where
            gNoMDEntriesBody''' = 
               LT.insert (tnum tMDEntryPx) tMDEntryPx $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tMDEntrySize) tMDEntrySize $
               LT.insert (tnum tMDEntryDate) tMDEntryDate $
               LT.insert (tnum tMDEntryTime) tMDEntryTime $
               LT.insert (tnum tTickDirection) tTickDirection $
               LT.insert (tnum tMDMkt) tMDMkt $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tQuoteCondition) tQuoteCondition $
               LT.insert (tnum tTradeCondition) tTradeCondition $
               LT.insert (tnum tMDEntryOriginator) tMDEntryOriginator $
               LT.insert (tnum tLocationID) tLocationID $
               LT.insert (tnum tDeskID) tDeskID $
               LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tSellerDays) tSellerDays $
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tQuoteEntryID) tQuoteEntryID $
               LT.insert (tnum tMDEntryBuyer) tMDEntryBuyer $
               LT.insert (tnum tMDEntrySeller) tMDEntrySeller $
               LT.insert (tnum tNumberOfOrders) tNumberOfOrders $
               LT.insert (tnum tMDEntryPositionNo) tMDEntryPositionNo $
               LT.insert (tnum tScope) tScope $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec
   { msName = "MarketDataIncrementalRefresh"
   , msType = C.pack "X"
   , msHeader = headerFIX43
   , msBody = mMarketDataIncrementalRefreshBody
   , msTrailer = trailerFIX43 }
   where
   mMarketDataIncrementalRefreshBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoMDEntriesSpec''' }

         gNoMDEntriesP''' = groupP gNoMDEntriesSpec'''
         gNoMDEntriesSpec''' = FGSpec
            { gsLength = tNoMDEntries
            , gsSeperator = tMDUpdateAction
            , gsBody = gNoMDEntriesBody''' }
            where
            gNoMDEntriesBody''' = 
               LT.insert (tnum tDeleteReason) tDeleteReason $
               LT.insert (tnum tMDEntryType) tMDEntryType $
               LT.insert (tnum tMDEntryID) tMDEntryID $
               LT.insert (tnum tMDEntryRefID) tMDEntryRefID $
               LT.insert (tnum tSymbol) tSymbol $
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tFinancialStatus) tFinancialStatus $
               LT.insert (tnum tCorporateAction) tCorporateAction $
               LT.insert (tnum tMDEntryPx) tMDEntryPx $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tMDEntrySize) tMDEntrySize $
               LT.insert (tnum tMDEntryDate) tMDEntryDate $
               LT.insert (tnum tMDEntryTime) tMDEntryTime $
               LT.insert (tnum tTickDirection) tTickDirection $
               LT.insert (tnum tMDMkt) tMDMkt $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tQuoteCondition) tQuoteCondition $
               LT.insert (tnum tTradeCondition) tTradeCondition $
               LT.insert (tnum tMDEntryOriginator) tMDEntryOriginator $
               LT.insert (tnum tLocationID) tLocationID $
               LT.insert (tnum tDeskID) tDeskID $
               LT.insert (tnum tOpenCloseSettleFlag) tOpenCloseSettleFlag $
               LT.insert (tnum tTimeInForce) tTimeInForce $
               LT.insert (tnum tExpireDate) tExpireDate $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tMinQty) tMinQty $
               LT.insert (tnum tExecInst) tExecInst $
               LT.insert (tnum tSellerDays) tSellerDays $
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tQuoteEntryID) tQuoteEntryID $
               LT.insert (tnum tMDEntryBuyer) tMDEntryBuyer $
               LT.insert (tnum tMDEntrySeller) tMDEntrySeller $
               LT.insert (tnum tNumberOfOrders) tNumberOfOrders $
               LT.insert (tnum tMDEntryPositionNo) tMDEntryPositionNo $
               LT.insert (tnum tScope) tScope $
               LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
               LT.insert (tnum tTotalVolumeTradedDate) tTotalVolumeTradedDate $
               LT.insert (tnum tTotalVolumeTradedTime) tTotalVolumeTradedTime $
               LT.insert (tnum tNetChgPrevDay) tNetChgPrevDay $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new




mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec
   { msName = "MarketDataRequestReject"
   , msType = C.pack "Y"
   , msHeader = headerFIX43
   , msBody = mMarketDataRequestRejectBody
   , msTrailer = trailerFIX43 }
   where
   mMarketDataRequestRejectBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mQuoteCancel :: FIXMessageSpec
mQuoteCancel = FMSpec
   { msName = "QuoteCancel"
   , msType = C.pack "Z"
   , msHeader = headerFIX43
   , msBody = mQuoteCancelBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteCancelBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries'''       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoQuoteEntries''' = FIXTag
            { tName = "NoQuoteEntries"
            , tnum = tnum tNoQuoteEntries
            , tparser = gNoQuoteEntriesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoQuoteEntriesSpec''' }

         gNoQuoteEntriesP''' = groupP gNoQuoteEntriesSpec'''
         gNoQuoteEntriesSpec''' = FGSpec
            { gsLength = tNoQuoteEntries
            , gsSeperator = tSymbol
            , gsBody = gNoQuoteEntriesBody''' }
            where
            gNoQuoteEntriesBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new




mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec
   { msName = "QuoteStatusRequest"
   , msType = C.pack "a"
   , msHeader = headerFIX43
   , msBody = mQuoteStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteStatusRequestBody = 
      LT.insert (tnum tQuoteStatusReqID) tQuoteStatusReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mMassQuoteAcknowledgement :: FIXMessageSpec
mMassQuoteAcknowledgement = FMSpec
   { msName = "MassQuoteAcknowledgement"
   , msType = C.pack "b"
   , msHeader = headerFIX43
   , msBody = mMassQuoteAcknowledgementBody
   , msTrailer = trailerFIX43 }
   where
   mMassQuoteAcknowledgementBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteStatus) tQuoteStatus $
      LT.insert (tnum tQuoteRejectReason) tQuoteRejectReason $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tQuoteType) tQuoteType $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoQuoteSetsSpec''' }

         gNoQuoteSetsP''' = groupP gNoQuoteSetsSpec'''
         gNoQuoteSetsSpec''' = FGSpec
            { gsLength = tNoQuoteSets
            , gsSeperator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' = 
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
               LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID'''''' $
               LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
               LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
               LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
               LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
               LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
               LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
               LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
               LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
               LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
               LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
               LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
               LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
               LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
               LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tTotQuoteEntries) tTotQuoteEntries $
               LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries''''''                LT.new
               where
                  gNoQuoteEntries'''''' = FIXTag
                     { tName = "NoQuoteEntries"
                     , tnum = tnum tNoQuoteEntries
                     , tparser = gNoQuoteEntriesP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoQuoteEntriesSpec'''''' }

                  gNoQuoteEntriesP'''''' = groupP gNoQuoteEntriesSpec''''''
                  gNoQuoteEntriesSpec'''''' = FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeperator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' = 
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
                        LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''''''''' $
                        LT.insert (tnum tProduct) tProduct $
                        LT.insert (tnum tCFICode) tCFICode $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDate) tMaturityDate $
                        LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
                        LT.insert (tnum tIssueDate) tIssueDate $
                        LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
                        LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
                        LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
                        LT.insert (tnum tFactor) tFactor $
                        LT.insert (tnum tCreditRating) tCreditRating $
                        LT.insert (tnum tInstrRegistry) tInstrRegistry $
                        LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
                        LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
                        LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
                        LT.insert (tnum tRedemptionDate) tRedemptionDate $
                        LT.insert (tnum tStrikePrice) tStrikePrice $
                        LT.insert (tnum tOptAttribute) tOptAttribute $
                        LT.insert (tnum tContractMultiplier) tContractMultiplier $
                        LT.insert (tnum tCouponRate) tCouponRate $
                        LT.insert (tnum tSecurityExchange) tSecurityExchange $
                        LT.insert (tnum tIssuer) tIssuer $
                        LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
                        LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
                        LT.insert (tnum tSecurityDesc) tSecurityDesc $
                        LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
                        LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
                        LT.insert (tnum tBidPx) tBidPx $
                        LT.insert (tnum tOfferPx) tOfferPx $
                        LT.insert (tnum tBidSize) tBidSize $
                        LT.insert (tnum tOfferSize) tOfferSize $
                        LT.insert (tnum tValidUntilTime) tValidUntilTime $
                        LT.insert (tnum tBidSpotRate) tBidSpotRate $
                        LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
                        LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
                        LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
                        LT.insert (tnum tMidPx) tMidPx $
                        LT.insert (tnum tBidYield) tBidYield $
                        LT.insert (tnum tMidYield) tMidYield $
                        LT.insert (tnum tOfferYield) tOfferYield $
                        LT.insert (tnum tTransactTime) tTransactTime $
                        LT.insert (tnum tTradingSessionID) tTradingSessionID $
                        LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
                        LT.insert (tnum tFutSettDate) tFutSettDate $
                        LT.insert (tnum tOrdType) tOrdType $
                        LT.insert (tnum tFutSettDate2) tFutSettDate2 $
                        LT.insert (tnum tOrderQty2) tOrderQty2 $
                        LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
                        LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
                        LT.insert (tnum tCurrency) tCurrency $
                        LT.insert (tnum tQuoteEntryRejectReason) tQuoteEntryRejectReason                         LT.new
                        where
                           gNoSecurityAltID''''''''' = FIXTag
                              { tName = "NoSecurityAltID"
                              , tnum = tnum tNoSecurityAltID
                              , tparser = gNoSecurityAltIDP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''''''''' }

                           gNoSecurityAltIDP''''''''' = groupP gNoSecurityAltIDSpec'''''''''
                           gNoSecurityAltIDSpec''''''''' = FGSpec
                              { gsLength = tNoSecurityAltID
                              , gsSeperator = tSecurityAltID
                              , gsBody = gNoSecurityAltIDBody''''''''' }
                              where
                              gNoSecurityAltIDBody''''''''' = 
                                 LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                                  LT.new


                  gNoUnderlyingSecurityAltID'''''' = FIXTag
                     { tName = "NoUnderlyingSecurityAltID"
                     , tnum = tnum tNoUnderlyingSecurityAltID
                     , tparser = gNoUnderlyingSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec'''''' }

                  gNoUnderlyingSecurityAltIDP'''''' = groupP gNoUnderlyingSecurityAltIDSpec''''''
                  gNoUnderlyingSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoUnderlyingSecurityAltID
                     , gsSeperator = tUnderlyingSecurityAltID
                     , gsBody = gNoUnderlyingSecurityAltIDBody'''''' }
                     where
                     gNoUnderlyingSecurityAltIDBody'''''' = 
                        LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                         LT.new




mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec
   { msName = "SecurityDefinitionRequest"
   , msType = C.pack "c"
   , msHeader = headerFIX43
   , msBody = mSecurityDefinitionRequestBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityDefinitionRequestBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityRequestType) tSecurityRequestType $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tNoLegs) gNoLegs''' $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoLegs''' = FIXTag
            { tName = "NoLegs"
            , tnum = tnum tNoLegs
            , tparser = gNoLegsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec''' }

         gNoLegsP''' = groupP gNoLegsSpec'''
         gNoLegsSpec''' = FGSpec
            { gsLength = tNoLegs
            , gsSeperator = tLegSymbol
            , gsBody = gNoLegsBody''' }
            where
            gNoLegsBody''' = 
               LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
               LT.insert (tnum tLegSecurityID) tLegSecurityID $
               LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
               LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID'''''' $
               LT.insert (tnum tLegProduct) tLegProduct $
               LT.insert (tnum tLegCFICode) tLegCFICode $
               LT.insert (tnum tLegSecurityType) tLegSecurityType $
               LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
               LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
               LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
               LT.insert (tnum tLegIssueDate) tLegIssueDate $
               LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
               LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
               LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
               LT.insert (tnum tLegFactor) tLegFactor $
               LT.insert (tnum tLegCreditRating) tLegCreditRating $
               LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
               LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
               LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
               LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
               LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
               LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
               LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
               LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
               LT.insert (tnum tLegCouponRate) tLegCouponRate $
               LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
               LT.insert (tnum tLegIssuer) tLegIssuer $
               LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
               LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
               LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
               LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
               LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
               LT.insert (tnum tLegRatioQty) tLegRatioQty $
               LT.insert (tnum tLegSide) tLegSide $
               LT.insert (tnum tLegCurrency) tLegCurrency                LT.new
               where
                  gNoLegSecurityAltID'''''' = FIXTag
                     { tName = "NoLegSecurityAltID"
                     , tnum = tnum tNoLegSecurityAltID
                     , tparser = gNoLegSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec'''''' }

                  gNoLegSecurityAltIDP'''''' = groupP gNoLegSecurityAltIDSpec''''''
                  gNoLegSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoLegSecurityAltID
                     , gsSeperator = tLegSecurityAltID
                     , gsBody = gNoLegSecurityAltIDBody'''''' }
                     where
                     gNoLegSecurityAltIDBody'''''' = 
                        LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                         LT.new


         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec
   { msName = "SecurityDefinition"
   , msType = C.pack "d"
   , msHeader = headerFIX43
   , msBody = mSecurityDefinitionBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityDefinitionBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNoLegs) gNoLegs''' $
      LT.insert (tnum tRoundLot) tRoundLot $
      LT.insert (tnum tMinTradeVol) tMinTradeVol       LT.new
      where
         gNoLegs''' = FIXTag
            { tName = "NoLegs"
            , tnum = tnum tNoLegs
            , tparser = gNoLegsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec''' }

         gNoLegsP''' = groupP gNoLegsSpec'''
         gNoLegsSpec''' = FGSpec
            { gsLength = tNoLegs
            , gsSeperator = tLegSymbol
            , gsBody = gNoLegsBody''' }
            where
            gNoLegsBody''' = 
               LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
               LT.insert (tnum tLegSecurityID) tLegSecurityID $
               LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
               LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID'''''' $
               LT.insert (tnum tLegProduct) tLegProduct $
               LT.insert (tnum tLegCFICode) tLegCFICode $
               LT.insert (tnum tLegSecurityType) tLegSecurityType $
               LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
               LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
               LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
               LT.insert (tnum tLegIssueDate) tLegIssueDate $
               LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
               LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
               LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
               LT.insert (tnum tLegFactor) tLegFactor $
               LT.insert (tnum tLegCreditRating) tLegCreditRating $
               LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
               LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
               LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
               LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
               LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
               LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
               LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
               LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
               LT.insert (tnum tLegCouponRate) tLegCouponRate $
               LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
               LT.insert (tnum tLegIssuer) tLegIssuer $
               LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
               LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
               LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
               LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
               LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
               LT.insert (tnum tLegRatioQty) tLegRatioQty $
               LT.insert (tnum tLegSide) tLegSide $
               LT.insert (tnum tLegCurrency) tLegCurrency                LT.new
               where
                  gNoLegSecurityAltID'''''' = FIXTag
                     { tName = "NoLegSecurityAltID"
                     , tnum = tnum tNoLegSecurityAltID
                     , tparser = gNoLegSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec'''''' }

                  gNoLegSecurityAltIDP'''''' = groupP gNoLegSecurityAltIDSpec''''''
                  gNoLegSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoLegSecurityAltID
                     , gsSeperator = tLegSecurityAltID
                     , gsBody = gNoLegSecurityAltIDBody'''''' }
                     where
                     gNoLegSecurityAltIDBody'''''' = 
                        LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                         LT.new


         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec
   { msName = "SecurityStatusRequest"
   , msType = C.pack "e"
   , msHeader = headerFIX43
   , msBody = mSecurityStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityStatusRequestBody = 
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec
   { msName = "SecurityStatus"
   , msType = C.pack "f"
   , msHeader = headerFIX43
   , msBody = mSecurityStatusBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityStatusBody = 
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
      LT.insert (tnum tSecurityTradingStatus) tSecurityTradingStatus $
      LT.insert (tnum tFinancialStatus) tFinancialStatus $
      LT.insert (tnum tCorporateAction) tCorporateAction $
      LT.insert (tnum tHaltReasonChar) tHaltReasonChar $
      LT.insert (tnum tInViewOfCommon) tInViewOfCommon $
      LT.insert (tnum tDueToRelated) tDueToRelated $
      LT.insert (tnum tBuyVolume) tBuyVolume $
      LT.insert (tnum tSellVolume) tSellVolume $
      LT.insert (tnum tHighPx) tHighPx $
      LT.insert (tnum tLowPx) tLowPx $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tAdjustment) tAdjustment $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec
   { msName = "TradingSessionStatusRequest"
   , msType = C.pack "g"
   , msHeader = headerFIX43
   , msBody = mTradingSessionStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mTradingSessionStatusRequestBody = 
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new


mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec
   { msName = "TradingSessionStatus"
   , msType = C.pack "h"
   , msHeader = headerFIX43
   , msBody = mTradingSessionStatusBody
   , msTrailer = trailerFIX43 }
   where
   mTradingSessionStatusBody = 
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
      LT.insert (tnum tTradSesStatus) tTradSesStatus $
      LT.insert (tnum tTradSesStatusRejReason) tTradSesStatusRejReason $
      LT.insert (tnum tTradSesStartTime) tTradSesStartTime $
      LT.insert (tnum tTradSesOpenTime) tTradSesOpenTime $
      LT.insert (tnum tTradSesPreCloseTime) tTradSesPreCloseTime $
      LT.insert (tnum tTradSesCloseTime) tTradSesCloseTime $
      LT.insert (tnum tTradSesEndTime) tTradSesEndTime $
      LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mMassQuote :: FIXMessageSpec
mMassQuote = FMSpec
   { msName = "MassQuote"
   , msType = C.pack "i"
   , msHeader = headerFIX43
   , msBody = mMassQuoteBody
   , msTrailer = trailerFIX43 }
   where
   mMassQuoteBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteType) tQuoteType $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDefBidSize) tDefBidSize $
      LT.insert (tnum tDefOfferSize) tDefOfferSize $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoQuoteSetsSpec''' }

         gNoQuoteSetsP''' = groupP gNoQuoteSetsSpec'''
         gNoQuoteSetsSpec''' = FGSpec
            { gsLength = tNoQuoteSets
            , gsSeperator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' = 
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
               LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID'''''' $
               LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
               LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
               LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
               LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
               LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
               LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
               LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
               LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
               LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
               LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
               LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
               LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
               LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
               LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
               LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
               LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
               LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
               LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
               LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
               LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
               LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
               LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
               LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
               LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
               LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
               LT.insert (tnum tQuoteSetValidUntilTime) tQuoteSetValidUntilTime $
               LT.insert (tnum tTotQuoteEntries) tTotQuoteEntries $
               LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries''''''                LT.new
               where
                  gNoQuoteEntries'''''' = FIXTag
                     { tName = "NoQuoteEntries"
                     , tnum = tnum tNoQuoteEntries
                     , tparser = gNoQuoteEntriesP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoQuoteEntriesSpec'''''' }

                  gNoQuoteEntriesP'''''' = groupP gNoQuoteEntriesSpec''''''
                  gNoQuoteEntriesSpec'''''' = FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeperator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' = 
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
                        LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''''''''' $
                        LT.insert (tnum tProduct) tProduct $
                        LT.insert (tnum tCFICode) tCFICode $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDate) tMaturityDate $
                        LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
                        LT.insert (tnum tIssueDate) tIssueDate $
                        LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
                        LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
                        LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
                        LT.insert (tnum tFactor) tFactor $
                        LT.insert (tnum tCreditRating) tCreditRating $
                        LT.insert (tnum tInstrRegistry) tInstrRegistry $
                        LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
                        LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
                        LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
                        LT.insert (tnum tRedemptionDate) tRedemptionDate $
                        LT.insert (tnum tStrikePrice) tStrikePrice $
                        LT.insert (tnum tOptAttribute) tOptAttribute $
                        LT.insert (tnum tContractMultiplier) tContractMultiplier $
                        LT.insert (tnum tCouponRate) tCouponRate $
                        LT.insert (tnum tSecurityExchange) tSecurityExchange $
                        LT.insert (tnum tIssuer) tIssuer $
                        LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
                        LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
                        LT.insert (tnum tSecurityDesc) tSecurityDesc $
                        LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
                        LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
                        LT.insert (tnum tBidPx) tBidPx $
                        LT.insert (tnum tOfferPx) tOfferPx $
                        LT.insert (tnum tBidSize) tBidSize $
                        LT.insert (tnum tOfferSize) tOfferSize $
                        LT.insert (tnum tValidUntilTime) tValidUntilTime $
                        LT.insert (tnum tBidSpotRate) tBidSpotRate $
                        LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
                        LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
                        LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
                        LT.insert (tnum tMidPx) tMidPx $
                        LT.insert (tnum tBidYield) tBidYield $
                        LT.insert (tnum tMidYield) tMidYield $
                        LT.insert (tnum tOfferYield) tOfferYield $
                        LT.insert (tnum tTransactTime) tTransactTime $
                        LT.insert (tnum tTradingSessionID) tTradingSessionID $
                        LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
                        LT.insert (tnum tFutSettDate) tFutSettDate $
                        LT.insert (tnum tOrdType) tOrdType $
                        LT.insert (tnum tFutSettDate2) tFutSettDate2 $
                        LT.insert (tnum tOrderQty2) tOrderQty2 $
                        LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
                        LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
                        LT.insert (tnum tCurrency) tCurrency                         LT.new
                        where
                           gNoSecurityAltID''''''''' = FIXTag
                              { tName = "NoSecurityAltID"
                              , tnum = tnum tNoSecurityAltID
                              , tparser = gNoSecurityAltIDP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''''''''' }

                           gNoSecurityAltIDP''''''''' = groupP gNoSecurityAltIDSpec'''''''''
                           gNoSecurityAltIDSpec''''''''' = FGSpec
                              { gsLength = tNoSecurityAltID
                              , gsSeperator = tSecurityAltID
                              , gsBody = gNoSecurityAltIDBody''''''''' }
                              where
                              gNoSecurityAltIDBody''''''''' = 
                                 LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                                  LT.new


                  gNoUnderlyingSecurityAltID'''''' = FIXTag
                     { tName = "NoUnderlyingSecurityAltID"
                     , tnum = tnum tNoUnderlyingSecurityAltID
                     , tparser = gNoUnderlyingSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec'''''' }

                  gNoUnderlyingSecurityAltIDP'''''' = groupP gNoUnderlyingSecurityAltIDSpec''''''
                  gNoUnderlyingSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoUnderlyingSecurityAltID
                     , gsSeperator = tUnderlyingSecurityAltID
                     , gsBody = gNoUnderlyingSecurityAltIDBody'''''' }
                     where
                     gNoUnderlyingSecurityAltIDBody'''''' = 
                        LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                         LT.new




mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec
   { msName = "BusinessMessageReject"
   , msType = C.pack "j"
   , msHeader = headerFIX43
   , msBody = mBusinessMessageRejectBody
   , msTrailer = trailerFIX43 }
   where
   mBusinessMessageRejectBody = 
      LT.insert (tnum tRefSeqNum) tRefSeqNum $
      LT.insert (tnum tRefMsgType) tRefMsgType $
      LT.insert (tnum tBusinessRejectRefID) tBusinessRejectRefID $
      LT.insert (tnum tBusinessRejectReason) tBusinessRejectReason $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mBidRequest :: FIXMessageSpec
mBidRequest = FMSpec
   { msName = "BidRequest"
   , msType = C.pack "k"
   , msHeader = headerFIX43
   , msBody = mBidRequestBody
   , msTrailer = trailerFIX43 }
   where
   mBidRequestBody = 
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tBidRequestTransType) tBidRequestTransType $
      LT.insert (tnum tListName) tListName $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tBidType) tBidType $
      LT.insert (tnum tNumTickets) tNumTickets $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSideValue1) tSideValue1 $
      LT.insert (tnum tSideValue2) tSideValue2 $
      LT.insert (tnum tNoBidDescriptors) gNoBidDescriptors''' $
      LT.insert (tnum tNoBidComponents) gNoBidComponents''' $
      LT.insert (tnum tLiquidityIndType) tLiquidityIndType $
      LT.insert (tnum tWtAverageLiquidity) tWtAverageLiquidity $
      LT.insert (tnum tExchangeForPhysical) tExchangeForPhysical $
      LT.insert (tnum tOutMainCntryUIndex) tOutMainCntryUIndex $
      LT.insert (tnum tCrossPercent) tCrossPercent $
      LT.insert (tnum tProgRptReqs) tProgRptReqs $
      LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
      LT.insert (tnum tIncTaxInd) tIncTaxInd $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tNumBidders) tNumBidders $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTradeType) tTradeType $
      LT.insert (tnum tBasisPxType) tBasisPxType $
      LT.insert (tnum tStrikeTime) tStrikeTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoBidComponents''' = FIXTag
            { tName = "NoBidComponents"
            , tnum = tnum tNoBidComponents
            , tparser = gNoBidComponentsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoBidComponentsSpec''' }

         gNoBidComponentsP''' = groupP gNoBidComponentsSpec'''
         gNoBidComponentsSpec''' = FGSpec
            { gsLength = tNoBidComponents
            , gsSeperator = tListID
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' = 
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tNetGrossInd) tNetGrossInd $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tAccount) tAccount                LT.new

         gNoBidDescriptors''' = FIXTag
            { tName = "NoBidDescriptors"
            , tnum = tnum tNoBidDescriptors
            , tparser = gNoBidDescriptorsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoBidDescriptorsSpec''' }

         gNoBidDescriptorsP''' = groupP gNoBidDescriptorsSpec'''
         gNoBidDescriptorsSpec''' = FGSpec
            { gsLength = tNoBidDescriptors
            , gsSeperator = tBidDescriptorType
            , gsBody = gNoBidDescriptorsBody''' }
            where
            gNoBidDescriptorsBody''' = 
               LT.insert (tnum tBidDescriptor) tBidDescriptor $
               LT.insert (tnum tSideValueInd) tSideValueInd $
               LT.insert (tnum tLiquidityValue) tLiquidityValue $
               LT.insert (tnum tLiquidityNumSecurities) tLiquidityNumSecurities $
               LT.insert (tnum tLiquidityPctLow) tLiquidityPctLow $
               LT.insert (tnum tLiquidityPctHigh) tLiquidityPctHigh $
               LT.insert (tnum tEFPTrackingError) tEFPTrackingError $
               LT.insert (tnum tFairValue) tFairValue $
               LT.insert (tnum tOutsideIndexPct) tOutsideIndexPct $
               LT.insert (tnum tValueOfFutures) tValueOfFutures                LT.new



mBidResponse :: FIXMessageSpec
mBidResponse = FMSpec
   { msName = "BidResponse"
   , msType = C.pack "l"
   , msHeader = headerFIX43
   , msBody = mBidResponseBody
   , msTrailer = trailerFIX43 }
   where
   mBidResponseBody = 
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tNoBidComponents) gNoBidComponents'''       LT.new
      where
         gNoBidComponents''' = FIXTag
            { tName = "NoBidComponents"
            , tnum = tnum tNoBidComponents
            , tparser = gNoBidComponentsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoBidComponentsSpec''' }

         gNoBidComponentsP''' = groupP gNoBidComponentsSpec'''
         gNoBidComponentsSpec''' = FGSpec
            { gsLength = tNoBidComponents
            , gsSeperator = tCommission
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' = 
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tListID) tListID $
               LT.insert (tnum tCountry) tCountry $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tPriceType) tPriceType $
               LT.insert (tnum tFairValue) tFairValue $
               LT.insert (tnum tNetGrossInd) tNetGrossInd $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec
   { msName = "ListStrikePrice"
   , msType = C.pack "m"
   , msHeader = headerFIX43
   , msBody = mListStrikePriceBody
   , msTrailer = trailerFIX43 }
   where
   mListStrikePriceBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
      LT.insert (tnum tNoStrikes) gNoStrikes'''       LT.new
      where
         gNoStrikes''' = FIXTag
            { tName = "NoStrikes"
            , tnum = tnum tNoStrikes
            , tparser = gNoStrikesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoStrikesSpec''' }

         gNoStrikesP''' = groupP gNoStrikesSpec'''
         gNoStrikesSpec''' = FGSpec
            { gsLength = tNoStrikes
            , gsSeperator = tSymbol
            , gsBody = gNoStrikesBody''' }
            where
            gNoStrikesBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new




mRegistrationInstructions :: FIXMessageSpec
mRegistrationInstructions = FMSpec
   { msName = "RegistrationInstructions"
   , msType = C.pack "o"
   , msHeader = headerFIX43
   , msBody = mRegistrationInstructionsBody
   , msTrailer = trailerFIX43 }
   where
   mRegistrationInstructionsBody = 
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tRegistTransType) tRegistTransType $
      LT.insert (tnum tRegistRefID) tRegistRefID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tRegistAcctType) tRegistAcctType $
      LT.insert (tnum tTaxAdvantageType) tTaxAdvantageType $
      LT.insert (tnum tOwnershipType) tOwnershipType $
      LT.insert (tnum tNoRegistDtls) gNoRegistDtls''' $
      LT.insert (tnum tNoDistribInsts) gNoDistribInsts'''       LT.new
      where
         gNoDistribInsts''' = FIXTag
            { tName = "NoDistribInsts"
            , tnum = tnum tNoDistribInsts
            , tparser = gNoDistribInstsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoDistribInstsSpec''' }

         gNoDistribInstsP''' = groupP gNoDistribInstsSpec'''
         gNoDistribInstsSpec''' = FGSpec
            { gsLength = tNoDistribInsts
            , gsSeperator = tDistribPaymentMethod
            , gsBody = gNoDistribInstsBody''' }
            where
            gNoDistribInstsBody''' = 
               LT.insert (tnum tDistribPercentage) tDistribPercentage $
               LT.insert (tnum tCashDistribCurr) tCashDistribCurr $
               LT.insert (tnum tCashDistribAgentName) tCashDistribAgentName $
               LT.insert (tnum tCashDistribAgentCode) tCashDistribAgentCode $
               LT.insert (tnum tCashDistribAgentAcctNumber) tCashDistribAgentAcctNumber $
               LT.insert (tnum tCashDistribPayRef) tCashDistribPayRef                LT.new

         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoRegistDtls''' = FIXTag
            { tName = "NoRegistDtls"
            , tnum = tnum tNoRegistDtls
            , tparser = gNoRegistDtlsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRegistDtlsSpec''' }

         gNoRegistDtlsP''' = groupP gNoRegistDtlsSpec'''
         gNoRegistDtlsSpec''' = FGSpec
            { gsLength = tNoRegistDtls
            , gsSeperator = tRegistDetls
            , gsBody = gNoRegistDtlsBody''' }
            where
            gNoRegistDtlsBody''' = 
               LT.insert (tnum tRegistEmail) tRegistEmail $
               LT.insert (tnum tMailingDtls) tMailingDtls $
               LT.insert (tnum tMailingInst) tMailingInst $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tOwnerType) tOwnerType $
               LT.insert (tnum tDateOfBirth) tDateOfBirth $
               LT.insert (tnum tInvestorCountryOfResidence) tInvestorCountryOfResidence                LT.new
               where
                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new




mRegistrationInstructionsResponse :: FIXMessageSpec
mRegistrationInstructionsResponse = FMSpec
   { msName = "RegistrationInstructionsResponse"
   , msType = C.pack "p"
   , msHeader = headerFIX43
   , msBody = mRegistrationInstructionsResponseBody
   , msTrailer = trailerFIX43 }
   where
   mRegistrationInstructionsResponseBody = 
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tRegistTransType) tRegistTransType $
      LT.insert (tnum tRegistRefID) tRegistRefID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tRegistStatus) tRegistStatus $
      LT.insert (tnum tRegistRejReasonCode) tRegistRejReasonCode $
      LT.insert (tnum tRegistRejReasonText) tRegistRejReasonText       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new



mOrderMassCancelRequest :: FIXMessageSpec
mOrderMassCancelRequest = FMSpec
   { msName = "OrderMassCancelRequest"
   , msType = C.pack "q"
   , msHeader = headerFIX43
   , msBody = mOrderMassCancelRequestBody
   , msTrailer = trailerFIX43 }
   where
   mOrderMassCancelRequestBody = 
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tMassCancelRequestType) tMassCancelRequestType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
      LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
      LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
      LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
      LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID''' $
      LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
      LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
      LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
      LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
      LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
      LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
      LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
      LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
      LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
      LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
      LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
      LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
      LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
      LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
      LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
      LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
      LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
      LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
      LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
      LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
      LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
      LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
      LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
      LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
      LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
      LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
      LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
      LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoUnderlyingSecurityAltID''' = FIXTag
            { tName = "NoUnderlyingSecurityAltID"
            , tnum = tnum tNoUnderlyingSecurityAltID
            , tparser = gNoUnderlyingSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec''' }

         gNoUnderlyingSecurityAltIDP''' = groupP gNoUnderlyingSecurityAltIDSpec'''
         gNoUnderlyingSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoUnderlyingSecurityAltID
            , gsSeperator = tUnderlyingSecurityAltID
            , gsBody = gNoUnderlyingSecurityAltIDBody''' }
            where
            gNoUnderlyingSecurityAltIDBody''' = 
               LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                LT.new



mOrderMassCancelReport :: FIXMessageSpec
mOrderMassCancelReport = FMSpec
   { msName = "OrderMassCancelReport"
   , msType = C.pack "r"
   , msHeader = headerFIX43
   , msBody = mOrderMassCancelReportBody
   , msTrailer = trailerFIX43 }
   where
   mOrderMassCancelReportBody = 
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tMassCancelRequestType) tMassCancelRequestType $
      LT.insert (tnum tMassCancelResponse) tMassCancelResponse $
      LT.insert (tnum tMassCancelRejectReason) tMassCancelRejectReason $
      LT.insert (tnum tTotalAffectedOrders) tTotalAffectedOrders $
      LT.insert (tnum tNoAffectedOrders) gNoAffectedOrders''' $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
      LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
      LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
      LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
      LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID''' $
      LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
      LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
      LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
      LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
      LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
      LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
      LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
      LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
      LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
      LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
      LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
      LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
      LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
      LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
      LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
      LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
      LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
      LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
      LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
      LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
      LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
      LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
      LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
      LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
      LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
      LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
      LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
      LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoAffectedOrders''' = FIXTag
            { tName = "NoAffectedOrders"
            , tnum = tnum tNoAffectedOrders
            , tparser = gNoAffectedOrdersP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAffectedOrdersSpec''' }

         gNoAffectedOrdersP''' = groupP gNoAffectedOrdersSpec'''
         gNoAffectedOrdersSpec''' = FGSpec
            { gsLength = tNoAffectedOrders
            , gsSeperator = tOrigClOrdID
            , gsBody = gNoAffectedOrdersBody''' }
            where
            gNoAffectedOrdersBody''' = 
               LT.insert (tnum tAffectedOrderID) tAffectedOrderID $
               LT.insert (tnum tAffectedSecondaryOrderID) tAffectedSecondaryOrderID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoUnderlyingSecurityAltID''' = FIXTag
            { tName = "NoUnderlyingSecurityAltID"
            , tnum = tnum tNoUnderlyingSecurityAltID
            , tparser = gNoUnderlyingSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec''' }

         gNoUnderlyingSecurityAltIDP''' = groupP gNoUnderlyingSecurityAltIDSpec'''
         gNoUnderlyingSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoUnderlyingSecurityAltID
            , gsSeperator = tUnderlyingSecurityAltID
            , gsBody = gNoUnderlyingSecurityAltIDBody''' }
            where
            gNoUnderlyingSecurityAltIDBody''' = 
               LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                LT.new



mNewOrderCross :: FIXMessageSpec
mNewOrderCross = FMSpec
   { msName = "NewOrderCross"
   , msType = C.pack "s"
   , msHeader = headerFIX43
   , msBody = mNewOrderCrossBody
   , msTrailer = trailerFIX43 }
   where
   mNewOrderCrossBody = 
      LT.insert (tnum tCrossID) tCrossID $
      LT.insert (tnum tCrossType) tCrossType $
      LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
      LT.insert (tnum tNoSides) gNoSides''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tNoStipulations) gNoStipulations''' $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tYieldType) tYieldType $
      LT.insert (tnum tYield) tYield $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoSides''' = FIXTag
            { tName = "NoSides"
            , tnum = tnum tNoSides
            , tparser = gNoSidesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSidesSpec''' }

         gNoSidesP''' = groupP gNoSidesSpec'''
         gNoSidesSpec''' = FGSpec
            { gsLength = tNoSides
            , gsSeperator = tSide
            , gsBody = gNoSidesBody''' }
            where
            gNoSidesBody''' = 
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
               LT.insert (tnum tNoPartyIDs) gNoPartyIDs'''''' $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tAccountType) tAccountType $
               LT.insert (tnum tDayBookingInst) tDayBookingInst $
               LT.insert (tnum tBookingUnit) tBookingUnit $
               LT.insert (tnum tPreallocMethod) tPreallocMethod $
               LT.insert (tnum tNoAllocs) gNoAllocs'''''' $
               LT.insert (tnum tQuantityType) tQuantityType $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrderPercent) tOrderPercent $
               LT.insert (tnum tRoundingDirection) tRoundingDirection $
               LT.insert (tnum tRoundingModulus) tRoundingModulus $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tOrderCapacity) tOrderCapacity $
               LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
               LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
               LT.insert (tnum tForexReq) tForexReq $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tPositionEffect) tPositionEffect $
               LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
               LT.insert (tnum tCashMargin) tCashMargin $
               LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
               LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
               LT.insert (tnum tSideComplianceID) tSideComplianceID                LT.new
               where
                  gNoAllocs'''''' = FIXTag
                     { tName = "NoAllocs"
                     , tnum = tnum tNoAllocs
                     , tparser = gNoAllocsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec'''''' }

                  gNoAllocsP'''''' = groupP gNoAllocsSpec''''''
                  gNoAllocsSpec'''''' = FGSpec
                     { gsLength = tNoAllocs
                     , gsSeperator = tAllocAccount
                     , gsBody = gNoAllocsBody'''''' }
                     where
                     gNoAllocsBody'''''' = 
                        LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
                        LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs''''''''' $
                        LT.insert (tnum tAllocQty) tAllocQty                         LT.new
                        where
                           gNoNestedPartyIDs''''''''' = FIXTag
                              { tName = "NoNestedPartyIDs"
                              , tnum = tnum tNoNestedPartyIDs
                              , tparser = gNoNestedPartyIDsP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec''''''''' }

                           gNoNestedPartyIDsP''''''''' = groupP gNoNestedPartyIDsSpec'''''''''
                           gNoNestedPartyIDsSpec''''''''' = FGSpec
                              { gsLength = tNoNestedPartyIDs
                              , gsSeperator = tNestedPartyID
                              , gsBody = gNoNestedPartyIDsBody''''''''' }
                              where
                              gNoNestedPartyIDsBody''''''''' = 
                                 LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                                 LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                                 LT.insert (tnum tNestedPartySubID) tNestedPartySubID                                  LT.new


                  gNoPartyIDs'''''' = FIXTag
                     { tName = "NoPartyIDs"
                     , tnum = tnum tNoPartyIDs
                     , tparser = gNoPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec'''''' }

                  gNoPartyIDsP'''''' = groupP gNoPartyIDsSpec''''''
                  gNoPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoPartyIDs
                     , gsSeperator = tPartyID
                     , gsBody = gNoPartyIDsBody'''''' }
                     where
                     gNoPartyIDsBody'''''' = 
                        LT.insert (tnum tPartyIDSource) tPartyIDSource $
                        LT.insert (tnum tPartyRole) tPartyRole $
                        LT.insert (tnum tPartySubID) tPartySubID                         LT.new


         gNoStipulations''' = FIXTag
            { tName = "NoStipulations"
            , tnum = tnum tNoStipulations
            , tparser = gNoStipulationsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec''' }

         gNoStipulationsP''' = groupP gNoStipulationsSpec'''
         gNoStipulationsSpec''' = FGSpec
            { gsLength = tNoStipulations
            , gsSeperator = tStipulationType
            , gsBody = gNoStipulationsBody''' }
            where
            gNoStipulationsBody''' = 
               LT.insert (tnum tStipulationValue) tStipulationValue                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mCrossOrderCancelRequest :: FIXMessageSpec
mCrossOrderCancelRequest = FMSpec
   { msName = "CrossOrderCancelRequest"
   , msType = C.pack "u"
   , msHeader = headerFIX43
   , msBody = mCrossOrderCancelRequestBody
   , msTrailer = trailerFIX43 }
   where
   mCrossOrderCancelRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tCrossID) tCrossID $
      LT.insert (tnum tOrigCrossID) tOrigCrossID $
      LT.insert (tnum tCrossType) tCrossType $
      LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
      LT.insert (tnum tNoSides) gNoSides''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tTransactTime) tTransactTime       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoSides''' = FIXTag
            { tName = "NoSides"
            , tnum = tnum tNoSides
            , tparser = gNoSidesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSidesSpec''' }

         gNoSidesP''' = groupP gNoSidesSpec'''
         gNoSidesSpec''' = FGSpec
            { gsLength = tNoSides
            , gsSeperator = tSide
            , gsBody = gNoSidesBody''' }
            where
            gNoSidesBody''' = 
               LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
               LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
               LT.insert (tnum tNoPartyIDs) gNoPartyIDs'''''' $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrderPercent) tOrderPercent $
               LT.insert (tnum tRoundingDirection) tRoundingDirection $
               LT.insert (tnum tRoundingModulus) tRoundingModulus $
               LT.insert (tnum tComplianceID) tComplianceID $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new
               where
                  gNoPartyIDs'''''' = FIXTag
                     { tName = "NoPartyIDs"
                     , tnum = tnum tNoPartyIDs
                     , tparser = gNoPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec'''''' }

                  gNoPartyIDsP'''''' = groupP gNoPartyIDsSpec''''''
                  gNoPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoPartyIDs
                     , gsSeperator = tPartyID
                     , gsBody = gNoPartyIDsBody'''''' }
                     where
                     gNoPartyIDsBody'''''' = 
                        LT.insert (tnum tPartyIDSource) tPartyIDSource $
                        LT.insert (tnum tPartyRole) tPartyRole $
                        LT.insert (tnum tPartySubID) tPartySubID                         LT.new




mCrossOrderCancelReplaceRequest :: FIXMessageSpec
mCrossOrderCancelReplaceRequest = FMSpec
   { msName = "CrossOrderCancelReplaceRequest"
   , msType = C.pack "t"
   , msHeader = headerFIX43
   , msBody = mCrossOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX43 }
   where
   mCrossOrderCancelReplaceRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tCrossID) tCrossID $
      LT.insert (tnum tOrigCrossID) tOrigCrossID $
      LT.insert (tnum tCrossType) tCrossType $
      LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
      LT.insert (tnum tNoSides) gNoSides''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tNoStipulations) gNoStipulations''' $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tSpread) tSpread $
      LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
      LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
      LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
      LT.insert (tnum tYieldType) tYieldType $
      LT.insert (tnum tYield) tYield $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoSides''' = FIXTag
            { tName = "NoSides"
            , tnum = tnum tNoSides
            , tparser = gNoSidesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSidesSpec''' }

         gNoSidesP''' = groupP gNoSidesSpec'''
         gNoSidesSpec''' = FGSpec
            { gsLength = tNoSides
            , gsSeperator = tSide
            , gsBody = gNoSidesBody''' }
            where
            gNoSidesBody''' = 
               LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
               LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
               LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
               LT.insert (tnum tNoPartyIDs) gNoPartyIDs'''''' $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tAccountType) tAccountType $
               LT.insert (tnum tDayBookingInst) tDayBookingInst $
               LT.insert (tnum tBookingUnit) tBookingUnit $
               LT.insert (tnum tPreallocMethod) tPreallocMethod $
               LT.insert (tnum tNoAllocs) gNoAllocs'''''' $
               LT.insert (tnum tQuantityType) tQuantityType $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrderPercent) tOrderPercent $
               LT.insert (tnum tRoundingDirection) tRoundingDirection $
               LT.insert (tnum tRoundingModulus) tRoundingModulus $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tOrderCapacity) tOrderCapacity $
               LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
               LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
               LT.insert (tnum tForexReq) tForexReq $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tPositionEffect) tPositionEffect $
               LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
               LT.insert (tnum tCashMargin) tCashMargin $
               LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
               LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
               LT.insert (tnum tSideComplianceID) tSideComplianceID                LT.new
               where
                  gNoAllocs'''''' = FIXTag
                     { tName = "NoAllocs"
                     , tnum = tnum tNoAllocs
                     , tparser = gNoAllocsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec'''''' }

                  gNoAllocsP'''''' = groupP gNoAllocsSpec''''''
                  gNoAllocsSpec'''''' = FGSpec
                     { gsLength = tNoAllocs
                     , gsSeperator = tAllocAccount
                     , gsBody = gNoAllocsBody'''''' }
                     where
                     gNoAllocsBody'''''' = 
                        LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
                        LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs''''''''' $
                        LT.insert (tnum tAllocQty) tAllocQty                         LT.new
                        where
                           gNoNestedPartyIDs''''''''' = FIXTag
                              { tName = "NoNestedPartyIDs"
                              , tnum = tnum tNoNestedPartyIDs
                              , tparser = gNoNestedPartyIDsP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec''''''''' }

                           gNoNestedPartyIDsP''''''''' = groupP gNoNestedPartyIDsSpec'''''''''
                           gNoNestedPartyIDsSpec''''''''' = FGSpec
                              { gsLength = tNoNestedPartyIDs
                              , gsSeperator = tNestedPartyID
                              , gsBody = gNoNestedPartyIDsBody''''''''' }
                              where
                              gNoNestedPartyIDsBody''''''''' = 
                                 LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                                 LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                                 LT.insert (tnum tNestedPartySubID) tNestedPartySubID                                  LT.new


                  gNoPartyIDs'''''' = FIXTag
                     { tName = "NoPartyIDs"
                     , tnum = tnum tNoPartyIDs
                     , tparser = gNoPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec'''''' }

                  gNoPartyIDsP'''''' = groupP gNoPartyIDsSpec''''''
                  gNoPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoPartyIDs
                     , gsSeperator = tPartyID
                     , gsBody = gNoPartyIDsBody'''''' }
                     where
                     gNoPartyIDsBody'''''' = 
                        LT.insert (tnum tPartyIDSource) tPartyIDSource $
                        LT.insert (tnum tPartyRole) tPartyRole $
                        LT.insert (tnum tPartySubID) tPartySubID                         LT.new


         gNoStipulations''' = FIXTag
            { tName = "NoStipulations"
            , tnum = tnum tNoStipulations
            , tparser = gNoStipulationsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec''' }

         gNoStipulationsP''' = groupP gNoStipulationsSpec'''
         gNoStipulationsSpec''' = FGSpec
            { gsLength = tNoStipulations
            , gsSeperator = tStipulationType
            , gsBody = gNoStipulationsBody''' }
            where
            gNoStipulationsBody''' = 
               LT.insert (tnum tStipulationValue) tStipulationValue                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mSecurityTypeRequest :: FIXMessageSpec
mSecurityTypeRequest = FMSpec
   { msName = "SecurityTypeRequest"
   , msType = C.pack "v"
   , msHeader = headerFIX43
   , msBody = mSecurityTypeRequestBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityTypeRequestBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID       LT.new


mSecurityTypes :: FIXMessageSpec
mSecurityTypes = FMSpec
   { msName = "SecurityTypes"
   , msType = C.pack "w"
   , msHeader = headerFIX43
   , msBody = mSecurityTypesBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityTypesBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
      LT.insert (tnum tTotalNumSecurityTypes) tTotalNumSecurityTypes $
      LT.insert (tnum tNoSecurityTypes) gNoSecurityTypes''' $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoSecurityTypes''' = FIXTag
            { tName = "NoSecurityTypes"
            , tnum = tnum tNoSecurityTypes
            , tparser = gNoSecurityTypesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityTypesSpec''' }

         gNoSecurityTypesP''' = groupP gNoSecurityTypesSpec'''
         gNoSecurityTypesSpec''' = FGSpec
            { gsLength = tNoSecurityTypes
            , gsSeperator = tSecurityType
            , gsBody = gNoSecurityTypesBody''' }
            where
            gNoSecurityTypesBody''' = 
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode                LT.new



mSecurityListRequest :: FIXMessageSpec
mSecurityListRequest = FMSpec
   { msName = "SecurityListRequest"
   , msType = C.pack "x"
   , msHeader = headerFIX43
   , msBody = mSecurityListRequestBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityListRequestBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityListRequestType) tSecurityListRequestType $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mSecurityList :: FIXMessageSpec
mSecurityList = FMSpec
   { msName = "SecurityList"
   , msType = C.pack "y"
   , msHeader = headerFIX43
   , msBody = mSecurityListBody
   , msTrailer = trailerFIX43 }
   where
   mSecurityListBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tNoLegs) gNoLegs'''''' $
               LT.insert (tnum tRoundLot) tRoundLot $
               LT.insert (tnum tMinTradeVol) tMinTradeVol $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new
               where
                  gNoLegs'''''' = FIXTag
                     { tName = "NoLegs"
                     , tnum = tnum tNoLegs
                     , tparser = gNoLegsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec'''''' }

                  gNoLegsP'''''' = groupP gNoLegsSpec''''''
                  gNoLegsSpec'''''' = FGSpec
                     { gsLength = tNoLegs
                     , gsSeperator = tLegSymbol
                     , gsBody = gNoLegsBody'''''' }
                     where
                     gNoLegsBody'''''' = 
                        LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
                        LT.insert (tnum tLegSecurityID) tLegSecurityID $
                        LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
                        LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID''''''''' $
                        LT.insert (tnum tLegProduct) tLegProduct $
                        LT.insert (tnum tLegCFICode) tLegCFICode $
                        LT.insert (tnum tLegSecurityType) tLegSecurityType $
                        LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
                        LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
                        LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
                        LT.insert (tnum tLegIssueDate) tLegIssueDate $
                        LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
                        LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
                        LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
                        LT.insert (tnum tLegFactor) tLegFactor $
                        LT.insert (tnum tLegCreditRating) tLegCreditRating $
                        LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
                        LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
                        LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
                        LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
                        LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
                        LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
                        LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
                        LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
                        LT.insert (tnum tLegCouponRate) tLegCouponRate $
                        LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
                        LT.insert (tnum tLegIssuer) tLegIssuer $
                        LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
                        LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
                        LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
                        LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
                        LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
                        LT.insert (tnum tLegRatioQty) tLegRatioQty $
                        LT.insert (tnum tLegSide) tLegSide $
                        LT.insert (tnum tLegCurrency) tLegCurrency                         LT.new
                        where
                           gNoLegSecurityAltID''''''''' = FIXTag
                              { tName = "NoLegSecurityAltID"
                              , tnum = tnum tNoLegSecurityAltID
                              , tparser = gNoLegSecurityAltIDP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec''''''''' }

                           gNoLegSecurityAltIDP''''''''' = groupP gNoLegSecurityAltIDSpec'''''''''
                           gNoLegSecurityAltIDSpec''''''''' = FGSpec
                              { gsLength = tNoLegSecurityAltID
                              , gsSeperator = tLegSecurityAltID
                              , gsBody = gNoLegSecurityAltIDBody''''''''' }
                              where
                              gNoLegSecurityAltIDBody''''''''' = 
                                 LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                                  LT.new


                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new




mDerivativeSecurityListRequest :: FIXMessageSpec
mDerivativeSecurityListRequest = FMSpec
   { msName = "DerivativeSecurityListRequest"
   , msType = C.pack "z"
   , msHeader = headerFIX43
   , msBody = mDerivativeSecurityListRequestBody
   , msTrailer = trailerFIX43 }
   where
   mDerivativeSecurityListRequestBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityListRequestType) tSecurityListRequestType $
      LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
      LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
      LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
      LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
      LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID''' $
      LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
      LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
      LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
      LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
      LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
      LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
      LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
      LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
      LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
      LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
      LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
      LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
      LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
      LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
      LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
      LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
      LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
      LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
      LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
      LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
      LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
      LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
      LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
      LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
      LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
      LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
      LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
      LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoUnderlyingSecurityAltID''' = FIXTag
            { tName = "NoUnderlyingSecurityAltID"
            , tnum = tnum tNoUnderlyingSecurityAltID
            , tparser = gNoUnderlyingSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec''' }

         gNoUnderlyingSecurityAltIDP''' = groupP gNoUnderlyingSecurityAltIDSpec'''
         gNoUnderlyingSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoUnderlyingSecurityAltID
            , gsSeperator = tUnderlyingSecurityAltID
            , gsBody = gNoUnderlyingSecurityAltIDBody''' }
            where
            gNoUnderlyingSecurityAltIDBody''' = 
               LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                LT.new



mDerivativeSecurityList :: FIXMessageSpec
mDerivativeSecurityList = FMSpec
   { msName = "DerivativeSecurityList"
   , msType = C.pack "AA"
   , msHeader = headerFIX43
   , msBody = mDerivativeSecurityListBody
   , msTrailer = trailerFIX43 }
   where
   mDerivativeSecurityListBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
      LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
      LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
      LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
      LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
      LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID''' $
      LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
      LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
      LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
      LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
      LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
      LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
      LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
      LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
      LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
      LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
      LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
      LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
      LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
      LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
      LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
      LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
      LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
      LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
      LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
      LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
      LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
      LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
      LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
      LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
      LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
      LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
      LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
      LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tNoLegs) gNoLegs'''''' $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new
               where
                  gNoLegs'''''' = FIXTag
                     { tName = "NoLegs"
                     , tnum = tnum tNoLegs
                     , tparser = gNoLegsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec'''''' }

                  gNoLegsP'''''' = groupP gNoLegsSpec''''''
                  gNoLegsSpec'''''' = FGSpec
                     { gsLength = tNoLegs
                     , gsSeperator = tLegSymbol
                     , gsBody = gNoLegsBody'''''' }
                     where
                     gNoLegsBody'''''' = 
                        LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
                        LT.insert (tnum tLegSecurityID) tLegSecurityID $
                        LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
                        LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID''''''''' $
                        LT.insert (tnum tLegProduct) tLegProduct $
                        LT.insert (tnum tLegCFICode) tLegCFICode $
                        LT.insert (tnum tLegSecurityType) tLegSecurityType $
                        LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
                        LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
                        LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
                        LT.insert (tnum tLegIssueDate) tLegIssueDate $
                        LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
                        LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
                        LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
                        LT.insert (tnum tLegFactor) tLegFactor $
                        LT.insert (tnum tLegCreditRating) tLegCreditRating $
                        LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
                        LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
                        LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
                        LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
                        LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
                        LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
                        LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
                        LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
                        LT.insert (tnum tLegCouponRate) tLegCouponRate $
                        LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
                        LT.insert (tnum tLegIssuer) tLegIssuer $
                        LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
                        LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
                        LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
                        LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
                        LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
                        LT.insert (tnum tLegRatioQty) tLegRatioQty $
                        LT.insert (tnum tLegSide) tLegSide $
                        LT.insert (tnum tLegCurrency) tLegCurrency                         LT.new
                        where
                           gNoLegSecurityAltID''''''''' = FIXTag
                              { tName = "NoLegSecurityAltID"
                              , tnum = tnum tNoLegSecurityAltID
                              , tparser = gNoLegSecurityAltIDP'''''''''
                              , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec''''''''' }

                           gNoLegSecurityAltIDP''''''''' = groupP gNoLegSecurityAltIDSpec'''''''''
                           gNoLegSecurityAltIDSpec''''''''' = FGSpec
                              { gsLength = tNoLegSecurityAltID
                              , gsSeperator = tLegSecurityAltID
                              , gsBody = gNoLegSecurityAltIDBody''''''''' }
                              where
                              gNoLegSecurityAltIDBody''''''''' = 
                                 LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                                  LT.new


                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new


         gNoUnderlyingSecurityAltID''' = FIXTag
            { tName = "NoUnderlyingSecurityAltID"
            , tnum = tnum tNoUnderlyingSecurityAltID
            , tparser = gNoUnderlyingSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec''' }

         gNoUnderlyingSecurityAltIDP''' = groupP gNoUnderlyingSecurityAltIDSpec'''
         gNoUnderlyingSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoUnderlyingSecurityAltID
            , gsSeperator = tUnderlyingSecurityAltID
            , gsBody = gNoUnderlyingSecurityAltIDBody''' }
            where
            gNoUnderlyingSecurityAltIDBody''' = 
               LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                LT.new



mNewOrderMultileg :: FIXMessageSpec
mNewOrderMultileg = FMSpec
   { msName = "NewOrderMultileg"
   , msType = C.pack "AB"
   , msHeader = headerFIX43
   , msBody = mNewOrderMultilegBody
   , msTrailer = trailerFIX43 }
   where
   mNewOrderMultilegBody = 
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDayBookingInst) tDayBookingInst $
      LT.insert (tnum tBookingUnit) tBookingUnit $
      LT.insert (tnum tPreallocMethod) tPreallocMethod $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tCashMargin) tCashMargin $
      LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tNoLegs) gNoLegs''' $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCommCurrency) tCommCurrency $
      LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
      LT.insert (tnum tOrderCapacity) tOrderCapacity $
      LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tMultiLegRptTypeReq) tMultiLegRptTypeReq $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
               LT.insert (tnum tAllocQty) tAllocQty                LT.new

         gNoLegs''' = FIXTag
            { tName = "NoLegs"
            , tnum = tnum tNoLegs
            , tparser = gNoLegsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec''' }

         gNoLegsP''' = groupP gNoLegsSpec'''
         gNoLegsSpec''' = FGSpec
            { gsLength = tNoLegs
            , gsSeperator = tLegSymbol
            , gsBody = gNoLegsBody''' }
            where
            gNoLegsBody''' = 
               LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
               LT.insert (tnum tLegSecurityID) tLegSecurityID $
               LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
               LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID'''''' $
               LT.insert (tnum tLegProduct) tLegProduct $
               LT.insert (tnum tLegCFICode) tLegCFICode $
               LT.insert (tnum tLegSecurityType) tLegSecurityType $
               LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
               LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
               LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
               LT.insert (tnum tLegIssueDate) tLegIssueDate $
               LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
               LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
               LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
               LT.insert (tnum tLegFactor) tLegFactor $
               LT.insert (tnum tLegCreditRating) tLegCreditRating $
               LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
               LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
               LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
               LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
               LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
               LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
               LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
               LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
               LT.insert (tnum tLegCouponRate) tLegCouponRate $
               LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
               LT.insert (tnum tLegIssuer) tLegIssuer $
               LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
               LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
               LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
               LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
               LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
               LT.insert (tnum tLegRatioQty) tLegRatioQty $
               LT.insert (tnum tLegSide) tLegSide $
               LT.insert (tnum tLegPositionEffect) tLegPositionEffect $
               LT.insert (tnum tLegCoveredOrUncovered) tLegCoveredOrUncovered $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tLegRefID) tLegRefID $
               LT.insert (tnum tLegPrice) tLegPrice $
               LT.insert (tnum tLegSettlmntTyp) tLegSettlmntTyp $
               LT.insert (tnum tLegFutSettDate) tLegFutSettDate                LT.new
               where
                  gNoLegSecurityAltID'''''' = FIXTag
                     { tName = "NoLegSecurityAltID"
                     , tnum = tnum tNoLegSecurityAltID
                     , tparser = gNoLegSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec'''''' }

                  gNoLegSecurityAltIDP'''''' = groupP gNoLegSecurityAltIDSpec''''''
                  gNoLegSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoLegSecurityAltID
                     , gsSeperator = tLegSecurityAltID
                     , gsBody = gNoLegSecurityAltIDBody'''''' }
                     where
                     gNoLegSecurityAltIDBody'''''' = 
                        LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                         LT.new

                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mMultilegOrderCancelReplaceRequest :: FIXMessageSpec
mMultilegOrderCancelReplaceRequest = FMSpec
   { msName = "MultilegOrderCancelReplaceRequest"
   , msType = C.pack "AC"
   , msHeader = headerFIX43
   , msBody = mMultilegOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX43 }
   where
   mMultilegOrderCancelReplaceRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
      LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
      LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tDayBookingInst) tDayBookingInst $
      LT.insert (tnum tBookingUnit) tBookingUnit $
      LT.insert (tnum tPreallocMethod) tPreallocMethod $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tCashMargin) tCashMargin $
      LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tProcessCode) tProcessCode $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tPrevClosePx) tPrevClosePx $
      LT.insert (tnum tNoLegs) gNoLegs''' $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tQuantityType) tQuantityType $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPriceType) tPriceType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tTimeInForce) tTimeInForce $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tExpireDate) tExpireDate $
      LT.insert (tnum tExpireTime) tExpireTime $
      LT.insert (tnum tGTBookingInst) tGTBookingInst $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCommCurrency) tCommCurrency $
      LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
      LT.insert (tnum tOrderCapacity) tOrderCapacity $
      LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tPositionEffect) tPositionEffect $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tCancellationRights) tCancellationRights $
      LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
      LT.insert (tnum tRegistID) tRegistID $
      LT.insert (tnum tDesignation) tDesignation $
      LT.insert (tnum tMultiLegRptTypeReq) tMultiLegRptTypeReq $
      LT.insert (tnum tNetMoney) tNetMoney       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoAllocsSpec''' }

         gNoAllocsP''' = groupP gNoAllocsSpec'''
         gNoAllocsSpec''' = FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
               LT.insert (tnum tAllocQty) tAllocQty                LT.new

         gNoLegs''' = FIXTag
            { tName = "NoLegs"
            , tnum = tnum tNoLegs
            , tparser = gNoLegsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoLegsSpec''' }

         gNoLegsP''' = groupP gNoLegsSpec'''
         gNoLegsSpec''' = FGSpec
            { gsLength = tNoLegs
            , gsSeperator = tLegSymbol
            , gsBody = gNoLegsBody''' }
            where
            gNoLegsBody''' = 
               LT.insert (tnum tLegSymbolSfx) tLegSymbolSfx $
               LT.insert (tnum tLegSecurityID) tLegSecurityID $
               LT.insert (tnum tLegSecurityIDSource) tLegSecurityIDSource $
               LT.insert (tnum tNoLegSecurityAltID) gNoLegSecurityAltID'''''' $
               LT.insert (tnum tLegProduct) tLegProduct $
               LT.insert (tnum tLegCFICode) tLegCFICode $
               LT.insert (tnum tLegSecurityType) tLegSecurityType $
               LT.insert (tnum tLegMaturityMonthYear) tLegMaturityMonthYear $
               LT.insert (tnum tLegMaturityDate) tLegMaturityDate $
               LT.insert (tnum tLegCouponPaymentDate) tLegCouponPaymentDate $
               LT.insert (tnum tLegIssueDate) tLegIssueDate $
               LT.insert (tnum tLegRepoCollateralSecurityType) tLegRepoCollateralSecurityType $
               LT.insert (tnum tLegRepurchaseTerm) tLegRepurchaseTerm $
               LT.insert (tnum tLegRepurchaseRate) tLegRepurchaseRate $
               LT.insert (tnum tLegFactor) tLegFactor $
               LT.insert (tnum tLegCreditRating) tLegCreditRating $
               LT.insert (tnum tLegInstrRegistry) tLegInstrRegistry $
               LT.insert (tnum tLegCountryOfIssue) tLegCountryOfIssue $
               LT.insert (tnum tLegStateOrProvinceOfIssue) tLegStateOrProvinceOfIssue $
               LT.insert (tnum tLegLocaleOfIssue) tLegLocaleOfIssue $
               LT.insert (tnum tLegRedemptionDate) tLegRedemptionDate $
               LT.insert (tnum tLegStrikePrice) tLegStrikePrice $
               LT.insert (tnum tLegOptAttribute) tLegOptAttribute $
               LT.insert (tnum tLegContractMultiplier) tLegContractMultiplier $
               LT.insert (tnum tLegCouponRate) tLegCouponRate $
               LT.insert (tnum tLegSecurityExchange) tLegSecurityExchange $
               LT.insert (tnum tLegIssuer) tLegIssuer $
               LT.insert (tnum tEncodedLegIssuerLen) tEncodedLegIssuerLen $
               LT.insert (tnum tEncodedLegIssuer) tEncodedLegIssuer $
               LT.insert (tnum tLegSecurityDesc) tLegSecurityDesc $
               LT.insert (tnum tEncodedLegSecurityDescLen) tEncodedLegSecurityDescLen $
               LT.insert (tnum tEncodedLegSecurityDesc) tEncodedLegSecurityDesc $
               LT.insert (tnum tLegRatioQty) tLegRatioQty $
               LT.insert (tnum tLegSide) tLegSide $
               LT.insert (tnum tLegPositionEffect) tLegPositionEffect $
               LT.insert (tnum tLegCoveredOrUncovered) tLegCoveredOrUncovered $
               LT.insert (tnum tNoNestedPartyIDs) gNoNestedPartyIDs'''''' $
               LT.insert (tnum tLegRefID) tLegRefID $
               LT.insert (tnum tLegPrice) tLegPrice $
               LT.insert (tnum tLegSettlmntTyp) tLegSettlmntTyp $
               LT.insert (tnum tLegFutSettDate) tLegFutSettDate                LT.new
               where
                  gNoLegSecurityAltID'''''' = FIXTag
                     { tName = "NoLegSecurityAltID"
                     , tnum = tnum tNoLegSecurityAltID
                     , tparser = gNoLegSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoLegSecurityAltIDSpec'''''' }

                  gNoLegSecurityAltIDP'''''' = groupP gNoLegSecurityAltIDSpec''''''
                  gNoLegSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoLegSecurityAltID
                     , gsSeperator = tLegSecurityAltID
                     , gsBody = gNoLegSecurityAltIDBody'''''' }
                     where
                     gNoLegSecurityAltIDBody'''''' = 
                        LT.insert (tnum tLegSecurityAltIDSource) tLegSecurityAltIDSource                         LT.new

                  gNoNestedPartyIDs'''''' = FIXTag
                     { tName = "NoNestedPartyIDs"
                     , tnum = tnum tNoNestedPartyIDs
                     , tparser = gNoNestedPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoNestedPartyIDsSpec'''''' }

                  gNoNestedPartyIDsP'''''' = groupP gNoNestedPartyIDsSpec''''''
                  gNoNestedPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoNestedPartyIDs
                     , gsSeperator = tNestedPartyID
                     , gsBody = gNoNestedPartyIDsBody'''''' }
                     where
                     gNoNestedPartyIDsBody'''''' = 
                        LT.insert (tnum tNestedPartyIDSource) tNestedPartyIDSource $
                        LT.insert (tnum tNestedPartyRole) tNestedPartyRole $
                        LT.insert (tnum tNestedPartySubID) tNestedPartySubID                         LT.new


         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoTradingSessionsSpec''' }

         gNoTradingSessionsP''' = groupP gNoTradingSessionsSpec'''
         gNoTradingSessionsSpec''' = FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new



mTradeCaptureReportRequest :: FIXMessageSpec
mTradeCaptureReportRequest = FMSpec
   { msName = "TradeCaptureReportRequest"
   , msType = C.pack "AD"
   , msHeader = headerFIX43
   , msBody = mTradeCaptureReportRequestBody
   , msTrailer = trailerFIX43 }
   where
   mTradeCaptureReportRequestBody = 
      LT.insert (tnum tTradeRequestID) tTradeRequestID $
      LT.insert (tnum tTradeRequestType) tTradeRequestType $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tMatchStatus) tMatchStatus $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tNoDates) gNoDates''' $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tTradeInputSource) tTradeInputSource $
      LT.insert (tnum tTradeInputDevice) tTradeInputDevice       LT.new
      where
         gNoDates''' = FIXTag
            { tName = "NoDates"
            , tnum = tnum tNoDates
            , tparser = gNoDatesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoDatesSpec''' }

         gNoDatesP''' = groupP gNoDatesSpec'''
         gNoDatesSpec''' = FGSpec
            { gsLength = tNoDates
            , gsSeperator = tTradeDate
            , gsBody = gNoDatesBody''' }
            where
            gNoDatesBody''' = 
               LT.insert (tnum tTransactTime) tTransactTime                LT.new

         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



mTradeCaptureReport :: FIXMessageSpec
mTradeCaptureReport = FMSpec
   { msName = "TradeCaptureReport"
   , msType = C.pack "AE"
   , msHeader = headerFIX43
   , msBody = mTradeCaptureReportBody
   , msTrailer = trailerFIX43 }
   where
   mTradeCaptureReportBody = 
      LT.insert (tnum tTradeReportID) tTradeReportID $
      LT.insert (tnum tTradeReportTransType) tTradeReportTransType $
      LT.insert (tnum tTradeRequestID) tTradeRequestID $
      LT.insert (tnum tExecType) tExecType $
      LT.insert (tnum tTradeReportRefID) tTradeReportRefID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
      LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
      LT.insert (tnum tPreviouslyReported) tPreviouslyReported $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrderPercent) tOrderPercent $
      LT.insert (tnum tRoundingDirection) tRoundingDirection $
      LT.insert (tnum tRoundingModulus) tRoundingModulus $
      LT.insert (tnum tLastQty) tLastQty $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tLastSpotRate) tLastSpotRate $
      LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tMatchStatus) tMatchStatus $
      LT.insert (tnum tMatchType) tMatchType $
      LT.insert (tnum tNoSides) gNoSides'''       LT.new
      where
         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoSides''' = FIXTag
            { tName = "NoSides"
            , tnum = tnum tNoSides
            , tparser = gNoSidesP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSidesSpec''' }

         gNoSidesP''' = groupP gNoSidesSpec'''
         gNoSidesSpec''' = FGSpec
            { gsLength = tNoSides
            , gsSeperator = tSide
            , gsBody = gNoSidesBody''' }
            where
            gNoSidesBody''' = 
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
               LT.insert (tnum tClOrdID) tClOrdID $
               LT.insert (tnum tNoPartyIDs) gNoPartyIDs'''''' $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tAccountType) tAccountType $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tOddLot) tOddLot $
               LT.insert (tnum tNoClearingInstructions) gNoClearingInstructions'''''' $
               LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
               LT.insert (tnum tTradeInputSource) tTradeInputSource $
               LT.insert (tnum tTradeInputDevice) tTradeInputDevice $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tComplianceID) tComplianceID $
               LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
               LT.insert (tnum tOrderCapacity) tOrderCapacity $
               LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
               LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
               LT.insert (tnum tTransBkdTime) tTransBkdTime $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
               LT.insert (tnum tCommCurrency) tCommCurrency $
               LT.insert (tnum tFundRenewWaiv) tFundRenewWaiv $
               LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
               LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
               LT.insert (tnum tExDate) tExDate $
               LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
               LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
               LT.insert (tnum tConcession) tConcession $
               LT.insert (tnum tTotalTakedown) tTotalTakedown $
               LT.insert (tnum tNetMoney) tNetMoney $
               LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
               LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
               LT.insert (tnum tPositionEffect) tPositionEffect $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
               LT.insert (tnum tNoContAmts) gNoContAmts'''''' $
               LT.insert (tnum tNoMiscFees) gNoMiscFees''''''                LT.new
               where
                  gNoClearingInstructions'''''' = FIXTag
                     { tName = "NoClearingInstructions"
                     , tnum = tnum tNoClearingInstructions
                     , tparser = gNoClearingInstructionsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoClearingInstructionsSpec'''''' }

                  gNoClearingInstructionsP'''''' = groupP gNoClearingInstructionsSpec''''''
                  gNoClearingInstructionsSpec'''''' = FGSpec
                     { gsLength = tNoClearingInstructions
                     , gsSeperator = tClearingInstruction
                     , gsBody = gNoClearingInstructionsBody'''''' }
                     where
                     gNoClearingInstructionsBody'''''' = 
                        LT.new

                  gNoContAmts'''''' = FIXTag
                     { tName = "NoContAmts"
                     , tnum = tnum tNoContAmts
                     , tparser = gNoContAmtsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoContAmtsSpec'''''' }

                  gNoContAmtsP'''''' = groupP gNoContAmtsSpec''''''
                  gNoContAmtsSpec'''''' = FGSpec
                     { gsLength = tNoContAmts
                     , gsSeperator = tContAmtType
                     , gsBody = gNoContAmtsBody'''''' }
                     where
                     gNoContAmtsBody'''''' = 
                        LT.insert (tnum tContAmtValue) tContAmtValue $
                        LT.insert (tnum tContAmtCurr) tContAmtCurr                         LT.new

                  gNoMiscFees'''''' = FIXTag
                     { tName = "NoMiscFees"
                     , tnum = tnum tNoMiscFees
                     , tparser = gNoMiscFeesP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoMiscFeesSpec'''''' }

                  gNoMiscFeesP'''''' = groupP gNoMiscFeesSpec''''''
                  gNoMiscFeesSpec'''''' = FGSpec
                     { gsLength = tNoMiscFees
                     , gsSeperator = tMiscFeeAmt
                     , gsBody = gNoMiscFeesBody'''''' }
                     where
                     gNoMiscFeesBody'''''' = 
                        LT.insert (tnum tMiscFeeCurr) tMiscFeeCurr $
                        LT.insert (tnum tMiscFeeType) tMiscFeeType                         LT.new

                  gNoPartyIDs'''''' = FIXTag
                     { tName = "NoPartyIDs"
                     , tnum = tnum tNoPartyIDs
                     , tparser = gNoPartyIDsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec'''''' }

                  gNoPartyIDsP'''''' = groupP gNoPartyIDsSpec''''''
                  gNoPartyIDsSpec'''''' = FGSpec
                     { gsLength = tNoPartyIDs
                     , gsSeperator = tPartyID
                     , gsBody = gNoPartyIDsBody'''''' }
                     where
                     gNoPartyIDsBody'''''' = 
                        LT.insert (tnum tPartyIDSource) tPartyIDSource $
                        LT.insert (tnum tPartyRole) tPartyRole $
                        LT.insert (tnum tPartySubID) tPartySubID                         LT.new




mOrderMassStatusRequest :: FIXMessageSpec
mOrderMassStatusRequest = FMSpec
   { msName = "OrderMassStatusRequest"
   , msType = C.pack "AF"
   , msHeader = headerFIX43
   , msBody = mOrderMassStatusRequestBody
   , msTrailer = trailerFIX43 }
   where
   mOrderMassStatusRequestBody = 
      LT.insert (tnum tMassStatusReqID) tMassStatusReqID $
      LT.insert (tnum tMassStatusReqType) tMassStatusReqType $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
      LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
      LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
      LT.insert (tnum tUnderlyingSecurityIDSource) tUnderlyingSecurityIDSource $
      LT.insert (tnum tNoUnderlyingSecurityAltID) gNoUnderlyingSecurityAltID''' $
      LT.insert (tnum tUnderlyingProduct) tUnderlyingProduct $
      LT.insert (tnum tUnderlyingCFICode) tUnderlyingCFICode $
      LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
      LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
      LT.insert (tnum tUnderlyingMaturityDate) tUnderlyingMaturityDate $
      LT.insert (tnum tUnderlyingCouponPaymentDate) tUnderlyingCouponPaymentDate $
      LT.insert (tnum tUnderlyingIssueDate) tUnderlyingIssueDate $
      LT.insert (tnum tUnderlyingRepoCollateralSecurityType) tUnderlyingRepoCollateralSecurityType $
      LT.insert (tnum tUnderlyingRepurchaseTerm) tUnderlyingRepurchaseTerm $
      LT.insert (tnum tUnderlyingRepurchaseRate) tUnderlyingRepurchaseRate $
      LT.insert (tnum tUnderlyingFactor) tUnderlyingFactor $
      LT.insert (tnum tUnderlyingCreditRating) tUnderlyingCreditRating $
      LT.insert (tnum tUnderlyingInstrRegistry) tUnderlyingInstrRegistry $
      LT.insert (tnum tUnderlyingCountryOfIssue) tUnderlyingCountryOfIssue $
      LT.insert (tnum tUnderlyingStateOrProvinceOfIssue) tUnderlyingStateOrProvinceOfIssue $
      LT.insert (tnum tUnderlyingLocaleOfIssue) tUnderlyingLocaleOfIssue $
      LT.insert (tnum tUnderlyingRedemptionDate) tUnderlyingRedemptionDate $
      LT.insert (tnum tUnderlyingStrikePrice) tUnderlyingStrikePrice $
      LT.insert (tnum tUnderlyingOptAttribute) tUnderlyingOptAttribute $
      LT.insert (tnum tUnderlyingContractMultiplier) tUnderlyingContractMultiplier $
      LT.insert (tnum tUnderlyingCouponRate) tUnderlyingCouponRate $
      LT.insert (tnum tUnderlyingSecurityExchange) tUnderlyingSecurityExchange $
      LT.insert (tnum tUnderlyingIssuer) tUnderlyingIssuer $
      LT.insert (tnum tEncodedUnderlyingIssuerLen) tEncodedUnderlyingIssuerLen $
      LT.insert (tnum tEncodedUnderlyingIssuer) tEncodedUnderlyingIssuer $
      LT.insert (tnum tUnderlyingSecurityDesc) tUnderlyingSecurityDesc $
      LT.insert (tnum tEncodedUnderlyingSecurityDescLen) tEncodedUnderlyingSecurityDescLen $
      LT.insert (tnum tEncodedUnderlyingSecurityDesc) tEncodedUnderlyingSecurityDesc $
      LT.insert (tnum tSide) tSide       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new

         gNoUnderlyingSecurityAltID''' = FIXTag
            { tName = "NoUnderlyingSecurityAltID"
            , tnum = tnum tNoUnderlyingSecurityAltID
            , tparser = gNoUnderlyingSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoUnderlyingSecurityAltIDSpec''' }

         gNoUnderlyingSecurityAltIDP''' = groupP gNoUnderlyingSecurityAltIDSpec'''
         gNoUnderlyingSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoUnderlyingSecurityAltID
            , gsSeperator = tUnderlyingSecurityAltID
            , gsBody = gNoUnderlyingSecurityAltIDBody''' }
            where
            gNoUnderlyingSecurityAltIDBody''' = 
               LT.insert (tnum tUnderlyingSecurityAltIDSource) tUnderlyingSecurityAltIDSource                LT.new



mQuoteRequestReject :: FIXMessageSpec
mQuoteRequestReject = FMSpec
   { msName = "QuoteRequestReject"
   , msType = C.pack "AG"
   , msHeader = headerFIX43
   , msBody = mQuoteRequestRejectBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteRequestRejectBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tRFQReqID) tRFQReqID $
      LT.insert (tnum tQuoteRequestRejectReason) tQuoteRequestRejectReason $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tQuoteRequestType) tQuoteRequestType $
               LT.insert (tnum tQuoteType) tQuoteType $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
               LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
               LT.insert (tnum tNoStipulations) gNoStipulations'''''' $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tQuantityType) tQuantityType $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tSpread) tSpread $
               LT.insert (tnum tBenchmarkCurveCurrency) tBenchmarkCurveCurrency $
               LT.insert (tnum tBenchmarkCurveName) tBenchmarkCurveName $
               LT.insert (tnum tBenchmarkCurvePoint) tBenchmarkCurvePoint $
               LT.insert (tnum tPriceType) tPriceType $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tPrice2) tPrice2 $
               LT.insert (tnum tYieldType) tYieldType $
               LT.insert (tnum tYield) tYield                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new

                  gNoStipulations'''''' = FIXTag
                     { tName = "NoStipulations"
                     , tnum = tnum tNoStipulations
                     , tparser = gNoStipulationsP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoStipulationsSpec'''''' }

                  gNoStipulationsP'''''' = groupP gNoStipulationsSpec''''''
                  gNoStipulationsSpec'''''' = FGSpec
                     { gsLength = tNoStipulations
                     , gsSeperator = tStipulationType
                     , gsBody = gNoStipulationsBody'''''' }
                     where
                     gNoStipulationsBody'''''' = 
                        LT.insert (tnum tStipulationValue) tStipulationValue                         LT.new




mRFQRequest :: FIXMessageSpec
mRFQRequest = FMSpec
   { msName = "RFQRequest"
   , msType = C.pack "AH"
   , msHeader = headerFIX43
   , msBody = mRFQRequestBody
   , msTrailer = trailerFIX43 }
   where
   mRFQRequestBody = 
      LT.insert (tnum tRFQReqID) tRFQReqID $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym''' $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP'''
            , arbitraryValue = arbibtraryFIXGroup gNoRelatedSymSpec''' }

         gNoRelatedSymP''' = groupP gNoRelatedSymSpec'''
         gNoRelatedSymSpec''' = FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
               LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID'''''' $
               LT.insert (tnum tProduct) tProduct $
               LT.insert (tnum tCFICode) tCFICode $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDate) tMaturityDate $
               LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
               LT.insert (tnum tIssueDate) tIssueDate $
               LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
               LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
               LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
               LT.insert (tnum tFactor) tFactor $
               LT.insert (tnum tCreditRating) tCreditRating $
               LT.insert (tnum tInstrRegistry) tInstrRegistry $
               LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
               LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
               LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
               LT.insert (tnum tRedemptionDate) tRedemptionDate $
               LT.insert (tnum tStrikePrice) tStrikePrice $
               LT.insert (tnum tOptAttribute) tOptAttribute $
               LT.insert (tnum tContractMultiplier) tContractMultiplier $
               LT.insert (tnum tCouponRate) tCouponRate $
               LT.insert (tnum tSecurityExchange) tSecurityExchange $
               LT.insert (tnum tIssuer) tIssuer $
               LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
               LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
               LT.insert (tnum tSecurityDesc) tSecurityDesc $
               LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
               LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
               LT.insert (tnum tPrevClosePx) tPrevClosePx $
               LT.insert (tnum tQuoteRequestType) tQuoteRequestType $
               LT.insert (tnum tQuoteType) tQuoteType $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID                LT.new
               where
                  gNoSecurityAltID'''''' = FIXTag
                     { tName = "NoSecurityAltID"
                     , tnum = tnum tNoSecurityAltID
                     , tparser = gNoSecurityAltIDP''''''
                     , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec'''''' }

                  gNoSecurityAltIDP'''''' = groupP gNoSecurityAltIDSpec''''''
                  gNoSecurityAltIDSpec'''''' = FGSpec
                     { gsLength = tNoSecurityAltID
                     , gsSeperator = tSecurityAltID
                     , gsBody = gNoSecurityAltIDBody'''''' }
                     where
                     gNoSecurityAltIDBody'''''' = 
                        LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                         LT.new




mQuoteStatusReport :: FIXMessageSpec
mQuoteStatusReport = FMSpec
   { msName = "QuoteStatusReport"
   , msType = C.pack "AI"
   , msHeader = headerFIX43
   , msBody = mQuoteStatusReportBody
   , msTrailer = trailerFIX43 }
   where
   mQuoteStatusReportBody = 
      LT.insert (tnum tQuoteStatusReqID) tQuoteStatusReqID $
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteType) tQuoteType $
      LT.insert (tnum tNoPartyIDs) gNoPartyIDs''' $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tAccountType) tAccountType $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tSecurityIDSource) tSecurityIDSource $
      LT.insert (tnum tNoSecurityAltID) gNoSecurityAltID''' $
      LT.insert (tnum tProduct) tProduct $
      LT.insert (tnum tCFICode) tCFICode $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDate) tMaturityDate $
      LT.insert (tnum tCouponPaymentDate) tCouponPaymentDate $
      LT.insert (tnum tIssueDate) tIssueDate $
      LT.insert (tnum tRepoCollateralSecurityType) tRepoCollateralSecurityType $
      LT.insert (tnum tRepurchaseTerm) tRepurchaseTerm $
      LT.insert (tnum tRepurchaseRate) tRepurchaseRate $
      LT.insert (tnum tFactor) tFactor $
      LT.insert (tnum tCreditRating) tCreditRating $
      LT.insert (tnum tInstrRegistry) tInstrRegistry $
      LT.insert (tnum tCountryOfIssue) tCountryOfIssue $
      LT.insert (tnum tStateOrProvinceOfIssue) tStateOrProvinceOfIssue $
      LT.insert (tnum tLocaleOfIssue) tLocaleOfIssue $
      LT.insert (tnum tRedemptionDate) tRedemptionDate $
      LT.insert (tnum tStrikePrice) tStrikePrice $
      LT.insert (tnum tOptAttribute) tOptAttribute $
      LT.insert (tnum tContractMultiplier) tContractMultiplier $
      LT.insert (tnum tCouponRate) tCouponRate $
      LT.insert (tnum tSecurityExchange) tSecurityExchange $
      LT.insert (tnum tIssuer) tIssuer $
      LT.insert (tnum tEncodedIssuerLen) tEncodedIssuerLen $
      LT.insert (tnum tEncodedIssuer) tEncodedIssuer $
      LT.insert (tnum tSecurityDesc) tSecurityDesc $
      LT.insert (tnum tEncodedSecurityDescLen) tEncodedSecurityDescLen $
      LT.insert (tnum tEncodedSecurityDesc) tEncodedSecurityDesc $
      LT.insert (tnum tBidPx) tBidPx $
      LT.insert (tnum tOfferPx) tOfferPx $
      LT.insert (tnum tMktBidPx) tMktBidPx $
      LT.insert (tnum tMktOfferPx) tMktOfferPx $
      LT.insert (tnum tMinBidSize) tMinBidSize $
      LT.insert (tnum tBidSize) tBidSize $
      LT.insert (tnum tMinOfferSize) tMinOfferSize $
      LT.insert (tnum tOfferSize) tOfferSize $
      LT.insert (tnum tValidUntilTime) tValidUntilTime $
      LT.insert (tnum tBidSpotRate) tBidSpotRate $
      LT.insert (tnum tOfferSpotRate) tOfferSpotRate $
      LT.insert (tnum tBidForwardPoints) tBidForwardPoints $
      LT.insert (tnum tOfferForwardPoints) tOfferForwardPoints $
      LT.insert (tnum tMidPx) tMidPx $
      LT.insert (tnum tBidYield) tBidYield $
      LT.insert (tnum tMidYield) tMidYield $
      LT.insert (tnum tOfferYield) tOfferYield $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
      LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tSettlCurrBidFxRate) tSettlCurrBidFxRate $
      LT.insert (tnum tSettlCurrOfferFxRate) tSettlCurrOfferFxRate $
      LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
      LT.insert (tnum tCommission) tCommission $
      LT.insert (tnum tCommType) tCommType $
      LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tQuoteStatus) tQuoteStatus       LT.new
      where
         gNoPartyIDs''' = FIXTag
            { tName = "NoPartyIDs"
            , tnum = tnum tNoPartyIDs
            , tparser = gNoPartyIDsP'''
            , arbitraryValue = arbibtraryFIXGroup gNoPartyIDsSpec''' }

         gNoPartyIDsP''' = groupP gNoPartyIDsSpec'''
         gNoPartyIDsSpec''' = FGSpec
            { gsLength = tNoPartyIDs
            , gsSeperator = tPartyID
            , gsBody = gNoPartyIDsBody''' }
            where
            gNoPartyIDsBody''' = 
               LT.insert (tnum tPartyIDSource) tPartyIDSource $
               LT.insert (tnum tPartyRole) tPartyRole $
               LT.insert (tnum tPartySubID) tPartySubID                LT.new

         gNoSecurityAltID''' = FIXTag
            { tName = "NoSecurityAltID"
            , tnum = tnum tNoSecurityAltID
            , tparser = gNoSecurityAltIDP'''
            , arbitraryValue = arbibtraryFIXGroup gNoSecurityAltIDSpec''' }

         gNoSecurityAltIDP''' = groupP gNoSecurityAltIDSpec'''
         gNoSecurityAltIDSpec''' = FGSpec
            { gsLength = tNoSecurityAltID
            , gsSeperator = tSecurityAltID
            , gsBody = gNoSecurityAltIDBody''' }
            where
            gNoSecurityAltIDBody''' = 
               LT.insert (tnum tSecurityAltIDSource) tSecurityAltIDSource                LT.new



fix43 :: FIXSpec
fix43 = FSpec
   { fsVersion = "FIX.4.3"
   , fsHeader = headerFIX43
   , fsTrailer = trailerFIX43
   , fsMessages = fix43Messages }
   where
      fix43Messages =
          LT.insert (msType mHeartbeat) mHeartbeat $
          LT.insert (msType mTestRequest) mTestRequest $
          LT.insert (msType mResendRequest) mResendRequest $
          LT.insert (msType mReject) mReject $
          LT.insert (msType mSequenceReset) mSequenceReset $
          LT.insert (msType mLogout) mLogout $
          LT.insert (msType mIOI) mIOI $
          LT.insert (msType mAdvertisement) mAdvertisement $
          LT.insert (msType mExecutionReport) mExecutionReport $
          LT.insert (msType mOrderCancelReject) mOrderCancelReject $
          LT.insert (msType mLogon) mLogon $
          LT.insert (msType mNews) mNews $
          LT.insert (msType mEmail) mEmail $
          LT.insert (msType mNewOrderSingle) mNewOrderSingle $
          LT.insert (msType mNewOrderList) mNewOrderList $
          LT.insert (msType mOrderCancelRequest) mOrderCancelRequest $
          LT.insert (msType mOrderCancelReplaceRequest) mOrderCancelReplaceRequest $
          LT.insert (msType mOrderStatusRequest) mOrderStatusRequest $
          LT.insert (msType mAllocation) mAllocation $
          LT.insert (msType mListCancelRequest) mListCancelRequest $
          LT.insert (msType mListExecute) mListExecute $
          LT.insert (msType mListStatusRequest) mListStatusRequest $
          LT.insert (msType mListStatus) mListStatus $
          LT.insert (msType mAllocationAck) mAllocationAck $
          LT.insert (msType mDontKnowTrade) mDontKnowTrade $
          LT.insert (msType mQuoteRequest) mQuoteRequest $
          LT.insert (msType mQuote) mQuote $
          LT.insert (msType mSettlementInstructions) mSettlementInstructions $
          LT.insert (msType mMarketDataRequest) mMarketDataRequest $
          LT.insert (msType mMarketDataSnapshotFullRefresh) mMarketDataSnapshotFullRefresh $
          LT.insert (msType mMarketDataIncrementalRefresh) mMarketDataIncrementalRefresh $
          LT.insert (msType mMarketDataRequestReject) mMarketDataRequestReject $
          LT.insert (msType mQuoteCancel) mQuoteCancel $
          LT.insert (msType mQuoteStatusRequest) mQuoteStatusRequest $
          LT.insert (msType mMassQuoteAcknowledgement) mMassQuoteAcknowledgement $
          LT.insert (msType mSecurityDefinitionRequest) mSecurityDefinitionRequest $
          LT.insert (msType mSecurityDefinition) mSecurityDefinition $
          LT.insert (msType mSecurityStatusRequest) mSecurityStatusRequest $
          LT.insert (msType mSecurityStatus) mSecurityStatus $
          LT.insert (msType mTradingSessionStatusRequest) mTradingSessionStatusRequest $
          LT.insert (msType mTradingSessionStatus) mTradingSessionStatus $
          LT.insert (msType mMassQuote) mMassQuote $
          LT.insert (msType mBusinessMessageReject) mBusinessMessageReject $
          LT.insert (msType mBidRequest) mBidRequest $
          LT.insert (msType mBidResponse) mBidResponse $
          LT.insert (msType mListStrikePrice) mListStrikePrice $
          LT.insert (msType mRegistrationInstructions) mRegistrationInstructions $
          LT.insert (msType mRegistrationInstructionsResponse) mRegistrationInstructionsResponse $
          LT.insert (msType mOrderMassCancelRequest) mOrderMassCancelRequest $
          LT.insert (msType mOrderMassCancelReport) mOrderMassCancelReport $
          LT.insert (msType mNewOrderCross) mNewOrderCross $
          LT.insert (msType mCrossOrderCancelRequest) mCrossOrderCancelRequest $
          LT.insert (msType mCrossOrderCancelReplaceRequest) mCrossOrderCancelReplaceRequest $
          LT.insert (msType mSecurityTypeRequest) mSecurityTypeRequest $
          LT.insert (msType mSecurityTypes) mSecurityTypes $
          LT.insert (msType mSecurityListRequest) mSecurityListRequest $
          LT.insert (msType mSecurityList) mSecurityList $
          LT.insert (msType mDerivativeSecurityListRequest) mDerivativeSecurityListRequest $
          LT.insert (msType mDerivativeSecurityList) mDerivativeSecurityList $
          LT.insert (msType mNewOrderMultileg) mNewOrderMultileg $
          LT.insert (msType mMultilegOrderCancelReplaceRequest) mMultilegOrderCancelReplaceRequest $
          LT.insert (msType mTradeCaptureReportRequest) mTradeCaptureReportRequest $
          LT.insert (msType mTradeCaptureReport) mTradeCaptureReport $
          LT.insert (msType mOrderMassStatusRequest) mOrderMassStatusRequest $
          LT.insert (msType mQuoteRequestReject) mQuoteRequestReject $
          LT.insert (msType mRFQRequest) mRFQRequest $
          LT.insert (msType mQuoteStatusReport) mQuoteStatusReport           LT.new 
