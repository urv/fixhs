module Data.FIX.FIX42 where
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT ( new, insert )
import Common.FIXMessage
import Common.FIXParser


tAccount :: FIXTag
tAccount = FIXTag 
   { tName = "Account"
   , tnum = 1
   , tparser = toFIXString }

tAdvId :: FIXTag
tAdvId = FIXTag 
   { tName = "AdvId"
   , tnum = 2
   , tparser = toFIXString }

tAdvRefID :: FIXTag
tAdvRefID = FIXTag 
   { tName = "AdvRefID"
   , tnum = 3
   , tparser = toFIXString }

tAdvSide :: FIXTag
tAdvSide = FIXTag 
   { tName = "AdvSide"
   , tnum = 4
   , tparser = toFIXChar }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag 
   { tName = "AdvTransType"
   , tnum = 5
   , tparser = toFIXString }

tAvgPx :: FIXTag
tAvgPx = FIXTag 
   { tName = "AvgPx"
   , tnum = 6
   , tparser = toFIXPrice }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag 
   { tName = "BeginSeqNo"
   , tnum = 7
   , tparser = toFIXInt }

tBeginString :: FIXTag
tBeginString = FIXTag 
   { tName = "BeginString"
   , tnum = 8
   , tparser = toFIXString }

tBodyLength :: FIXTag
tBodyLength = FIXTag 
   { tName = "BodyLength"
   , tnum = 9
   , tparser = toFIXInt }

tCheckSum :: FIXTag
tCheckSum = FIXTag 
   { tName = "CheckSum"
   , tnum = 10
   , tparser = toFIXString }

tClOrdID :: FIXTag
tClOrdID = FIXTag 
   { tName = "ClOrdID"
   , tnum = 11
   , tparser = toFIXString }

tCommission :: FIXTag
tCommission = FIXTag 
   { tName = "Commission"
   , tnum = 12
   , tparser = toFIXAmt }

tCommType :: FIXTag
tCommType = FIXTag 
   { tName = "CommType"
   , tnum = 13
   , tparser = toFIXChar }

tCumQty :: FIXTag
tCumQty = FIXTag 
   { tName = "CumQty"
   , tnum = 14
   , tparser = toFIXQuantity }

tCurrency :: FIXTag
tCurrency = FIXTag 
   { tName = "Currency"
   , tnum = 15
   , tparser = toFIXCurrency }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag 
   { tName = "EndSeqNo"
   , tnum = 16
   , tparser = toFIXInt }

tExecID :: FIXTag
tExecID = FIXTag 
   { tName = "ExecID"
   , tnum = 17
   , tparser = toFIXString }

tExecInst :: FIXTag
tExecInst = FIXTag 
   { tName = "ExecInst"
   , tnum = 18
   , tparser = toFIXMultipleValueString }

tExecRefID :: FIXTag
tExecRefID = FIXTag 
   { tName = "ExecRefID"
   , tnum = 19
   , tparser = toFIXString }

tExecTransType :: FIXTag
tExecTransType = FIXTag 
   { tName = "ExecTransType"
   , tnum = 20
   , tparser = toFIXChar }

tHandlInst :: FIXTag
tHandlInst = FIXTag 
   { tName = "HandlInst"
   , tnum = 21
   , tparser = toFIXChar }

tIDSource :: FIXTag
tIDSource = FIXTag 
   { tName = "IDSource"
   , tnum = 22
   , tparser = toFIXString }

tIOIid :: FIXTag
tIOIid = FIXTag 
   { tName = "IOIid"
   , tnum = 23
   , tparser = toFIXString }

tIOIOthSvc :: FIXTag
tIOIOthSvc = FIXTag 
   { tName = "IOIOthSvc"
   , tnum = 24
   , tparser = toFIXChar }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag 
   { tName = "IOIQltyInd"
   , tnum = 25
   , tparser = toFIXChar }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag 
   { tName = "IOIRefID"
   , tnum = 26
   , tparser = toFIXString }

tIOIShares :: FIXTag
tIOIShares = FIXTag 
   { tName = "IOIShares"
   , tnum = 27
   , tparser = toFIXString }

tIOITransType :: FIXTag
tIOITransType = FIXTag 
   { tName = "IOITransType"
   , tnum = 28
   , tparser = toFIXChar }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag 
   { tName = "LastCapacity"
   , tnum = 29
   , tparser = toFIXChar }

tLastMkt :: FIXTag
tLastMkt = FIXTag 
   { tName = "LastMkt"
   , tnum = 30
   , tparser = toFIXExchange }

tLastPx :: FIXTag
tLastPx = FIXTag 
   { tName = "LastPx"
   , tnum = 31
   , tparser = toFIXPrice }

tLastShares :: FIXTag
tLastShares = FIXTag 
   { tName = "LastShares"
   , tnum = 32
   , tparser = toFIXQuantity }

tLinesOfText :: FIXTag
tLinesOfText = FIXTag 
   { tName = "LinesOfText"
   , tnum = 33
   , tparser = toFIXInt }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag 
   { tName = "MsgSeqNum"
   , tnum = 34
   , tparser = toFIXInt }

tMsgType :: FIXTag
tMsgType = FIXTag 
   { tName = "MsgType"
   , tnum = 35
   , tparser = toFIXString }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag 
   { tName = "NewSeqNo"
   , tnum = 36
   , tparser = toFIXInt }

tOrderID :: FIXTag
tOrderID = FIXTag 
   { tName = "OrderID"
   , tnum = 37
   , tparser = toFIXString }

tOrderQty :: FIXTag
tOrderQty = FIXTag 
   { tName = "OrderQty"
   , tnum = 38
   , tparser = toFIXQuantity }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag 
   { tName = "OrdStatus"
   , tnum = 39
   , tparser = toFIXChar }

tOrdType :: FIXTag
tOrdType = FIXTag 
   { tName = "OrdType"
   , tnum = 40
   , tparser = toFIXChar }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag 
   { tName = "OrigClOrdID"
   , tnum = 41
   , tparser = toFIXString }

tOrigTime :: FIXTag
tOrigTime = FIXTag 
   { tName = "OrigTime"
   , tnum = 42
   , tparser = toFIXUTCTimestamp }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag 
   { tName = "PossDupFlag"
   , tnum = 43
   , tparser = toFIXBool }

tPrice :: FIXTag
tPrice = FIXTag 
   { tName = "Price"
   , tnum = 44
   , tparser = toFIXPrice }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag 
   { tName = "RefSeqNum"
   , tnum = 45
   , tparser = toFIXInt }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag 
   { tName = "RelatdSym"
   , tnum = 46
   , tparser = toFIXString }

tRule80A :: FIXTag
tRule80A = FIXTag 
   { tName = "Rule80A"
   , tnum = 47
   , tparser = toFIXChar }

tSecurityID :: FIXTag
tSecurityID = FIXTag 
   { tName = "SecurityID"
   , tnum = 48
   , tparser = toFIXString }

tSenderCompID :: FIXTag
tSenderCompID = FIXTag 
   { tName = "SenderCompID"
   , tnum = 49
   , tparser = toFIXString }

tSenderSubID :: FIXTag
tSenderSubID = FIXTag 
   { tName = "SenderSubID"
   , tnum = 50
   , tparser = toFIXString }

tSendingDate :: FIXTag
tSendingDate = FIXTag 
   { tName = "SendingDate"
   , tnum = 51
   , tparser = toFIXLocalMktDate }

tSendingTime :: FIXTag
tSendingTime = FIXTag 
   { tName = "SendingTime"
   , tnum = 52
   , tparser = toFIXUTCTimestamp }

tShares :: FIXTag
tShares = FIXTag 
   { tName = "Shares"
   , tnum = 53
   , tparser = toFIXQuantity }

tSide :: FIXTag
tSide = FIXTag 
   { tName = "Side"
   , tnum = 54
   , tparser = toFIXChar }

tSymbol :: FIXTag
tSymbol = FIXTag 
   { tName = "Symbol"
   , tnum = 55
   , tparser = toFIXString }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag 
   { tName = "TargetCompID"
   , tnum = 56
   , tparser = toFIXString }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag 
   { tName = "TargetSubID"
   , tnum = 57
   , tparser = toFIXString }

tText :: FIXTag
tText = FIXTag 
   { tName = "Text"
   , tnum = 58
   , tparser = toFIXString }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag 
   { tName = "TimeInForce"
   , tnum = 59
   , tparser = toFIXChar }

tTransactTime :: FIXTag
tTransactTime = FIXTag 
   { tName = "TransactTime"
   , tnum = 60
   , tparser = toFIXUTCTimestamp }

tUrgency :: FIXTag
tUrgency = FIXTag 
   { tName = "Urgency"
   , tnum = 61
   , tparser = toFIXChar }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag 
   { tName = "ValidUntilTime"
   , tnum = 62
   , tparser = toFIXUTCTimestamp }

tSettlmntTyp :: FIXTag
tSettlmntTyp = FIXTag 
   { tName = "SettlmntTyp"
   , tnum = 63
   , tparser = toFIXChar }

tFutSettDate :: FIXTag
tFutSettDate = FIXTag 
   { tName = "FutSettDate"
   , tnum = 64
   , tparser = toFIXLocalMktDate }

tSymbolSfx :: FIXTag
tSymbolSfx = FIXTag 
   { tName = "SymbolSfx"
   , tnum = 65
   , tparser = toFIXString }

tListID :: FIXTag
tListID = FIXTag 
   { tName = "ListID"
   , tnum = 66
   , tparser = toFIXString }

tListSeqNo :: FIXTag
tListSeqNo = FIXTag 
   { tName = "ListSeqNo"
   , tnum = 67
   , tparser = toFIXInt }

tTotNoOrders :: FIXTag
tTotNoOrders = FIXTag 
   { tName = "TotNoOrders"
   , tnum = 68
   , tparser = toFIXInt }

tListExecInst :: FIXTag
tListExecInst = FIXTag 
   { tName = "ListExecInst"
   , tnum = 69
   , tparser = toFIXString }

tAllocID :: FIXTag
tAllocID = FIXTag 
   { tName = "AllocID"
   , tnum = 70
   , tparser = toFIXString }

tAllocTransType :: FIXTag
tAllocTransType = FIXTag 
   { tName = "AllocTransType"
   , tnum = 71
   , tparser = toFIXChar }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag 
   { tName = "RefAllocID"
   , tnum = 72
   , tparser = toFIXString }

tNoOrders :: FIXTag
tNoOrders = FIXTag 
   { tName = "NoOrders"
   , tnum = 73
   , tparser = toFIXInt }

tAvgPrxPrecision :: FIXTag
tAvgPrxPrecision = FIXTag 
   { tName = "AvgPrxPrecision"
   , tnum = 74
   , tparser = toFIXInt }

tTradeDate :: FIXTag
tTradeDate = FIXTag 
   { tName = "TradeDate"
   , tnum = 75
   , tparser = toFIXLocalMktDate }

tExecBroker :: FIXTag
tExecBroker = FIXTag 
   { tName = "ExecBroker"
   , tnum = 76
   , tparser = toFIXString }

tOpenClose :: FIXTag
tOpenClose = FIXTag 
   { tName = "OpenClose"
   , tnum = 77
   , tparser = toFIXChar }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag 
   { tName = "NoAllocs"
   , tnum = 78
   , tparser = toFIXInt }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag 
   { tName = "AllocAccount"
   , tnum = 79
   , tparser = toFIXString }

tAllocShares :: FIXTag
tAllocShares = FIXTag 
   { tName = "AllocShares"
   , tnum = 80
   , tparser = toFIXQuantity }

tProcessCode :: FIXTag
tProcessCode = FIXTag 
   { tName = "ProcessCode"
   , tnum = 81
   , tparser = toFIXChar }

tNoRpts :: FIXTag
tNoRpts = FIXTag 
   { tName = "NoRpts"
   , tnum = 82
   , tparser = toFIXInt }

tRptSeq :: FIXTag
tRptSeq = FIXTag 
   { tName = "RptSeq"
   , tnum = 83
   , tparser = toFIXInt }

tCxlQty :: FIXTag
tCxlQty = FIXTag 
   { tName = "CxlQty"
   , tnum = 84
   , tparser = toFIXQuantity }

tNoDlvyInst :: FIXTag
tNoDlvyInst = FIXTag 
   { tName = "NoDlvyInst"
   , tnum = 85
   , tparser = toFIXInt }

tDlvyInst :: FIXTag
tDlvyInst = FIXTag 
   { tName = "DlvyInst"
   , tnum = 86
   , tparser = toFIXString }

tAllocStatus :: FIXTag
tAllocStatus = FIXTag 
   { tName = "AllocStatus"
   , tnum = 87
   , tparser = toFIXInt }

tAllocRejCode :: FIXTag
tAllocRejCode = FIXTag 
   { tName = "AllocRejCode"
   , tnum = 88
   , tparser = toFIXInt }

tSignature :: FIXTag
tSignature = FIXTag 
   { tName = "Signature"
   , tnum = 89
   , tparser = toFIXData }

tSecureDataLen :: FIXTag
tSecureDataLen = FIXTag 
   { tName = "SecureDataLen"
   , tnum = 90
   , tparser = toFIXDataLen }

tSecureData :: FIXTag
tSecureData = FIXTag 
   { tName = "SecureData"
   , tnum = 91
   , tparser = toFIXData }

tBrokerOfCredit :: FIXTag
tBrokerOfCredit = FIXTag 
   { tName = "BrokerOfCredit"
   , tnum = 92
   , tparser = toFIXString }

tSignatureLength :: FIXTag
tSignatureLength = FIXTag 
   { tName = "SignatureLength"
   , tnum = 93
   , tparser = toFIXDataLen }

tEmailType :: FIXTag
tEmailType = FIXTag 
   { tName = "EmailType"
   , tnum = 94
   , tparser = toFIXChar }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag 
   { tName = "RawDataLength"
   , tnum = 95
   , tparser = toFIXDataLen }

tRawData :: FIXTag
tRawData = FIXTag 
   { tName = "RawData"
   , tnum = 96
   , tparser = toFIXData }

tPossResend :: FIXTag
tPossResend = FIXTag 
   { tName = "PossResend"
   , tnum = 97
   , tparser = toFIXBool }

tEncryptMethod :: FIXTag
tEncryptMethod = FIXTag 
   { tName = "EncryptMethod"
   , tnum = 98
   , tparser = toFIXInt }

tStopPx :: FIXTag
tStopPx = FIXTag 
   { tName = "StopPx"
   , tnum = 99
   , tparser = toFIXPrice }

tExDestination :: FIXTag
tExDestination = FIXTag 
   { tName = "ExDestination"
   , tnum = 100
   , tparser = toFIXExchange }

tCxlRejReason :: FIXTag
tCxlRejReason = FIXTag 
   { tName = "CxlRejReason"
   , tnum = 102
   , tparser = toFIXInt }

tOrdRejReason :: FIXTag
tOrdRejReason = FIXTag 
   { tName = "OrdRejReason"
   , tnum = 103
   , tparser = toFIXInt }

tIOIQualifier :: FIXTag
tIOIQualifier = FIXTag 
   { tName = "IOIQualifier"
   , tnum = 104
   , tparser = toFIXChar }

tWaveNo :: FIXTag
tWaveNo = FIXTag 
   { tName = "WaveNo"
   , tnum = 105
   , tparser = toFIXString }

tIssuer :: FIXTag
tIssuer = FIXTag 
   { tName = "Issuer"
   , tnum = 106
   , tparser = toFIXString }

tSecurityDesc :: FIXTag
tSecurityDesc = FIXTag 
   { tName = "SecurityDesc"
   , tnum = 107
   , tparser = toFIXString }

tHeartBtInt :: FIXTag
tHeartBtInt = FIXTag 
   { tName = "HeartBtInt"
   , tnum = 108
   , tparser = toFIXInt }

tClientID :: FIXTag
tClientID = FIXTag 
   { tName = "ClientID"
   , tnum = 109
   , tparser = toFIXString }

tMinQty :: FIXTag
tMinQty = FIXTag 
   { tName = "MinQty"
   , tnum = 110
   , tparser = toFIXQuantity }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag 
   { tName = "MaxFloor"
   , tnum = 111
   , tparser = toFIXQuantity }

tTestReqID :: FIXTag
tTestReqID = FIXTag 
   { tName = "TestReqID"
   , tnum = 112
   , tparser = toFIXString }

tReportToExch :: FIXTag
tReportToExch = FIXTag 
   { tName = "ReportToExch"
   , tnum = 113
   , tparser = toFIXBool }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag 
   { tName = "LocateReqd"
   , tnum = 114
   , tparser = toFIXBool }

tOnBehalfOfCompID :: FIXTag
tOnBehalfOfCompID = FIXTag 
   { tName = "OnBehalfOfCompID"
   , tnum = 115
   , tparser = toFIXString }

tOnBehalfOfSubID :: FIXTag
tOnBehalfOfSubID = FIXTag 
   { tName = "OnBehalfOfSubID"
   , tnum = 116
   , tparser = toFIXString }

tQuoteID :: FIXTag
tQuoteID = FIXTag 
   { tName = "QuoteID"
   , tnum = 117
   , tparser = toFIXString }

tNetMoney :: FIXTag
tNetMoney = FIXTag 
   { tName = "NetMoney"
   , tnum = 118
   , tparser = toFIXAmt }

tSettlCurrAmt :: FIXTag
tSettlCurrAmt = FIXTag 
   { tName = "SettlCurrAmt"
   , tnum = 119
   , tparser = toFIXAmt }

tSettlCurrency :: FIXTag
tSettlCurrency = FIXTag 
   { tName = "SettlCurrency"
   , tnum = 120
   , tparser = toFIXCurrency }

tForexReq :: FIXTag
tForexReq = FIXTag 
   { tName = "ForexReq"
   , tnum = 121
   , tparser = toFIXBool }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag 
   { tName = "OrigSendingTime"
   , tnum = 122
   , tparser = toFIXUTCTimestamp }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag 
   { tName = "GapFillFlag"
   , tnum = 123
   , tparser = toFIXBool }

tNoExecs :: FIXTag
tNoExecs = FIXTag 
   { tName = "NoExecs"
   , tnum = 124
   , tparser = toFIXInt }

tCxlType :: FIXTag
tCxlType = FIXTag 
   { tName = "CxlType"
   , tnum = 125
   , tparser = toFIXChar }

tExpireTime :: FIXTag
tExpireTime = FIXTag 
   { tName = "ExpireTime"
   , tnum = 126
   , tparser = toFIXUTCTimestamp }

tDKReason :: FIXTag
tDKReason = FIXTag 
   { tName = "DKReason"
   , tnum = 127
   , tparser = toFIXChar }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag 
   { tName = "DeliverToCompID"
   , tnum = 128
   , tparser = toFIXString }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag 
   { tName = "DeliverToSubID"
   , tnum = 129
   , tparser = toFIXString }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag 
   { tName = "IOINaturalFlag"
   , tnum = 130
   , tparser = toFIXBool }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag 
   { tName = "QuoteReqID"
   , tnum = 131
   , tparser = toFIXString }

tBidPx :: FIXTag
tBidPx = FIXTag 
   { tName = "BidPx"
   , tnum = 132
   , tparser = toFIXPrice }

tOfferPx :: FIXTag
tOfferPx = FIXTag 
   { tName = "OfferPx"
   , tnum = 133
   , tparser = toFIXPrice }

tBidSize :: FIXTag
tBidSize = FIXTag 
   { tName = "BidSize"
   , tnum = 134
   , tparser = toFIXQuantity }

tOfferSize :: FIXTag
tOfferSize = FIXTag 
   { tName = "OfferSize"
   , tnum = 135
   , tparser = toFIXQuantity }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag 
   { tName = "NoMiscFees"
   , tnum = 136
   , tparser = toFIXInt }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag 
   { tName = "MiscFeeAmt"
   , tnum = 137
   , tparser = toFIXAmt }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag 
   { tName = "MiscFeeCurr"
   , tnum = 138
   , tparser = toFIXCurrency }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag 
   { tName = "MiscFeeType"
   , tnum = 139
   , tparser = toFIXChar }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag 
   { tName = "PrevClosePx"
   , tnum = 140
   , tparser = toFIXPrice }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag 
   { tName = "ResetSeqNumFlag"
   , tnum = 141
   , tparser = toFIXBool }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag 
   { tName = "SenderLocationID"
   , tnum = 142
   , tparser = toFIXString }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag 
   { tName = "TargetLocationID"
   , tnum = 143
   , tparser = toFIXString }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag 
   { tName = "OnBehalfOfLocationID"
   , tnum = 144
   , tparser = toFIXString }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag 
   { tName = "DeliverToLocationID"
   , tnum = 145
   , tparser = toFIXString }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag 
   { tName = "NoRelatedSym"
   , tnum = 146
   , tparser = toFIXInt }

tSubject :: FIXTag
tSubject = FIXTag 
   { tName = "Subject"
   , tnum = 147
   , tparser = toFIXString }

tHeadline :: FIXTag
tHeadline = FIXTag 
   { tName = "Headline"
   , tnum = 148
   , tparser = toFIXString }

tURLLink :: FIXTag
tURLLink = FIXTag 
   { tName = "URLLink"
   , tnum = 149
   , tparser = toFIXString }

tExecType :: FIXTag
tExecType = FIXTag 
   { tName = "ExecType"
   , tnum = 150
   , tparser = toFIXChar }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag 
   { tName = "LeavesQty"
   , tnum = 151
   , tparser = toFIXQuantity }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag 
   { tName = "CashOrderQty"
   , tnum = 152
   , tparser = toFIXQuantity }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag 
   { tName = "AllocAvgPx"
   , tnum = 153
   , tparser = toFIXPrice }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag 
   { tName = "AllocNetMoney"
   , tnum = 154
   , tparser = toFIXAmt }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag 
   { tName = "SettlCurrFxRate"
   , tnum = 155
   , tparser = toFIXFloat }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag 
   { tName = "SettlCurrFxRateCalc"
   , tnum = 156
   , tparser = toFIXChar }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag 
   { tName = "NumDaysInterest"
   , tnum = 157
   , tparser = toFIXInt }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag 
   { tName = "AccruedInterestRate"
   , tnum = 158
   , tparser = toFIXFloat }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag 
   { tName = "AccruedInterestAmt"
   , tnum = 159
   , tparser = toFIXAmt }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag 
   { tName = "SettlInstMode"
   , tnum = 160
   , tparser = toFIXChar }

tAllocText :: FIXTag
tAllocText = FIXTag 
   { tName = "AllocText"
   , tnum = 161
   , tparser = toFIXString }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag 
   { tName = "SettlInstID"
   , tnum = 162
   , tparser = toFIXString }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag 
   { tName = "SettlInstTransType"
   , tnum = 163
   , tparser = toFIXChar }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag 
   { tName = "EmailThreadID"
   , tnum = 164
   , tparser = toFIXString }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag 
   { tName = "SettlInstSource"
   , tnum = 165
   , tparser = toFIXChar }

tSettlLocation :: FIXTag
tSettlLocation = FIXTag 
   { tName = "SettlLocation"
   , tnum = 166
   , tparser = toFIXString }

tSecurityType :: FIXTag
tSecurityType = FIXTag 
   { tName = "SecurityType"
   , tnum = 167
   , tparser = toFIXString }

tEffectiveTime :: FIXTag
tEffectiveTime = FIXTag 
   { tName = "EffectiveTime"
   , tnum = 168
   , tparser = toFIXUTCTimestamp }

tStandInstDbType :: FIXTag
tStandInstDbType = FIXTag 
   { tName = "StandInstDbType"
   , tnum = 169
   , tparser = toFIXInt }

tStandInstDbName :: FIXTag
tStandInstDbName = FIXTag 
   { tName = "StandInstDbName"
   , tnum = 170
   , tparser = toFIXString }

tStandInstDbID :: FIXTag
tStandInstDbID = FIXTag 
   { tName = "StandInstDbID"
   , tnum = 171
   , tparser = toFIXString }

tSettlDeliveryType :: FIXTag
tSettlDeliveryType = FIXTag 
   { tName = "SettlDeliveryType"
   , tnum = 172
   , tparser = toFIXInt }

tSettlDepositoryCode :: FIXTag
tSettlDepositoryCode = FIXTag 
   { tName = "SettlDepositoryCode"
   , tnum = 173
   , tparser = toFIXString }

tSettlBrkrCode :: FIXTag
tSettlBrkrCode = FIXTag 
   { tName = "SettlBrkrCode"
   , tnum = 174
   , tparser = toFIXString }

tSettlInstCode :: FIXTag
tSettlInstCode = FIXTag 
   { tName = "SettlInstCode"
   , tnum = 175
   , tparser = toFIXString }

tSecuritySettlAgentName :: FIXTag
tSecuritySettlAgentName = FIXTag 
   { tName = "SecuritySettlAgentName"
   , tnum = 176
   , tparser = toFIXString }

tSecuritySettlAgentCode :: FIXTag
tSecuritySettlAgentCode = FIXTag 
   { tName = "SecuritySettlAgentCode"
   , tnum = 177
   , tparser = toFIXString }

tSecuritySettlAgentAcctNum :: FIXTag
tSecuritySettlAgentAcctNum = FIXTag 
   { tName = "SecuritySettlAgentAcctNum"
   , tnum = 178
   , tparser = toFIXString }

tSecuritySettlAgentAcctName :: FIXTag
tSecuritySettlAgentAcctName = FIXTag 
   { tName = "SecuritySettlAgentAcctName"
   , tnum = 179
   , tparser = toFIXString }

tSecuritySettlAgentContactName :: FIXTag
tSecuritySettlAgentContactName = FIXTag 
   { tName = "SecuritySettlAgentContactName"
   , tnum = 180
   , tparser = toFIXString }

tSecuritySettlAgentContactPhone :: FIXTag
tSecuritySettlAgentContactPhone = FIXTag 
   { tName = "SecuritySettlAgentContactPhone"
   , tnum = 181
   , tparser = toFIXString }

tCashSettlAgentName :: FIXTag
tCashSettlAgentName = FIXTag 
   { tName = "CashSettlAgentName"
   , tnum = 182
   , tparser = toFIXString }

tCashSettlAgentCode :: FIXTag
tCashSettlAgentCode = FIXTag 
   { tName = "CashSettlAgentCode"
   , tnum = 183
   , tparser = toFIXString }

tCashSettlAgentAcctNum :: FIXTag
tCashSettlAgentAcctNum = FIXTag 
   { tName = "CashSettlAgentAcctNum"
   , tnum = 184
   , tparser = toFIXString }

tCashSettlAgentAcctName :: FIXTag
tCashSettlAgentAcctName = FIXTag 
   { tName = "CashSettlAgentAcctName"
   , tnum = 185
   , tparser = toFIXString }

tCashSettlAgentContactName :: FIXTag
tCashSettlAgentContactName = FIXTag 
   { tName = "CashSettlAgentContactName"
   , tnum = 186
   , tparser = toFIXString }

tCashSettlAgentContactPhone :: FIXTag
tCashSettlAgentContactPhone = FIXTag 
   { tName = "CashSettlAgentContactPhone"
   , tnum = 187
   , tparser = toFIXString }

tBidSpotRate :: FIXTag
tBidSpotRate = FIXTag 
   { tName = "BidSpotRate"
   , tnum = 188
   , tparser = toFIXPrice }

tBidForwardPoints :: FIXTag
tBidForwardPoints = FIXTag 
   { tName = "BidForwardPoints"
   , tnum = 189
   , tparser = toFIXPriceOffset }

tOfferSpotRate :: FIXTag
tOfferSpotRate = FIXTag 
   { tName = "OfferSpotRate"
   , tnum = 190
   , tparser = toFIXPrice }

tOfferForwardPoints :: FIXTag
tOfferForwardPoints = FIXTag 
   { tName = "OfferForwardPoints"
   , tnum = 191
   , tparser = toFIXPriceOffset }

tOrderQty2 :: FIXTag
tOrderQty2 = FIXTag 
   { tName = "OrderQty2"
   , tnum = 192
   , tparser = toFIXQuantity }

tFutSettDate2 :: FIXTag
tFutSettDate2 = FIXTag 
   { tName = "FutSettDate2"
   , tnum = 193
   , tparser = toFIXLocalMktDate }

tLastSpotRate :: FIXTag
tLastSpotRate = FIXTag 
   { tName = "LastSpotRate"
   , tnum = 194
   , tparser = toFIXPrice }

tLastForwardPoints :: FIXTag
tLastForwardPoints = FIXTag 
   { tName = "LastForwardPoints"
   , tnum = 195
   , tparser = toFIXPriceOffset }

tAllocLinkID :: FIXTag
tAllocLinkID = FIXTag 
   { tName = "AllocLinkID"
   , tnum = 196
   , tparser = toFIXString }

tAllocLinkType :: FIXTag
tAllocLinkType = FIXTag 
   { tName = "AllocLinkType"
   , tnum = 197
   , tparser = toFIXInt }

tSecondaryOrderID :: FIXTag
tSecondaryOrderID = FIXTag 
   { tName = "SecondaryOrderID"
   , tnum = 198
   , tparser = toFIXString }

tNoIOIQualifiers :: FIXTag
tNoIOIQualifiers = FIXTag 
   { tName = "NoIOIQualifiers"
   , tnum = 199
   , tparser = toFIXInt }

tMaturityMonthYear :: FIXTag
tMaturityMonthYear = FIXTag 
   { tName = "MaturityMonthYear"
   , tnum = 200
   , tparser = toFIXMonthYear }

tPutOrCall :: FIXTag
tPutOrCall = FIXTag 
   { tName = "PutOrCall"
   , tnum = 201
   , tparser = toFIXInt }

tStrikePrice :: FIXTag
tStrikePrice = FIXTag 
   { tName = "StrikePrice"
   , tnum = 202
   , tparser = toFIXPrice }

tCoveredOrUncovered :: FIXTag
tCoveredOrUncovered = FIXTag 
   { tName = "CoveredOrUncovered"
   , tnum = 203
   , tparser = toFIXInt }

tCustomerOrFirm :: FIXTag
tCustomerOrFirm = FIXTag 
   { tName = "CustomerOrFirm"
   , tnum = 204
   , tparser = toFIXInt }

tMaturityDay :: FIXTag
tMaturityDay = FIXTag 
   { tName = "MaturityDay"
   , tnum = 205
   , tparser = toFIXDayOfMonth }

tOptAttribute :: FIXTag
tOptAttribute = FIXTag 
   { tName = "OptAttribute"
   , tnum = 206
   , tparser = toFIXChar }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag 
   { tName = "SecurityExchange"
   , tnum = 207
   , tparser = toFIXExchange }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag 
   { tName = "NotifyBrokerOfCredit"
   , tnum = 208
   , tparser = toFIXBool }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag 
   { tName = "AllocHandlInst"
   , tnum = 209
   , tparser = toFIXInt }

tMaxShow :: FIXTag
tMaxShow = FIXTag 
   { tName = "MaxShow"
   , tnum = 210
   , tparser = toFIXQuantity }

tPegDifference :: FIXTag
tPegDifference = FIXTag 
   { tName = "PegDifference"
   , tnum = 211
   , tparser = toFIXPriceOffset }

tXmlDataLen :: FIXTag
tXmlDataLen = FIXTag 
   { tName = "XmlDataLen"
   , tnum = 212
   , tparser = toFIXDataLen }

tXmlData :: FIXTag
tXmlData = FIXTag 
   { tName = "XmlData"
   , tnum = 213
   , tparser = toFIXData }

tSettlInstRefID :: FIXTag
tSettlInstRefID = FIXTag 
   { tName = "SettlInstRefID"
   , tnum = 214
   , tparser = toFIXString }

tNoRoutingIDs :: FIXTag
tNoRoutingIDs = FIXTag 
   { tName = "NoRoutingIDs"
   , tnum = 215
   , tparser = toFIXInt }

tRoutingType :: FIXTag
tRoutingType = FIXTag 
   { tName = "RoutingType"
   , tnum = 216
   , tparser = toFIXInt }

tRoutingID :: FIXTag
tRoutingID = FIXTag 
   { tName = "RoutingID"
   , tnum = 217
   , tparser = toFIXString }

tSpreadToBenchmark :: FIXTag
tSpreadToBenchmark = FIXTag 
   { tName = "SpreadToBenchmark"
   , tnum = 218
   , tparser = toFIXPriceOffset }

tBenchmark :: FIXTag
tBenchmark = FIXTag 
   { tName = "Benchmark"
   , tnum = 219
   , tparser = toFIXChar }

tCouponRate :: FIXTag
tCouponRate = FIXTag 
   { tName = "CouponRate"
   , tnum = 223
   , tparser = toFIXFloat }

tContractMultiplier :: FIXTag
tContractMultiplier = FIXTag 
   { tName = "ContractMultiplier"
   , tnum = 231
   , tparser = toFIXFloat }

tMDReqID :: FIXTag
tMDReqID = FIXTag 
   { tName = "MDReqID"
   , tnum = 262
   , tparser = toFIXString }

tSubscriptionRequestType :: FIXTag
tSubscriptionRequestType = FIXTag 
   { tName = "SubscriptionRequestType"
   , tnum = 263
   , tparser = toFIXChar }

tMarketDepth :: FIXTag
tMarketDepth = FIXTag 
   { tName = "MarketDepth"
   , tnum = 264
   , tparser = toFIXInt }

tMDUpdateType :: FIXTag
tMDUpdateType = FIXTag 
   { tName = "MDUpdateType"
   , tnum = 265
   , tparser = toFIXInt }

tAggregatedBook :: FIXTag
tAggregatedBook = FIXTag 
   { tName = "AggregatedBook"
   , tnum = 266
   , tparser = toFIXBool }

tNoMDEntryTypes :: FIXTag
tNoMDEntryTypes = FIXTag 
   { tName = "NoMDEntryTypes"
   , tnum = 267
   , tparser = toFIXInt }

tNoMDEntries :: FIXTag
tNoMDEntries = FIXTag 
   { tName = "NoMDEntries"
   , tnum = 268
   , tparser = toFIXInt }

tMDEntryType :: FIXTag
tMDEntryType = FIXTag 
   { tName = "MDEntryType"
   , tnum = 269
   , tparser = toFIXChar }

tMDEntryPx :: FIXTag
tMDEntryPx = FIXTag 
   { tName = "MDEntryPx"
   , tnum = 270
   , tparser = toFIXPrice }

tMDEntrySize :: FIXTag
tMDEntrySize = FIXTag 
   { tName = "MDEntrySize"
   , tnum = 271
   , tparser = toFIXQuantity }

tMDEntryDate :: FIXTag
tMDEntryDate = FIXTag 
   { tName = "MDEntryDate"
   , tnum = 272
   , tparser = toFIXUTCDate }

tMDEntryTime :: FIXTag
tMDEntryTime = FIXTag 
   { tName = "MDEntryTime"
   , tnum = 273
   , tparser = toFIXUTCTimeOnly }

tTickDirection :: FIXTag
tTickDirection = FIXTag 
   { tName = "TickDirection"
   , tnum = 274
   , tparser = toFIXChar }

tMDMkt :: FIXTag
tMDMkt = FIXTag 
   { tName = "MDMkt"
   , tnum = 275
   , tparser = toFIXExchange }

tQuoteCondition :: FIXTag
tQuoteCondition = FIXTag 
   { tName = "QuoteCondition"
   , tnum = 276
   , tparser = toFIXMultipleValueString }

tTradeCondition :: FIXTag
tTradeCondition = FIXTag 
   { tName = "TradeCondition"
   , tnum = 277
   , tparser = toFIXMultipleValueString }

tMDEntryID :: FIXTag
tMDEntryID = FIXTag 
   { tName = "MDEntryID"
   , tnum = 278
   , tparser = toFIXString }

tMDUpdateAction :: FIXTag
tMDUpdateAction = FIXTag 
   { tName = "MDUpdateAction"
   , tnum = 279
   , tparser = toFIXChar }

tMDEntryRefID :: FIXTag
tMDEntryRefID = FIXTag 
   { tName = "MDEntryRefID"
   , tnum = 280
   , tparser = toFIXString }

tMDReqRejReason :: FIXTag
tMDReqRejReason = FIXTag 
   { tName = "MDReqRejReason"
   , tnum = 281
   , tparser = toFIXChar }

tMDEntryOriginator :: FIXTag
tMDEntryOriginator = FIXTag 
   { tName = "MDEntryOriginator"
   , tnum = 282
   , tparser = toFIXString }

tLocationID :: FIXTag
tLocationID = FIXTag 
   { tName = "LocationID"
   , tnum = 283
   , tparser = toFIXString }

tDeskID :: FIXTag
tDeskID = FIXTag 
   { tName = "DeskID"
   , tnum = 284
   , tparser = toFIXString }

tDeleteReason :: FIXTag
tDeleteReason = FIXTag 
   { tName = "DeleteReason"
   , tnum = 285
   , tparser = toFIXChar }

tOpenCloseSettleFlag :: FIXTag
tOpenCloseSettleFlag = FIXTag 
   { tName = "OpenCloseSettleFlag"
   , tnum = 286
   , tparser = toFIXChar }

tSellerDays :: FIXTag
tSellerDays = FIXTag 
   { tName = "SellerDays"
   , tnum = 287
   , tparser = toFIXInt }

tMDEntryBuyer :: FIXTag
tMDEntryBuyer = FIXTag 
   { tName = "MDEntryBuyer"
   , tnum = 288
   , tparser = toFIXString }

tMDEntrySeller :: FIXTag
tMDEntrySeller = FIXTag 
   { tName = "MDEntrySeller"
   , tnum = 289
   , tparser = toFIXString }

tMDEntryPositionNo :: FIXTag
tMDEntryPositionNo = FIXTag 
   { tName = "MDEntryPositionNo"
   , tnum = 290
   , tparser = toFIXInt }

tFinancialStatus :: FIXTag
tFinancialStatus = FIXTag 
   { tName = "FinancialStatus"
   , tnum = 291
   , tparser = toFIXChar }

tCorporateAction :: FIXTag
tCorporateAction = FIXTag 
   { tName = "CorporateAction"
   , tnum = 292
   , tparser = toFIXChar }

tDefBidSize :: FIXTag
tDefBidSize = FIXTag 
   { tName = "DefBidSize"
   , tnum = 293
   , tparser = toFIXQuantity }

tDefOfferSize :: FIXTag
tDefOfferSize = FIXTag 
   { tName = "DefOfferSize"
   , tnum = 294
   , tparser = toFIXQuantity }

tNoQuoteEntries :: FIXTag
tNoQuoteEntries = FIXTag 
   { tName = "NoQuoteEntries"
   , tnum = 295
   , tparser = toFIXInt }

tNoQuoteSets :: FIXTag
tNoQuoteSets = FIXTag 
   { tName = "NoQuoteSets"
   , tnum = 296
   , tparser = toFIXInt }

tQuoteAckStatus :: FIXTag
tQuoteAckStatus = FIXTag 
   { tName = "QuoteAckStatus"
   , tnum = 297
   , tparser = toFIXInt }

tQuoteCancelType :: FIXTag
tQuoteCancelType = FIXTag 
   { tName = "QuoteCancelType"
   , tnum = 298
   , tparser = toFIXInt }

tQuoteEntryID :: FIXTag
tQuoteEntryID = FIXTag 
   { tName = "QuoteEntryID"
   , tnum = 299
   , tparser = toFIXString }

tQuoteRejectReason :: FIXTag
tQuoteRejectReason = FIXTag 
   { tName = "QuoteRejectReason"
   , tnum = 300
   , tparser = toFIXInt }

tQuoteResponseLevel :: FIXTag
tQuoteResponseLevel = FIXTag 
   { tName = "QuoteResponseLevel"
   , tnum = 301
   , tparser = toFIXInt }

tQuoteSetID :: FIXTag
tQuoteSetID = FIXTag 
   { tName = "QuoteSetID"
   , tnum = 302
   , tparser = toFIXString }

tQuoteRequestType :: FIXTag
tQuoteRequestType = FIXTag 
   { tName = "QuoteRequestType"
   , tnum = 303
   , tparser = toFIXInt }

tTotQuoteEntries :: FIXTag
tTotQuoteEntries = FIXTag 
   { tName = "TotQuoteEntries"
   , tnum = 304
   , tparser = toFIXInt }

tUnderlyingIDSource :: FIXTag
tUnderlyingIDSource = FIXTag 
   { tName = "UnderlyingIDSource"
   , tnum = 305
   , tparser = toFIXString }

tUnderlyingIssuer :: FIXTag
tUnderlyingIssuer = FIXTag 
   { tName = "UnderlyingIssuer"
   , tnum = 306
   , tparser = toFIXString }

tUnderlyingSecurityDesc :: FIXTag
tUnderlyingSecurityDesc = FIXTag 
   { tName = "UnderlyingSecurityDesc"
   , tnum = 307
   , tparser = toFIXString }

tUnderlyingSecurityExchange :: FIXTag
tUnderlyingSecurityExchange = FIXTag 
   { tName = "UnderlyingSecurityExchange"
   , tnum = 308
   , tparser = toFIXExchange }

tUnderlyingSecurityID :: FIXTag
tUnderlyingSecurityID = FIXTag 
   { tName = "UnderlyingSecurityID"
   , tnum = 309
   , tparser = toFIXString }

tUnderlyingSecurityType :: FIXTag
tUnderlyingSecurityType = FIXTag 
   { tName = "UnderlyingSecurityType"
   , tnum = 310
   , tparser = toFIXString }

tUnderlyingSymbol :: FIXTag
tUnderlyingSymbol = FIXTag 
   { tName = "UnderlyingSymbol"
   , tnum = 311
   , tparser = toFIXString }

tUnderlyingSymbolSfx :: FIXTag
tUnderlyingSymbolSfx = FIXTag 
   { tName = "UnderlyingSymbolSfx"
   , tnum = 312
   , tparser = toFIXString }

tUnderlyingMaturityMonthYear :: FIXTag
tUnderlyingMaturityMonthYear = FIXTag 
   { tName = "UnderlyingMaturityMonthYear"
   , tnum = 313
   , tparser = toFIXMonthYear }

tUnderlyingMaturityDay :: FIXTag
tUnderlyingMaturityDay = FIXTag 
   { tName = "UnderlyingMaturityDay"
   , tnum = 314
   , tparser = toFIXDayOfMonth }

tUnderlyingPutOrCall :: FIXTag
tUnderlyingPutOrCall = FIXTag 
   { tName = "UnderlyingPutOrCall"
   , tnum = 315
   , tparser = toFIXInt }

tUnderlyingStrikePrice :: FIXTag
tUnderlyingStrikePrice = FIXTag 
   { tName = "UnderlyingStrikePrice"
   , tnum = 316
   , tparser = toFIXPrice }

tUnderlyingOptAttribute :: FIXTag
tUnderlyingOptAttribute = FIXTag 
   { tName = "UnderlyingOptAttribute"
   , tnum = 317
   , tparser = toFIXChar }

tUnderlyingCurrency :: FIXTag
tUnderlyingCurrency = FIXTag 
   { tName = "UnderlyingCurrency"
   , tnum = 318
   , tparser = toFIXCurrency }

tRatioQty :: FIXTag
tRatioQty = FIXTag 
   { tName = "RatioQty"
   , tnum = 319
   , tparser = toFIXQuantity }

tSecurityReqID :: FIXTag
tSecurityReqID = FIXTag 
   { tName = "SecurityReqID"
   , tnum = 320
   , tparser = toFIXString }

tSecurityRequestType :: FIXTag
tSecurityRequestType = FIXTag 
   { tName = "SecurityRequestType"
   , tnum = 321
   , tparser = toFIXInt }

tSecurityResponseID :: FIXTag
tSecurityResponseID = FIXTag 
   { tName = "SecurityResponseID"
   , tnum = 322
   , tparser = toFIXString }

tSecurityResponseType :: FIXTag
tSecurityResponseType = FIXTag 
   { tName = "SecurityResponseType"
   , tnum = 323
   , tparser = toFIXInt }

tSecurityStatusReqID :: FIXTag
tSecurityStatusReqID = FIXTag 
   { tName = "SecurityStatusReqID"
   , tnum = 324
   , tparser = toFIXString }

tUnsolicitedIndicator :: FIXTag
tUnsolicitedIndicator = FIXTag 
   { tName = "UnsolicitedIndicator"
   , tnum = 325
   , tparser = toFIXBool }

tSecurityTradingStatus :: FIXTag
tSecurityTradingStatus = FIXTag 
   { tName = "SecurityTradingStatus"
   , tnum = 326
   , tparser = toFIXInt }

tHaltReasonChar :: FIXTag
tHaltReasonChar = FIXTag 
   { tName = "HaltReasonChar"
   , tnum = 327
   , tparser = toFIXChar }

tInViewOfCommon :: FIXTag
tInViewOfCommon = FIXTag 
   { tName = "InViewOfCommon"
   , tnum = 328
   , tparser = toFIXBool }

tDueToRelated :: FIXTag
tDueToRelated = FIXTag 
   { tName = "DueToRelated"
   , tnum = 329
   , tparser = toFIXBool }

tBuyVolume :: FIXTag
tBuyVolume = FIXTag 
   { tName = "BuyVolume"
   , tnum = 330
   , tparser = toFIXQuantity }

tSellVolume :: FIXTag
tSellVolume = FIXTag 
   { tName = "SellVolume"
   , tnum = 331
   , tparser = toFIXQuantity }

tHighPx :: FIXTag
tHighPx = FIXTag 
   { tName = "HighPx"
   , tnum = 332
   , tparser = toFIXPrice }

tLowPx :: FIXTag
tLowPx = FIXTag 
   { tName = "LowPx"
   , tnum = 333
   , tparser = toFIXPrice }

tAdjustment :: FIXTag
tAdjustment = FIXTag 
   { tName = "Adjustment"
   , tnum = 334
   , tparser = toFIXInt }

tTradSesReqID :: FIXTag
tTradSesReqID = FIXTag 
   { tName = "TradSesReqID"
   , tnum = 335
   , tparser = toFIXString }

tTradingSessionID :: FIXTag
tTradingSessionID = FIXTag 
   { tName = "TradingSessionID"
   , tnum = 336
   , tparser = toFIXString }

tContraTrader :: FIXTag
tContraTrader = FIXTag 
   { tName = "ContraTrader"
   , tnum = 337
   , tparser = toFIXString }

tTradSesMethod :: FIXTag
tTradSesMethod = FIXTag 
   { tName = "TradSesMethod"
   , tnum = 338
   , tparser = toFIXInt }

tTradSesMode :: FIXTag
tTradSesMode = FIXTag 
   { tName = "TradSesMode"
   , tnum = 339
   , tparser = toFIXInt }

tTradSesStatus :: FIXTag
tTradSesStatus = FIXTag 
   { tName = "TradSesStatus"
   , tnum = 340
   , tparser = toFIXInt }

tTradSesStartTime :: FIXTag
tTradSesStartTime = FIXTag 
   { tName = "TradSesStartTime"
   , tnum = 341
   , tparser = toFIXUTCTimestamp }

tTradSesOpenTime :: FIXTag
tTradSesOpenTime = FIXTag 
   { tName = "TradSesOpenTime"
   , tnum = 342
   , tparser = toFIXUTCTimestamp }

tTradSesPreCloseTime :: FIXTag
tTradSesPreCloseTime = FIXTag 
   { tName = "TradSesPreCloseTime"
   , tnum = 343
   , tparser = toFIXUTCTimestamp }

tTradSesCloseTime :: FIXTag
tTradSesCloseTime = FIXTag 
   { tName = "TradSesCloseTime"
   , tnum = 344
   , tparser = toFIXUTCTimestamp }

tTradSesEndTime :: FIXTag
tTradSesEndTime = FIXTag 
   { tName = "TradSesEndTime"
   , tnum = 345
   , tparser = toFIXUTCTimestamp }

tNumberOfOrders :: FIXTag
tNumberOfOrders = FIXTag 
   { tName = "NumberOfOrders"
   , tnum = 346
   , tparser = toFIXInt }

tMessageEncoding :: FIXTag
tMessageEncoding = FIXTag 
   { tName = "MessageEncoding"
   , tnum = 347
   , tparser = toFIXString }

tEncodedIssuerLen :: FIXTag
tEncodedIssuerLen = FIXTag 
   { tName = "EncodedIssuerLen"
   , tnum = 348
   , tparser = toFIXDataLen }

tEncodedIssuer :: FIXTag
tEncodedIssuer = FIXTag 
   { tName = "EncodedIssuer"
   , tnum = 349
   , tparser = toFIXData }

tEncodedSecurityDescLen :: FIXTag
tEncodedSecurityDescLen = FIXTag 
   { tName = "EncodedSecurityDescLen"
   , tnum = 350
   , tparser = toFIXDataLen }

tEncodedSecurityDesc :: FIXTag
tEncodedSecurityDesc = FIXTag 
   { tName = "EncodedSecurityDesc"
   , tnum = 351
   , tparser = toFIXData }

tEncodedListExecInstLen :: FIXTag
tEncodedListExecInstLen = FIXTag 
   { tName = "EncodedListExecInstLen"
   , tnum = 352
   , tparser = toFIXDataLen }

tEncodedListExecInst :: FIXTag
tEncodedListExecInst = FIXTag 
   { tName = "EncodedListExecInst"
   , tnum = 353
   , tparser = toFIXData }

tEncodedTextLen :: FIXTag
tEncodedTextLen = FIXTag 
   { tName = "EncodedTextLen"
   , tnum = 354
   , tparser = toFIXDataLen }

tEncodedText :: FIXTag
tEncodedText = FIXTag 
   { tName = "EncodedText"
   , tnum = 355
   , tparser = toFIXData }

tEncodedSubjectLen :: FIXTag
tEncodedSubjectLen = FIXTag 
   { tName = "EncodedSubjectLen"
   , tnum = 356
   , tparser = toFIXDataLen }

tEncodedSubject :: FIXTag
tEncodedSubject = FIXTag 
   { tName = "EncodedSubject"
   , tnum = 357
   , tparser = toFIXData }

tEncodedHeadlineLen :: FIXTag
tEncodedHeadlineLen = FIXTag 
   { tName = "EncodedHeadlineLen"
   , tnum = 358
   , tparser = toFIXDataLen }

tEncodedHeadline :: FIXTag
tEncodedHeadline = FIXTag 
   { tName = "EncodedHeadline"
   , tnum = 359
   , tparser = toFIXData }

tEncodedAllocTextLen :: FIXTag
tEncodedAllocTextLen = FIXTag 
   { tName = "EncodedAllocTextLen"
   , tnum = 360
   , tparser = toFIXDataLen }

tEncodedAllocText :: FIXTag
tEncodedAllocText = FIXTag 
   { tName = "EncodedAllocText"
   , tnum = 361
   , tparser = toFIXData }

tEncodedUnderlyingIssuerLen :: FIXTag
tEncodedUnderlyingIssuerLen = FIXTag 
   { tName = "EncodedUnderlyingIssuerLen"
   , tnum = 362
   , tparser = toFIXDataLen }

tEncodedUnderlyingIssuer :: FIXTag
tEncodedUnderlyingIssuer = FIXTag 
   { tName = "EncodedUnderlyingIssuer"
   , tnum = 363
   , tparser = toFIXData }

tEncodedUnderlyingSecurityDescLen :: FIXTag
tEncodedUnderlyingSecurityDescLen = FIXTag 
   { tName = "EncodedUnderlyingSecurityDescLen"
   , tnum = 364
   , tparser = toFIXDataLen }

tEncodedUnderlyingSecurityDesc :: FIXTag
tEncodedUnderlyingSecurityDesc = FIXTag 
   { tName = "EncodedUnderlyingSecurityDesc"
   , tnum = 365
   , tparser = toFIXData }

tAllocPrice :: FIXTag
tAllocPrice = FIXTag 
   { tName = "AllocPrice"
   , tnum = 366
   , tparser = toFIXPrice }

tQuoteSetValidUntilTime :: FIXTag
tQuoteSetValidUntilTime = FIXTag 
   { tName = "QuoteSetValidUntilTime"
   , tnum = 367
   , tparser = toFIXUTCTimestamp }

tQuoteEntryRejectReason :: FIXTag
tQuoteEntryRejectReason = FIXTag 
   { tName = "QuoteEntryRejectReason"
   , tnum = 368
   , tparser = toFIXInt }

tLastMsgSeqNumProcessed :: FIXTag
tLastMsgSeqNumProcessed = FIXTag 
   { tName = "LastMsgSeqNumProcessed"
   , tnum = 369
   , tparser = toFIXInt }

tOnBehalfOfSendingTime :: FIXTag
tOnBehalfOfSendingTime = FIXTag 
   { tName = "OnBehalfOfSendingTime"
   , tnum = 370
   , tparser = toFIXUTCTimestamp }

tRefTagID :: FIXTag
tRefTagID = FIXTag 
   { tName = "RefTagID"
   , tnum = 371
   , tparser = toFIXInt }

tRefMsgType :: FIXTag
tRefMsgType = FIXTag 
   { tName = "RefMsgType"
   , tnum = 372
   , tparser = toFIXString }

tSessionRejectReason :: FIXTag
tSessionRejectReason = FIXTag 
   { tName = "SessionRejectReason"
   , tnum = 373
   , tparser = toFIXInt }

tBidRequestTransType :: FIXTag
tBidRequestTransType = FIXTag 
   { tName = "BidRequestTransType"
   , tnum = 374
   , tparser = toFIXChar }

tContraBroker :: FIXTag
tContraBroker = FIXTag 
   { tName = "ContraBroker"
   , tnum = 375
   , tparser = toFIXString }

tComplianceID :: FIXTag
tComplianceID = FIXTag 
   { tName = "ComplianceID"
   , tnum = 376
   , tparser = toFIXString }

tSolicitedFlag :: FIXTag
tSolicitedFlag = FIXTag 
   { tName = "SolicitedFlag"
   , tnum = 377
   , tparser = toFIXBool }

tExecRestatementReason :: FIXTag
tExecRestatementReason = FIXTag 
   { tName = "ExecRestatementReason"
   , tnum = 378
   , tparser = toFIXInt }

tBusinessRejectRefID :: FIXTag
tBusinessRejectRefID = FIXTag 
   { tName = "BusinessRejectRefID"
   , tnum = 379
   , tparser = toFIXString }

tBusinessRejectReason :: FIXTag
tBusinessRejectReason = FIXTag 
   { tName = "BusinessRejectReason"
   , tnum = 380
   , tparser = toFIXInt }

tGrossTradeAmt :: FIXTag
tGrossTradeAmt = FIXTag 
   { tName = "GrossTradeAmt"
   , tnum = 381
   , tparser = toFIXAmt }

tNoContraBrokers :: FIXTag
tNoContraBrokers = FIXTag 
   { tName = "NoContraBrokers"
   , tnum = 382
   , tparser = toFIXInt }

tMaxMessageSize :: FIXTag
tMaxMessageSize = FIXTag 
   { tName = "MaxMessageSize"
   , tnum = 383
   , tparser = toFIXInt }

tNoMsgTypes :: FIXTag
tNoMsgTypes = FIXTag 
   { tName = "NoMsgTypes"
   , tnum = 384
   , tparser = toFIXInt }

tMsgDirection :: FIXTag
tMsgDirection = FIXTag 
   { tName = "MsgDirection"
   , tnum = 385
   , tparser = toFIXChar }

tNoTradingSessions :: FIXTag
tNoTradingSessions = FIXTag 
   { tName = "NoTradingSessions"
   , tnum = 386
   , tparser = toFIXInt }

tTotalVolumeTraded :: FIXTag
tTotalVolumeTraded = FIXTag 
   { tName = "TotalVolumeTraded"
   , tnum = 387
   , tparser = toFIXQuantity }

tDiscretionInst :: FIXTag
tDiscretionInst = FIXTag 
   { tName = "DiscretionInst"
   , tnum = 388
   , tparser = toFIXChar }

tDiscretionOffset :: FIXTag
tDiscretionOffset = FIXTag 
   { tName = "DiscretionOffset"
   , tnum = 389
   , tparser = toFIXPriceOffset }

tBidID :: FIXTag
tBidID = FIXTag 
   { tName = "BidID"
   , tnum = 390
   , tparser = toFIXString }

tClientBidID :: FIXTag
tClientBidID = FIXTag 
   { tName = "ClientBidID"
   , tnum = 391
   , tparser = toFIXString }

tListName :: FIXTag
tListName = FIXTag 
   { tName = "ListName"
   , tnum = 392
   , tparser = toFIXString }

tTotalNumSecurities :: FIXTag
tTotalNumSecurities = FIXTag 
   { tName = "TotalNumSecurities"
   , tnum = 393
   , tparser = toFIXInt }

tBidType :: FIXTag
tBidType = FIXTag 
   { tName = "BidType"
   , tnum = 394
   , tparser = toFIXInt }

tNumTickets :: FIXTag
tNumTickets = FIXTag 
   { tName = "NumTickets"
   , tnum = 395
   , tparser = toFIXInt }

tSideValue1 :: FIXTag
tSideValue1 = FIXTag 
   { tName = "SideValue1"
   , tnum = 396
   , tparser = toFIXAmt }

tSideValue2 :: FIXTag
tSideValue2 = FIXTag 
   { tName = "SideValue2"
   , tnum = 397
   , tparser = toFIXAmt }

tNoBidDescriptors :: FIXTag
tNoBidDescriptors = FIXTag 
   { tName = "NoBidDescriptors"
   , tnum = 398
   , tparser = toFIXInt }

tBidDescriptorType :: FIXTag
tBidDescriptorType = FIXTag 
   { tName = "BidDescriptorType"
   , tnum = 399
   , tparser = toFIXInt }

tBidDescriptor :: FIXTag
tBidDescriptor = FIXTag 
   { tName = "BidDescriptor"
   , tnum = 400
   , tparser = toFIXString }

tSideValueInd :: FIXTag
tSideValueInd = FIXTag 
   { tName = "SideValueInd"
   , tnum = 401
   , tparser = toFIXInt }

tLiquidityPctLow :: FIXTag
tLiquidityPctLow = FIXTag 
   { tName = "LiquidityPctLow"
   , tnum = 402
   , tparser = toFIXFloat }

tLiquidityPctHigh :: FIXTag
tLiquidityPctHigh = FIXTag 
   { tName = "LiquidityPctHigh"
   , tnum = 403
   , tparser = toFIXFloat }

tLiquidityValue :: FIXTag
tLiquidityValue = FIXTag 
   { tName = "LiquidityValue"
   , tnum = 404
   , tparser = toFIXAmt }

tEFPTrackingError :: FIXTag
tEFPTrackingError = FIXTag 
   { tName = "EFPTrackingError"
   , tnum = 405
   , tparser = toFIXFloat }

tFairValue :: FIXTag
tFairValue = FIXTag 
   { tName = "FairValue"
   , tnum = 406
   , tparser = toFIXAmt }

tOutsideIndexPct :: FIXTag
tOutsideIndexPct = FIXTag 
   { tName = "OutsideIndexPct"
   , tnum = 407
   , tparser = toFIXFloat }

tValueOfFutures :: FIXTag
tValueOfFutures = FIXTag 
   { tName = "ValueOfFutures"
   , tnum = 408
   , tparser = toFIXAmt }

tLiquidityIndType :: FIXTag
tLiquidityIndType = FIXTag 
   { tName = "LiquidityIndType"
   , tnum = 409
   , tparser = toFIXInt }

tWtAverageLiquidity :: FIXTag
tWtAverageLiquidity = FIXTag 
   { tName = "WtAverageLiquidity"
   , tnum = 410
   , tparser = toFIXFloat }

tExchangeForPhysical :: FIXTag
tExchangeForPhysical = FIXTag 
   { tName = "ExchangeForPhysical"
   , tnum = 411
   , tparser = toFIXBool }

tOutMainCntryUIndex :: FIXTag
tOutMainCntryUIndex = FIXTag 
   { tName = "OutMainCntryUIndex"
   , tnum = 412
   , tparser = toFIXAmt }

tCrossPercent :: FIXTag
tCrossPercent = FIXTag 
   { tName = "CrossPercent"
   , tnum = 413
   , tparser = toFIXFloat }

tProgRptReqs :: FIXTag
tProgRptReqs = FIXTag 
   { tName = "ProgRptReqs"
   , tnum = 414
   , tparser = toFIXInt }

tProgPeriodInterval :: FIXTag
tProgPeriodInterval = FIXTag 
   { tName = "ProgPeriodInterval"
   , tnum = 415
   , tparser = toFIXInt }

tIncTaxInd :: FIXTag
tIncTaxInd = FIXTag 
   { tName = "IncTaxInd"
   , tnum = 416
   , tparser = toFIXInt }

tNumBidders :: FIXTag
tNumBidders = FIXTag 
   { tName = "NumBidders"
   , tnum = 417
   , tparser = toFIXInt }

tTradeType :: FIXTag
tTradeType = FIXTag 
   { tName = "TradeType"
   , tnum = 418
   , tparser = toFIXChar }

tBasisPxType :: FIXTag
tBasisPxType = FIXTag 
   { tName = "BasisPxType"
   , tnum = 419
   , tparser = toFIXChar }

tNoBidComponents :: FIXTag
tNoBidComponents = FIXTag 
   { tName = "NoBidComponents"
   , tnum = 420
   , tparser = toFIXInt }

tCountry :: FIXTag
tCountry = FIXTag 
   { tName = "Country"
   , tnum = 421
   , tparser = toFIXString }

tTotNoStrikes :: FIXTag
tTotNoStrikes = FIXTag 
   { tName = "TotNoStrikes"
   , tnum = 422
   , tparser = toFIXInt }

tPriceType :: FIXTag
tPriceType = FIXTag 
   { tName = "PriceType"
   , tnum = 423
   , tparser = toFIXInt }

tDayOrderQty :: FIXTag
tDayOrderQty = FIXTag 
   { tName = "DayOrderQty"
   , tnum = 424
   , tparser = toFIXQuantity }

tDayCumQty :: FIXTag
tDayCumQty = FIXTag 
   { tName = "DayCumQty"
   , tnum = 425
   , tparser = toFIXQuantity }

tDayAvgPx :: FIXTag
tDayAvgPx = FIXTag 
   { tName = "DayAvgPx"
   , tnum = 426
   , tparser = toFIXPrice }

tGTBookingInst :: FIXTag
tGTBookingInst = FIXTag 
   { tName = "GTBookingInst"
   , tnum = 427
   , tparser = toFIXInt }

tNoStrikes :: FIXTag
tNoStrikes = FIXTag 
   { tName = "NoStrikes"
   , tnum = 428
   , tparser = toFIXInt }

tListStatusType :: FIXTag
tListStatusType = FIXTag 
   { tName = "ListStatusType"
   , tnum = 429
   , tparser = toFIXInt }

tNetGrossInd :: FIXTag
tNetGrossInd = FIXTag 
   { tName = "NetGrossInd"
   , tnum = 430
   , tparser = toFIXInt }

tListOrderStatus :: FIXTag
tListOrderStatus = FIXTag 
   { tName = "ListOrderStatus"
   , tnum = 431
   , tparser = toFIXInt }

tExpireDate :: FIXTag
tExpireDate = FIXTag 
   { tName = "ExpireDate"
   , tnum = 432
   , tparser = toFIXLocalMktDate }

tListExecInstType :: FIXTag
tListExecInstType = FIXTag 
   { tName = "ListExecInstType"
   , tnum = 433
   , tparser = toFIXChar }

tCxlRejResponseTo :: FIXTag
tCxlRejResponseTo = FIXTag 
   { tName = "CxlRejResponseTo"
   , tnum = 434
   , tparser = toFIXChar }

tUnderlyingCouponRate :: FIXTag
tUnderlyingCouponRate = FIXTag 
   { tName = "UnderlyingCouponRate"
   , tnum = 435
   , tparser = toFIXFloat }

tUnderlyingContractMultiplier :: FIXTag
tUnderlyingContractMultiplier = FIXTag 
   { tName = "UnderlyingContractMultiplier"
   , tnum = 436
   , tparser = toFIXFloat }

tContraTradeQty :: FIXTag
tContraTradeQty = FIXTag 
   { tName = "ContraTradeQty"
   , tnum = 437
   , tparser = toFIXQuantity }

tContraTradeTime :: FIXTag
tContraTradeTime = FIXTag 
   { tName = "ContraTradeTime"
   , tnum = 438
   , tparser = toFIXUTCTimestamp }

tClearingFirm :: FIXTag
tClearingFirm = FIXTag 
   { tName = "ClearingFirm"
   , tnum = 439
   , tparser = toFIXString }

tClearingAccount :: FIXTag
tClearingAccount = FIXTag 
   { tName = "ClearingAccount"
   , tnum = 440
   , tparser = toFIXString }

tLiquidityNumSecurities :: FIXTag
tLiquidityNumSecurities = FIXTag 
   { tName = "LiquidityNumSecurities"
   , tnum = 441
   , tparser = toFIXInt }

tMultiLegReportingType :: FIXTag
tMultiLegReportingType = FIXTag 
   { tName = "MultiLegReportingType"
   , tnum = 442
   , tparser = toFIXChar }

tStrikeTime :: FIXTag
tStrikeTime = FIXTag 
   { tName = "StrikeTime"
   , tnum = 443
   , tparser = toFIXUTCTimestamp }

tListStatusText :: FIXTag
tListStatusText = FIXTag 
   { tName = "ListStatusText"
   , tnum = 444
   , tparser = toFIXString }

tEncodedListStatusTextLen :: FIXTag
tEncodedListStatusTextLen = FIXTag 
   { tName = "EncodedListStatusTextLen"
   , tnum = 445
   , tparser = toFIXDataLen }

tEncodedListStatusText :: FIXTag
tEncodedListStatusText = FIXTag 
   { tName = "EncodedListStatusText"
   , tnum = 446
   , tparser = toFIXData }

headerFIX42 :: FIXTags
headerFIX42 = 
   LT.insert (tnum tBeginString) tBeginString $
   LT.insert (tnum tBodyLength) tBodyLength $
   LT.insert (tnum tMsgType) tMsgType $
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
   LT.insert (tnum tOnBehalfOfSendingTime) tOnBehalfOfSendingTime    LT.new


trailerFIX42 :: FIXTags
trailerFIX42 = 
   LT.insert (tnum tSignatureLength) tSignatureLength $
   LT.insert (tnum tSignature) tSignature $
   LT.insert (tnum tCheckSum) tCheckSum    LT.new


mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { msName = "Heartbeat"
   , msType = C.pack "0"
   , msHeader = headerFIX42
   , msBody = mHeartbeatBody
   , msTrailer = trailerFIX42 }
   where
   mHeartbeatBody = 
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { msName = "TestRequest"
   , msType = C.pack "1"
   , msHeader = headerFIX42
   , msBody = mTestRequestBody
   , msTrailer = trailerFIX42 }
   where
   mTestRequestBody = 
      LT.insert (tnum tTestReqID) tTestReqID       LT.new


mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { msName = "ResendRequest"
   , msType = C.pack "2"
   , msHeader = headerFIX42
   , msBody = mResendRequestBody
   , msTrailer = trailerFIX42 }
   where
   mResendRequestBody = 
      LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
      LT.insert (tnum tEndSeqNo) tEndSeqNo       LT.new


mReject :: FIXMessageSpec
mReject = FMSpec
   { msName = "Reject"
   , msType = C.pack "3"
   , msHeader = headerFIX42
   , msBody = mRejectBody
   , msTrailer = trailerFIX42 }
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
   , msHeader = headerFIX42
   , msBody = mSequenceResetBody
   , msTrailer = trailerFIX42 }
   where
   mSequenceResetBody = 
      LT.insert (tnum tGapFillFlag) tGapFillFlag $
      LT.insert (tnum tNewSeqNo) tNewSeqNo       LT.new


mLogout :: FIXMessageSpec
mLogout = FMSpec
   { msName = "Logout"
   , msType = C.pack "5"
   , msHeader = headerFIX42
   , msBody = mLogoutBody
   , msTrailer = trailerFIX42 }
   where
   mLogoutBody = 
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mIndicationofInterest :: FIXMessageSpec
mIndicationofInterest = FMSpec
   { msName = "IndicationofInterest"
   , msType = C.pack "6"
   , msHeader = headerFIX42
   , msBody = mIndicationofInterestBody
   , msTrailer = trailerFIX42 }
   where
   mIndicationofInterestBody = 
      LT.insert (tnum tIOIid) tIOIid $
      LT.insert (tnum tIOITransType) tIOITransType $
      LT.insert (tnum tIOIRefID) tIOIRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tIOIShares) tIOIShares $
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
      LT.insert (tnum tSpreadToBenchmark) tSpreadToBenchmark $
      LT.insert (tnum tBenchmark) tBenchmark       LT.new
      where
         gNoIOIQualifiers''' = FIXTag
            { tName = "NoIOIQualifiers"
            , tnum = tnum tNoIOIQualifiers
            , tparser = gNoIOIQualifiersP''' }

         gNoIOIQualifiersP''' = groupP FGSpec
            { gsLength = tNoIOIQualifiers
            , gsSeperator = tIOIQualifier
            , gsBody = gNoIOIQualifiersBody''' }
            where
            gNoIOIQualifiersBody''' = 
               LT.new

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP''' }

         gNoRoutingIDsP''' = groupP FGSpec
            { gsLength = tNoRoutingIDs
            , gsSeperator = tRoutingType
            , gsBody = gNoRoutingIDsBody''' }
            where
            gNoRoutingIDsBody''' = 
               LT.insert (tnum tRoutingID) tRoutingID                LT.new



mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { msName = "Advertisement"
   , msType = C.pack "7"
   , msHeader = headerFIX42
   , msBody = mAdvertisementBody
   , msTrailer = trailerFIX42 }
   where
   mAdvertisementBody = 
      LT.insert (tnum tAdvId) tAdvId $
      LT.insert (tnum tAdvTransType) tAdvTransType $
      LT.insert (tnum tAdvRefID) tAdvRefID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tShares) tShares $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tURLLink) tURLLink $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { msName = "ExecutionReport"
   , msType = C.pack "8"
   , msHeader = headerFIX42
   , msBody = mExecutionReportBody
   , msTrailer = trailerFIX42 }
   where
   mExecutionReportBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tNoContraBrokers) gNoContraBrokers''' $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tExecTransType) tExecTransType $
      LT.insert (tnum tExecRefID) tExecRefID $
      LT.insert (tnum tExecType) tExecType $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tOrdRejReason) tOrdRejReason $
      LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tOrdType) tOrdType $
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
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tLastShares) tLastShares $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tLastSpotRate) tLastSpotRate $
      LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
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
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
      LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount $
      LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType       LT.new
      where
         gNoContraBrokers''' = FIXTag
            { tName = "NoContraBrokers"
            , tnum = tnum tNoContraBrokers
            , tparser = gNoContraBrokersP''' }

         gNoContraBrokersP''' = groupP FGSpec
            { gsLength = tNoContraBrokers
            , gsSeperator = tContraBroker
            , gsBody = gNoContraBrokersBody''' }
            where
            gNoContraBrokersBody''' = 
               LT.insert (tnum tContraTrader) tContraTrader $
               LT.insert (tnum tContraTradeQty) tContraTradeQty $
               LT.insert (tnum tContraTradeTime) tContraTradeTime                LT.new



mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { msName = "OrderCancelReject"
   , msType = C.pack "9"
   , msHeader = headerFIX42
   , msBody = mOrderCancelRejectBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelRejectBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrdStatus) tOrdStatus $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
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
   , msHeader = headerFIX42
   , msBody = mLogonBody
   , msTrailer = trailerFIX42 }
   where
   mLogonBody = 
      LT.insert (tnum tEncryptMethod) tEncryptMethod $
      LT.insert (tnum tHeartBtInt) tHeartBtInt $
      LT.insert (tnum tRawDataLength) tRawDataLength $
      LT.insert (tnum tRawData) tRawData $
      LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag $
      LT.insert (tnum tMaxMessageSize) tMaxMessageSize $
      LT.insert (tnum tNoMsgTypes) gNoMsgTypes'''       LT.new
      where
         gNoMsgTypes''' = FIXTag
            { tName = "NoMsgTypes"
            , tnum = tnum tNoMsgTypes
            , tparser = gNoMsgTypesP''' }

         gNoMsgTypesP''' = groupP FGSpec
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
   , msHeader = headerFIX42
   , msBody = mNewsBody
   , msTrailer = trailerFIX42 }
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
            , tparser = gLinesOfTextP''' }

         gLinesOfTextP''' = groupP FGSpec
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
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tRelatdSym
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP''' }

         gNoRoutingIDsP''' = groupP FGSpec
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
   , msHeader = headerFIX42
   , msBody = mEmailBody
   , msTrailer = trailerFIX42 }
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
            , tparser = gLinesOfTextP''' }

         gLinesOfTextP''' = groupP FGSpec
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
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tRelatdSym
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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

         gNoRoutingIDs''' = FIXTag
            { tName = "NoRoutingIDs"
            , tnum = tnum tNoRoutingIDs
            , tparser = gNoRoutingIDsP''' }

         gNoRoutingIDsP''' = groupP FGSpec
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
   , msHeader = headerFIX42
   , msBody = mNewOrderSingleBody
   , msTrailer = trailerFIX42 }
   where
   mNewOrderSingleBody = 
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
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
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tOrderQty) tOrderQty $
      LT.insert (tnum tCashOrderQty) tCashOrderQty $
      LT.insert (tnum tOrdType) tOrdType $
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
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tPegDifference) tPegDifference $
      LT.insert (tnum tDiscretionInst) tDiscretionInst $
      LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP''' }

         gNoAllocsP''' = groupP FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tAllocShares) tAllocShares                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP''' }

         gNoTradingSessionsP''' = groupP FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.new



mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { msName = "NewOrderList"
   , msType = C.pack "E"
   , msHeader = headerFIX42
   , msBody = mNewOrderListBody
   , msTrailer = trailerFIX42 }
   where
   mNewOrderListBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tProgRptReqs) tProgRptReqs $
      LT.insert (tnum tBidType) tBidType $
      LT.insert (tnum tProgPeriodInterval) tProgPeriodInterval $
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
            , tparser = gNoOrdersP''' }

         gNoOrdersP''' = groupP FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tListSeqNo) tListSeqNo $
               LT.insert (tnum tSettlInstMode) tSettlInstMode $
               LT.insert (tnum tClientID) tClientID $
               LT.insert (tnum tExecBroker) tExecBroker $
               LT.insert (tnum tAccount) tAccount $
               LT.insert (tnum tNoAllocs) gNoAllocs'''''' $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
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
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tCashOrderQty) tCashOrderQty $
               LT.insert (tnum tOrdType) tOrdType $
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
               LT.insert (tnum tRule80A) tRule80A $
               LT.insert (tnum tForexReq) tForexReq $
               LT.insert (tnum tSettlCurrency) tSettlCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tOpenClose) tOpenClose $
               LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
               LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
               LT.insert (tnum tMaxShow) tMaxShow $
               LT.insert (tnum tPegDifference) tPegDifference $
               LT.insert (tnum tDiscretionInst) tDiscretionInst $
               LT.insert (tnum tDiscretionOffset) tDiscretionOffset $
               LT.insert (tnum tClearingFirm) tClearingFirm $
               LT.insert (tnum tClearingAccount) tClearingAccount                LT.new
               where
                  gNoAllocs'''''' = FIXTag
                     { tName = "NoAllocs"
                     , tnum = tnum tNoAllocs
                     , tparser = gNoAllocsP'''''' }

                  gNoAllocsP'''''' = groupP FGSpec
                     { gsLength = tNoAllocs
                     , gsSeperator = tAllocAccount
                     , gsBody = gNoAllocsBody'''''' }
                     where
                     gNoAllocsBody'''''' = 
                        LT.insert (tnum tAllocShares) tAllocShares                         LT.new

                  gNoTradingSessions'''''' = FIXTag
                     { tName = "NoTradingSessions"
                     , tnum = tnum tNoTradingSessions
                     , tparser = gNoTradingSessionsP'''''' }

                  gNoTradingSessionsP'''''' = groupP FGSpec
                     { gsLength = tNoTradingSessions
                     , gsSeperator = tTradingSessionID
                     , gsBody = gNoTradingSessionsBody'''''' }
                     where
                     gNoTradingSessionsBody'''''' = 
                        LT.new




mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { msName = "OrderCancelRequest"
   , msType = C.pack "F"
   , msHeader = headerFIX42
   , msBody = mOrderCancelRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelRequestBody = 
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tComplianceID) tComplianceID $
      LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { msName = "OrderCancelReplaceRequest"
   , msType = C.pack "G"
   , msHeader = headerFIX42
   , msBody = mOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderCancelReplaceRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tNoAllocs) gNoAllocs''' $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tHandlInst) tHandlInst $
      LT.insert (tnum tExecInst) tExecInst $
      LT.insert (tnum tMinQty) tMinQty $
      LT.insert (tnum tMaxFloor) tMaxFloor $
      LT.insert (tnum tExDestination) tExDestination $
      LT.insert (tnum tNoTradingSessions) gNoTradingSessions''' $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tPrice) tPrice $
      LT.insert (tnum tStopPx) tStopPx $
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
      LT.insert (tnum tRule80A) tRule80A $
      LT.insert (tnum tForexReq) tForexReq $
      LT.insert (tnum tSettlCurrency) tSettlCurrency $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
      LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
      LT.insert (tnum tMaxShow) tMaxShow $
      LT.insert (tnum tLocateReqd) tLocateReqd $
      LT.insert (tnum tClearingFirm) tClearingFirm $
      LT.insert (tnum tClearingAccount) tClearingAccount       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP''' }

         gNoAllocsP''' = groupP FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tAllocShares) tAllocShares                LT.new

         gNoTradingSessions''' = FIXTag
            { tName = "NoTradingSessions"
            , tnum = tnum tNoTradingSessions
            , tparser = gNoTradingSessionsP''' }

         gNoTradingSessionsP''' = groupP FGSpec
            { gsLength = tNoTradingSessions
            , gsSeperator = tTradingSessionID
            , gsBody = gNoTradingSessionsBody''' }
            where
            gNoTradingSessionsBody''' = 
               LT.new



mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { msName = "OrderStatusRequest"
   , msType = C.pack "H"
   , msHeader = headerFIX42
   , msBody = mOrderStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mOrderStatusRequestBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tClOrdID) tClOrdID $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tAccount) tAccount $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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


mAllocation :: FIXMessageSpec
mAllocation = FMSpec
   { msName = "Allocation"
   , msType = C.pack "J"
   , msHeader = headerFIX42
   , msBody = mAllocationBody
   , msTrailer = trailerFIX42 }
   where
   mAllocationBody = 
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tAllocTransType) tAllocTransType $
      LT.insert (tnum tRefAllocID) tRefAllocID $
      LT.insert (tnum tAllocLinkID) tAllocLinkID $
      LT.insert (tnum tAllocLinkType) tAllocLinkType $
      LT.insert (tnum tNoOrders) gNoOrders''' $
      LT.insert (tnum tNoExecs) gNoExecs''' $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tShares) tShares $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tAvgPx) tAvgPx $
      LT.insert (tnum tCurrency) tCurrency $
      LT.insert (tnum tAvgPrxPrecision) tAvgPrxPrecision $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
      LT.insert (tnum tNetMoney) tNetMoney $
      LT.insert (tnum tOpenClose) tOpenClose $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
      LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
      LT.insert (tnum tNoAllocs) gNoAllocs'''       LT.new
      where
         gNoAllocs''' = FIXTag
            { tName = "NoAllocs"
            , tnum = tnum tNoAllocs
            , tparser = gNoAllocsP''' }

         gNoAllocsP''' = groupP FGSpec
            { gsLength = tNoAllocs
            , gsSeperator = tAllocAccount
            , gsBody = gNoAllocsBody''' }
            where
            gNoAllocsBody''' = 
               LT.insert (tnum tAllocPrice) tAllocPrice $
               LT.insert (tnum tAllocShares) tAllocShares $
               LT.insert (tnum tProcessCode) tProcessCode $
               LT.insert (tnum tBrokerOfCredit) tBrokerOfCredit $
               LT.insert (tnum tNotifyBrokerOfCredit) tNotifyBrokerOfCredit $
               LT.insert (tnum tAllocHandlInst) tAllocHandlInst $
               LT.insert (tnum tAllocText) tAllocText $
               LT.insert (tnum tEncodedAllocTextLen) tEncodedAllocTextLen $
               LT.insert (tnum tEncodedAllocText) tEncodedAllocText $
               LT.insert (tnum tExecBroker) tExecBroker $
               LT.insert (tnum tClientID) tClientID $
               LT.insert (tnum tCommission) tCommission $
               LT.insert (tnum tCommType) tCommType $
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
                     , tparser = gNoMiscFeesP'''''' }

                  gNoMiscFeesP'''''' = groupP FGSpec
                     { gsLength = tNoMiscFees
                     , gsSeperator = tMiscFeeAmt
                     , gsBody = gNoMiscFeesBody'''''' }
                     where
                     gNoMiscFeesBody'''''' = 
                        LT.insert (tnum tMiscFeeCurr) tMiscFeeCurr $
                        LT.insert (tnum tMiscFeeType) tMiscFeeType                         LT.new


         gNoExecs''' = FIXTag
            { tName = "NoExecs"
            , tnum = tnum tNoExecs
            , tparser = gNoExecsP''' }

         gNoExecsP''' = groupP FGSpec
            { gsLength = tNoExecs
            , gsSeperator = tLastShares
            , gsBody = gNoExecsBody''' }
            where
            gNoExecsBody''' = 
               LT.insert (tnum tExecID) tExecID $
               LT.insert (tnum tLastPx) tLastPx $
               LT.insert (tnum tLastCapacity) tLastCapacity                LT.new

         gNoOrders''' = FIXTag
            { tName = "NoOrders"
            , tnum = tnum tNoOrders
            , tparser = gNoOrdersP''' }

         gNoOrdersP''' = groupP FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tOrderID) tOrderID $
               LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
               LT.insert (tnum tListID) tListID $
               LT.insert (tnum tWaveNo) tWaveNo                LT.new



mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { msName = "ListCancelRequest"
   , msType = C.pack "K"
   , msHeader = headerFIX42
   , msBody = mListCancelRequestBody
   , msTrailer = trailerFIX42 }
   where
   mListCancelRequestBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { msName = "ListExecute"
   , msType = C.pack "L"
   , msHeader = headerFIX42
   , msBody = mListExecuteBody
   , msTrailer = trailerFIX42 }
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
   , msHeader = headerFIX42
   , msBody = mListStatusRequestBody
   , msTrailer = trailerFIX42 }
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
   , msHeader = headerFIX42
   , msBody = mListStatusBody
   , msTrailer = trailerFIX42 }
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
            , tparser = gNoOrdersP''' }

         gNoOrdersP''' = groupP FGSpec
            { gsLength = tNoOrders
            , gsSeperator = tClOrdID
            , gsBody = gNoOrdersBody''' }
            where
            gNoOrdersBody''' = 
               LT.insert (tnum tCumQty) tCumQty $
               LT.insert (tnum tOrdStatus) tOrdStatus $
               LT.insert (tnum tLeavesQty) tLeavesQty $
               LT.insert (tnum tCxlQty) tCxlQty $
               LT.insert (tnum tAvgPx) tAvgPx $
               LT.insert (tnum tOrdRejReason) tOrdRejReason $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mAllocationACK :: FIXMessageSpec
mAllocationACK = FMSpec
   { msName = "AllocationACK"
   , msType = C.pack "P"
   , msHeader = headerFIX42
   , msBody = mAllocationACKBody
   , msTrailer = trailerFIX42 }
   where
   mAllocationACKBody = 
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tAllocStatus) tAllocStatus $
      LT.insert (tnum tAllocRejCode) tAllocRejCode $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { msName = "DontKnowTrade"
   , msType = C.pack "Q"
   , msHeader = headerFIX42
   , msBody = mDontKnowTradeBody
   , msTrailer = trailerFIX42 }
   where
   mDontKnowTradeBody = 
      LT.insert (tnum tOrderID) tOrderID $
      LT.insert (tnum tExecID) tExecID $
      LT.insert (tnum tDKReason) tDKReason $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tLastShares) tLastShares $
      LT.insert (tnum tLastPx) tLastPx $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText       LT.new


mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { msName = "QuoteRequest"
   , msType = C.pack "R"
   , msHeader = headerFIX42
   , msBody = mQuoteRequestBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteRequestBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tOrderQty) tOrderQty $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tOrdType) tOrdType $
               LT.insert (tnum tFutSettDate2) tFutSettDate2 $
               LT.insert (tnum tOrderQty2) tOrderQty2 $
               LT.insert (tnum tExpireTime) tExpireTime $
               LT.insert (tnum tTransactTime) tTransactTime $
               LT.insert (tnum tCurrency) tCurrency                LT.new



mQuote :: FIXMessageSpec
mQuote = FMSpec
   { msName = "Quote"
   , msType = C.pack "S"
   , msHeader = headerFIX42
   , msBody = mQuoteBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tFutSettDate) tFutSettDate $
      LT.insert (tnum tOrdType) tOrdType $
      LT.insert (tnum tFutSettDate2) tFutSettDate2 $
      LT.insert (tnum tOrderQty2) tOrderQty2 $
      LT.insert (tnum tCurrency) tCurrency       LT.new


mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { msName = "SettlementInstructions"
   , msType = C.pack "T"
   , msHeader = headerFIX42
   , msBody = mSettlementInstructionsBody
   , msTrailer = trailerFIX42 }
   where
   mSettlementInstructionsBody = 
      LT.insert (tnum tSettlInstID) tSettlInstID $
      LT.insert (tnum tSettlInstTransType) tSettlInstTransType $
      LT.insert (tnum tSettlInstRefID) tSettlInstRefID $
      LT.insert (tnum tSettlInstMode) tSettlInstMode $
      LT.insert (tnum tSettlInstSource) tSettlInstSource $
      LT.insert (tnum tAllocAccount) tAllocAccount $
      LT.insert (tnum tSettlLocation) tSettlLocation $
      LT.insert (tnum tTradeDate) tTradeDate $
      LT.insert (tnum tAllocID) tAllocID $
      LT.insert (tnum tLastMkt) tLastMkt $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tSide) tSide $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tEffectiveTime) tEffectiveTime $
      LT.insert (tnum tTransactTime) tTransactTime $
      LT.insert (tnum tClientID) tClientID $
      LT.insert (tnum tExecBroker) tExecBroker $
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
      LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone       LT.new


mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec
   { msName = "MarketDataRequest"
   , msType = C.pack "V"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataRequestBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
      LT.insert (tnum tMarketDepth) tMarketDepth $
      LT.insert (tnum tMDUpdateType) tMDUpdateType $
      LT.insert (tnum tAggregatedBook) tAggregatedBook $
      LT.insert (tnum tNoMDEntryTypes) gNoMDEntryTypes''' $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoMDEntryTypes''' = FIXTag
            { tName = "NoMDEntryTypes"
            , tnum = tnum tNoMDEntryTypes
            , tparser = gNoMDEntryTypesP''' }

         gNoMDEntryTypesP''' = groupP FGSpec
            { gsLength = tNoMDEntryTypes
            , gsSeperator = tMDEntryType
            , gsBody = gNoMDEntryTypesBody''' }
            where
            gNoMDEntryTypesBody''' = 
               LT.new

         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tTradingSessionID) tTradingSessionID                LT.new



mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec
   { msName = "MarketDataSnapshotFullRefresh"
   , msType = C.pack "W"
   , msHeader = headerFIX42
   , msBody = mMarketDataSnapshotFullRefreshBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataSnapshotFullRefreshBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP''' }

         gNoMDEntriesP''' = groupP FGSpec
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
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec
   { msName = "MarketDataIncrementalRefresh"
   , msType = C.pack "X"
   , msHeader = headerFIX42
   , msBody = mMarketDataIncrementalRefreshBody
   , msTrailer = trailerFIX42 }
   where
   mMarketDataIncrementalRefreshBody = 
      LT.insert (tnum tMDReqID) tMDReqID $
      LT.insert (tnum tNoMDEntries) gNoMDEntries'''       LT.new
      where
         gNoMDEntries''' = FIXTag
            { tName = "NoMDEntries"
            , tnum = tnum tNoMDEntries
            , tparser = gNoMDEntriesP''' }

         gNoMDEntriesP''' = groupP FGSpec
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
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec
   { msName = "MarketDataRequestReject"
   , msType = C.pack "Y"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestRejectBody
   , msTrailer = trailerFIX42 }
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
   , msHeader = headerFIX42
   , msBody = mQuoteCancelBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteCancelBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tNoQuoteEntries) gNoQuoteEntries'''       LT.new
      where
         gNoQuoteEntries''' = FIXTag
            { tName = "NoQuoteEntries"
            , tnum = tnum tNoQuoteEntries
            , tparser = gNoQuoteEntriesP''' }

         gNoQuoteEntriesP''' = groupP FGSpec
            { gsLength = tNoQuoteEntries
            , gsSeperator = tSymbol
            , gsBody = gNoQuoteEntriesBody''' }
            where
            gNoQuoteEntriesBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol                LT.new



mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec
   { msName = "QuoteStatusRequest"
   , msType = C.pack "a"
   , msHeader = headerFIX42
   , msBody = mQuoteStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteStatusRequestBody = 
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mQuoteAcknowledgement :: FIXMessageSpec
mQuoteAcknowledgement = FMSpec
   { msName = "QuoteAcknowledgement"
   , msType = C.pack "b"
   , msHeader = headerFIX42
   , msBody = mQuoteAcknowledgementBody
   , msTrailer = trailerFIX42 }
   where
   mQuoteAcknowledgementBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteAckStatus) tQuoteAckStatus $
      LT.insert (tnum tQuoteRejectReason) tQuoteRejectReason $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tText) tText $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP''' }

         gNoQuoteSetsP''' = groupP FGSpec
            { gsLength = tNoQuoteSets
            , gsSeperator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' = 
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
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
                     , tparser = gNoQuoteEntriesP'''''' }

                  gNoQuoteEntriesP'''''' = groupP FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeperator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' = 
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tIDSource) tIDSource $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDay) tMaturityDay $
                        LT.insert (tnum tPutOrCall) tPutOrCall $
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
                        LT.insert (tnum tQuoteEntryRejectReason) tQuoteEntryRejectReason                         LT.new




mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec
   { msName = "SecurityDefinitionRequest"
   , msType = C.pack "c"
   , msHeader = headerFIX42
   , msBody = mSecurityDefinitionRequestBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityDefinitionRequestBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityRequestType) tSecurityRequestType $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tUnderlyingSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
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
               LT.insert (tnum tRatioQty) tRatioQty $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tUnderlyingCurrency) tUnderlyingCurrency                LT.new



mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec
   { msName = "SecurityDefinition"
   , msType = C.pack "d"
   , msHeader = headerFIX42
   , msBody = mSecurityDefinitionBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityDefinitionBody = 
      LT.insert (tnum tSecurityReqID) tSecurityReqID $
      LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
      LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
      LT.insert (tnum tTotalNumSecurities) tTotalNumSecurities $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tText) tText $
      LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
      LT.insert (tnum tEncodedText) tEncodedText $
      LT.insert (tnum tNoRelatedSym) gNoRelatedSym'''       LT.new
      where
         gNoRelatedSym''' = FIXTag
            { tName = "NoRelatedSym"
            , tnum = tnum tNoRelatedSym
            , tparser = gNoRelatedSymP''' }

         gNoRelatedSymP''' = groupP FGSpec
            { gsLength = tNoRelatedSym
            , gsSeperator = tUnderlyingSymbol
            , gsBody = gNoRelatedSymBody''' }
            where
            gNoRelatedSymBody''' = 
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
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
               LT.insert (tnum tRatioQty) tRatioQty $
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tUnderlyingCurrency) tUnderlyingCurrency                LT.new



mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec
   { msName = "SecurityStatusRequest"
   , msType = C.pack "e"
   , msHeader = headerFIX42
   , msBody = mSecurityStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityStatusRequestBody = 
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tTradingSessionID) tTradingSessionID       LT.new


mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec
   { msName = "SecurityStatus"
   , msType = C.pack "f"
   , msHeader = headerFIX42
   , msBody = mSecurityStatusBody
   , msTrailer = trailerFIX42 }
   where
   mSecurityStatusBody = 
      LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
      LT.insert (tnum tSymbol) tSymbol $
      LT.insert (tnum tSymbolSfx) tSymbolSfx $
      LT.insert (tnum tSecurityID) tSecurityID $
      LT.insert (tnum tIDSource) tIDSource $
      LT.insert (tnum tSecurityType) tSecurityType $
      LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
      LT.insert (tnum tMaturityDay) tMaturityDay $
      LT.insert (tnum tPutOrCall) tPutOrCall $
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
      LT.insert (tnum tAdjustment) tAdjustment       LT.new


mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec
   { msName = "TradingSessionStatusRequest"
   , msType = C.pack "g"
   , msHeader = headerFIX42
   , msBody = mTradingSessionStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
   mTradingSessionStatusRequestBody = 
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType       LT.new


mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec
   { msName = "TradingSessionStatus"
   , msType = C.pack "h"
   , msHeader = headerFIX42
   , msBody = mTradingSessionStatusBody
   , msTrailer = trailerFIX42 }
   where
   mTradingSessionStatusBody = 
      LT.insert (tnum tTradSesReqID) tTradSesReqID $
      LT.insert (tnum tTradingSessionID) tTradingSessionID $
      LT.insert (tnum tTradSesMethod) tTradSesMethod $
      LT.insert (tnum tTradSesMode) tTradSesMode $
      LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
      LT.insert (tnum tTradSesStatus) tTradSesStatus $
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
   , msHeader = headerFIX42
   , msBody = mMassQuoteBody
   , msTrailer = trailerFIX42 }
   where
   mMassQuoteBody = 
      LT.insert (tnum tQuoteReqID) tQuoteReqID $
      LT.insert (tnum tQuoteID) tQuoteID $
      LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
      LT.insert (tnum tDefBidSize) tDefBidSize $
      LT.insert (tnum tDefOfferSize) tDefOfferSize $
      LT.insert (tnum tNoQuoteSets) gNoQuoteSets'''       LT.new
      where
         gNoQuoteSets''' = FIXTag
            { tName = "NoQuoteSets"
            , tnum = tnum tNoQuoteSets
            , tparser = gNoQuoteSetsP''' }

         gNoQuoteSetsP''' = groupP FGSpec
            { gsLength = tNoQuoteSets
            , gsSeperator = tQuoteSetID
            , gsBody = gNoQuoteSetsBody''' }
            where
            gNoQuoteSetsBody''' = 
               LT.insert (tnum tUnderlyingSymbol) tUnderlyingSymbol $
               LT.insert (tnum tUnderlyingSymbolSfx) tUnderlyingSymbolSfx $
               LT.insert (tnum tUnderlyingSecurityID) tUnderlyingSecurityID $
               LT.insert (tnum tUnderlyingIDSource) tUnderlyingIDSource $
               LT.insert (tnum tUnderlyingSecurityType) tUnderlyingSecurityType $
               LT.insert (tnum tUnderlyingMaturityMonthYear) tUnderlyingMaturityMonthYear $
               LT.insert (tnum tUnderlyingMaturityDay) tUnderlyingMaturityDay $
               LT.insert (tnum tUnderlyingPutOrCall) tUnderlyingPutOrCall $
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
                     , tparser = gNoQuoteEntriesP'''''' }

                  gNoQuoteEntriesP'''''' = groupP FGSpec
                     { gsLength = tNoQuoteEntries
                     , gsSeperator = tQuoteEntryID
                     , gsBody = gNoQuoteEntriesBody'''''' }
                     where
                     gNoQuoteEntriesBody'''''' = 
                        LT.insert (tnum tSymbol) tSymbol $
                        LT.insert (tnum tSymbolSfx) tSymbolSfx $
                        LT.insert (tnum tSecurityID) tSecurityID $
                        LT.insert (tnum tIDSource) tIDSource $
                        LT.insert (tnum tSecurityType) tSecurityType $
                        LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
                        LT.insert (tnum tMaturityDay) tMaturityDay $
                        LT.insert (tnum tPutOrCall) tPutOrCall $
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
                        LT.insert (tnum tTransactTime) tTransactTime $
                        LT.insert (tnum tTradingSessionID) tTradingSessionID $
                        LT.insert (tnum tFutSettDate) tFutSettDate $
                        LT.insert (tnum tOrdType) tOrdType $
                        LT.insert (tnum tFutSettDate2) tFutSettDate2 $
                        LT.insert (tnum tOrderQty2) tOrderQty2 $
                        LT.insert (tnum tCurrency) tCurrency                         LT.new




mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec
   { msName = "BusinessMessageReject"
   , msType = C.pack "j"
   , msHeader = headerFIX42
   , msBody = mBusinessMessageRejectBody
   , msTrailer = trailerFIX42 }
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
   , msHeader = headerFIX42
   , msBody = mBidRequestBody
   , msTrailer = trailerFIX42 }
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
            , tparser = gNoBidComponentsP''' }

         gNoBidComponentsP''' = groupP FGSpec
            { gsLength = tNoBidComponents
            , gsSeperator = tListID
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' = 
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tTradingSessionID) tTradingSessionID $
               LT.insert (tnum tNetGrossInd) tNetGrossInd $
               LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
               LT.insert (tnum tFutSettDate) tFutSettDate $
               LT.insert (tnum tAccount) tAccount                LT.new

         gNoBidDescriptors''' = FIXTag
            { tName = "NoBidDescriptors"
            , tnum = tnum tNoBidDescriptors
            , tparser = gNoBidDescriptorsP''' }

         gNoBidDescriptorsP''' = groupP FGSpec
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
   , msHeader = headerFIX42
   , msBody = mBidResponseBody
   , msTrailer = trailerFIX42 }
   where
   mBidResponseBody = 
      LT.insert (tnum tBidID) tBidID $
      LT.insert (tnum tClientBidID) tClientBidID $
      LT.insert (tnum tNoBidComponents) gNoBidComponents'''       LT.new
      where
         gNoBidComponents''' = FIXTag
            { tName = "NoBidComponents"
            , tnum = tnum tNoBidComponents
            , tparser = gNoBidComponentsP''' }

         gNoBidComponentsP''' = groupP FGSpec
            { gsLength = tNoBidComponents
            , gsSeperator = tCommission
            , gsBody = gNoBidComponentsBody''' }
            where
            gNoBidComponentsBody''' = 
               LT.insert (tnum tCommType) tCommType $
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
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec
   { msName = "ListStrikePrice"
   , msType = C.pack "m"
   , msHeader = headerFIX42
   , msBody = mListStrikePriceBody
   , msTrailer = trailerFIX42 }
   where
   mListStrikePriceBody = 
      LT.insert (tnum tListID) tListID $
      LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
      LT.insert (tnum tNoStrikes) gNoStrikes'''       LT.new
      where
         gNoStrikes''' = FIXTag
            { tName = "NoStrikes"
            , tnum = tnum tNoStrikes
            , tparser = gNoStrikesP''' }

         gNoStrikesP''' = groupP FGSpec
            { gsLength = tNoStrikes
            , gsSeperator = tSymbol
            , gsBody = gNoStrikesBody''' }
            where
            gNoStrikesBody''' = 
               LT.insert (tnum tSymbolSfx) tSymbolSfx $
               LT.insert (tnum tSecurityID) tSecurityID $
               LT.insert (tnum tIDSource) tIDSource $
               LT.insert (tnum tSecurityType) tSecurityType $
               LT.insert (tnum tMaturityMonthYear) tMaturityMonthYear $
               LT.insert (tnum tMaturityDay) tMaturityDay $
               LT.insert (tnum tPutOrCall) tPutOrCall $
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
               LT.insert (tnum tSide) tSide $
               LT.insert (tnum tPrice) tPrice $
               LT.insert (tnum tCurrency) tCurrency $
               LT.insert (tnum tText) tText $
               LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
               LT.insert (tnum tEncodedText) tEncodedText                LT.new



fix42 :: FIXSpec
fix42 = FSpec
   { fsHeader = headerFIX42
   , fsTrailer = trailerFIX42
   , fsMessages = fix42Messages }
   where
      fix42Messages =
          LT.insert (msType mHeartbeat) mHeartbeat $
          LT.insert (msType mTestRequest) mTestRequest $
          LT.insert (msType mResendRequest) mResendRequest $
          LT.insert (msType mReject) mReject $
          LT.insert (msType mSequenceReset) mSequenceReset $
          LT.insert (msType mLogout) mLogout $
          LT.insert (msType mIndicationofInterest) mIndicationofInterest $
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
          LT.insert (msType mAllocationACK) mAllocationACK $
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
          LT.insert (msType mQuoteAcknowledgement) mQuoteAcknowledgement $
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
          LT.insert (msType mListStrikePrice) mListStrikePrice           LT.new 
