module Data.FIX.FIX41 where
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT ( new, insert )
import Common.FIXMessage
import Common.FIXParser


tAccount :: FIXTag
tAccount = FIXTag { tnum = 1, tparser = toFIXChar }

tAdvId :: FIXTag
tAdvId = FIXTag { tnum = 2, tparser = toFIXChar }

tAdvRefID :: FIXTag
tAdvRefID = FIXTag { tnum = 3, tparser = toFIXChar }

tAdvSide :: FIXTag
tAdvSide = FIXTag { tnum = 4, tparser = toFIXChar }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag { tnum = 5, tparser = toFIXChar }

tAvgPx :: FIXTag
tAvgPx = FIXTag { tnum = 6, tparser = toFIXFloat }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag { tnum = 7, tparser = toFIXInt }

tBeginString :: FIXTag
tBeginString = FIXTag { tnum = 8, tparser = toFIXChar }

tBodyLength :: FIXTag
tBodyLength = FIXTag { tnum = 9, tparser = toFIXInt }

tCheckSum :: FIXTag
tCheckSum = FIXTag { tnum = 10, tparser = toFIXChar }

tClOrdID :: FIXTag
tClOrdID = FIXTag { tnum = 11, tparser = toFIXChar }

tCommission :: FIXTag
tCommission = FIXTag { tnum = 12, tparser = toFIXFloat }

tCommType :: FIXTag
tCommType = FIXTag { tnum = 13, tparser = toFIXChar }

tCumQty :: FIXTag
tCumQty = FIXTag { tnum = 14, tparser = toFIXInt }

tCurrency :: FIXTag
tCurrency = FIXTag { tnum = 15, tparser = toFIXChar }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag { tnum = 16, tparser = toFIXInt }

tExecID :: FIXTag
tExecID = FIXTag { tnum = 17, tparser = toFIXChar }

tExecInst :: FIXTag
tExecInst = FIXTag { tnum = 18, tparser = toFIXChar }

tExecRefID :: FIXTag
tExecRefID = FIXTag { tnum = 19, tparser = toFIXChar }

tExecTransType :: FIXTag
tExecTransType = FIXTag { tnum = 20, tparser = toFIXChar }

tHandlInst :: FIXTag
tHandlInst = FIXTag { tnum = 21, tparser = toFIXChar }

tIDSource :: FIXTag
tIDSource = FIXTag { tnum = 22, tparser = toFIXChar }

tIOIid :: FIXTag
tIOIid = FIXTag { tnum = 23, tparser = toFIXChar }

tIOIOthSvc :: FIXTag
tIOIOthSvc = FIXTag { tnum = 24, tparser = toFIXChar }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag { tnum = 25, tparser = toFIXChar }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag { tnum = 26, tparser = toFIXChar }

tIOIShares :: FIXTag
tIOIShares = FIXTag { tnum = 27, tparser = toFIXChar }

tIOITransType :: FIXTag
tIOITransType = FIXTag { tnum = 28, tparser = toFIXChar }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag { tnum = 29, tparser = toFIXChar }

tLastMkt :: FIXTag
tLastMkt = FIXTag { tnum = 30, tparser = toFIXChar }

tLastPx :: FIXTag
tLastPx = FIXTag { tnum = 31, tparser = toFIXFloat }

tLastShares :: FIXTag
tLastShares = FIXTag { tnum = 32, tparser = toFIXInt }

tLinesOfText :: FIXTag
tLinesOfText = FIXTag { tnum = 33, tparser = toFIXInt }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag { tnum = 34, tparser = toFIXInt }

tMsgType :: FIXTag
tMsgType = FIXTag { tnum = 35, tparser = toFIXChar }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag { tnum = 36, tparser = toFIXInt }

tOrderID :: FIXTag
tOrderID = FIXTag { tnum = 37, tparser = toFIXChar }

tOrderQty :: FIXTag
tOrderQty = FIXTag { tnum = 38, tparser = toFIXInt }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag { tnum = 39, tparser = toFIXChar }

tOrdType :: FIXTag
tOrdType = FIXTag { tnum = 40, tparser = toFIXChar }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag { tnum = 41, tparser = toFIXChar }

tOrigTime :: FIXTag
tOrigTime = FIXTag { tnum = 42, tparser = toFIXString }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag { tnum = 43, tparser = toFIXChar }

tPrice :: FIXTag
tPrice = FIXTag { tnum = 44, tparser = toFIXFloat }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag { tnum = 45, tparser = toFIXInt }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag { tnum = 46, tparser = toFIXChar }

tRule80A :: FIXTag
tRule80A = FIXTag { tnum = 47, tparser = toFIXChar }

tSecurityID :: FIXTag
tSecurityID = FIXTag { tnum = 48, tparser = toFIXChar }

tSenderCompID :: FIXTag
tSenderCompID = FIXTag { tnum = 49, tparser = toFIXChar }

tSenderSubID :: FIXTag
tSenderSubID = FIXTag { tnum = 50, tparser = toFIXChar }

tSendingDate :: FIXTag
tSendingDate = FIXTag { tnum = 51, tparser = toFIXString }

tSendingTime :: FIXTag
tSendingTime = FIXTag { tnum = 52, tparser = toFIXString }

tShares :: FIXTag
tShares = FIXTag { tnum = 53, tparser = toFIXInt }

tSide :: FIXTag
tSide = FIXTag { tnum = 54, tparser = toFIXChar }

tSymbol :: FIXTag
tSymbol = FIXTag { tnum = 55, tparser = toFIXChar }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag { tnum = 56, tparser = toFIXChar }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag { tnum = 57, tparser = toFIXChar }

tText :: FIXTag
tText = FIXTag { tnum = 58, tparser = toFIXChar }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag { tnum = 59, tparser = toFIXChar }

tTransactTime :: FIXTag
tTransactTime = FIXTag { tnum = 60, tparser = toFIXString }

tUrgency :: FIXTag
tUrgency = FIXTag { tnum = 61, tparser = toFIXChar }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag { tnum = 62, tparser = toFIXString }

tSettlmntTyp :: FIXTag
tSettlmntTyp = FIXTag { tnum = 63, tparser = toFIXChar }

tFutSettDate :: FIXTag
tFutSettDate = FIXTag { tnum = 64, tparser = toFIXString }

tSymbolSfx :: FIXTag
tSymbolSfx = FIXTag { tnum = 65, tparser = toFIXChar }

tListID :: FIXTag
tListID = FIXTag { tnum = 66, tparser = toFIXChar }

tListSeqNo :: FIXTag
tListSeqNo = FIXTag { tnum = 67, tparser = toFIXInt }

tListNoOrds :: FIXTag
tListNoOrds = FIXTag { tnum = 68, tparser = toFIXInt }

tListExecInst :: FIXTag
tListExecInst = FIXTag { tnum = 69, tparser = toFIXChar }

tAllocID :: FIXTag
tAllocID = FIXTag { tnum = 70, tparser = toFIXChar }

tAllocTransType :: FIXTag
tAllocTransType = FIXTag { tnum = 71, tparser = toFIXChar }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag { tnum = 72, tparser = toFIXChar }

tNoOrders :: FIXTag
tNoOrders = FIXTag { tnum = 73, tparser = toFIXInt }

tAvgPrxPrecision :: FIXTag
tAvgPrxPrecision = FIXTag { tnum = 74, tparser = toFIXInt }

tTradeDate :: FIXTag
tTradeDate = FIXTag { tnum = 75, tparser = toFIXString }

tExecBroker :: FIXTag
tExecBroker = FIXTag { tnum = 76, tparser = toFIXChar }

tOpenClose :: FIXTag
tOpenClose = FIXTag { tnum = 77, tparser = toFIXChar }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag { tnum = 78, tparser = toFIXInt }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag { tnum = 79, tparser = toFIXChar }

tAllocShares :: FIXTag
tAllocShares = FIXTag { tnum = 80, tparser = toFIXInt }

tProcessCode :: FIXTag
tProcessCode = FIXTag { tnum = 81, tparser = toFIXChar }

tNoRpts :: FIXTag
tNoRpts = FIXTag { tnum = 82, tparser = toFIXInt }

tRptSeq :: FIXTag
tRptSeq = FIXTag { tnum = 83, tparser = toFIXInt }

tCxlQty :: FIXTag
tCxlQty = FIXTag { tnum = 84, tparser = toFIXInt }

tNoDlvyInst :: FIXTag
tNoDlvyInst = FIXTag { tnum = 85, tparser = toFIXInt }

tDlvyInst :: FIXTag
tDlvyInst = FIXTag { tnum = 86, tparser = toFIXChar }

tAllocStatus :: FIXTag
tAllocStatus = FIXTag { tnum = 87, tparser = toFIXInt }

tAllocRejCode :: FIXTag
tAllocRejCode = FIXTag { tnum = 88, tparser = toFIXInt }

tSignature :: FIXTag
tSignature = FIXTag { tnum = 89, tparser = toFIXString }

tSecureDataLen :: FIXTag
tSecureDataLen = FIXTag { tnum = 90, tparser = toFIXString }

tSecureData :: FIXTag
tSecureData = FIXTag { tnum = 91, tparser = toFIXString }

tBrokerOfCredit :: FIXTag
tBrokerOfCredit = FIXTag { tnum = 92, tparser = toFIXChar }

tSignatureLength :: FIXTag
tSignatureLength = FIXTag { tnum = 93, tparser = toFIXString }

tEmailType :: FIXTag
tEmailType = FIXTag { tnum = 94, tparser = toFIXChar }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag { tnum = 95, tparser = toFIXString }

tRawData :: FIXTag
tRawData = FIXTag { tnum = 96, tparser = toFIXString }

tPossResend :: FIXTag
tPossResend = FIXTag { tnum = 97, tparser = toFIXChar }

tEncryptMethod :: FIXTag
tEncryptMethod = FIXTag { tnum = 98, tparser = toFIXInt }

tStopPx :: FIXTag
tStopPx = FIXTag { tnum = 99, tparser = toFIXFloat }

tExDestination :: FIXTag
tExDestination = FIXTag { tnum = 100, tparser = toFIXChar }

tCxlRejReason :: FIXTag
tCxlRejReason = FIXTag { tnum = 102, tparser = toFIXInt }

tOrdRejReason :: FIXTag
tOrdRejReason = FIXTag { tnum = 103, tparser = toFIXInt }

tIOIQualifier :: FIXTag
tIOIQualifier = FIXTag { tnum = 104, tparser = toFIXChar }

tWaveNo :: FIXTag
tWaveNo = FIXTag { tnum = 105, tparser = toFIXChar }

tIssuer :: FIXTag
tIssuer = FIXTag { tnum = 106, tparser = toFIXChar }

tSecurityDesc :: FIXTag
tSecurityDesc = FIXTag { tnum = 107, tparser = toFIXChar }

tHeartBtInt :: FIXTag
tHeartBtInt = FIXTag { tnum = 108, tparser = toFIXInt }

tClientID :: FIXTag
tClientID = FIXTag { tnum = 109, tparser = toFIXChar }

tMinQty :: FIXTag
tMinQty = FIXTag { tnum = 110, tparser = toFIXInt }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag { tnum = 111, tparser = toFIXInt }

tTestReqID :: FIXTag
tTestReqID = FIXTag { tnum = 112, tparser = toFIXChar }

tReportToExch :: FIXTag
tReportToExch = FIXTag { tnum = 113, tparser = toFIXChar }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag { tnum = 114, tparser = toFIXChar }

tOnBehalfOfCompID :: FIXTag
tOnBehalfOfCompID = FIXTag { tnum = 115, tparser = toFIXChar }

tOnBehalfOfSubID :: FIXTag
tOnBehalfOfSubID = FIXTag { tnum = 116, tparser = toFIXChar }

tQuoteID :: FIXTag
tQuoteID = FIXTag { tnum = 117, tparser = toFIXChar }

tNetMoney :: FIXTag
tNetMoney = FIXTag { tnum = 118, tparser = toFIXFloat }

tSettlCurrAmt :: FIXTag
tSettlCurrAmt = FIXTag { tnum = 119, tparser = toFIXFloat }

tSettlCurrency :: FIXTag
tSettlCurrency = FIXTag { tnum = 120, tparser = toFIXChar }

tForexReq :: FIXTag
tForexReq = FIXTag { tnum = 121, tparser = toFIXChar }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag { tnum = 122, tparser = toFIXString }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag { tnum = 123, tparser = toFIXChar }

tNoExecs :: FIXTag
tNoExecs = FIXTag { tnum = 124, tparser = toFIXInt }

tCxlType :: FIXTag
tCxlType = FIXTag { tnum = 125, tparser = toFIXString }

tExpireTime :: FIXTag
tExpireTime = FIXTag { tnum = 126, tparser = toFIXString }

tDKReason :: FIXTag
tDKReason = FIXTag { tnum = 127, tparser = toFIXChar }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag { tnum = 128, tparser = toFIXChar }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag { tnum = 129, tparser = toFIXChar }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag { tnum = 130, tparser = toFIXChar }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag { tnum = 131, tparser = toFIXChar }

tBidPx :: FIXTag
tBidPx = FIXTag { tnum = 132, tparser = toFIXFloat }

tOfferPx :: FIXTag
tOfferPx = FIXTag { tnum = 133, tparser = toFIXFloat }

tBidSize :: FIXTag
tBidSize = FIXTag { tnum = 134, tparser = toFIXInt }

tOfferSize :: FIXTag
tOfferSize = FIXTag { tnum = 135, tparser = toFIXInt }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag { tnum = 136, tparser = toFIXInt }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag { tnum = 137, tparser = toFIXFloat }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag { tnum = 138, tparser = toFIXChar }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag { tnum = 139, tparser = toFIXChar }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag { tnum = 140, tparser = toFIXFloat }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag { tnum = 141, tparser = toFIXChar }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag { tnum = 142, tparser = toFIXChar }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag { tnum = 143, tparser = toFIXChar }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag { tnum = 144, tparser = toFIXChar }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag { tnum = 145, tparser = toFIXChar }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag { tnum = 146, tparser = toFIXInt }

tSubject :: FIXTag
tSubject = FIXTag { tnum = 147, tparser = toFIXChar }

tHeadline :: FIXTag
tHeadline = FIXTag { tnum = 148, tparser = toFIXChar }

tURLLink :: FIXTag
tURLLink = FIXTag { tnum = 149, tparser = toFIXChar }

tExecType :: FIXTag
tExecType = FIXTag { tnum = 150, tparser = toFIXChar }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag { tnum = 151, tparser = toFIXInt }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag { tnum = 152, tparser = toFIXFloat }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag { tnum = 153, tparser = toFIXFloat }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag { tnum = 154, tparser = toFIXFloat }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag { tnum = 155, tparser = toFIXFloat }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag { tnum = 156, tparser = toFIXChar }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag { tnum = 157, tparser = toFIXInt }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag { tnum = 158, tparser = toFIXFloat }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag { tnum = 159, tparser = toFIXFloat }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag { tnum = 160, tparser = toFIXChar }

tAllocText :: FIXTag
tAllocText = FIXTag { tnum = 161, tparser = toFIXChar }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag { tnum = 162, tparser = toFIXChar }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag { tnum = 163, tparser = toFIXChar }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag { tnum = 164, tparser = toFIXChar }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag { tnum = 165, tparser = toFIXChar }

tSettlLocation :: FIXTag
tSettlLocation = FIXTag { tnum = 166, tparser = toFIXChar }

tSecurityType :: FIXTag
tSecurityType = FIXTag { tnum = 167, tparser = toFIXChar }

tEffectiveTime :: FIXTag
tEffectiveTime = FIXTag { tnum = 168, tparser = toFIXString }

tStandInstDbType :: FIXTag
tStandInstDbType = FIXTag { tnum = 169, tparser = toFIXInt }

tStandInstDbName :: FIXTag
tStandInstDbName = FIXTag { tnum = 170, tparser = toFIXChar }

tStandInstDbID :: FIXTag
tStandInstDbID = FIXTag { tnum = 171, tparser = toFIXChar }

tSettlDeliveryType :: FIXTag
tSettlDeliveryType = FIXTag { tnum = 172, tparser = toFIXInt }

tSettlDepositoryCode :: FIXTag
tSettlDepositoryCode = FIXTag { tnum = 173, tparser = toFIXChar }

tSettlBrkrCode :: FIXTag
tSettlBrkrCode = FIXTag { tnum = 174, tparser = toFIXChar }

tSettlInstCode :: FIXTag
tSettlInstCode = FIXTag { tnum = 175, tparser = toFIXChar }

tSecuritySettlAgentName :: FIXTag
tSecuritySettlAgentName = FIXTag { tnum = 176, tparser = toFIXChar }

tSecuritySettlAgentCode :: FIXTag
tSecuritySettlAgentCode = FIXTag { tnum = 177, tparser = toFIXChar }

tSecuritySettlAgentAcctNum :: FIXTag
tSecuritySettlAgentAcctNum = FIXTag { tnum = 178, tparser = toFIXChar }

tSecuritySettlAgentAcctName :: FIXTag
tSecuritySettlAgentAcctName = FIXTag { tnum = 179, tparser = toFIXChar }

tSecuritySettlAgentContactName :: FIXTag
tSecuritySettlAgentContactName = FIXTag { tnum = 180, tparser = toFIXChar }

tSecuritySettlAgentContactPhone :: FIXTag
tSecuritySettlAgentContactPhone = FIXTag { tnum = 181, tparser = toFIXChar }

tCashSettlAgentName :: FIXTag
tCashSettlAgentName = FIXTag { tnum = 182, tparser = toFIXChar }

tCashSettlAgentCode :: FIXTag
tCashSettlAgentCode = FIXTag { tnum = 183, tparser = toFIXChar }

tCashSettlAgentAcctNum :: FIXTag
tCashSettlAgentAcctNum = FIXTag { tnum = 184, tparser = toFIXChar }

tCashSettlAgentAcctName :: FIXTag
tCashSettlAgentAcctName = FIXTag { tnum = 185, tparser = toFIXChar }

tCashSettlAgentContactName :: FIXTag
tCashSettlAgentContactName = FIXTag { tnum = 186, tparser = toFIXChar }

tCashSettlAgentContactPhone :: FIXTag
tCashSettlAgentContactPhone = FIXTag { tnum = 187, tparser = toFIXChar }

tBidSpotRate :: FIXTag
tBidSpotRate = FIXTag { tnum = 188, tparser = toFIXFloat }

tBidForwardPoints :: FIXTag
tBidForwardPoints = FIXTag { tnum = 189, tparser = toFIXFloat }

tOfferSpotRate :: FIXTag
tOfferSpotRate = FIXTag { tnum = 190, tparser = toFIXFloat }

tOfferForwardPoints :: FIXTag
tOfferForwardPoints = FIXTag { tnum = 191, tparser = toFIXFloat }

tOrderQty2 :: FIXTag
tOrderQty2 = FIXTag { tnum = 192, tparser = toFIXFloat }

tFutSettDate2 :: FIXTag
tFutSettDate2 = FIXTag { tnum = 193, tparser = toFIXString }

tLastSpotRate :: FIXTag
tLastSpotRate = FIXTag { tnum = 194, tparser = toFIXFloat }

tLastForwardPoints :: FIXTag
tLastForwardPoints = FIXTag { tnum = 195, tparser = toFIXFloat }

tAllocLinkID :: FIXTag
tAllocLinkID = FIXTag { tnum = 196, tparser = toFIXChar }

tAllocLinkType :: FIXTag
tAllocLinkType = FIXTag { tnum = 197, tparser = toFIXInt }

tSecondaryOrderID :: FIXTag
tSecondaryOrderID = FIXTag { tnum = 198, tparser = toFIXChar }

tNoIOIQualifiers :: FIXTag
tNoIOIQualifiers = FIXTag { tnum = 199, tparser = toFIXInt }

tMaturityMonthYear :: FIXTag
tMaturityMonthYear = FIXTag { tnum = 200, tparser = toFIXString }

tPutOrCall :: FIXTag
tPutOrCall = FIXTag { tnum = 201, tparser = toFIXInt }

tStrikePrice :: FIXTag
tStrikePrice = FIXTag { tnum = 202, tparser = toFIXFloat }

tCoveredOrUncovered :: FIXTag
tCoveredOrUncovered = FIXTag { tnum = 203, tparser = toFIXInt }

tCustomerOrFirm :: FIXTag
tCustomerOrFirm = FIXTag { tnum = 204, tparser = toFIXInt }

tMaturityDay :: FIXTag
tMaturityDay = FIXTag { tnum = 205, tparser = toFIXDayOfMonth }

tOptAttribute :: FIXTag
tOptAttribute = FIXTag { tnum = 206, tparser = toFIXChar }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag { tnum = 207, tparser = toFIXChar }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag { tnum = 208, tparser = toFIXChar }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag { tnum = 209, tparser = toFIXInt }

tMaxShow :: FIXTag
tMaxShow = FIXTag { tnum = 210, tparser = toFIXInt }

tPegDifference :: FIXTag
tPegDifference = FIXTag { tnum = 211, tparser = toFIXFloat }

headerFIX41 :: FIXTags
headerFIX41 = 
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
    LT.insert (tnum tOrigSendingTime) tOrigSendingTime LT.new

trailerFIX41 :: FIXTags
trailerFIX41 = 
    LT.insert (tnum tSignatureLength) tSignatureLength $
    LT.insert (tnum tSignature) tSignature $
    LT.insert (tnum tCheckSum) tCheckSum LT.new

mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { msType = C.pack "0"
   , msHeader = headerFIX41
   , msBody = mHeartbeatBody
   , msTrailer = trailerFIX41 }
   where
      mHeartbeatBody = 
          LT.insert (tnum tTestReqID) tTestReqID LT.new

mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { msType = C.pack "1"
   , msHeader = headerFIX41
   , msBody = mTestRequestBody
   , msTrailer = trailerFIX41 }
   where
      mTestRequestBody = 
          LT.insert (tnum tTestReqID) tTestReqID LT.new

mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { msType = C.pack "2"
   , msHeader = headerFIX41
   , msBody = mResendRequestBody
   , msTrailer = trailerFIX41 }
   where
      mResendRequestBody = 
          LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
          LT.insert (tnum tEndSeqNo) tEndSeqNo LT.new

mReject :: FIXMessageSpec
mReject = FMSpec
   { msType = C.pack "3"
   , msHeader = headerFIX41
   , msBody = mRejectBody
   , msTrailer = trailerFIX41 }
   where
      mRejectBody = 
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tText) tText LT.new

mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec
   { msType = C.pack "4"
   , msHeader = headerFIX41
   , msBody = mSequenceResetBody
   , msTrailer = trailerFIX41 }
   where
      mSequenceResetBody = 
          LT.insert (tnum tGapFillFlag) tGapFillFlag $
          LT.insert (tnum tNewSeqNo) tNewSeqNo LT.new

mLogout :: FIXMessageSpec
mLogout = FMSpec
   { msType = C.pack "5"
   , msHeader = headerFIX41
   , msBody = mLogoutBody
   , msTrailer = trailerFIX41 }
   where
      mLogoutBody = 
          LT.insert (tnum tText) tText LT.new

mIndicationofInterest :: FIXMessageSpec
mIndicationofInterest = FMSpec
   { msType = C.pack "6"
   , msHeader = headerFIX41
   , msBody = mIndicationofInterestBody
   , msTrailer = trailerFIX41 }
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tIOIShares) tIOIShares $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tValidUntilTime) tValidUntilTime $
          LT.insert (tnum tIOIQltyInd) tIOIQltyInd $
          LT.insert (tnum tIOIOthSvc) tIOIOthSvc $
          LT.insert (tnum tIOINaturalFlag) tIOINaturalFlag $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tURLLink) tURLLink LT.new

mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { msType = C.pack "7"
   , msHeader = headerFIX41
   , msBody = mAdvertisementBody
   , msTrailer = trailerFIX41 }
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tAdvSide) tAdvSide $
          LT.insert (tnum tShares) tShares $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tLastMkt) tLastMkt LT.new

mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { msType = C.pack "8"
   , msHeader = headerFIX41
   , msBody = mExecutionReportBody
   , msTrailer = trailerFIX41 }
   where
      mExecutionReportBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tExecTransType) tExecTransType $
          LT.insert (tnum tExecRefID) tExecRefID $
          LT.insert (tnum tExecType) tExecType $
          LT.insert (tnum tOrdStatus) tOrdStatus $
          LT.insert (tnum tOrdRejReason) tOrdRejReason $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tPegDifference) tPegDifference $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tRule80A) tRule80A $
          LT.insert (tnum tLastShares) tLastShares $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tLastSpotRate) tLastSpotRate $
          LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tLastCapacity) tLastCapacity $
          LT.insert (tnum tLeavesQty) tLeavesQty $
          LT.insert (tnum tCumQty) tCumQty $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tReportToExch) tReportToExch $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tText) tText LT.new

mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { msType = C.pack "9"
   , msHeader = headerFIX41
   , msBody = mOrderCancelRejectBody
   , msTrailer = trailerFIX41 }
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
          LT.insert (tnum tCxlRejReason) tCxlRejReason $
          LT.insert (tnum tText) tText LT.new

mLogon :: FIXMessageSpec
mLogon = FMSpec
   { msType = C.pack "A"
   , msHeader = headerFIX41
   , msBody = mLogonBody
   , msTrailer = trailerFIX41 }
   where
      mLogonBody = 
          LT.insert (tnum tEncryptMethod) tEncryptMethod $
          LT.insert (tnum tHeartBtInt) tHeartBtInt $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag LT.new

mNews :: FIXMessageSpec
mNews = FMSpec
   { msType = C.pack "B"
   , msHeader = headerFIX41
   , msBody = mNewsBody
   , msTrailer = trailerFIX41 }
   where
      mNewsBody = 
          LT.insert (tnum tOrigTime) tOrigTime $
          LT.insert (tnum tUrgency) tUrgency $
          LT.insert (tnum tHeadline) tHeadline $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData LT.new

mEmail :: FIXMessageSpec
mEmail = FMSpec
   { msType = C.pack "C"
   , msHeader = headerFIX41
   , msBody = mEmailBody
   , msTrailer = trailerFIX41 }
   where
      mEmailBody = 
          LT.insert (tnum tEmailThreadID) tEmailThreadID $
          LT.insert (tnum tEmailType) tEmailType $
          LT.insert (tnum tOrigTime) tOrigTime $
          LT.insert (tnum tSubject) tSubject $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData LT.new

mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec
   { msType = C.pack "D"
   , msHeader = headerFIX41
   , msBody = mNewOrderSingleBody
   , msTrailer = trailerFIX41 }
   where
      mNewOrderSingleBody = 
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tCashOrderQty) tCashOrderQty $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tIOIid) tIOIid $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tRule80A) tRule80A $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tFutSettDate2) tFutSettDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tOpenClose) tOpenClose $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tPegDifference) tPegDifference LT.new

mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { msType = C.pack "E"
   , msHeader = headerFIX41
   , msBody = mNewOrderListBody
   , msTrailer = trailerFIX41 }
   where
      mNewOrderListBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tListSeqNo) tListSeqNo $
          LT.insert (tnum tListNoOrds) tListNoOrds $
          LT.insert (tnum tListExecInst) tListExecInst $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tPegDifference) tPegDifference $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tRule80A) tRule80A $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tFutSettDate2) tFutSettDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tOpenClose) tOpenClose $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
          LT.insert (tnum tMaxShow) tMaxShow LT.new

mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { msType = C.pack "F"
   , msHeader = headerFIX41
   , msBody = mOrderCancelRequestBody
   , msTrailer = trailerFIX41 }
   where
      mOrderCancelRequestBody = 
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tListID) tListID $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tCashOrderQty) tCashOrderQty $
          LT.insert (tnum tText) tText LT.new

mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { msType = C.pack "G"
   , msHeader = headerFIX41
   , msBody = mOrderCancelReplaceRequestBody
   , msTrailer = trailerFIX41 }
   where
      mOrderCancelReplaceRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tCashOrderQty) tCashOrderQty $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tPegDifference) tPegDifference $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tRule80A) tRule80A $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tFutSettDate2) tFutSettDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tOpenClose) tOpenClose $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tCustomerOrFirm) tCustomerOrFirm $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tLocateReqd) tLocateReqd LT.new

mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { msType = C.pack "H"
   , msHeader = headerFIX41
   , msBody = mOrderStatusRequestBody
   , msTrailer = trailerFIX41 }
   where
      mOrderStatusRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide LT.new

mAllocation :: FIXMessageSpec
mAllocation = FMSpec
   { msType = C.pack "J"
   , msHeader = headerFIX41
   , msBody = mAllocationBody
   , msTrailer = trailerFIX41 }
   where
      mAllocationBody = 
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tAllocTransType) tAllocTransType $
          LT.insert (tnum tRefAllocID) tRefAllocID $
          LT.insert (tnum tAllocLinkID) tAllocLinkID $
          LT.insert (tnum tAllocLinkType) tAllocLinkType $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tShares) tShares $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAvgPrxPrecision) tAvgPrxPrecision $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSettlmntTyp) tSettlmntTyp $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tNetMoney) tNetMoney $
          LT.insert (tnum tOpenClose) tOpenClose $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate LT.new

mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { msType = C.pack "K"
   , msHeader = headerFIX41
   , msBody = mListCancelRequestBody
   , msTrailer = trailerFIX41 }
   where
      mListCancelRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tText) tText LT.new

mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { msType = C.pack "L"
   , msHeader = headerFIX41
   , msBody = mListExecuteBody
   , msTrailer = trailerFIX41 }
   where
      mListExecuteBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tText) tText LT.new

mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec
   { msType = C.pack "M"
   , msHeader = headerFIX41
   , msBody = mListStatusRequestBody
   , msTrailer = trailerFIX41 }
   where
      mListStatusRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tText) tText LT.new

mListStatus :: FIXMessageSpec
mListStatus = FMSpec
   { msType = C.pack "N"
   , msHeader = headerFIX41
   , msBody = mListStatusBody
   , msTrailer = trailerFIX41 }
   where
      mListStatusBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tWaveNo) tWaveNo $
          LT.insert (tnum tNoRpts) tNoRpts $
          LT.insert (tnum tRptSeq) tRptSeq LT.new

mAllocationACK :: FIXMessageSpec
mAllocationACK = FMSpec
   { msType = C.pack "P"
   , msHeader = headerFIX41
   , msBody = mAllocationACKBody
   , msTrailer = trailerFIX41 }
   where
      mAllocationACKBody = 
          LT.insert (tnum tClientID) tClientID $
          LT.insert (tnum tExecBroker) tExecBroker $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAllocStatus) tAllocStatus $
          LT.insert (tnum tAllocRejCode) tAllocRejCode $
          LT.insert (tnum tText) tText LT.new

mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { msType = C.pack "Q"
   , msHeader = headerFIX41
   , msBody = mDontKnowTradeBody
   , msTrailer = trailerFIX41 }
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tCashOrderQty) tCashOrderQty $
          LT.insert (tnum tLastShares) tLastShares $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tText) tText LT.new

mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { msType = C.pack "R"
   , msHeader = headerFIX41
   , msBody = mQuoteRequestBody
   , msTrailer = trailerFIX41 }
   where
      mQuoteRequestBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tOrderQty) tOrderQty $
          LT.insert (tnum tFutSettDate) tFutSettDate $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tFutSettDate2) tFutSettDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 LT.new

mQuote :: FIXMessageSpec
mQuote = FMSpec
   { msType = C.pack "S"
   , msHeader = headerFIX41
   , msBody = mQuoteBody
   , msTrailer = trailerFIX41 }
   where
      mQuoteBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
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
          LT.insert (tnum tSecurityExchange) tSecurityExchange $
          LT.insert (tnum tIssuer) tIssuer $
          LT.insert (tnum tSecurityDesc) tSecurityDesc $
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
          LT.insert (tnum tOrderQty2) tOrderQty2 LT.new

mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { msType = C.pack "T"
   , msHeader = headerFIX41
   , msBody = mSettlementInstructionsBody
   , msTrailer = trailerFIX41 }
   where
      mSettlementInstructionsBody = 
          LT.insert (tnum tSettlInstID) tSettlInstID $
          LT.insert (tnum tSettlInstTransType) tSettlInstTransType $
          LT.insert (tnum tSettlInstMode) tSettlInstMode $
          LT.insert (tnum tSettlInstSource) tSettlInstSource $
          LT.insert (tnum tAllocAccount) tAllocAccount $
          LT.insert (tnum tSettlLocation) tSettlLocation $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tLastMkt) tLastMkt $
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
          LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone LT.new

fix41 :: FIXSpec
fix41 = FSpec
   { fsHeader = headerFIX41
   , fsTrailer = trailerFIX41
   , fsMessages = fix41Messages }
   where
      fix41Messages =
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
                  LT.insert (msType mSettlementInstructions) mSettlementInstructions LT.new 
