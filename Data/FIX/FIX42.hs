module Data.FIX.FIX42 where
import qualified Data.ByteString.Char8 as C
import qualified Data.LookupTable as LT ( new, insert )
import Common.FIXMessage
import Common.FIXParser


tAccount :: FIXTag
tAccount = FIXTag { tnum = 1, tparser = toFIXString }

tAdvId :: FIXTag
tAdvId = FIXTag { tnum = 2, tparser = toFIXString }

tAdvRefID :: FIXTag
tAdvRefID = FIXTag { tnum = 3, tparser = toFIXString }

tAdvSide :: FIXTag
tAdvSide = FIXTag { tnum = 4, tparser = toFIXChar }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag { tnum = 5, tparser = toFIXString }

tAvgPx :: FIXTag
tAvgPx = FIXTag { tnum = 6, tparser = toFIXPrice }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag { tnum = 7, tparser = toFIXInt }

tBeginString :: FIXTag
tBeginString = FIXTag { tnum = 8, tparser = toFIXString }

tBodyLength :: FIXTag
tBodyLength = FIXTag { tnum = 9, tparser = toFIXInt }

tCheckSum :: FIXTag
tCheckSum = FIXTag { tnum = 10, tparser = toFIXString }

tClOrdID :: FIXTag
tClOrdID = FIXTag { tnum = 11, tparser = toFIXString }

tCommission :: FIXTag
tCommission = FIXTag { tnum = 12, tparser = toFIXAmt }

tCommType :: FIXTag
tCommType = FIXTag { tnum = 13, tparser = toFIXChar }

tCumQty :: FIXTag
tCumQty = FIXTag { tnum = 14, tparser = toFIXQuantity }

tCurrency :: FIXTag
tCurrency = FIXTag { tnum = 15, tparser = toFIXCurrency }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag { tnum = 16, tparser = toFIXInt }

tExecID :: FIXTag
tExecID = FIXTag { tnum = 17, tparser = toFIXString }

tExecInst :: FIXTag
tExecInst = FIXTag { tnum = 18, tparser = toFIXMultipleValueString }

tExecRefID :: FIXTag
tExecRefID = FIXTag { tnum = 19, tparser = toFIXString }

tExecTransType :: FIXTag
tExecTransType = FIXTag { tnum = 20, tparser = toFIXChar }

tHandlInst :: FIXTag
tHandlInst = FIXTag { tnum = 21, tparser = toFIXChar }

tIDSource :: FIXTag
tIDSource = FIXTag { tnum = 22, tparser = toFIXString }

tIOIid :: FIXTag
tIOIid = FIXTag { tnum = 23, tparser = toFIXString }

tIOIOthSvc :: FIXTag
tIOIOthSvc = FIXTag { tnum = 24, tparser = toFIXChar }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag { tnum = 25, tparser = toFIXChar }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag { tnum = 26, tparser = toFIXString }

tIOIShares :: FIXTag
tIOIShares = FIXTag { tnum = 27, tparser = toFIXString }

tIOITransType :: FIXTag
tIOITransType = FIXTag { tnum = 28, tparser = toFIXChar }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag { tnum = 29, tparser = toFIXChar }

tLastMkt :: FIXTag
tLastMkt = FIXTag { tnum = 30, tparser = toFIXExchange }

tLastPx :: FIXTag
tLastPx = FIXTag { tnum = 31, tparser = toFIXPrice }

tLastShares :: FIXTag
tLastShares = FIXTag { tnum = 32, tparser = toFIXQuantity }

tLinesOfText :: FIXTag
tLinesOfText = FIXTag { tnum = 33, tparser = toFIXInt }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag { tnum = 34, tparser = toFIXInt }

tMsgType :: FIXTag
tMsgType = FIXTag { tnum = 35, tparser = toFIXString }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag { tnum = 36, tparser = toFIXInt }

tOrderID :: FIXTag
tOrderID = FIXTag { tnum = 37, tparser = toFIXString }

tOrderQty :: FIXTag
tOrderQty = FIXTag { tnum = 38, tparser = toFIXQuantity }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag { tnum = 39, tparser = toFIXChar }

tOrdType :: FIXTag
tOrdType = FIXTag { tnum = 40, tparser = toFIXChar }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag { tnum = 41, tparser = toFIXString }

tOrigTime :: FIXTag
tOrigTime = FIXTag { tnum = 42, tparser = toFIXUTCTimestamp }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag { tnum = 43, tparser = toFIXBool }

tPrice :: FIXTag
tPrice = FIXTag { tnum = 44, tparser = toFIXPrice }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag { tnum = 45, tparser = toFIXInt }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag { tnum = 46, tparser = toFIXString }

tRule80A :: FIXTag
tRule80A = FIXTag { tnum = 47, tparser = toFIXChar }

tSecurityID :: FIXTag
tSecurityID = FIXTag { tnum = 48, tparser = toFIXString }

tSenderCompID :: FIXTag
tSenderCompID = FIXTag { tnum = 49, tparser = toFIXString }

tSenderSubID :: FIXTag
tSenderSubID = FIXTag { tnum = 50, tparser = toFIXString }

tSendingDate :: FIXTag
tSendingDate = FIXTag { tnum = 51, tparser = toFIXLocalMktDate }

tSendingTime :: FIXTag
tSendingTime = FIXTag { tnum = 52, tparser = toFIXUTCTimestamp }

tShares :: FIXTag
tShares = FIXTag { tnum = 53, tparser = toFIXQuantity }

tSide :: FIXTag
tSide = FIXTag { tnum = 54, tparser = toFIXChar }

tSymbol :: FIXTag
tSymbol = FIXTag { tnum = 55, tparser = toFIXString }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag { tnum = 56, tparser = toFIXString }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag { tnum = 57, tparser = toFIXString }

tText :: FIXTag
tText = FIXTag { tnum = 58, tparser = toFIXString }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag { tnum = 59, tparser = toFIXChar }

tTransactTime :: FIXTag
tTransactTime = FIXTag { tnum = 60, tparser = toFIXUTCTimestamp }

tUrgency :: FIXTag
tUrgency = FIXTag { tnum = 61, tparser = toFIXChar }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag { tnum = 62, tparser = toFIXUTCTimestamp }

tSettlmntTyp :: FIXTag
tSettlmntTyp = FIXTag { tnum = 63, tparser = toFIXChar }

tFutSettDate :: FIXTag
tFutSettDate = FIXTag { tnum = 64, tparser = toFIXLocalMktDate }

tSymbolSfx :: FIXTag
tSymbolSfx = FIXTag { tnum = 65, tparser = toFIXString }

tListID :: FIXTag
tListID = FIXTag { tnum = 66, tparser = toFIXString }

tListSeqNo :: FIXTag
tListSeqNo = FIXTag { tnum = 67, tparser = toFIXInt }

tTotNoOrders :: FIXTag
tTotNoOrders = FIXTag { tnum = 68, tparser = toFIXInt }

tListExecInst :: FIXTag
tListExecInst = FIXTag { tnum = 69, tparser = toFIXString }

tAllocID :: FIXTag
tAllocID = FIXTag { tnum = 70, tparser = toFIXString }

tAllocTransType :: FIXTag
tAllocTransType = FIXTag { tnum = 71, tparser = toFIXChar }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag { tnum = 72, tparser = toFIXString }

tNoOrders :: FIXTag
tNoOrders = FIXTag { tnum = 73, tparser = toFIXInt }

tAvgPrxPrecision :: FIXTag
tAvgPrxPrecision = FIXTag { tnum = 74, tparser = toFIXInt }

tTradeDate :: FIXTag
tTradeDate = FIXTag { tnum = 75, tparser = toFIXLocalMktDate }

tExecBroker :: FIXTag
tExecBroker = FIXTag { tnum = 76, tparser = toFIXString }

tOpenClose :: FIXTag
tOpenClose = FIXTag { tnum = 77, tparser = toFIXChar }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag { tnum = 78, tparser = toFIXInt }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag { tnum = 79, tparser = toFIXString }

tAllocShares :: FIXTag
tAllocShares = FIXTag { tnum = 80, tparser = toFIXQuantity }

tProcessCode :: FIXTag
tProcessCode = FIXTag { tnum = 81, tparser = toFIXChar }

tNoRpts :: FIXTag
tNoRpts = FIXTag { tnum = 82, tparser = toFIXInt }

tRptSeq :: FIXTag
tRptSeq = FIXTag { tnum = 83, tparser = toFIXInt }

tCxlQty :: FIXTag
tCxlQty = FIXTag { tnum = 84, tparser = toFIXQuantity }

tNoDlvyInst :: FIXTag
tNoDlvyInst = FIXTag { tnum = 85, tparser = toFIXInt }

tDlvyInst :: FIXTag
tDlvyInst = FIXTag { tnum = 86, tparser = toFIXString }

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
tBrokerOfCredit = FIXTag { tnum = 92, tparser = toFIXString }

tSignatureLength :: FIXTag
tSignatureLength = FIXTag { tnum = 93, tparser = toFIXString }

tEmailType :: FIXTag
tEmailType = FIXTag { tnum = 94, tparser = toFIXChar }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag { tnum = 95, tparser = toFIXString }

tRawData :: FIXTag
tRawData = FIXTag { tnum = 96, tparser = toFIXString }

tPossResend :: FIXTag
tPossResend = FIXTag { tnum = 97, tparser = toFIXBool }

tEncryptMethod :: FIXTag
tEncryptMethod = FIXTag { tnum = 98, tparser = toFIXInt }

tStopPx :: FIXTag
tStopPx = FIXTag { tnum = 99, tparser = toFIXPrice }

tExDestination :: FIXTag
tExDestination = FIXTag { tnum = 100, tparser = toFIXExchange }

tCxlRejReason :: FIXTag
tCxlRejReason = FIXTag { tnum = 102, tparser = toFIXInt }

tOrdRejReason :: FIXTag
tOrdRejReason = FIXTag { tnum = 103, tparser = toFIXInt }

tIOIQualifier :: FIXTag
tIOIQualifier = FIXTag { tnum = 104, tparser = toFIXChar }

tWaveNo :: FIXTag
tWaveNo = FIXTag { tnum = 105, tparser = toFIXString }

tIssuer :: FIXTag
tIssuer = FIXTag { tnum = 106, tparser = toFIXString }

tSecurityDesc :: FIXTag
tSecurityDesc = FIXTag { tnum = 107, tparser = toFIXString }

tHeartBtInt :: FIXTag
tHeartBtInt = FIXTag { tnum = 108, tparser = toFIXInt }

tClientID :: FIXTag
tClientID = FIXTag { tnum = 109, tparser = toFIXString }

tMinQty :: FIXTag
tMinQty = FIXTag { tnum = 110, tparser = toFIXQuantity }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag { tnum = 111, tparser = toFIXQuantity }

tTestReqID :: FIXTag
tTestReqID = FIXTag { tnum = 112, tparser = toFIXString }

tReportToExch :: FIXTag
tReportToExch = FIXTag { tnum = 113, tparser = toFIXBool }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag { tnum = 114, tparser = toFIXBool }

tOnBehalfOfCompID :: FIXTag
tOnBehalfOfCompID = FIXTag { tnum = 115, tparser = toFIXString }

tOnBehalfOfSubID :: FIXTag
tOnBehalfOfSubID = FIXTag { tnum = 116, tparser = toFIXString }

tQuoteID :: FIXTag
tQuoteID = FIXTag { tnum = 117, tparser = toFIXString }

tNetMoney :: FIXTag
tNetMoney = FIXTag { tnum = 118, tparser = toFIXAmt }

tSettlCurrAmt :: FIXTag
tSettlCurrAmt = FIXTag { tnum = 119, tparser = toFIXAmt }

tSettlCurrency :: FIXTag
tSettlCurrency = FIXTag { tnum = 120, tparser = toFIXCurrency }

tForexReq :: FIXTag
tForexReq = FIXTag { tnum = 121, tparser = toFIXBool }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag { tnum = 122, tparser = toFIXUTCTimestamp }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag { tnum = 123, tparser = toFIXBool }

tNoExecs :: FIXTag
tNoExecs = FIXTag { tnum = 124, tparser = toFIXInt }

tCxlType :: FIXTag
tCxlType = FIXTag { tnum = 125, tparser = toFIXString }

tExpireTime :: FIXTag
tExpireTime = FIXTag { tnum = 126, tparser = toFIXUTCTimestamp }

tDKReason :: FIXTag
tDKReason = FIXTag { tnum = 127, tparser = toFIXChar }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag { tnum = 128, tparser = toFIXString }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag { tnum = 129, tparser = toFIXString }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag { tnum = 130, tparser = toFIXBool }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag { tnum = 131, tparser = toFIXString }

tBidPx :: FIXTag
tBidPx = FIXTag { tnum = 132, tparser = toFIXPrice }

tOfferPx :: FIXTag
tOfferPx = FIXTag { tnum = 133, tparser = toFIXPrice }

tBidSize :: FIXTag
tBidSize = FIXTag { tnum = 134, tparser = toFIXQuantity }

tOfferSize :: FIXTag
tOfferSize = FIXTag { tnum = 135, tparser = toFIXQuantity }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag { tnum = 136, tparser = toFIXInt }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag { tnum = 137, tparser = toFIXAmt }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag { tnum = 138, tparser = toFIXCurrency }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag { tnum = 139, tparser = toFIXChar }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag { tnum = 140, tparser = toFIXPrice }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag { tnum = 141, tparser = toFIXBool }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag { tnum = 142, tparser = toFIXString }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag { tnum = 143, tparser = toFIXString }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag { tnum = 144, tparser = toFIXString }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag { tnum = 145, tparser = toFIXString }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag { tnum = 146, tparser = toFIXInt }

tSubject :: FIXTag
tSubject = FIXTag { tnum = 147, tparser = toFIXString }

tHeadline :: FIXTag
tHeadline = FIXTag { tnum = 148, tparser = toFIXString }

tURLLink :: FIXTag
tURLLink = FIXTag { tnum = 149, tparser = toFIXString }

tExecType :: FIXTag
tExecType = FIXTag { tnum = 150, tparser = toFIXChar }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag { tnum = 151, tparser = toFIXQuantity }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag { tnum = 152, tparser = toFIXQuantity }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag { tnum = 153, tparser = toFIXPrice }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag { tnum = 154, tparser = toFIXAmt }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag { tnum = 155, tparser = toFIXFloat }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag { tnum = 156, tparser = toFIXChar }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag { tnum = 157, tparser = toFIXInt }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag { tnum = 158, tparser = toFIXFloat }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag { tnum = 159, tparser = toFIXAmt }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag { tnum = 160, tparser = toFIXChar }

tAllocText :: FIXTag
tAllocText = FIXTag { tnum = 161, tparser = toFIXString }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag { tnum = 162, tparser = toFIXString }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag { tnum = 163, tparser = toFIXChar }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag { tnum = 164, tparser = toFIXString }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag { tnum = 165, tparser = toFIXChar }

tSettlLocation :: FIXTag
tSettlLocation = FIXTag { tnum = 166, tparser = toFIXString }

tSecurityType :: FIXTag
tSecurityType = FIXTag { tnum = 167, tparser = toFIXString }

tEffectiveTime :: FIXTag
tEffectiveTime = FIXTag { tnum = 168, tparser = toFIXUTCTimestamp }

tStandInstDbType :: FIXTag
tStandInstDbType = FIXTag { tnum = 169, tparser = toFIXInt }

tStandInstDbName :: FIXTag
tStandInstDbName = FIXTag { tnum = 170, tparser = toFIXString }

tStandInstDbID :: FIXTag
tStandInstDbID = FIXTag { tnum = 171, tparser = toFIXString }

tSettlDeliveryType :: FIXTag
tSettlDeliveryType = FIXTag { tnum = 172, tparser = toFIXInt }

tSettlDepositoryCode :: FIXTag
tSettlDepositoryCode = FIXTag { tnum = 173, tparser = toFIXString }

tSettlBrkrCode :: FIXTag
tSettlBrkrCode = FIXTag { tnum = 174, tparser = toFIXString }

tSettlInstCode :: FIXTag
tSettlInstCode = FIXTag { tnum = 175, tparser = toFIXString }

tSecuritySettlAgentName :: FIXTag
tSecuritySettlAgentName = FIXTag { tnum = 176, tparser = toFIXString }

tSecuritySettlAgentCode :: FIXTag
tSecuritySettlAgentCode = FIXTag { tnum = 177, tparser = toFIXString }

tSecuritySettlAgentAcctNum :: FIXTag
tSecuritySettlAgentAcctNum = FIXTag { tnum = 178, tparser = toFIXString }

tSecuritySettlAgentAcctName :: FIXTag
tSecuritySettlAgentAcctName = FIXTag { tnum = 179, tparser = toFIXString }

tSecuritySettlAgentContactName :: FIXTag
tSecuritySettlAgentContactName = FIXTag { tnum = 180, tparser = toFIXString }

tSecuritySettlAgentContactPhone :: FIXTag
tSecuritySettlAgentContactPhone = FIXTag { tnum = 181, tparser = toFIXString }

tCashSettlAgentName :: FIXTag
tCashSettlAgentName = FIXTag { tnum = 182, tparser = toFIXString }

tCashSettlAgentCode :: FIXTag
tCashSettlAgentCode = FIXTag { tnum = 183, tparser = toFIXString }

tCashSettlAgentAcctNum :: FIXTag
tCashSettlAgentAcctNum = FIXTag { tnum = 184, tparser = toFIXString }

tCashSettlAgentAcctName :: FIXTag
tCashSettlAgentAcctName = FIXTag { tnum = 185, tparser = toFIXString }

tCashSettlAgentContactName :: FIXTag
tCashSettlAgentContactName = FIXTag { tnum = 186, tparser = toFIXString }

tCashSettlAgentContactPhone :: FIXTag
tCashSettlAgentContactPhone = FIXTag { tnum = 187, tparser = toFIXString }

tBidSpotRate :: FIXTag
tBidSpotRate = FIXTag { tnum = 188, tparser = toFIXPrice }

tBidForwardPoints :: FIXTag
tBidForwardPoints = FIXTag { tnum = 189, tparser = toFIXPriceOffset }

tOfferSpotRate :: FIXTag
tOfferSpotRate = FIXTag { tnum = 190, tparser = toFIXPrice }

tOfferForwardPoints :: FIXTag
tOfferForwardPoints = FIXTag { tnum = 191, tparser = toFIXPriceOffset }

tOrderQty2 :: FIXTag
tOrderQty2 = FIXTag { tnum = 192, tparser = toFIXQuantity }

tFutSettDate2 :: FIXTag
tFutSettDate2 = FIXTag { tnum = 193, tparser = toFIXLocalMktDate }

tLastSpotRate :: FIXTag
tLastSpotRate = FIXTag { tnum = 194, tparser = toFIXPrice }

tLastForwardPoints :: FIXTag
tLastForwardPoints = FIXTag { tnum = 195, tparser = toFIXPriceOffset }

tAllocLinkID :: FIXTag
tAllocLinkID = FIXTag { tnum = 196, tparser = toFIXString }

tAllocLinkType :: FIXTag
tAllocLinkType = FIXTag { tnum = 197, tparser = toFIXInt }

tSecondaryOrderID :: FIXTag
tSecondaryOrderID = FIXTag { tnum = 198, tparser = toFIXString }

tNoIOIQualifiers :: FIXTag
tNoIOIQualifiers = FIXTag { tnum = 199, tparser = toFIXInt }

tMaturityMonthYear :: FIXTag
tMaturityMonthYear = FIXTag { tnum = 200, tparser = toFIXString }

tPutOrCall :: FIXTag
tPutOrCall = FIXTag { tnum = 201, tparser = toFIXInt }

tStrikePrice :: FIXTag
tStrikePrice = FIXTag { tnum = 202, tparser = toFIXPrice }

tCoveredOrUncovered :: FIXTag
tCoveredOrUncovered = FIXTag { tnum = 203, tparser = toFIXInt }

tCustomerOrFirm :: FIXTag
tCustomerOrFirm = FIXTag { tnum = 204, tparser = toFIXInt }

tMaturityDay :: FIXTag
tMaturityDay = FIXTag { tnum = 205, tparser = toFIXDayOfMonth }

tOptAttribute :: FIXTag
tOptAttribute = FIXTag { tnum = 206, tparser = toFIXChar }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag { tnum = 207, tparser = toFIXExchange }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag { tnum = 208, tparser = toFIXBool }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag { tnum = 209, tparser = toFIXInt }

tMaxShow :: FIXTag
tMaxShow = FIXTag { tnum = 210, tparser = toFIXQuantity }

tPegDifference :: FIXTag
tPegDifference = FIXTag { tnum = 211, tparser = toFIXPriceOffset }

tXmlDataLen :: FIXTag
tXmlDataLen = FIXTag { tnum = 212, tparser = toFIXString }

tXmlData :: FIXTag
tXmlData = FIXTag { tnum = 213, tparser = toFIXString }

tSettlInstRefID :: FIXTag
tSettlInstRefID = FIXTag { tnum = 214, tparser = toFIXString }

tNoRoutingIDs :: FIXTag
tNoRoutingIDs = FIXTag { tnum = 215, tparser = toFIXInt }

tRoutingType :: FIXTag
tRoutingType = FIXTag { tnum = 216, tparser = toFIXInt }

tRoutingID :: FIXTag
tRoutingID = FIXTag { tnum = 217, tparser = toFIXString }

tSpreadToBenchmark :: FIXTag
tSpreadToBenchmark = FIXTag { tnum = 218, tparser = toFIXPriceOffset }

tBenchmark :: FIXTag
tBenchmark = FIXTag { tnum = 219, tparser = toFIXChar }

tCouponRate :: FIXTag
tCouponRate = FIXTag { tnum = 223, tparser = toFIXFloat }

tContractMultiplier :: FIXTag
tContractMultiplier = FIXTag { tnum = 231, tparser = toFIXFloat }

tMDReqID :: FIXTag
tMDReqID = FIXTag { tnum = 262, tparser = toFIXString }

tSubscriptionRequestType :: FIXTag
tSubscriptionRequestType = FIXTag { tnum = 263, tparser = toFIXChar }

tMarketDepth :: FIXTag
tMarketDepth = FIXTag { tnum = 264, tparser = toFIXInt }

tMDUpdateType :: FIXTag
tMDUpdateType = FIXTag { tnum = 265, tparser = toFIXInt }

tAggregatedBook :: FIXTag
tAggregatedBook = FIXTag { tnum = 266, tparser = toFIXBool }

tNoMDEntryTypes :: FIXTag
tNoMDEntryTypes = FIXTag { tnum = 267, tparser = toFIXInt }

tNoMDEntries :: FIXTag
tNoMDEntries = FIXTag { tnum = 268, tparser = toFIXInt }

tMDEntryType :: FIXTag
tMDEntryType = FIXTag { tnum = 269, tparser = toFIXChar }

tMDEntryPx :: FIXTag
tMDEntryPx = FIXTag { tnum = 270, tparser = toFIXPrice }

tMDEntrySize :: FIXTag
tMDEntrySize = FIXTag { tnum = 271, tparser = toFIXQuantity }

tMDEntryDate :: FIXTag
tMDEntryDate = FIXTag { tnum = 272, tparser = toFIXString }

tMDEntryTime :: FIXTag
tMDEntryTime = FIXTag { tnum = 273, tparser = toFIXUTCTimeOnly }

tTickDirection :: FIXTag
tTickDirection = FIXTag { tnum = 274, tparser = toFIXChar }

tMDMkt :: FIXTag
tMDMkt = FIXTag { tnum = 275, tparser = toFIXExchange }

tQuoteCondition :: FIXTag
tQuoteCondition = FIXTag { tnum = 276, tparser = toFIXMultipleValueString }

tTradeCondition :: FIXTag
tTradeCondition = FIXTag { tnum = 277, tparser = toFIXMultipleValueString }

tMDEntryID :: FIXTag
tMDEntryID = FIXTag { tnum = 278, tparser = toFIXString }

tMDUpdateAction :: FIXTag
tMDUpdateAction = FIXTag { tnum = 279, tparser = toFIXChar }

tMDEntryRefID :: FIXTag
tMDEntryRefID = FIXTag { tnum = 280, tparser = toFIXString }

tMDReqRejReason :: FIXTag
tMDReqRejReason = FIXTag { tnum = 281, tparser = toFIXChar }

tMDEntryOriginator :: FIXTag
tMDEntryOriginator = FIXTag { tnum = 282, tparser = toFIXString }

tLocationID :: FIXTag
tLocationID = FIXTag { tnum = 283, tparser = toFIXString }

tDeskID :: FIXTag
tDeskID = FIXTag { tnum = 284, tparser = toFIXString }

tDeleteReason :: FIXTag
tDeleteReason = FIXTag { tnum = 285, tparser = toFIXChar }

tOpenCloseSettleFlag :: FIXTag
tOpenCloseSettleFlag = FIXTag { tnum = 286, tparser = toFIXChar }

tSellerDays :: FIXTag
tSellerDays = FIXTag { tnum = 287, tparser = toFIXInt }

tMDEntryBuyer :: FIXTag
tMDEntryBuyer = FIXTag { tnum = 288, tparser = toFIXString }

tMDEntrySeller :: FIXTag
tMDEntrySeller = FIXTag { tnum = 289, tparser = toFIXString }

tMDEntryPositionNo :: FIXTag
tMDEntryPositionNo = FIXTag { tnum = 290, tparser = toFIXInt }

tFinancialStatus :: FIXTag
tFinancialStatus = FIXTag { tnum = 291, tparser = toFIXChar }

tCorporateAction :: FIXTag
tCorporateAction = FIXTag { tnum = 292, tparser = toFIXChar }

tDefBidSize :: FIXTag
tDefBidSize = FIXTag { tnum = 293, tparser = toFIXQuantity }

tDefOfferSize :: FIXTag
tDefOfferSize = FIXTag { tnum = 294, tparser = toFIXQuantity }

tNoQuoteEntries :: FIXTag
tNoQuoteEntries = FIXTag { tnum = 295, tparser = toFIXInt }

tNoQuoteSets :: FIXTag
tNoQuoteSets = FIXTag { tnum = 296, tparser = toFIXInt }

tQuoteAckStatus :: FIXTag
tQuoteAckStatus = FIXTag { tnum = 297, tparser = toFIXInt }

tQuoteCancelType :: FIXTag
tQuoteCancelType = FIXTag { tnum = 298, tparser = toFIXInt }

tQuoteEntryID :: FIXTag
tQuoteEntryID = FIXTag { tnum = 299, tparser = toFIXString }

tQuoteRejectReason :: FIXTag
tQuoteRejectReason = FIXTag { tnum = 300, tparser = toFIXInt }

tQuoteResponseLevel :: FIXTag
tQuoteResponseLevel = FIXTag { tnum = 301, tparser = toFIXInt }

tQuoteSetID :: FIXTag
tQuoteSetID = FIXTag { tnum = 302, tparser = toFIXString }

tQuoteRequestType :: FIXTag
tQuoteRequestType = FIXTag { tnum = 303, tparser = toFIXInt }

tTotQuoteEntries :: FIXTag
tTotQuoteEntries = FIXTag { tnum = 304, tparser = toFIXInt }

tUnderlyingIDSource :: FIXTag
tUnderlyingIDSource = FIXTag { tnum = 305, tparser = toFIXString }

tUnderlyingIssuer :: FIXTag
tUnderlyingIssuer = FIXTag { tnum = 306, tparser = toFIXString }

tUnderlyingSecurityDesc :: FIXTag
tUnderlyingSecurityDesc = FIXTag { tnum = 307, tparser = toFIXString }

tUnderlyingSecurityExchange :: FIXTag
tUnderlyingSecurityExchange = FIXTag { tnum = 308, tparser = toFIXExchange }

tUnderlyingSecurityID :: FIXTag
tUnderlyingSecurityID = FIXTag { tnum = 309, tparser = toFIXString }

tUnderlyingSecurityType :: FIXTag
tUnderlyingSecurityType = FIXTag { tnum = 310, tparser = toFIXString }

tUnderlyingSymbol :: FIXTag
tUnderlyingSymbol = FIXTag { tnum = 311, tparser = toFIXString }

tUnderlyingSymbolSfx :: FIXTag
tUnderlyingSymbolSfx = FIXTag { tnum = 312, tparser = toFIXString }

tUnderlyingMaturityMonthYear :: FIXTag
tUnderlyingMaturityMonthYear = FIXTag { tnum = 313, tparser = toFIXString }

tUnderlyingMaturityDay :: FIXTag
tUnderlyingMaturityDay = FIXTag { tnum = 314, tparser = toFIXDayOfMonth }

tUnderlyingPutOrCall :: FIXTag
tUnderlyingPutOrCall = FIXTag { tnum = 315, tparser = toFIXInt }

tUnderlyingStrikePrice :: FIXTag
tUnderlyingStrikePrice = FIXTag { tnum = 316, tparser = toFIXPrice }

tUnderlyingOptAttribute :: FIXTag
tUnderlyingOptAttribute = FIXTag { tnum = 317, tparser = toFIXChar }

tUnderlyingCurrency :: FIXTag
tUnderlyingCurrency = FIXTag { tnum = 318, tparser = toFIXCurrency }

tRatioQty :: FIXTag
tRatioQty = FIXTag { tnum = 319, tparser = toFIXString }

tSecurityReqID :: FIXTag
tSecurityReqID = FIXTag { tnum = 320, tparser = toFIXString }

tSecurityRequestType :: FIXTag
tSecurityRequestType = FIXTag { tnum = 321, tparser = toFIXInt }

tSecurityResponseID :: FIXTag
tSecurityResponseID = FIXTag { tnum = 322, tparser = toFIXString }

tSecurityResponseType :: FIXTag
tSecurityResponseType = FIXTag { tnum = 323, tparser = toFIXInt }

tSecurityStatusReqID :: FIXTag
tSecurityStatusReqID = FIXTag { tnum = 324, tparser = toFIXString }

tUnsolicitedIndicator :: FIXTag
tUnsolicitedIndicator = FIXTag { tnum = 325, tparser = toFIXBool }

tSecurityTradingStatus :: FIXTag
tSecurityTradingStatus = FIXTag { tnum = 326, tparser = toFIXInt }

tHaltReasonChar :: FIXTag
tHaltReasonChar = FIXTag { tnum = 327, tparser = toFIXChar }

tInViewOfCommon :: FIXTag
tInViewOfCommon = FIXTag { tnum = 328, tparser = toFIXBool }

tDueToRelated :: FIXTag
tDueToRelated = FIXTag { tnum = 329, tparser = toFIXBool }

tBuyVolume :: FIXTag
tBuyVolume = FIXTag { tnum = 330, tparser = toFIXQuantity }

tSellVolume :: FIXTag
tSellVolume = FIXTag { tnum = 331, tparser = toFIXQuantity }

tHighPx :: FIXTag
tHighPx = FIXTag { tnum = 332, tparser = toFIXPrice }

tLowPx :: FIXTag
tLowPx = FIXTag { tnum = 333, tparser = toFIXPrice }

tAdjustment :: FIXTag
tAdjustment = FIXTag { tnum = 334, tparser = toFIXInt }

tTradSesReqID :: FIXTag
tTradSesReqID = FIXTag { tnum = 335, tparser = toFIXString }

tTradingSessionID :: FIXTag
tTradingSessionID = FIXTag { tnum = 336, tparser = toFIXString }

tContraTrader :: FIXTag
tContraTrader = FIXTag { tnum = 337, tparser = toFIXString }

tTradSesMethod :: FIXTag
tTradSesMethod = FIXTag { tnum = 338, tparser = toFIXInt }

tTradSesMode :: FIXTag
tTradSesMode = FIXTag { tnum = 339, tparser = toFIXInt }

tTradSesStatus :: FIXTag
tTradSesStatus = FIXTag { tnum = 340, tparser = toFIXInt }

tTradSesStartTime :: FIXTag
tTradSesStartTime = FIXTag { tnum = 341, tparser = toFIXUTCTimestamp }

tTradSesOpenTime :: FIXTag
tTradSesOpenTime = FIXTag { tnum = 342, tparser = toFIXUTCTimestamp }

tTradSesPreCloseTime :: FIXTag
tTradSesPreCloseTime = FIXTag { tnum = 343, tparser = toFIXUTCTimestamp }

tTradSesCloseTime :: FIXTag
tTradSesCloseTime = FIXTag { tnum = 344, tparser = toFIXUTCTimestamp }

tTradSesEndTime :: FIXTag
tTradSesEndTime = FIXTag { tnum = 345, tparser = toFIXUTCTimestamp }

tNumberOfOrders :: FIXTag
tNumberOfOrders = FIXTag { tnum = 346, tparser = toFIXInt }

tMessageEncoding :: FIXTag
tMessageEncoding = FIXTag { tnum = 347, tparser = toFIXString }

tEncodedIssuerLen :: FIXTag
tEncodedIssuerLen = FIXTag { tnum = 348, tparser = toFIXString }

tEncodedIssuer :: FIXTag
tEncodedIssuer = FIXTag { tnum = 349, tparser = toFIXString }

tEncodedSecurityDescLen :: FIXTag
tEncodedSecurityDescLen = FIXTag { tnum = 350, tparser = toFIXString }

tEncodedSecurityDesc :: FIXTag
tEncodedSecurityDesc = FIXTag { tnum = 351, tparser = toFIXString }

tEncodedListExecInstLen :: FIXTag
tEncodedListExecInstLen = FIXTag { tnum = 352, tparser = toFIXString }

tEncodedListExecInst :: FIXTag
tEncodedListExecInst = FIXTag { tnum = 353, tparser = toFIXString }

tEncodedTextLen :: FIXTag
tEncodedTextLen = FIXTag { tnum = 354, tparser = toFIXString }

tEncodedText :: FIXTag
tEncodedText = FIXTag { tnum = 355, tparser = toFIXString }

tEncodedSubjectLen :: FIXTag
tEncodedSubjectLen = FIXTag { tnum = 356, tparser = toFIXString }

tEncodedSubject :: FIXTag
tEncodedSubject = FIXTag { tnum = 357, tparser = toFIXString }

tEncodedHeadlineLen :: FIXTag
tEncodedHeadlineLen = FIXTag { tnum = 358, tparser = toFIXString }

tEncodedHeadline :: FIXTag
tEncodedHeadline = FIXTag { tnum = 359, tparser = toFIXString }

tEncodedAllocTextLen :: FIXTag
tEncodedAllocTextLen = FIXTag { tnum = 360, tparser = toFIXString }

tEncodedAllocText :: FIXTag
tEncodedAllocText = FIXTag { tnum = 361, tparser = toFIXString }

tEncodedUnderlyingIssuerLen :: FIXTag
tEncodedUnderlyingIssuerLen = FIXTag { tnum = 362, tparser = toFIXString }

tEncodedUnderlyingIssuer :: FIXTag
tEncodedUnderlyingIssuer = FIXTag { tnum = 363, tparser = toFIXString }

tEncodedUnderlyingSecurityDescLen :: FIXTag
tEncodedUnderlyingSecurityDescLen = FIXTag { tnum = 364, tparser = toFIXString }

tEncodedUnderlyingSecurityDesc :: FIXTag
tEncodedUnderlyingSecurityDesc = FIXTag { tnum = 365, tparser = toFIXString }

tAllocPrice :: FIXTag
tAllocPrice = FIXTag { tnum = 366, tparser = toFIXPrice }

tQuoteSetValidUntilTime :: FIXTag
tQuoteSetValidUntilTime = FIXTag { tnum = 367, tparser = toFIXUTCTimestamp }

tQuoteEntryRejectReason :: FIXTag
tQuoteEntryRejectReason = FIXTag { tnum = 368, tparser = toFIXInt }

tLastMsgSeqNumProcessed :: FIXTag
tLastMsgSeqNumProcessed = FIXTag { tnum = 369, tparser = toFIXInt }

tOnBehalfOfSendingTime :: FIXTag
tOnBehalfOfSendingTime = FIXTag { tnum = 370, tparser = toFIXUTCTimestamp }

tRefTagID :: FIXTag
tRefTagID = FIXTag { tnum = 371, tparser = toFIXInt }

tRefMsgType :: FIXTag
tRefMsgType = FIXTag { tnum = 372, tparser = toFIXString }

tSessionRejectReason :: FIXTag
tSessionRejectReason = FIXTag { tnum = 373, tparser = toFIXInt }

tBidRequestTransType :: FIXTag
tBidRequestTransType = FIXTag { tnum = 374, tparser = toFIXChar }

tContraBroker :: FIXTag
tContraBroker = FIXTag { tnum = 375, tparser = toFIXString }

tComplianceID :: FIXTag
tComplianceID = FIXTag { tnum = 376, tparser = toFIXString }

tSolicitedFlag :: FIXTag
tSolicitedFlag = FIXTag { tnum = 377, tparser = toFIXBool }

tExecRestatementReason :: FIXTag
tExecRestatementReason = FIXTag { tnum = 378, tparser = toFIXInt }

tBusinessRejectRefID :: FIXTag
tBusinessRejectRefID = FIXTag { tnum = 379, tparser = toFIXString }

tBusinessRejectReason :: FIXTag
tBusinessRejectReason = FIXTag { tnum = 380, tparser = toFIXInt }

tGrossTradeAmt :: FIXTag
tGrossTradeAmt = FIXTag { tnum = 381, tparser = toFIXAmt }

tNoContraBrokers :: FIXTag
tNoContraBrokers = FIXTag { tnum = 382, tparser = toFIXInt }

tMaxMessageSize :: FIXTag
tMaxMessageSize = FIXTag { tnum = 383, tparser = toFIXInt }

tNoMsgTypes :: FIXTag
tNoMsgTypes = FIXTag { tnum = 384, tparser = toFIXInt }

tMsgDirection :: FIXTag
tMsgDirection = FIXTag { tnum = 385, tparser = toFIXChar }

tNoTradingSessions :: FIXTag
tNoTradingSessions = FIXTag { tnum = 386, tparser = toFIXInt }

tTotalVolumeTraded :: FIXTag
tTotalVolumeTraded = FIXTag { tnum = 387, tparser = toFIXQuantity }

tDiscretionInst :: FIXTag
tDiscretionInst = FIXTag { tnum = 388, tparser = toFIXChar }

tDiscretionOffset :: FIXTag
tDiscretionOffset = FIXTag { tnum = 389, tparser = toFIXPriceOffset }

tBidID :: FIXTag
tBidID = FIXTag { tnum = 390, tparser = toFIXString }

tClientBidID :: FIXTag
tClientBidID = FIXTag { tnum = 391, tparser = toFIXString }

tListName :: FIXTag
tListName = FIXTag { tnum = 392, tparser = toFIXString }

tTotalNumSecurities :: FIXTag
tTotalNumSecurities = FIXTag { tnum = 393, tparser = toFIXInt }

tBidType :: FIXTag
tBidType = FIXTag { tnum = 394, tparser = toFIXInt }

tNumTickets :: FIXTag
tNumTickets = FIXTag { tnum = 395, tparser = toFIXInt }

tSideValue1 :: FIXTag
tSideValue1 = FIXTag { tnum = 396, tparser = toFIXAmt }

tSideValue2 :: FIXTag
tSideValue2 = FIXTag { tnum = 397, tparser = toFIXAmt }

tNoBidDescriptors :: FIXTag
tNoBidDescriptors = FIXTag { tnum = 398, tparser = toFIXInt }

tBidDescriptorType :: FIXTag
tBidDescriptorType = FIXTag { tnum = 399, tparser = toFIXInt }

tBidDescriptor :: FIXTag
tBidDescriptor = FIXTag { tnum = 400, tparser = toFIXString }

tSideValueInd :: FIXTag
tSideValueInd = FIXTag { tnum = 401, tparser = toFIXInt }

tLiquidityPctLow :: FIXTag
tLiquidityPctLow = FIXTag { tnum = 402, tparser = toFIXFloat }

tLiquidityPctHigh :: FIXTag
tLiquidityPctHigh = FIXTag { tnum = 403, tparser = toFIXFloat }

tLiquidityValue :: FIXTag
tLiquidityValue = FIXTag { tnum = 404, tparser = toFIXAmt }

tEFPTrackingError :: FIXTag
tEFPTrackingError = FIXTag { tnum = 405, tparser = toFIXFloat }

tFairValue :: FIXTag
tFairValue = FIXTag { tnum = 406, tparser = toFIXAmt }

tOutsideIndexPct :: FIXTag
tOutsideIndexPct = FIXTag { tnum = 407, tparser = toFIXFloat }

tValueOfFutures :: FIXTag
tValueOfFutures = FIXTag { tnum = 408, tparser = toFIXAmt }

tLiquidityIndType :: FIXTag
tLiquidityIndType = FIXTag { tnum = 409, tparser = toFIXInt }

tWtAverageLiquidity :: FIXTag
tWtAverageLiquidity = FIXTag { tnum = 410, tparser = toFIXFloat }

tExchangeForPhysical :: FIXTag
tExchangeForPhysical = FIXTag { tnum = 411, tparser = toFIXBool }

tOutMainCntryUIndex :: FIXTag
tOutMainCntryUIndex = FIXTag { tnum = 412, tparser = toFIXAmt }

tCrossPercent :: FIXTag
tCrossPercent = FIXTag { tnum = 413, tparser = toFIXFloat }

tProgRptReqs :: FIXTag
tProgRptReqs = FIXTag { tnum = 414, tparser = toFIXInt }

tProgPeriodInterval :: FIXTag
tProgPeriodInterval = FIXTag { tnum = 415, tparser = toFIXInt }

tIncTaxInd :: FIXTag
tIncTaxInd = FIXTag { tnum = 416, tparser = toFIXInt }

tNumBidders :: FIXTag
tNumBidders = FIXTag { tnum = 417, tparser = toFIXInt }

tTradeType :: FIXTag
tTradeType = FIXTag { tnum = 418, tparser = toFIXChar }

tBasisPxType :: FIXTag
tBasisPxType = FIXTag { tnum = 419, tparser = toFIXChar }

tNoBidComponents :: FIXTag
tNoBidComponents = FIXTag { tnum = 420, tparser = toFIXInt }

tCountry :: FIXTag
tCountry = FIXTag { tnum = 421, tparser = toFIXString }

tTotNoStrikes :: FIXTag
tTotNoStrikes = FIXTag { tnum = 422, tparser = toFIXInt }

tPriceType :: FIXTag
tPriceType = FIXTag { tnum = 423, tparser = toFIXInt }

tDayOrderQty :: FIXTag
tDayOrderQty = FIXTag { tnum = 424, tparser = toFIXQuantity }

tDayCumQty :: FIXTag
tDayCumQty = FIXTag { tnum = 425, tparser = toFIXQuantity }

tDayAvgPx :: FIXTag
tDayAvgPx = FIXTag { tnum = 426, tparser = toFIXPrice }

tGTBookingInst :: FIXTag
tGTBookingInst = FIXTag { tnum = 427, tparser = toFIXInt }

tNoStrikes :: FIXTag
tNoStrikes = FIXTag { tnum = 428, tparser = toFIXInt }

tListStatusType :: FIXTag
tListStatusType = FIXTag { tnum = 429, tparser = toFIXInt }

tNetGrossInd :: FIXTag
tNetGrossInd = FIXTag { tnum = 430, tparser = toFIXInt }

tListOrderStatus :: FIXTag
tListOrderStatus = FIXTag { tnum = 431, tparser = toFIXInt }

tExpireDate :: FIXTag
tExpireDate = FIXTag { tnum = 432, tparser = toFIXLocalMktDate }

tListExecInstType :: FIXTag
tListExecInstType = FIXTag { tnum = 433, tparser = toFIXChar }

tCxlRejResponseTo :: FIXTag
tCxlRejResponseTo = FIXTag { tnum = 434, tparser = toFIXChar }

tUnderlyingCouponRate :: FIXTag
tUnderlyingCouponRate = FIXTag { tnum = 435, tparser = toFIXFloat }

tUnderlyingContractMultiplier :: FIXTag
tUnderlyingContractMultiplier = FIXTag { tnum = 436, tparser = toFIXFloat }

tContraTradeQty :: FIXTag
tContraTradeQty = FIXTag { tnum = 437, tparser = toFIXQuantity }

tContraTradeTime :: FIXTag
tContraTradeTime = FIXTag { tnum = 438, tparser = toFIXUTCTimestamp }

tClearingFirm :: FIXTag
tClearingFirm = FIXTag { tnum = 439, tparser = toFIXString }

tClearingAccount :: FIXTag
tClearingAccount = FIXTag { tnum = 440, tparser = toFIXString }

tLiquidityNumSecurities :: FIXTag
tLiquidityNumSecurities = FIXTag { tnum = 441, tparser = toFIXInt }

tMultiLegReportingType :: FIXTag
tMultiLegReportingType = FIXTag { tnum = 442, tparser = toFIXChar }

tStrikeTime :: FIXTag
tStrikeTime = FIXTag { tnum = 443, tparser = toFIXUTCTimestamp }

tListStatusText :: FIXTag
tListStatusText = FIXTag { tnum = 444, tparser = toFIXString }

tEncodedListStatusTextLen :: FIXTag
tEncodedListStatusTextLen = FIXTag { tnum = 445, tparser = toFIXString }

tEncodedListStatusText :: FIXTag
tEncodedListStatusText = FIXTag { tnum = 446, tparser = toFIXString }

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
    LT.insert (tnum tOnBehalfOfSendingTime) tOnBehalfOfSendingTime LT.new

trailerFIX42 :: FIXTags
trailerFIX42 = 
    LT.insert (tnum tSignatureLength) tSignatureLength $
    LT.insert (tnum tSignature) tSignature $
    LT.insert (tnum tCheckSum) tCheckSum LT.new

mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { msType = C.pack "0"
   , msHeader = headerFIX42
   , msBody = mHeartbeatBody
   , msTrailer = trailerFIX42 }
   where
      mHeartbeatBody = 
          LT.insert (tnum tTestReqID) tTestReqID LT.new

mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { msType = C.pack "1"
   , msHeader = headerFIX42
   , msBody = mTestRequestBody
   , msTrailer = trailerFIX42 }
   where
      mTestRequestBody = 
          LT.insert (tnum tTestReqID) tTestReqID LT.new

mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { msType = C.pack "2"
   , msHeader = headerFIX42
   , msBody = mResendRequestBody
   , msTrailer = trailerFIX42 }
   where
      mResendRequestBody = 
          LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
          LT.insert (tnum tEndSeqNo) tEndSeqNo LT.new

mReject :: FIXMessageSpec
mReject = FMSpec
   { msType = C.pack "3"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec
   { msType = C.pack "4"
   , msHeader = headerFIX42
   , msBody = mSequenceResetBody
   , msTrailer = trailerFIX42 }
   where
      mSequenceResetBody = 
          LT.insert (tnum tGapFillFlag) tGapFillFlag $
          LT.insert (tnum tNewSeqNo) tNewSeqNo LT.new

mLogout :: FIXMessageSpec
mLogout = FMSpec
   { msType = C.pack "5"
   , msHeader = headerFIX42
   , msBody = mLogoutBody
   , msTrailer = trailerFIX42 }
   where
      mLogoutBody = 
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mIndicationofInterest :: FIXMessageSpec
mIndicationofInterest = FMSpec
   { msType = C.pack "6"
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
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tSpreadToBenchmark) tSpreadToBenchmark $
          LT.insert (tnum tBenchmark) tBenchmark LT.new

mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { msType = C.pack "7"
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
          LT.insert (tnum tTradingSessionID) tTradingSessionID LT.new

mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { msType = C.pack "8"
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
          LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType LT.new

mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { msType = C.pack "9"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mLogon :: FIXMessageSpec
mLogon = FMSpec
   { msType = C.pack "A"
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
          LT.insert (tnum tMaxMessageSize) tMaxMessageSize LT.new

mNews :: FIXMessageSpec
mNews = FMSpec
   { msType = C.pack "B"
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
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData LT.new

mEmail :: FIXMessageSpec
mEmail = FMSpec
   { msType = C.pack "C"
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
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData LT.new

mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec
   { msType = C.pack "D"
   , msHeader = headerFIX42
   , msBody = mNewOrderSingleBody
   , msTrailer = trailerFIX42 }
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
          LT.insert (tnum tClearingAccount) tClearingAccount LT.new

mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { msType = C.pack "E"
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
          LT.insert (tnum tTotNoOrders) tTotNoOrders LT.new

mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { msType = C.pack "F"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { msType = C.pack "G"
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
          LT.insert (tnum tClearingAccount) tClearingAccount LT.new

mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { msType = C.pack "H"
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
          LT.insert (tnum tSide) tSide LT.new

mAllocation :: FIXMessageSpec
mAllocation = FMSpec
   { msType = C.pack "J"
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
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate LT.new

mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { msType = C.pack "K"
   , msHeader = headerFIX42
   , msBody = mListCancelRequestBody
   , msTrailer = trailerFIX42 }
   where
      mListCancelRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { msType = C.pack "L"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec
   { msType = C.pack "M"
   , msHeader = headerFIX42
   , msBody = mListStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
      mListStatusRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mListStatus :: FIXMessageSpec
mListStatus = FMSpec
   { msType = C.pack "N"
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
          LT.insert (tnum tTotNoOrders) tTotNoOrders LT.new

mAllocationACK :: FIXMessageSpec
mAllocationACK = FMSpec
   { msType = C.pack "P"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { msType = C.pack "Q"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { msType = C.pack "R"
   , msHeader = headerFIX42
   , msBody = mQuoteRequestBody
   , msTrailer = trailerFIX42 }
   where
      mQuoteRequestBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID LT.new

mQuote :: FIXMessageSpec
mQuote = FMSpec
   { msType = C.pack "S"
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
          LT.insert (tnum tCurrency) tCurrency LT.new

mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { msType = C.pack "T"
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
          LT.insert (tnum tCashSettlAgentContactPhone) tCashSettlAgentContactPhone LT.new

mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec
   { msType = C.pack "V"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestBody
   , msTrailer = trailerFIX42 }
   where
      mMarketDataRequestBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tMarketDepth) tMarketDepth $
          LT.insert (tnum tMDUpdateType) tMDUpdateType $
          LT.insert (tnum tAggregatedBook) tAggregatedBook LT.new

mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec
   { msType = C.pack "W"
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
          LT.insert (tnum tTotalVolumeTraded) tTotalVolumeTraded LT.new

mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec
   { msType = C.pack "X"
   , msHeader = headerFIX42
   , msBody = mMarketDataIncrementalRefreshBody
   , msTrailer = trailerFIX42 }
   where
      mMarketDataIncrementalRefreshBody = 
          LT.insert (tnum tMDReqID) tMDReqID LT.new

mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec
   { msType = C.pack "Y"
   , msHeader = headerFIX42
   , msBody = mMarketDataRequestRejectBody
   , msTrailer = trailerFIX42 }
   where
      mMarketDataRequestRejectBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mQuoteCancel :: FIXMessageSpec
mQuoteCancel = FMSpec
   { msType = C.pack "Z"
   , msHeader = headerFIX42
   , msBody = mQuoteCancelBody
   , msTrailer = trailerFIX42 }
   where
      mQuoteCancelBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tTradingSessionID) tTradingSessionID LT.new

mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec
   { msType = C.pack "a"
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
          LT.insert (tnum tTradingSessionID) tTradingSessionID LT.new

mQuoteAcknowledgement :: FIXMessageSpec
mQuoteAcknowledgement = FMSpec
   { msType = C.pack "b"
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
          LT.insert (tnum tText) tText LT.new

mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec
   { msType = C.pack "c"
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
          LT.insert (tnum tTradingSessionID) tTradingSessionID LT.new

mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec
   { msType = C.pack "d"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec
   { msType = C.pack "e"
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
          LT.insert (tnum tTradingSessionID) tTradingSessionID LT.new

mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec
   { msType = C.pack "f"
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
          LT.insert (tnum tAdjustment) tAdjustment LT.new

mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec
   { msType = C.pack "g"
   , msHeader = headerFIX42
   , msBody = mTradingSessionStatusRequestBody
   , msTrailer = trailerFIX42 }
   where
      mTradingSessionStatusRequestBody = 
          LT.insert (tnum tTradSesReqID) tTradSesReqID $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradSesMethod) tTradSesMethod $
          LT.insert (tnum tTradSesMode) tTradSesMode $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType LT.new

mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec
   { msType = C.pack "h"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mMassQuote :: FIXMessageSpec
mMassQuote = FMSpec
   { msType = C.pack "i"
   , msHeader = headerFIX42
   , msBody = mMassQuoteBody
   , msTrailer = trailerFIX42 }
   where
      mMassQuoteBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tDefBidSize) tDefBidSize $
          LT.insert (tnum tDefOfferSize) tDefOfferSize LT.new

mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec
   { msType = C.pack "j"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mBidRequest :: FIXMessageSpec
mBidRequest = FMSpec
   { msType = C.pack "k"
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
          LT.insert (tnum tEncodedText) tEncodedText LT.new

mBidResponse :: FIXMessageSpec
mBidResponse = FMSpec
   { msType = C.pack "l"
   , msHeader = headerFIX42
   , msBody = mBidResponseBody
   , msTrailer = trailerFIX42 }
   where
      mBidResponseBody = 
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tClientBidID) tClientBidID LT.new

mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec
   { msType = C.pack "m"
   , msHeader = headerFIX42
   , msBody = mListStrikePriceBody
   , msTrailer = trailerFIX42 }
   where
      mListStrikePriceBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTotNoStrikes) tTotNoStrikes LT.new

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
          LT.insert (msType mListStrikePrice) mListStrikePrice LT.new 
