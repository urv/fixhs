module Data.FIX42.Tags where
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
tAdvSide = FIXTag { tnum = 4, tparser = toFIXString }

tAdvTransType :: FIXTag
tAdvTransType = FIXTag { tnum = 5, tparser = toFIXString }

tAvgPx :: FIXTag
tAvgPx = FIXTag { tnum = 6, tparser = toFIXPrice }

tBeginSeqNo :: FIXTag
tBeginSeqNo = FIXTag { tnum = 7, tparser = toFIXString }

tBeginString :: FIXTag
tBeginString = FIXTag { tnum = 8, tparser = toFIXString }

tBodyLength :: FIXTag
tBodyLength = FIXTag { tnum = 9, tparser = toFIXString }

tCheckSum :: FIXTag
tCheckSum = FIXTag { tnum = 10, tparser = toFIXString }

tClOrdID :: FIXTag
tClOrdID = FIXTag { tnum = 11, tparser = toFIXString }

tCommission :: FIXTag
tCommission = FIXTag { tnum = 12, tparser = toFIXAmt }

tCommType :: FIXTag
tCommType = FIXTag { tnum = 13, tparser = toFIXString }

tCumQty :: FIXTag
tCumQty = FIXTag { tnum = 14, tparser = toFIXString }

tCurrency :: FIXTag
tCurrency = FIXTag { tnum = 15, tparser = toFIXCurrency }

tEndSeqNo :: FIXTag
tEndSeqNo = FIXTag { tnum = 16, tparser = toFIXString }

tExecID :: FIXTag
tExecID = FIXTag { tnum = 17, tparser = toFIXString }

tExecInst :: FIXTag
tExecInst = FIXTag { tnum = 18, tparser = toFIXMultipleValueString }

tExecRefID :: FIXTag
tExecRefID = FIXTag { tnum = 19, tparser = toFIXString }

tExecTransType :: FIXTag
tExecTransType = FIXTag { tnum = 20, tparser = toFIXString }

tHandlInst :: FIXTag
tHandlInst = FIXTag { tnum = 21, tparser = toFIXString }

tSecurityIDSource :: FIXTag
tSecurityIDSource = FIXTag { tnum = 22, tparser = toFIXString }

tIOIID :: FIXTag
tIOIID = FIXTag { tnum = 23, tparser = toFIXString }

tIOIOthSvc :: FIXTag
tIOIOthSvc = FIXTag { tnum = 24, tparser = toFIXString }

tIOIQltyInd :: FIXTag
tIOIQltyInd = FIXTag { tnum = 25, tparser = toFIXString }

tIOIRefID :: FIXTag
tIOIRefID = FIXTag { tnum = 26, tparser = toFIXString }

tIOIQty :: FIXTag
tIOIQty = FIXTag { tnum = 27, tparser = toFIXString }

tIOITransType :: FIXTag
tIOITransType = FIXTag { tnum = 28, tparser = toFIXString }

tLastCapacity :: FIXTag
tLastCapacity = FIXTag { tnum = 29, tparser = toFIXString }

tLastMkt :: FIXTag
tLastMkt = FIXTag { tnum = 30, tparser = toFIXExchange }

tLastPx :: FIXTag
tLastPx = FIXTag { tnum = 31, tparser = toFIXPrice }

tLastQty :: FIXTag
tLastQty = FIXTag { tnum = 32, tparser = toFIXString }

tNoLinesOfText :: FIXTag
tNoLinesOfText = FIXTag { tnum = 33, tparser = toFIXString }

tMsgSeqNum :: FIXTag
tMsgSeqNum = FIXTag { tnum = 34, tparser = toFIXString }

tMsgType :: FIXTag
tMsgType = FIXTag { tnum = 35, tparser = toFIXString }

tNewSeqNo :: FIXTag
tNewSeqNo = FIXTag { tnum = 36, tparser = toFIXString }

tOrderID :: FIXTag
tOrderID = FIXTag { tnum = 37, tparser = toFIXString }

tOrderQty :: FIXTag
tOrderQty = FIXTag { tnum = 38, tparser = toFIXString }

tOrdStatus :: FIXTag
tOrdStatus = FIXTag { tnum = 39, tparser = toFIXString }

tOrdType :: FIXTag
tOrdType = FIXTag { tnum = 40, tparser = toFIXString }

tOrigClOrdID :: FIXTag
tOrigClOrdID = FIXTag { tnum = 41, tparser = toFIXString }

tOrigTime :: FIXTag
tOrigTime = FIXTag { tnum = 42, tparser = toFIXUTCTimestamp }

tPossDupFlag :: FIXTag
tPossDupFlag = FIXTag { tnum = 43, tparser = toFIXString }

tPrice :: FIXTag
tPrice = FIXTag { tnum = 44, tparser = toFIXPrice }

tRefSeqNum :: FIXTag
tRefSeqNum = FIXTag { tnum = 45, tparser = toFIXString }

tRelatdSym :: FIXTag
tRelatdSym = FIXTag { tnum = 46, tparser = toFIXString }

tRule80A :: FIXTag
tRule80A = FIXTag { tnum = 47, tparser = toFIXString }

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

tQuantity :: FIXTag
tQuantity = FIXTag { tnum = 53, tparser = toFIXString }

tSide :: FIXTag
tSide = FIXTag { tnum = 54, tparser = toFIXString }

tSymbol :: FIXTag
tSymbol = FIXTag { tnum = 55, tparser = toFIXString }

tTargetCompID :: FIXTag
tTargetCompID = FIXTag { tnum = 56, tparser = toFIXString }

tTargetSubID :: FIXTag
tTargetSubID = FIXTag { tnum = 57, tparser = toFIXString }

tText :: FIXTag
tText = FIXTag { tnum = 58, tparser = toFIXString }

tTimeInForce :: FIXTag
tTimeInForce = FIXTag { tnum = 59, tparser = toFIXString }

tTransactTime :: FIXTag
tTransactTime = FIXTag { tnum = 60, tparser = toFIXUTCTimestamp }

tUrgency :: FIXTag
tUrgency = FIXTag { tnum = 61, tparser = toFIXString }

tValidUntilTime :: FIXTag
tValidUntilTime = FIXTag { tnum = 62, tparser = toFIXUTCTimestamp }

tSettlType :: FIXTag
tSettlType = FIXTag { tnum = 63, tparser = toFIXString }

tSettlDate :: FIXTag
tSettlDate = FIXTag { tnum = 64, tparser = toFIXLocalMktDate }

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
tAllocTransType = FIXTag { tnum = 71, tparser = toFIXString }

tRefAllocID :: FIXTag
tRefAllocID = FIXTag { tnum = 72, tparser = toFIXString }

tNoOrders :: FIXTag
tNoOrders = FIXTag { tnum = 73, tparser = toFIXString }

tAvgPxPrecision :: FIXTag
tAvgPxPrecision = FIXTag { tnum = 74, tparser = toFIXInt }

tTradeDate :: FIXTag
tTradeDate = FIXTag { tnum = 75, tparser = toFIXLocalMktDate }

tExecBroker :: FIXTag
tExecBroker = FIXTag { tnum = 76, tparser = toFIXString }

tPositionEffect :: FIXTag
tPositionEffect = FIXTag { tnum = 77, tparser = toFIXString }

tNoAllocs :: FIXTag
tNoAllocs = FIXTag { tnum = 78, tparser = toFIXString }

tAllocAccount :: FIXTag
tAllocAccount = FIXTag { tnum = 79, tparser = toFIXString }

tAllocQty :: FIXTag
tAllocQty = FIXTag { tnum = 80, tparser = toFIXString }

tProcessCode :: FIXTag
tProcessCode = FIXTag { tnum = 81, tparser = toFIXString }

tNoRpts :: FIXTag
tNoRpts = FIXTag { tnum = 82, tparser = toFIXInt }

tRptSeq :: FIXTag
tRptSeq = FIXTag { tnum = 83, tparser = toFIXInt }

tCxlQty :: FIXTag
tCxlQty = FIXTag { tnum = 84, tparser = toFIXString }

tNoDlvyInst :: FIXTag
tNoDlvyInst = FIXTag { tnum = 85, tparser = toFIXString }

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
tEmailType = FIXTag { tnum = 94, tparser = toFIXString }

tRawDataLength :: FIXTag
tRawDataLength = FIXTag { tnum = 95, tparser = toFIXString }

tRawData :: FIXTag
tRawData = FIXTag { tnum = 96, tparser = toFIXString }

tPossResend :: FIXTag
tPossResend = FIXTag { tnum = 97, tparser = toFIXString }

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
tIOIQualifier = FIXTag { tnum = 104, tparser = toFIXString }

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
tMinQty = FIXTag { tnum = 110, tparser = toFIXString }

tMaxFloor :: FIXTag
tMaxFloor = FIXTag { tnum = 111, tparser = toFIXString }

tTestReqID :: FIXTag
tTestReqID = FIXTag { tnum = 112, tparser = toFIXString }

tReportToExch :: FIXTag
tReportToExch = FIXTag { tnum = 113, tparser = toFIXString }

tLocateReqd :: FIXTag
tLocateReqd = FIXTag { tnum = 114, tparser = toFIXString }

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
tForexReq = FIXTag { tnum = 121, tparser = toFIXString }

tOrigSendingTime :: FIXTag
tOrigSendingTime = FIXTag { tnum = 122, tparser = toFIXUTCTimestamp }

tGapFillFlag :: FIXTag
tGapFillFlag = FIXTag { tnum = 123, tparser = toFIXString }

tNoExecs :: FIXTag
tNoExecs = FIXTag { tnum = 124, tparser = toFIXString }

tCxlType :: FIXTag
tCxlType = FIXTag { tnum = 125, tparser = toFIXString }

tExpireTime :: FIXTag
tExpireTime = FIXTag { tnum = 126, tparser = toFIXUTCTimestamp }

tDKReason :: FIXTag
tDKReason = FIXTag { tnum = 127, tparser = toFIXString }

tDeliverToCompID :: FIXTag
tDeliverToCompID = FIXTag { tnum = 128, tparser = toFIXString }

tDeliverToSubID :: FIXTag
tDeliverToSubID = FIXTag { tnum = 129, tparser = toFIXString }

tIOINaturalFlag :: FIXTag
tIOINaturalFlag = FIXTag { tnum = 130, tparser = toFIXString }

tQuoteReqID :: FIXTag
tQuoteReqID = FIXTag { tnum = 131, tparser = toFIXString }

tBidPx :: FIXTag
tBidPx = FIXTag { tnum = 132, tparser = toFIXPrice }

tOfferPx :: FIXTag
tOfferPx = FIXTag { tnum = 133, tparser = toFIXPrice }

tBidSize :: FIXTag
tBidSize = FIXTag { tnum = 134, tparser = toFIXString }

tOfferSize :: FIXTag
tOfferSize = FIXTag { tnum = 135, tparser = toFIXString }

tNoMiscFees :: FIXTag
tNoMiscFees = FIXTag { tnum = 136, tparser = toFIXString }

tMiscFeeAmt :: FIXTag
tMiscFeeAmt = FIXTag { tnum = 137, tparser = toFIXAmt }

tMiscFeeCurr :: FIXTag
tMiscFeeCurr = FIXTag { tnum = 138, tparser = toFIXCurrency }

tMiscFeeType :: FIXTag
tMiscFeeType = FIXTag { tnum = 139, tparser = toFIXString }

tPrevClosePx :: FIXTag
tPrevClosePx = FIXTag { tnum = 140, tparser = toFIXPrice }

tResetSeqNumFlag :: FIXTag
tResetSeqNumFlag = FIXTag { tnum = 141, tparser = toFIXString }

tSenderLocationID :: FIXTag
tSenderLocationID = FIXTag { tnum = 142, tparser = toFIXString }

tTargetLocationID :: FIXTag
tTargetLocationID = FIXTag { tnum = 143, tparser = toFIXString }

tOnBehalfOfLocationID :: FIXTag
tOnBehalfOfLocationID = FIXTag { tnum = 144, tparser = toFIXString }

tDeliverToLocationID :: FIXTag
tDeliverToLocationID = FIXTag { tnum = 145, tparser = toFIXString }

tNoRelatedSym :: FIXTag
tNoRelatedSym = FIXTag { tnum = 146, tparser = toFIXString }

tSubject :: FIXTag
tSubject = FIXTag { tnum = 147, tparser = toFIXString }

tHeadline :: FIXTag
tHeadline = FIXTag { tnum = 148, tparser = toFIXString }

tURLLink :: FIXTag
tURLLink = FIXTag { tnum = 149, tparser = toFIXString }

tExecType :: FIXTag
tExecType = FIXTag { tnum = 150, tparser = toFIXString }

tLeavesQty :: FIXTag
tLeavesQty = FIXTag { tnum = 151, tparser = toFIXString }

tCashOrderQty :: FIXTag
tCashOrderQty = FIXTag { tnum = 152, tparser = toFIXString }

tAllocAvgPx :: FIXTag
tAllocAvgPx = FIXTag { tnum = 153, tparser = toFIXPrice }

tAllocNetMoney :: FIXTag
tAllocNetMoney = FIXTag { tnum = 154, tparser = toFIXAmt }

tSettlCurrFxRate :: FIXTag
tSettlCurrFxRate = FIXTag { tnum = 155, tparser = toFIXFloat }

tSettlCurrFxRateCalc :: FIXTag
tSettlCurrFxRateCalc = FIXTag { tnum = 156, tparser = toFIXString }

tNumDaysInterest :: FIXTag
tNumDaysInterest = FIXTag { tnum = 157, tparser = toFIXInt }

tAccruedInterestRate :: FIXTag
tAccruedInterestRate = FIXTag { tnum = 158, tparser = toFIXString }

tAccruedInterestAmt :: FIXTag
tAccruedInterestAmt = FIXTag { tnum = 159, tparser = toFIXAmt }

tSettlInstMode :: FIXTag
tSettlInstMode = FIXTag { tnum = 160, tparser = toFIXString }

tAllocText :: FIXTag
tAllocText = FIXTag { tnum = 161, tparser = toFIXString }

tSettlInstID :: FIXTag
tSettlInstID = FIXTag { tnum = 162, tparser = toFIXString }

tSettlInstTransType :: FIXTag
tSettlInstTransType = FIXTag { tnum = 163, tparser = toFIXString }

tEmailThreadID :: FIXTag
tEmailThreadID = FIXTag { tnum = 164, tparser = toFIXString }

tSettlInstSource :: FIXTag
tSettlInstSource = FIXTag { tnum = 165, tparser = toFIXString }

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
tOrderQty2 = FIXTag { tnum = 192, tparser = toFIXString }

tSettlDate2 :: FIXTag
tSettlDate2 = FIXTag { tnum = 193, tparser = toFIXLocalMktDate }

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
tNoIOIQualifiers = FIXTag { tnum = 199, tparser = toFIXString }

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
tOptAttribute = FIXTag { tnum = 206, tparser = toFIXString }

tSecurityExchange :: FIXTag
tSecurityExchange = FIXTag { tnum = 207, tparser = toFIXExchange }

tNotifyBrokerOfCredit :: FIXTag
tNotifyBrokerOfCredit = FIXTag { tnum = 208, tparser = toFIXString }

tAllocHandlInst :: FIXTag
tAllocHandlInst = FIXTag { tnum = 209, tparser = toFIXInt }

tMaxShow :: FIXTag
tMaxShow = FIXTag { tnum = 210, tparser = toFIXString }

tPegOffsetValue :: FIXTag
tPegOffsetValue = FIXTag { tnum = 211, tparser = toFIXFloat }

tXmlDataLen :: FIXTag
tXmlDataLen = FIXTag { tnum = 212, tparser = toFIXString }

tXmlData :: FIXTag
tXmlData = FIXTag { tnum = 213, tparser = toFIXString }

tSettlInstRefID :: FIXTag
tSettlInstRefID = FIXTag { tnum = 214, tparser = toFIXString }

tNoRoutingIDs :: FIXTag
tNoRoutingIDs = FIXTag { tnum = 215, tparser = toFIXString }

tRoutingType :: FIXTag
tRoutingType = FIXTag { tnum = 216, tparser = toFIXInt }

tRoutingID :: FIXTag
tRoutingID = FIXTag { tnum = 217, tparser = toFIXString }

tSpread :: FIXTag
tSpread = FIXTag { tnum = 218, tparser = toFIXPriceOffset }

tBenchmark :: FIXTag
tBenchmark = FIXTag { tnum = 219, tparser = toFIXString }

tBenchmarkCurveCurrency :: FIXTag
tBenchmarkCurveCurrency = FIXTag { tnum = 220, tparser = toFIXCurrency }

tBenchmarkCurveName :: FIXTag
tBenchmarkCurveName = FIXTag { tnum = 221, tparser = toFIXString }

tBenchmarkCurvePoint :: FIXTag
tBenchmarkCurvePoint = FIXTag { tnum = 222, tparser = toFIXString }

tCouponRate :: FIXTag
tCouponRate = FIXTag { tnum = 223, tparser = toFIXString }

tCouponPaymentDate :: FIXTag
tCouponPaymentDate = FIXTag { tnum = 224, tparser = toFIXLocalMktDate }

tIssueDate :: FIXTag
tIssueDate = FIXTag { tnum = 225, tparser = toFIXLocalMktDate }

tRepurchaseTerm :: FIXTag
tRepurchaseTerm = FIXTag { tnum = 226, tparser = toFIXInt }

tRepurchaseRate :: FIXTag
tRepurchaseRate = FIXTag { tnum = 227, tparser = toFIXString }

tFactor :: FIXTag
tFactor = FIXTag { tnum = 228, tparser = toFIXFloat }

tTradeOriginationDate :: FIXTag
tTradeOriginationDate = FIXTag { tnum = 229, tparser = toFIXLocalMktDate }

tExDate :: FIXTag
tExDate = FIXTag { tnum = 230, tparser = toFIXLocalMktDate }

tContractMultiplier :: FIXTag
tContractMultiplier = FIXTag { tnum = 231, tparser = toFIXFloat }

tNoStipulations :: FIXTag
tNoStipulations = FIXTag { tnum = 232, tparser = toFIXString }

tStipulationType :: FIXTag
tStipulationType = FIXTag { tnum = 233, tparser = toFIXString }

tStipulationValue :: FIXTag
tStipulationValue = FIXTag { tnum = 234, tparser = toFIXString }

tYieldType :: FIXTag
tYieldType = FIXTag { tnum = 235, tparser = toFIXString }

tYield :: FIXTag
tYield = FIXTag { tnum = 236, tparser = toFIXString }

tTotalTakedown :: FIXTag
tTotalTakedown = FIXTag { tnum = 237, tparser = toFIXAmt }

tConcession :: FIXTag
tConcession = FIXTag { tnum = 238, tparser = toFIXAmt }

tRepoCollateralSecurityType :: FIXTag
tRepoCollateralSecurityType = FIXTag { tnum = 239, tparser = toFIXInt }

tRedemptionDate :: FIXTag
tRedemptionDate = FIXTag { tnum = 240, tparser = toFIXLocalMktDate }

tUnderlyingCouponPaymentDate :: FIXTag
tUnderlyingCouponPaymentDate = FIXTag { tnum = 241, tparser = toFIXLocalMktDate }

tUnderlyingIssueDate :: FIXTag
tUnderlyingIssueDate = FIXTag { tnum = 242, tparser = toFIXLocalMktDate }

tUnderlyingRepoCollateralSecurityType :: FIXTag
tUnderlyingRepoCollateralSecurityType = FIXTag { tnum = 243, tparser = toFIXInt }

tUnderlyingRepurchaseTerm :: FIXTag
tUnderlyingRepurchaseTerm = FIXTag { tnum = 244, tparser = toFIXInt }

tUnderlyingRepurchaseRate :: FIXTag
tUnderlyingRepurchaseRate = FIXTag { tnum = 245, tparser = toFIXString }

tUnderlyingFactor :: FIXTag
tUnderlyingFactor = FIXTag { tnum = 246, tparser = toFIXFloat }

tUnderlyingRedemptionDate :: FIXTag
tUnderlyingRedemptionDate = FIXTag { tnum = 247, tparser = toFIXLocalMktDate }

tLegCouponPaymentDate :: FIXTag
tLegCouponPaymentDate = FIXTag { tnum = 248, tparser = toFIXLocalMktDate }

tLegIssueDate :: FIXTag
tLegIssueDate = FIXTag { tnum = 249, tparser = toFIXLocalMktDate }

tLegRepoCollateralSecurityType :: FIXTag
tLegRepoCollateralSecurityType = FIXTag { tnum = 250, tparser = toFIXInt }

tLegRepurchaseTerm :: FIXTag
tLegRepurchaseTerm = FIXTag { tnum = 251, tparser = toFIXInt }

tLegRepurchaseRate :: FIXTag
tLegRepurchaseRate = FIXTag { tnum = 252, tparser = toFIXString }

tLegFactor :: FIXTag
tLegFactor = FIXTag { tnum = 253, tparser = toFIXFloat }

tLegRedemptionDate :: FIXTag
tLegRedemptionDate = FIXTag { tnum = 254, tparser = toFIXLocalMktDate }

tCreditRating :: FIXTag
tCreditRating = FIXTag { tnum = 255, tparser = toFIXString }

tUnderlyingCreditRating :: FIXTag
tUnderlyingCreditRating = FIXTag { tnum = 256, tparser = toFIXString }

tLegCreditRating :: FIXTag
tLegCreditRating = FIXTag { tnum = 257, tparser = toFIXString }

tTradedFlatSwitch :: FIXTag
tTradedFlatSwitch = FIXTag { tnum = 258, tparser = toFIXString }

tBasisFeatureDate :: FIXTag
tBasisFeatureDate = FIXTag { tnum = 259, tparser = toFIXLocalMktDate }

tBasisFeaturePrice :: FIXTag
tBasisFeaturePrice = FIXTag { tnum = 260, tparser = toFIXPrice }

tMDReqID :: FIXTag
tMDReqID = FIXTag { tnum = 262, tparser = toFIXString }

tSubscriptionRequestType :: FIXTag
tSubscriptionRequestType = FIXTag { tnum = 263, tparser = toFIXString }

tMarketDepth :: FIXTag
tMarketDepth = FIXTag { tnum = 264, tparser = toFIXInt }

tMDUpdateType :: FIXTag
tMDUpdateType = FIXTag { tnum = 265, tparser = toFIXInt }

tAggregatedBook :: FIXTag
tAggregatedBook = FIXTag { tnum = 266, tparser = toFIXString }

tNoMDEntryTypes :: FIXTag
tNoMDEntryTypes = FIXTag { tnum = 267, tparser = toFIXString }

tNoMDEntries :: FIXTag
tNoMDEntries = FIXTag { tnum = 268, tparser = toFIXString }

tMDEntryType :: FIXTag
tMDEntryType = FIXTag { tnum = 269, tparser = toFIXString }

tMDEntryPx :: FIXTag
tMDEntryPx = FIXTag { tnum = 270, tparser = toFIXPrice }

tMDEntrySize :: FIXTag
tMDEntrySize = FIXTag { tnum = 271, tparser = toFIXString }

tMDEntryDate :: FIXTag
tMDEntryDate = FIXTag { tnum = 272, tparser = toFIXString }

tMDEntryTime :: FIXTag
tMDEntryTime = FIXTag { tnum = 273, tparser = toFIXUTCTimeOnly }

tTickDirection :: FIXTag
tTickDirection = FIXTag { tnum = 274, tparser = toFIXString }

tMDMkt :: FIXTag
tMDMkt = FIXTag { tnum = 275, tparser = toFIXExchange }

tQuoteCondition :: FIXTag
tQuoteCondition = FIXTag { tnum = 276, tparser = toFIXMultipleValueString }

tTradeCondition :: FIXTag
tTradeCondition = FIXTag { tnum = 277, tparser = toFIXMultipleValueString }

tMDEntryID :: FIXTag
tMDEntryID = FIXTag { tnum = 278, tparser = toFIXString }

tMDUpdateAction :: FIXTag
tMDUpdateAction = FIXTag { tnum = 279, tparser = toFIXString }

tMDEntryRefID :: FIXTag
tMDEntryRefID = FIXTag { tnum = 280, tparser = toFIXString }

tMDReqRejReason :: FIXTag
tMDReqRejReason = FIXTag { tnum = 281, tparser = toFIXString }

tMDEntryOriginator :: FIXTag
tMDEntryOriginator = FIXTag { tnum = 282, tparser = toFIXString }

tLocationID :: FIXTag
tLocationID = FIXTag { tnum = 283, tparser = toFIXString }

tDeskID :: FIXTag
tDeskID = FIXTag { tnum = 284, tparser = toFIXString }

tDeleteReason :: FIXTag
tDeleteReason = FIXTag { tnum = 285, tparser = toFIXString }

tOpenCloseSettlFlag :: FIXTag
tOpenCloseSettlFlag = FIXTag { tnum = 286, tparser = toFIXMultipleValueString }

tSellerDays :: FIXTag
tSellerDays = FIXTag { tnum = 287, tparser = toFIXInt }

tMDEntryBuyer :: FIXTag
tMDEntryBuyer = FIXTag { tnum = 288, tparser = toFIXString }

tMDEntrySeller :: FIXTag
tMDEntrySeller = FIXTag { tnum = 289, tparser = toFIXString }

tMDEntryPositionNo :: FIXTag
tMDEntryPositionNo = FIXTag { tnum = 290, tparser = toFIXInt }

tFinancialStatus :: FIXTag
tFinancialStatus = FIXTag { tnum = 291, tparser = toFIXMultipleValueString }

tCorporateAction :: FIXTag
tCorporateAction = FIXTag { tnum = 292, tparser = toFIXMultipleValueString }

tDefBidSize :: FIXTag
tDefBidSize = FIXTag { tnum = 293, tparser = toFIXString }

tDefOfferSize :: FIXTag
tDefOfferSize = FIXTag { tnum = 294, tparser = toFIXString }

tNoQuoteEntries :: FIXTag
tNoQuoteEntries = FIXTag { tnum = 295, tparser = toFIXString }

tNoQuoteSets :: FIXTag
tNoQuoteSets = FIXTag { tnum = 296, tparser = toFIXString }

tQuoteStatus :: FIXTag
tQuoteStatus = FIXTag { tnum = 297, tparser = toFIXInt }

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

tTotNoQuoteEntries :: FIXTag
tTotNoQuoteEntries = FIXTag { tnum = 304, tparser = toFIXInt }

tUnderlyingSecurityIDSource :: FIXTag
tUnderlyingSecurityIDSource = FIXTag { tnum = 305, tparser = toFIXString }

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
tUnderlyingOptAttribute = FIXTag { tnum = 317, tparser = toFIXString }

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
tUnsolicitedIndicator = FIXTag { tnum = 325, tparser = toFIXString }

tSecurityTradingStatus :: FIXTag
tSecurityTradingStatus = FIXTag { tnum = 326, tparser = toFIXInt }

tHaltReasonChar :: FIXTag
tHaltReasonChar = FIXTag { tnum = 327, tparser = toFIXString }

tInViewOfCommon :: FIXTag
tInViewOfCommon = FIXTag { tnum = 328, tparser = toFIXString }

tDueToRelated :: FIXTag
tDueToRelated = FIXTag { tnum = 329, tparser = toFIXString }

tBuyVolume :: FIXTag
tBuyVolume = FIXTag { tnum = 330, tparser = toFIXString }

tSellVolume :: FIXTag
tSellVolume = FIXTag { tnum = 331, tparser = toFIXString }

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
tLastMsgSeqNumProcessed = FIXTag { tnum = 369, tparser = toFIXString }

tOnBehalfOfSendingTime :: FIXTag
tOnBehalfOfSendingTime = FIXTag { tnum = 370, tparser = toFIXUTCTimestamp }

tRefTagID :: FIXTag
tRefTagID = FIXTag { tnum = 371, tparser = toFIXInt }

tRefMsgType :: FIXTag
tRefMsgType = FIXTag { tnum = 372, tparser = toFIXString }

tSessionRejectReason :: FIXTag
tSessionRejectReason = FIXTag { tnum = 373, tparser = toFIXInt }

tBidRequestTransType :: FIXTag
tBidRequestTransType = FIXTag { tnum = 374, tparser = toFIXString }

tContraBroker :: FIXTag
tContraBroker = FIXTag { tnum = 375, tparser = toFIXString }

tComplianceID :: FIXTag
tComplianceID = FIXTag { tnum = 376, tparser = toFIXString }

tSolicitedFlag :: FIXTag
tSolicitedFlag = FIXTag { tnum = 377, tparser = toFIXString }

tExecRestatementReason :: FIXTag
tExecRestatementReason = FIXTag { tnum = 378, tparser = toFIXInt }

tBusinessRejectRefID :: FIXTag
tBusinessRejectRefID = FIXTag { tnum = 379, tparser = toFIXString }

tBusinessRejectReason :: FIXTag
tBusinessRejectReason = FIXTag { tnum = 380, tparser = toFIXInt }

tGrossTradeAmt :: FIXTag
tGrossTradeAmt = FIXTag { tnum = 381, tparser = toFIXAmt }

tNoContraBrokers :: FIXTag
tNoContraBrokers = FIXTag { tnum = 382, tparser = toFIXString }

tMaxMessageSize :: FIXTag
tMaxMessageSize = FIXTag { tnum = 383, tparser = toFIXString }

tNoMsgTypes :: FIXTag
tNoMsgTypes = FIXTag { tnum = 384, tparser = toFIXString }

tMsgDirection :: FIXTag
tMsgDirection = FIXTag { tnum = 385, tparser = toFIXString }

tNoTradingSessions :: FIXTag
tNoTradingSessions = FIXTag { tnum = 386, tparser = toFIXString }

tTotalVolumeTraded :: FIXTag
tTotalVolumeTraded = FIXTag { tnum = 387, tparser = toFIXString }

tDiscretionInst :: FIXTag
tDiscretionInst = FIXTag { tnum = 388, tparser = toFIXString }

tDiscretionOffsetValue :: FIXTag
tDiscretionOffsetValue = FIXTag { tnum = 389, tparser = toFIXFloat }

tBidID :: FIXTag
tBidID = FIXTag { tnum = 390, tparser = toFIXString }

tClientBidID :: FIXTag
tClientBidID = FIXTag { tnum = 391, tparser = toFIXString }

tListName :: FIXTag
tListName = FIXTag { tnum = 392, tparser = toFIXString }

tTotNoRelatedSym :: FIXTag
tTotNoRelatedSym = FIXTag { tnum = 393, tparser = toFIXInt }

tBidType :: FIXTag
tBidType = FIXTag { tnum = 394, tparser = toFIXInt }

tNumTickets :: FIXTag
tNumTickets = FIXTag { tnum = 395, tparser = toFIXInt }

tSideValue1 :: FIXTag
tSideValue1 = FIXTag { tnum = 396, tparser = toFIXAmt }

tSideValue2 :: FIXTag
tSideValue2 = FIXTag { tnum = 397, tparser = toFIXAmt }

tNoBidDescriptors :: FIXTag
tNoBidDescriptors = FIXTag { tnum = 398, tparser = toFIXString }

tBidDescriptorType :: FIXTag
tBidDescriptorType = FIXTag { tnum = 399, tparser = toFIXInt }

tBidDescriptor :: FIXTag
tBidDescriptor = FIXTag { tnum = 400, tparser = toFIXString }

tSideValueInd :: FIXTag
tSideValueInd = FIXTag { tnum = 401, tparser = toFIXInt }

tLiquidityPctLow :: FIXTag
tLiquidityPctLow = FIXTag { tnum = 402, tparser = toFIXString }

tLiquidityPctHigh :: FIXTag
tLiquidityPctHigh = FIXTag { tnum = 403, tparser = toFIXString }

tLiquidityValue :: FIXTag
tLiquidityValue = FIXTag { tnum = 404, tparser = toFIXAmt }

tEFPTrackingError :: FIXTag
tEFPTrackingError = FIXTag { tnum = 405, tparser = toFIXString }

tFairValue :: FIXTag
tFairValue = FIXTag { tnum = 406, tparser = toFIXAmt }

tOutsideIndexPct :: FIXTag
tOutsideIndexPct = FIXTag { tnum = 407, tparser = toFIXString }

tValueOfFutures :: FIXTag
tValueOfFutures = FIXTag { tnum = 408, tparser = toFIXAmt }

tLiquidityIndType :: FIXTag
tLiquidityIndType = FIXTag { tnum = 409, tparser = toFIXInt }

tWtAverageLiquidity :: FIXTag
tWtAverageLiquidity = FIXTag { tnum = 410, tparser = toFIXString }

tExchangeForPhysical :: FIXTag
tExchangeForPhysical = FIXTag { tnum = 411, tparser = toFIXString }

tOutMainCntryUIndex :: FIXTag
tOutMainCntryUIndex = FIXTag { tnum = 412, tparser = toFIXAmt }

tCrossPercent :: FIXTag
tCrossPercent = FIXTag { tnum = 413, tparser = toFIXString }

tProgRptReqs :: FIXTag
tProgRptReqs = FIXTag { tnum = 414, tparser = toFIXInt }

tProgPeriodInterval :: FIXTag
tProgPeriodInterval = FIXTag { tnum = 415, tparser = toFIXInt }

tIncTaxInd :: FIXTag
tIncTaxInd = FIXTag { tnum = 416, tparser = toFIXInt }

tNumBidders :: FIXTag
tNumBidders = FIXTag { tnum = 417, tparser = toFIXInt }

tBidTradeType :: FIXTag
tBidTradeType = FIXTag { tnum = 418, tparser = toFIXString }

tBasisPxType :: FIXTag
tBasisPxType = FIXTag { tnum = 419, tparser = toFIXString }

tNoBidComponents :: FIXTag
tNoBidComponents = FIXTag { tnum = 420, tparser = toFIXString }

tCountry :: FIXTag
tCountry = FIXTag { tnum = 421, tparser = toFIXString }

tTotNoStrikes :: FIXTag
tTotNoStrikes = FIXTag { tnum = 422, tparser = toFIXInt }

tPriceType :: FIXTag
tPriceType = FIXTag { tnum = 423, tparser = toFIXInt }

tDayOrderQty :: FIXTag
tDayOrderQty = FIXTag { tnum = 424, tparser = toFIXString }

tDayCumQty :: FIXTag
tDayCumQty = FIXTag { tnum = 425, tparser = toFIXString }

tDayAvgPx :: FIXTag
tDayAvgPx = FIXTag { tnum = 426, tparser = toFIXPrice }

tGTBookingInst :: FIXTag
tGTBookingInst = FIXTag { tnum = 427, tparser = toFIXInt }

tNoStrikes :: FIXTag
tNoStrikes = FIXTag { tnum = 428, tparser = toFIXString }

tListStatusType :: FIXTag
tListStatusType = FIXTag { tnum = 429, tparser = toFIXInt }

tNetGrossInd :: FIXTag
tNetGrossInd = FIXTag { tnum = 430, tparser = toFIXInt }

tListOrderStatus :: FIXTag
tListOrderStatus = FIXTag { tnum = 431, tparser = toFIXInt }

tExpireDate :: FIXTag
tExpireDate = FIXTag { tnum = 432, tparser = toFIXLocalMktDate }

tListExecInstType :: FIXTag
tListExecInstType = FIXTag { tnum = 433, tparser = toFIXString }

tCxlRejResponseTo :: FIXTag
tCxlRejResponseTo = FIXTag { tnum = 434, tparser = toFIXString }

tUnderlyingCouponRate :: FIXTag
tUnderlyingCouponRate = FIXTag { tnum = 435, tparser = toFIXString }

tUnderlyingContractMultiplier :: FIXTag
tUnderlyingContractMultiplier = FIXTag { tnum = 436, tparser = toFIXFloat }

tContraTradeQty :: FIXTag
tContraTradeQty = FIXTag { tnum = 437, tparser = toFIXString }

tContraTradeTime :: FIXTag
tContraTradeTime = FIXTag { tnum = 438, tparser = toFIXUTCTimestamp }

tClearingFirm :: FIXTag
tClearingFirm = FIXTag { tnum = 439, tparser = toFIXString }

tClearingAccount :: FIXTag
tClearingAccount = FIXTag { tnum = 440, tparser = toFIXString }

tLiquidityNumSecurities :: FIXTag
tLiquidityNumSecurities = FIXTag { tnum = 441, tparser = toFIXInt }

tMultiLegReportingType :: FIXTag
tMultiLegReportingType = FIXTag { tnum = 442, tparser = toFIXString }

tStrikeTime :: FIXTag
tStrikeTime = FIXTag { tnum = 443, tparser = toFIXUTCTimestamp }

tListStatusText :: FIXTag
tListStatusText = FIXTag { tnum = 444, tparser = toFIXString }

tEncodedListStatusTextLen :: FIXTag
tEncodedListStatusTextLen = FIXTag { tnum = 445, tparser = toFIXString }

tEncodedListStatusText :: FIXTag
tEncodedListStatusText = FIXTag { tnum = 446, tparser = toFIXString }

tPartyIDSource :: FIXTag
tPartyIDSource = FIXTag { tnum = 447, tparser = toFIXString }

tPartyID :: FIXTag
tPartyID = FIXTag { tnum = 448, tparser = toFIXString }

tTotalVolumeTradedDate :: FIXTag
tTotalVolumeTradedDate = FIXTag { tnum = 449, tparser = toFIXString }

tTotalVolumeTradedTime :: FIXTag
tTotalVolumeTradedTime = FIXTag { tnum = 450, tparser = toFIXUTCTimeOnly }

tNetChgPrevDay :: FIXTag
tNetChgPrevDay = FIXTag { tnum = 451, tparser = toFIXPriceOffset }

tPartyRole :: FIXTag
tPartyRole = FIXTag { tnum = 452, tparser = toFIXInt }

tNoPartyIDs :: FIXTag
tNoPartyIDs = FIXTag { tnum = 453, tparser = toFIXString }

tNoSecurityAltID :: FIXTag
tNoSecurityAltID = FIXTag { tnum = 454, tparser = toFIXString }

tSecurityAltID :: FIXTag
tSecurityAltID = FIXTag { tnum = 455, tparser = toFIXString }

tSecurityAltIDSource :: FIXTag
tSecurityAltIDSource = FIXTag { tnum = 456, tparser = toFIXString }

tNoUnderlyingSecurityAltID :: FIXTag
tNoUnderlyingSecurityAltID = FIXTag { tnum = 457, tparser = toFIXString }

tUnderlyingSecurityAltID :: FIXTag
tUnderlyingSecurityAltID = FIXTag { tnum = 458, tparser = toFIXString }

tUnderlyingSecurityAltIDSource :: FIXTag
tUnderlyingSecurityAltIDSource = FIXTag { tnum = 459, tparser = toFIXString }

tProduct :: FIXTag
tProduct = FIXTag { tnum = 460, tparser = toFIXInt }

tCFICode :: FIXTag
tCFICode = FIXTag { tnum = 461, tparser = toFIXString }

tUnderlyingProduct :: FIXTag
tUnderlyingProduct = FIXTag { tnum = 462, tparser = toFIXInt }

tUnderlyingCFICode :: FIXTag
tUnderlyingCFICode = FIXTag { tnum = 463, tparser = toFIXString }

tTestMessageIndicator :: FIXTag
tTestMessageIndicator = FIXTag { tnum = 464, tparser = toFIXString }

tQuantityType :: FIXTag
tQuantityType = FIXTag { tnum = 465, tparser = toFIXInt }

tBookingRefID :: FIXTag
tBookingRefID = FIXTag { tnum = 466, tparser = toFIXString }

tIndividualAllocID :: FIXTag
tIndividualAllocID = FIXTag { tnum = 467, tparser = toFIXString }

tRoundingDirection :: FIXTag
tRoundingDirection = FIXTag { tnum = 468, tparser = toFIXString }

tRoundingModulus :: FIXTag
tRoundingModulus = FIXTag { tnum = 469, tparser = toFIXFloat }

tCountryOfIssue :: FIXTag
tCountryOfIssue = FIXTag { tnum = 470, tparser = toFIXString }

tStateOrProvinceOfIssue :: FIXTag
tStateOrProvinceOfIssue = FIXTag { tnum = 471, tparser = toFIXString }

tLocaleOfIssue :: FIXTag
tLocaleOfIssue = FIXTag { tnum = 472, tparser = toFIXString }

tNoRegistDtls :: FIXTag
tNoRegistDtls = FIXTag { tnum = 473, tparser = toFIXString }

tMailingDtls :: FIXTag
tMailingDtls = FIXTag { tnum = 474, tparser = toFIXString }

tInvestorCountryOfResidence :: FIXTag
tInvestorCountryOfResidence = FIXTag { tnum = 475, tparser = toFIXString }

tPaymentRef :: FIXTag
tPaymentRef = FIXTag { tnum = 476, tparser = toFIXString }

tDistribPaymentMethod :: FIXTag
tDistribPaymentMethod = FIXTag { tnum = 477, tparser = toFIXInt }

tCashDistribCurr :: FIXTag
tCashDistribCurr = FIXTag { tnum = 478, tparser = toFIXCurrency }

tCommCurrency :: FIXTag
tCommCurrency = FIXTag { tnum = 479, tparser = toFIXCurrency }

tCancellationRights :: FIXTag
tCancellationRights = FIXTag { tnum = 480, tparser = toFIXString }

tMoneyLaunderingStatus :: FIXTag
tMoneyLaunderingStatus = FIXTag { tnum = 481, tparser = toFIXString }

tMailingInst :: FIXTag
tMailingInst = FIXTag { tnum = 482, tparser = toFIXString }

tTransBkdTime :: FIXTag
tTransBkdTime = FIXTag { tnum = 483, tparser = toFIXUTCTimestamp }

tExecPriceType :: FIXTag
tExecPriceType = FIXTag { tnum = 484, tparser = toFIXString }

tExecPriceAdjustment :: FIXTag
tExecPriceAdjustment = FIXTag { tnum = 485, tparser = toFIXFloat }

tDateOfBirth :: FIXTag
tDateOfBirth = FIXTag { tnum = 486, tparser = toFIXLocalMktDate }

tTradeReportTransType :: FIXTag
tTradeReportTransType = FIXTag { tnum = 487, tparser = toFIXInt }

tCardHolderName :: FIXTag
tCardHolderName = FIXTag { tnum = 488, tparser = toFIXString }

tCardNumber :: FIXTag
tCardNumber = FIXTag { tnum = 489, tparser = toFIXString }

tCardExpDate :: FIXTag
tCardExpDate = FIXTag { tnum = 490, tparser = toFIXLocalMktDate }

tCardIssNum :: FIXTag
tCardIssNum = FIXTag { tnum = 491, tparser = toFIXString }

tPaymentMethod :: FIXTag
tPaymentMethod = FIXTag { tnum = 492, tparser = toFIXInt }

tRegistAcctType :: FIXTag
tRegistAcctType = FIXTag { tnum = 493, tparser = toFIXString }

tDesignation :: FIXTag
tDesignation = FIXTag { tnum = 494, tparser = toFIXString }

tTaxAdvantageType :: FIXTag
tTaxAdvantageType = FIXTag { tnum = 495, tparser = toFIXInt }

tRegistRejReasonText :: FIXTag
tRegistRejReasonText = FIXTag { tnum = 496, tparser = toFIXString }

tFundRenewWaiv :: FIXTag
tFundRenewWaiv = FIXTag { tnum = 497, tparser = toFIXString }

tCashDistribAgentName :: FIXTag
tCashDistribAgentName = FIXTag { tnum = 498, tparser = toFIXString }

tCashDistribAgentCode :: FIXTag
tCashDistribAgentCode = FIXTag { tnum = 499, tparser = toFIXString }

tCashDistribAgentAcctNumber :: FIXTag
tCashDistribAgentAcctNumber = FIXTag { tnum = 500, tparser = toFIXString }

tCashDistribPayRef :: FIXTag
tCashDistribPayRef = FIXTag { tnum = 501, tparser = toFIXString }

tCashDistribAgentAcctName :: FIXTag
tCashDistribAgentAcctName = FIXTag { tnum = 502, tparser = toFIXString }

tCardStartDate :: FIXTag
tCardStartDate = FIXTag { tnum = 503, tparser = toFIXLocalMktDate }

tPaymentDate :: FIXTag
tPaymentDate = FIXTag { tnum = 504, tparser = toFIXLocalMktDate }

tPaymentRemitterID :: FIXTag
tPaymentRemitterID = FIXTag { tnum = 505, tparser = toFIXString }

tRegistStatus :: FIXTag
tRegistStatus = FIXTag { tnum = 506, tparser = toFIXString }

tRegistRejReasonCode :: FIXTag
tRegistRejReasonCode = FIXTag { tnum = 507, tparser = toFIXInt }

tRegistRefID :: FIXTag
tRegistRefID = FIXTag { tnum = 508, tparser = toFIXString }

tRegistDtls :: FIXTag
tRegistDtls = FIXTag { tnum = 509, tparser = toFIXString }

tNoDistribInsts :: FIXTag
tNoDistribInsts = FIXTag { tnum = 510, tparser = toFIXString }

tRegistEmail :: FIXTag
tRegistEmail = FIXTag { tnum = 511, tparser = toFIXString }

tDistribPercentage :: FIXTag
tDistribPercentage = FIXTag { tnum = 512, tparser = toFIXString }

tRegistID :: FIXTag
tRegistID = FIXTag { tnum = 513, tparser = toFIXString }

tRegistTransType :: FIXTag
tRegistTransType = FIXTag { tnum = 514, tparser = toFIXString }

tExecValuationPoint :: FIXTag
tExecValuationPoint = FIXTag { tnum = 515, tparser = toFIXUTCTimestamp }

tOrderPercent :: FIXTag
tOrderPercent = FIXTag { tnum = 516, tparser = toFIXString }

tOwnershipType :: FIXTag
tOwnershipType = FIXTag { tnum = 517, tparser = toFIXString }

tNoContAmts :: FIXTag
tNoContAmts = FIXTag { tnum = 518, tparser = toFIXString }

tContAmtType :: FIXTag
tContAmtType = FIXTag { tnum = 519, tparser = toFIXInt }

tContAmtValue :: FIXTag
tContAmtValue = FIXTag { tnum = 520, tparser = toFIXFloat }

tContAmtCurr :: FIXTag
tContAmtCurr = FIXTag { tnum = 521, tparser = toFIXCurrency }

tOwnerType :: FIXTag
tOwnerType = FIXTag { tnum = 522, tparser = toFIXInt }

tPartySubID :: FIXTag
tPartySubID = FIXTag { tnum = 523, tparser = toFIXString }

tNestedPartyID :: FIXTag
tNestedPartyID = FIXTag { tnum = 524, tparser = toFIXString }

tNestedPartyIDSource :: FIXTag
tNestedPartyIDSource = FIXTag { tnum = 525, tparser = toFIXString }

tSecondaryClOrdID :: FIXTag
tSecondaryClOrdID = FIXTag { tnum = 526, tparser = toFIXString }

tSecondaryExecID :: FIXTag
tSecondaryExecID = FIXTag { tnum = 527, tparser = toFIXString }

tOrderCapacity :: FIXTag
tOrderCapacity = FIXTag { tnum = 528, tparser = toFIXString }

tOrderRestrictions :: FIXTag
tOrderRestrictions = FIXTag { tnum = 529, tparser = toFIXMultipleValueString }

tMassCancelRequestType :: FIXTag
tMassCancelRequestType = FIXTag { tnum = 530, tparser = toFIXString }

tMassCancelResponse :: FIXTag
tMassCancelResponse = FIXTag { tnum = 531, tparser = toFIXString }

tMassCancelRejectReason :: FIXTag
tMassCancelRejectReason = FIXTag { tnum = 532, tparser = toFIXString }

tTotalAffectedOrders :: FIXTag
tTotalAffectedOrders = FIXTag { tnum = 533, tparser = toFIXInt }

tNoAffectedOrders :: FIXTag
tNoAffectedOrders = FIXTag { tnum = 534, tparser = toFIXInt }

tAffectedOrderID :: FIXTag
tAffectedOrderID = FIXTag { tnum = 535, tparser = toFIXString }

tAffectedSecondaryOrderID :: FIXTag
tAffectedSecondaryOrderID = FIXTag { tnum = 536, tparser = toFIXString }

tQuoteType :: FIXTag
tQuoteType = FIXTag { tnum = 537, tparser = toFIXInt }

tNestedPartyRole :: FIXTag
tNestedPartyRole = FIXTag { tnum = 538, tparser = toFIXInt }

tNoNestedPartyIDs :: FIXTag
tNoNestedPartyIDs = FIXTag { tnum = 539, tparser = toFIXString }

tTotalAccruedInterestAmt :: FIXTag
tTotalAccruedInterestAmt = FIXTag { tnum = 540, tparser = toFIXAmt }

tMaturityDate :: FIXTag
tMaturityDate = FIXTag { tnum = 541, tparser = toFIXLocalMktDate }

tUnderlyingMaturityDate :: FIXTag
tUnderlyingMaturityDate = FIXTag { tnum = 542, tparser = toFIXLocalMktDate }

tInstrRegistry :: FIXTag
tInstrRegistry = FIXTag { tnum = 543, tparser = toFIXString }

tCashMargin :: FIXTag
tCashMargin = FIXTag { tnum = 544, tparser = toFIXString }

tNestedPartySubID :: FIXTag
tNestedPartySubID = FIXTag { tnum = 545, tparser = toFIXString }

tScope :: FIXTag
tScope = FIXTag { tnum = 546, tparser = toFIXMultipleValueString }

tMDImplicitDelete :: FIXTag
tMDImplicitDelete = FIXTag { tnum = 547, tparser = toFIXString }

tCrossID :: FIXTag
tCrossID = FIXTag { tnum = 548, tparser = toFIXString }

tCrossType :: FIXTag
tCrossType = FIXTag { tnum = 549, tparser = toFIXInt }

tCrossPrioritization :: FIXTag
tCrossPrioritization = FIXTag { tnum = 550, tparser = toFIXInt }

tOrigCrossID :: FIXTag
tOrigCrossID = FIXTag { tnum = 551, tparser = toFIXString }

tNoSides :: FIXTag
tNoSides = FIXTag { tnum = 552, tparser = toFIXString }

tUsername :: FIXTag
tUsername = FIXTag { tnum = 553, tparser = toFIXString }

tPassword :: FIXTag
tPassword = FIXTag { tnum = 554, tparser = toFIXString }

tNoLegs :: FIXTag
tNoLegs = FIXTag { tnum = 555, tparser = toFIXString }

tLegCurrency :: FIXTag
tLegCurrency = FIXTag { tnum = 556, tparser = toFIXCurrency }

tTotNoSecurityTypes :: FIXTag
tTotNoSecurityTypes = FIXTag { tnum = 557, tparser = toFIXInt }

tNoSecurityTypes :: FIXTag
tNoSecurityTypes = FIXTag { tnum = 558, tparser = toFIXString }

tSecurityListRequestType :: FIXTag
tSecurityListRequestType = FIXTag { tnum = 559, tparser = toFIXInt }

tSecurityRequestResult :: FIXTag
tSecurityRequestResult = FIXTag { tnum = 560, tparser = toFIXInt }

tRoundLot :: FIXTag
tRoundLot = FIXTag { tnum = 561, tparser = toFIXString }

tMinTradeVol :: FIXTag
tMinTradeVol = FIXTag { tnum = 562, tparser = toFIXString }

tMultiLegRptTypeReq :: FIXTag
tMultiLegRptTypeReq = FIXTag { tnum = 563, tparser = toFIXInt }

tLegPositionEffect :: FIXTag
tLegPositionEffect = FIXTag { tnum = 564, tparser = toFIXString }

tLegCoveredOrUncovered :: FIXTag
tLegCoveredOrUncovered = FIXTag { tnum = 565, tparser = toFIXInt }

tLegPrice :: FIXTag
tLegPrice = FIXTag { tnum = 566, tparser = toFIXPrice }

tTradSesStatusRejReason :: FIXTag
tTradSesStatusRejReason = FIXTag { tnum = 567, tparser = toFIXInt }

tTradeRequestID :: FIXTag
tTradeRequestID = FIXTag { tnum = 568, tparser = toFIXString }

tTradeRequestType :: FIXTag
tTradeRequestType = FIXTag { tnum = 569, tparser = toFIXInt }

tPreviouslyReported :: FIXTag
tPreviouslyReported = FIXTag { tnum = 570, tparser = toFIXString }

tTradeReportID :: FIXTag
tTradeReportID = FIXTag { tnum = 571, tparser = toFIXString }

tTradeReportRefID :: FIXTag
tTradeReportRefID = FIXTag { tnum = 572, tparser = toFIXString }

tMatchStatus :: FIXTag
tMatchStatus = FIXTag { tnum = 573, tparser = toFIXString }

tMatchType :: FIXTag
tMatchType = FIXTag { tnum = 574, tparser = toFIXString }

tOddLot :: FIXTag
tOddLot = FIXTag { tnum = 575, tparser = toFIXString }

tNoClearingInstructions :: FIXTag
tNoClearingInstructions = FIXTag { tnum = 576, tparser = toFIXString }

tClearingInstruction :: FIXTag
tClearingInstruction = FIXTag { tnum = 577, tparser = toFIXInt }

tTradeInputSource :: FIXTag
tTradeInputSource = FIXTag { tnum = 578, tparser = toFIXString }

tTradeInputDevice :: FIXTag
tTradeInputDevice = FIXTag { tnum = 579, tparser = toFIXString }

tNoDates :: FIXTag
tNoDates = FIXTag { tnum = 580, tparser = toFIXInt }

tAccountType :: FIXTag
tAccountType = FIXTag { tnum = 581, tparser = toFIXInt }

tCustOrderCapacity :: FIXTag
tCustOrderCapacity = FIXTag { tnum = 582, tparser = toFIXInt }

tClOrdLinkID :: FIXTag
tClOrdLinkID = FIXTag { tnum = 583, tparser = toFIXString }

tMassStatusReqID :: FIXTag
tMassStatusReqID = FIXTag { tnum = 584, tparser = toFIXString }

tMassStatusReqType :: FIXTag
tMassStatusReqType = FIXTag { tnum = 585, tparser = toFIXInt }

tOrigOrdModTime :: FIXTag
tOrigOrdModTime = FIXTag { tnum = 586, tparser = toFIXUTCTimestamp }

tLegSettlType :: FIXTag
tLegSettlType = FIXTag { tnum = 587, tparser = toFIXString }

tLegSettlDate :: FIXTag
tLegSettlDate = FIXTag { tnum = 588, tparser = toFIXLocalMktDate }

tDayBookingInst :: FIXTag
tDayBookingInst = FIXTag { tnum = 589, tparser = toFIXString }

tBookingUnit :: FIXTag
tBookingUnit = FIXTag { tnum = 590, tparser = toFIXString }

tPreallocMethod :: FIXTag
tPreallocMethod = FIXTag { tnum = 591, tparser = toFIXString }

tUnderlyingCountryOfIssue :: FIXTag
tUnderlyingCountryOfIssue = FIXTag { tnum = 592, tparser = toFIXString }

tUnderlyingStateOrProvinceOfIssue :: FIXTag
tUnderlyingStateOrProvinceOfIssue = FIXTag { tnum = 593, tparser = toFIXString }

tUnderlyingLocaleOfIssue :: FIXTag
tUnderlyingLocaleOfIssue = FIXTag { tnum = 594, tparser = toFIXString }

tUnderlyingInstrRegistry :: FIXTag
tUnderlyingInstrRegistry = FIXTag { tnum = 595, tparser = toFIXString }

tLegCountryOfIssue :: FIXTag
tLegCountryOfIssue = FIXTag { tnum = 596, tparser = toFIXString }

tLegStateOrProvinceOfIssue :: FIXTag
tLegStateOrProvinceOfIssue = FIXTag { tnum = 597, tparser = toFIXString }

tLegLocaleOfIssue :: FIXTag
tLegLocaleOfIssue = FIXTag { tnum = 598, tparser = toFIXString }

tLegInstrRegistry :: FIXTag
tLegInstrRegistry = FIXTag { tnum = 599, tparser = toFIXString }

tLegSymbol :: FIXTag
tLegSymbol = FIXTag { tnum = 600, tparser = toFIXString }

tLegSymbolSfx :: FIXTag
tLegSymbolSfx = FIXTag { tnum = 601, tparser = toFIXString }

tLegSecurityID :: FIXTag
tLegSecurityID = FIXTag { tnum = 602, tparser = toFIXString }

tLegSecurityIDSource :: FIXTag
tLegSecurityIDSource = FIXTag { tnum = 603, tparser = toFIXString }

tNoLegSecurityAltID :: FIXTag
tNoLegSecurityAltID = FIXTag { tnum = 604, tparser = toFIXString }

tLegSecurityAltID :: FIXTag
tLegSecurityAltID = FIXTag { tnum = 605, tparser = toFIXString }

tLegSecurityAltIDSource :: FIXTag
tLegSecurityAltIDSource = FIXTag { tnum = 606, tparser = toFIXString }

tLegProduct :: FIXTag
tLegProduct = FIXTag { tnum = 607, tparser = toFIXInt }

tLegCFICode :: FIXTag
tLegCFICode = FIXTag { tnum = 608, tparser = toFIXString }

tLegSecurityType :: FIXTag
tLegSecurityType = FIXTag { tnum = 609, tparser = toFIXString }

tLegMaturityMonthYear :: FIXTag
tLegMaturityMonthYear = FIXTag { tnum = 610, tparser = toFIXString }

tLegMaturityDate :: FIXTag
tLegMaturityDate = FIXTag { tnum = 611, tparser = toFIXLocalMktDate }

tLegStrikePrice :: FIXTag
tLegStrikePrice = FIXTag { tnum = 612, tparser = toFIXPrice }

tLegOptAttribute :: FIXTag
tLegOptAttribute = FIXTag { tnum = 613, tparser = toFIXString }

tLegContractMultiplier :: FIXTag
tLegContractMultiplier = FIXTag { tnum = 614, tparser = toFIXFloat }

tLegCouponRate :: FIXTag
tLegCouponRate = FIXTag { tnum = 615, tparser = toFIXString }

tLegSecurityExchange :: FIXTag
tLegSecurityExchange = FIXTag { tnum = 616, tparser = toFIXExchange }

tLegIssuer :: FIXTag
tLegIssuer = FIXTag { tnum = 617, tparser = toFIXString }

tEncodedLegIssuerLen :: FIXTag
tEncodedLegIssuerLen = FIXTag { tnum = 618, tparser = toFIXString }

tEncodedLegIssuer :: FIXTag
tEncodedLegIssuer = FIXTag { tnum = 619, tparser = toFIXString }

tLegSecurityDesc :: FIXTag
tLegSecurityDesc = FIXTag { tnum = 620, tparser = toFIXString }

tEncodedLegSecurityDescLen :: FIXTag
tEncodedLegSecurityDescLen = FIXTag { tnum = 621, tparser = toFIXString }

tEncodedLegSecurityDesc :: FIXTag
tEncodedLegSecurityDesc = FIXTag { tnum = 622, tparser = toFIXString }

tLegRatioQty :: FIXTag
tLegRatioQty = FIXTag { tnum = 623, tparser = toFIXFloat }

tLegSide :: FIXTag
tLegSide = FIXTag { tnum = 624, tparser = toFIXString }

tTradingSessionSubID :: FIXTag
tTradingSessionSubID = FIXTag { tnum = 625, tparser = toFIXString }

tAllocType :: FIXTag
tAllocType = FIXTag { tnum = 626, tparser = toFIXInt }

tNoHops :: FIXTag
tNoHops = FIXTag { tnum = 627, tparser = toFIXString }

tHopCompID :: FIXTag
tHopCompID = FIXTag { tnum = 628, tparser = toFIXString }

tHopSendingTime :: FIXTag
tHopSendingTime = FIXTag { tnum = 629, tparser = toFIXUTCTimestamp }

tHopRefID :: FIXTag
tHopRefID = FIXTag { tnum = 630, tparser = toFIXString }

tMidPx :: FIXTag
tMidPx = FIXTag { tnum = 631, tparser = toFIXPrice }

tBidYield :: FIXTag
tBidYield = FIXTag { tnum = 632, tparser = toFIXString }

tMidYield :: FIXTag
tMidYield = FIXTag { tnum = 633, tparser = toFIXString }

tOfferYield :: FIXTag
tOfferYield = FIXTag { tnum = 634, tparser = toFIXString }

tClearingFeeIndicator :: FIXTag
tClearingFeeIndicator = FIXTag { tnum = 635, tparser = toFIXString }

tWorkingIndicator :: FIXTag
tWorkingIndicator = FIXTag { tnum = 636, tparser = toFIXString }

tLegLastPx :: FIXTag
tLegLastPx = FIXTag { tnum = 637, tparser = toFIXPrice }

tPriorityIndicator :: FIXTag
tPriorityIndicator = FIXTag { tnum = 638, tparser = toFIXInt }

tPriceImprovement :: FIXTag
tPriceImprovement = FIXTag { tnum = 639, tparser = toFIXPriceOffset }

tPrice2 :: FIXTag
tPrice2 = FIXTag { tnum = 640, tparser = toFIXPrice }

tLastForwardPoints2 :: FIXTag
tLastForwardPoints2 = FIXTag { tnum = 641, tparser = toFIXPriceOffset }

tBidForwardPoints2 :: FIXTag
tBidForwardPoints2 = FIXTag { tnum = 642, tparser = toFIXPriceOffset }

tOfferForwardPoints2 :: FIXTag
tOfferForwardPoints2 = FIXTag { tnum = 643, tparser = toFIXPriceOffset }

tRFQReqID :: FIXTag
tRFQReqID = FIXTag { tnum = 644, tparser = toFIXString }

tMktBidPx :: FIXTag
tMktBidPx = FIXTag { tnum = 645, tparser = toFIXPrice }

tMktOfferPx :: FIXTag
tMktOfferPx = FIXTag { tnum = 646, tparser = toFIXPrice }

tMinBidSize :: FIXTag
tMinBidSize = FIXTag { tnum = 647, tparser = toFIXString }

tMinOfferSize :: FIXTag
tMinOfferSize = FIXTag { tnum = 648, tparser = toFIXString }

tQuoteStatusReqID :: FIXTag
tQuoteStatusReqID = FIXTag { tnum = 649, tparser = toFIXString }

tLegalConfirm :: FIXTag
tLegalConfirm = FIXTag { tnum = 650, tparser = toFIXString }

tUnderlyingLastPx :: FIXTag
tUnderlyingLastPx = FIXTag { tnum = 651, tparser = toFIXPrice }

tUnderlyingLastQty :: FIXTag
tUnderlyingLastQty = FIXTag { tnum = 652, tparser = toFIXString }

tSecDefStatus :: FIXTag
tSecDefStatus = FIXTag { tnum = 653, tparser = toFIXInt }

tLegRefID :: FIXTag
tLegRefID = FIXTag { tnum = 654, tparser = toFIXString }

tContraLegRefID :: FIXTag
tContraLegRefID = FIXTag { tnum = 655, tparser = toFIXString }

tSettlCurrBidFxRate :: FIXTag
tSettlCurrBidFxRate = FIXTag { tnum = 656, tparser = toFIXFloat }

tSettlCurrOfferFxRate :: FIXTag
tSettlCurrOfferFxRate = FIXTag { tnum = 657, tparser = toFIXFloat }

tQuoteRequestRejectReason :: FIXTag
tQuoteRequestRejectReason = FIXTag { tnum = 658, tparser = toFIXInt }

tSideComplianceID :: FIXTag
tSideComplianceID = FIXTag { tnum = 659, tparser = toFIXString }

tAcctIDSource :: FIXTag
tAcctIDSource = FIXTag { tnum = 660, tparser = toFIXInt }

tAllocAcctIDSource :: FIXTag
tAllocAcctIDSource = FIXTag { tnum = 661, tparser = toFIXInt }

tBenchmarkPrice :: FIXTag
tBenchmarkPrice = FIXTag { tnum = 662, tparser = toFIXPrice }

tBenchmarkPriceType :: FIXTag
tBenchmarkPriceType = FIXTag { tnum = 663, tparser = toFIXInt }

tConfirmID :: FIXTag
tConfirmID = FIXTag { tnum = 664, tparser = toFIXString }

tConfirmStatus :: FIXTag
tConfirmStatus = FIXTag { tnum = 665, tparser = toFIXInt }

tConfirmTransType :: FIXTag
tConfirmTransType = FIXTag { tnum = 666, tparser = toFIXInt }

tContractSettlMonth :: FIXTag
tContractSettlMonth = FIXTag { tnum = 667, tparser = toFIXString }

tDeliveryForm :: FIXTag
tDeliveryForm = FIXTag { tnum = 668, tparser = toFIXInt }

tLastParPx :: FIXTag
tLastParPx = FIXTag { tnum = 669, tparser = toFIXPrice }

tNoLegAllocs :: FIXTag
tNoLegAllocs = FIXTag { tnum = 670, tparser = toFIXString }

tLegAllocAccount :: FIXTag
tLegAllocAccount = FIXTag { tnum = 671, tparser = toFIXString }

tLegIndividualAllocID :: FIXTag
tLegIndividualAllocID = FIXTag { tnum = 672, tparser = toFIXString }

tLegAllocQty :: FIXTag
tLegAllocQty = FIXTag { tnum = 673, tparser = toFIXString }

tLegAllocAcctIDSource :: FIXTag
tLegAllocAcctIDSource = FIXTag { tnum = 674, tparser = toFIXString }

tLegSettlCurrency :: FIXTag
tLegSettlCurrency = FIXTag { tnum = 675, tparser = toFIXCurrency }

tLegBenchmarkCurveCurrency :: FIXTag
tLegBenchmarkCurveCurrency = FIXTag { tnum = 676, tparser = toFIXCurrency }

tLegBenchmarkCurveName :: FIXTag
tLegBenchmarkCurveName = FIXTag { tnum = 677, tparser = toFIXString }

tLegBenchmarkCurvePoint :: FIXTag
tLegBenchmarkCurvePoint = FIXTag { tnum = 678, tparser = toFIXString }

tLegBenchmarkPrice :: FIXTag
tLegBenchmarkPrice = FIXTag { tnum = 679, tparser = toFIXPrice }

tLegBenchmarkPriceType :: FIXTag
tLegBenchmarkPriceType = FIXTag { tnum = 680, tparser = toFIXInt }

tLegBidPx :: FIXTag
tLegBidPx = FIXTag { tnum = 681, tparser = toFIXPrice }

tLegIOIQty :: FIXTag
tLegIOIQty = FIXTag { tnum = 682, tparser = toFIXString }

tNoLegStipulations :: FIXTag
tNoLegStipulations = FIXTag { tnum = 683, tparser = toFIXString }

tLegOfferPx :: FIXTag
tLegOfferPx = FIXTag { tnum = 684, tparser = toFIXPrice }

tLegOrderQty :: FIXTag
tLegOrderQty = FIXTag { tnum = 685, tparser = toFIXString }

tLegPriceType :: FIXTag
tLegPriceType = FIXTag { tnum = 686, tparser = toFIXInt }

tLegQty :: FIXTag
tLegQty = FIXTag { tnum = 687, tparser = toFIXString }

tLegStipulationType :: FIXTag
tLegStipulationType = FIXTag { tnum = 688, tparser = toFIXString }

tLegStipulationValue :: FIXTag
tLegStipulationValue = FIXTag { tnum = 689, tparser = toFIXString }

tLegSwapType :: FIXTag
tLegSwapType = FIXTag { tnum = 690, tparser = toFIXInt }

tPool :: FIXTag
tPool = FIXTag { tnum = 691, tparser = toFIXString }

tQuotePriceType :: FIXTag
tQuotePriceType = FIXTag { tnum = 692, tparser = toFIXInt }

tQuoteRespID :: FIXTag
tQuoteRespID = FIXTag { tnum = 693, tparser = toFIXString }

tQuoteRespType :: FIXTag
tQuoteRespType = FIXTag { tnum = 694, tparser = toFIXInt }

tQuoteQualifier :: FIXTag
tQuoteQualifier = FIXTag { tnum = 695, tparser = toFIXString }

tYieldRedemptionDate :: FIXTag
tYieldRedemptionDate = FIXTag { tnum = 696, tparser = toFIXLocalMktDate }

tYieldRedemptionPrice :: FIXTag
tYieldRedemptionPrice = FIXTag { tnum = 697, tparser = toFIXPrice }

tYieldRedemptionPriceType :: FIXTag
tYieldRedemptionPriceType = FIXTag { tnum = 698, tparser = toFIXInt }

tBenchmarkSecurityID :: FIXTag
tBenchmarkSecurityID = FIXTag { tnum = 699, tparser = toFIXString }

tReversalIndicator :: FIXTag
tReversalIndicator = FIXTag { tnum = 700, tparser = toFIXString }

tYieldCalcDate :: FIXTag
tYieldCalcDate = FIXTag { tnum = 701, tparser = toFIXLocalMktDate }

tNoPositions :: FIXTag
tNoPositions = FIXTag { tnum = 702, tparser = toFIXString }

tPosType :: FIXTag
tPosType = FIXTag { tnum = 703, tparser = toFIXString }

tLongQty :: FIXTag
tLongQty = FIXTag { tnum = 704, tparser = toFIXString }

tShortQty :: FIXTag
tShortQty = FIXTag { tnum = 705, tparser = toFIXString }

tPosQtyStatus :: FIXTag
tPosQtyStatus = FIXTag { tnum = 706, tparser = toFIXInt }

tPosAmtType :: FIXTag
tPosAmtType = FIXTag { tnum = 707, tparser = toFIXString }

tPosAmt :: FIXTag
tPosAmt = FIXTag { tnum = 708, tparser = toFIXAmt }

tPosTransType :: FIXTag
tPosTransType = FIXTag { tnum = 709, tparser = toFIXInt }

tPosReqID :: FIXTag
tPosReqID = FIXTag { tnum = 710, tparser = toFIXString }

tNoUnderlyings :: FIXTag
tNoUnderlyings = FIXTag { tnum = 711, tparser = toFIXString }

tPosMaintAction :: FIXTag
tPosMaintAction = FIXTag { tnum = 712, tparser = toFIXInt }

tOrigPosReqRefID :: FIXTag
tOrigPosReqRefID = FIXTag { tnum = 713, tparser = toFIXString }

tPosMaintRptRefID :: FIXTag
tPosMaintRptRefID = FIXTag { tnum = 714, tparser = toFIXString }

tClearingBusinessDate :: FIXTag
tClearingBusinessDate = FIXTag { tnum = 715, tparser = toFIXLocalMktDate }

tSettlSessID :: FIXTag
tSettlSessID = FIXTag { tnum = 716, tparser = toFIXString }

tSettlSessSubID :: FIXTag
tSettlSessSubID = FIXTag { tnum = 717, tparser = toFIXString }

tAdjustmentType :: FIXTag
tAdjustmentType = FIXTag { tnum = 718, tparser = toFIXInt }

tContraryInstructionIndicator :: FIXTag
tContraryInstructionIndicator = FIXTag { tnum = 719, tparser = toFIXString }

tPriorSpreadIndicator :: FIXTag
tPriorSpreadIndicator = FIXTag { tnum = 720, tparser = toFIXString }

tPosMaintRptID :: FIXTag
tPosMaintRptID = FIXTag { tnum = 721, tparser = toFIXString }

tPosMaintStatus :: FIXTag
tPosMaintStatus = FIXTag { tnum = 722, tparser = toFIXInt }

tPosMaintResult :: FIXTag
tPosMaintResult = FIXTag { tnum = 723, tparser = toFIXInt }

tPosReqType :: FIXTag
tPosReqType = FIXTag { tnum = 724, tparser = toFIXInt }

tResponseTransportType :: FIXTag
tResponseTransportType = FIXTag { tnum = 725, tparser = toFIXInt }

tResponseDestination :: FIXTag
tResponseDestination = FIXTag { tnum = 726, tparser = toFIXString }

tTotalNumPosReports :: FIXTag
tTotalNumPosReports = FIXTag { tnum = 727, tparser = toFIXInt }

tPosReqResult :: FIXTag
tPosReqResult = FIXTag { tnum = 728, tparser = toFIXInt }

tPosReqStatus :: FIXTag
tPosReqStatus = FIXTag { tnum = 729, tparser = toFIXInt }

tSettlPrice :: FIXTag
tSettlPrice = FIXTag { tnum = 730, tparser = toFIXPrice }

tSettlPriceType :: FIXTag
tSettlPriceType = FIXTag { tnum = 731, tparser = toFIXInt }

tUnderlyingSettlPrice :: FIXTag
tUnderlyingSettlPrice = FIXTag { tnum = 732, tparser = toFIXPrice }

tUnderlyingSettlPriceType :: FIXTag
tUnderlyingSettlPriceType = FIXTag { tnum = 733, tparser = toFIXInt }

tPriorSettlPrice :: FIXTag
tPriorSettlPrice = FIXTag { tnum = 734, tparser = toFIXPrice }

tNoQuoteQualifiers :: FIXTag
tNoQuoteQualifiers = FIXTag { tnum = 735, tparser = toFIXString }

tAllocSettlCurrency :: FIXTag
tAllocSettlCurrency = FIXTag { tnum = 736, tparser = toFIXCurrency }

tAllocSettlCurrAmt :: FIXTag
tAllocSettlCurrAmt = FIXTag { tnum = 737, tparser = toFIXAmt }

tInterestAtMaturity :: FIXTag
tInterestAtMaturity = FIXTag { tnum = 738, tparser = toFIXAmt }

tLegDatedDate :: FIXTag
tLegDatedDate = FIXTag { tnum = 739, tparser = toFIXLocalMktDate }

tLegPool :: FIXTag
tLegPool = FIXTag { tnum = 740, tparser = toFIXString }

tAllocInterestAtMaturity :: FIXTag
tAllocInterestAtMaturity = FIXTag { tnum = 741, tparser = toFIXAmt }

tAllocAccruedInterestAmt :: FIXTag
tAllocAccruedInterestAmt = FIXTag { tnum = 742, tparser = toFIXAmt }

tDeliveryDate :: FIXTag
tDeliveryDate = FIXTag { tnum = 743, tparser = toFIXLocalMktDate }

tAssignmentMethod :: FIXTag
tAssignmentMethod = FIXTag { tnum = 744, tparser = toFIXString }

tAssignmentUnit :: FIXTag
tAssignmentUnit = FIXTag { tnum = 745, tparser = toFIXString }

tOpenInterest :: FIXTag
tOpenInterest = FIXTag { tnum = 746, tparser = toFIXAmt }

tExerciseMethod :: FIXTag
tExerciseMethod = FIXTag { tnum = 747, tparser = toFIXString }

tTotNumTradeReports :: FIXTag
tTotNumTradeReports = FIXTag { tnum = 748, tparser = toFIXInt }

tTradeRequestResult :: FIXTag
tTradeRequestResult = FIXTag { tnum = 749, tparser = toFIXInt }

tTradeRequestStatus :: FIXTag
tTradeRequestStatus = FIXTag { tnum = 750, tparser = toFIXInt }

tTradeReportRejectReason :: FIXTag
tTradeReportRejectReason = FIXTag { tnum = 751, tparser = toFIXInt }

tSideMultiLegReportingType :: FIXTag
tSideMultiLegReportingType = FIXTag { tnum = 752, tparser = toFIXInt }

tNoPosAmt :: FIXTag
tNoPosAmt = FIXTag { tnum = 753, tparser = toFIXString }

tAutoAcceptIndicator :: FIXTag
tAutoAcceptIndicator = FIXTag { tnum = 754, tparser = toFIXString }

tAllocReportID :: FIXTag
tAllocReportID = FIXTag { tnum = 755, tparser = toFIXString }

tNoNested2PartyIDs :: FIXTag
tNoNested2PartyIDs = FIXTag { tnum = 756, tparser = toFIXString }

tNested2PartyID :: FIXTag
tNested2PartyID = FIXTag { tnum = 757, tparser = toFIXString }

tNested2PartyIDSource :: FIXTag
tNested2PartyIDSource = FIXTag { tnum = 758, tparser = toFIXString }

tNested2PartyRole :: FIXTag
tNested2PartyRole = FIXTag { tnum = 759, tparser = toFIXInt }

tNested2PartySubID :: FIXTag
tNested2PartySubID = FIXTag { tnum = 760, tparser = toFIXString }

tBenchmarkSecurityIDSource :: FIXTag
tBenchmarkSecurityIDSource = FIXTag { tnum = 761, tparser = toFIXString }

tSecuritySubType :: FIXTag
tSecuritySubType = FIXTag { tnum = 762, tparser = toFIXString }

tUnderlyingSecuritySubType :: FIXTag
tUnderlyingSecuritySubType = FIXTag { tnum = 763, tparser = toFIXString }

tLegSecuritySubType :: FIXTag
tLegSecuritySubType = FIXTag { tnum = 764, tparser = toFIXString }

tAllowableOneSidednessPct :: FIXTag
tAllowableOneSidednessPct = FIXTag { tnum = 765, tparser = toFIXString }

tAllowableOneSidednessValue :: FIXTag
tAllowableOneSidednessValue = FIXTag { tnum = 766, tparser = toFIXAmt }

tAllowableOneSidednessCurr :: FIXTag
tAllowableOneSidednessCurr = FIXTag { tnum = 767, tparser = toFIXCurrency }

tNoTrdRegTimestamps :: FIXTag
tNoTrdRegTimestamps = FIXTag { tnum = 768, tparser = toFIXString }

tTrdRegTimestamp :: FIXTag
tTrdRegTimestamp = FIXTag { tnum = 769, tparser = toFIXUTCTimestamp }

tTrdRegTimestampType :: FIXTag
tTrdRegTimestampType = FIXTag { tnum = 770, tparser = toFIXInt }

tTrdRegTimestampOrigin :: FIXTag
tTrdRegTimestampOrigin = FIXTag { tnum = 771, tparser = toFIXString }

tConfirmRefID :: FIXTag
tConfirmRefID = FIXTag { tnum = 772, tparser = toFIXString }

tConfirmType :: FIXTag
tConfirmType = FIXTag { tnum = 773, tparser = toFIXInt }

tConfirmRejReason :: FIXTag
tConfirmRejReason = FIXTag { tnum = 774, tparser = toFIXInt }

tBookingType :: FIXTag
tBookingType = FIXTag { tnum = 775, tparser = toFIXInt }

tIndividualAllocRejCode :: FIXTag
tIndividualAllocRejCode = FIXTag { tnum = 776, tparser = toFIXInt }

tSettlInstMsgID :: FIXTag
tSettlInstMsgID = FIXTag { tnum = 777, tparser = toFIXString }

tNoSettlInst :: FIXTag
tNoSettlInst = FIXTag { tnum = 778, tparser = toFIXString }

tLastUpdateTime :: FIXTag
tLastUpdateTime = FIXTag { tnum = 779, tparser = toFIXUTCTimestamp }

tAllocSettlInstType :: FIXTag
tAllocSettlInstType = FIXTag { tnum = 780, tparser = toFIXInt }

tNoSettlPartyIDs :: FIXTag
tNoSettlPartyIDs = FIXTag { tnum = 781, tparser = toFIXString }

tSettlPartyID :: FIXTag
tSettlPartyID = FIXTag { tnum = 782, tparser = toFIXString }

tSettlPartyIDSource :: FIXTag
tSettlPartyIDSource = FIXTag { tnum = 783, tparser = toFIXString }

tSettlPartyRole :: FIXTag
tSettlPartyRole = FIXTag { tnum = 784, tparser = toFIXInt }

tSettlPartySubID :: FIXTag
tSettlPartySubID = FIXTag { tnum = 785, tparser = toFIXString }

tSettlPartySubIDType :: FIXTag
tSettlPartySubIDType = FIXTag { tnum = 786, tparser = toFIXInt }

tDlvyInstType :: FIXTag
tDlvyInstType = FIXTag { tnum = 787, tparser = toFIXString }

tTerminationType :: FIXTag
tTerminationType = FIXTag { tnum = 788, tparser = toFIXInt }

tNextExpectedMsgSeqNum :: FIXTag
tNextExpectedMsgSeqNum = FIXTag { tnum = 789, tparser = toFIXString }

tOrdStatusReqID :: FIXTag
tOrdStatusReqID = FIXTag { tnum = 790, tparser = toFIXString }

tSettlInstReqID :: FIXTag
tSettlInstReqID = FIXTag { tnum = 791, tparser = toFIXString }

tSettlInstReqRejCode :: FIXTag
tSettlInstReqRejCode = FIXTag { tnum = 792, tparser = toFIXInt }

tSecondaryAllocID :: FIXTag
tSecondaryAllocID = FIXTag { tnum = 793, tparser = toFIXString }

tAllocReportType :: FIXTag
tAllocReportType = FIXTag { tnum = 794, tparser = toFIXInt }

tAllocReportRefID :: FIXTag
tAllocReportRefID = FIXTag { tnum = 795, tparser = toFIXString }

tAllocCancReplaceReason :: FIXTag
tAllocCancReplaceReason = FIXTag { tnum = 796, tparser = toFIXInt }

tCopyMsgIndicator :: FIXTag
tCopyMsgIndicator = FIXTag { tnum = 797, tparser = toFIXString }

tAllocAccountType :: FIXTag
tAllocAccountType = FIXTag { tnum = 798, tparser = toFIXInt }

tOrderAvgPx :: FIXTag
tOrderAvgPx = FIXTag { tnum = 799, tparser = toFIXPrice }

tOrderBookingQty :: FIXTag
tOrderBookingQty = FIXTag { tnum = 800, tparser = toFIXString }

tNoSettlPartySubIDs :: FIXTag
tNoSettlPartySubIDs = FIXTag { tnum = 801, tparser = toFIXString }

tNoPartySubIDs :: FIXTag
tNoPartySubIDs = FIXTag { tnum = 802, tparser = toFIXString }

tPartySubIDType :: FIXTag
tPartySubIDType = FIXTag { tnum = 803, tparser = toFIXInt }

tNoNestedPartySubIDs :: FIXTag
tNoNestedPartySubIDs = FIXTag { tnum = 804, tparser = toFIXString }

tNestedPartySubIDType :: FIXTag
tNestedPartySubIDType = FIXTag { tnum = 805, tparser = toFIXInt }

tNoNested2PartySubIDs :: FIXTag
tNoNested2PartySubIDs = FIXTag { tnum = 806, tparser = toFIXString }

tNested2PartySubIDType :: FIXTag
tNested2PartySubIDType = FIXTag { tnum = 807, tparser = toFIXInt }

tAllocIntermedReqType :: FIXTag
tAllocIntermedReqType = FIXTag { tnum = 808, tparser = toFIXInt }

tUnderlyingPx :: FIXTag
tUnderlyingPx = FIXTag { tnum = 810, tparser = toFIXPrice }

tPriceDelta :: FIXTag
tPriceDelta = FIXTag { tnum = 811, tparser = toFIXFloat }

tApplQueueMax :: FIXTag
tApplQueueMax = FIXTag { tnum = 812, tparser = toFIXInt }

tApplQueueDepth :: FIXTag
tApplQueueDepth = FIXTag { tnum = 813, tparser = toFIXInt }

tApplQueueResolution :: FIXTag
tApplQueueResolution = FIXTag { tnum = 814, tparser = toFIXInt }

tApplQueueAction :: FIXTag
tApplQueueAction = FIXTag { tnum = 815, tparser = toFIXInt }

tNoAltMDSource :: FIXTag
tNoAltMDSource = FIXTag { tnum = 816, tparser = toFIXString }

tAltMDSourceID :: FIXTag
tAltMDSourceID = FIXTag { tnum = 817, tparser = toFIXString }

tSecondaryTradeReportID :: FIXTag
tSecondaryTradeReportID = FIXTag { tnum = 818, tparser = toFIXString }

tAvgPxIndicator :: FIXTag
tAvgPxIndicator = FIXTag { tnum = 819, tparser = toFIXInt }

tTradeLinkID :: FIXTag
tTradeLinkID = FIXTag { tnum = 820, tparser = toFIXString }

tOrderInputDevice :: FIXTag
tOrderInputDevice = FIXTag { tnum = 821, tparser = toFIXString }

tUnderlyingTradingSessionID :: FIXTag
tUnderlyingTradingSessionID = FIXTag { tnum = 822, tparser = toFIXString }

tUnderlyingTradingSessionSubID :: FIXTag
tUnderlyingTradingSessionSubID = FIXTag { tnum = 823, tparser = toFIXString }

tTradeLegRefID :: FIXTag
tTradeLegRefID = FIXTag { tnum = 824, tparser = toFIXString }

tExchangeRule :: FIXTag
tExchangeRule = FIXTag { tnum = 825, tparser = toFIXString }

tTradeAllocIndicator :: FIXTag
tTradeAllocIndicator = FIXTag { tnum = 826, tparser = toFIXInt }

tExpirationCycle :: FIXTag
tExpirationCycle = FIXTag { tnum = 827, tparser = toFIXInt }

tTrdType :: FIXTag
tTrdType = FIXTag { tnum = 828, tparser = toFIXInt }

tTrdSubType :: FIXTag
tTrdSubType = FIXTag { tnum = 829, tparser = toFIXInt }

tTransferReason :: FIXTag
tTransferReason = FIXTag { tnum = 830, tparser = toFIXString }

tAsgnReqID :: FIXTag
tAsgnReqID = FIXTag { tnum = 831, tparser = toFIXString }

tTotNumAssignmentReports :: FIXTag
tTotNumAssignmentReports = FIXTag { tnum = 832, tparser = toFIXInt }

tAsgnRptID :: FIXTag
tAsgnRptID = FIXTag { tnum = 833, tparser = toFIXString }

tThresholdAmount :: FIXTag
tThresholdAmount = FIXTag { tnum = 834, tparser = toFIXPriceOffset }

tPegMoveType :: FIXTag
tPegMoveType = FIXTag { tnum = 835, tparser = toFIXInt }

tPegOffsetType :: FIXTag
tPegOffsetType = FIXTag { tnum = 836, tparser = toFIXInt }

tPegLimitType :: FIXTag
tPegLimitType = FIXTag { tnum = 837, tparser = toFIXInt }

tPegRoundDirection :: FIXTag
tPegRoundDirection = FIXTag { tnum = 838, tparser = toFIXInt }

tPeggedPrice :: FIXTag
tPeggedPrice = FIXTag { tnum = 839, tparser = toFIXPrice }

tPegScope :: FIXTag
tPegScope = FIXTag { tnum = 840, tparser = toFIXInt }

tDiscretionMoveType :: FIXTag
tDiscretionMoveType = FIXTag { tnum = 841, tparser = toFIXInt }

tDiscretionOffsetType :: FIXTag
tDiscretionOffsetType = FIXTag { tnum = 842, tparser = toFIXInt }

tDiscretionLimitType :: FIXTag
tDiscretionLimitType = FIXTag { tnum = 843, tparser = toFIXInt }

tDiscretionRoundDirection :: FIXTag
tDiscretionRoundDirection = FIXTag { tnum = 844, tparser = toFIXInt }

tDiscretionPrice :: FIXTag
tDiscretionPrice = FIXTag { tnum = 845, tparser = toFIXPrice }

tDiscretionScope :: FIXTag
tDiscretionScope = FIXTag { tnum = 846, tparser = toFIXInt }

tTargetStrategy :: FIXTag
tTargetStrategy = FIXTag { tnum = 847, tparser = toFIXInt }

tTargetStrategyParameters :: FIXTag
tTargetStrategyParameters = FIXTag { tnum = 848, tparser = toFIXString }

tParticipationRate :: FIXTag
tParticipationRate = FIXTag { tnum = 849, tparser = toFIXString }

tTargetStrategyPerformance :: FIXTag
tTargetStrategyPerformance = FIXTag { tnum = 850, tparser = toFIXFloat }

tLastLiquidityInd :: FIXTag
tLastLiquidityInd = FIXTag { tnum = 851, tparser = toFIXInt }

tPublishTrdIndicator :: FIXTag
tPublishTrdIndicator = FIXTag { tnum = 852, tparser = toFIXString }

tShortSaleReason :: FIXTag
tShortSaleReason = FIXTag { tnum = 853, tparser = toFIXInt }

tQtyType :: FIXTag
tQtyType = FIXTag { tnum = 854, tparser = toFIXInt }

tSecondaryTrdType :: FIXTag
tSecondaryTrdType = FIXTag { tnum = 855, tparser = toFIXInt }

tTradeReportType :: FIXTag
tTradeReportType = FIXTag { tnum = 856, tparser = toFIXInt }

tAllocNoOrdersType :: FIXTag
tAllocNoOrdersType = FIXTag { tnum = 857, tparser = toFIXInt }

tSharedCommission :: FIXTag
tSharedCommission = FIXTag { tnum = 858, tparser = toFIXAmt }

tConfirmReqID :: FIXTag
tConfirmReqID = FIXTag { tnum = 859, tparser = toFIXString }

tAvgParPx :: FIXTag
tAvgParPx = FIXTag { tnum = 860, tparser = toFIXPrice }

tReportedPx :: FIXTag
tReportedPx = FIXTag { tnum = 861, tparser = toFIXPrice }

tNoCapacities :: FIXTag
tNoCapacities = FIXTag { tnum = 862, tparser = toFIXString }

tOrderCapacityQty :: FIXTag
tOrderCapacityQty = FIXTag { tnum = 863, tparser = toFIXString }

tNoEvents :: FIXTag
tNoEvents = FIXTag { tnum = 864, tparser = toFIXString }

tEventType :: FIXTag
tEventType = FIXTag { tnum = 865, tparser = toFIXInt }

tEventDate :: FIXTag
tEventDate = FIXTag { tnum = 866, tparser = toFIXLocalMktDate }

tEventPx :: FIXTag
tEventPx = FIXTag { tnum = 867, tparser = toFIXPrice }

tEventText :: FIXTag
tEventText = FIXTag { tnum = 868, tparser = toFIXString }

tPctAtRisk :: FIXTag
tPctAtRisk = FIXTag { tnum = 869, tparser = toFIXString }

tNoInstrAttrib :: FIXTag
tNoInstrAttrib = FIXTag { tnum = 870, tparser = toFIXString }

tInstrAttribType :: FIXTag
tInstrAttribType = FIXTag { tnum = 871, tparser = toFIXInt }

tInstrAttribValue :: FIXTag
tInstrAttribValue = FIXTag { tnum = 872, tparser = toFIXString }

tDatedDate :: FIXTag
tDatedDate = FIXTag { tnum = 873, tparser = toFIXLocalMktDate }

tInterestAccrualDate :: FIXTag
tInterestAccrualDate = FIXTag { tnum = 874, tparser = toFIXLocalMktDate }

tCPProgram :: FIXTag
tCPProgram = FIXTag { tnum = 875, tparser = toFIXInt }

tCPRegType :: FIXTag
tCPRegType = FIXTag { tnum = 876, tparser = toFIXString }

tUnderlyingCPProgram :: FIXTag
tUnderlyingCPProgram = FIXTag { tnum = 877, tparser = toFIXString }

tUnderlyingCPRegType :: FIXTag
tUnderlyingCPRegType = FIXTag { tnum = 878, tparser = toFIXString }

tUnderlyingQty :: FIXTag
tUnderlyingQty = FIXTag { tnum = 879, tparser = toFIXString }

tTrdMatchID :: FIXTag
tTrdMatchID = FIXTag { tnum = 880, tparser = toFIXString }

tSecondaryTradeReportRefID :: FIXTag
tSecondaryTradeReportRefID = FIXTag { tnum = 881, tparser = toFIXString }

tUnderlyingDirtyPrice :: FIXTag
tUnderlyingDirtyPrice = FIXTag { tnum = 882, tparser = toFIXPrice }

tUnderlyingEndPrice :: FIXTag
tUnderlyingEndPrice = FIXTag { tnum = 883, tparser = toFIXPrice }

tUnderlyingStartValue :: FIXTag
tUnderlyingStartValue = FIXTag { tnum = 884, tparser = toFIXAmt }

tUnderlyingCurrentValue :: FIXTag
tUnderlyingCurrentValue = FIXTag { tnum = 885, tparser = toFIXAmt }

tUnderlyingEndValue :: FIXTag
tUnderlyingEndValue = FIXTag { tnum = 886, tparser = toFIXAmt }

tNoUnderlyingStips :: FIXTag
tNoUnderlyingStips = FIXTag { tnum = 887, tparser = toFIXString }

tUnderlyingStipType :: FIXTag
tUnderlyingStipType = FIXTag { tnum = 888, tparser = toFIXString }

tUnderlyingStipValue :: FIXTag
tUnderlyingStipValue = FIXTag { tnum = 889, tparser = toFIXString }

tMaturityNetMoney :: FIXTag
tMaturityNetMoney = FIXTag { tnum = 890, tparser = toFIXAmt }

tMiscFeeBasis :: FIXTag
tMiscFeeBasis = FIXTag { tnum = 891, tparser = toFIXInt }

tTotNoAllocs :: FIXTag
tTotNoAllocs = FIXTag { tnum = 892, tparser = toFIXInt }

tLastFragment :: FIXTag
tLastFragment = FIXTag { tnum = 893, tparser = toFIXString }

tCollReqID :: FIXTag
tCollReqID = FIXTag { tnum = 894, tparser = toFIXString }

tCollAsgnReason :: FIXTag
tCollAsgnReason = FIXTag { tnum = 895, tparser = toFIXInt }

tCollInquiryQualifier :: FIXTag
tCollInquiryQualifier = FIXTag { tnum = 896, tparser = toFIXInt }

tNoTrades :: FIXTag
tNoTrades = FIXTag { tnum = 897, tparser = toFIXString }

tMarginRatio :: FIXTag
tMarginRatio = FIXTag { tnum = 898, tparser = toFIXString }

tMarginExcess :: FIXTag
tMarginExcess = FIXTag { tnum = 899, tparser = toFIXAmt }

tTotalNetValue :: FIXTag
tTotalNetValue = FIXTag { tnum = 900, tparser = toFIXAmt }

tCashOutstanding :: FIXTag
tCashOutstanding = FIXTag { tnum = 901, tparser = toFIXAmt }

tCollAsgnID :: FIXTag
tCollAsgnID = FIXTag { tnum = 902, tparser = toFIXString }

tCollAsgnTransType :: FIXTag
tCollAsgnTransType = FIXTag { tnum = 903, tparser = toFIXInt }

tCollRespID :: FIXTag
tCollRespID = FIXTag { tnum = 904, tparser = toFIXString }

tCollAsgnRespType :: FIXTag
tCollAsgnRespType = FIXTag { tnum = 905, tparser = toFIXInt }

tCollAsgnRejectReason :: FIXTag
tCollAsgnRejectReason = FIXTag { tnum = 906, tparser = toFIXInt }

tCollAsgnRefID :: FIXTag
tCollAsgnRefID = FIXTag { tnum = 907, tparser = toFIXString }

tCollRptID :: FIXTag
tCollRptID = FIXTag { tnum = 908, tparser = toFIXString }

tCollInquiryID :: FIXTag
tCollInquiryID = FIXTag { tnum = 909, tparser = toFIXString }

tCollStatus :: FIXTag
tCollStatus = FIXTag { tnum = 910, tparser = toFIXInt }

tTotNumReports :: FIXTag
tTotNumReports = FIXTag { tnum = 911, tparser = toFIXInt }

tLastRptRequested :: FIXTag
tLastRptRequested = FIXTag { tnum = 912, tparser = toFIXString }

tAgreementDesc :: FIXTag
tAgreementDesc = FIXTag { tnum = 913, tparser = toFIXString }

tAgreementID :: FIXTag
tAgreementID = FIXTag { tnum = 914, tparser = toFIXString }

tAgreementDate :: FIXTag
tAgreementDate = FIXTag { tnum = 915, tparser = toFIXLocalMktDate }

tStartDate :: FIXTag
tStartDate = FIXTag { tnum = 916, tparser = toFIXLocalMktDate }

tEndDate :: FIXTag
tEndDate = FIXTag { tnum = 917, tparser = toFIXLocalMktDate }

tAgreementCurrency :: FIXTag
tAgreementCurrency = FIXTag { tnum = 918, tparser = toFIXCurrency }

tDeliveryType :: FIXTag
tDeliveryType = FIXTag { tnum = 919, tparser = toFIXInt }

tEndAccruedInterestAmt :: FIXTag
tEndAccruedInterestAmt = FIXTag { tnum = 920, tparser = toFIXAmt }

tStartCash :: FIXTag
tStartCash = FIXTag { tnum = 921, tparser = toFIXAmt }

tEndCash :: FIXTag
tEndCash = FIXTag { tnum = 922, tparser = toFIXAmt }

tUserRequestID :: FIXTag
tUserRequestID = FIXTag { tnum = 923, tparser = toFIXString }

tUserRequestType :: FIXTag
tUserRequestType = FIXTag { tnum = 924, tparser = toFIXInt }

tNewPassword :: FIXTag
tNewPassword = FIXTag { tnum = 925, tparser = toFIXString }

tUserStatus :: FIXTag
tUserStatus = FIXTag { tnum = 926, tparser = toFIXInt }

tUserStatusText :: FIXTag
tUserStatusText = FIXTag { tnum = 927, tparser = toFIXString }

tStatusValue :: FIXTag
tStatusValue = FIXTag { tnum = 928, tparser = toFIXInt }

tStatusText :: FIXTag
tStatusText = FIXTag { tnum = 929, tparser = toFIXString }

tRefCompID :: FIXTag
tRefCompID = FIXTag { tnum = 930, tparser = toFIXString }

tRefSubID :: FIXTag
tRefSubID = FIXTag { tnum = 931, tparser = toFIXString }

tNetworkResponseID :: FIXTag
tNetworkResponseID = FIXTag { tnum = 932, tparser = toFIXString }

tNetworkRequestID :: FIXTag
tNetworkRequestID = FIXTag { tnum = 933, tparser = toFIXString }

tLastNetworkResponseID :: FIXTag
tLastNetworkResponseID = FIXTag { tnum = 934, tparser = toFIXString }

tNetworkRequestType :: FIXTag
tNetworkRequestType = FIXTag { tnum = 935, tparser = toFIXInt }

tNoCompIDs :: FIXTag
tNoCompIDs = FIXTag { tnum = 936, tparser = toFIXString }

tNetworkStatusResponseType :: FIXTag
tNetworkStatusResponseType = FIXTag { tnum = 937, tparser = toFIXInt }

tNoCollInquiryQualifier :: FIXTag
tNoCollInquiryQualifier = FIXTag { tnum = 938, tparser = toFIXString }

tTrdRptStatus :: FIXTag
tTrdRptStatus = FIXTag { tnum = 939, tparser = toFIXInt }

tAffirmStatus :: FIXTag
tAffirmStatus = FIXTag { tnum = 940, tparser = toFIXInt }

tUnderlyingStrikeCurrency :: FIXTag
tUnderlyingStrikeCurrency = FIXTag { tnum = 941, tparser = toFIXCurrency }

tLegStrikeCurrency :: FIXTag
tLegStrikeCurrency = FIXTag { tnum = 942, tparser = toFIXCurrency }

tTimeBracket :: FIXTag
tTimeBracket = FIXTag { tnum = 943, tparser = toFIXString }

tCollAction :: FIXTag
tCollAction = FIXTag { tnum = 944, tparser = toFIXInt }

tCollInquiryStatus :: FIXTag
tCollInquiryStatus = FIXTag { tnum = 945, tparser = toFIXInt }

tCollInquiryResult :: FIXTag
tCollInquiryResult = FIXTag { tnum = 946, tparser = toFIXInt }

tStrikeCurrency :: FIXTag
tStrikeCurrency = FIXTag { tnum = 947, tparser = toFIXCurrency }

tNoNested3PartyIDs :: FIXTag
tNoNested3PartyIDs = FIXTag { tnum = 948, tparser = toFIXString }

tNested3PartyID :: FIXTag
tNested3PartyID = FIXTag { tnum = 949, tparser = toFIXString }

tNested3PartyIDSource :: FIXTag
tNested3PartyIDSource = FIXTag { tnum = 950, tparser = toFIXString }

tNested3PartyRole :: FIXTag
tNested3PartyRole = FIXTag { tnum = 951, tparser = toFIXInt }

tNoNested3PartySubIDs :: FIXTag
tNoNested3PartySubIDs = FIXTag { tnum = 952, tparser = toFIXString }

tNested3PartySubID :: FIXTag
tNested3PartySubID = FIXTag { tnum = 953, tparser = toFIXString }

tNested3PartySubIDType :: FIXTag
tNested3PartySubIDType = FIXTag { tnum = 954, tparser = toFIXInt }

tLegContractSettlMonth :: FIXTag
tLegContractSettlMonth = FIXTag { tnum = 955, tparser = toFIXString }

tLegInterestAccrualDate :: FIXTag
tLegInterestAccrualDate = FIXTag { tnum = 956, tparser = toFIXLocalMktDate }

headerFIX44 :: FIXTags
headerFIX44 = 
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
    LT.new

trailerFIX44 :: FIXTags
trailerFIX44 = 
    LT.insert (tnum tSignatureLength) tSignatureLength $
    LT.insert (tnum tSignature) tSignature $
    LT.insert (tnum tCheckSum) tCheckSum $
    LT.new

mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec
   { mType = C.pack "0"
   , mHeader = headerFIX44
   , mBody = mHeartbeatBody
   , mTrailer = trailerFIX44 }
   where
      mHeartbeatBody = 
          LT.insert (tnum tTestReqID) tTestReqID $
          LT.new

mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec
   { mType = C.pack "1"
   , mHeader = headerFIX44
   , mBody = mTestRequestBody
   , mTrailer = trailerFIX44 }
   where
      mTestRequestBody = 
          LT.insert (tnum tTestReqID) tTestReqID $
          LT.new

mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec
   { mType = C.pack "2"
   , mHeader = headerFIX44
   , mBody = mResendRequestBody
   , mTrailer = trailerFIX44 }
   where
      mResendRequestBody = 
          LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
          LT.insert (tnum tEndSeqNo) tEndSeqNo $
          LT.new

mReject :: FIXMessageSpec
mReject = FMSpec
   { mType = C.pack "3"
   , mHeader = headerFIX44
   , mBody = mRejectBody
   , mTrailer = trailerFIX44 }
   where
      mRejectBody = 
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tRefTagID) tRefTagID $
          LT.insert (tnum tRefMsgType) tRefMsgType $
          LT.insert (tnum tSessionRejectReason) tSessionRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec
   { mType = C.pack "4"
   , mHeader = headerFIX44
   , mBody = mSequenceResetBody
   , mTrailer = trailerFIX44 }
   where
      mSequenceResetBody = 
          LT.insert (tnum tGapFillFlag) tGapFillFlag $
          LT.insert (tnum tNewSeqNo) tNewSeqNo $
          LT.new

mLogout :: FIXMessageSpec
mLogout = FMSpec
   { mType = C.pack "5"
   , mHeader = headerFIX44
   , mBody = mLogoutBody
   , mTrailer = trailerFIX44 }
   where
      mLogoutBody = 
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mIOI :: FIXMessageSpec
mIOI = FMSpec
   { mType = C.pack "6"
   , mHeader = headerFIX44
   , mBody = mIOIBody
   , mTrailer = trailerFIX44 }
   where
      mIOIBody = 
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tIOITransType) tIOITransType $
          LT.insert (tnum tIOIRefID) tIOIRefID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tIOIQty) tIOIQty $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tValidUntilTime) tValidUntilTime $
          LT.insert (tnum tIOIQltyInd) tIOIQltyInd $
          LT.insert (tnum tIOINaturalFlag) tIOINaturalFlag $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tURLLink) tURLLink $
          LT.new

mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec
   { mType = C.pack "7"
   , mHeader = headerFIX44
   , mBody = mAdvertisementBody
   , mTrailer = trailerFIX44 }
   where
      mAdvertisementBody = 
          LT.insert (tnum tAdvId) tAdvId $
          LT.insert (tnum tAdvTransType) tAdvTransType $
          LT.insert (tnum tAdvRefID) tAdvRefID $
          LT.insert (tnum tAdvSide) tAdvSide $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
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
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.new

mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec
   { mType = C.pack "8"
   , mHeader = headerFIX44
   , mBody = mExecutionReportBody
   , mTrailer = trailerFIX44 }
   where
      mExecutionReportBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tQuoteRespID) tQuoteRespID $
          LT.insert (tnum tOrdStatusReqID) tOrdStatusReqID $
          LT.insert (tnum tMassStatusReqID) tMassStatusReqID $
          LT.insert (tnum tTotNumReports) tTotNumReports $
          LT.insert (tnum tLastRptRequested) tLastRptRequested $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
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
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDayBookingInst) tDayBookingInst $
          LT.insert (tnum tBookingUnit) tBookingUnit $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tCashMargin) tCashMargin $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tPeggedPrice) tPeggedPrice $
          LT.insert (tnum tDiscretionPrice) tDiscretionPrice $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tTargetStrategyPerformance) tTargetStrategyPerformance $
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
          LT.insert (tnum tLastQty) tLastQty $
          LT.insert (tnum tUnderlyingLastQty) tUnderlyingLastQty $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tUnderlyingLastPx) tUnderlyingLastPx $
          LT.insert (tnum tLastParPx) tLastParPx $
          LT.insert (tnum tLastSpotRate) tLastSpotRate $
          LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tTimeBracket) tTimeBracket $
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
          LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tExDate) tExDate $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tInterestAtMaturity) tInterestAtMaturity $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
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
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
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
          LT.insert (tnum tLastLiquidityInd) tLastLiquidityInd $
          LT.insert (tnum tCopyMsgIndicator) tCopyMsgIndicator $
          LT.new

mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec
   { mType = C.pack "9"
   , mHeader = headerFIX44
   , mBody = mOrderCancelRejectBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tCxlRejResponseTo) tCxlRejResponseTo $
          LT.insert (tnum tCxlRejReason) tCxlRejReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mLogon :: FIXMessageSpec
mLogon = FMSpec
   { mType = C.pack "A"
   , mHeader = headerFIX44
   , mBody = mLogonBody
   , mTrailer = trailerFIX44 }
   where
      mLogonBody = 
          LT.insert (tnum tEncryptMethod) tEncryptMethod $
          LT.insert (tnum tHeartBtInt) tHeartBtInt $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.insert (tnum tResetSeqNumFlag) tResetSeqNumFlag $
          LT.insert (tnum tNextExpectedMsgSeqNum) tNextExpectedMsgSeqNum $
          LT.insert (tnum tMaxMessageSize) tMaxMessageSize $
          LT.insert (tnum tTestMessageIndicator) tTestMessageIndicator $
          LT.insert (tnum tUsername) tUsername $
          LT.insert (tnum tPassword) tPassword $
          LT.new

mNews :: FIXMessageSpec
mNews = FMSpec
   { mType = C.pack "B"
   , mHeader = headerFIX44
   , mBody = mNewsBody
   , mTrailer = trailerFIX44 }
   where
      mNewsBody = 
          LT.insert (tnum tOrigTime) tOrigTime $
          LT.insert (tnum tUrgency) tUrgency $
          LT.insert (tnum tHeadline) tHeadline $
          LT.insert (tnum tEncodedHeadlineLen) tEncodedHeadlineLen $
          LT.insert (tnum tEncodedHeadline) tEncodedHeadline $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.new

mEmail :: FIXMessageSpec
mEmail = FMSpec
   { mType = C.pack "C"
   , mHeader = headerFIX44
   , mBody = mEmailBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tRawData) tRawData $
          LT.new

mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec
   { mType = C.pack "D"
   , mHeader = headerFIX44
   , mBody = mNewOrderSingleBody
   , mTrailer = trailerFIX44 }
   where
      mNewOrderSingleBody = 
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDayBookingInst) tDayBookingInst $
          LT.insert (tnum tBookingUnit) tBookingUnit $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tCashMargin) tCashMargin $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tPrice2) tPrice2 $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tCancellationRights) tCancellationRights $
          LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tDesignation) tDesignation $
          LT.new

mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec
   { mType = C.pack "E"
   , mHeader = headerFIX44
   , mBody = mNewOrderListBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tAllowableOneSidednessPct) tAllowableOneSidednessPct $
          LT.insert (tnum tAllowableOneSidednessValue) tAllowableOneSidednessValue $
          LT.insert (tnum tAllowableOneSidednessCurr) tAllowableOneSidednessCurr $
          LT.insert (tnum tTotNoOrders) tTotNoOrders $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec
   { mType = C.pack "F"
   , mHeader = headerFIX44
   , mBody = mOrderCancelRequestBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec
   { mType = C.pack "G"
   , mHeader = headerFIX44
   , mBody = mOrderCancelReplaceRequestBody
   , mTrailer = trailerFIX44 }
   where
      mOrderCancelReplaceRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDayBookingInst) tDayBookingInst $
          LT.insert (tnum tBookingUnit) tBookingUnit $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tCashMargin) tCashMargin $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
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
          LT.new

mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec
   { mType = C.pack "H"
   , mHeader = headerFIX44
   , mBody = mOrderStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mOrderStatusRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tOrdStatusReqID) tOrdStatusReqID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tSide) tSide $
          LT.new

mAllocationInstruction :: FIXMessageSpec
mAllocationInstruction = FMSpec
   { mType = C.pack "J"
   , mHeader = headerFIX44
   , mBody = mAllocationInstructionBody
   , mTrailer = trailerFIX44 }
   where
      mAllocationInstructionBody = 
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tAllocTransType) tAllocTransType $
          LT.insert (tnum tAllocType) tAllocType $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tRefAllocID) tRefAllocID $
          LT.insert (tnum tAllocCancReplaceReason) tAllocCancReplaceReason $
          LT.insert (tnum tAllocIntermedReqType) tAllocIntermedReqType $
          LT.insert (tnum tAllocLinkID) tAllocLinkID $
          LT.insert (tnum tAllocLinkType) tAllocLinkType $
          LT.insert (tnum tBookingRefID) tBookingRefID $
          LT.insert (tnum tAllocNoOrdersType) tAllocNoOrdersType $
          LT.insert (tnum tPreviouslyReported) tPreviouslyReported $
          LT.insert (tnum tReversalIndicator) tReversalIndicator $
          LT.insert (tnum tMatchType) tMatchType $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tAvgParPx) tAvgParPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAvgPxPrecision) tAvgPxPrecision $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
          LT.insert (tnum tConcession) tConcession $
          LT.insert (tnum tTotalTakedown) tTotalTakedown $
          LT.insert (tnum tNetMoney) tNetMoney $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tAutoAcceptIndicator) tAutoAcceptIndicator $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tTotalAccruedInterestAmt) tTotalAccruedInterestAmt $
          LT.insert (tnum tInterestAtMaturity) tInterestAtMaturity $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tLegalConfirm) tLegalConfirm $
          LT.insert (tnum tTotNoAllocs) tTotNoAllocs $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec
   { mType = C.pack "K"
   , mHeader = headerFIX44
   , mBody = mListCancelRequestBody
   , mTrailer = trailerFIX44 }
   where
      mListCancelRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mListExecute :: FIXMessageSpec
mListExecute = FMSpec
   { mType = C.pack "L"
   , mHeader = headerFIX44
   , mBody = mListExecuteBody
   , mTrailer = trailerFIX44 }
   where
      mListExecuteBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec
   { mType = C.pack "M"
   , mHeader = headerFIX44
   , mBody = mListStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mListStatusRequestBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mListStatus :: FIXMessageSpec
mListStatus = FMSpec
   { mType = C.pack "N"
   , mHeader = headerFIX44
   , mBody = mListStatusBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mAllocationInstructionAck :: FIXMessageSpec
mAllocationInstructionAck = FMSpec
   { mType = C.pack "P"
   , mHeader = headerFIX44
   , mBody = mAllocationInstructionAckBody
   , mTrailer = trailerFIX44 }
   where
      mAllocationInstructionAckBody = 
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAllocStatus) tAllocStatus $
          LT.insert (tnum tAllocRejCode) tAllocRejCode $
          LT.insert (tnum tAllocType) tAllocType $
          LT.insert (tnum tAllocIntermedReqType) tAllocIntermedReqType $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tProduct) tProduct $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec
   { mType = C.pack "Q"
   , mHeader = headerFIX44
   , mBody = mDontKnowTradeBody
   , mTrailer = trailerFIX44 }
   where
      mDontKnowTradeBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tDKReason) tDKReason $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tLastQty) tLastQty $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec
   { mType = C.pack "R"
   , mHeader = headerFIX44
   , mBody = mQuoteRequestBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteRequestBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mQuote :: FIXMessageSpec
mQuote = FMSpec
   { mType = C.pack "S"
   , mHeader = headerFIX44
   , mBody = mQuoteBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteRespID) tQuoteRespID $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
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
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
          LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
          LT.insert (tnum tSettlCurrBidFxRate) tSettlCurrBidFxRate $
          LT.insert (tnum tSettlCurrOfferFxRate) tSettlCurrOfferFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec
   { mType = C.pack "T"
   , mHeader = headerFIX44
   , mBody = mSettlementInstructionsBody
   , mTrailer = trailerFIX44 }
   where
      mSettlementInstructionsBody = 
          LT.insert (tnum tSettlInstMsgID) tSettlInstMsgID $
          LT.insert (tnum tSettlInstReqID) tSettlInstReqID $
          LT.insert (tnum tSettlInstMode) tSettlInstMode $
          LT.insert (tnum tSettlInstReqRejCode) tSettlInstReqRejCode $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.new

mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec
   { mType = C.pack "V"
   , mHeader = headerFIX44
   , mBody = mMarketDataRequestBody
   , mTrailer = trailerFIX44 }
   where
      mMarketDataRequestBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tMarketDepth) tMarketDepth $
          LT.insert (tnum tMDUpdateType) tMDUpdateType $
          LT.insert (tnum tAggregatedBook) tAggregatedBook $
          LT.insert (tnum tOpenCloseSettlFlag) tOpenCloseSettlFlag $
          LT.insert (tnum tScope) tScope $
          LT.insert (tnum tMDImplicitDelete) tMDImplicitDelete $
          LT.new

mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec
   { mType = C.pack "W"
   , mHeader = headerFIX44
   , mBody = mMarketDataSnapshotFullRefreshBody
   , mTrailer = trailerFIX44 }
   where
      mMarketDataSnapshotFullRefreshBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tFinancialStatus) tFinancialStatus $
          LT.insert (tnum tCorporateAction) tCorporateAction $
          LT.insert (tnum tNetChgPrevDay) tNetChgPrevDay $
          LT.insert (tnum tApplQueueDepth) tApplQueueDepth $
          LT.insert (tnum tApplQueueResolution) tApplQueueResolution $
          LT.new

mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec
   { mType = C.pack "X"
   , mHeader = headerFIX44
   , mBody = mMarketDataIncrementalRefreshBody
   , mTrailer = trailerFIX44 }
   where
      mMarketDataIncrementalRefreshBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tApplQueueDepth) tApplQueueDepth $
          LT.insert (tnum tApplQueueResolution) tApplQueueResolution $
          LT.new

mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec
   { mType = C.pack "Y"
   , mHeader = headerFIX44
   , mBody = mMarketDataRequestRejectBody
   , mTrailer = trailerFIX44 }
   where
      mMarketDataRequestRejectBody = 
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mQuoteCancel :: FIXMessageSpec
mQuoteCancel = FMSpec
   { mType = C.pack "Z"
   , mHeader = headerFIX44
   , mBody = mQuoteCancelBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteCancelBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.new

mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec
   { mType = C.pack "a"
   , mHeader = headerFIX44
   , mBody = mQuoteStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteStatusRequestBody = 
          LT.insert (tnum tQuoteStatusReqID) tQuoteStatusReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mMassQuoteAcknowledgement :: FIXMessageSpec
mMassQuoteAcknowledgement = FMSpec
   { mType = C.pack "b"
   , mHeader = headerFIX44
   , mBody = mMassQuoteAcknowledgementBody
   , mTrailer = trailerFIX44 }
   where
      mMassQuoteAcknowledgementBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteStatus) tQuoteStatus $
          LT.insert (tnum tQuoteRejectReason) tQuoteRejectReason $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec
   { mType = C.pack "c"
   , mHeader = headerFIX44
   , mBody = mSecurityDefinitionRequestBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityDefinitionRequestBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityRequestType) tSecurityRequestType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tExpirationCycle) tExpirationCycle $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec
   { mType = C.pack "d"
   , mHeader = headerFIX44
   , mBody = mSecurityDefinitionBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityDefinitionBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tExpirationCycle) tExpirationCycle $
          LT.insert (tnum tRoundLot) tRoundLot $
          LT.insert (tnum tMinTradeVol) tMinTradeVol $
          LT.new

mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec
   { mType = C.pack "e"
   , mHeader = headerFIX44
   , mBody = mSecurityStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityStatusRequestBody = 
          LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.new

mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec
   { mType = C.pack "f"
   , mHeader = headerFIX44
   , mBody = mSecurityStatusBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityStatusBody = 
          LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
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
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec
   { mType = C.pack "g"
   , mHeader = headerFIX44
   , mBody = mTradingSessionStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mTradingSessionStatusRequestBody = 
          LT.insert (tnum tTradSesReqID) tTradSesReqID $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tTradSesMethod) tTradSesMethod $
          LT.insert (tnum tTradSesMode) tTradSesMode $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec
   { mType = C.pack "h"
   , mHeader = headerFIX44
   , mBody = mTradingSessionStatusBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mMassQuote :: FIXMessageSpec
mMassQuote = FMSpec
   { mType = C.pack "i"
   , mHeader = headerFIX44
   , mBody = mMassQuoteBody
   , mTrailer = trailerFIX44 }
   where
      mMassQuoteBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDefBidSize) tDefBidSize $
          LT.insert (tnum tDefOfferSize) tDefOfferSize $
          LT.new

mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec
   { mType = C.pack "j"
   , mHeader = headerFIX44
   , mBody = mBusinessMessageRejectBody
   , mTrailer = trailerFIX44 }
   where
      mBusinessMessageRejectBody = 
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tRefMsgType) tRefMsgType $
          LT.insert (tnum tBusinessRejectRefID) tBusinessRejectRefID $
          LT.insert (tnum tBusinessRejectReason) tBusinessRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mBidRequest :: FIXMessageSpec
mBidRequest = FMSpec
   { mType = C.pack "k"
   , mHeader = headerFIX44
   , mBody = mBidRequestBody
   , mTrailer = trailerFIX44 }
   where
      mBidRequestBody = 
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.insert (tnum tBidRequestTransType) tBidRequestTransType $
          LT.insert (tnum tListName) tListName $
          LT.insert (tnum tTotNoRelatedSym) tTotNoRelatedSym $
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
          LT.insert (tnum tBidTradeType) tBidTradeType $
          LT.insert (tnum tBasisPxType) tBasisPxType $
          LT.insert (tnum tStrikeTime) tStrikeTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mBidResponse :: FIXMessageSpec
mBidResponse = FMSpec
   { mType = C.pack "l"
   , mHeader = headerFIX44
   , mBody = mBidResponseBody
   , mTrailer = trailerFIX44 }
   where
      mBidResponseBody = 
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.new

mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec
   { mType = C.pack "m"
   , mHeader = headerFIX44
   , mBody = mListStrikePriceBody
   , mTrailer = trailerFIX44 }
   where
      mListStrikePriceBody = 
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mRegistrationInstructions :: FIXMessageSpec
mRegistrationInstructions = FMSpec
   { mType = C.pack "o"
   , mHeader = headerFIX44
   , mBody = mRegistrationInstructionsBody
   , mTrailer = trailerFIX44 }
   where
      mRegistrationInstructionsBody = 
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tRegistTransType) tRegistTransType $
          LT.insert (tnum tRegistRefID) tRegistRefID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tRegistAcctType) tRegistAcctType $
          LT.insert (tnum tTaxAdvantageType) tTaxAdvantageType $
          LT.insert (tnum tOwnershipType) tOwnershipType $
          LT.new

mRegistrationInstructionsResponse :: FIXMessageSpec
mRegistrationInstructionsResponse = FMSpec
   { mType = C.pack "p"
   , mHeader = headerFIX44
   , mBody = mRegistrationInstructionsResponseBody
   , mTrailer = trailerFIX44 }
   where
      mRegistrationInstructionsResponseBody = 
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tRegistTransType) tRegistTransType $
          LT.insert (tnum tRegistRefID) tRegistRefID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tRegistStatus) tRegistStatus $
          LT.insert (tnum tRegistRejReasonCode) tRegistRejReasonCode $
          LT.insert (tnum tRegistRejReasonText) tRegistRejReasonText $
          LT.new

mOrderMassCancelRequest :: FIXMessageSpec
mOrderMassCancelRequest = FMSpec
   { mType = C.pack "q"
   , mHeader = headerFIX44
   , mBody = mOrderMassCancelRequestBody
   , mTrailer = trailerFIX44 }
   where
      mOrderMassCancelRequestBody = 
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tMassCancelRequestType) tMassCancelRequestType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mOrderMassCancelReport :: FIXMessageSpec
mOrderMassCancelReport = FMSpec
   { mType = C.pack "r"
   , mHeader = headerFIX44
   , mBody = mOrderMassCancelReportBody
   , mTrailer = trailerFIX44 }
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
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mNewOrderCross :: FIXMessageSpec
mNewOrderCross = FMSpec
   { mType = C.pack "s"
   , mHeader = headerFIX44
   , mBody = mNewOrderCrossBody
   , mTrailer = trailerFIX44 }
   where
      mNewOrderCrossBody = 
          LT.insert (tnum tCrossID) tCrossID $
          LT.insert (tnum tCrossType) tCrossType $
          LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tCancellationRights) tCancellationRights $
          LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tDesignation) tDesignation $
          LT.new

mCrossOrderCancelReplaceRequest :: FIXMessageSpec
mCrossOrderCancelReplaceRequest = FMSpec
   { mType = C.pack "t"
   , mHeader = headerFIX44
   , mBody = mCrossOrderCancelReplaceRequestBody
   , mTrailer = trailerFIX44 }
   where
      mCrossOrderCancelReplaceRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tCrossID) tCrossID $
          LT.insert (tnum tOrigCrossID) tOrigCrossID $
          LT.insert (tnum tCrossType) tCrossType $
          LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tCancellationRights) tCancellationRights $
          LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tDesignation) tDesignation $
          LT.new

mCrossOrderCancelRequest :: FIXMessageSpec
mCrossOrderCancelRequest = FMSpec
   { mType = C.pack "u"
   , mHeader = headerFIX44
   , mBody = mCrossOrderCancelRequestBody
   , mTrailer = trailerFIX44 }
   where
      mCrossOrderCancelRequestBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tCrossID) tCrossID $
          LT.insert (tnum tOrigCrossID) tOrigCrossID $
          LT.insert (tnum tCrossType) tCrossType $
          LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.new

mSecurityTypeRequest :: FIXMessageSpec
mSecurityTypeRequest = FMSpec
   { mType = C.pack "v"
   , mHeader = headerFIX44
   , mBody = mSecurityTypeRequestBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityTypeRequestBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tProduct) tProduct $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tSecuritySubType) tSecuritySubType $
          LT.new

mSecurityTypes :: FIXMessageSpec
mSecurityTypes = FMSpec
   { mType = C.pack "w"
   , mHeader = headerFIX44
   , mBody = mSecurityTypesBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityTypesBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityResponseType) tSecurityResponseType $
          LT.insert (tnum tTotNoSecurityTypes) tTotNoSecurityTypes $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mSecurityListRequest :: FIXMessageSpec
mSecurityListRequest = FMSpec
   { mType = C.pack "x"
   , mHeader = headerFIX44
   , mBody = mSecurityListRequestBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityListRequestBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityListRequestType) tSecurityListRequestType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mSecurityList :: FIXMessageSpec
mSecurityList = FMSpec
   { mType = C.pack "y"
   , mHeader = headerFIX44
   , mBody = mSecurityListBody
   , mTrailer = trailerFIX44 }
   where
      mSecurityListBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
          LT.insert (tnum tTotNoRelatedSym) tTotNoRelatedSym $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mDerivativeSecurityListRequest :: FIXMessageSpec
mDerivativeSecurityListRequest = FMSpec
   { mType = C.pack "z"
   , mHeader = headerFIX44
   , mBody = mDerivativeSecurityListRequestBody
   , mTrailer = trailerFIX44 }
   where
      mDerivativeSecurityListRequestBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityListRequestType) tSecurityListRequestType $
          LT.insert (tnum tSecuritySubType) tSecuritySubType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mDerivativeSecurityList :: FIXMessageSpec
mDerivativeSecurityList = FMSpec
   { mType = C.pack "AA"
   , mHeader = headerFIX44
   , mBody = mDerivativeSecurityListBody
   , mTrailer = trailerFIX44 }
   where
      mDerivativeSecurityListBody = 
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
          LT.insert (tnum tTotNoRelatedSym) tTotNoRelatedSym $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mNewOrderMultileg :: FIXMessageSpec
mNewOrderMultileg = FMSpec
   { mType = C.pack "AB"
   , mHeader = headerFIX44
   , mBody = mNewOrderMultilegBody
   , mTrailer = trailerFIX44 }
   where
      mNewOrderMultilegBody = 
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDayBookingInst) tDayBookingInst $
          LT.insert (tnum tBookingUnit) tBookingUnit $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tCashMargin) tCashMargin $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tCancellationRights) tCancellationRights $
          LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tDesignation) tDesignation $
          LT.insert (tnum tMultiLegRptTypeReq) tMultiLegRptTypeReq $
          LT.new

mMultilegOrderCancelReplace :: FIXMessageSpec
mMultilegOrderCancelReplace = FMSpec
   { mType = C.pack "AC"
   , mHeader = headerFIX44
   , mBody = mMultilegOrderCancelReplaceBody
   , mTrailer = trailerFIX44 }
   where
      mMultilegOrderCancelReplaceBody = 
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tOrigClOrdID) tOrigClOrdID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tOrigOrdModTime) tOrigOrdModTime $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDayBookingInst) tDayBookingInst $
          LT.insert (tnum tBookingUnit) tBookingUnit $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tCashMargin) tCashMargin $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tHandlInst) tHandlInst $
          LT.insert (tnum tExecInst) tExecInst $
          LT.insert (tnum tMinQty) tMinQty $
          LT.insert (tnum tMaxFloor) tMaxFloor $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrevClosePx) tPrevClosePx $
          LT.insert (tnum tLocateReqd) tLocateReqd $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tStopPx) tStopPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tComplianceID) tComplianceID $
          LT.insert (tnum tSolicitedFlag) tSolicitedFlag $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tTimeInForce) tTimeInForce $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tGTBookingInst) tGTBookingInst $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tForexReq) tForexReq $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tCoveredOrUncovered) tCoveredOrUncovered $
          LT.insert (tnum tMaxShow) tMaxShow $
          LT.insert (tnum tTargetStrategy) tTargetStrategy $
          LT.insert (tnum tTargetStrategyParameters) tTargetStrategyParameters $
          LT.insert (tnum tParticipationRate) tParticipationRate $
          LT.insert (tnum tCancellationRights) tCancellationRights $
          LT.insert (tnum tMoneyLaunderingStatus) tMoneyLaunderingStatus $
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tDesignation) tDesignation $
          LT.insert (tnum tMultiLegRptTypeReq) tMultiLegRptTypeReq $
          LT.new

mTradeCaptureReportRequest :: FIXMessageSpec
mTradeCaptureReportRequest = FMSpec
   { mType = C.pack "AD"
   , mHeader = headerFIX44
   , mBody = mTradeCaptureReportRequestBody
   , mTrailer = trailerFIX44 }
   where
      mTradeCaptureReportRequestBody = 
          LT.insert (tnum tTradeRequestID) tTradeRequestID $
          LT.insert (tnum tTradeRequestType) tTradeRequestType $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTradeReportID) tTradeReportID $
          LT.insert (tnum tSecondaryTradeReportID) tSecondaryTradeReportID $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tExecType) tExecType $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tTrdType) tTrdType $
          LT.insert (tnum tTrdSubType) tTrdSubType $
          LT.insert (tnum tTransferReason) tTransferReason $
          LT.insert (tnum tSecondaryTrdType) tSecondaryTrdType $
          LT.insert (tnum tTradeLinkID) tTradeLinkID $
          LT.insert (tnum tTrdMatchID) tTrdMatchID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tTimeBracket) tTimeBracket $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
          LT.insert (tnum tTradeInputSource) tTradeInputSource $
          LT.insert (tnum tTradeInputDevice) tTradeInputDevice $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mTradeCaptureReport :: FIXMessageSpec
mTradeCaptureReport = FMSpec
   { mType = C.pack "AE"
   , mHeader = headerFIX44
   , mBody = mTradeCaptureReportBody
   , mTrailer = trailerFIX44 }
   where
      mTradeCaptureReportBody = 
          LT.insert (tnum tTradeReportID) tTradeReportID $
          LT.insert (tnum tTradeReportTransType) tTradeReportTransType $
          LT.insert (tnum tTradeReportType) tTradeReportType $
          LT.insert (tnum tTradeRequestID) tTradeRequestID $
          LT.insert (tnum tTrdType) tTrdType $
          LT.insert (tnum tTrdSubType) tTrdSubType $
          LT.insert (tnum tSecondaryTrdType) tSecondaryTrdType $
          LT.insert (tnum tTransferReason) tTransferReason $
          LT.insert (tnum tExecType) tExecType $
          LT.insert (tnum tTotNumTradeReports) tTotNumTradeReports $
          LT.insert (tnum tLastRptRequested) tLastRptRequested $
          LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTradeReportRefID) tTradeReportRefID $
          LT.insert (tnum tSecondaryTradeReportRefID) tSecondaryTradeReportRefID $
          LT.insert (tnum tSecondaryTradeReportID) tSecondaryTradeReportID $
          LT.insert (tnum tTradeLinkID) tTradeLinkID $
          LT.insert (tnum tTrdMatchID) tTrdMatchID $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tOrdStatus) tOrdStatus $
          LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
          LT.insert (tnum tExecRestatementReason) tExecRestatementReason $
          LT.insert (tnum tPreviouslyReported) tPreviouslyReported $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tUnderlyingTradingSessionID) tUnderlyingTradingSessionID $
          LT.insert (tnum tUnderlyingTradingSessionSubID) tUnderlyingTradingSessionSubID $
          LT.insert (tnum tLastQty) tLastQty $
          LT.insert (tnum tLastPx) tLastPx $
          LT.insert (tnum tLastParPx) tLastParPx $
          LT.insert (tnum tLastSpotRate) tLastSpotRate $
          LT.insert (tnum tLastForwardPoints) tLastForwardPoints $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tAvgPxIndicator) tAvgPxIndicator $
          LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
          LT.insert (tnum tTradeLegRefID) tTradeLegRefID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tMatchType) tMatchType $
          LT.new

mOrderMassStatusRequest :: FIXMessageSpec
mOrderMassStatusRequest = FMSpec
   { mType = C.pack "AF"
   , mHeader = headerFIX44
   , mBody = mOrderMassStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mOrderMassStatusRequestBody = 
          LT.insert (tnum tMassStatusReqID) tMassStatusReqID $
          LT.insert (tnum tMassStatusReqType) tMassStatusReqType $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.new

mQuoteRequestReject :: FIXMessageSpec
mQuoteRequestReject = FMSpec
   { mType = C.pack "AG"
   , mHeader = headerFIX44
   , mBody = mQuoteRequestRejectBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteRequestRejectBody = 
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tQuoteRequestRejectReason) tQuoteRequestRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mRFQRequest :: FIXMessageSpec
mRFQRequest = FMSpec
   { mType = C.pack "AH"
   , mHeader = headerFIX44
   , mBody = mRFQRequestBody
   , mTrailer = trailerFIX44 }
   where
      mRFQRequestBody = 
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.new

mQuoteStatusReport :: FIXMessageSpec
mQuoteStatusReport = FMSpec
   { mType = C.pack "AI"
   , mHeader = headerFIX44
   , mBody = mQuoteStatusReportBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteStatusReportBody = 
          LT.insert (tnum tQuoteStatusReqID) tQuoteStatusReqID $
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteRespID) tQuoteRespID $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
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
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
          LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
          LT.insert (tnum tSettlCurrBidFxRate) tSettlCurrBidFxRate $
          LT.insert (tnum tSettlCurrOfferFxRate) tSettlCurrOfferFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tQuoteStatus) tQuoteStatus $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mQuoteResponse :: FIXMessageSpec
mQuoteResponse = FMSpec
   { mType = C.pack "AJ"
   , mHeader = headerFIX44
   , mBody = mQuoteResponseBody
   , mTrailer = trailerFIX44 }
   where
      mQuoteResponseBody = 
          LT.insert (tnum tQuoteRespID) tQuoteRespID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteRespType) tQuoteRespType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tIOIID) tIOIID $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tSettlDate2) tSettlDate2 $
          LT.insert (tnum tOrderQty2) tOrderQty2 $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
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
          LT.insert (tnum tOrdType) tOrdType $
          LT.insert (tnum tBidForwardPoints2) tBidForwardPoints2 $
          LT.insert (tnum tOfferForwardPoints2) tOfferForwardPoints2 $
          LT.insert (tnum tSettlCurrBidFxRate) tSettlCurrBidFxRate $
          LT.insert (tnum tSettlCurrOfferFxRate) tSettlCurrOfferFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tCommission) tCommission $
          LT.insert (tnum tCommType) tCommType $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tExDestination) tExDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.new

mConfirmation :: FIXMessageSpec
mConfirmation = FMSpec
   { mType = C.pack "AK"
   , mHeader = headerFIX44
   , mBody = mConfirmationBody
   , mTrailer = trailerFIX44 }
   where
      mConfirmationBody = 
          LT.insert (tnum tConfirmID) tConfirmID $
          LT.insert (tnum tConfirmRefID) tConfirmRefID $
          LT.insert (tnum tConfirmReqID) tConfirmReqID $
          LT.insert (tnum tConfirmTransType) tConfirmTransType $
          LT.insert (tnum tConfirmType) tConfirmType $
          LT.insert (tnum tCopyMsgIndicator) tCopyMsgIndicator $
          LT.insert (tnum tLegalConfirm) tLegalConfirm $
          LT.insert (tnum tConfirmStatus) tConfirmStatus $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tAllocQty) tAllocQty $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tAllocAccount) tAllocAccount $
          LT.insert (tnum tAllocAcctIDSource) tAllocAcctIDSource $
          LT.insert (tnum tAllocAccountType) tAllocAccountType $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tAvgPxPrecision) tAvgPxPrecision $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAvgParPx) tAvgParPx $
          LT.insert (tnum tReportedPx) tReportedPx $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tProcessCode) tProcessCode $
          LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tExDate) tExDate $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tInterestAtMaturity) tInterestAtMaturity $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tConcession) tConcession $
          LT.insert (tnum tTotalTakedown) tTotalTakedown $
          LT.insert (tnum tNetMoney) tNetMoney $
          LT.insert (tnum tMaturityNetMoney) tMaturityNetMoney $
          LT.insert (tnum tSettlCurrAmt) tSettlCurrAmt $
          LT.insert (tnum tSettlCurrency) tSettlCurrency $
          LT.insert (tnum tSettlCurrFxRate) tSettlCurrFxRate $
          LT.insert (tnum tSettlCurrFxRateCalc) tSettlCurrFxRateCalc $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tSharedCommission) tSharedCommission $
          LT.new

mPositionMaintenanceRequest :: FIXMessageSpec
mPositionMaintenanceRequest = FMSpec
   { mType = C.pack "AL"
   , mHeader = headerFIX44
   , mBody = mPositionMaintenanceRequestBody
   , mTrailer = trailerFIX44 }
   where
      mPositionMaintenanceRequestBody = 
          LT.insert (tnum tPosReqID) tPosReqID $
          LT.insert (tnum tPosTransType) tPosTransType $
          LT.insert (tnum tPosMaintAction) tPosMaintAction $
          LT.insert (tnum tOrigPosReqRefID) tOrigPosReqRefID $
          LT.insert (tnum tPosMaintRptRefID) tPosMaintRptRefID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAdjustmentType) tAdjustmentType $
          LT.insert (tnum tContraryInstructionIndicator) tContraryInstructionIndicator $
          LT.insert (tnum tPriorSpreadIndicator) tPriorSpreadIndicator $
          LT.insert (tnum tThresholdAmount) tThresholdAmount $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mPositionMaintenanceReport :: FIXMessageSpec
mPositionMaintenanceReport = FMSpec
   { mType = C.pack "AM"
   , mHeader = headerFIX44
   , mBody = mPositionMaintenanceReportBody
   , mTrailer = trailerFIX44 }
   where
      mPositionMaintenanceReportBody = 
          LT.insert (tnum tPosMaintRptID) tPosMaintRptID $
          LT.insert (tnum tPosTransType) tPosTransType $
          LT.insert (tnum tPosReqID) tPosReqID $
          LT.insert (tnum tPosMaintAction) tPosMaintAction $
          LT.insert (tnum tOrigPosReqRefID) tOrigPosReqRefID $
          LT.insert (tnum tPosMaintStatus) tPosMaintStatus $
          LT.insert (tnum tPosMaintResult) tPosMaintResult $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAdjustmentType) tAdjustmentType $
          LT.insert (tnum tThresholdAmount) tThresholdAmount $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mRequestForPositions :: FIXMessageSpec
mRequestForPositions = FMSpec
   { mType = C.pack "AN"
   , mHeader = headerFIX44
   , mBody = mRequestForPositionsBody
   , mTrailer = trailerFIX44 }
   where
      mRequestForPositionsBody = 
          LT.insert (tnum tPosReqID) tPosReqID $
          LT.insert (tnum tPosReqType) tPosReqType $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mRequestForPositionsAck :: FIXMessageSpec
mRequestForPositionsAck = FMSpec
   { mType = C.pack "AO"
   , mHeader = headerFIX44
   , mBody = mRequestForPositionsAckBody
   , mTrailer = trailerFIX44 }
   where
      mRequestForPositionsAckBody = 
          LT.insert (tnum tPosMaintRptID) tPosMaintRptID $
          LT.insert (tnum tPosReqID) tPosReqID $
          LT.insert (tnum tTotalNumPosReports) tTotalNumPosReports $
          LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
          LT.insert (tnum tPosReqResult) tPosReqResult $
          LT.insert (tnum tPosReqStatus) tPosReqStatus $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mPositionReport :: FIXMessageSpec
mPositionReport = FMSpec
   { mType = C.pack "AP"
   , mHeader = headerFIX44
   , mBody = mPositionReportBody
   , mTrailer = trailerFIX44 }
   where
      mPositionReportBody = 
          LT.insert (tnum tPosMaintRptID) tPosMaintRptID $
          LT.insert (tnum tPosReqID) tPosReqID $
          LT.insert (tnum tPosReqType) tPosReqType $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTotalNumPosReports) tTotalNumPosReports $
          LT.insert (tnum tUnsolicitedIndicator) tUnsolicitedIndicator $
          LT.insert (tnum tPosReqResult) tPosReqResult $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tSettlPrice) tSettlPrice $
          LT.insert (tnum tSettlPriceType) tSettlPriceType $
          LT.insert (tnum tPriorSettlPrice) tPriorSettlPrice $
          LT.insert (tnum tRegistStatus) tRegistStatus $
          LT.insert (tnum tDeliveryDate) tDeliveryDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mTradeCaptureReportRequestAck :: FIXMessageSpec
mTradeCaptureReportRequestAck = FMSpec
   { mType = C.pack "AQ"
   , mHeader = headerFIX44
   , mBody = mTradeCaptureReportRequestAckBody
   , mTrailer = trailerFIX44 }
   where
      mTradeCaptureReportRequestAckBody = 
          LT.insert (tnum tTradeRequestID) tTradeRequestID $
          LT.insert (tnum tTradeRequestType) tTradeRequestType $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTotNumTradeReports) tTotNumTradeReports $
          LT.insert (tnum tTradeRequestResult) tTradeRequestResult $
          LT.insert (tnum tTradeRequestStatus) tTradeRequestStatus $
          LT.insert (tnum tMultiLegReportingType) tMultiLegReportingType $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mTradeCaptureReportAck :: FIXMessageSpec
mTradeCaptureReportAck = FMSpec
   { mType = C.pack "AR"
   , mHeader = headerFIX44
   , mBody = mTradeCaptureReportAckBody
   , mTrailer = trailerFIX44 }
   where
      mTradeCaptureReportAckBody = 
          LT.insert (tnum tTradeReportID) tTradeReportID $
          LT.insert (tnum tTradeReportTransType) tTradeReportTransType $
          LT.insert (tnum tTradeReportType) tTradeReportType $
          LT.insert (tnum tTrdType) tTrdType $
          LT.insert (tnum tTrdSubType) tTrdSubType $
          LT.insert (tnum tSecondaryTrdType) tSecondaryTrdType $
          LT.insert (tnum tTransferReason) tTransferReason $
          LT.insert (tnum tExecType) tExecType $
          LT.insert (tnum tTradeReportRefID) tTradeReportRefID $
          LT.insert (tnum tSecondaryTradeReportRefID) tSecondaryTradeReportRefID $
          LT.insert (tnum tTrdRptStatus) tTrdRptStatus $
          LT.insert (tnum tTradeReportRejectReason) tTradeReportRejectReason $
          LT.insert (tnum tSecondaryTradeReportID) tSecondaryTradeReportID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTradeLinkID) tTradeLinkID $
          LT.insert (tnum tTrdMatchID) tTrdMatchID $
          LT.insert (tnum tExecID) tExecID $
          LT.insert (tnum tSecondaryExecID) tSecondaryExecID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tClearingFeeIndicator) tClearingFeeIndicator $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tOrderRestrictions) tOrderRestrictions $
          LT.insert (tnum tCustOrderCapacity) tCustOrderCapacity $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tPreallocMethod) tPreallocMethod $
          LT.new

mAllocationReport :: FIXMessageSpec
mAllocationReport = FMSpec
   { mType = C.pack "AS"
   , mHeader = headerFIX44
   , mBody = mAllocationReportBody
   , mTrailer = trailerFIX44 }
   where
      mAllocationReportBody = 
          LT.insert (tnum tAllocReportID) tAllocReportID $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tAllocTransType) tAllocTransType $
          LT.insert (tnum tAllocReportRefID) tAllocReportRefID $
          LT.insert (tnum tAllocCancReplaceReason) tAllocCancReplaceReason $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tAllocReportType) tAllocReportType $
          LT.insert (tnum tAllocStatus) tAllocStatus $
          LT.insert (tnum tAllocRejCode) tAllocRejCode $
          LT.insert (tnum tRefAllocID) tRefAllocID $
          LT.insert (tnum tAllocIntermedReqType) tAllocIntermedReqType $
          LT.insert (tnum tAllocLinkID) tAllocLinkID $
          LT.insert (tnum tAllocLinkType) tAllocLinkType $
          LT.insert (tnum tBookingRefID) tBookingRefID $
          LT.insert (tnum tAllocNoOrdersType) tAllocNoOrdersType $
          LT.insert (tnum tPreviouslyReported) tPreviouslyReported $
          LT.insert (tnum tReversalIndicator) tReversalIndicator $
          LT.insert (tnum tMatchType) tMatchType $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tLastMkt) tLastMkt $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAvgPx) tAvgPx $
          LT.insert (tnum tAvgParPx) tAvgParPx $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tAvgPxPrecision) tAvgPxPrecision $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSettlType) tSettlType $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tBookingType) tBookingType $
          LT.insert (tnum tGrossTradeAmt) tGrossTradeAmt $
          LT.insert (tnum tConcession) tConcession $
          LT.insert (tnum tTotalTakedown) tTotalTakedown $
          LT.insert (tnum tNetMoney) tNetMoney $
          LT.insert (tnum tPositionEffect) tPositionEffect $
          LT.insert (tnum tAutoAcceptIndicator) tAutoAcceptIndicator $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tNumDaysInterest) tNumDaysInterest $
          LT.insert (tnum tAccruedInterestRate) tAccruedInterestRate $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tTotalAccruedInterestAmt) tTotalAccruedInterestAmt $
          LT.insert (tnum tInterestAtMaturity) tInterestAtMaturity $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tLegalConfirm) tLegalConfirm $
          LT.insert (tnum tTotNoAllocs) tTotNoAllocs $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.new

mAllocationReportAck :: FIXMessageSpec
mAllocationReportAck = FMSpec
   { mType = C.pack "AT"
   , mHeader = headerFIX44
   , mBody = mAllocationReportAckBody
   , mTrailer = trailerFIX44 }
   where
      mAllocationReportAckBody = 
          LT.insert (tnum tAllocReportID) tAllocReportID $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAllocStatus) tAllocStatus $
          LT.insert (tnum tAllocRejCode) tAllocRejCode $
          LT.insert (tnum tAllocReportType) tAllocReportType $
          LT.insert (tnum tAllocIntermedReqType) tAllocIntermedReqType $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tProduct) tProduct $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mConfirmationAck :: FIXMessageSpec
mConfirmationAck = FMSpec
   { mType = C.pack "AU"
   , mHeader = headerFIX44
   , mBody = mConfirmationAckBody
   , mTrailer = trailerFIX44 }
   where
      mConfirmationAckBody = 
          LT.insert (tnum tConfirmID) tConfirmID $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAffirmStatus) tAffirmStatus $
          LT.insert (tnum tConfirmRejReason) tConfirmRejReason $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mSettlementInstructionRequest :: FIXMessageSpec
mSettlementInstructionRequest = FMSpec
   { mType = C.pack "AV"
   , mHeader = headerFIX44
   , mBody = mSettlementInstructionRequestBody
   , mTrailer = trailerFIX44 }
   where
      mSettlementInstructionRequestBody = 
          LT.insert (tnum tSettlInstReqID) tSettlInstReqID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAllocAccount) tAllocAccount $
          LT.insert (tnum tAllocAcctIDSource) tAllocAcctIDSource $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tProduct) tProduct $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tCFICode) tCFICode $
          LT.insert (tnum tEffectiveTime) tEffectiveTime $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tLastUpdateTime) tLastUpdateTime $
          LT.insert (tnum tStandInstDbType) tStandInstDbType $
          LT.insert (tnum tStandInstDbName) tStandInstDbName $
          LT.insert (tnum tStandInstDbID) tStandInstDbID $
          LT.new

mAssignmentReport :: FIXMessageSpec
mAssignmentReport = FMSpec
   { mType = C.pack "AW"
   , mHeader = headerFIX44
   , mBody = mAssignmentReportBody
   , mTrailer = trailerFIX44 }
   where
      mAssignmentReportBody = 
          LT.insert (tnum tAsgnRptID) tAsgnRptID $
          LT.insert (tnum tTotNumAssignmentReports) tTotNumAssignmentReports $
          LT.insert (tnum tLastRptRequested) tLastRptRequested $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tThresholdAmount) tThresholdAmount $
          LT.insert (tnum tSettlPrice) tSettlPrice $
          LT.insert (tnum tSettlPriceType) tSettlPriceType $
          LT.insert (tnum tUnderlyingSettlPrice) tUnderlyingSettlPrice $
          LT.insert (tnum tExpireDate) tExpireDate $
          LT.insert (tnum tAssignmentMethod) tAssignmentMethod $
          LT.insert (tnum tAssignmentUnit) tAssignmentUnit $
          LT.insert (tnum tOpenInterest) tOpenInterest $
          LT.insert (tnum tExerciseMethod) tExerciseMethod $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mCollateralRequest :: FIXMessageSpec
mCollateralRequest = FMSpec
   { mType = C.pack "AX"
   , mHeader = headerFIX44
   , mBody = mCollateralRequestBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralRequestBody = 
          LT.insert (tnum tCollReqID) tCollReqID $
          LT.insert (tnum tCollAsgnReason) tCollAsgnReason $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tMarginExcess) tMarginExcess $
          LT.insert (tnum tTotalNetValue) tTotalNetValue $
          LT.insert (tnum tCashOutstanding) tCashOutstanding $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mCollateralAssignment :: FIXMessageSpec
mCollateralAssignment = FMSpec
   { mType = C.pack "AY"
   , mHeader = headerFIX44
   , mBody = mCollateralAssignmentBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralAssignmentBody = 
          LT.insert (tnum tCollAsgnID) tCollAsgnID $
          LT.insert (tnum tCollReqID) tCollReqID $
          LT.insert (tnum tCollAsgnReason) tCollAsgnReason $
          LT.insert (tnum tCollAsgnTransType) tCollAsgnTransType $
          LT.insert (tnum tCollAsgnRefID) tCollAsgnRefID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tExpireTime) tExpireTime $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tMarginExcess) tMarginExcess $
          LT.insert (tnum tTotalNetValue) tTotalNetValue $
          LT.insert (tnum tCashOutstanding) tCashOutstanding $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mCollateralResponse :: FIXMessageSpec
mCollateralResponse = FMSpec
   { mType = C.pack "AZ"
   , mHeader = headerFIX44
   , mBody = mCollateralResponseBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralResponseBody = 
          LT.insert (tnum tCollRespID) tCollRespID $
          LT.insert (tnum tCollAsgnID) tCollAsgnID $
          LT.insert (tnum tCollReqID) tCollReqID $
          LT.insert (tnum tCollAsgnReason) tCollAsgnReason $
          LT.insert (tnum tCollAsgnTransType) tCollAsgnTransType $
          LT.insert (tnum tCollAsgnRespType) tCollAsgnRespType $
          LT.insert (tnum tCollAsgnRejectReason) tCollAsgnRejectReason $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tMarginExcess) tMarginExcess $
          LT.insert (tnum tTotalNetValue) tTotalNetValue $
          LT.insert (tnum tCashOutstanding) tCashOutstanding $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mCollateralReport :: FIXMessageSpec
mCollateralReport = FMSpec
   { mType = C.pack "BA"
   , mHeader = headerFIX44
   , mBody = mCollateralReportBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralReportBody = 
          LT.insert (tnum tCollRptID) tCollRptID $
          LT.insert (tnum tCollInquiryID) tCollInquiryID $
          LT.insert (tnum tCollStatus) tCollStatus $
          LT.insert (tnum tTotNumReports) tTotNumReports $
          LT.insert (tnum tLastRptRequested) tLastRptRequested $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tMarginExcess) tMarginExcess $
          LT.insert (tnum tTotalNetValue) tTotalNetValue $
          LT.insert (tnum tCashOutstanding) tCashOutstanding $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mCollateralInquiry :: FIXMessageSpec
mCollateralInquiry = FMSpec
   { mType = C.pack "BB"
   , mHeader = headerFIX44
   , mBody = mCollateralInquiryBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralInquiryBody = 
          LT.insert (tnum tCollInquiryID) tCollInquiryID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tMarginExcess) tMarginExcess $
          LT.insert (tnum tTotalNetValue) tTotalNetValue $
          LT.insert (tnum tCashOutstanding) tCashOutstanding $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tPrice) tPrice $
          LT.insert (tnum tPriceType) tPriceType $
          LT.insert (tnum tAccruedInterestAmt) tAccruedInterestAmt $
          LT.insert (tnum tEndAccruedInterestAmt) tEndAccruedInterestAmt $
          LT.insert (tnum tStartCash) tStartCash $
          LT.insert (tnum tEndCash) tEndCash $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mNetworkCounterpartySystemStatusRequest :: FIXMessageSpec
mNetworkCounterpartySystemStatusRequest = FMSpec
   { mType = C.pack "BC"
   , mHeader = headerFIX44
   , mBody = mNetworkCounterpartySystemStatusRequestBody
   , mTrailer = trailerFIX44 }
   where
      mNetworkCounterpartySystemStatusRequestBody = 
          LT.insert (tnum tNetworkRequestType) tNetworkRequestType $
          LT.insert (tnum tNetworkRequestID) tNetworkRequestID $
          LT.new

mNetworkCounterpartySystemStatusResponse :: FIXMessageSpec
mNetworkCounterpartySystemStatusResponse = FMSpec
   { mType = C.pack "BD"
   , mHeader = headerFIX44
   , mBody = mNetworkCounterpartySystemStatusResponseBody
   , mTrailer = trailerFIX44 }
   where
      mNetworkCounterpartySystemStatusResponseBody = 
          LT.insert (tnum tNetworkStatusResponseType) tNetworkStatusResponseType $
          LT.insert (tnum tNetworkRequestID) tNetworkRequestID $
          LT.insert (tnum tNetworkResponseID) tNetworkResponseID $
          LT.insert (tnum tLastNetworkResponseID) tLastNetworkResponseID $
          LT.new

mUserRequest :: FIXMessageSpec
mUserRequest = FMSpec
   { mType = C.pack "BE"
   , mHeader = headerFIX44
   , mBody = mUserRequestBody
   , mTrailer = trailerFIX44 }
   where
      mUserRequestBody = 
          LT.insert (tnum tUserRequestID) tUserRequestID $
          LT.insert (tnum tUserRequestType) tUserRequestType $
          LT.insert (tnum tUsername) tUsername $
          LT.insert (tnum tPassword) tPassword $
          LT.insert (tnum tNewPassword) tNewPassword $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.new

mUserResponse :: FIXMessageSpec
mUserResponse = FMSpec
   { mType = C.pack "BF"
   , mHeader = headerFIX44
   , mBody = mUserResponseBody
   , mTrailer = trailerFIX44 }
   where
      mUserResponseBody = 
          LT.insert (tnum tUserRequestID) tUserRequestID $
          LT.insert (tnum tUsername) tUsername $
          LT.insert (tnum tUserStatus) tUserStatus $
          LT.insert (tnum tUserStatusText) tUserStatusText $
          LT.new

mCollateralInquiryAck :: FIXMessageSpec
mCollateralInquiryAck = FMSpec
   { mType = C.pack "BG"
   , mHeader = headerFIX44
   , mBody = mCollateralInquiryAckBody
   , mTrailer = trailerFIX44 }
   where
      mCollateralInquiryAckBody = 
          LT.insert (tnum tCollInquiryID) tCollInquiryID $
          LT.insert (tnum tCollInquiryStatus) tCollInquiryStatus $
          LT.insert (tnum tCollInquiryResult) tCollInquiryResult $
          LT.insert (tnum tTotNumReports) tTotNumReports $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tSecondaryOrderID) tSecondaryOrderID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tSettlDate) tSettlDate $
          LT.insert (tnum tQuantity) tQuantity $
          LT.insert (tnum tQtyType) tQtyType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSettlSessID) tSettlSessID $
          LT.insert (tnum tSettlSessSubID) tSettlSessSubID $
          LT.insert (tnum tClearingBusinessDate) tClearingBusinessDate $
          LT.insert (tnum tResponseTransportType) tResponseTransportType $
          LT.insert (tnum tResponseDestination) tResponseDestination $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

mConfirmationRequest :: FIXMessageSpec
mConfirmationRequest = FMSpec
   { mType = C.pack "BH"
   , mHeader = headerFIX44
   , mBody = mConfirmationRequestBody
   , mTrailer = trailerFIX44 }
   where
      mConfirmationRequestBody = 
          LT.insert (tnum tConfirmReqID) tConfirmReqID $
          LT.insert (tnum tConfirmType) tConfirmType $
          LT.insert (tnum tAllocID) tAllocID $
          LT.insert (tnum tSecondaryAllocID) tSecondaryAllocID $
          LT.insert (tnum tIndividualAllocID) tIndividualAllocID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAllocAccount) tAllocAccount $
          LT.insert (tnum tAllocAcctIDSource) tAllocAcctIDSource $
          LT.insert (tnum tAllocAccountType) tAllocAccountType $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.new

fix44 :: FIXSpec
fix44 = FSpec
   { fHeader = headerFIX44
   , fTrailer = trailerFIX44
   , fMessages = fix44Messages }
   where
      fix44Messages =
          LT.insert (mType mHeartbeat) mHeartbeat $
          LT.insert (mType mTestRequest) mTestRequest $
          LT.insert (mType mResendRequest) mResendRequest $
          LT.insert (mType mReject) mReject $
          LT.insert (mType mSequenceReset) mSequenceReset $
          LT.insert (mType mLogout) mLogout $
          LT.insert (mType mIOI) mIOI $
          LT.insert (mType mAdvertisement) mAdvertisement $
          LT.insert (mType mExecutionReport) mExecutionReport $
          LT.insert (mType mOrderCancelReject) mOrderCancelReject $
          LT.insert (mType mLogon) mLogon $
          LT.insert (mType mNews) mNews $
          LT.insert (mType mEmail) mEmail $
          LT.insert (mType mNewOrderSingle) mNewOrderSingle $
          LT.insert (mType mNewOrderList) mNewOrderList $
          LT.insert (mType mOrderCancelRequest) mOrderCancelRequest $
          LT.insert (mType mOrderCancelReplaceRequest) mOrderCancelReplaceRequest $
          LT.insert (mType mOrderStatusRequest) mOrderStatusRequest $
          LT.insert (mType mAllocationInstruction) mAllocationInstruction $
          LT.insert (mType mListCancelRequest) mListCancelRequest $
          LT.insert (mType mListExecute) mListExecute $
          LT.insert (mType mListStatusRequest) mListStatusRequest $
          LT.insert (mType mListStatus) mListStatus $
          LT.insert (mType mAllocationInstructionAck) mAllocationInstructionAck $
          LT.insert (mType mDontKnowTrade) mDontKnowTrade $
          LT.insert (mType mQuoteRequest) mQuoteRequest $
          LT.insert (mType mQuote) mQuote $
          LT.insert (mType mSettlementInstructions) mSettlementInstructions $
          LT.insert (mType mMarketDataRequest) mMarketDataRequest $
          LT.insert (mType mMarketDataSnapshotFullRefresh) mMarketDataSnapshotFullRefresh $
          LT.insert (mType mMarketDataIncrementalRefresh) mMarketDataIncrementalRefresh $
          LT.insert (mType mMarketDataRequestReject) mMarketDataRequestReject $
          LT.insert (mType mQuoteCancel) mQuoteCancel $
          LT.insert (mType mQuoteStatusRequest) mQuoteStatusRequest $
          LT.insert (mType mMassQuoteAcknowledgement) mMassQuoteAcknowledgement $
          LT.insert (mType mSecurityDefinitionRequest) mSecurityDefinitionRequest $
          LT.insert (mType mSecurityDefinition) mSecurityDefinition $
          LT.insert (mType mSecurityStatusRequest) mSecurityStatusRequest $
          LT.insert (mType mSecurityStatus) mSecurityStatus $
          LT.insert (mType mTradingSessionStatusRequest) mTradingSessionStatusRequest $
          LT.insert (mType mTradingSessionStatus) mTradingSessionStatus $
          LT.insert (mType mMassQuote) mMassQuote $
          LT.insert (mType mBusinessMessageReject) mBusinessMessageReject $
          LT.insert (mType mBidRequest) mBidRequest $
          LT.insert (mType mBidResponse) mBidResponse $
          LT.insert (mType mListStrikePrice) mListStrikePrice $
          LT.insert (mType mRegistrationInstructions) mRegistrationInstructions $
          LT.insert (mType mRegistrationInstructionsResponse) mRegistrationInstructionsResponse $
          LT.insert (mType mOrderMassCancelRequest) mOrderMassCancelRequest $
          LT.insert (mType mOrderMassCancelReport) mOrderMassCancelReport $
          LT.insert (mType mNewOrderCross) mNewOrderCross $
          LT.insert (mType mCrossOrderCancelReplaceRequest) mCrossOrderCancelReplaceRequest $
          LT.insert (mType mCrossOrderCancelRequest) mCrossOrderCancelRequest $
          LT.insert (mType mSecurityTypeRequest) mSecurityTypeRequest $
          LT.insert (mType mSecurityTypes) mSecurityTypes $
          LT.insert (mType mSecurityListRequest) mSecurityListRequest $
          LT.insert (mType mSecurityList) mSecurityList $
          LT.insert (mType mDerivativeSecurityListRequest) mDerivativeSecurityListRequest $
          LT.insert (mType mDerivativeSecurityList) mDerivativeSecurityList $
          LT.insert (mType mNewOrderMultileg) mNewOrderMultileg $
          LT.insert (mType mMultilegOrderCancelReplace) mMultilegOrderCancelReplace $
          LT.insert (mType mTradeCaptureReportRequest) mTradeCaptureReportRequest $
          LT.insert (mType mTradeCaptureReport) mTradeCaptureReport $
          LT.insert (mType mOrderMassStatusRequest) mOrderMassStatusRequest $
          LT.insert (mType mQuoteRequestReject) mQuoteRequestReject $
          LT.insert (mType mRFQRequest) mRFQRequest $
          LT.insert (mType mQuoteStatusReport) mQuoteStatusReport $
          LT.insert (mType mQuoteResponse) mQuoteResponse $
          LT.insert (mType mConfirmation) mConfirmation $
          LT.insert (mType mPositionMaintenanceRequest) mPositionMaintenanceRequest $
          LT.insert (mType mPositionMaintenanceReport) mPositionMaintenanceReport $
          LT.insert (mType mRequestForPositions) mRequestForPositions $
          LT.insert (mType mRequestForPositionsAck) mRequestForPositionsAck $
          LT.insert (mType mPositionReport) mPositionReport $
          LT.insert (mType mTradeCaptureReportRequestAck) mTradeCaptureReportRequestAck $
          LT.insert (mType mTradeCaptureReportAck) mTradeCaptureReportAck $
          LT.insert (mType mAllocationReport) mAllocationReport $
          LT.insert (mType mAllocationReportAck) mAllocationReportAck $
          LT.insert (mType mConfirmationAck) mConfirmationAck $
          LT.insert (mType mSettlementInstructionRequest) mSettlementInstructionRequest $
          LT.insert (mType mAssignmentReport) mAssignmentReport $
          LT.insert (mType mCollateralRequest) mCollateralRequest $
          LT.insert (mType mCollateralAssignment) mCollateralAssignment $
          LT.insert (mType mCollateralResponse) mCollateralResponse $
          LT.insert (mType mCollateralReport) mCollateralReport $
          LT.insert (mType mCollateralInquiry) mCollateralInquiry $
          LT.insert (mType mNetworkCounterpartySystemStatusRequest) mNetworkCounterpartySystemStatusRequest $
          LT.insert (mType mNetworkCounterpartySystemStatusResponse) mNetworkCounterpartySystemStatusResponse $
          LT.insert (mType mUserRequest) mUserRequest $
          LT.insert (mType mUserResponse) mUserResponse $
          LT.insert (mType mCollateralInquiryAck) mCollateralInquiryAck $
          LT.insert (mType mConfirmationRequest) mConfirmationRequest $
          LT.new 
