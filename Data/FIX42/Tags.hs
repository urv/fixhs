
tAccount = FIXTag { tnum = 1, tparser = toFIXString }
tAdvId = FIXTag { tnum = 2, tparser = toFIXString }
tAdvRefID = FIXTag { tnum = 3, tparser = toFIXString }
tAdvSide = FIXTag { tnum = 4, tparser = toFIXString }
tAdvTransType = FIXTag { tnum = 5, tparser = toFIXString }
tAvgPx = FIXTag { tnum = 6, tparser = toFIXPrice }
tBeginSeqNo = FIXTag { tnum = 7, tparser = toFIXString }
tBeginString = FIXTag { tnum = 8, tparser = toFIXString }
tBodyLength = FIXTag { tnum = 9, tparser = toFIXString }
tCheckSum = FIXTag { tnum = 10, tparser = toFIXString }
tClOrdID = FIXTag { tnum = 11, tparser = toFIXString }
tCommission = FIXTag { tnum = 12, tparser = toFIXAmt }
tCommType = FIXTag { tnum = 13, tparser = toFIXString }
tCumQty = FIXTag { tnum = 14, tparser = toFIXString }
tCurrency = FIXTag { tnum = 15, tparser = toFIXCurrency }
tEndSeqNo = FIXTag { tnum = 16, tparser = toFIXString }
tExecID = FIXTag { tnum = 17, tparser = toFIXString }
tExecInst = FIXTag { tnum = 18, tparser = toFIXMultipleValueString }
tExecRefID = FIXTag { tnum = 19, tparser = toFIXString }
tExecTransType = FIXTag { tnum = 20, tparser = toFIXString }
tHandlInst = FIXTag { tnum = 21, tparser = toFIXString }
tSecurityIDSource = FIXTag { tnum = 22, tparser = toFIXString }
tIOIID = FIXTag { tnum = 23, tparser = toFIXString }
tIOIOthSvc = FIXTag { tnum = 24, tparser = toFIXString }
tIOIQltyInd = FIXTag { tnum = 25, tparser = toFIXString }
tIOIRefID = FIXTag { tnum = 26, tparser = toFIXString }
tIOIQty = FIXTag { tnum = 27, tparser = toFIXString }
tIOITransType = FIXTag { tnum = 28, tparser = toFIXString }
tLastCapacity = FIXTag { tnum = 29, tparser = toFIXString }
tLastMkt = FIXTag { tnum = 30, tparser = toFIXExchange }
tLastPx = FIXTag { tnum = 31, tparser = toFIXPrice }
tLastQty = FIXTag { tnum = 32, tparser = toFIXString }
tNoLinesOfText = FIXTag { tnum = 33, tparser = toFIXString }
tMsgSeqNum = FIXTag { tnum = 34, tparser = toFIXString }
tMsgType = FIXTag { tnum = 35, tparser = toFIXString }
tNewSeqNo = FIXTag { tnum = 36, tparser = toFIXString }
tOrderID = FIXTag { tnum = 37, tparser = toFIXString }
tOrderQty = FIXTag { tnum = 38, tparser = toFIXString }
tOrdStatus = FIXTag { tnum = 39, tparser = toFIXString }
tOrdType = FIXTag { tnum = 40, tparser = toFIXString }
tOrigClOrdID = FIXTag { tnum = 41, tparser = toFIXString }
tOrigTime = FIXTag { tnum = 42, tparser = toFIXUTCTimestamp }
tPossDupFlag = FIXTag { tnum = 43, tparser = toFIXString }
tPrice = FIXTag { tnum = 44, tparser = toFIXPrice }
tRefSeqNum = FIXTag { tnum = 45, tparser = toFIXString }
tRelatdSym = FIXTag { tnum = 46, tparser = toFIXString }
tRule80A = FIXTag { tnum = 47, tparser = toFIXString }
tSecurityID = FIXTag { tnum = 48, tparser = toFIXString }
tSenderCompID = FIXTag { tnum = 49, tparser = toFIXString }
tSenderSubID = FIXTag { tnum = 50, tparser = toFIXString }
tSendingDate = FIXTag { tnum = 51, tparser = toFIXLocalMktDate }
tSendingTime = FIXTag { tnum = 52, tparser = toFIXUTCTimestamp }
tQuantity = FIXTag { tnum = 53, tparser = toFIXString }
tSide = FIXTag { tnum = 54, tparser = toFIXString }
tSymbol = FIXTag { tnum = 55, tparser = toFIXString }
tTargetCompID = FIXTag { tnum = 56, tparser = toFIXString }
tTargetSubID = FIXTag { tnum = 57, tparser = toFIXString }
tText = FIXTag { tnum = 58, tparser = toFIXString }
tTimeInForce = FIXTag { tnum = 59, tparser = toFIXString }
tTransactTime = FIXTag { tnum = 60, tparser = toFIXUTCTimestamp }
tUrgency = FIXTag { tnum = 61, tparser = toFIXString }
tValidUntilTime = FIXTag { tnum = 62, tparser = toFIXUTCTimestamp }
tSettlType = FIXTag { tnum = 63, tparser = toFIXString }
tSettlDate = FIXTag { tnum = 64, tparser = toFIXLocalMktDate }
tSymbolSfx = FIXTag { tnum = 65, tparser = toFIXString }
tListID = FIXTag { tnum = 66, tparser = toFIXString }
tListSeqNo = FIXTag { tnum = 67, tparser = toFIXInt }
tTotNoOrders = FIXTag { tnum = 68, tparser = toFIXInt }
tListExecInst = FIXTag { tnum = 69, tparser = toFIXString }
tAllocID = FIXTag { tnum = 70, tparser = toFIXString }
tAllocTransType = FIXTag { tnum = 71, tparser = toFIXString }
tRefAllocID = FIXTag { tnum = 72, tparser = toFIXString }
tNoOrders = FIXTag { tnum = 73, tparser = toFIXString }
tAvgPxPrecision = FIXTag { tnum = 74, tparser = toFIXInt }
tTradeDate = FIXTag { tnum = 75, tparser = toFIXLocalMktDate }
tExecBroker = FIXTag { tnum = 76, tparser = toFIXString }
tPositionEffect = FIXTag { tnum = 77, tparser = toFIXString }
tNoAllocs = FIXTag { tnum = 78, tparser = toFIXString }
tAllocAccount = FIXTag { tnum = 79, tparser = toFIXString }
tAllocQty = FIXTag { tnum = 80, tparser = toFIXString }
tProcessCode = FIXTag { tnum = 81, tparser = toFIXString }
tNoRpts = FIXTag { tnum = 82, tparser = toFIXInt }
tRptSeq = FIXTag { tnum = 83, tparser = toFIXInt }
tCxlQty = FIXTag { tnum = 84, tparser = toFIXString }
tNoDlvyInst = FIXTag { tnum = 85, tparser = toFIXString }
tDlvyInst = FIXTag { tnum = 86, tparser = toFIXString }
tAllocStatus = FIXTag { tnum = 87, tparser = toFIXInt }
tAllocRejCode = FIXTag { tnum = 88, tparser = toFIXInt }
tSignature = FIXTag { tnum = 89, tparser = toFIXString }
tSecureDataLen = FIXTag { tnum = 90, tparser = toFIXString }
tSecureData = FIXTag { tnum = 91, tparser = toFIXString }
tBrokerOfCredit = FIXTag { tnum = 92, tparser = toFIXString }
tSignatureLength = FIXTag { tnum = 93, tparser = toFIXString }
tEmailType = FIXTag { tnum = 94, tparser = toFIXString }
tRawDataLength = FIXTag { tnum = 95, tparser = toFIXString }
tRawData = FIXTag { tnum = 96, tparser = toFIXString }
tPossResend = FIXTag { tnum = 97, tparser = toFIXString }
tEncryptMethod = FIXTag { tnum = 98, tparser = toFIXInt }
tStopPx = FIXTag { tnum = 99, tparser = toFIXPrice }
tExDestination = FIXTag { tnum = 100, tparser = toFIXExchange }
tCxlRejReason = FIXTag { tnum = 102, tparser = toFIXInt }
tOrdRejReason = FIXTag { tnum = 103, tparser = toFIXInt }
tIOIQualifier = FIXTag { tnum = 104, tparser = toFIXString }
tWaveNo = FIXTag { tnum = 105, tparser = toFIXString }
tIssuer = FIXTag { tnum = 106, tparser = toFIXString }
tSecurityDesc = FIXTag { tnum = 107, tparser = toFIXString }
tHeartBtInt = FIXTag { tnum = 108, tparser = toFIXInt }
tClientID = FIXTag { tnum = 109, tparser = toFIXString }
tMinQty = FIXTag { tnum = 110, tparser = toFIXString }
tMaxFloor = FIXTag { tnum = 111, tparser = toFIXString }
tTestReqID = FIXTag { tnum = 112, tparser = toFIXString }
tReportToExch = FIXTag { tnum = 113, tparser = toFIXString }
tLocateReqd = FIXTag { tnum = 114, tparser = toFIXString }
tOnBehalfOfCompID = FIXTag { tnum = 115, tparser = toFIXString }
tOnBehalfOfSubID = FIXTag { tnum = 116, tparser = toFIXString }
tQuoteID = FIXTag { tnum = 117, tparser = toFIXString }
tNetMoney = FIXTag { tnum = 118, tparser = toFIXAmt }
tSettlCurrAmt = FIXTag { tnum = 119, tparser = toFIXAmt }
tSettlCurrency = FIXTag { tnum = 120, tparser = toFIXCurrency }
tForexReq = FIXTag { tnum = 121, tparser = toFIXString }
tOrigSendingTime = FIXTag { tnum = 122, tparser = toFIXUTCTimestamp }
tGapFillFlag = FIXTag { tnum = 123, tparser = toFIXString }
tNoExecs = FIXTag { tnum = 124, tparser = toFIXString }
tCxlType = FIXTag { tnum = 125, tparser = toFIXString }
tExpireTime = FIXTag { tnum = 126, tparser = toFIXUTCTimestamp }
tDKReason = FIXTag { tnum = 127, tparser = toFIXString }
tDeliverToCompID = FIXTag { tnum = 128, tparser = toFIXString }
tDeliverToSubID = FIXTag { tnum = 129, tparser = toFIXString }
tIOINaturalFlag = FIXTag { tnum = 130, tparser = toFIXString }
tQuoteReqID = FIXTag { tnum = 131, tparser = toFIXString }
tBidPx = FIXTag { tnum = 132, tparser = toFIXPrice }
tOfferPx = FIXTag { tnum = 133, tparser = toFIXPrice }
tBidSize = FIXTag { tnum = 134, tparser = toFIXString }
tOfferSize = FIXTag { tnum = 135, tparser = toFIXString }
tNoMiscFees = FIXTag { tnum = 136, tparser = toFIXString }
tMiscFeeAmt = FIXTag { tnum = 137, tparser = toFIXAmt }
tMiscFeeCurr = FIXTag { tnum = 138, tparser = toFIXCurrency }
tMiscFeeType = FIXTag { tnum = 139, tparser = toFIXString }
tPrevClosePx = FIXTag { tnum = 140, tparser = toFIXPrice }
tResetSeqNumFlag = FIXTag { tnum = 141, tparser = toFIXString }
tSenderLocationID = FIXTag { tnum = 142, tparser = toFIXString }
tTargetLocationID = FIXTag { tnum = 143, tparser = toFIXString }
tOnBehalfOfLocationID = FIXTag { tnum = 144, tparser = toFIXString }
tDeliverToLocationID = FIXTag { tnum = 145, tparser = toFIXString }
tNoRelatedSym = FIXTag { tnum = 146, tparser = toFIXString }
tSubject = FIXTag { tnum = 147, tparser = toFIXString }
tHeadline = FIXTag { tnum = 148, tparser = toFIXString }
tURLLink = FIXTag { tnum = 149, tparser = toFIXString }
tExecType = FIXTag { tnum = 150, tparser = toFIXString }
tLeavesQty = FIXTag { tnum = 151, tparser = toFIXString }
tCashOrderQty = FIXTag { tnum = 152, tparser = toFIXString }
tAllocAvgPx = FIXTag { tnum = 153, tparser = toFIXPrice }
tAllocNetMoney = FIXTag { tnum = 154, tparser = toFIXAmt }
tSettlCurrFxRate = FIXTag { tnum = 155, tparser = toFIXFloat }
tSettlCurrFxRateCalc = FIXTag { tnum = 156, tparser = toFIXString }
tNumDaysInterest = FIXTag { tnum = 157, tparser = toFIXInt }
tAccruedInterestRate = FIXTag { tnum = 158, tparser = toFIXString }
tAccruedInterestAmt = FIXTag { tnum = 159, tparser = toFIXAmt }
tSettlInstMode = FIXTag { tnum = 160, tparser = toFIXString }
tAllocText = FIXTag { tnum = 161, tparser = toFIXString }
tSettlInstID = FIXTag { tnum = 162, tparser = toFIXString }
tSettlInstTransType = FIXTag { tnum = 163, tparser = toFIXString }
tEmailThreadID = FIXTag { tnum = 164, tparser = toFIXString }
tSettlInstSource = FIXTag { tnum = 165, tparser = toFIXString }
tSettlLocation = FIXTag { tnum = 166, tparser = toFIXString }
tSecurityType = FIXTag { tnum = 167, tparser = toFIXString }
tEffectiveTime = FIXTag { tnum = 168, tparser = toFIXUTCTimestamp }
tStandInstDbType = FIXTag { tnum = 169, tparser = toFIXInt }
tStandInstDbName = FIXTag { tnum = 170, tparser = toFIXString }
tStandInstDbID = FIXTag { tnum = 171, tparser = toFIXString }
tSettlDeliveryType = FIXTag { tnum = 172, tparser = toFIXInt }
tSettlDepositoryCode = FIXTag { tnum = 173, tparser = toFIXString }
tSettlBrkrCode = FIXTag { tnum = 174, tparser = toFIXString }
tSettlInstCode = FIXTag { tnum = 175, tparser = toFIXString }
tSecuritySettlAgentName = FIXTag { tnum = 176, tparser = toFIXString }
tSecuritySettlAgentCode = FIXTag { tnum = 177, tparser = toFIXString }
tSecuritySettlAgentAcctNum = FIXTag { tnum = 178, tparser = toFIXString }
tSecuritySettlAgentAcctName = FIXTag { tnum = 179, tparser = toFIXString }
tSecuritySettlAgentContactName = FIXTag { tnum = 180, tparser = toFIXString }
tSecuritySettlAgentContactPhone = FIXTag { tnum = 181, tparser = toFIXString }
tCashSettlAgentName = FIXTag { tnum = 182, tparser = toFIXString }
tCashSettlAgentCode = FIXTag { tnum = 183, tparser = toFIXString }
tCashSettlAgentAcctNum = FIXTag { tnum = 184, tparser = toFIXString }
tCashSettlAgentAcctName = FIXTag { tnum = 185, tparser = toFIXString }
tCashSettlAgentContactName = FIXTag { tnum = 186, tparser = toFIXString }
tCashSettlAgentContactPhone = FIXTag { tnum = 187, tparser = toFIXString }
tBidSpotRate = FIXTag { tnum = 188, tparser = toFIXPrice }
tBidForwardPoints = FIXTag { tnum = 189, tparser = toFIXPriceOffset }
tOfferSpotRate = FIXTag { tnum = 190, tparser = toFIXPrice }
tOfferForwardPoints = FIXTag { tnum = 191, tparser = toFIXPriceOffset }
tOrderQty2 = FIXTag { tnum = 192, tparser = toFIXString }
tSettlDate2 = FIXTag { tnum = 193, tparser = toFIXLocalMktDate }
tLastSpotRate = FIXTag { tnum = 194, tparser = toFIXPrice }
tLastForwardPoints = FIXTag { tnum = 195, tparser = toFIXPriceOffset }
tAllocLinkID = FIXTag { tnum = 196, tparser = toFIXString }
tAllocLinkType = FIXTag { tnum = 197, tparser = toFIXInt }
tSecondaryOrderID = FIXTag { tnum = 198, tparser = toFIXString }
tNoIOIQualifiers = FIXTag { tnum = 199, tparser = toFIXString }
tMaturityMonthYear = FIXTag { tnum = 200, tparser = toFIXString }
tPutOrCall = FIXTag { tnum = 201, tparser = toFIXInt }
tStrikePrice = FIXTag { tnum = 202, tparser = toFIXPrice }
tCoveredOrUncovered = FIXTag { tnum = 203, tparser = toFIXInt }
tCustomerOrFirm = FIXTag { tnum = 204, tparser = toFIXInt }
tMaturityDay = FIXTag { tnum = 205, tparser = toFIXDayOfMonth }
tOptAttribute = FIXTag { tnum = 206, tparser = toFIXString }
tSecurityExchange = FIXTag { tnum = 207, tparser = toFIXExchange }
tNotifyBrokerOfCredit = FIXTag { tnum = 208, tparser = toFIXString }
tAllocHandlInst = FIXTag { tnum = 209, tparser = toFIXInt }
tMaxShow = FIXTag { tnum = 210, tparser = toFIXString }
tPegOffsetValue = FIXTag { tnum = 211, tparser = toFIXFloat }
tXmlDataLen = FIXTag { tnum = 212, tparser = toFIXString }
tXmlData = FIXTag { tnum = 213, tparser = toFIXString }
tSettlInstRefID = FIXTag { tnum = 214, tparser = toFIXString }
tNoRoutingIDs = FIXTag { tnum = 215, tparser = toFIXString }
tRoutingType = FIXTag { tnum = 216, tparser = toFIXInt }
tRoutingID = FIXTag { tnum = 217, tparser = toFIXString }
tSpread = FIXTag { tnum = 218, tparser = toFIXPriceOffset }
tBenchmark = FIXTag { tnum = 219, tparser = toFIXString }
tBenchmarkCurveCurrency = FIXTag { tnum = 220, tparser = toFIXCurrency }
tBenchmarkCurveName = FIXTag { tnum = 221, tparser = toFIXString }
tBenchmarkCurvePoint = FIXTag { tnum = 222, tparser = toFIXString }
tCouponRate = FIXTag { tnum = 223, tparser = toFIXString }
tCouponPaymentDate = FIXTag { tnum = 224, tparser = toFIXLocalMktDate }
tIssueDate = FIXTag { tnum = 225, tparser = toFIXLocalMktDate }
tRepurchaseTerm = FIXTag { tnum = 226, tparser = toFIXInt }
tRepurchaseRate = FIXTag { tnum = 227, tparser = toFIXString }
tFactor = FIXTag { tnum = 228, tparser = toFIXFloat }
tTradeOriginationDate = FIXTag { tnum = 229, tparser = toFIXLocalMktDate }
tExDate = FIXTag { tnum = 230, tparser = toFIXLocalMktDate }
tContractMultiplier = FIXTag { tnum = 231, tparser = toFIXFloat }
tNoStipulations = FIXTag { tnum = 232, tparser = toFIXString }
tStipulationType = FIXTag { tnum = 233, tparser = toFIXString }
tStipulationValue = FIXTag { tnum = 234, tparser = toFIXString }
tYieldType = FIXTag { tnum = 235, tparser = toFIXString }
tYield = FIXTag { tnum = 236, tparser = toFIXString }
tTotalTakedown = FIXTag { tnum = 237, tparser = toFIXAmt }
tConcession = FIXTag { tnum = 238, tparser = toFIXAmt }
tRepoCollateralSecurityType = FIXTag { tnum = 239, tparser = toFIXInt }
tRedemptionDate = FIXTag { tnum = 240, tparser = toFIXLocalMktDate }
tUnderlyingCouponPaymentDate = FIXTag { tnum = 241, tparser = toFIXLocalMktDate }
tUnderlyingIssueDate = FIXTag { tnum = 242, tparser = toFIXLocalMktDate }
tUnderlyingRepoCollateralSecurityType = FIXTag { tnum = 243, tparser = toFIXInt }
tUnderlyingRepurchaseTerm = FIXTag { tnum = 244, tparser = toFIXInt }
tUnderlyingRepurchaseRate = FIXTag { tnum = 245, tparser = toFIXString }
tUnderlyingFactor = FIXTag { tnum = 246, tparser = toFIXFloat }
tUnderlyingRedemptionDate = FIXTag { tnum = 247, tparser = toFIXLocalMktDate }
tLegCouponPaymentDate = FIXTag { tnum = 248, tparser = toFIXLocalMktDate }
tLegIssueDate = FIXTag { tnum = 249, tparser = toFIXLocalMktDate }
tLegRepoCollateralSecurityType = FIXTag { tnum = 250, tparser = toFIXInt }
tLegRepurchaseTerm = FIXTag { tnum = 251, tparser = toFIXInt }
tLegRepurchaseRate = FIXTag { tnum = 252, tparser = toFIXString }
tLegFactor = FIXTag { tnum = 253, tparser = toFIXFloat }
tLegRedemptionDate = FIXTag { tnum = 254, tparser = toFIXLocalMktDate }
tCreditRating = FIXTag { tnum = 255, tparser = toFIXString }
tUnderlyingCreditRating = FIXTag { tnum = 256, tparser = toFIXString }
tLegCreditRating = FIXTag { tnum = 257, tparser = toFIXString }
tTradedFlatSwitch = FIXTag { tnum = 258, tparser = toFIXString }
tBasisFeatureDate = FIXTag { tnum = 259, tparser = toFIXLocalMktDate }
tBasisFeaturePrice = FIXTag { tnum = 260, tparser = toFIXPrice }
tMDReqID = FIXTag { tnum = 262, tparser = toFIXString }
tSubscriptionRequestType = FIXTag { tnum = 263, tparser = toFIXString }
tMarketDepth = FIXTag { tnum = 264, tparser = toFIXInt }
tMDUpdateType = FIXTag { tnum = 265, tparser = toFIXInt }
tAggregatedBook = FIXTag { tnum = 266, tparser = toFIXString }
tNoMDEntryTypes = FIXTag { tnum = 267, tparser = toFIXString }
tNoMDEntries = FIXTag { tnum = 268, tparser = toFIXString }
tMDEntryType = FIXTag { tnum = 269, tparser = toFIXString }
tMDEntryPx = FIXTag { tnum = 270, tparser = toFIXPrice }
tMDEntrySize = FIXTag { tnum = 271, tparser = toFIXString }
tMDEntryDate = FIXTag { tnum = 272, tparser = toFIXString }
tMDEntryTime = FIXTag { tnum = 273, tparser = toFIXUTCTimeOnly }
tTickDirection = FIXTag { tnum = 274, tparser = toFIXString }
tMDMkt = FIXTag { tnum = 275, tparser = toFIXExchange }
tQuoteCondition = FIXTag { tnum = 276, tparser = toFIXMultipleValueString }
tTradeCondition = FIXTag { tnum = 277, tparser = toFIXMultipleValueString }
tMDEntryID = FIXTag { tnum = 278, tparser = toFIXString }
tMDUpdateAction = FIXTag { tnum = 279, tparser = toFIXString }
tMDEntryRefID = FIXTag { tnum = 280, tparser = toFIXString }
tMDReqRejReason = FIXTag { tnum = 281, tparser = toFIXString }
tMDEntryOriginator = FIXTag { tnum = 282, tparser = toFIXString }
tLocationID = FIXTag { tnum = 283, tparser = toFIXString }
tDeskID = FIXTag { tnum = 284, tparser = toFIXString }
tDeleteReason = FIXTag { tnum = 285, tparser = toFIXString }
tOpenCloseSettlFlag = FIXTag { tnum = 286, tparser = toFIXMultipleValueString }
tSellerDays = FIXTag { tnum = 287, tparser = toFIXInt }
tMDEntryBuyer = FIXTag { tnum = 288, tparser = toFIXString }
tMDEntrySeller = FIXTag { tnum = 289, tparser = toFIXString }
tMDEntryPositionNo = FIXTag { tnum = 290, tparser = toFIXInt }
tFinancialStatus = FIXTag { tnum = 291, tparser = toFIXMultipleValueString }
tCorporateAction = FIXTag { tnum = 292, tparser = toFIXMultipleValueString }
tDefBidSize = FIXTag { tnum = 293, tparser = toFIXString }
tDefOfferSize = FIXTag { tnum = 294, tparser = toFIXString }
tNoQuoteEntries = FIXTag { tnum = 295, tparser = toFIXString }
tNoQuoteSets = FIXTag { tnum = 296, tparser = toFIXString }
tQuoteStatus = FIXTag { tnum = 297, tparser = toFIXInt }
tQuoteCancelType = FIXTag { tnum = 298, tparser = toFIXInt }
tQuoteEntryID = FIXTag { tnum = 299, tparser = toFIXString }
tQuoteRejectReason = FIXTag { tnum = 300, tparser = toFIXInt }
tQuoteResponseLevel = FIXTag { tnum = 301, tparser = toFIXInt }
tQuoteSetID = FIXTag { tnum = 302, tparser = toFIXString }
tQuoteRequestType = FIXTag { tnum = 303, tparser = toFIXInt }
tTotNoQuoteEntries = FIXTag { tnum = 304, tparser = toFIXInt }
tUnderlyingSecurityIDSource = FIXTag { tnum = 305, tparser = toFIXString }
tUnderlyingIssuer = FIXTag { tnum = 306, tparser = toFIXString }
tUnderlyingSecurityDesc = FIXTag { tnum = 307, tparser = toFIXString }
tUnderlyingSecurityExchange = FIXTag { tnum = 308, tparser = toFIXExchange }
tUnderlyingSecurityID = FIXTag { tnum = 309, tparser = toFIXString }
tUnderlyingSecurityType = FIXTag { tnum = 310, tparser = toFIXString }
tUnderlyingSymbol = FIXTag { tnum = 311, tparser = toFIXString }
tUnderlyingSymbolSfx = FIXTag { tnum = 312, tparser = toFIXString }
tUnderlyingMaturityMonthYear = FIXTag { tnum = 313, tparser = toFIXString }
tUnderlyingMaturityDay = FIXTag { tnum = 314, tparser = toFIXDayOfMonth }
tUnderlyingPutOrCall = FIXTag { tnum = 315, tparser = toFIXInt }
tUnderlyingStrikePrice = FIXTag { tnum = 316, tparser = toFIXPrice }
tUnderlyingOptAttribute = FIXTag { tnum = 317, tparser = toFIXString }
tUnderlyingCurrency = FIXTag { tnum = 318, tparser = toFIXCurrency }
tRatioQty = FIXTag { tnum = 319, tparser = toFIXString }
tSecurityReqID = FIXTag { tnum = 320, tparser = toFIXString }
tSecurityRequestType = FIXTag { tnum = 321, tparser = toFIXInt }
tSecurityResponseID = FIXTag { tnum = 322, tparser = toFIXString }
tSecurityResponseType = FIXTag { tnum = 323, tparser = toFIXInt }
tSecurityStatusReqID = FIXTag { tnum = 324, tparser = toFIXString }
tUnsolicitedIndicator = FIXTag { tnum = 325, tparser = toFIXString }
tSecurityTradingStatus = FIXTag { tnum = 326, tparser = toFIXInt }
tHaltReasonChar = FIXTag { tnum = 327, tparser = toFIXString }
tInViewOfCommon = FIXTag { tnum = 328, tparser = toFIXString }
tDueToRelated = FIXTag { tnum = 329, tparser = toFIXString }
tBuyVolume = FIXTag { tnum = 330, tparser = toFIXString }
tSellVolume = FIXTag { tnum = 331, tparser = toFIXString }
tHighPx = FIXTag { tnum = 332, tparser = toFIXPrice }
tLowPx = FIXTag { tnum = 333, tparser = toFIXPrice }
tAdjustment = FIXTag { tnum = 334, tparser = toFIXInt }
tTradSesReqID = FIXTag { tnum = 335, tparser = toFIXString }
tTradingSessionID = FIXTag { tnum = 336, tparser = toFIXString }
tContraTrader = FIXTag { tnum = 337, tparser = toFIXString }
tTradSesMethod = FIXTag { tnum = 338, tparser = toFIXInt }
tTradSesMode = FIXTag { tnum = 339, tparser = toFIXInt }
tTradSesStatus = FIXTag { tnum = 340, tparser = toFIXInt }
tTradSesStartTime = FIXTag { tnum = 341, tparser = toFIXUTCTimestamp }
tTradSesOpenTime = FIXTag { tnum = 342, tparser = toFIXUTCTimestamp }
tTradSesPreCloseTime = FIXTag { tnum = 343, tparser = toFIXUTCTimestamp }
tTradSesCloseTime = FIXTag { tnum = 344, tparser = toFIXUTCTimestamp }
tTradSesEndTime = FIXTag { tnum = 345, tparser = toFIXUTCTimestamp }
tNumberOfOrders = FIXTag { tnum = 346, tparser = toFIXInt }
tMessageEncoding = FIXTag { tnum = 347, tparser = toFIXString }
tEncodedIssuerLen = FIXTag { tnum = 348, tparser = toFIXString }
tEncodedIssuer = FIXTag { tnum = 349, tparser = toFIXString }
tEncodedSecurityDescLen = FIXTag { tnum = 350, tparser = toFIXString }
tEncodedSecurityDesc = FIXTag { tnum = 351, tparser = toFIXString }
tEncodedListExecInstLen = FIXTag { tnum = 352, tparser = toFIXString }
tEncodedListExecInst = FIXTag { tnum = 353, tparser = toFIXString }
tEncodedTextLen = FIXTag { tnum = 354, tparser = toFIXString }
tEncodedText = FIXTag { tnum = 355, tparser = toFIXString }
tEncodedSubjectLen = FIXTag { tnum = 356, tparser = toFIXString }
tEncodedSubject = FIXTag { tnum = 357, tparser = toFIXString }
tEncodedHeadlineLen = FIXTag { tnum = 358, tparser = toFIXString }
tEncodedHeadline = FIXTag { tnum = 359, tparser = toFIXString }
tEncodedAllocTextLen = FIXTag { tnum = 360, tparser = toFIXString }
tEncodedAllocText = FIXTag { tnum = 361, tparser = toFIXString }
tEncodedUnderlyingIssuerLen = FIXTag { tnum = 362, tparser = toFIXString }
tEncodedUnderlyingIssuer = FIXTag { tnum = 363, tparser = toFIXString }
tEncodedUnderlyingSecurityDescLen = FIXTag { tnum = 364, tparser = toFIXString }
tEncodedUnderlyingSecurityDesc = FIXTag { tnum = 365, tparser = toFIXString }
tAllocPrice = FIXTag { tnum = 366, tparser = toFIXPrice }
tQuoteSetValidUntilTime = FIXTag { tnum = 367, tparser = toFIXUTCTimestamp }
tQuoteEntryRejectReason = FIXTag { tnum = 368, tparser = toFIXInt }
tLastMsgSeqNumProcessed = FIXTag { tnum = 369, tparser = toFIXString }
tOnBehalfOfSendingTime = FIXTag { tnum = 370, tparser = toFIXUTCTimestamp }
tRefTagID = FIXTag { tnum = 371, tparser = toFIXInt }
tRefMsgType = FIXTag { tnum = 372, tparser = toFIXString }
tSessionRejectReason = FIXTag { tnum = 373, tparser = toFIXInt }
tBidRequestTransType = FIXTag { tnum = 374, tparser = toFIXString }
tContraBroker = FIXTag { tnum = 375, tparser = toFIXString }
tComplianceID = FIXTag { tnum = 376, tparser = toFIXString }
tSolicitedFlag = FIXTag { tnum = 377, tparser = toFIXString }
tExecRestatementReason = FIXTag { tnum = 378, tparser = toFIXInt }
tBusinessRejectRefID = FIXTag { tnum = 379, tparser = toFIXString }
tBusinessRejectReason = FIXTag { tnum = 380, tparser = toFIXInt }
tGrossTradeAmt = FIXTag { tnum = 381, tparser = toFIXAmt }
tNoContraBrokers = FIXTag { tnum = 382, tparser = toFIXString }
tMaxMessageSize = FIXTag { tnum = 383, tparser = toFIXString }
tNoMsgTypes = FIXTag { tnum = 384, tparser = toFIXString }
tMsgDirection = FIXTag { tnum = 385, tparser = toFIXString }
tNoTradingSessions = FIXTag { tnum = 386, tparser = toFIXString }
tTotalVolumeTraded = FIXTag { tnum = 387, tparser = toFIXString }
tDiscretionInst = FIXTag { tnum = 388, tparser = toFIXString }
tDiscretionOffsetValue = FIXTag { tnum = 389, tparser = toFIXFloat }
tBidID = FIXTag { tnum = 390, tparser = toFIXString }
tClientBidID = FIXTag { tnum = 391, tparser = toFIXString }
tListName = FIXTag { tnum = 392, tparser = toFIXString }
tTotNoRelatedSym = FIXTag { tnum = 393, tparser = toFIXInt }
tBidType = FIXTag { tnum = 394, tparser = toFIXInt }
tNumTickets = FIXTag { tnum = 395, tparser = toFIXInt }
tSideValue1 = FIXTag { tnum = 396, tparser = toFIXAmt }
tSideValue2 = FIXTag { tnum = 397, tparser = toFIXAmt }
tNoBidDescriptors = FIXTag { tnum = 398, tparser = toFIXString }
tBidDescriptorType = FIXTag { tnum = 399, tparser = toFIXInt }
tBidDescriptor = FIXTag { tnum = 400, tparser = toFIXString }
tSideValueInd = FIXTag { tnum = 401, tparser = toFIXInt }
tLiquidityPctLow = FIXTag { tnum = 402, tparser = toFIXString }
tLiquidityPctHigh = FIXTag { tnum = 403, tparser = toFIXString }
tLiquidityValue = FIXTag { tnum = 404, tparser = toFIXAmt }
tEFPTrackingError = FIXTag { tnum = 405, tparser = toFIXString }
tFairValue = FIXTag { tnum = 406, tparser = toFIXAmt }
tOutsideIndexPct = FIXTag { tnum = 407, tparser = toFIXString }
tValueOfFutures = FIXTag { tnum = 408, tparser = toFIXAmt }
tLiquidityIndType = FIXTag { tnum = 409, tparser = toFIXInt }
tWtAverageLiquidity = FIXTag { tnum = 410, tparser = toFIXString }
tExchangeForPhysical = FIXTag { tnum = 411, tparser = toFIXString }
tOutMainCntryUIndex = FIXTag { tnum = 412, tparser = toFIXAmt }
tCrossPercent = FIXTag { tnum = 413, tparser = toFIXString }
tProgRptReqs = FIXTag { tnum = 414, tparser = toFIXInt }
tProgPeriodInterval = FIXTag { tnum = 415, tparser = toFIXInt }
tIncTaxInd = FIXTag { tnum = 416, tparser = toFIXInt }
tNumBidders = FIXTag { tnum = 417, tparser = toFIXInt }
tBidTradeType = FIXTag { tnum = 418, tparser = toFIXString }
tBasisPxType = FIXTag { tnum = 419, tparser = toFIXString }
tNoBidComponents = FIXTag { tnum = 420, tparser = toFIXString }
tCountry = FIXTag { tnum = 421, tparser = toFIXString }
tTotNoStrikes = FIXTag { tnum = 422, tparser = toFIXInt }
tPriceType = FIXTag { tnum = 423, tparser = toFIXInt }
tDayOrderQty = FIXTag { tnum = 424, tparser = toFIXString }
tDayCumQty = FIXTag { tnum = 425, tparser = toFIXString }
tDayAvgPx = FIXTag { tnum = 426, tparser = toFIXPrice }
tGTBookingInst = FIXTag { tnum = 427, tparser = toFIXInt }
tNoStrikes = FIXTag { tnum = 428, tparser = toFIXString }
tListStatusType = FIXTag { tnum = 429, tparser = toFIXInt }
tNetGrossInd = FIXTag { tnum = 430, tparser = toFIXInt }
tListOrderStatus = FIXTag { tnum = 431, tparser = toFIXInt }
tExpireDate = FIXTag { tnum = 432, tparser = toFIXLocalMktDate }
tListExecInstType = FIXTag { tnum = 433, tparser = toFIXString }
tCxlRejResponseTo = FIXTag { tnum = 434, tparser = toFIXString }
tUnderlyingCouponRate = FIXTag { tnum = 435, tparser = toFIXString }
tUnderlyingContractMultiplier = FIXTag { tnum = 436, tparser = toFIXFloat }
tContraTradeQty = FIXTag { tnum = 437, tparser = toFIXString }
tContraTradeTime = FIXTag { tnum = 438, tparser = toFIXUTCTimestamp }
tClearingFirm = FIXTag { tnum = 439, tparser = toFIXString }
tClearingAccount = FIXTag { tnum = 440, tparser = toFIXString }
tLiquidityNumSecurities = FIXTag { tnum = 441, tparser = toFIXInt }
tMultiLegReportingType = FIXTag { tnum = 442, tparser = toFIXString }
tStrikeTime = FIXTag { tnum = 443, tparser = toFIXUTCTimestamp }
tListStatusText = FIXTag { tnum = 444, tparser = toFIXString }
tEncodedListStatusTextLen = FIXTag { tnum = 445, tparser = toFIXString }
tEncodedListStatusText = FIXTag { tnum = 446, tparser = toFIXString }
tPartyIDSource = FIXTag { tnum = 447, tparser = toFIXString }
tPartyID = FIXTag { tnum = 448, tparser = toFIXString }
tTotalVolumeTradedDate = FIXTag { tnum = 449, tparser = toFIXString }
tTotalVolumeTradedTime = FIXTag { tnum = 450, tparser = toFIXUTCTimeOnly }
tNetChgPrevDay = FIXTag { tnum = 451, tparser = toFIXPriceOffset }
tPartyRole = FIXTag { tnum = 452, tparser = toFIXInt }
tNoPartyIDs = FIXTag { tnum = 453, tparser = toFIXString }
tNoSecurityAltID = FIXTag { tnum = 454, tparser = toFIXString }
tSecurityAltID = FIXTag { tnum = 455, tparser = toFIXString }
tSecurityAltIDSource = FIXTag { tnum = 456, tparser = toFIXString }
tNoUnderlyingSecurityAltID = FIXTag { tnum = 457, tparser = toFIXString }
tUnderlyingSecurityAltID = FIXTag { tnum = 458, tparser = toFIXString }
tUnderlyingSecurityAltIDSource = FIXTag { tnum = 459, tparser = toFIXString }
tProduct = FIXTag { tnum = 460, tparser = toFIXInt }
tCFICode = FIXTag { tnum = 461, tparser = toFIXString }
tUnderlyingProduct = FIXTag { tnum = 462, tparser = toFIXInt }
tUnderlyingCFICode = FIXTag { tnum = 463, tparser = toFIXString }
tTestMessageIndicator = FIXTag { tnum = 464, tparser = toFIXString }
tQuantityType = FIXTag { tnum = 465, tparser = toFIXInt }
tBookingRefID = FIXTag { tnum = 466, tparser = toFIXString }
tIndividualAllocID = FIXTag { tnum = 467, tparser = toFIXString }
tRoundingDirection = FIXTag { tnum = 468, tparser = toFIXString }
tRoundingModulus = FIXTag { tnum = 469, tparser = toFIXFloat }
tCountryOfIssue = FIXTag { tnum = 470, tparser = toFIXString }
tStateOrProvinceOfIssue = FIXTag { tnum = 471, tparser = toFIXString }
tLocaleOfIssue = FIXTag { tnum = 472, tparser = toFIXString }
tNoRegistDtls = FIXTag { tnum = 473, tparser = toFIXString }
tMailingDtls = FIXTag { tnum = 474, tparser = toFIXString }
tInvestorCountryOfResidence = FIXTag { tnum = 475, tparser = toFIXString }
tPaymentRef = FIXTag { tnum = 476, tparser = toFIXString }
tDistribPaymentMethod = FIXTag { tnum = 477, tparser = toFIXInt }
tCashDistribCurr = FIXTag { tnum = 478, tparser = toFIXCurrency }
tCommCurrency = FIXTag { tnum = 479, tparser = toFIXCurrency }
tCancellationRights = FIXTag { tnum = 480, tparser = toFIXString }
tMoneyLaunderingStatus = FIXTag { tnum = 481, tparser = toFIXString }
tMailingInst = FIXTag { tnum = 482, tparser = toFIXString }
tTransBkdTime = FIXTag { tnum = 483, tparser = toFIXUTCTimestamp }
tExecPriceType = FIXTag { tnum = 484, tparser = toFIXString }
tExecPriceAdjustment = FIXTag { tnum = 485, tparser = toFIXFloat }
tDateOfBirth = FIXTag { tnum = 486, tparser = toFIXLocalMktDate }
tTradeReportTransType = FIXTag { tnum = 487, tparser = toFIXInt }
tCardHolderName = FIXTag { tnum = 488, tparser = toFIXString }
tCardNumber = FIXTag { tnum = 489, tparser = toFIXString }
tCardExpDate = FIXTag { tnum = 490, tparser = toFIXLocalMktDate }
tCardIssNum = FIXTag { tnum = 491, tparser = toFIXString }
tPaymentMethod = FIXTag { tnum = 492, tparser = toFIXInt }
tRegistAcctType = FIXTag { tnum = 493, tparser = toFIXString }
tDesignation = FIXTag { tnum = 494, tparser = toFIXString }
tTaxAdvantageType = FIXTag { tnum = 495, tparser = toFIXInt }
tRegistRejReasonText = FIXTag { tnum = 496, tparser = toFIXString }
tFundRenewWaiv = FIXTag { tnum = 497, tparser = toFIXString }
tCashDistribAgentName = FIXTag { tnum = 498, tparser = toFIXString }
tCashDistribAgentCode = FIXTag { tnum = 499, tparser = toFIXString }
tCashDistribAgentAcctNumber = FIXTag { tnum = 500, tparser = toFIXString }
tCashDistribPayRef = FIXTag { tnum = 501, tparser = toFIXString }
tCashDistribAgentAcctName = FIXTag { tnum = 502, tparser = toFIXString }
tCardStartDate = FIXTag { tnum = 503, tparser = toFIXLocalMktDate }
tPaymentDate = FIXTag { tnum = 504, tparser = toFIXLocalMktDate }
tPaymentRemitterID = FIXTag { tnum = 505, tparser = toFIXString }
tRegistStatus = FIXTag { tnum = 506, tparser = toFIXString }
tRegistRejReasonCode = FIXTag { tnum = 507, tparser = toFIXInt }
tRegistRefID = FIXTag { tnum = 508, tparser = toFIXString }
tRegistDtls = FIXTag { tnum = 509, tparser = toFIXString }
tNoDistribInsts = FIXTag { tnum = 510, tparser = toFIXString }
tRegistEmail = FIXTag { tnum = 511, tparser = toFIXString }
tDistribPercentage = FIXTag { tnum = 512, tparser = toFIXString }
tRegistID = FIXTag { tnum = 513, tparser = toFIXString }
tRegistTransType = FIXTag { tnum = 514, tparser = toFIXString }
tExecValuationPoint = FIXTag { tnum = 515, tparser = toFIXUTCTimestamp }
tOrderPercent = FIXTag { tnum = 516, tparser = toFIXString }
tOwnershipType = FIXTag { tnum = 517, tparser = toFIXString }
tNoContAmts = FIXTag { tnum = 518, tparser = toFIXString }
tContAmtType = FIXTag { tnum = 519, tparser = toFIXInt }
tContAmtValue = FIXTag { tnum = 520, tparser = toFIXFloat }
tContAmtCurr = FIXTag { tnum = 521, tparser = toFIXCurrency }
tOwnerType = FIXTag { tnum = 522, tparser = toFIXInt }
tPartySubID = FIXTag { tnum = 523, tparser = toFIXString }
tNestedPartyID = FIXTag { tnum = 524, tparser = toFIXString }
tNestedPartyIDSource = FIXTag { tnum = 525, tparser = toFIXString }
tSecondaryClOrdID = FIXTag { tnum = 526, tparser = toFIXString }
tSecondaryExecID = FIXTag { tnum = 527, tparser = toFIXString }
tOrderCapacity = FIXTag { tnum = 528, tparser = toFIXString }
tOrderRestrictions = FIXTag { tnum = 529, tparser = toFIXMultipleValueString }
tMassCancelRequestType = FIXTag { tnum = 530, tparser = toFIXString }
tMassCancelResponse = FIXTag { tnum = 531, tparser = toFIXString }
tMassCancelRejectReason = FIXTag { tnum = 532, tparser = toFIXString }
tTotalAffectedOrders = FIXTag { tnum = 533, tparser = toFIXInt }
tNoAffectedOrders = FIXTag { tnum = 534, tparser = toFIXInt }
tAffectedOrderID = FIXTag { tnum = 535, tparser = toFIXString }
tAffectedSecondaryOrderID = FIXTag { tnum = 536, tparser = toFIXString }
tQuoteType = FIXTag { tnum = 537, tparser = toFIXInt }
tNestedPartyRole = FIXTag { tnum = 538, tparser = toFIXInt }
tNoNestedPartyIDs = FIXTag { tnum = 539, tparser = toFIXString }
tTotalAccruedInterestAmt = FIXTag { tnum = 540, tparser = toFIXAmt }
tMaturityDate = FIXTag { tnum = 541, tparser = toFIXLocalMktDate }
tUnderlyingMaturityDate = FIXTag { tnum = 542, tparser = toFIXLocalMktDate }
tInstrRegistry = FIXTag { tnum = 543, tparser = toFIXString }
tCashMargin = FIXTag { tnum = 544, tparser = toFIXString }
tNestedPartySubID = FIXTag { tnum = 545, tparser = toFIXString }
tScope = FIXTag { tnum = 546, tparser = toFIXMultipleValueString }
tMDImplicitDelete = FIXTag { tnum = 547, tparser = toFIXString }
tCrossID = FIXTag { tnum = 548, tparser = toFIXString }
tCrossType = FIXTag { tnum = 549, tparser = toFIXInt }
tCrossPrioritization = FIXTag { tnum = 550, tparser = toFIXInt }
tOrigCrossID = FIXTag { tnum = 551, tparser = toFIXString }
tNoSides = FIXTag { tnum = 552, tparser = toFIXString }
tUsername = FIXTag { tnum = 553, tparser = toFIXString }
tPassword = FIXTag { tnum = 554, tparser = toFIXString }
tNoLegs = FIXTag { tnum = 555, tparser = toFIXString }
tLegCurrency = FIXTag { tnum = 556, tparser = toFIXCurrency }
tTotNoSecurityTypes = FIXTag { tnum = 557, tparser = toFIXInt }
tNoSecurityTypes = FIXTag { tnum = 558, tparser = toFIXString }
tSecurityListRequestType = FIXTag { tnum = 559, tparser = toFIXInt }
tSecurityRequestResult = FIXTag { tnum = 560, tparser = toFIXInt }
tRoundLot = FIXTag { tnum = 561, tparser = toFIXString }
tMinTradeVol = FIXTag { tnum = 562, tparser = toFIXString }
tMultiLegRptTypeReq = FIXTag { tnum = 563, tparser = toFIXInt }
tLegPositionEffect = FIXTag { tnum = 564, tparser = toFIXString }
tLegCoveredOrUncovered = FIXTag { tnum = 565, tparser = toFIXInt }
tLegPrice = FIXTag { tnum = 566, tparser = toFIXPrice }
tTradSesStatusRejReason = FIXTag { tnum = 567, tparser = toFIXInt }
tTradeRequestID = FIXTag { tnum = 568, tparser = toFIXString }
tTradeRequestType = FIXTag { tnum = 569, tparser = toFIXInt }
tPreviouslyReported = FIXTag { tnum = 570, tparser = toFIXString }
tTradeReportID = FIXTag { tnum = 571, tparser = toFIXString }
tTradeReportRefID = FIXTag { tnum = 572, tparser = toFIXString }
tMatchStatus = FIXTag { tnum = 573, tparser = toFIXString }
tMatchType = FIXTag { tnum = 574, tparser = toFIXString }
tOddLot = FIXTag { tnum = 575, tparser = toFIXString }
tNoClearingInstructions = FIXTag { tnum = 576, tparser = toFIXString }
tClearingInstruction = FIXTag { tnum = 577, tparser = toFIXInt }
tTradeInputSource = FIXTag { tnum = 578, tparser = toFIXString }
tTradeInputDevice = FIXTag { tnum = 579, tparser = toFIXString }
tNoDates = FIXTag { tnum = 580, tparser = toFIXInt }
tAccountType = FIXTag { tnum = 581, tparser = toFIXInt }
tCustOrderCapacity = FIXTag { tnum = 582, tparser = toFIXInt }
tClOrdLinkID = FIXTag { tnum = 583, tparser = toFIXString }
tMassStatusReqID = FIXTag { tnum = 584, tparser = toFIXString }
tMassStatusReqType = FIXTag { tnum = 585, tparser = toFIXInt }
tOrigOrdModTime = FIXTag { tnum = 586, tparser = toFIXUTCTimestamp }
tLegSettlType = FIXTag { tnum = 587, tparser = toFIXString }
tLegSettlDate = FIXTag { tnum = 588, tparser = toFIXLocalMktDate }
tDayBookingInst = FIXTag { tnum = 589, tparser = toFIXString }
tBookingUnit = FIXTag { tnum = 590, tparser = toFIXString }
tPreallocMethod = FIXTag { tnum = 591, tparser = toFIXString }
tUnderlyingCountryOfIssue = FIXTag { tnum = 592, tparser = toFIXString }
tUnderlyingStateOrProvinceOfIssue = FIXTag { tnum = 593, tparser = toFIXString }
tUnderlyingLocaleOfIssue = FIXTag { tnum = 594, tparser = toFIXString }
tUnderlyingInstrRegistry = FIXTag { tnum = 595, tparser = toFIXString }
tLegCountryOfIssue = FIXTag { tnum = 596, tparser = toFIXString }
tLegStateOrProvinceOfIssue = FIXTag { tnum = 597, tparser = toFIXString }
tLegLocaleOfIssue = FIXTag { tnum = 598, tparser = toFIXString }
tLegInstrRegistry = FIXTag { tnum = 599, tparser = toFIXString }
tLegSymbol = FIXTag { tnum = 600, tparser = toFIXString }
tLegSymbolSfx = FIXTag { tnum = 601, tparser = toFIXString }
tLegSecurityID = FIXTag { tnum = 602, tparser = toFIXString }
tLegSecurityIDSource = FIXTag { tnum = 603, tparser = toFIXString }
tNoLegSecurityAltID = FIXTag { tnum = 604, tparser = toFIXString }
tLegSecurityAltID = FIXTag { tnum = 605, tparser = toFIXString }
tLegSecurityAltIDSource = FIXTag { tnum = 606, tparser = toFIXString }
tLegProduct = FIXTag { tnum = 607, tparser = toFIXInt }
tLegCFICode = FIXTag { tnum = 608, tparser = toFIXString }
tLegSecurityType = FIXTag { tnum = 609, tparser = toFIXString }
tLegMaturityMonthYear = FIXTag { tnum = 610, tparser = toFIXString }
tLegMaturityDate = FIXTag { tnum = 611, tparser = toFIXLocalMktDate }
tLegStrikePrice = FIXTag { tnum = 612, tparser = toFIXPrice }
tLegOptAttribute = FIXTag { tnum = 613, tparser = toFIXString }
tLegContractMultiplier = FIXTag { tnum = 614, tparser = toFIXFloat }
tLegCouponRate = FIXTag { tnum = 615, tparser = toFIXString }
tLegSecurityExchange = FIXTag { tnum = 616, tparser = toFIXExchange }
tLegIssuer = FIXTag { tnum = 617, tparser = toFIXString }
tEncodedLegIssuerLen = FIXTag { tnum = 618, tparser = toFIXString }
tEncodedLegIssuer = FIXTag { tnum = 619, tparser = toFIXString }
tLegSecurityDesc = FIXTag { tnum = 620, tparser = toFIXString }
tEncodedLegSecurityDescLen = FIXTag { tnum = 621, tparser = toFIXString }
tEncodedLegSecurityDesc = FIXTag { tnum = 622, tparser = toFIXString }
tLegRatioQty = FIXTag { tnum = 623, tparser = toFIXFloat }
tLegSide = FIXTag { tnum = 624, tparser = toFIXString }
tTradingSessionSubID = FIXTag { tnum = 625, tparser = toFIXString }
tAllocType = FIXTag { tnum = 626, tparser = toFIXInt }
tNoHops = FIXTag { tnum = 627, tparser = toFIXString }
tHopCompID = FIXTag { tnum = 628, tparser = toFIXString }
tHopSendingTime = FIXTag { tnum = 629, tparser = toFIXUTCTimestamp }
tHopRefID = FIXTag { tnum = 630, tparser = toFIXString }
tMidPx = FIXTag { tnum = 631, tparser = toFIXPrice }
tBidYield = FIXTag { tnum = 632, tparser = toFIXString }
tMidYield = FIXTag { tnum = 633, tparser = toFIXString }
tOfferYield = FIXTag { tnum = 634, tparser = toFIXString }
tClearingFeeIndicator = FIXTag { tnum = 635, tparser = toFIXString }
tWorkingIndicator = FIXTag { tnum = 636, tparser = toFIXString }
tLegLastPx = FIXTag { tnum = 637, tparser = toFIXPrice }
tPriorityIndicator = FIXTag { tnum = 638, tparser = toFIXInt }
tPriceImprovement = FIXTag { tnum = 639, tparser = toFIXPriceOffset }
tPrice2 = FIXTag { tnum = 640, tparser = toFIXPrice }
tLastForwardPoints2 = FIXTag { tnum = 641, tparser = toFIXPriceOffset }
tBidForwardPoints2 = FIXTag { tnum = 642, tparser = toFIXPriceOffset }
tOfferForwardPoints2 = FIXTag { tnum = 643, tparser = toFIXPriceOffset }
tRFQReqID = FIXTag { tnum = 644, tparser = toFIXString }
tMktBidPx = FIXTag { tnum = 645, tparser = toFIXPrice }
tMktOfferPx = FIXTag { tnum = 646, tparser = toFIXPrice }
tMinBidSize = FIXTag { tnum = 647, tparser = toFIXString }
tMinOfferSize = FIXTag { tnum = 648, tparser = toFIXString }
tQuoteStatusReqID = FIXTag { tnum = 649, tparser = toFIXString }
tLegalConfirm = FIXTag { tnum = 650, tparser = toFIXString }
tUnderlyingLastPx = FIXTag { tnum = 651, tparser = toFIXPrice }
tUnderlyingLastQty = FIXTag { tnum = 652, tparser = toFIXString }
tSecDefStatus = FIXTag { tnum = 653, tparser = toFIXInt }
tLegRefID = FIXTag { tnum = 654, tparser = toFIXString }
tContraLegRefID = FIXTag { tnum = 655, tparser = toFIXString }
tSettlCurrBidFxRate = FIXTag { tnum = 656, tparser = toFIXFloat }
tSettlCurrOfferFxRate = FIXTag { tnum = 657, tparser = toFIXFloat }
tQuoteRequestRejectReason = FIXTag { tnum = 658, tparser = toFIXInt }
tSideComplianceID = FIXTag { tnum = 659, tparser = toFIXString }
tAcctIDSource = FIXTag { tnum = 660, tparser = toFIXInt }
tAllocAcctIDSource = FIXTag { tnum = 661, tparser = toFIXInt }
tBenchmarkPrice = FIXTag { tnum = 662, tparser = toFIXPrice }
tBenchmarkPriceType = FIXTag { tnum = 663, tparser = toFIXInt }
tConfirmID = FIXTag { tnum = 664, tparser = toFIXString }
tConfirmStatus = FIXTag { tnum = 665, tparser = toFIXInt }
tConfirmTransType = FIXTag { tnum = 666, tparser = toFIXInt }
tContractSettlMonth = FIXTag { tnum = 667, tparser = toFIXString }
tDeliveryForm = FIXTag { tnum = 668, tparser = toFIXInt }
tLastParPx = FIXTag { tnum = 669, tparser = toFIXPrice }
tNoLegAllocs = FIXTag { tnum = 670, tparser = toFIXString }
tLegAllocAccount = FIXTag { tnum = 671, tparser = toFIXString }
tLegIndividualAllocID = FIXTag { tnum = 672, tparser = toFIXString }
tLegAllocQty = FIXTag { tnum = 673, tparser = toFIXString }
tLegAllocAcctIDSource = FIXTag { tnum = 674, tparser = toFIXString }
tLegSettlCurrency = FIXTag { tnum = 675, tparser = toFIXCurrency }
tLegBenchmarkCurveCurrency = FIXTag { tnum = 676, tparser = toFIXCurrency }
tLegBenchmarkCurveName = FIXTag { tnum = 677, tparser = toFIXString }
tLegBenchmarkCurvePoint = FIXTag { tnum = 678, tparser = toFIXString }
tLegBenchmarkPrice = FIXTag { tnum = 679, tparser = toFIXPrice }
tLegBenchmarkPriceType = FIXTag { tnum = 680, tparser = toFIXInt }
tLegBidPx = FIXTag { tnum = 681, tparser = toFIXPrice }
tLegIOIQty = FIXTag { tnum = 682, tparser = toFIXString }
tNoLegStipulations = FIXTag { tnum = 683, tparser = toFIXString }
tLegOfferPx = FIXTag { tnum = 684, tparser = toFIXPrice }
tLegOrderQty = FIXTag { tnum = 685, tparser = toFIXString }
tLegPriceType = FIXTag { tnum = 686, tparser = toFIXInt }
tLegQty = FIXTag { tnum = 687, tparser = toFIXString }
tLegStipulationType = FIXTag { tnum = 688, tparser = toFIXString }
tLegStipulationValue = FIXTag { tnum = 689, tparser = toFIXString }
tLegSwapType = FIXTag { tnum = 690, tparser = toFIXInt }
tPool = FIXTag { tnum = 691, tparser = toFIXString }
tQuotePriceType = FIXTag { tnum = 692, tparser = toFIXInt }
tQuoteRespID = FIXTag { tnum = 693, tparser = toFIXString }
tQuoteRespType = FIXTag { tnum = 694, tparser = toFIXInt }
tQuoteQualifier = FIXTag { tnum = 695, tparser = toFIXString }
tYieldRedemptionDate = FIXTag { tnum = 696, tparser = toFIXLocalMktDate }
tYieldRedemptionPrice = FIXTag { tnum = 697, tparser = toFIXPrice }
tYieldRedemptionPriceType = FIXTag { tnum = 698, tparser = toFIXInt }
tBenchmarkSecurityID = FIXTag { tnum = 699, tparser = toFIXString }
tReversalIndicator = FIXTag { tnum = 700, tparser = toFIXString }
tYieldCalcDate = FIXTag { tnum = 701, tparser = toFIXLocalMktDate }
tNoPositions = FIXTag { tnum = 702, tparser = toFIXString }
tPosType = FIXTag { tnum = 703, tparser = toFIXString }
tLongQty = FIXTag { tnum = 704, tparser = toFIXString }
tShortQty = FIXTag { tnum = 705, tparser = toFIXString }
tPosQtyStatus = FIXTag { tnum = 706, tparser = toFIXInt }
tPosAmtType = FIXTag { tnum = 707, tparser = toFIXString }
tPosAmt = FIXTag { tnum = 708, tparser = toFIXAmt }
tPosTransType = FIXTag { tnum = 709, tparser = toFIXInt }
tPosReqID = FIXTag { tnum = 710, tparser = toFIXString }
tNoUnderlyings = FIXTag { tnum = 711, tparser = toFIXString }
tPosMaintAction = FIXTag { tnum = 712, tparser = toFIXInt }
tOrigPosReqRefID = FIXTag { tnum = 713, tparser = toFIXString }
tPosMaintRptRefID = FIXTag { tnum = 714, tparser = toFIXString }
tClearingBusinessDate = FIXTag { tnum = 715, tparser = toFIXLocalMktDate }
tSettlSessID = FIXTag { tnum = 716, tparser = toFIXString }
tSettlSessSubID = FIXTag { tnum = 717, tparser = toFIXString }
tAdjustmentType = FIXTag { tnum = 718, tparser = toFIXInt }
tContraryInstructionIndicator = FIXTag { tnum = 719, tparser = toFIXString }
tPriorSpreadIndicator = FIXTag { tnum = 720, tparser = toFIXString }
tPosMaintRptID = FIXTag { tnum = 721, tparser = toFIXString }
tPosMaintStatus = FIXTag { tnum = 722, tparser = toFIXInt }
tPosMaintResult = FIXTag { tnum = 723, tparser = toFIXInt }
tPosReqType = FIXTag { tnum = 724, tparser = toFIXInt }
tResponseTransportType = FIXTag { tnum = 725, tparser = toFIXInt }
tResponseDestination = FIXTag { tnum = 726, tparser = toFIXString }
tTotalNumPosReports = FIXTag { tnum = 727, tparser = toFIXInt }
tPosReqResult = FIXTag { tnum = 728, tparser = toFIXInt }
tPosReqStatus = FIXTag { tnum = 729, tparser = toFIXInt }
tSettlPrice = FIXTag { tnum = 730, tparser = toFIXPrice }
tSettlPriceType = FIXTag { tnum = 731, tparser = toFIXInt }
tUnderlyingSettlPrice = FIXTag { tnum = 732, tparser = toFIXPrice }
tUnderlyingSettlPriceType = FIXTag { tnum = 733, tparser = toFIXInt }
tPriorSettlPrice = FIXTag { tnum = 734, tparser = toFIXPrice }
tNoQuoteQualifiers = FIXTag { tnum = 735, tparser = toFIXString }
tAllocSettlCurrency = FIXTag { tnum = 736, tparser = toFIXCurrency }
tAllocSettlCurrAmt = FIXTag { tnum = 737, tparser = toFIXAmt }
tInterestAtMaturity = FIXTag { tnum = 738, tparser = toFIXAmt }
tLegDatedDate = FIXTag { tnum = 739, tparser = toFIXLocalMktDate }
tLegPool = FIXTag { tnum = 740, tparser = toFIXString }
tAllocInterestAtMaturity = FIXTag { tnum = 741, tparser = toFIXAmt }
tAllocAccruedInterestAmt = FIXTag { tnum = 742, tparser = toFIXAmt }
tDeliveryDate = FIXTag { tnum = 743, tparser = toFIXLocalMktDate }
tAssignmentMethod = FIXTag { tnum = 744, tparser = toFIXString }
tAssignmentUnit = FIXTag { tnum = 745, tparser = toFIXString }
tOpenInterest = FIXTag { tnum = 746, tparser = toFIXAmt }
tExerciseMethod = FIXTag { tnum = 747, tparser = toFIXString }
tTotNumTradeReports = FIXTag { tnum = 748, tparser = toFIXInt }
tTradeRequestResult = FIXTag { tnum = 749, tparser = toFIXInt }
tTradeRequestStatus = FIXTag { tnum = 750, tparser = toFIXInt }
tTradeReportRejectReason = FIXTag { tnum = 751, tparser = toFIXInt }
tSideMultiLegReportingType = FIXTag { tnum = 752, tparser = toFIXInt }
tNoPosAmt = FIXTag { tnum = 753, tparser = toFIXString }
tAutoAcceptIndicator = FIXTag { tnum = 754, tparser = toFIXString }
tAllocReportID = FIXTag { tnum = 755, tparser = toFIXString }
tNoNested2PartyIDs = FIXTag { tnum = 756, tparser = toFIXString }
tNested2PartyID = FIXTag { tnum = 757, tparser = toFIXString }
tNested2PartyIDSource = FIXTag { tnum = 758, tparser = toFIXString }
tNested2PartyRole = FIXTag { tnum = 759, tparser = toFIXInt }
tNested2PartySubID = FIXTag { tnum = 760, tparser = toFIXString }
tBenchmarkSecurityIDSource = FIXTag { tnum = 761, tparser = toFIXString }
tSecuritySubType = FIXTag { tnum = 762, tparser = toFIXString }
tUnderlyingSecuritySubType = FIXTag { tnum = 763, tparser = toFIXString }
tLegSecuritySubType = FIXTag { tnum = 764, tparser = toFIXString }
tAllowableOneSidednessPct = FIXTag { tnum = 765, tparser = toFIXString }
tAllowableOneSidednessValue = FIXTag { tnum = 766, tparser = toFIXAmt }
tAllowableOneSidednessCurr = FIXTag { tnum = 767, tparser = toFIXCurrency }
tNoTrdRegTimestamps = FIXTag { tnum = 768, tparser = toFIXString }
tTrdRegTimestamp = FIXTag { tnum = 769, tparser = toFIXUTCTimestamp }
tTrdRegTimestampType = FIXTag { tnum = 770, tparser = toFIXInt }
tTrdRegTimestampOrigin = FIXTag { tnum = 771, tparser = toFIXString }
tConfirmRefID = FIXTag { tnum = 772, tparser = toFIXString }
tConfirmType = FIXTag { tnum = 773, tparser = toFIXInt }
tConfirmRejReason = FIXTag { tnum = 774, tparser = toFIXInt }
tBookingType = FIXTag { tnum = 775, tparser = toFIXInt }
tIndividualAllocRejCode = FIXTag { tnum = 776, tparser = toFIXInt }
tSettlInstMsgID = FIXTag { tnum = 777, tparser = toFIXString }
tNoSettlInst = FIXTag { tnum = 778, tparser = toFIXString }
tLastUpdateTime = FIXTag { tnum = 779, tparser = toFIXUTCTimestamp }
tAllocSettlInstType = FIXTag { tnum = 780, tparser = toFIXInt }
tNoSettlPartyIDs = FIXTag { tnum = 781, tparser = toFIXString }
tSettlPartyID = FIXTag { tnum = 782, tparser = toFIXString }
tSettlPartyIDSource = FIXTag { tnum = 783, tparser = toFIXString }
tSettlPartyRole = FIXTag { tnum = 784, tparser = toFIXInt }
tSettlPartySubID = FIXTag { tnum = 785, tparser = toFIXString }
tSettlPartySubIDType = FIXTag { tnum = 786, tparser = toFIXInt }
tDlvyInstType = FIXTag { tnum = 787, tparser = toFIXString }
tTerminationType = FIXTag { tnum = 788, tparser = toFIXInt }
tNextExpectedMsgSeqNum = FIXTag { tnum = 789, tparser = toFIXString }
tOrdStatusReqID = FIXTag { tnum = 790, tparser = toFIXString }
tSettlInstReqID = FIXTag { tnum = 791, tparser = toFIXString }
tSettlInstReqRejCode = FIXTag { tnum = 792, tparser = toFIXInt }
tSecondaryAllocID = FIXTag { tnum = 793, tparser = toFIXString }
tAllocReportType = FIXTag { tnum = 794, tparser = toFIXInt }
tAllocReportRefID = FIXTag { tnum = 795, tparser = toFIXString }
tAllocCancReplaceReason = FIXTag { tnum = 796, tparser = toFIXInt }
tCopyMsgIndicator = FIXTag { tnum = 797, tparser = toFIXString }
tAllocAccountType = FIXTag { tnum = 798, tparser = toFIXInt }
tOrderAvgPx = FIXTag { tnum = 799, tparser = toFIXPrice }
tOrderBookingQty = FIXTag { tnum = 800, tparser = toFIXString }
tNoSettlPartySubIDs = FIXTag { tnum = 801, tparser = toFIXString }
tNoPartySubIDs = FIXTag { tnum = 802, tparser = toFIXString }
tPartySubIDType = FIXTag { tnum = 803, tparser = toFIXInt }
tNoNestedPartySubIDs = FIXTag { tnum = 804, tparser = toFIXString }
tNestedPartySubIDType = FIXTag { tnum = 805, tparser = toFIXInt }
tNoNested2PartySubIDs = FIXTag { tnum = 806, tparser = toFIXString }
tNested2PartySubIDType = FIXTag { tnum = 807, tparser = toFIXInt }
tAllocIntermedReqType = FIXTag { tnum = 808, tparser = toFIXInt }
tUnderlyingPx = FIXTag { tnum = 810, tparser = toFIXPrice }
tPriceDelta = FIXTag { tnum = 811, tparser = toFIXFloat }
tApplQueueMax = FIXTag { tnum = 812, tparser = toFIXInt }
tApplQueueDepth = FIXTag { tnum = 813, tparser = toFIXInt }
tApplQueueResolution = FIXTag { tnum = 814, tparser = toFIXInt }
tApplQueueAction = FIXTag { tnum = 815, tparser = toFIXInt }
tNoAltMDSource = FIXTag { tnum = 816, tparser = toFIXString }
tAltMDSourceID = FIXTag { tnum = 817, tparser = toFIXString }
tSecondaryTradeReportID = FIXTag { tnum = 818, tparser = toFIXString }
tAvgPxIndicator = FIXTag { tnum = 819, tparser = toFIXInt }
tTradeLinkID = FIXTag { tnum = 820, tparser = toFIXString }
tOrderInputDevice = FIXTag { tnum = 821, tparser = toFIXString }
tUnderlyingTradingSessionID = FIXTag { tnum = 822, tparser = toFIXString }
tUnderlyingTradingSessionSubID = FIXTag { tnum = 823, tparser = toFIXString }
tTradeLegRefID = FIXTag { tnum = 824, tparser = toFIXString }
tExchangeRule = FIXTag { tnum = 825, tparser = toFIXString }
tTradeAllocIndicator = FIXTag { tnum = 826, tparser = toFIXInt }
tExpirationCycle = FIXTag { tnum = 827, tparser = toFIXInt }
tTrdType = FIXTag { tnum = 828, tparser = toFIXInt }
tTrdSubType = FIXTag { tnum = 829, tparser = toFIXInt }
tTransferReason = FIXTag { tnum = 830, tparser = toFIXString }
tAsgnReqID = FIXTag { tnum = 831, tparser = toFIXString }
tTotNumAssignmentReports = FIXTag { tnum = 832, tparser = toFIXInt }
tAsgnRptID = FIXTag { tnum = 833, tparser = toFIXString }
tThresholdAmount = FIXTag { tnum = 834, tparser = toFIXPriceOffset }
tPegMoveType = FIXTag { tnum = 835, tparser = toFIXInt }
tPegOffsetType = FIXTag { tnum = 836, tparser = toFIXInt }
tPegLimitType = FIXTag { tnum = 837, tparser = toFIXInt }
tPegRoundDirection = FIXTag { tnum = 838, tparser = toFIXInt }
tPeggedPrice = FIXTag { tnum = 839, tparser = toFIXPrice }
tPegScope = FIXTag { tnum = 840, tparser = toFIXInt }
tDiscretionMoveType = FIXTag { tnum = 841, tparser = toFIXInt }
tDiscretionOffsetType = FIXTag { tnum = 842, tparser = toFIXInt }
tDiscretionLimitType = FIXTag { tnum = 843, tparser = toFIXInt }
tDiscretionRoundDirection = FIXTag { tnum = 844, tparser = toFIXInt }
tDiscretionPrice = FIXTag { tnum = 845, tparser = toFIXPrice }
tDiscretionScope = FIXTag { tnum = 846, tparser = toFIXInt }
tTargetStrategy = FIXTag { tnum = 847, tparser = toFIXInt }
tTargetStrategyParameters = FIXTag { tnum = 848, tparser = toFIXString }
tParticipationRate = FIXTag { tnum = 849, tparser = toFIXString }
tTargetStrategyPerformance = FIXTag { tnum = 850, tparser = toFIXFloat }
tLastLiquidityInd = FIXTag { tnum = 851, tparser = toFIXInt }
tPublishTrdIndicator = FIXTag { tnum = 852, tparser = toFIXString }
tShortSaleReason = FIXTag { tnum = 853, tparser = toFIXInt }
tQtyType = FIXTag { tnum = 854, tparser = toFIXInt }
tSecondaryTrdType = FIXTag { tnum = 855, tparser = toFIXInt }
tTradeReportType = FIXTag { tnum = 856, tparser = toFIXInt }
tAllocNoOrdersType = FIXTag { tnum = 857, tparser = toFIXInt }
tSharedCommission = FIXTag { tnum = 858, tparser = toFIXAmt }
tConfirmReqID = FIXTag { tnum = 859, tparser = toFIXString }
tAvgParPx = FIXTag { tnum = 860, tparser = toFIXPrice }
tReportedPx = FIXTag { tnum = 861, tparser = toFIXPrice }
tNoCapacities = FIXTag { tnum = 862, tparser = toFIXString }
tOrderCapacityQty = FIXTag { tnum = 863, tparser = toFIXString }
tNoEvents = FIXTag { tnum = 864, tparser = toFIXString }
tEventType = FIXTag { tnum = 865, tparser = toFIXInt }
tEventDate = FIXTag { tnum = 866, tparser = toFIXLocalMktDate }
tEventPx = FIXTag { tnum = 867, tparser = toFIXPrice }
tEventText = FIXTag { tnum = 868, tparser = toFIXString }
tPctAtRisk = FIXTag { tnum = 869, tparser = toFIXString }
tNoInstrAttrib = FIXTag { tnum = 870, tparser = toFIXString }
tInstrAttribType = FIXTag { tnum = 871, tparser = toFIXInt }
tInstrAttribValue = FIXTag { tnum = 872, tparser = toFIXString }
tDatedDate = FIXTag { tnum = 873, tparser = toFIXLocalMktDate }
tInterestAccrualDate = FIXTag { tnum = 874, tparser = toFIXLocalMktDate }
tCPProgram = FIXTag { tnum = 875, tparser = toFIXInt }
tCPRegType = FIXTag { tnum = 876, tparser = toFIXString }
tUnderlyingCPProgram = FIXTag { tnum = 877, tparser = toFIXString }
tUnderlyingCPRegType = FIXTag { tnum = 878, tparser = toFIXString }
tUnderlyingQty = FIXTag { tnum = 879, tparser = toFIXString }
tTrdMatchID = FIXTag { tnum = 880, tparser = toFIXString }
tSecondaryTradeReportRefID = FIXTag { tnum = 881, tparser = toFIXString }
tUnderlyingDirtyPrice = FIXTag { tnum = 882, tparser = toFIXPrice }
tUnderlyingEndPrice = FIXTag { tnum = 883, tparser = toFIXPrice }
tUnderlyingStartValue = FIXTag { tnum = 884, tparser = toFIXAmt }
tUnderlyingCurrentValue = FIXTag { tnum = 885, tparser = toFIXAmt }
tUnderlyingEndValue = FIXTag { tnum = 886, tparser = toFIXAmt }
tNoUnderlyingStips = FIXTag { tnum = 887, tparser = toFIXString }
tUnderlyingStipType = FIXTag { tnum = 888, tparser = toFIXString }
tUnderlyingStipValue = FIXTag { tnum = 889, tparser = toFIXString }
tMaturityNetMoney = FIXTag { tnum = 890, tparser = toFIXAmt }
tMiscFeeBasis = FIXTag { tnum = 891, tparser = toFIXInt }
tTotNoAllocs = FIXTag { tnum = 892, tparser = toFIXInt }
tLastFragment = FIXTag { tnum = 893, tparser = toFIXString }
tCollReqID = FIXTag { tnum = 894, tparser = toFIXString }
tCollAsgnReason = FIXTag { tnum = 895, tparser = toFIXInt }
tCollInquiryQualifier = FIXTag { tnum = 896, tparser = toFIXInt }
tNoTrades = FIXTag { tnum = 897, tparser = toFIXString }
tMarginRatio = FIXTag { tnum = 898, tparser = toFIXString }
tMarginExcess = FIXTag { tnum = 899, tparser = toFIXAmt }
tTotalNetValue = FIXTag { tnum = 900, tparser = toFIXAmt }
tCashOutstanding = FIXTag { tnum = 901, tparser = toFIXAmt }
tCollAsgnID = FIXTag { tnum = 902, tparser = toFIXString }
tCollAsgnTransType = FIXTag { tnum = 903, tparser = toFIXInt }
tCollRespID = FIXTag { tnum = 904, tparser = toFIXString }
tCollAsgnRespType = FIXTag { tnum = 905, tparser = toFIXInt }
tCollAsgnRejectReason = FIXTag { tnum = 906, tparser = toFIXInt }
tCollAsgnRefID = FIXTag { tnum = 907, tparser = toFIXString }
tCollRptID = FIXTag { tnum = 908, tparser = toFIXString }
tCollInquiryID = FIXTag { tnum = 909, tparser = toFIXString }
tCollStatus = FIXTag { tnum = 910, tparser = toFIXInt }
tTotNumReports = FIXTag { tnum = 911, tparser = toFIXInt }
tLastRptRequested = FIXTag { tnum = 912, tparser = toFIXString }
tAgreementDesc = FIXTag { tnum = 913, tparser = toFIXString }
tAgreementID = FIXTag { tnum = 914, tparser = toFIXString }
tAgreementDate = FIXTag { tnum = 915, tparser = toFIXLocalMktDate }
tStartDate = FIXTag { tnum = 916, tparser = toFIXLocalMktDate }
tEndDate = FIXTag { tnum = 917, tparser = toFIXLocalMktDate }
tAgreementCurrency = FIXTag { tnum = 918, tparser = toFIXCurrency }
tDeliveryType = FIXTag { tnum = 919, tparser = toFIXInt }
tEndAccruedInterestAmt = FIXTag { tnum = 920, tparser = toFIXAmt }
tStartCash = FIXTag { tnum = 921, tparser = toFIXAmt }
tEndCash = FIXTag { tnum = 922, tparser = toFIXAmt }
tUserRequestID = FIXTag { tnum = 923, tparser = toFIXString }
tUserRequestType = FIXTag { tnum = 924, tparser = toFIXInt }
tNewPassword = FIXTag { tnum = 925, tparser = toFIXString }
tUserStatus = FIXTag { tnum = 926, tparser = toFIXInt }
tUserStatusText = FIXTag { tnum = 927, tparser = toFIXString }
tStatusValue = FIXTag { tnum = 928, tparser = toFIXInt }
tStatusText = FIXTag { tnum = 929, tparser = toFIXString }
tRefCompID = FIXTag { tnum = 930, tparser = toFIXString }
tRefSubID = FIXTag { tnum = 931, tparser = toFIXString }
tNetworkResponseID = FIXTag { tnum = 932, tparser = toFIXString }
tNetworkRequestID = FIXTag { tnum = 933, tparser = toFIXString }
tLastNetworkResponseID = FIXTag { tnum = 934, tparser = toFIXString }
tNetworkRequestType = FIXTag { tnum = 935, tparser = toFIXInt }
tNoCompIDs = FIXTag { tnum = 936, tparser = toFIXString }
tNetworkStatusResponseType = FIXTag { tnum = 937, tparser = toFIXInt }
tNoCollInquiryQualifier = FIXTag { tnum = 938, tparser = toFIXString }
tTrdRptStatus = FIXTag { tnum = 939, tparser = toFIXInt }
tAffirmStatus = FIXTag { tnum = 940, tparser = toFIXInt }
tUnderlyingStrikeCurrency = FIXTag { tnum = 941, tparser = toFIXCurrency }
tLegStrikeCurrency = FIXTag { tnum = 942, tparser = toFIXCurrency }
tTimeBracket = FIXTag { tnum = 943, tparser = toFIXString }
tCollAction = FIXTag { tnum = 944, tparser = toFIXInt }
tCollInquiryStatus = FIXTag { tnum = 945, tparser = toFIXInt }
tCollInquiryResult = FIXTag { tnum = 946, tparser = toFIXInt }
tStrikeCurrency = FIXTag { tnum = 947, tparser = toFIXCurrency }
tNoNested3PartyIDs = FIXTag { tnum = 948, tparser = toFIXString }
tNested3PartyID = FIXTag { tnum = 949, tparser = toFIXString }
tNested3PartyIDSource = FIXTag { tnum = 950, tparser = toFIXString }
tNested3PartyRole = FIXTag { tnum = 951, tparser = toFIXInt }
tNoNested3PartySubIDs = FIXTag { tnum = 952, tparser = toFIXString }
tNested3PartySubID = FIXTag { tnum = 953, tparser = toFIXString }
tNested3PartySubIDType = FIXTag { tnum = 954, tparser = toFIXInt }
tLegContractSettlMonth = FIXTag { tnum = 955, tparser = toFIXString }
tLegInterestAccrualDate = FIXTag { tnum = 956, tparser = toFIXLocalMktDate }
mHeartbeat :: FIXMessageSpec
mHeartbeat = FMSpec { mType = (C.pack "0"), mTags = mHeartbeatTags }
   where
      mHeartbeatTags = 
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
          LT.insert (tnum tTestReqID) tTestReqID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTestRequest :: FIXMessageSpec
mTestRequest = FMSpec { mType = (C.pack "1"), mTags = mTestRequestTags }
   where
      mTestRequestTags = 
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
          LT.insert (tnum tTestReqID) tTestReqID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mResendRequest :: FIXMessageSpec
mResendRequest = FMSpec { mType = (C.pack "2"), mTags = mResendRequestTags }
   where
      mResendRequestTags = 
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
          LT.insert (tnum tBeginSeqNo) tBeginSeqNo $
          LT.insert (tnum tEndSeqNo) tEndSeqNo $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mReject :: FIXMessageSpec
mReject = FMSpec { mType = (C.pack "3"), mTags = mRejectTags }
   where
      mRejectTags = 
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
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tRefTagID) tRefTagID $
          LT.insert (tnum tRefMsgType) tRefMsgType $
          LT.insert (tnum tSessionRejectReason) tSessionRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSequenceReset :: FIXMessageSpec
mSequenceReset = FMSpec { mType = (C.pack "4"), mTags = mSequenceResetTags }
   where
      mSequenceResetTags = 
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
          LT.insert (tnum tGapFillFlag) tGapFillFlag $
          LT.insert (tnum tNewSeqNo) tNewSeqNo $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mLogout :: FIXMessageSpec
mLogout = FMSpec { mType = (C.pack "5"), mTags = mLogoutTags }
   where
      mLogoutTags = 
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
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mIOI :: FIXMessageSpec
mIOI = FMSpec { mType = (C.pack "6"), mTags = mIOITags }
   where
      mIOITags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAdvertisement :: FIXMessageSpec
mAdvertisement = FMSpec { mType = (C.pack "7"), mTags = mAdvertisementTags }
   where
      mAdvertisementTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mExecutionReport :: FIXMessageSpec
mExecutionReport = FMSpec { mType = (C.pack "8"), mTags = mExecutionReportTags }
   where
      mExecutionReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderCancelReject :: FIXMessageSpec
mOrderCancelReject = FMSpec { mType = (C.pack "9"), mTags = mOrderCancelRejectTags }
   where
      mOrderCancelRejectTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mLogon :: FIXMessageSpec
mLogon = FMSpec { mType = (C.pack "A"), mTags = mLogonTags }
   where
      mLogonTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNews :: FIXMessageSpec
mNews = FMSpec { mType = (C.pack "B"), mTags = mNewsTags }
   where
      mNewsTags = 
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
          LT.insert (tnum tOrigTime) tOrigTime $
          LT.insert (tnum tUrgency) tUrgency $
          LT.insert (tnum tHeadline) tHeadline $
          LT.insert (tnum tEncodedHeadlineLen) tEncodedHeadlineLen $
          LT.insert (tnum tEncodedHeadline) tEncodedHeadline $
          LT.insert (tnum tURLLink) tURLLink $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mEmail :: FIXMessageSpec
mEmail = FMSpec { mType = (C.pack "C"), mTags = mEmailTags }
   where
      mEmailTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNewOrderSingle :: FIXMessageSpec
mNewOrderSingle = FMSpec { mType = (C.pack "D"), mTags = mNewOrderSingleTags }
   where
      mNewOrderSingleTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNewOrderList :: FIXMessageSpec
mNewOrderList = FMSpec { mType = (C.pack "E"), mTags = mNewOrderListTags }
   where
      mNewOrderListTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderCancelRequest :: FIXMessageSpec
mOrderCancelRequest = FMSpec { mType = (C.pack "F"), mTags = mOrderCancelRequestTags }
   where
      mOrderCancelRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderCancelReplaceRequest :: FIXMessageSpec
mOrderCancelReplaceRequest = FMSpec { mType = (C.pack "G"), mTags = mOrderCancelReplaceRequestTags }
   where
      mOrderCancelReplaceRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderStatusRequest :: FIXMessageSpec
mOrderStatusRequest = FMSpec { mType = (C.pack "H"), mTags = mOrderStatusRequestTags }
   where
      mOrderStatusRequestTags = 
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
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tSecondaryClOrdID) tSecondaryClOrdID $
          LT.insert (tnum tClOrdLinkID) tClOrdLinkID $
          LT.insert (tnum tOrdStatusReqID) tOrdStatusReqID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAllocationInstruction :: FIXMessageSpec
mAllocationInstruction = FMSpec { mType = (C.pack "J"), mTags = mAllocationInstructionTags }
   where
      mAllocationInstructionTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mListCancelRequest :: FIXMessageSpec
mListCancelRequest = FMSpec { mType = (C.pack "K"), mTags = mListCancelRequestTags }
   where
      mListCancelRequestTags = 
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
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tTradeOriginationDate) tTradeOriginationDate $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mListExecute :: FIXMessageSpec
mListExecute = FMSpec { mType = (C.pack "L"), mTags = mListExecuteTags }
   where
      mListExecuteTags = 
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
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mListStatusRequest :: FIXMessageSpec
mListStatusRequest = FMSpec { mType = (C.pack "M"), mTags = mListStatusRequestTags }
   where
      mListStatusRequestTags = 
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
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mListStatus :: FIXMessageSpec
mListStatus = FMSpec { mType = (C.pack "N"), mTags = mListStatusTags }
   where
      mListStatusTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAllocationInstructionAck :: FIXMessageSpec
mAllocationInstructionAck = FMSpec { mType = (C.pack "P"), mTags = mAllocationInstructionAckTags }
   where
      mAllocationInstructionAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mDontKnowTrade :: FIXMessageSpec
mDontKnowTrade = FMSpec { mType = (C.pack "Q"), mTags = mDontKnowTradeTags }
   where
      mDontKnowTradeTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteRequest :: FIXMessageSpec
mQuoteRequest = FMSpec { mType = (C.pack "R"), mTags = mQuoteRequestTags }
   where
      mQuoteRequestTags = 
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
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tOrderCapacity) tOrderCapacity $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuote :: FIXMessageSpec
mQuote = FMSpec { mType = (C.pack "S"), mTags = mQuoteTags }
   where
      mQuoteTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSettlementInstructions :: FIXMessageSpec
mSettlementInstructions = FMSpec { mType = (C.pack "T"), mTags = mSettlementInstructionsTags }
   where
      mSettlementInstructionsTags = 
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
          LT.insert (tnum tSettlInstMsgID) tSettlInstMsgID $
          LT.insert (tnum tSettlInstReqID) tSettlInstReqID $
          LT.insert (tnum tSettlInstMode) tSettlInstMode $
          LT.insert (tnum tSettlInstReqRejCode) tSettlInstReqRejCode $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMarketDataRequest :: FIXMessageSpec
mMarketDataRequest = FMSpec { mType = (C.pack "V"), mTags = mMarketDataRequestTags }
   where
      mMarketDataRequestTags = 
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
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tMarketDepth) tMarketDepth $
          LT.insert (tnum tMDUpdateType) tMDUpdateType $
          LT.insert (tnum tAggregatedBook) tAggregatedBook $
          LT.insert (tnum tOpenCloseSettlFlag) tOpenCloseSettlFlag $
          LT.insert (tnum tScope) tScope $
          LT.insert (tnum tMDImplicitDelete) tMDImplicitDelete $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMarketDataSnapshotFullRefresh :: FIXMessageSpec
mMarketDataSnapshotFullRefresh = FMSpec { mType = (C.pack "W"), mTags = mMarketDataSnapshotFullRefreshTags }
   where
      mMarketDataSnapshotFullRefreshTags = 
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
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tFinancialStatus) tFinancialStatus $
          LT.insert (tnum tCorporateAction) tCorporateAction $
          LT.insert (tnum tNetChgPrevDay) tNetChgPrevDay $
          LT.insert (tnum tApplQueueDepth) tApplQueueDepth $
          LT.insert (tnum tApplQueueResolution) tApplQueueResolution $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMarketDataIncrementalRefresh :: FIXMessageSpec
mMarketDataIncrementalRefresh = FMSpec { mType = (C.pack "X"), mTags = mMarketDataIncrementalRefreshTags }
   where
      mMarketDataIncrementalRefreshTags = 
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
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tApplQueueDepth) tApplQueueDepth $
          LT.insert (tnum tApplQueueResolution) tApplQueueResolution $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMarketDataRequestReject :: FIXMessageSpec
mMarketDataRequestReject = FMSpec { mType = (C.pack "Y"), mTags = mMarketDataRequestRejectTags }
   where
      mMarketDataRequestRejectTags = 
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
          LT.insert (tnum tMDReqID) tMDReqID $
          LT.insert (tnum tMDReqRejReason) tMDReqRejReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteCancel :: FIXMessageSpec
mQuoteCancel = FMSpec { mType = (C.pack "Z"), mTags = mQuoteCancelTags }
   where
      mQuoteCancelTags = 
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
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteCancelType) tQuoteCancelType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteStatusRequest :: FIXMessageSpec
mQuoteStatusRequest = FMSpec { mType = (C.pack "a"), mTags = mQuoteStatusRequestTags }
   where
      mQuoteStatusRequestTags = 
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
          LT.insert (tnum tQuoteStatusReqID) tQuoteStatusReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMassQuoteAcknowledgement :: FIXMessageSpec
mMassQuoteAcknowledgement = FMSpec { mType = (C.pack "b"), mTags = mMassQuoteAcknowledgementTags }
   where
      mMassQuoteAcknowledgementTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityDefinitionRequest :: FIXMessageSpec
mSecurityDefinitionRequest = FMSpec { mType = (C.pack "c"), mTags = mSecurityDefinitionRequestTags }
   where
      mSecurityDefinitionRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityDefinition :: FIXMessageSpec
mSecurityDefinition = FMSpec { mType = (C.pack "d"), mTags = mSecurityDefinitionTags }
   where
      mSecurityDefinitionTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityStatusRequest :: FIXMessageSpec
mSecurityStatusRequest = FMSpec { mType = (C.pack "e"), mTags = mSecurityStatusRequestTags }
   where
      mSecurityStatusRequestTags = 
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
          LT.insert (tnum tSecurityStatusReqID) tSecurityStatusReqID $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityStatus :: FIXMessageSpec
mSecurityStatus = FMSpec { mType = (C.pack "f"), mTags = mSecurityStatusTags }
   where
      mSecurityStatusTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradingSessionStatusRequest :: FIXMessageSpec
mTradingSessionStatusRequest = FMSpec { mType = (C.pack "g"), mTags = mTradingSessionStatusRequestTags }
   where
      mTradingSessionStatusRequestTags = 
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
          LT.insert (tnum tTradSesReqID) tTradSesReqID $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tTradSesMethod) tTradSesMethod $
          LT.insert (tnum tTradSesMode) tTradSesMode $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradingSessionStatus :: FIXMessageSpec
mTradingSessionStatus = FMSpec { mType = (C.pack "h"), mTags = mTradingSessionStatusTags }
   where
      mTradingSessionStatusTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMassQuote :: FIXMessageSpec
mMassQuote = FMSpec { mType = (C.pack "i"), mTags = mMassQuoteTags }
   where
      mMassQuoteTags = 
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
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tQuoteID) tQuoteID $
          LT.insert (tnum tQuoteType) tQuoteType $
          LT.insert (tnum tQuoteResponseLevel) tQuoteResponseLevel $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tAccountType) tAccountType $
          LT.insert (tnum tDefBidSize) tDefBidSize $
          LT.insert (tnum tDefOfferSize) tDefOfferSize $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mBusinessMessageReject :: FIXMessageSpec
mBusinessMessageReject = FMSpec { mType = (C.pack "j"), mTags = mBusinessMessageRejectTags }
   where
      mBusinessMessageRejectTags = 
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
          LT.insert (tnum tRefSeqNum) tRefSeqNum $
          LT.insert (tnum tRefMsgType) tRefMsgType $
          LT.insert (tnum tBusinessRejectRefID) tBusinessRejectRefID $
          LT.insert (tnum tBusinessRejectReason) tBusinessRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mBidRequest :: FIXMessageSpec
mBidRequest = FMSpec { mType = (C.pack "k"), mTags = mBidRequestTags }
   where
      mBidRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mBidResponse :: FIXMessageSpec
mBidResponse = FMSpec { mType = (C.pack "l"), mTags = mBidResponseTags }
   where
      mBidResponseTags = 
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
          LT.insert (tnum tBidID) tBidID $
          LT.insert (tnum tClientBidID) tClientBidID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mListStrikePrice :: FIXMessageSpec
mListStrikePrice = FMSpec { mType = (C.pack "m"), mTags = mListStrikePriceTags }
   where
      mListStrikePriceTags = 
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
          LT.insert (tnum tListID) tListID $
          LT.insert (tnum tTotNoStrikes) tTotNoStrikes $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mRegistrationInstructions :: FIXMessageSpec
mRegistrationInstructions = FMSpec { mType = (C.pack "o"), mTags = mRegistrationInstructionsTags }
   where
      mRegistrationInstructionsTags = 
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
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tRegistTransType) tRegistTransType $
          LT.insert (tnum tRegistRefID) tRegistRefID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tRegistAcctType) tRegistAcctType $
          LT.insert (tnum tTaxAdvantageType) tTaxAdvantageType $
          LT.insert (tnum tOwnershipType) tOwnershipType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mRegistrationInstructionsResponse :: FIXMessageSpec
mRegistrationInstructionsResponse = FMSpec { mType = (C.pack "p"), mTags = mRegistrationInstructionsResponseTags }
   where
      mRegistrationInstructionsResponseTags = 
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
          LT.insert (tnum tRegistID) tRegistID $
          LT.insert (tnum tRegistTransType) tRegistTransType $
          LT.insert (tnum tRegistRefID) tRegistRefID $
          LT.insert (tnum tClOrdID) tClOrdID $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tRegistStatus) tRegistStatus $
          LT.insert (tnum tRegistRejReasonCode) tRegistRejReasonCode $
          LT.insert (tnum tRegistRejReasonText) tRegistRejReasonText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderMassCancelRequest :: FIXMessageSpec
mOrderMassCancelRequest = FMSpec { mType = (C.pack "q"), mTags = mOrderMassCancelRequestTags }
   where
      mOrderMassCancelRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderMassCancelReport :: FIXMessageSpec
mOrderMassCancelReport = FMSpec { mType = (C.pack "r"), mTags = mOrderMassCancelReportTags }
   where
      mOrderMassCancelReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNewOrderCross :: FIXMessageSpec
mNewOrderCross = FMSpec { mType = (C.pack "s"), mTags = mNewOrderCrossTags }
   where
      mNewOrderCrossTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCrossOrderCancelReplaceRequest :: FIXMessageSpec
mCrossOrderCancelReplaceRequest = FMSpec { mType = (C.pack "t"), mTags = mCrossOrderCancelReplaceRequestTags }
   where
      mCrossOrderCancelReplaceRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCrossOrderCancelRequest :: FIXMessageSpec
mCrossOrderCancelRequest = FMSpec { mType = (C.pack "u"), mTags = mCrossOrderCancelRequestTags }
   where
      mCrossOrderCancelRequestTags = 
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
          LT.insert (tnum tOrderID) tOrderID $
          LT.insert (tnum tCrossID) tCrossID $
          LT.insert (tnum tOrigCrossID) tOrigCrossID $
          LT.insert (tnum tCrossType) tCrossType $
          LT.insert (tnum tCrossPrioritization) tCrossPrioritization $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityTypeRequest :: FIXMessageSpec
mSecurityTypeRequest = FMSpec { mType = (C.pack "v"), mTags = mSecurityTypeRequestTags }
   where
      mSecurityTypeRequestTags = 
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
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tProduct) tProduct $
          LT.insert (tnum tSecurityType) tSecurityType $
          LT.insert (tnum tSecuritySubType) tSecuritySubType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityTypes :: FIXMessageSpec
mSecurityTypes = FMSpec { mType = (C.pack "w"), mTags = mSecurityTypesTags }
   where
      mSecurityTypesTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityListRequest :: FIXMessageSpec
mSecurityListRequest = FMSpec { mType = (C.pack "x"), mTags = mSecurityListRequestTags }
   where
      mSecurityListRequestTags = 
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
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityListRequestType) tSecurityListRequestType $
          LT.insert (tnum tCurrency) tCurrency $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSecurityList :: FIXMessageSpec
mSecurityList = FMSpec { mType = (C.pack "y"), mTags = mSecurityListTags }
   where
      mSecurityListTags = 
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
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
          LT.insert (tnum tTotNoRelatedSym) tTotNoRelatedSym $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mDerivativeSecurityListRequest :: FIXMessageSpec
mDerivativeSecurityListRequest = FMSpec { mType = (C.pack "z"), mTags = mDerivativeSecurityListRequestTags }
   where
      mDerivativeSecurityListRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mDerivativeSecurityList :: FIXMessageSpec
mDerivativeSecurityList = FMSpec { mType = (C.pack "AA"), mTags = mDerivativeSecurityListTags }
   where
      mDerivativeSecurityListTags = 
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
          LT.insert (tnum tSecurityReqID) tSecurityReqID $
          LT.insert (tnum tSecurityResponseID) tSecurityResponseID $
          LT.insert (tnum tSecurityRequestResult) tSecurityRequestResult $
          LT.insert (tnum tTotNoRelatedSym) tTotNoRelatedSym $
          LT.insert (tnum tLastFragment) tLastFragment $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNewOrderMultileg :: FIXMessageSpec
mNewOrderMultileg = FMSpec { mType = (C.pack "AB"), mTags = mNewOrderMultilegTags }
   where
      mNewOrderMultilegTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mMultilegOrderCancelReplace :: FIXMessageSpec
mMultilegOrderCancelReplace = FMSpec { mType = (C.pack "AC"), mTags = mMultilegOrderCancelReplaceTags }
   where
      mMultilegOrderCancelReplaceTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradeCaptureReportRequest :: FIXMessageSpec
mTradeCaptureReportRequest = FMSpec { mType = (C.pack "AD"), mTags = mTradeCaptureReportRequestTags }
   where
      mTradeCaptureReportRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradeCaptureReport :: FIXMessageSpec
mTradeCaptureReport = FMSpec { mType = (C.pack "AE"), mTags = mTradeCaptureReportTags }
   where
      mTradeCaptureReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mOrderMassStatusRequest :: FIXMessageSpec
mOrderMassStatusRequest = FMSpec { mType = (C.pack "AF"), mTags = mOrderMassStatusRequestTags }
   where
      mOrderMassStatusRequestTags = 
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
          LT.insert (tnum tMassStatusReqID) tMassStatusReqID $
          LT.insert (tnum tMassStatusReqType) tMassStatusReqType $
          LT.insert (tnum tAccount) tAccount $
          LT.insert (tnum tAcctIDSource) tAcctIDSource $
          LT.insert (tnum tTradingSessionID) tTradingSessionID $
          LT.insert (tnum tTradingSessionSubID) tTradingSessionSubID $
          LT.insert (tnum tSide) tSide $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteRequestReject :: FIXMessageSpec
mQuoteRequestReject = FMSpec { mType = (C.pack "AG"), mTags = mQuoteRequestRejectTags }
   where
      mQuoteRequestRejectTags = 
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
          LT.insert (tnum tQuoteReqID) tQuoteReqID $
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tQuoteRequestRejectReason) tQuoteRequestRejectReason $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mRFQRequest :: FIXMessageSpec
mRFQRequest = FMSpec { mType = (C.pack "AH"), mTags = mRFQRequestTags }
   where
      mRFQRequestTags = 
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
          LT.insert (tnum tRFQReqID) tRFQReqID $
          LT.insert (tnum tSubscriptionRequestType) tSubscriptionRequestType $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteStatusReport :: FIXMessageSpec
mQuoteStatusReport = FMSpec { mType = (C.pack "AI"), mTags = mQuoteStatusReportTags }
   where
      mQuoteStatusReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mQuoteResponse :: FIXMessageSpec
mQuoteResponse = FMSpec { mType = (C.pack "AJ"), mTags = mQuoteResponseTags }
   where
      mQuoteResponseTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mConfirmation :: FIXMessageSpec
mConfirmation = FMSpec { mType = (C.pack "AK"), mTags = mConfirmationTags }
   where
      mConfirmationTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mPositionMaintenanceRequest :: FIXMessageSpec
mPositionMaintenanceRequest = FMSpec { mType = (C.pack "AL"), mTags = mPositionMaintenanceRequestTags }
   where
      mPositionMaintenanceRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mPositionMaintenanceReport :: FIXMessageSpec
mPositionMaintenanceReport = FMSpec { mType = (C.pack "AM"), mTags = mPositionMaintenanceReportTags }
   where
      mPositionMaintenanceReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mRequestForPositions :: FIXMessageSpec
mRequestForPositions = FMSpec { mType = (C.pack "AN"), mTags = mRequestForPositionsTags }
   where
      mRequestForPositionsTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mRequestForPositionsAck :: FIXMessageSpec
mRequestForPositionsAck = FMSpec { mType = (C.pack "AO"), mTags = mRequestForPositionsAckTags }
   where
      mRequestForPositionsAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mPositionReport :: FIXMessageSpec
mPositionReport = FMSpec { mType = (C.pack "AP"), mTags = mPositionReportTags }
   where
      mPositionReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradeCaptureReportRequestAck :: FIXMessageSpec
mTradeCaptureReportRequestAck = FMSpec { mType = (C.pack "AQ"), mTags = mTradeCaptureReportRequestAckTags }
   where
      mTradeCaptureReportRequestAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mTradeCaptureReportAck :: FIXMessageSpec
mTradeCaptureReportAck = FMSpec { mType = (C.pack "AR"), mTags = mTradeCaptureReportAckTags }
   where
      mTradeCaptureReportAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAllocationReport :: FIXMessageSpec
mAllocationReport = FMSpec { mType = (C.pack "AS"), mTags = mAllocationReportTags }
   where
      mAllocationReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAllocationReportAck :: FIXMessageSpec
mAllocationReportAck = FMSpec { mType = (C.pack "AT"), mTags = mAllocationReportAckTags }
   where
      mAllocationReportAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mConfirmationAck :: FIXMessageSpec
mConfirmationAck = FMSpec { mType = (C.pack "AU"), mTags = mConfirmationAckTags }
   where
      mConfirmationAckTags = 
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
          LT.insert (tnum tConfirmID) tConfirmID $
          LT.insert (tnum tTradeDate) tTradeDate $
          LT.insert (tnum tTransactTime) tTransactTime $
          LT.insert (tnum tAffirmStatus) tAffirmStatus $
          LT.insert (tnum tConfirmRejReason) tConfirmRejReason $
          LT.insert (tnum tMatchStatus) tMatchStatus $
          LT.insert (tnum tText) tText $
          LT.insert (tnum tEncodedTextLen) tEncodedTextLen $
          LT.insert (tnum tEncodedText) tEncodedText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mSettlementInstructionRequest :: FIXMessageSpec
mSettlementInstructionRequest = FMSpec { mType = (C.pack "AV"), mTags = mSettlementInstructionRequestTags }
   where
      mSettlementInstructionRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mAssignmentReport :: FIXMessageSpec
mAssignmentReport = FMSpec { mType = (C.pack "AW"), mTags = mAssignmentReportTags }
   where
      mAssignmentReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralRequest :: FIXMessageSpec
mCollateralRequest = FMSpec { mType = (C.pack "AX"), mTags = mCollateralRequestTags }
   where
      mCollateralRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralAssignment :: FIXMessageSpec
mCollateralAssignment = FMSpec { mType = (C.pack "AY"), mTags = mCollateralAssignmentTags }
   where
      mCollateralAssignmentTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralResponse :: FIXMessageSpec
mCollateralResponse = FMSpec { mType = (C.pack "AZ"), mTags = mCollateralResponseTags }
   where
      mCollateralResponseTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralReport :: FIXMessageSpec
mCollateralReport = FMSpec { mType = (C.pack "BA"), mTags = mCollateralReportTags }
   where
      mCollateralReportTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralInquiry :: FIXMessageSpec
mCollateralInquiry = FMSpec { mType = (C.pack "BB"), mTags = mCollateralInquiryTags }
   where
      mCollateralInquiryTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNetworkCounterpartySystemStatusRequest :: FIXMessageSpec
mNetworkCounterpartySystemStatusRequest = FMSpec { mType = (C.pack "BC"), mTags = mNetworkCounterpartySystemStatusRequestTags }
   where
      mNetworkCounterpartySystemStatusRequestTags = 
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
          LT.insert (tnum tNetworkRequestType) tNetworkRequestType $
          LT.insert (tnum tNetworkRequestID) tNetworkRequestID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mNetworkCounterpartySystemStatusResponse :: FIXMessageSpec
mNetworkCounterpartySystemStatusResponse = FMSpec { mType = (C.pack "BD"), mTags = mNetworkCounterpartySystemStatusResponseTags }
   where
      mNetworkCounterpartySystemStatusResponseTags = 
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
          LT.insert (tnum tNetworkStatusResponseType) tNetworkStatusResponseType $
          LT.insert (tnum tNetworkRequestID) tNetworkRequestID $
          LT.insert (tnum tNetworkResponseID) tNetworkResponseID $
          LT.insert (tnum tLastNetworkResponseID) tLastNetworkResponseID $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mUserRequest :: FIXMessageSpec
mUserRequest = FMSpec { mType = (C.pack "BE"), mTags = mUserRequestTags }
   where
      mUserRequestTags = 
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
          LT.insert (tnum tUserRequestID) tUserRequestID $
          LT.insert (tnum tUserRequestType) tUserRequestType $
          LT.insert (tnum tUsername) tUsername $
          LT.insert (tnum tPassword) tPassword $
          LT.insert (tnum tNewPassword) tNewPassword $
          LT.insert (tnum tRawDataLength) tRawDataLength $
          LT.insert (tnum tRawData) tRawData $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mUserResponse :: FIXMessageSpec
mUserResponse = FMSpec { mType = (C.pack "BF"), mTags = mUserResponseTags }
   where
      mUserResponseTags = 
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
          LT.insert (tnum tUserRequestID) tUserRequestID $
          LT.insert (tnum tUsername) tUsername $
          LT.insert (tnum tUserStatus) tUserStatus $
          LT.insert (tnum tUserStatusText) tUserStatusText $
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mCollateralInquiryAck :: FIXMessageSpec
mCollateralInquiryAck = FMSpec { mType = (C.pack "BG"), mTags = mCollateralInquiryAckTags }
   where
      mCollateralInquiryAckTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

mConfirmationRequest :: FIXMessageSpec
mConfirmationRequest = FMSpec { mType = (C.pack "BH"), mTags = mConfirmationRequestTags }
   where
      mConfirmationRequestTags = 
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
          LT.insert (tnum tSignatureLength) tSignatureLength $
          LT.insert (tnum tSignature) tSignature $
          LT.insert (tnum tCheckSum) tCheckSum $
          LT.new

