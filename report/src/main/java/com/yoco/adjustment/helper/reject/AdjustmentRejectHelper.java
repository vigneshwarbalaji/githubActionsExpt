package com.yoco.adjustment.helper.reject;

import com.yoco.adjustment.helper.delete.AdjustmentDeleteHelper;
import com.yoco.adjustment.util.AdjustmentUtil;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.REPORT_STATUS;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.fullservices.iammanagement.PermissionManager;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.events.ReportsUtil;
import com.yoco.commons.validations.Validator;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

public class AdjustmentRejectHelper {

    private AdjustmentRejectHelper(){}

    public static Map<String, Object> validateAuthorizationAndGetAdjustmentEvent(String entryID, PeopleRelationJDO loggedInUserPro) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> event = ReportImpl.getReportImplInstance().getEntryByID(entryID);
        Validator.checkArgument(!AdjustmentDeleteHelper.isValidAdjustment(event), ADJUSTMENTS_ERROR_RESPONSE.ACTION_ALREADY_TAKEN.value());
        Validator.checkArgument(!AdjustmentRejectHelper.isUserAuthorizedToRejectAdjustment(event,loggedInUserPro), COMMON_ERROR_RESPONSE.USER_NOT_AUTHORIZED.value());
        return event;
    }

    public static boolean isUserAuthorizedToRejectAdjustment(Map<String, Object> event, PeopleRelationJDO loggedInUserPro) {
        String entryAccountID = ReportsUtil.getAccountIDFromEvent(event);
        if(!loggedInUserPro.getUniquepin().equals(entryAccountID)){
            return false;
        }
        return PermissionManager.checkUserHasAdjustmentEditAccess(loggedInUserPro);
    }

    public static String validateAndExtractMessage(String payload) {
        Validator.checkArgument(!JsonUtil.isValidJson(payload), ADJUSTMENTS_ERROR_RESPONSE.INVALID_DESCRIPTION.value());
        Map<String,Object> payloadMap = JsonUtil.convertJsonToMap(payload);
        String message = Validator.unescapeHtml((String)payloadMap.get(Commons.MESSAGE));
        Validator.checkArgument(ObjUtils.isNullOrEmpty(message), ADJUSTMENTS_ERROR_RESPONSE.INVALID_DESCRIPTION.value());
        return message;
    }

    public static void updateEventForRejection(Map<String,Object> event, String adminContactID, String reason){
        Map<String,Object> metaData = ReportsUtil.getMetaDataMap(event);
        metaData.put(SchedulingKeys.STATUS, REPORT_STATUS.REJECTED.toString());
        Map<String,Object> additionalInfo = AdjustmentUtil.extractAdditionalInfo(metaData);
        Map<String,Object> adjustmentInfo = AdjustmentUtil.extractAdjustmentInfo(additionalInfo);
        adjustmentInfo.put(SchedulingKeys.REJECT_INFO,AdjustmentUtil.setAdjustmentInfo(adminContactID,reason));
        additionalInfo.put(SchedulingKeys.ADJUSTMENT_INFO,adjustmentInfo);
        metaData.put(SchedulingKeys.ADDITIONAL_INFO,additionalInfo);

        Map<String,Object> queryableMetaMap = ReportsUtil.getQueryableMetaMap(event);
        queryableMetaMap.put(SchedulingKeys.STATUS, REPORT_STATUS.REJECTED.toString());

        event.put(SchedulingKeys.METADATA, metaData);
        event.put(SchedulingKeys.QUERYABLE_META, queryableMetaMap);
    }
}
