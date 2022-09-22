package com.yoco.payroll.modal;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import com.yoco.enums.ADJUSTMENTS_ERROR_RESPONSE;
import com.yoco.payroll.helper.adminupdate.PayrollAdminUpdateHelper;
import lombok.Data;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Data
public class AdminUpdatePayloadDTO {
    private String message;
    private long adjustedInTime;
    private long adjustedOutTime;
    private Map<String,Object> event;
    private boolean isReUpdateRequest = false;

    public AdminUpdatePayloadDTO(){}

    public AdminUpdatePayloadDTO(String payloadJson, String entryID, String contactID, PeopleRelationJDO requesterPro) throws IOException, NoSuchAlgorithmException {
        Map<String,Object> payloadMap = Validator.validateAndExtractPayload(payloadJson);
        this.validateAndExtractMessage(payloadMap);
        this.validateAndExtractAdjustedLongTime(payloadMap, requesterPro.getTimeZone());
        if(Boolean.TRUE.equals(payloadMap.get("isReUpdateRequest"))){
            this.isReUpdateRequest = true;
        }
        this.event = new PayrollAdminUpdateHelper().validateAuthorizationAndGetEvent(entryID,contactID,requesterPro, this.isReUpdateRequest);
    }

    private void validateAndExtractAdjustedLongTime(Map<String, Object> payloadMap, String timezone) {
        long inTimeMillis = DateUtil.convertDateTimeTextToMillis((String)payloadMap.get("inTime"), DateFormats.DD_MMM_YYYY_HH_MM_SS_A, timezone);
        long outTimeMillis = DateUtil.convertDateTimeTextToMillis((String)payloadMap.get("outTime"), DateFormats.DD_MMM_YYYY_HH_MM_SS_A, timezone);
        Validator.checkArgument(outTimeMillis <= inTimeMillis, ADJUSTMENTS_ERROR_RESPONSE.IN_TIME_GREATER_THAN_OUT_TIME.value());
        Validator.checkArgument(outTimeMillis > DateUtil.getCurrentTime(), ADJUSTMENTS_ERROR_RESPONSE.ADJUSTED_TIME_GREATER_THAN_CURRENT_TIME.value());
        this.adjustedInTime = inTimeMillis;
        this.adjustedOutTime = outTimeMillis;
    }

    private void validateAndExtractMessage(Map<String,Object> payloadMap){
        String updateMessage = Validator.unescapeHtml((String)payloadMap.get(Commons.MESSAGE));
        Validator.checkArgument(ObjUtils.isNullOrEmpty(updateMessage), ADJUSTMENTS_ERROR_RESPONSE.INVALID_DESCRIPTION.value());
        this.message = updateMessage;
    }
}
