package com.yoco.adjustment.modal;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;
import lombok.Data;
import java.util.Map;

@Data
public class AdjustmentDeletePayloadDTO{
    private Map<String,Object> eventToDelete;
    private boolean shouldDeleteSubsets;
    private PeopleRelationJDO loggedInUserPro;
    private PeopleRelationJDO entryContactPro;
    private String timezone;
    private DateFormats dateFormat;
    private String message;
    private boolean isRejectAdjustmentRequest = false;

    public AdjustmentDeletePayloadDTO(){}

    public AdjustmentDeletePayloadDTO(Map<String, Object> event, boolean shouldDeleteSubsets, PeopleRelationJDO loggedInPro, PeopleRelationJDO entryContactPro, String timezone, String dateFormat, String message){
        this.eventToDelete = event;
        this.shouldDeleteSubsets = shouldDeleteSubsets;
        this.loggedInUserPro = loggedInPro;
        this.entryContactPro = entryContactPro;
        this.timezone = Validator.isValidTimeZone(timezone) ? timezone : loggedInPro.getTimeZone();
        this.dateFormat = DateFormats.parseAndGetDateFormat(dateFormat);
        this.message = ObjUtils.isNullOrEmpty(message) ? "" : message.trim();
        if(!ObjUtils.isNullOrEmpty(this.message)){
            isRejectAdjustmentRequest = true;
        }
    }
}
