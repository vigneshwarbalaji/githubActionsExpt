package com.yoco.activity.service;

import com.yoco.activity.enums.ACTIVITY_ERROR_RESPONSE;
import com.yoco.commons.dataservices.impl.ActivityImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.UserClockinSubStatusJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.CollectionResponse;
import com.yoco.commons.utils.*;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.CommonConstants;
import lombok.extern.slf4j.Slf4j;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
public class ActivityService {

    public Map<String, Object> createActivity (String accountID, Contact currentUser, String payload) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(currentUser), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        Map<String, Object> payloadMap = JsonUtil.convertJsonToMap(payload);

        String contactID = payloadMap.containsKey("contactID") ? payloadMap.get("contactID").toString() : currentUser.getId();
        String projectID = payloadMap.containsKey("projectID") ? payloadMap.get("projectID").toString() : "";
        String activity = payloadMap.containsKey(CommonConstants.ACTIVITY) ? payloadMap.get(CommonConstants.ACTIVITY).toString() : "";
        String activityType = payloadMap.containsKey("activityType") ? payloadMap.get("activityType").toString() : ActivityUtil.ACTIVITIES.DUMMY.value();
        String eventDate = payloadMap.containsKey("eventDate") ? payloadMap.get("eventDate").toString() : "";
        String timeZoneID = payloadMap.containsKey("timeZoneID") ? payloadMap.get("timeZoneID").toString() : "";

        Validator.checkArgument(ObjUtils.isNullOrEmpty(activity), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(timeZoneID), TimeZoneUtil.INVALID_TIME_ZONE);


        PeopleRelationJDO userPro = new UserImpl().getUserWithoutContact(accountID, contactID);

        long eventReceivedTime = DateUtil.getCurrentTime();
        long eventLongTime = DateUtil.convertDateTimeTextToMillis(eventDate, DateFormats.DD_MMM_YYYY_HH_MM_SS_A, timeZoneID);

        Validator.checkArgument((DateUtil.getCurrentTime()<eventLongTime), ACTIVITY_ERROR_RESPONSE.EVENT_TIME_GREATER_THAN_CURRENT_TIME.value());

        UserClockinSubStatusJDO objUserClockinSubStatusJDO = new UserClockinSubStatusJDO(accountID, contactID, projectID, userPro.getEmailID(), activity, activityType, eventLongTime);
        objUserClockinSubStatusJDO = ActivityUtil.saveActivity( objUserClockinSubStatusJDO, eventReceivedTime);

        if(!ObjUtils.isNull(objUserClockinSubStatusJDO)){
            HashMap<String, Object> responseMap = new HashMap<>();
            objUserClockinSubStatusJDO.setEventDateTime(DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z, objUserClockinSubStatusJDO.getEventLongTime(), timeZoneID));
            responseMap.put(CommonConstants.ACTIVITY, objUserClockinSubStatusJDO);
            return responseMap;
        }

        return new HashMap<>();
    }

    public Map<String, Object> getActivities(String accountID, String contactID, String from, String to, String timeZoneID, String cursor, String isPrevious) {

        Validator.checkArgument( ObjUtils.isNullOrEmpty( accountID ), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument( ObjUtils.isNullOrEmpty( contactID ), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        Validator.checkArgument( ObjUtils.isNullOrEmpty( from ), ACTIVITY_ERROR_RESPONSE.INVALID_START_TIME.value());
        Validator.checkArgument( ObjUtils.isNullOrEmpty( timeZoneID ), TimeZoneUtil.INVALID_TIME_ZONE);

        long startMillis = DateUtil.convertDateTimeTextToMillis(from, DateFormats.DD_MMM_YYYY_HH_MM_SS_A, timeZoneID);

        long endMillis = !ObjUtils.isNullOrEmpty(to) ? DateUtil.convertDateTimeTextToMillis(to, DateFormats.DD_MMM_YYYY_HH_MM_SS_A, timeZoneID) : DateUtil.getCurrentTime();
        endMillis += 999;

        HashMap<String, Object> responseMap = new HashMap<>();

        ActivityImpl objActivityImpl = new ActivityImpl();

        CollectionResponse<UserClockinSubStatusJDO> collectionResponse = objActivityImpl.getActivityForRange(accountID, contactID, startMillis, endMillis, cursor, 5000, isPrevious);

        List<UserClockinSubStatusJDO> activitiesList = (List<UserClockinSubStatusJDO>) collectionResponse.getItems();

        if (!ObjUtils.isEmptyList(activitiesList)) {

            log.info(" activities List size " + activitiesList.size());

            for (UserClockinSubStatusJDO activity : activitiesList) {

                String eventDateTime = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD_T_HH_MM_SS_A_Z, activity.getEventLongTime(), timeZoneID);
                activity.setEventDateTime(eventDateTime);

            }
            responseMap.put("activities", activitiesList);
            if(!"true".equalsIgnoreCase(isPrevious)){
                responseMap.put("cursor", collectionResponse.getCursor());
            }
        }

        return responseMap;

    }
}
