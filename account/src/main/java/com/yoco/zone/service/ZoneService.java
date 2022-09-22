package com.yoco.zone.service;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.dao.UserDao;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.FCMService;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.*;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.zone.enums.ZONE_ERROR_MESSAGE;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.*;

public class ZoneService {

    public String getAutoDetectedZoneSevice (String offset) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty( offset ), ZONE_ERROR_MESSAGE.INVALID_SYSTEM_OFFSET.value());

        String zoneId = TimeZoneUtil.getTimeZoneID( offset );

        return TimeZoneUtil.getZoneInFormat( zoneId, TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH);

    }

    public Map<String, Object> getDates( String timeZoneID, String range, String fromDate, String toDate, String order, String weekStartDay ) {

        HashMap<String,Object> responseMap = new HashMap<>();
        Validator.checkArgument(!RangeValidator.isValidZoneID(timeZoneID),TimeZoneUtil.INVALID_TIME_ZONE);
        RangeValidator.validateRange(range,fromDate,toDate);
        List<HashMap<Long,HashMap<String,String>>> datesList = DateUtil.getDatesList(timeZoneID,range,fromDate,toDate,order,weekStartDay);
        if(ObjUtils.isEmptyList(datesList)){
            responseMap.put(Commons.SUCCESS,false);
        }else{
            responseMap.put(Commons.SUCCESS,true);
            responseMap.put("datesList",datesList);
        }
        return responseMap;

    }

    public Map<String, Object> updateTimezone(String accountID, String contactID, String payload, String srcClientID) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(contactID), COMMON_ERROR_RESPONSE.INVALID_CONTACT_ID.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());

        HashMap<String, Object> payloadMap = (HashMap<String, Object>) JsonUtil.convertJsonToMap(payload);

        Validator.checkArgument(!payloadMap.containsKey("timezoneID"), TimeZoneUtil.INVALID_TIME_ZONE);

        var timeZoneID = payloadMap.get("timezoneID").toString();

        if(!ObjUtils.isNullOrEmpty(timeZoneID)){

            HashMap<String, Object> responseMap = new HashMap<>();
            UserDao objUserDao = new UserImpl();

            PeopleRelationJDO userPRO = objUserDao.getUserWithoutContact(accountID, contactID);

            if(!ObjUtils.isNull(userPRO)) {

                String timezoneDisplayName = TimeZoneUtil.getZoneInFormat( timeZoneID, TimeZoneUtil.OFFSET_ZONE_DISPLAY_NAME_LONG, Locale.ENGLISH);

                userPRO.setTimeZone(timeZoneID);
                userPRO.setTimeZoneDisplayName(timezoneDisplayName);
                userPRO.setDateModified(new Date().getTime());

                objUserDao.savePro(userPRO);

                Set<String> contactIDSet = new HashSet<>();
                contactIDSet.add(contactID);

                new FCMService().notifyFCM( accountID, contactIDSet,"timeZone", true, userPRO.getTimeZone(), srcClientID);

                RTMService.publishToAW(accountID, contactID, "TIMEZONE_UPDATE", "", "");

                String activityMsg = "Timezone has been updated to " + userPRO.getTimeZoneDisplayName();
                ActivityUtil.saveActivity(accountID, contactID,"TIME_ZONE_UPDATE", userPRO.getEmailID(), activityMsg, ActivityUtil.ACTIVITIES.DUMMY.value(), DateUtil.getCurrentTime());

                UserDTO userDTO = new UserDTO(userPRO,null);
                RTMService.publishToChannel(accountID, "timezoneUpdate", "user", userDTO);

                long startTimeOfDay = DateUtil.getMillis(timeZoneID, 0, 0, 0, 0, 0, 0, 0);

                responseMap.put(Commons.SUCCESS, true);
                responseMap.put("startTimeOfDay", startTimeOfDay);
                responseMap.put("user",  userDTO);

                return responseMap;

            }
        }
        return new HashMap<>();

    }
}
