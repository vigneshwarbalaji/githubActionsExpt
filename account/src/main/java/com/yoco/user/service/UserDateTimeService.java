package com.yoco.user.service;

import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.modal.date.RangeInfoDTO;
import com.yoco.commons.modal.user.UserDTO;
import com.yoco.commons.services.RTMService;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.utils.TimeZoneUtil;
import com.yoco.commons.utils.UserPROUtil;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.user.enums.USER_ERROR_MESSAGE;
import java.util.HashMap;
import java.util.Map;

public class UserDateTimeService {
    public RangeInfoDTO getRangeInfo(String accountID, String contactID, String range, String fromDate, String toDate){
        RangeValidator.validateRange(range,fromDate,toDate);
        PeopleRelationJDO userPro = UserPROUtil.validateAndExtractUserPRO(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPro), USER_ERROR_MESSAGE.USER_NOT_FOUND.value());
        String userTimezone = userPro.getTimeZone();
        var rangeInfoDTO = DateUtil.getRangeDetails(userPro.getTimeZone(),range,fromDate,toDate,accountID);
        rangeInfoDTO.setFromDate(DateUtil.convertMillisToDateTimeText(DateFormats.MM_DD_YYYY,rangeInfoDTO.getFromDateEpochMilliseconds(),userTimezone));
        rangeInfoDTO.setToDate(DateUtil.convertMillisToDateTimeText(DateFormats.MM_DD_YYYY,rangeInfoDTO.getToDateEpochMilliseconds(),userTimezone));
        rangeInfoDTO.setTimezone(userTimezone);
        return rangeInfoDTO;
    }

    public Map<String, Object> checkDSTorNot (String accountID, String contactID) {

        PeopleRelationJDO userPRO = UserPROUtil.getUserPro(accountID, contactID);
        Validator.checkArgument(ObjUtils.isNull(userPRO),COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());

        var javaOffset = TimeZoneUtil.getCurrentOffset(userPRO.getTimeZone());
        var jdoOffset = userPRO.getTimeZoneDisplayName();
        UserDTO userDTO = new UserDTO(userPRO,null);
        if(!javaOffset.substring(4,10).equalsIgnoreCase(jdoOffset.substring(4,10))) {

            userPRO.setTimeZoneDisplayName(javaOffset);
            userPRO.setDateModified(DateUtil.getCurrentTime());
            UserImpl.getUserImplInstance().savePro(userPRO);

            userDTO = new UserDTO(userPRO,null);

            RTMService.publishToChannel(accountID, "DST_update", "user", userDTO );

        }

        Map<String, Object> responseMap = new HashMap<>();
        responseMap.put("user", userDTO);

        return responseMap;

    }
}
