package com.yoco.commons.utils.events;

import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.Validator;

import java.util.HashMap;
import java.util.Map;

public class ClockIn {

    private static final String INVALID_USER_PRO = "Invalid user pro";
    private static final String INVALID_HEADER_INFO = "Invalid header details";

    public static Map<String, Object> clockInHandler (PeopleRelationJDO userPRO, Map<String, Object> headerMap, Map<String, Object> payload, long receivedMilli, String clientID) {

        Validator.checkArgument(ObjUtils.isNull(userPRO), INVALID_USER_PRO);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(headerMap), INVALID_HEADER_INFO);
        Validator.checkArgument(ObjUtils.isNullOrEmpty(payload), COMMON_ERROR_RESPONSE.INVALID_PAYLOAD.value());
        Validator.checkArgument(ObjUtils.isNullOrEmpty(headerMap), INVALID_HEADER_INFO);

        return new HashMap<>();

    }

}
