package com.yoco.commons.validations;

import com.yoco.commons.constants.DateConstants;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;

import java.time.ZoneId;
import java.util.Arrays;

public class RangeValidator {

    private RangeValidator(){}

    private static final String[] REPORTS_RANGE = {DateConstants.TODAY,DateConstants.CURRENT_WEEK, DateConstants.LAST_WEEK};

    public static void validateRange(String range, String from, String to) {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(range), COMMON_ERROR_RESPONSE.INVALID_RANGE.value());

        if (DateConstants.BY_DATE.equalsIgnoreCase(range)) {
            Validator.checkArgument((ObjUtils.isNullOrEmpty(from) || ObjUtils.isNullOrEmpty(to)), COMMON_ERROR_RESPONSE.FROM_AND_TO_DATE_MANDATORY.value());
            Validator.checkArgument((ObjUtils.isNull(DateUtil.parseDate(from, DateFormats.MM_DD_YYYY)) || ObjUtils.isNull(DateUtil.parseDate(to, DateFormats.MM_DD_YYYY))), COMMON_ERROR_RESPONSE.INVALID_DATEFORMAT.value());
        } else {
            Validator.checkArgument( !Arrays.asList(REPORTS_RANGE).contains(range),COMMON_ERROR_RESPONSE.INVALID_RANGE.value());
        }
    }

    public static boolean isValidZoneID(String zoneID){
        try {
            ZoneId.of(zoneID);
            return true;
        } catch (Exception e) {
            return false;
        }
    }

    public static String validateAndReturnZoneID(String zoneID){
        return isValidZoneID(zoneID) ? zoneID : ZoneId.systemDefault().getId();
    }
}
