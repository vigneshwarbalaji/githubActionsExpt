package com.yoco.downloads.report.service;

import com.yoco.commons.constants.Commons;
import com.yoco.commons.constants.SchedulingKeys;
import com.yoco.commons.dataservices.impl.AccountImpl;
import com.yoco.commons.dataservices.impl.ReportImpl;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.entity.SettingsJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.report.ReportsDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import lombok.extern.slf4j.Slf4j;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

@Slf4j
public class ReportDownloadService {

    public static final String REPORT_TYPE_HOURS = "HoursReport";

    public Map<String, Object> downloadGlobal(String accountID, Contact loggedInUserContact, Map<String, Object> payload, String cursor) throws IOException, NoSuchAlgorithmException {

        Validator.checkArgument(ObjUtils.isNullOrEmpty(accountID), COMMON_ERROR_RESPONSE.INVALID_ACCOUNT_ID.value());
        Validator.checkArgument(ObjUtils.isNull(loggedInUserContact), COMMON_ERROR_RESPONSE.INVALID_CONSTRAINTS.value());

        var range = (String)payload.get("range");
        var from = (String)payload.get("from");
        var to = (String)payload.get("to");
        RangeValidator.validateRange(range, from, to);

        Map<String, Object> responseMap = new HashMap<>();

        PeopleRelationJDO userPro = new UserImpl().getUserWithoutContact(accountID, loggedInUserContact.getId());

        if(!ObjUtils.isNull(userPro)) {
            var rangeInfoDTO =  DateUtil.getRangeDetails(userPro.getTimeZone(), range, from, to, accountID);
            var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), userPro.getTimeZone());
            var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(), userPro.getTimeZone());
            Map<String,Object> eventsResp = new ReportImpl().getAllEntriesOfAccount(accountID,startDate,endDate,1000,cursor);
            eventsResp = processDownloadEntries(eventsResp, userPro.getTimeZone());
            if(!eventsResp.containsKey(Commons.CURSOR)){
                String fileName = getStandardFilename(REPORT_TYPE_HOURS, accountID, "ALL", rangeInfoDTO.getFromDateEpochMilliseconds(), rangeInfoDTO.getToDateEpochMilliseconds(), userPro.getTimeZone());
                eventsResp.put("fileName", fileName);
            }
            return eventsResp;
        }

        return responseMap;
    }

    private Map<String, Object> processDownloadEntries (Map<String,Object> entries, String zone) {

        Map<String,Object> response = new HashMap<>();

        Map<String, Object> schRespData = (HashMap<String, Object>) entries.get(SchedulingKeys.DATA);
        List<ReportsDTO> convertedEntries = new ArrayList<>();

        if(!ObjUtils.isNullOrEmpty(schRespData)) {

            List<Map<String, Object>> eventsList = (ArrayList) schRespData.get(SchedulingKeys.EVENTS);
            log.info(eventsList.size() + " events size");
            if (!ObjUtils.isNullOrEmpty(eventsList)) {

                convertedEntries = eventsList.stream().map(event -> new ReportsDTO(event, "", zone)).collect(Collectors.toList());

                if (schRespData.containsKey(Commons.CURSOR)) {
                    response.put(Commons.CURSOR, schRespData.get(Commons.CURSOR));
                }
            }
        }
        response.put(SchedulingKeys.ENTRIES,convertedEntries);
        return response;

    }

    private String getStandardFilename(String reportType, String accountID, String staffName,
                                       long startTimeLong, long endTimeLong, String timezoneId) {

        String fileName = "";
        String displayDomainName = "";
        String startDate = "";
        String endDate = "";
        try {
            SettingsJDO settingsJDO = new AccountImpl().getById(accountID);

            displayDomainName = settingsJDO.getDisplayDomainName().replace(" ", "-");
            fileName = reportType + "_" + displayDomainName;

            if (!ObjUtils.isNullOrEmpty(staffName)) {
                fileName += "_" + staffName;
            }

            if(!ObjUtils.isNullOrEmpty(timezoneId)){

                if(startTimeLong != 0L){
                    startDate = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD, startTimeLong, timezoneId);
                }

                if(endTimeLong != 0L){
                    endDate = DateUtil.convertMillisToDateTimeText(DateFormats.YYYY_MM_DD, endTimeLong, timezoneId);
                }
            }

            if (!ObjUtils.isNullOrEmpty(startDate) && !ObjUtils.isNullOrEmpty(endDate)) {

                fileName += startDate.equalsIgnoreCase(endDate) ? "_" + startDate
                        : "_" + startDate + "_to_" + endDate;
            }

        } catch (Exception e) {
            log.info("Exception occurred on generating file name :: " + e.getMessage());
        }
        return fileName;
    }

}
