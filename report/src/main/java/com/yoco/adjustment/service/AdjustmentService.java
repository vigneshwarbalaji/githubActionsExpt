package com.yoco.adjustment.service;

import com.yoco.adjustment.helper.delete.AdjustmentDeleteHelper;
import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.adjustment.helper.reject.AdjustmentRejectHelper;
import com.yoco.adjustment.modal.AdjustmentDeletePayloadDTO;
import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.commons.constants.Commons;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.modal.GenericResponse;
import com.yoco.commons.modal.report.AdjustmentDTO;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.constants.EventConstants;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Slf4j
@Service
public class AdjustmentService {

    public Map<String, Object> getAdjustments(String accountID, String status, GetAdjustmentPayloadDTO getAdjustmentPayloadDTO, String cursor, String isThisRequestFromHoursPage) throws IOException, NoSuchAlgorithmException {
        Map<String, Object>responseMap = new HashMap<>();
        Contact loggedInUserContact = getAdjustmentPayloadDTO.getLoggedInUserContact();
        String contactID = getAdjustmentPayloadDTO.getContactID();
        String range = getAdjustmentPayloadDTO.getRange();
        String from = getAdjustmentPayloadDTO.getFrom();
        String to = getAdjustmentPayloadDTO.getTo();
        RangeValidator.validateRange(range, from, to);
        PeopleRelationJDO userPRO = UserImpl.getUserImplInstance().getUserWithoutContact(accountID, loggedInUserContact.getId());
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        boolean canThisUserViewAdjustment = AdjustmentHelper.getInstance().verifyUserSkillSet(contactID, userPRO);
        Validator.checkArgument(Boolean.FALSE.equals(canThisUserViewAdjustment), EVENTS_ERROR_RESPONSE.NO_SKILL_SET.value());
        String timeZone = !ObjUtils.isNullOrEmpty(getAdjustmentPayloadDTO.getTimeZone()) ? getAdjustmentPayloadDTO.getTimeZone() : userPRO.getTimeZone();
        var rangeInfoDTO = DateUtil.getRangeDetails(timeZone, range, from, to, accountID);
        var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), timeZone);
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),timeZone);
        Map<String, Object> eventListMap = AdjustmentHelper.getInstance().getAdjustmentsBasedOnStatus(accountID, contactID, startDate, endDate, status, cursor);
        List<Map<String, Object>> eventList = (List<Map<String, Object>>) eventListMap.get(EventConstants.EVENT_LIST);
        List<AdjustmentDTO> convertedAdjustmentList = AdjustmentHelper.getInstance().convertEventListIntoAdjustmentList(eventList, timeZone, isThisRequestFromHoursPage);
        responseMap.put(EventConstants.ADJUSTMENTS,convertedAdjustmentList);
        responseMap.put(Commons.CURSOR, eventListMap.get(Commons.CURSOR));
        return responseMap;
    }

    public GenericResponse deleteAdjustment(String accountID, String entryID, boolean shouldDeleteSubsets, Contact loggedInUserContact, String timezone, String dateFormat) throws IOException, NoSuchAlgorithmException {
        PeopleRelationJDO loggedInUserPro = AdjustmentHelper.validateAndExtractLoggedInPro(accountID, loggedInUserContact);
        Map<String,Object> event = AdjustmentDeleteHelper.validateAuthorizationAndGetAdjustmentEvent(entryID, loggedInUserPro);
        PeopleRelationJDO entryContactPro = AdjustmentHelper.getEntryContactPro(loggedInUserPro,event);
        return AdjustmentDeleteHelper.validateAndDeleteAdjustment(new AdjustmentDeletePayloadDTO(event,shouldDeleteSubsets,loggedInUserPro,entryContactPro,timezone,dateFormat,null));
    }

    public GenericResponse rejectAdjustment(String accountID, String entryID, boolean shouldRejectSubsets, Contact loggedInUserContact, String timezone, String dateFormat, String payload) throws IOException, NoSuchAlgorithmException {
        String message = AdjustmentRejectHelper.validateAndExtractMessage(payload);
        PeopleRelationJDO loggedInUserPro = AdjustmentHelper.validateAndExtractLoggedInPro(accountID, loggedInUserContact);
        Map<String,Object> event = AdjustmentRejectHelper.validateAuthorizationAndGetAdjustmentEvent(entryID, loggedInUserPro);
        PeopleRelationJDO entryContactPro = AdjustmentHelper.getEntryContactPro(loggedInUserPro,event);
        return AdjustmentDeleteHelper.validateAndDeleteAdjustment(new AdjustmentDeletePayloadDTO(event,shouldRejectSubsets,loggedInUserPro,entryContactPro,timezone,dateFormat,message));
    }
}

