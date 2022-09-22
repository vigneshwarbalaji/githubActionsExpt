package com.yoco.downloads.adjustment.service;

import com.itextpdf.text.DocumentException;
import com.yoco.adjustment.helper.AdjustmentHelper;
import com.yoco.downloads.adjustment.helper.AdjustmentDownloadHelper;
import com.yoco.adjustment.modal.GetAdjustmentPayloadDTO;
import com.yoco.commons.dataservices.impl.UserImpl;
import com.yoco.commons.entity.Contact;
import com.yoco.commons.entity.PeopleRelationJDO;
import com.yoco.commons.enums.DateFormats;
import com.yoco.commons.enums.error.COMMON_ERROR_RESPONSE;
import com.yoco.commons.utils.DateUtil;
import com.yoco.commons.utils.JsonUtil;
import com.yoco.commons.utils.ObjUtils;
import com.yoco.commons.validations.RangeValidator;
import com.yoco.commons.validations.Validator;
import com.yoco.enums.EVENTS_ERROR_RESPONSE;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.security.NoSuchAlgorithmException;
import java.util.Map;

@Slf4j
@Service
public class AdjustmentDownloadService {
    public Map<String, Object> downloadAdjustmentsPdf(String accountID, String status, GetAdjustmentPayloadDTO getAdjustmentPayloadDTO) throws DocumentException, IOException, NoSuchAlgorithmException {
        Contact loggedInUserContact = getAdjustmentPayloadDTO.getLoggedInUserContact();
        PeopleRelationJDO userPRO = UserImpl.getUserImplInstance().getUserWithoutContact(accountID, loggedInUserContact.getId());
        Validator.checkArgument(ObjUtils.isNull(userPRO), COMMON_ERROR_RESPONSE.USER_NOT_FOUND.value());
        Map<String, Object> skillsInfoMap = JsonUtil.convertJsonToMap(userPRO.getSkillsets());
        boolean canThisUserViewAdjustment = AdjustmentHelper.getInstance().checkIfTheUserHasRequiredAdjustmentPermission(skillsInfoMap);
        Validator.checkArgument(!canThisUserViewAdjustment, EVENTS_ERROR_RESPONSE.NO_SKILL_SET.value());
        String timeZone = userPRO.getTimeZone();
        String range = getAdjustmentPayloadDTO.getRange();
        String from = getAdjustmentPayloadDTO.getFrom();
        String to = getAdjustmentPayloadDTO.getTo();
        RangeValidator.validateRange(range, from, to);
        var rangeInfoDTO = DateUtil.getRangeDetails(timeZone, range, from, to, accountID);
        var startDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getFromDateEpochMilliseconds(), timeZone);
        var endDate= DateUtil.convertMillisToDateTimeText(DateFormats.ZULU, rangeInfoDTO.getToDateEpochMilliseconds(),timeZone);
        return AdjustmentDownloadHelper.getInstance().getPdfDocumentForDownload(accountID, status, startDate, endDate, timeZone, rangeInfoDTO);
    }
}
